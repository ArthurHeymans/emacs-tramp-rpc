;;; tramp-rpc-process.el --- Async and PTY process support for TRAMP-RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Keywords: comm, processes
;; Package-Requires: ((emacs "30.1") (msgpack "0.1.1"))

;; This file is part of tramp-rpc.

;; tramp-rpc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This file provides async (pipe) and PTY process support for tramp-rpc.
;; It handles:
;; - Starting remote processes (pipe and PTY modes)
;; - Async callback-based I/O for pipe processes (used by LSP, compilation)
;; - PTY process support via direct SSH or RPC
;; - Terminal resize handling for vterm/eat/shell-mode
;; - Process write queuing and serialization
;; - Adaptive poll-based I/O fallback

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 'tramp-rpc-protocol)

;; Functions from tramp.el
(declare-function tramp-add-external-operation "tramp")
(declare-function tramp-remove-external-operation "tramp")
(declare-function tramp-rpc--sudo-password-required-p "tramp-rpc")
(declare-function tramp-rpc--sudo-read-password "tramp-rpc")

;; Silence byte-compiler warnings for variables defined in vterm
(defvar vterm-copy-mode)
(defvar vterm-min-window-width)
(defvar vterm--term)

;; Functions from tramp-rpc.el (loaded before us)
(declare-function tramp-rpc--debug "tramp-rpc")
(declare-function tramp-rpc--ensure-connection "tramp-rpc")
(declare-function tramp-rpc--call "tramp-rpc" (vec method params &optional connection))
(declare-function tramp-rpc--call-fast "tramp-rpc")
(declare-function tramp-rpc--call-async "tramp-rpc" (vec method params callback &optional connection))
(declare-function tramp-rpc--get-direnv-environment "tramp-rpc")
(declare-function tramp-rpc--get-remote-login-shell "tramp-rpc")
(declare-function tramp-rpc--binary-bytes "tramp-rpc")
(declare-function tramp-rpc--controlmaster-socket-path "tramp-rpc")
(declare-function tramp-rpc--hops-to-proxyjump "tramp-rpc")
(declare-function tramp-rpc--port-to-string "tramp-rpc")
(declare-function tramp-rpc--ensure-inside-emacs-env "tramp-rpc")
(declare-function tramp-rpc--merge-environments "tramp-rpc")
(declare-function tramp-rpc--remote-path-environment "tramp-rpc")
(declare-function tramp-rpc--tramp-remote-process-environment "tramp-rpc")
(declare-function tramp-rpc--caller-environment "tramp-rpc")
(declare-function tramp-rpc--ssh-detail-user "tramp-rpc")
(declare-function tramp-rpc--sudo-rpc-hop-vec "tramp-rpc")
(declare-function tramp-rpc-file-name-p "tramp-rpc")

;; Variables from tramp-rpc.el
(defvar tramp-rpc-use-direct-ssh-pty)
(defvar tramp-rpc-use-controlmaster)
(defvar tramp-rpc-controlmaster-persist)
(defvar tramp-rpc-ssh-options)
(defvar tramp-rpc-ssh-args)
(defvar tramp-rpc-async-read-timeout-ms)
(defvar tramp-rpc--delivering-output)
(defvar tramp-rpc--closing-local-relay)

;; ============================================================================
;; Process tracking state
;; ============================================================================

(defvar tramp-rpc--async-processes (make-hash-table :test 'eq)
  "Hash table mapping local relay processes to their remote process info.
Value is a plist with :vec, :pid, :connection-process, :delivery-timer,
:poll-timer, :stderr-buffer.")

(defvar tramp-rpc--pty-processes (make-hash-table :test 'eq)
  "Hash table mapping local relay processes to their remote PTY process info.
Value is a plist with :vec, :pid.")

(defvar tramp-rpc--process-write-queues (make-hash-table :test 'equal)
  "Hash table mapping (connection process . remote PID) to write queue state.
Value is a plist with :vec, :connection, :connection-process, :owner-process,
:pending, :current, :writing, and :failure.")

(defvar tramp-rpc-write-queue-drain-timeout 5.0
  "Seconds to wait for queued process writes before signaling an error.")

(defun tramp-rpc--schedule-process-timer (table process timer-key function &rest args)
  "Schedule FUNCTION in PROCESS's TIMER-KEY slot in TABLE.
A callback that runs after cleanup sees no tracking entry and cannot
reschedule itself.  Delivery and polling use independent slots, so scheduling
the next read cannot discard output waiting for relay delivery."
  (when-let* ((info (gethash process table)))
    (when-let* ((old (plist-get info timer-key)))
      (cancel-timer old))
    (let (timer)
      (setq timer
            (run-at-time
             0 nil
             (lambda ()
               (when-let* ((current (gethash process table))
                           ((eq timer (plist-get current timer-key))))
                 (puthash process (plist-put current timer-key nil) table)
                 (apply function args)))))
      (puthash process (plist-put info timer-key timer) table)
      timer)))

(defun tramp-rpc--cancel-process-timers (table process)
  "Cancel and clear PROCESS's delivery and polling timers in TABLE."
  (when-let* ((info (gethash process table)))
    (dolist (timer-key '(:delivery-timer :poll-timer))
      (when-let* ((timer (plist-get info timer-key)))
        (cancel-timer timer)
        (setq info (plist-put info timer-key nil))))
    (puthash process info table)))

(define-error 'tramp-rpc-process-write-error
  "TRAMP-RPC process write failed" 'remote-file-error)

(declare-function tramp-rpc--get-connection "tramp-rpc" (vec))

(defun tramp-rpc--process-write-queue-key (vec pid &optional connection)
  "Return the write queue key for VEC and remote PID.
CONNECTION is the captured RPC connection process."
  (list (or connection
            (plist-get (tramp-rpc--get-connection vec) :process)
            vec)
        pid))

(defun tramp-rpc--process-write-queue-current-p (queue)
  "Return non-nil when QUEUE still belongs to VEC's current connection."
  (let ((connection (plist-get queue :connection-process))
        (current (tramp-rpc--get-connection (plist-get queue :vec))))
    (and connection current
         (eq connection (plist-get current :process)))))

(defun tramp-rpc--process-write-queue-live-p (queue)
  "Return non-nil when QUEUE still owns live local and RPC processes."
  (let ((connection (plist-get queue :connection-process))
        (owner (plist-get queue :owner-process)))
    (and queue
         (or (not (processp connection)) (process-live-p connection))
         (or (not (processp owner)) (process-live-p owner)))))

(defun tramp-rpc--process-write-data-bytes (data)
  "Return already-encoded DATA as a unibyte string for the server.
The process I/O handlers perform the requested input encoding before DATA
enters the queue.  Keep this helper byte-preserving so queued data cannot be
encoded a second time."
  (if (multibyte-string-p data)
      (encode-coding-string data 'binary)
    data))

(defun tramp-rpc--process-coding (process)
  "Return PROCESS's public (DECODING . ENCODING) coding pair."
  (or (process-get process :tramp-rpc-coding)
      (cons (car default-process-coding-system)
            (cdr default-process-coding-system))))

(defun tramp-rpc--encode-process-input (process data)
  "Encode DATA once with PROCESS's public encoding coding system.
Return raw unibyte bytes suitable for MessagePack bin payloads."
  (let ((encoding (cdr (tramp-rpc--process-coding process))))
    (if (multibyte-string-p data)
        (encode-coding-string data encoding)
      ;; Coding unibyte input preserves arbitrary bytes for binary and
      ;; no-conversion, while Latin-1 and UTF-8 apply their normal semantics.
      (encode-coding-string data encoding))))

(defun tramp-rpc--configure-relay-coding (process coding)
  "Configure PROCESS's binary relay and remember public CODING.
The relay's write side stays binary because output delivered to it is already
raw remote bytes.  Its read side remains the requested decoder, allowing
Emacs to retain decoder state across RPC chunks."
  (let* ((requested (tramp-rpc--normalize-coding
                     (or coding default-process-coding-system)))
         (pair (if (consp requested)
                   requested
                 (cons requested requested))))
    (set-process-coding-system process (car pair) 'binary)
    (process-put process :tramp-rpc-coding pair)
    pair))

(defun tramp-rpc--process-write-queue-pending-bytes (queue)
  "Return the number of bytes still held by QUEUE, including its current write."
  (+ (length (tramp-rpc--process-write-data-bytes
              (or (plist-get queue :current) "")))
     (cl-loop for item in (plist-get queue :pending)
              sum (length (tramp-rpc--process-write-data-bytes
                           (plist-get item :data))))))

(defun tramp-rpc--process-write-queue-fail (queue reason &optional error)
  "Mark QUEUE failed for REASON, preserving its unsent bytes and ERROR."
  (unless (plist-get queue :failure)
    (setq queue
          (plist-put queue :failure
                     (list :reason reason :error error
                           :pending-bytes
                           (tramp-rpc--process-write-queue-pending-bytes queue)))))
  (plist-put queue :writing nil))

(defun tramp-rpc--signal-process-write-failure (queue)
  "Signal QUEUE's structured write failure."
  (let ((failure (plist-get queue :failure)))
    (when failure
      (signal 'tramp-rpc-process-write-error
              (list (format "Process write failed (%s; %d pending bytes)"
                            (plist-get failure :reason)
                            (plist-get failure :pending-bytes))
                    failure)))))

(defun tramp-rpc--process-write-queue-key-for-owner (vec pid owner)
  "Return the exact queue key for VEC, PID, and OWNER.
Capture a connection only when this is the first operation for OWNER."
  (or (and (processp owner) (process-get owner :tramp-rpc-write-queue-key))
      (let (key)
        (maphash (lambda (candidate queue)
                   (when (and owner (eq owner (plist-get queue :owner-process)))
                     (setq key candidate)))
                 tramp-rpc--process-write-queues)
        key)
      (let* ((connection (or (and (processp owner)
                                  (process-get owner :tramp-rpc-connection))
                             (tramp-rpc--ensure-connection vec)))
             (key (tramp-rpc--process-write-queue-key
                   vec pid (plist-get connection :process))))
        (when (processp owner)
          (process-put owner :tramp-rpc-connection connection)
          (process-put owner :tramp-rpc-write-queue-key key))
        key)))

;; ============================================================================
;; Remote process primitives
;; ============================================================================

(defun tramp-rpc--start-remote-process (vec program args cwd &optional env)
  "Start PROGRAM with ARGS in CWD on remote host VEC.
ENV is an optional alist of environment variables.
Returns the remote process PID."
  (let ((result (tramp-rpc--call vec "process.start"
                                 `((cmd . ,program)
                                   (args . ,(vconcat args))
                                   (cwd . ,cwd)
                                   ,@(when env `((env . ,env)))))))
    (alist-get 'pid result)))

(defun tramp-rpc--read-remote-process (vec pid)
  "Read output from remote process PID on VEC.
Returns plist with :stdout, :stderr, :exited, :exit-code."
  (let ((result (tramp-rpc--call vec "process.read" `((pid . ,pid)))))
    (list :stdout (when-let* ((s (alist-get 'stdout result)))
                    (tramp-rpc--binary-bytes s))
          :stderr (when-let* ((s (alist-get 'stderr result)))
                    (tramp-rpc--binary-bytes s))
          :exited (alist-get 'exited result)
          :exit-code (alist-get 'exit_code result))))

(defun tramp-rpc--write-remote-process (vec pid data &optional owner-process)
  "Write DATA to stdin of remote process PID on VEC.
The queue remains bound to OWNER-PROCESS's original connection generation."
  (let* ((queue-key (tramp-rpc--process-write-queue-key-for-owner
                     vec pid owner-process))
         (queue (gethash queue-key tramp-rpc--process-write-queues))
         (connection (or (plist-get queue :connection)
                         (and (processp owner-process)
                              (process-get owner-process :tramp-rpc-connection))
                         (tramp-rpc--ensure-connection vec)))
         (state (or queue
                    (list :vec vec :pid pid :connection connection
                          :connection-process (plist-get connection :process)
                          :owner-process owner-process :pending nil
                          :current nil :writing nil))))
    (cond
     ((plist-get state :failure)
      (tramp-rpc--signal-process-write-failure state))
     ((not (tramp-rpc--process-write-queue-current-p state))
      (setq state (tramp-rpc--process-write-queue-fail
                   state :connection-replaced))
      (puthash queue-key state tramp-rpc--process-write-queues)
      (tramp-rpc--signal-process-write-failure state))
     (t
      (setq state (plist-put state :pending
                             (append (plist-get state :pending)
                                     (list (list :vec vec :pid pid :data data)))))
      (puthash queue-key state tramp-rpc--process-write-queues)
      (unless (plist-get state :writing)
        (tramp-rpc--process-write-queue queue-key))))))

(defun tramp-rpc--process-write-queue (queue-key)
  "Process the next pending write for QUEUE-KEY."
  (let* ((queue (gethash queue-key tramp-rpc--process-write-queues))
         (pending (plist-get queue :pending)))
    (when (and pending (not (plist-get queue :failure)))
      (cond
       ((not (tramp-rpc--process-write-queue-current-p queue))
        (puthash queue-key (tramp-rpc--process-write-queue-fail
                            queue :connection-replaced)
                 tramp-rpc--process-write-queues))
       ((not (tramp-rpc--process-write-queue-live-p queue))
        (puthash queue-key (tramp-rpc--process-write-queue-fail
                            queue :connection-unavailable)
                 tramp-rpc--process-write-queues))
       (t
        (let* ((item (car pending))
               (vec (plist-get item :vec))
               (pid (plist-get item :pid))
               (data (plist-get item :data))
               (state (plist-put (plist-put (copy-sequence queue)
                                            :pending (cdr pending))
                                 :current data)))
          (setq state (plist-put state :writing t))
          (puthash queue-key state tramp-rpc--process-write-queues)
          (condition-case err
              (tramp-rpc--call-async
               vec "process.write"
               `((pid . ,pid)
                 (data . ,(msgpack-bin-make
                            (tramp-rpc--process-write-data-bytes data))))
               (lambda (response)
                 ;; Cleanup may have removed the queue while this response was
                 ;; in flight.  Never recreate it or affect another generation.
                 (when-let* ((q (gethash queue-key
                                         tramp-rpc--process-write-queues)))
                   (cond
                    ((not (tramp-rpc--process-write-queue-current-p q))
                     (puthash queue-key (tramp-rpc--process-write-queue-fail
                                         q :connection-replaced)
                              tramp-rpc--process-write-queues))
                    ((plist-get response :error)
                     (puthash queue-key (tramp-rpc--process-write-queue-fail
                                         q :rpc-error (plist-get response :error))
                              tramp-rpc--process-write-queues))
                    (t
                     (setq q (plist-put q :current nil))
                     (setq q (plist-put q :writing nil))
                     (puthash queue-key q tramp-rpc--process-write-queues)
                     (tramp-rpc--process-write-queue queue-key)))))
               (plist-get state :connection))
            (error
             (let ((q (gethash queue-key tramp-rpc--process-write-queues)))
               (when q
                 (puthash queue-key
                          (tramp-rpc--process-write-queue-fail
                           q :send-error err)
                          tramp-rpc--process-write-queues)))
             (signal (car err) (cdr err))))))))))

(defun tramp-rpc--drain-write-queue (vec pid &optional owner-process)
  "Wait for VEC and PID's exact captured write queue to complete."
  (let* ((queue-key (tramp-rpc--process-write-queue-key-for-owner
                     vec pid owner-process))
         (deadline (+ (float-time) tramp-rpc-write-queue-drain-timeout)))
    (while (let ((queue (gethash queue-key tramp-rpc--process-write-queues)))
             (and queue (not (plist-get queue :failure))
                  (tramp-rpc--process-write-queue-current-p queue)
                  (tramp-rpc--process-write-queue-live-p queue)
                  (or (plist-get queue :writing) (plist-get queue :pending))
                  (< (float-time) deadline)))
      (accept-process-output nil 0.01))
    (when-let* ((queue (gethash queue-key tramp-rpc--process-write-queues)))
      (cond
       ((not (tramp-rpc--process-write-queue-current-p queue))
        (setq queue (tramp-rpc--process-write-queue-fail
                     queue :connection-replaced))
        (puthash queue-key queue tramp-rpc--process-write-queues))
       ((not (tramp-rpc--process-write-queue-live-p queue))
        (setq queue (tramp-rpc--process-write-queue-fail
                     queue :connection-unavailable))
        (puthash queue-key queue tramp-rpc--process-write-queues))
       ((or (plist-get queue :writing) (plist-get queue :pending))
        (setq queue (tramp-rpc--process-write-queue-fail queue :timeout))
        (puthash queue-key queue tramp-rpc--process-write-queues))
       )
      (tramp-rpc--signal-process-write-failure queue))))

(defun tramp-rpc--close-remote-stdin (vec pid &optional owner-process)
  "Close stdin on OWNER-PROCESS's original RPC connection generation."
  (let* ((queue-key (tramp-rpc--process-write-queue-key-for-owner
                     vec pid owner-process))
         (queue (gethash queue-key tramp-rpc--process-write-queues))
         (connection (or (plist-get queue :connection)
                         (and (processp owner-process)
                              (process-get owner-process :tramp-rpc-connection))
                         (tramp-rpc--ensure-connection vec))))
    (tramp-rpc--drain-write-queue vec pid owner-process)
    (when (and queue (not (tramp-rpc--process-write-queue-current-p queue)))
      (setq queue (tramp-rpc--process-write-queue-fail
                   queue :connection-replaced))
      (puthash queue-key queue tramp-rpc--process-write-queues)
      (tramp-rpc--signal-process-write-failure queue))
    (unless (eq (plist-get connection :process)
                (plist-get (tramp-rpc--get-connection vec) :process))
      (signal 'tramp-rpc-process-write-error
              (list "Process write failed (:connection-replaced; 0 pending bytes)"
                    (list :reason :connection-replaced :pending-bytes 0))))
    (condition-case err
        (tramp-rpc--call vec "process.close_stdin" `((pid . ,pid)) connection)
      (error
       ;; Closing stdin is idempotent: the remote process may be reaped after
       ;; the successful queue drain but before this RPC reaches the server.
       (unless (and (eq (car err) 'remote-file-error)
                    (string-match-p
                     "\\(?:Process not found\\|Process stdin is closed\\)"
                     (error-message-string err)))
         (signal (car err) (cdr err)))))))

(defun tramp-rpc--kill-remote-process (vec pid &optional signal)
  "Send SIGNAL to remote process PID on VEC."
  (tramp-rpc--call vec "process.kill"
                   `((pid . ,pid)
                     (signal . ,(or signal 15))))) ; SIGTERM

;; ============================================================================
;; Async Callback-based Process Reading (for LSP and interactive processes)
;; ============================================================================

(defun tramp-rpc--start-async-read (local-process)
  "Start an async read loop for LOCAL-PROCESS.
Sends a blocking read request; when response arrives, delivers output
and chains another read. This provides fast async I/O for LSP servers."
  (when (and (processp local-process)
             (process-live-p local-process)
             (gethash local-process tramp-rpc--async-processes))
    (let* ((info (gethash local-process tramp-rpc--async-processes))
           (vec (plist-get info :vec))
           (pid (plist-get info :pid)))
      (when (and vec pid)
        (tramp-rpc--debug "ASYNC-READ starting for pid=%s process=%s" pid local-process)
        ;; Send async read request with blocking timeout on server
        (tramp-rpc--call-async
         vec "process.read"
         `((pid . ,pid) (timeout_ms . ,tramp-rpc-async-read-timeout-ms))
         (lambda (response)
           (tramp-rpc--debug "ASYNC-READ callback invoked for pid=%s" pid)
           (tramp-rpc--handle-async-read-response local-process response)))))))

(defun tramp-rpc--deliver-process-output (local-process stdout stderr stderr-buffer)
  "Deliver STDOUT and STDERR to LOCAL-PROCESS.
Writes to the local cat relay process, which triggers proper I/O events
that satisfy accept-process-output.
STDERR-BUFFER is the separate stderr buffer, or nil to mix with stdout."
  (when (and (processp local-process) (process-live-p local-process))
    ;; Set flag to bypass our handler - we're writing TO the local process,
    ;; not sending data to the remote process
    (let ((tramp-rpc--delivering-output t))
      ;; Deliver stdout by writing to the cat relay process
      ;; This triggers actual I/O events that accept-process-output detects
      (when (and stdout (> (length stdout) 0))
        (tramp-rpc--debug "DELIVER stdout %d bytes to %s" (length stdout) local-process)
        (process-send-string local-process stdout))

      ;; Deliver stderr
      (when (and stderr (> (length stderr) 0))
        (tramp-rpc--debug "DELIVER stderr %d bytes" (length stderr))
        (let ((stderr-process
               (when stderr-buffer
                 (plist-get (gethash local-process tramp-rpc--async-processes)
                            :stderr-process))))
          (cond
           ;; Write to stderr cat relay if available, triggering proper I/O events
           ((and stderr-process (process-live-p stderr-process))
            (process-send-string stderr-process stderr))
           ;; Mix with stdout if no separate stderr buffer - write to cat relay
           (t
            (process-send-string local-process stderr))))))))

(defun tramp-rpc--deliver-pending-process-output (local-process)
  "Deliver all queued output chunks for LOCAL-PROCESS in response order."
  (when-let* ((info (gethash local-process tramp-rpc--async-processes)))
    (let ((pending (plist-get info :pending-output)))
      (puthash local-process (plist-put info :pending-output nil)
               tramp-rpc--async-processes)
      (dolist (chunk pending)
        (apply #'tramp-rpc--deliver-process-output local-process chunk)))))

(defun tramp-rpc--queue-process-output (local-process stdout stderr stderr-buffer)
  "Queue one non-exit output chunk and schedule its exact delivery once."
  (when-let* ((info (gethash local-process tramp-rpc--async-processes)))
    (setq info (plist-put info :pending-output
                          (append (plist-get info :pending-output)
                                  (list (list stdout stderr stderr-buffer)))))
    (puthash local-process info tramp-rpc--async-processes)
    (unless (plist-get info :delivery-timer)
      (tramp-rpc--schedule-process-timer
       tramp-rpc--async-processes local-process :delivery-timer
       #'tramp-rpc--deliver-pending-process-output local-process))))

(defun tramp-rpc--call-user-sentinel-once (process sentinel event)
"Call SENTINEL for PROCESS once, recording that it was called."
(when (and sentinel
(not (process-get process :tramp-rpc-user-sentinel-called)))
(process-put process :tramp-rpc-user-sentinel-called t)
(funcall sentinel process event)))

(defun tramp-rpc--pipe-process-sentinel (proc event &optional user-sentinel)
"Sentinel for pipe relay PROC, preserving USER-SENTINEL exactly once.
The relay remains tracked while its sentinel runs; cleanup removes it after
that chain has completed."
(when (and (memq (process-status proc) '(exit signal))
(gethash proc tramp-rpc--async-processes))
(let* ((info (gethash proc tramp-rpc--async-processes))
(vec (plist-get info :vec))
(pid (plist-get info :pid))
(connection (plist-get info :connection-process)))
(tramp-rpc--cancel-process-timers tramp-rpc--async-processes proc)
(unless (or (process-get proc :tramp-rpc-exited)
(process-get proc :tramp-rpc-transport-cleanup)
                  (and (processp connection)
                       (process-get connection :tramp-rpc-transport-dead)))
(when (and vec pid)
(ignore-errors (tramp-rpc--kill-remote-process vec pid 9))))
(when-let* ((stderr-process (plist-get info :stderr-process)))
(when (process-live-p stderr-process)
(ignore-errors (delete-process stderr-process))))
(let ((remote-exit (process-get proc :tramp-rpc-exit-code)))
(tramp-rpc--call-user-sentinel-once
proc user-sentinel
(if remote-exit
(if (= remote-exit 0)
"finished\n"
(format "exited abnormally with code %d\n" remote-exit))
event)))
(remhash proc tramp-rpc--async-processes))))

(defun tramp-rpc--handle-async-read-response (local-process response)
  "Handle async read response for LOCAL-PROCESS.
RESPONSE is the decoded RPC response plist."
  ;; Check process is still valid
  (when (and (processp local-process)
             (process-live-p local-process)
             (gethash local-process tramp-rpc--async-processes))
    (if-let* ((rpc-error (plist-get response :error)))
        (progn
          (tramp-rpc--debug "ASYNC-READ RPC error: %S" rpc-error)
          (tramp-rpc--handle-process-exit local-process -1))
      (condition-case err
          (let* ((info (gethash local-process tramp-rpc--async-processes))
                 (stderr-buffer (plist-get info :stderr-buffer))
                 (result (plist-get response :result))
                 ;; RPC process streams are MessagePack bin values.  Keep
                 ;; them raw until the local relay's incremental decoder.
                 (stdout (when-let* ((s (alist-get 'stdout result)))
                           (tramp-rpc--binary-bytes s)))
                 (stderr (when-let* ((s (alist-get 'stderr result)))
                           (tramp-rpc--binary-bytes s)))
                 (exited (alist-get 'exited result))
                 (exit-code (alist-get 'exit_code result)))

            (tramp-rpc--debug "ASYNC-READ response: stdout=%s stderr=%s exited=%s"
                             (if stdout (length stdout) "nil")
                             (if stderr (length stderr) "nil")
                             exited)

            ;; Deliver output.  When the remote process reports EXITED, flush
            ;; data immediately before sending EOF to the local relay; otherwise
            ;; the deferred delivery can race with relay shutdown and lose output.
            (if exited
                (progn
                  ;; Drain earlier non-exit chunks before EOF can close the
                  ;; relay, then synchronously flush this final chunk.
                  (tramp-rpc--deliver-pending-process-output local-process)
                  (when (or stdout stderr)
                    (tramp-rpc--deliver-process-output
                     local-process stdout stderr stderr-buffer)))
              ;; Queue rather than replace deferred chunks: a later read must
              ;; never cancel output that is still waiting for relay delivery.
              (when (or stdout stderr)
                (tramp-rpc--queue-process-output
                 local-process stdout stderr stderr-buffer)))

            ;; Handle process exit or chain next read
            (if exited
                ;; Handle exit immediately so `process-live-p' flips to nil
                ;; before callers can issue another round of remote operations.
                ;; Deferring this via `run-at-time 0' leaves a small window where
                ;; loops that poll `process-live-p' can observe a stale live
                ;; process and run one extra iteration.
                (tramp-rpc--handle-process-exit local-process exit-code)
              ;; Chain another read - use run-at-time to avoid stack overflow
              (tramp-rpc--schedule-process-timer
               tramp-rpc--async-processes local-process :poll-timer
               #'tramp-rpc--start-async-read local-process)))
        (error
         (tramp-rpc--debug "ASYNC-READ-ERROR: %S" err)
         ;; On error, clean up
         (tramp-rpc--schedule-process-timer
          tramp-rpc--async-processes local-process :poll-timer
          #'tramp-rpc--handle-process-exit local-process -1))))))

(defun tramp-rpc--handle-process-exit (local-process exit-code)
  "Handle exit of remote process associated with LOCAL-PROCESS.
Stores the remote exit code and sends EOF to the local cat relay so
it flushes remaining output and exits naturally.  The process sentinel
\(`tramp-rpc--pipe-process-sentinel') fires when cat exits, handles
cleanup, and calls the user's sentinel with the correct event string.

This design follows TRAMP's approach: let Emacs's process machinery
handle sentinel dispatch rather than fighting it with `delete-process'
+ deferred `run-at-time' sentinel calls.  Doing `delete-process'
before the cat relay drains its pipe causes a stale FD that makes
`input-pending-p' return t permanently, starving keyboard input."
  (let ((info (gethash local-process tramp-rpc--async-processes)))
    (when info
      (tramp-rpc--cancel-process-timers tramp-rpc--async-processes local-process)
      ;; Clean up only this connection's write queue for this process.
      (when-let* ((pid (plist-get info :pid))
                   (vec (plist-get info :vec)))
        (remhash (or (process-get local-process :tramp-rpc-write-queue-key)
                     (tramp-rpc--process-write-queue-key
                      vec pid (plist-get info :connection-process)))
                 tramp-rpc--process-write-queues))
      ;; Store exit code (the sentinel reads this to construct the event
      ;; string).  Do NOT set :tramp-rpc-exited yet — the process-status
      ;; handler returns 'exit when that flag is set, which makes
      ;; `process-live-p' return nil and would prevent the EOF below
      ;; from being sent.
      (process-put local-process :tramp-rpc-exit-code (or exit-code 0))
      ;; Send EOF to the stderr cat relay so it exits cleanly.
      (when-let* ((stderr-process (plist-get info :stderr-process)))
        (when (process-live-p stderr-process)
          (ignore-errors (process-send-eof stderr-process))))
      ;; Send EOF to the LOCAL cat relay (not the remote process).
      ;; Bind `tramp-rpc--closing-local-relay' so the `process-send-eof'
      ;; handler calls the original function instead of routing to the
      ;; remote stdin (which has already exited).  Cat will flush any
      ;; remaining data to stdout, then exit naturally on EOF.  Emacs
      ;; fires the sentinel chain; the cleanup installed by
      ;; `tramp-rpc--install-process-cleanup' then deletes the process.
      (when (process-live-p local-process)
        (let ((tramp-rpc--closing-local-relay t))
          (ignore-errors (process-send-eof local-process))))
      ;; Now mark as exited so process-status handler returns 'exit.
      (process-put local-process :tramp-rpc-exited t))))

;; ============================================================================
;; Process cleanup after exit
;; ============================================================================

(defun tramp-rpc--install-process-cleanup (process)
  "Add sentinel cleanup to PROCESS so it is deleted after exit.
Wrap the caller's current sentinel and invoke it at event time before
scheduling cleanup.  Keep symbol sentinels as symbols when calling them
so dynamic rebinding, such as TRAMP's `shell-command-sentinel' test
rebinding, is still honored.  Without cleanup, `get-buffer-process'
keeps returning the dead cat relay, which makes `vc-dir-busy' think an
update is still running."
  (cl-labels ((cleanup
               (proc)
               (run-at-time
                0 nil
                (lambda ()
                  (when (processp proc)
                    (remhash proc tramp-rpc--async-processes)
                    (unless (process-live-p proc)
                      (ignore-errors
                        (delete-process proc))))))))
    (cond
     ((process-live-p process)
      (let ((sentinel (process-sentinel process)))
        (unless (process-get process :tramp-rpc-cleanup-sentinel-installed)
          (process-put process :tramp-rpc-cleanup-sentinel-installed t)
          (set-process-sentinel
           process
           (lambda (proc event)
             (when sentinel
               (funcall sentinel proc event)
               ;; The deferred installer may have captured a caller sentinel
               ;; that replaced our wrapper; preserve its exactly-once result.
               (process-put proc :tramp-rpc-user-sentinel-called t))
             (when (memq (process-status proc) '(exit signal))
               ;; Keep our tracking cleanup in the chain even when the caller
               ;; replaced the sentinel after process creation.
               (tramp-rpc--pipe-process-sentinel proc event nil)
               ;; Defer deletion so the full sentinel chain completes first.
               (cleanup proc)))))))
     ((processp process)
      ;; The relay can finish before the deferred installer runs.  Its original
      ;; sentinel has already fired in that case, so just remove stale tracking.
      (cleanup process)))))

;; ============================================================================
;; Coding helper
;; ============================================================================

(defun tramp-rpc--coding-args (coding)
  "Return list of arguments for `set-process-coding-system' from CODING.
CODING is a non-nil `make-process' :coding value: either a symbol
(used for both decoding and encoding) or a cons cell (DECODING .
ENCODING).  When DECODING or ENCODING is nil inside a cons, it is
replaced with the corresponding default from
`default-process-coding-system', matching how native `make-process'
handles nil :coding elements."
  (let ((coding (tramp-rpc--normalize-coding coding)))
    (if (consp coding)
        (list (car coding) (cdr coding))
      (list coding coding))))

(defun tramp-rpc--normalize-coding (coding)
  "Normalize a `make-process' :coding value for internal use.

CODING may be nil, a coding-system symbol, or a cons cell
(DECODING . ENCODING).  Nil sides in a cons are replaced with the
corresponding side of `default-process-coding-system', like native
`make-process'.  When the resolved decoding and encoding coding systems
are identical, return the single symbol form.  This preserves native
semantics while avoiding fragile paths which assume a same-sided coding
pair is a symbol."
  (cond
   ((null coding) nil)
   ((consp coding)
    (let ((decoding (or (car coding) (car default-process-coding-system)))
          (encoding (or (cdr coding) (cdr default-process-coding-system))))
      (if (eq decoding encoding)
          decoding
        (cons decoding encoding))))
   (t coding)))

(defun tramp-rpc--maybe-login-shell-command (vec command)
  "Add login-shell args to interactive PTY login-shell COMMAND.
Non-shell commands, `shell -c ...', and script launches are left untouched.
Login args are appended after the existing args: `M-x shell' passes bash
`--noediting -i', and bash rejects the long `--noediting' option once a short
option like `-l' precedes it (an invalid-option error)."
  (let* ((program (car command))
         (args (cdr command))
         (base (and (stringp program) (file-name-nondirectory program)))
         (login-args (tramp-get-method-parameter
                      vec 'tramp-remote-shell-login)))
    (if (and base
             (or (null args)
                 (member "-i" args)
                 (member "--interactive" args))
             (not (member "-c" args))
             (not (member "--command" args))
             (not (member "--login" args))
             (not (cl-intersection login-args args :test #'string=))
             (equal base (ignore-errors
                           (file-name-nondirectory
                            (tramp-rpc--get-remote-login-shell vec)))))
        ;; bash refuses `bash -l --noediting -i' but accepts `bash --noediting -i -l'.
        (append command login-args)
      command)))

;; ============================================================================
;; make-process handler
;; ============================================================================

(defun tramp-rpc--make-process-skeleton-args (args)
  "Return ARGS adjusted for `tramp-skeleton-make-process'.
TRAMP's skeleton uses `or' when resolving `:connection-type', which would
turn an explicit nil into the dynamic `process-connection-type'.  Native
`make-process' treats explicit nil as a pipe, so pass `pipe' to the skeleton
for that one case.

The skeleton also treats string `:stderr' as a remote stderr file.  Preserve
tramp-rpc's existing compatibility behavior where non-remote strings name
stderr buffers."
  (let ((args (copy-sequence args)))
    (when (and (plist-member args :connection-type)
               (null (plist-get args :connection-type)))
      (setq args (plist-put args :connection-type 'pipe)))
    (when-let* ((stderr (plist-get args :stderr)))
      (when (and (stringp stderr)
                 (not (tramp-tramp-file-p stderr)))
        (setq args (plist-put args :stderr (get-buffer-create stderr)))))
    args))

(defun tramp-rpc-handle-make-process (&rest args)
  "Create an async process on the remote host.
ARGS are keyword arguments as per `make-process'.
Supports PTY allocation when :connection-type is \='pty or t,
or when `process-connection-type' is t.
For pipe mode, uses async polling for long-running processes.
Resolves program path and loads direnv environment from working directory."
  (let ((args (tramp-rpc--make-process-skeleton-args args)))
    ;; Reuse TRAMP's make-process skeleton for type checks, unique process names,
    ;; buffer setup, stderr validation, and TRAMP file-name parsing.  The body
    ;; below is only the RPC-specific process creation path.
    (tramp-skeleton-make-process args nil t
      ;; Unquote localname in case of file-name-quoted paths (e.g. /: prefix).
      (setq localname (file-name-unquote localname))
      (let* ((sudo-command (and (car command)
                                (string= (file-name-nondirectory (car command))
                                         "sudo")))
             ;; The skeleton normalizes t to `pty'.  `pipe' and nil are both
             ;; non-PTY from the RPC process launcher's perspective.
             (use-pty (eq connection-type 'pty))
             (pipe-sudo (and sudo-command (not use-pty)))
             (sudo-password nil)
             ;; Native process creation resolves omitted/nil coding from
             ;; `default-process-coding-system'.
             (coding (tramp-rpc--normalize-coding
                      (or coding default-process-coding-system))))
        ;; For non-PTY literal sudo commands, validate sudo in the same RPC pipe
        ;; execution context that will run the command.  If a password is needed,
        ;; use sudo's stdin wrapper; a separate PTY preauth can miss tty-scoped
        ;; sudo timestamps and leave the pipe-mode command unauthenticated.
        (when pipe-sudo
          (let ((ssh-user (or (tramp-rpc--ssh-detail-user v) (user-login-name))))
            (if (tramp-rpc--sudo-password-required-p v)
                (setq sudo-password (tramp-rpc--sudo-read-password v ssh-user)
                      ;; This RPC path invokes sudo directly, so unlike the SSH
                      ;; server-start path the empty prompt argument is preserved.
                      ;; Suppress prompt text so it does not pollute the async
                      ;; process' stdout/stderr.
                      command (append (list (car command) "-k" "-S" "-p" "")
                                      (cdr command)))
              (setq command (append (list (car command) "-n")
                                    (cdr command))))))
        (when use-pty
          (setq command (tramp-rpc--maybe-login-shell-command v command)))
        ;; Get the remote process environment: PATH from `tramp-remote-path'
        ;; (or deprecated `tramp-rpc-remote-path'), direnv for this directory,
        ;; INSIDE_EMACS, and caller-set env vars (e.g. GIT_INDEX_FILE from magit).
        (let ((process-env (tramp-rpc--ensure-inside-emacs-env
                            (tramp-rpc--merge-environments
                             (tramp-rpc--remote-path-environment v)
                             (tramp-rpc--tramp-remote-process-environment)
                             (tramp-rpc--get-direnv-environment v localname)
                             (tramp-rpc--caller-environment)))))
          (if use-pty
              ;; PTY mode - start async process with PTY.  Do not add -n or
              ;; pre-authenticate here; sudo owns the terminal prompt.
              (tramp-rpc--make-pty-process
               v name buffer command coding noquery
               filter sentinel localname process-env)
            ;; Pipe mode - use a local cat process as relay for proper I/O events.
            ;; This is needed because accept-process-output waits for actual I/O,
            ;; not just filter calls.  Leave relative PROGRAM names unresolved so
            ;; the server's process launcher searches the PATH we pass in
            ;; PROCESS-ENV.
            (let* ((program (car command))
                   (program-args (cdr command))
                   (remote-pid (tramp-rpc--start-remote-process
                                v program program-args localname process-env))
                   ;; Use a local cat process as relay: we write output to its
                   ;; stdin and it echoes to stdout, triggering proper I/O events.
                   (local-process (let ((process-connection-type nil))
                                    (start-process (or name "tramp-rpc-async")
                                                   buffer
                                                   "cat")))
                   (stderr-buffer (cond
                                   ((bufferp stderr) stderr)
                                   ((stringp stderr) (get-buffer-create stderr))
                                   (t nil)))
                   ;; Create a stderr cat relay so that
                   ;; (get-buffer-process stderr-buffer) returns a process,
                   ;; matching the contract of native `make-process' with :stderr.
                   (stderr-process
                    (when stderr-buffer
                      (let* ((process-connection-type nil)
                             (proc (start-process
                                    (format "%s-stderr" (or name "tramp-rpc-async"))
                                    stderr-buffer
                                    "cat")))
                        (set-process-query-on-exit-flag proc nil)
                        proc))))

              ;; The public pair controls user input encoding.  The local
              ;; relay itself reads with the requested decoder and writes
              ;; binary remote bytes without another encoding pass.
              (tramp-rpc--configure-relay-coding local-process coding)
              (when stderr-process
                (tramp-rpc--configure-relay-coding stderr-process coding))
              (set-process-query-on-exit-flag local-process (not noquery))

              ;; Feed sudo's stdin password before exposing the relay to callers;
              ;; subsequent writes use the same queue and stay ordered after it.
              (when sudo-password
                (tramp-rpc--write-remote-process
                 v remote-pid (tramp-rpc--encode-process-input
                               local-process (concat sudo-password "\n"))))

              (process-put local-process :tramp-rpc-vec v)
              (process-put local-process :tramp-rpc-pid remote-pid)
              (let ((connection (tramp-rpc--get-connection v)))
                (process-put local-process :tramp-rpc-connection connection)
                (process-put local-process :tramp-rpc-connection-process
                             (plist-get connection :process)))
              (process-put local-process 'tramp-vector v)
              (process-put local-process 'remote-command orig-command)

              (when filter
                (set-process-filter local-process filter))
              ;; Keep our wrapper even when the caller supplied no sentinel;
              ;; normal exits must still remove tracking.
              (process-put local-process :tramp-rpc-user-sentinel sentinel)
              (set-process-sentinel
               local-process
               (lambda (proc event)
                 (tramp-rpc--pipe-process-sentinel
                  proc event (process-get proc :tramp-rpc-user-sentinel))))

              ;; Store process info.
              (puthash local-process
                       (list :vec v
                             :pid remote-pid
                             :connection-process
                             (process-get local-process :tramp-rpc-connection-process)
                             :stderr-buffer stderr-buffer
                             :stderr-process stderr-process
                             :pending-output nil
                             :delivery-timer nil
                             :poll-timer nil)
                       tramp-rpc--async-processes)

              (tramp-rpc--debug
               "MAKE-PROCESS created local=%s remote-pid=%s program=%s"
               local-process remote-pid program)

              ;; Start async read loop.
              (tramp-rpc--start-async-read local-process)

              ;; Schedule deferred sentinel cleanup.  Callers like `vc-do-command'
              ;; replace the sentinel with `set-process-sentinel' AFTER
              ;; `start-file-process' returns, so we must add our cleanup wrapper
              ;; after that.  `run-at-time 0' ensures it runs once the current
              ;; code path (including the caller's sentinel setup) completes.
              ;; The wrapper calls `delete-process' after the sentinel chain
              ;; finishes, which removes the process from `Vprocess_alist'.
              ;; Without this, `get-buffer-process' returns stale exited cat
              ;; relays, causing e.g. `vc-dir-busy' to report a false positive.
              (let ((proc local-process))
                (run-at-time 0 nil
                             (lambda ()
                               (when (processp proc)
                                 (tramp-rpc--install-process-cleanup proc)))))

              local-process)))))))

(defun tramp-rpc-handle-start-file-process (name buffer program &rest args)
  "Start async process on remote host.
NAME is the process name, BUFFER is the output buffer,
PROGRAM is the command to run, ARGS are its arguments."
  (tramp-rpc-handle-make-process
   :name name
   :buffer buffer
   :command (cons program args)))

;; ============================================================================
;; PTY Process Support
;; ============================================================================

(defun tramp-rpc--make-pty-process (vec name buffer command coding noquery
                                         filter sentinel localname &optional direnv-env)
  "Create a PTY-based process for terminal emulators.
VEC is the tramp connection vector.
NAME, BUFFER, COMMAND, CODING, NOQUERY, FILTER, SENTINEL are process params.
LOCALNAME is the remote working directory.
DIRENV-ENV is an optional alist of environment variables from direnv.

When `tramp-rpc-use-direct-ssh-pty' is non-nil (the default), this uses
a direct SSH connection for the PTY, providing much lower latency for
interactive terminal use.  Otherwise, uses the RPC-based PTY implementation."
  (if (and tramp-rpc-use-direct-ssh-pty
           ;; Sudo-via-RPC paths must not use the final sudo vec for direct SSH,
           ;; because that would attempt to SSH as root instead of using the rpc
           ;; hop's identity and elevated RPC server.
           (not (tramp-rpc--sudo-rpc-hop-vec vec)))
      ;; Use direct SSH for low-latency PTY
      (tramp-rpc--make-direct-ssh-pty-process
       vec name buffer command coding noquery filter sentinel localname direnv-env)
    ;; Use RPC-based PTY
    (tramp-rpc--make-rpc-pty-process
     vec name buffer command coding noquery filter sentinel localname direnv-env)))

(defun tramp-rpc--direct-ssh-pty-sentinel (process event)
  "Remove direct SSH PTY PROCESS and invoke its user sentinel once."
  (when (memq (process-status process) '(exit signal))
    (remhash process tramp-rpc--pty-processes)
    (tramp-rpc--call-user-sentinel-once
     process (process-get process :tramp-rpc-user-sentinel) event)))

(defun tramp-rpc--install-direct-ssh-pty-sentinel (process)
  "Reinstall direct SSH PTY tracking after callers finish setup.
Callers may replace PROCESS's sentinel after `make-process' returns.  Capture
that sentinel on the next event-loop turn, preserve it once, and keep the
tracking cleanup wrapper in the chain."
  (when (processp process)
    (if (process-live-p process)
        (let ((sentinel (process-sentinel process)))
          (unless (eq sentinel #'tramp-rpc--direct-ssh-pty-sentinel)
            (set-process-sentinel
             process
             (lambda (proc event)
               (tramp-rpc--call-user-sentinel-once proc sentinel event)
               (tramp-rpc--direct-ssh-pty-sentinel proc event)))))
      ;; Its existing sentinel already observed the exit; only release tracking.
      (remhash process tramp-rpc--pty-processes))))

(defun tramp-rpc--make-direct-ssh-pty-process (vec name buffer command coding noquery
                                                    filter sentinel localname &optional direnv-env)
  "Create a PTY process using direct SSH connection.
This provides much lower latency than the RPC-based PTY by using a direct
SSH connection with `-t` for the terminal.  The SSH connection reuses the
existing ControlMaster socket, so authentication is already handled.

VEC is the tramp connection vector.
NAME, BUFFER, COMMAND, CODING, NOQUERY, FILTER, SENTINEL are process params.
LOCALNAME is the remote working directory.
DIRENV-ENV is an optional alist of environment variables from direnv."
  (let* ((host (tramp-file-name-host vec))
         (user (tramp-file-name-user vec))
         (port (tramp-rpc--port-to-string (tramp-file-name-port vec)))
         (program (car command))
         (program-args (cdr command))
         ;; Build environment exports for the remote command
         (env-exports (mapconcat
                       (lambda (pair)
                          (format "export %s=%s;"
                                  (car pair)
                                  (tramp-shell-quote-argument (cdr pair))))
                       (append direnv-env
                               `(("TERM" . ,(or (getenv "TERM") "xterm-256color"))))
                       " "))
         ;; Build the remote command - cd to dir, export env, exec program
         (remote-cmd (format "cd %s && %s exec %s %s"
                             (tramp-shell-quote-argument localname)
                             env-exports
                              (tramp-shell-quote-argument program)
                              (mapconcat #'tramp-shell-quote-argument program-args " ")))
         (proxyjump (tramp-rpc--hops-to-proxyjump vec))
         ;; Build SSH arguments for direct PTY connection
         (ssh-args (append
                    (list "ssh")
                    ;; Request PTY allocation
                    (list "-t" "-t")  ; Force PTY even without controlling terminal
                    ;; Multi-hop via ProxyJump
                    (when proxyjump (list "-J" proxyjump))
                    ;; Reuse ControlMaster if enabled
                    (when tramp-rpc-use-controlmaster
                      (list "-o" "ControlMaster=auto"
                            "-o" (format "ControlPath=%s"
                                         (tramp-rpc--controlmaster-socket-path vec))
                            "-o" (format "ControlPersist=%s"
                                         tramp-rpc-controlmaster-persist)))
                    ;; Only use BatchMode=yes when ControlMaster handles auth;
                    ;; without it, BatchMode=yes prevents password prompts.
                    (when tramp-rpc-use-controlmaster
                      (list "-o" "BatchMode=yes"))
                    (list "-o" "StrictHostKeyChecking=accept-new")
                    ;; Suppress "Shared connection to ... closed." messages
                    (list "-o" "LogLevel=error")
                    ;; User-specified SSH options
                    (mapcan (lambda (opt) (list "-o" opt))
                            tramp-rpc-ssh-options)
                    ;; Raw SSH arguments
                    tramp-rpc-ssh-args
                    ;; Connection parameters
                    (when user (list "-l" user))
                    (when port (list "-p" port))
                    ;; Host and command
                    (list host remote-cmd)))
         ;; Normalize buffer
         (actual-buffer (cond
                         ((bufferp buffer) buffer)
                         ((stringp buffer) (get-buffer-create buffer))
                         ((eq buffer t) (current-buffer))
                         (t nil)))
         ;; Start the SSH process with PTY
         (process-connection-type t)  ; Use PTY
         (process (apply #'start-process
                         (or name "tramp-rpc-direct-pty")
                         actual-buffer
                         ssh-args)))

    (tramp-rpc--debug "DIRECT-SSH-PTY started: %s -> %s %S"
                     process host command)

    ;; Configure the process
    (when coding
      (apply #'set-process-coding-system process
             (tramp-rpc--coding-args coding)))
    (set-process-query-on-exit-flag process (not noquery))

    ;; Set up filter
    (when filter
      (set-process-filter process filter))

    ;; Always retain a wrapper so normal exits remove PTY tracking.
    (set-process-sentinel process #'tramp-rpc--direct-ssh-pty-sentinel)

    ;; Store tramp-rpc metadata for compatibility with other code
    (let ((connection (tramp-rpc--get-connection vec)))
      (process-put process :tramp-rpc-connection-process
                   (plist-get connection :process)))
    (puthash process
             (list :vec vec
                   :connection-process
                   (process-get process :tramp-rpc-connection-process)
                   :direct-ssh t)
             tramp-rpc--pty-processes)
    (process-put process :tramp-rpc-pty t)
    (process-put process :tramp-rpc-direct-ssh t)
    (process-put process :tramp-rpc-vec vec)
    (process-put process :tramp-rpc-user-sentinel sentinel)
    (process-put process :tramp-rpc-command command)
    ;; Standard tramp property expected by tests and upstream code
    (process-put process 'remote-command command)
    (process-put process 'tramp-vector vec)

    ;; Match pipe relays: callers can replace the sentinel while their process
    ;; setup still owns the stack, so reinstall tracking after that setup.
    (let ((proc process))
      (run-at-time 0 nil
                   (lambda ()
                     (tramp-rpc--install-direct-ssh-pty-sentinel proc))))

    process))

(defun tramp-rpc--make-rpc-pty-process (vec name buffer command coding noquery
                                             filter sentinel localname &optional direnv-env)
  "Create a PTY-based process using the RPC server.
This is the fallback when direct SSH PTY is disabled.

VEC is the tramp connection vector.
NAME, BUFFER, COMMAND, CODING, NOQUERY, FILTER, SENTINEL are process params.
LOCALNAME is the remote working directory.
DIRENV-ENV is an optional alist of environment variables for the process."
  (let* ((program (car command))
         (program-args (cdr command))
         ;; Get terminal dimensions from buffer or use defaults
         (size (tramp-rpc--get-terminal-size buffer))
         (rows (cdr size))
         (cols (car size))
         ;; Build environment - add TERM after caller/remote env so it wins.
         (term-env (or (getenv "TERM") "xterm-256color"))
         (full-env (append direnv-env `(("TERM" . ,term-env))))
         ;; Start the PTY process on remote
         (result (tramp-rpc--call vec "process.start_pty"
                                   `((cmd . ,program)
                                     (args . ,(vconcat program-args))
                                     (cwd . ,localname)
                                     (rows . ,rows)
                                     (cols . ,cols)
                                     (env . ,full-env))))
         (remote-pid (alist-get 'pid result))
         (tty-name (alist-get 'tty_name result))
         ;; Normalize buffer - it can be t, nil, a buffer, or a string
         (actual-buffer (cond
                         ((bufferp buffer) buffer)
                         ((stringp buffer) (get-buffer-create buffer))
                         ((eq buffer t) (current-buffer))
                         (t nil)))
         ;; Use a local cat relay so Emacs owns incremental decoding of raw
         ;; PTY bytes, exactly as it does for pipe process output.
         (local-process (let ((process-connection-type nil))
                          (start-process (or name "tramp-rpc-pty")
                                         actual-buffer "cat"))))

    ;; Configure the local relay process.  Its write side is binary; the
    ;; public coding pair is retained for process API and input encoding.
    (tramp-rpc--configure-relay-coding local-process coding)
    (set-process-filter local-process (or filter #'tramp-rpc--pty-default-filter))
    (set-process-sentinel local-process #'tramp-rpc--pty-sentinel)
    (set-process-query-on-exit-flag local-process (not noquery))

    ;; Store process info
    (process-put local-process :tramp-rpc-pty t)
    (process-put local-process :tramp-rpc-pid remote-pid)
    (process-put local-process :tramp-rpc-vec vec)
    (process-put local-process :tramp-rpc-user-sentinel sentinel)
    (process-put local-process :tramp-rpc-command command)
    (process-put local-process :tramp-rpc-tty-name tty-name)
    ;; Standard tramp property expected by tests and upstream code
    (process-put local-process 'remote-command command)
    (process-put local-process 'tramp-vector vec)

    ;; Set up window size adjustment function
    (process-put local-process 'adjust-window-size-function
                 #'tramp-rpc--adjust-pty-window-size)

    ;; Track the PTY process and its exact transport generation.
    (let ((connection (tramp-rpc--get-connection vec)))
      (puthash local-process
               (list :vec vec :pid remote-pid
                     :connection-process (plist-get connection :process)
                     :rpc-pty t
                     :poll-timer nil)
               tramp-rpc--pty-processes)
      (process-put local-process :tramp-rpc-connection-process
                   (plist-get connection :process)))

    ;; Start async read loop
    (tramp-rpc--pty-start-async-read local-process)

    local-process))

(defun tramp-rpc--pty-default-filter (process output)
  "Default filter for PTY processes - insert output into process buffer."
  (when-let* ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((moving (= (point) (process-mark process))))
          (save-excursion
            (goto-char (process-mark process))
            (insert output)
            (set-marker (process-mark process) (point)))
          (when moving
            (goto-char (process-mark process))))))))

(defun tramp-rpc--get-terminal-size (buffer)
  "Get terminal size for BUFFER.
Returns (COLS . ROWS)."
  (let ((buf (cond
              ((bufferp buffer) buffer)
              ((stringp buffer) (get-buffer buffer))
              (t nil))))
    (if (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (let ((window (get-buffer-window buf)))
            (if window
                (cons (window-body-width window)
                      (window-body-height window))
              '(80 . 24))))
      '(80 . 24))))

(defun tramp-rpc--pty-start-async-read (local-process)
  "Start an async read loop for LOCAL-PROCESS.
Sends a blocking read request; when response arrives, delivers output
and chains another read.  This provides truly async PTY I/O."
  (when (and (processp local-process)
             (process-live-p local-process)
             (gethash local-process tramp-rpc--pty-processes))
    (let* ((vec (process-get local-process :tramp-rpc-vec))
           (pid (process-get local-process :tramp-rpc-pid)))
      (when (and vec pid)
        ;; Send async read request with blocking timeout on server
        (tramp-rpc--call-async
         vec "process.read_pty"
         `((pid . ,pid) (timeout_ms . 100))
         (lambda (response)
           (tramp-rpc--pty-handle-async-response local-process response)))))))

(defun tramp-rpc--pty-handle-async-response (local-process response)
  "Handle async read response for LOCAL-PROCESS.
RESPONSE is the decoded RPC response plist."
  ;; Check process is still valid
  (when (and (processp local-process)
             (process-live-p local-process)
             (gethash local-process tramp-rpc--pty-processes))
    (condition-case nil
               (let* ((result (plist-get response :result))
               ;; Keep PTY bytes raw; the relay decoder owns character
               ;; conversion and preserves split multibyte sequences.
               (output (when-let* ((o (alist-get 'output result)))
                         (tramp-rpc--binary-bytes o)))
               (exited (alist-get 'exited result))
                (exit-code (alist-get 'exit_code result)))

          ;; Feed raw bytes to the relay.  Its read-side decoder is
          ;; incremental, so a character split across RPC responses is kept
          ;; intact instead of being decoded once per chunk.
          (when (and output (> (length output) 0))
            (let ((tramp-rpc--delivering-output t))
              (process-send-string local-process output)
              ;; Exit handling deletes the relay immediately; service the
              ;; just-written bytes before that lifecycle transition.
              (accept-process-output local-process 0 nil t)))

          ;; Handle process exit or chain next read
          (if exited
              (tramp-rpc--handle-pty-exit local-process exit-code)
            ;; Chain via a tracked timer so cleanup can cancel it.
            (tramp-rpc--schedule-process-timer
             tramp-rpc--pty-processes local-process :poll-timer
             #'tramp-rpc--pty-start-async-read local-process)))
      (error
       ;; On error, clean up
       (tramp-rpc--handle-pty-exit local-process nil)))))

(defun tramp-rpc--handle-pty-exit (local-process exit-code)
  "Handle exit of PTY process associated with LOCAL-PROCESS."
  (when (gethash local-process tramp-rpc--pty-processes)
    (tramp-rpc--cancel-process-timers tramp-rpc--pty-processes local-process)
    ;; Close the remote PTY while the transport is still available.  Keep the
    ;; local entry until delete-process dispatches its wrapped sentinel.
    (when-let* ((vec (process-get local-process :tramp-rpc-vec))
               (pid (process-get local-process :tramp-rpc-pid)))
      (ignore-errors
        (tramp-rpc--call vec "process.close_pty" `((pid . ,pid)))))
    (process-put local-process :tramp-rpc-exit-code (or exit-code 0))
    (process-put local-process :tramp-rpc-exited t)
    (when (process-live-p local-process)
      (delete-process local-process))))

(defun tramp-rpc--pty-sentinel (process event)
  "Sentinel for PTY relay PROCESS, preserving its user sentinel once."
  (when (memq (process-status process) '(exit signal))
    (when-let* ((info (gethash process tramp-rpc--pty-processes)))
      (tramp-rpc--cancel-process-timers tramp-rpc--pty-processes process)
      ;; A transport cleanup already requested/closed the remote PTY.
      (unless (or (process-get process :tramp-rpc-exited)
                  (process-get process :tramp-rpc-transport-cleanup)
                  (and (processp (plist-get info :connection-process))
                       (process-get (plist-get info :connection-process)
                                    :tramp-rpc-transport-dead)))
        (when-let* ((vec (plist-get info :vec))
                    (pid (plist-get info :pid)))
          (ignore-errors
            (tramp-rpc--call vec "process.kill_pty"
                             `((pid . ,pid) (signal . 9))))))
      (let ((exit-code (process-get process :tramp-rpc-exit-code)))
        (tramp-rpc--call-user-sentinel-once
         process (process-get process :tramp-rpc-user-sentinel)
         (if exit-code
             (if (= exit-code 0)
                 "finished\n"
               (format "exited abnormally with code %d\n" exit-code))
           event)))
      ;; Run after the wrapped/user sentinel has observed the exit.
      (remhash process tramp-rpc--pty-processes))))

;; ============================================================================
;; PTY window resize support
;; ============================================================================

(defun tramp-rpc--adjust-pty-window-size (process _windows)
  "Adjust PTY window size when Emacs window size changes.
PROCESS is the local relay process, WINDOWS is the list of windows.
Returns nil to tell Emacs not to call `set-process-window-size' on
the local relay process (we handle resizing via RPC to the remote)."
  (when (and (process-live-p process)
             (process-get process :tramp-rpc-pty))
    (when-let* ((vec (process-get process :tramp-rpc-vec))
               (pid (process-get process :tramp-rpc-pid)))
      (let ((size (tramp-rpc--get-terminal-size (process-buffer process))))
        ;; Resize the remote PTY
        (ignore-errors
          (tramp-rpc--call-fast vec "process.resize_pty"
                                `((pid . ,pid)
                                  (cols . ,(car size))
                                  (rows . ,(cdr size))))))))
  ;; Return nil - we handle resizing ourselves, Emacs shouldn't try to
  ;; set-process-window-size on our local relay process
  nil)

(defun tramp-rpc--handle-pty-resize (process windows size-adjuster display-updater)
  "Handle PTY resize for tramp-rpc PROCESS displayed in WINDOWS.
SIZE-ADJUSTER is a function (width height) -> (width . height) that adjusts
the calculated size for the specific terminal emulator.
DISPLAY-UPDATER is a function (width height) that updates the terminal display.
Returns the final (width . height) cons, or nil if resize was not handled."
  (when (process-live-p process)
    (when-let* ((vec (process-get process :tramp-rpc-vec))
               (pid (process-get process :tramp-rpc-pid))
               (buf (process-buffer process)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let* ((size (funcall window-adjust-process-window-size-function
                                process windows))
                 (width (car size))
                 (height (cdr size))
                 (inhibit-read-only t))
            (when size
              ;; Let terminal-specific code adjust size
              (when size-adjuster
                (let ((adjusted (funcall size-adjuster width height)))
                  (setq width (car adjusted)
                        height (cdr adjusted))))
              (when (and (> width 0) (> height 0))
                ;; Resize remote PTY
                (ignore-errors
                  (tramp-rpc--call-fast vec "process.resize_pty"
                                        `((pid . ,pid)
                                          (cols . ,width)
                                          (rows . ,height))))
                ;; Let terminal-specific code update display
                (when display-updater
                  (funcall display-updater width height))
                (cons width height)))))))))

(defun tramp-rpc-handle-vterm--window-adjust-process-window-size (process windows)
  "Handler for vterm's window adjust function to handle TRAMP-RPC PTY processes.
For tramp-rpc processes, resize the remote PTY and update vterm's display.
For direct SSH PTY, let the original function handle it (SSH handles resize)."
  (cond
   ;; Direct SSH PTY - let original function handle it
   ((and (processp process)
         (process-get process :tramp-rpc-direct-ssh))
    (tramp-run-real-handler
     'vterm--window-adjust-process-window-size (list process windows)))
   ;; RPC-based PTY - resize via RPC
   ((and (processp process)
         (process-get process :tramp-rpc-pty))
    (unless vterm-copy-mode
        (tramp-rpc--handle-pty-resize
         process windows
         ;; Size adjuster: apply vterm margins and minimum width
         (lambda (width height)
           (when (fboundp 'vterm--get-margin-width)
             (setq width (- width (vterm--get-margin-width))))
           (cons (max width vterm-min-window-width) height))
         ;; Display updater: call vterm--set-size
         (lambda (width height)
           (when (and (boundp 'vterm--term) vterm--term
                    (fboundp 'vterm--set-size))
             (vterm--set-size vterm--term height width))))))
   ;; Not our process, call original
   (t (tramp-run-real-handler
       'vterm--window-adjust-process-window-size (list process windows)))))

(defun tramp-rpc-handle-eat--adjust-process-window-size (process windows)
  "Handler for eat's window adjust function to handle TRAMP-RPC PTY processes.
For tramp-rpc processes, resize the remote PTY and update eat's display.
For direct SSH PTY, let the original function handle it (SSH handles resize)."
  (cond
   ;; Direct SSH PTY - let original function handle it
   ((and (processp process)
         (process-get process :tramp-rpc-direct-ssh))
    (tramp-run-real-handler
     'eat--adjust-process-window-size (list process windows)))
   ;; RPC-based PTY - resize via RPC
   ((and (processp process)
         (process-get process :tramp-rpc-pty))
    (tramp-rpc--handle-pty-resize
       process windows
       ;; Size adjuster: ensure minimum of 1
       (lambda (width height)
         (cons (max width 1) (max height 1)))
       ;; Display updater: resize eat terminal and run hooks
       (lambda (width height)
         (when (and (boundp 'eat-terminal) eat-terminal
                    (fboundp 'eat-term-resize)
                    (fboundp 'eat-term-redisplay))
           (eat-term-resize eat-terminal width height)
           (eat-term-redisplay eat-terminal))
         (pcase major-mode
           ('eat-mode (run-hooks 'eat-update-hook))
           ('eshell-mode (run-hooks 'eat-eshell-update-hook))))))
   ;; Not our process, call original
   (t (tramp-run-real-handler
       'eat--adjust-process-window-size (list process windows)))))

;; ============================================================================
;; Process cleanup
;; ============================================================================

(defun tramp-rpc--cleanup-pty-processes (&optional vec connection-process remote-cleanup)
  "Clean up PTY processes for VEC and CONNECTION-PROCESS.
When REMOTE-CLEANUP is non-nil, request remote PTY termination before local
state is removed."
  (maphash
   (lambda (local-process info)
     (when (and (or (null vec)
                    (equal (tramp-rpc--connection-key (plist-get info :vec))
                           (tramp-rpc--connection-key vec)))
                (or (null connection-process)
                    (eq connection-process (plist-get info :connection-process))))
       (when (and remote-cleanup (plist-get info :rpc-pty)
                  (process-live-p connection-process))
         (let ((connection (tramp-rpc--get-connection (plist-get info :vec))))
           (when (eq connection-process (plist-get connection :process))
             (ignore-errors
               (tramp-rpc--call (plist-get info :vec) "process.kill_pty"
                                `((pid . ,(plist-get info :pid)) (signal . 9))
                                connection)))))
       ;; Keep tracking through delete-process so the wrapped sentinel and
       ;; user's sentinel are not suppressed.  The final remhash is idempotent.
       (process-put local-process :tramp-rpc-transport-cleanup t)
       (process-put local-process :tramp-rpc-transport-dead t)
       (when (process-live-p local-process)
         (delete-process local-process))
       (remhash local-process tramp-rpc--pty-processes)))
   tramp-rpc--pty-processes))

(defun tramp-rpc--cleanup-process-write-queues (&optional vec connection-process)
  "Remove write queues for VEC and CONNECTION-PROCESS.
When CONNECTION-PROCESS is non-nil, remove only that exact connection
generation.  With VEC nil, remove all queues."
  (let (stale)
    (maphash
     (lambda (key queue)
       (when (and (or (null vec)
                      (equal (tramp-rpc--connection-key (plist-get queue :vec))
                             (tramp-rpc--connection-key vec)))
                  (or (null connection-process)
                      (eq (plist-get queue :connection-process)
                          connection-process)))
         (push key stale)))
     tramp-rpc--process-write-queues)
    (dolist (key stale)
      (remhash key tramp-rpc--process-write-queues))))

(defun tramp-rpc--cleanup-async-processes (&optional vec connection-process remote-cleanup)
  "Clean up async processes for VEC and CONNECTION-PROCESS.
When REMOTE-CLEANUP is non-nil, request remote process termination while the
transport is live."
  (maphash
   (lambda (local-process info)
     (when (and (or (null vec)
                    (equal (tramp-rpc--connection-key (plist-get info :vec))
                           (tramp-rpc--connection-key vec)))
                (or (null connection-process)
                    (eq (plist-get info :connection-process)
                        connection-process)))
       (when (and remote-cleanup (process-live-p connection-process))
         (let ((connection (tramp-rpc--get-connection (plist-get info :vec))))
           (when (eq connection-process (plist-get connection :process))
             (ignore-errors
               (tramp-rpc--call (plist-get info :vec) "process.kill"
                                `((pid . ,(plist-get info :pid)) (signal . 9))
                                connection)))))
       (tramp-rpc--cancel-process-timers tramp-rpc--async-processes local-process)
       (when-let* ((stderr-process (plist-get info :stderr-process)))
         (when (process-live-p stderr-process)
           (ignore-errors (delete-process stderr-process))))
       ;; Keep tracking while delete-process dispatches the wrapped sentinel.
       (process-put local-process :tramp-rpc-transport-cleanup t)
       (process-put local-process :tramp-rpc-transport-dead t)
       (when (process-live-p local-process)
         (delete-process local-process))
       (remhash local-process tramp-rpc--async-processes)))
   tramp-rpc--async-processes)
  (tramp-rpc--cleanup-process-write-queues vec connection-process))

;; Forward declare for cleanup
(declare-function tramp-rpc--connection-key "tramp-rpc")

;; Install terminal emulator handler.  When vterm/eat is already loaded while
;; `tramp-rpc' is being required, defer until `tramp-rpc' is provided; otherwise
;; `tramp-add-external-operation' calls `(require 'tramp-rpc)' recursively.
(with-eval-after-load 'vterm
  (with-eval-after-load 'tramp-rpc
    (tramp-add-external-operation
     'vterm--window-adjust-process-window-size
     #'tramp-rpc-handle-vterm--window-adjust-process-window-size
     'tramp-rpc 'process)))

(with-eval-after-load 'eat
  (with-eval-after-load 'tramp-rpc
    (tramp-add-external-operation
     'eat--adjust-process-window-size
     #'tramp-rpc-handle-eat--adjust-process-window-size
     'tramp-rpc 'process)))

(defun tramp-rpc--process-handler-remove ()
  "Remove handlers."
  (tramp-remove-external-operation
   'vterm--window-adjust-process-window-size 'tramp-rpc)
  (tramp-remove-external-operation
   'eat--adjust-process-window-size 'tramp-rpc))

;; ============================================================================
;; Unload support
;; ============================================================================

(defun tramp-rpc-process-unload-function ()
  "Unload function for tramp-rpc-process.
Removes handlers and cleans up async processes."
  ;; Remove all handlers
  (tramp-rpc--process-handler-remove)
  ;; Clean up all async processes.
  (tramp-rpc--cleanup-async-processes)
  ;; Clean up PTY processes.
  (tramp-rpc--cleanup-pty-processes)
  ;; Return nil to allow normal unload to proceed
  nil)

(add-hook 'tramp-rpc-unload-hook
	  (lambda ()
	    (when (featurep 'tramp-rpc-process)
	      (unload-feature 'tramp-rpc-process 'force))))

(provide 'tramp-rpc-process)
;;; tramp-rpc-process.el ends here
