;;; tramp-rpc-process.el --- Async and PTY process support for TRAMP-RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Assisted-by: various LLMs
;; Keywords: comm, processes

;; This file is part of tramp-rpc.

;; tramp-rpc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This file provides async (pipe) and PTY process support for tramp-rpc.
;; It handles:
;; - Starting remote processes (pipe and PTY modes)
;; - Server-pushed output and exit notifications for pipe and RPC PTY
;;   processes (used by LSP, compilation, terminals)
;; - Unified managed-process tracking with O(1) (transport . pid) lookup
;;   and O(1) FIFO output/stdin queues
;; - PTY process support via direct SSH or RPC
;; - Non-blocking terminal resize handling for vterm/eat/shell-mode
;; - Process write queuing and serialization

;;; Code:

(require 'cl-lib)
(require 'msgpack)
(require 'tramp)
(require 'tramp-sh)
(require 'tramp-rpc-protocol)
(require 'tramp-rpc-connection)
(require 'tramp-rpc-hops)
(require 'tramp-rpc-transport)
(require 'tramp-rpc-environment)

;; Silence byte-compiler warnings for variables defined in vterm
(defvar vterm-copy-mode)
(defvar vterm-min-window-width)

;; Emitted inside the autoload form in tramp-rpc.el.
(declare-function tramp-rpc-file-name-p "tramp-rpc")
(declare-function tramp-rpc-protocol-signal-description "tramp-rpc-protocol"
                  (signal &optional os))
(declare-function tramp-rpc--cached-system-info "tramp-rpc-transport" (vec))
(declare-function tramp-rpc--process-directory "tramp-rpc-transport" (localname))
(declare-function tramp-rpc--sudo-ssh-user "tramp-rpc-transport" (vec))

;; ============================================================================
;; Process tracking state
;; ============================================================================

(defmacro tramp-rpc--best-effort (&rest body)
  "Run BODY for teardown and log failures instead of hiding them.
Best-effort cleanup must not replace an active transport or process error, but
unexpected failures remain visible in TRAMP-RPC debug output."
  (declare (indent 0) (debug t))
  `(condition-case err
       (progn ,@body)
     (error
      (tramp-rpc--debug "best-effort cleanup failed in %S: %s"
                        ',(car body) (error-message-string err))
      nil)))

(defvar tramp-rpc--bypass-next-relay-call nil
  "One-shot relay process token for bypassing tramp-rpc's send/eof advice.
Bound by `tramp-rpc--send-local-relay-string' and `tramp-rpc--close-local-relay'
and immediately cleared to nil inside the advice before invoking `orig-fun'.
This avoids private `nadvice' internals while keeping any nested filter or timer
calls (even for the exact same relay) properly routed to the remote process.")

(defun tramp-rpc--send-local-relay-string (relay string)
  "Write STRING to local output RELAY without process routing."
  (let ((tramp-rpc--bypass-next-relay-call relay))
    (process-send-string relay string)))

(defcustom tramp-rpc-synchronous-pipe-writes nil
  "Whether pipe process writes wait for remote acknowledgement.
When nil, `process-send-string' and `process-send-region' enqueue ordered
writes and return without a network round trip.  Asynchronous failures are
reported by the next write or by a flush operation such as
`process-send-eof'.  When non-nil, every write drains its queue before
returning, providing immediate error reporting at the cost of one or more
round trips."
  :type 'boolean
  :group 'tramp-rpc)

;; One `tramp-rpc-remote-process' object holds every piece of state a
;; local relay process has about its remote counterpart: identity
;; (vec, pid, owning connection generation), public process metadata
;; (command, tty, coding, sentinels), terminal status, and the output
;; delivery queue.  The relay process carries a single pointer to it
;; under :tramp-rpc-remote-process, and while the process is live
;; `tramp-rpc--managed-processes' maps the relay to that same object and
;; `tramp-rpc--pid-index' maps (transport . pid) to the relay in O(1).
;; Terminal status therefore remains readable from the relay after its live
;; index entries are removed, without a second copy of the state.

(cl-defstruct (tramp-rpc-remote-process
               (:constructor tramp-rpc--make-remote-process)
               (:copier nil))
  "State of one managed remote process behind a local relay process."
  ;; TRAMP vector and remote PID; PID is nil for direct SSH PTYs, whose
  ;; remote process is the SSH session itself.
  vec pid
  ;; The `tramp-rpc-connection' generation that started the process.
  connection
  ;; Flavor flags: RPC PTY, or direct SSH PTY (PTY is then also set).
  pty direct-ssh
  ;; Public metadata reported through the process API.
  command tty-name coding remote-os
  ;; Sentinel chain: tramp-rpc's own sentinel wraps the caller's,
  ;; which must run exactly once.
  own-sentinel user-sentinel user-sentinel-called
  ;; Terminal status.  EXITED is the local relay, REMOTE-EXITED the
  ;; remote process; TRANSPORT-CLEANUP marks transport teardown.
  exited remote-exited transport-cleanup
  exit-code exit-signal
  ;; Ordered stdin write queue key, see `tramp-rpc--process-write-queues'.
  write-queue-key
  ;; Optional separate stderr relay.
  stderr-buffer stderr-process
  ;; O(1) FIFO output delivery queue (HEAD . TAIL) or nil, single exit,
  ;; its timer, and the reentrancy guard for the delivery loop.
  pending-output pending-exit delivery-timer delivering
  ;; Serialized async PTY resize state: at most one resize RPC in flight,
  ;; with newer (COLS . ROWS) coalesced into `pending-resize'.
  resize-in-flight pending-resize)

(cl-defstruct (tramp-rpc-write-queue
               (:constructor tramp-rpc--make-write-queue)
               (:copier nil))
  "Ordered asynchronous stdin write queue for one remote process."
  vec pid connection connection-process owner-process
  ;; O(1) FIFO queue: (HEAD . TAIL) cons cell, or nil when empty.
  pending
  current writing failure)

(defun tramp-rpc--fifo-push (fifo item)
  "Append ITEM to FIFO (a (HEAD . TAIL) cons or nil) in O(1) and return it."
  (let ((cell (list item)))
    (if fifo
        (progn
          (setcdr (cdr fifo) cell)
          (setcdr fifo cell)
          fifo)
      (cons cell cell))))

(defun tramp-rpc--fifo-push-front (fifo item)
  "Prepend ITEM to FIFO (a (HEAD . TAIL) cons or nil) and return it."
  (if fifo
      (cons (cons item (car fifo)) (cdr fifo))
    (let ((cell (list item)))
      (cons cell cell))))

(defun tramp-rpc--fifo-items (fifo)
  "Return the list of items currently in FIFO without mutating it."
  (and fifo (car fifo)))

(defun tramp-rpc--remote-process (process)
  "Return PROCESS's `tramp-rpc-remote-process' state, or nil.
Use this where PROCESS may be an ordinary local process; slot accessors
require the non-nil result."
  (and (processp process)
       (process-get process :tramp-rpc-remote-process)))

(defun tramp-rpc--ensure-remote-process (process)
  "Return PROCESS's remote-process state, attaching a new one if needed."
  (or (tramp-rpc--remote-process process)
      (let ((remote (tramp-rpc--make-remote-process)))
        (process-put process :tramp-rpc-remote-process remote)
        remote)))

(defvar tramp-rpc--managed-processes (make-hash-table :test 'eq)
  "Hash table mapping live local relay processes to `tramp-rpc-remote-process'.
Holds pipe relays, RPC PTY relays, and direct SSH PTYs uniformly; flavor is
recorded in the struct's `pty' and `direct-ssh' slots.")

(defvar tramp-rpc--pid-index (make-hash-table :test 'equal)
  "O(1) secondary index mapping (TRANSPORT . PID) to live local relay process.")

(defun tramp-rpc--pid-index-key (remote)
  "Return the (TRANSPORT . PID) index key for REMOTE, or nil if unindexed."
  (when-let* ((pid (tramp-rpc-remote-process-pid remote))
              (transport (tramp-rpc--connection-transport
                          (tramp-rpc-remote-process-connection remote))))
    (cons transport pid)))

(defun tramp-rpc--track-managed-process
    (process vec pid connection pty &optional stderr-buffer stderr-process direct-ssh)
  "Register PROCESS with PID on VEC using its starting CONNECTION.
PTY marks PTY processes; DIRECT-SSH marks direct SSH PTYs (which have nil PID).
STDERR-BUFFER and STDERR-PROCESS identify the optional separate stderr relay.
Terminal status remains on PROCESS after its live index entry is removed."
  (let ((remote (tramp-rpc--ensure-remote-process process)))
    (setf (tramp-rpc-remote-process-vec remote) vec
          (tramp-rpc-remote-process-pid remote) pid
          (tramp-rpc-remote-process-connection remote) connection
          (tramp-rpc-remote-process-pty remote) pty
          (tramp-rpc-remote-process-direct-ssh remote) direct-ssh
          (tramp-rpc-remote-process-stderr-buffer remote) stderr-buffer
          (tramp-rpc-remote-process-stderr-process remote) stderr-process)
    (puthash process remote tramp-rpc--managed-processes)
    (when-let* ((key (tramp-rpc--pid-index-key remote)))
      (puthash key process tramp-rpc--pid-index))))

(defun tramp-rpc--unindex-managed-process (process &optional remote)
  "Remove PROCESS from `tramp-rpc--managed-processes' and `tramp-rpc--pid-index'.
REMOTE is PROCESS's remote-process record when the caller already has it."
  (let ((info (or remote (gethash process tramp-rpc--managed-processes))))
    (when info
      (when-let* ((key (tramp-rpc--pid-index-key info)))
        (when (eq (gethash key tramp-rpc--pid-index) process)
          (remhash key tramp-rpc--pid-index))))
    (remhash process tramp-rpc--managed-processes)))

(defun tramp-rpc--clear-delivery-state (process)
  "Drop PROCESS's queued delivery state, retaining its terminal status.
Pending output used to live only in the index tables and became garbage
when an entry was removed.  On the process-attached state it must be
released explicitly, or anything retaining the dead relay also retains
every undelivered chunk."
  (when-let* ((remote (tramp-rpc--remote-process process)))
    (setf (tramp-rpc-remote-process-pending-output remote) nil
          (tramp-rpc-remote-process-pending-exit remote) nil
          (tramp-rpc-remote-process-stderr-buffer remote) nil
          (tramp-rpc-remote-process-stderr-process remote) nil)))

(defun tramp-rpc--forget-managed-process (process)
  "Release PROCESS's live indexes and timers, retaining its terminal status."
  (tramp-rpc--cancel-process-timers process)
  (tramp-rpc--unindex-managed-process process)
  (tramp-rpc--clear-delivery-state process))

(defvar tramp-rpc--process-write-queues (make-hash-table :test 'equal)
  "Hash table mapping (connection-process . remote-pid) to `tramp-rpc-write-queue'.")

(defcustom tramp-rpc-write-queue-drain-timeout 5.0
  "Seconds to wait for queued process writes before signaling an error."
  :type 'number
  :group 'tramp-rpc)

(defun tramp-rpc--schedule-process-timer (process function &rest args)
  "Schedule FUNCTION as PROCESS's delivery timer.
A callback that runs after cleanup sees no tracking entry and cannot
reschedule itself."
  (when-let* ((remote (gethash process tramp-rpc--managed-processes)))
    (when-let* ((old (tramp-rpc-remote-process-delivery-timer remote)))
      (cancel-timer old))
    (let (timer)
      (setq timer
            (run-at-time
             0 nil
             (lambda ()
               (when-let* ((current (gethash process tramp-rpc--managed-processes))
                           ((eq timer (tramp-rpc-remote-process-delivery-timer
                                       current))))
                 (setf (tramp-rpc-remote-process-delivery-timer current) nil)
                 (apply function args)))))
      (setf (tramp-rpc-remote-process-delivery-timer remote) timer)
      (when tramp-rpc--process-timer-recorder
        ;; Record a predicate, not the slot location: the transport's
        ;; suspended-timer wait only needs to know whether TIMER is
        ;; still this process's current delivery timer.
        (funcall tramp-rpc--process-timer-recorder
                 timer
                 (lambda ()
                   (when-let* ((current (gethash process tramp-rpc--managed-processes)))
                     (eq timer (tramp-rpc-remote-process-delivery-timer
                                current))))))
      timer)))

(defun tramp-rpc--cancel-process-timers (process)
  "Cancel and clear PROCESS's delivery timer."
  (when-let* ((remote (or (gethash process tramp-rpc--managed-processes)
                          (tramp-rpc--remote-process process))))
    (when-let* ((timer (tramp-rpc-remote-process-delivery-timer remote)))
      (cancel-timer timer)
      (setf (tramp-rpc-remote-process-delivery-timer remote) nil))))

(define-error 'tramp-rpc-process-write-error
  "TRAMP-RPC process write failed" 'remote-file-error)

(defun tramp-rpc--process-write-queue-key (vec pid &optional connection)
  "Return the write queue key for VEC and remote PID.
CONNECTION is the captured RPC connection process."
  (list (or connection
            (tramp-rpc--connection-transport (tramp-rpc--get-connection vec))
            vec)
        pid))

(defun tramp-rpc--process-write-queue-current-p (queue)
  "Return non-nil when QUEUE still belongs to VEC's current connection."
  (let ((connection (tramp-rpc-write-queue-connection-process queue))
        (current (tramp-rpc--get-connection (tramp-rpc-write-queue-vec queue))))
    (and connection current
         (eq connection (tramp-rpc-connection-process current)))))

(defun tramp-rpc--process-write-queue-live-p (queue)
  "Return non-nil when QUEUE still owns live local and RPC processes."
  (let ((connection (tramp-rpc-write-queue-connection-process queue))
        (owner (tramp-rpc-write-queue-owner-process queue)))
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
  (or (when-let* ((remote (tramp-rpc--remote-process process)))
        (tramp-rpc-remote-process-coding remote))
      (cons (car default-process-coding-system)
            (cdr default-process-coding-system))))

(defun tramp-rpc--owner-connection (process)
  "Return the connection generation captured for relay PROCESS, or nil."
  (when-let* ((remote (tramp-rpc--remote-process process)))
    (tramp-rpc-remote-process-connection remote)))

(defun tramp-rpc--user-sentinel (process)
  "Return relay PROCESS's caller sentinel, or nil when unmanaged."
  (when-let* ((remote (tramp-rpc--remote-process process)))
    (tramp-rpc-remote-process-user-sentinel remote)))

(defun tramp-rpc--rpc-pty-process-p (process)
  "Return non-nil when PROCESS is an RPC PTY relay, not a direct SSH PTY."
  (when-let* ((remote (tramp-rpc--remote-process process)))
    (and (tramp-rpc-remote-process-pty remote)
         (not (tramp-rpc-remote-process-direct-ssh remote)))))

(defun tramp-rpc--encode-process-input (process data)
  "Encode DATA once with PROCESS's public encoding coding system.
Return raw unibyte bytes suitable for MessagePack bin payloads."
  ;; Coding unibyte input preserves arbitrary bytes for binary and
  ;; no-conversion, while Latin-1 and UTF-8 apply their normal semantics.
  (encode-coding-string data (cdr (tramp-rpc--process-coding process))))

(defun tramp-rpc--start-cat-relays (name buffer stderr-buffer cleanup)
  "Start local cat relays for NAME, BUFFER, and STDERR-BUFFER.
If relay construction fails, delete any relay already created and call CLEANUP
before propagating the original error.
Return (STDOUT-PROCESS . STDERR-PROCESS)."
  (let ((default-directory (default-toplevel-value 'temporary-file-directory))
        (exec-path (default-toplevel-value 'exec-path))
        (process-environment (default-toplevel-value 'process-environment))
        stdout-process stderr-process complete)
    (unwind-protect
        (prog1
            (progn
              (let ((process-connection-type nil))
                (setq stdout-process
                      (start-process name buffer "cat")))
              (when stderr-buffer
                (let ((process-connection-type nil))
                  (setq stderr-process
                        (start-process (format "%s-stderr" name)
                                       stderr-buffer "cat")))
                (set-process-query-on-exit-flag stderr-process nil))
              (cons stdout-process stderr-process))
          (setq complete t))
      (unless complete
        (dolist (process (list stderr-process stdout-process))
          (when (process-live-p process)
            (tramp-rpc--best-effort (delete-process process))))
        (tramp-rpc--best-effort (funcall cleanup))))))

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
    (setf (tramp-rpc-remote-process-coding (tramp-rpc--ensure-remote-process process)) pair)
    pair))

(defun tramp-rpc--process-write-queue-pending-bytes (queue)
  "Return the number of bytes still held by QUEUE, including its current write."
  (+ (length (tramp-rpc--process-write-data-bytes
              (or (tramp-rpc-write-queue-current queue) "")))
     (cl-loop for item in (tramp-rpc--fifo-items
                           (tramp-rpc-write-queue-pending queue))
              sum (length (tramp-rpc--process-write-data-bytes
                           (plist-get item :data))))))

(defun tramp-rpc--process-write-queue-fail (queue reason &optional error)
  "Mark QUEUE failed for REASON, preserving its unsent bytes and ERROR."
  (unless (tramp-rpc-write-queue-failure queue)
    (setf (tramp-rpc-write-queue-failure queue)
          (list :reason reason :error error
                :pending-bytes
                (tramp-rpc--process-write-queue-pending-bytes queue))))
  (setf (tramp-rpc-write-queue-writing queue) nil)
  queue)

(defun tramp-rpc--signal-process-write-failure (failure-or-queue)
  "Signal structured write failure from FAILURE-OR-QUEUE."
  (let ((failure (if (tramp-rpc-write-queue-p failure-or-queue)
                     (tramp-rpc-write-queue-failure failure-or-queue)
                   (plist-get failure-or-queue :failure))))
    (when failure
      (signal 'tramp-rpc-process-write-error
              (list (format "Process write failed (%s; %d pending bytes)"
                            (plist-get failure :reason)
                            (plist-get failure :pending-bytes))
                    failure)))))

(defun tramp-rpc--process-write-queue-key-for-owner (vec pid owner)
  "Return the exact queue key for VEC, PID, and OWNER.
Capture a connection only when this is the first operation for OWNER."
  (or (when-let* ((remote (tramp-rpc--remote-process owner)))
        (tramp-rpc-remote-process-write-queue-key remote))
      (let* ((connection (or (tramp-rpc--owner-connection owner)
                             (tramp-rpc--ensure-connection vec)))
             (key (tramp-rpc--process-write-queue-key
                   vec pid (tramp-rpc-connection-process connection))))
        (when (processp owner)
          (let ((remote (tramp-rpc--ensure-remote-process owner)))
            (setf (tramp-rpc-remote-process-connection remote) connection
                  (tramp-rpc-remote-process-write-queue-key remote) key)))
        key)))

;; ============================================================================
;; Remote process primitives
;; ============================================================================

(defun tramp-rpc--start-remote-process (vec program args cwd &optional env connection)
  "Start PROGRAM with ARGS in CWD on remote host VEC.
ENV is an optional alist of environment variables.
CONNECTION is the captured generation that will own the process.
Returns the remote process PID."
  (let ((result (tramp-rpc--call vec "process.start"
                                 `((cmd . ,program)
                                   (args . ,(vconcat args))
                                   (cwd . ,(when cwd
                                             (tramp-rpc--path-to-bin cwd)))
                                   ,@(when env `((env . ,env))))
                                 connection)))
    (alist-get 'pid result)))

(defun tramp-rpc--forget-write-queue (_local-process remote)
  "Remove the write queue of a relay from its REMOTE state."
  (when-let* ((pid (tramp-rpc-remote-process-pid remote))
              (vec (tramp-rpc-remote-process-vec remote)))
    (remhash (or (tramp-rpc-remote-process-write-queue-key remote)
                 (tramp-rpc--process-write-queue-key
                  vec pid (tramp-rpc--connection-transport
                           (tramp-rpc-remote-process-connection remote))))
             tramp-rpc--process-write-queues)))

(defun tramp-rpc--write-remote-process (vec pid data &optional owner-process)
  "Write DATA to stdin of remote process PID on VEC.
The queue remains bound to OWNER-PROCESS's original connection generation.
Like for a local process, writing after OWNER-PROCESS has died is an error;
its queue is gone and must not be created again."
  (when (and (processp owner-process) (not (process-live-p owner-process)))
    (error "Process %s not running" (process-name owner-process)))
  (let* ((queue-key (tramp-rpc--process-write-queue-key-for-owner
                     vec pid owner-process))
         (queue (gethash queue-key tramp-rpc--process-write-queues))
         (connection (or (and queue (tramp-rpc-write-queue-connection queue))
                         (tramp-rpc--owner-connection owner-process)
                         (tramp-rpc--ensure-connection vec)))
         (state (or queue
                    (let ((q (tramp-rpc--make-write-queue
                              :vec vec :pid pid :connection connection
                              :connection-process
                              (tramp-rpc-connection-process connection)
                              :owner-process owner-process)))
                      (puthash queue-key q tramp-rpc--process-write-queues)
                      q))))
    (cond
     ((tramp-rpc-write-queue-failure state)
      (tramp-rpc--signal-process-write-failure state))
     ((not (tramp-rpc--process-write-queue-current-p state))
      (tramp-rpc--process-write-queue-fail state :connection-replaced)
      (tramp-rpc--signal-process-write-failure state))
     (t
      (setf (tramp-rpc-write-queue-pending state)
            (tramp-rpc--fifo-push (tramp-rpc-write-queue-pending state)
                                  (list :vec vec :pid pid :data data)))
      (unless (tramp-rpc-write-queue-writing state)
        (tramp-rpc--process-write-queue queue-key))))))

(defun tramp-rpc--process-write-queue (queue-key)
  "Process the next pending write for QUEUE-KEY."
  (let* ((queue (gethash queue-key tramp-rpc--process-write-queues))
         (pending (and queue (tramp-rpc-write-queue-pending queue))))
    (when (and pending (not (tramp-rpc-write-queue-failure queue)))
      (cond
       ((not (tramp-rpc--process-write-queue-current-p queue))
        (tramp-rpc--process-write-queue-fail queue :connection-replaced))
       ((not (tramp-rpc--process-write-queue-live-p queue))
        (tramp-rpc--process-write-queue-fail queue :connection-unavailable))
       (t
        (let* ((item (caar pending))
               (next (cdar pending))
               (vec (plist-get item :vec))
               (pid (plist-get item :pid))
               (data (plist-get item :data)))
          (setf (tramp-rpc-write-queue-pending queue)
                (when next (cons next (cdr pending)))
                (tramp-rpc-write-queue-current queue) data
                (tramp-rpc-write-queue-writing queue) t)
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
                     (tramp-rpc--process-write-queue-fail q :connection-replaced))
                    ((eql (tramp-rpc-protocol-error-code response)
                          tramp-rpc-protocol-error-server-busy)
                     ;; Admission rejected the write before it reached the
                     ;; child.  Send it again first, keeping `writing' set so
                     ;; later writes stay queued behind it.
                     (setf (tramp-rpc-write-queue-pending q)
                           (tramp-rpc--fifo-push-front
                            (tramp-rpc-write-queue-pending q) item)
                           (tramp-rpc-write-queue-current q) nil)
                     (run-at-time tramp-rpc--server-busy-retry-delay nil
                                  #'tramp-rpc--process-write-queue queue-key))
                    ((plist-get response :error)
                     (tramp-rpc--process-write-queue-fail
                      q :rpc-error (plist-get response :error)))
                    (t
                     (setf (tramp-rpc-write-queue-current q) nil
                           (tramp-rpc-write-queue-writing q) nil)
                     (tramp-rpc--process-write-queue queue-key)))))
               (tramp-rpc-write-queue-connection queue))
            (error
             (when-let* ((q (gethash queue-key tramp-rpc--process-write-queues)))
               (tramp-rpc--process-write-queue-fail q :send-error err))
             (signal (car err) (cdr err))))))))))

(defun tramp-rpc--drain-write-queue (vec pid &optional owner-process)
  "Wait for VEC and PID's exact captured write queue to complete.
OWNER-PROCESS owns the request."
  (let* ((queue-key (tramp-rpc--process-write-queue-key-for-owner
                     vec pid owner-process))
         (deadline (+ (float-time) tramp-rpc-write-queue-drain-timeout)))
    (while (let ((queue (gethash queue-key tramp-rpc--process-write-queues)))
             (and queue (not (tramp-rpc-write-queue-failure queue))
                  (tramp-rpc--process-write-queue-current-p queue)
                  (tramp-rpc--process-write-queue-live-p queue)
                  (or (tramp-rpc-write-queue-writing queue)
                      (tramp-rpc-write-queue-pending queue))
                  (< (float-time) deadline)))
      (accept-process-output nil 0.01))
    (when-let* ((queue (gethash queue-key tramp-rpc--process-write-queues)))
      (cond
       ((not (tramp-rpc--process-write-queue-current-p queue))
        (tramp-rpc--process-write-queue-fail queue :connection-replaced))
       ((not (tramp-rpc--process-write-queue-live-p queue))
        (tramp-rpc--process-write-queue-fail queue :connection-unavailable))
       ((or (tramp-rpc-write-queue-writing queue)
            (tramp-rpc-write-queue-pending queue))
        (tramp-rpc--process-write-queue-fail queue :timeout)))
      (tramp-rpc--signal-process-write-failure queue))))

(defun tramp-rpc--process-error-kind (err)
  "Return the structured process error kind carried by condition ERR."
  (when (eq (car err) 'remote-file-error)
    (let ((data (car (last (cdr err)))))
      (and (listp data) (alist-get 'process_error data)))))

(defun tramp-rpc--close-remote-stdin (vec pid &optional owner-process)
  "Close stdin on OWNER-PROCESS's original RPC connection generation.
VEC is the TRAMP connection vector.
PID is the remote process ID."
  (let* ((queue-key (tramp-rpc--process-write-queue-key-for-owner
                     vec pid owner-process))
         (queue (gethash queue-key tramp-rpc--process-write-queues))
         (connection (or (and queue (tramp-rpc-write-queue-connection queue))
                         (tramp-rpc--owner-connection owner-process)
                         (tramp-rpc--ensure-connection vec))))
    ;; The drain reports a replaced connection itself.  Do not store QUEUE
    ;; again: cleanup may have removed it while the drain waited.
    (tramp-rpc--drain-write-queue vec pid owner-process)
    (unless (eq (tramp-rpc-connection-process connection)
                (tramp-rpc--connection-transport (tramp-rpc--get-connection vec)))
      (tramp-rpc--signal-process-write-failure
       (list :failure (list :reason :connection-replaced
                            :pending-bytes 0))))
    (condition-case err
        (tramp-rpc--call vec "process.close_stdin" `((pid . ,pid)) connection)
      (error
       ;; Closing stdin is idempotent: the remote process may be reaped after
       ;; the successful queue drain but before this RPC reaches the server.
       ;; Match the server's structured subtype so diagnostic wording can change
       ;; without turning that benign race into a client-visible failure.
       (unless (equal (tramp-rpc--process-error-kind err) "not_found")
         (signal (car err) (cdr err)))))))

(defun tramp-rpc--kill-remote-process (vec pid &optional signal connection)
  "Send SIGNAL to remote process PID on VEC through CONNECTION."
  (tramp-rpc--call vec "process.kill"
                   `((pid . ,pid)
                     (signal . ,(or signal 15)))
                   connection)) ; SIGTERM

;; ============================================================================
;; Server-pushed Process Output (for LSP and interactive processes)
;; ============================================================================

(defvar tramp-rpc--process-starts 0
  "Number of managed process starts awaiting local registration.")

(defvar tramp-rpc--early-process-notifications nil
  "Notifications for PIDs whose start is not registered yet, newest first.
Entries are (TRANSPORT PID METHOD . PARAMS).")

(defmacro tramp-rpc--with-process-start (&rest body)
  "Run BODY to start a managed process and register its relay.
The server pushes output as soon as the start response is written, so that
output can be handled together with the response, before BODY registers the
relay.  Notifications for unknown PIDs are therefore kept while BODY runs
and claimed by `tramp-rpc--register-managed-process'."
  (declare (indent 0) (debug t))
  `(progn
     (cl-incf tramp-rpc--process-starts)
     (unwind-protect
         (progn ,@body)
       (when (zerop (cl-decf tramp-rpc--process-starts))
         (setq tramp-rpc--early-process-notifications nil)))))

(defun tramp-rpc--register-managed-process (local-process)
  "Save LOCAL-PROCESS's remote OS and deliver its early notifications."
  ;; Keep the OS on the relay: teardown can clear the connection cache before
  ;; its sentinel runs.  The startup system.info is already cached, so no RPC
  ;; is needed while registering or reporting an exit.
  (when-let* ((remote (tramp-rpc--remote-process local-process))
              (vec (tramp-rpc-remote-process-vec remote)))
    (setf (tramp-rpc-remote-process-remote-os remote)
          (alist-get 'os (tramp-rpc--cached-system-info vec))))
  (let* ((remote (tramp-rpc--remote-process local-process))
         (transport (tramp-rpc--connection-transport
                     (tramp-rpc-remote-process-connection remote)))
         (pid (tramp-rpc-remote-process-pid remote))
         matched remaining)
    ;; Partition early notifications in O(N), preserving oldest-first delivery.
    (dolist (entry tramp-rpc--early-process-notifications)
      (if (and (eq (nth 0 entry) transport) (eql (nth 1 entry) pid))
          (push entry matched)
        (push entry remaining)))
    (setq tramp-rpc--early-process-notifications (nreverse remaining))
    (dolist (entry matched)
      (tramp-rpc--dispatch-process-notification
       local-process (nth 2 entry) (nthcdr 3 entry)))
    ;; Transport teardown may have run while process.start was waiting, before
    ;; there was a relay to clean up.  Do not leave that late registration live.
    (when (tramp-rpc--transport-dead-p transport)
      ;; A final notification received with the start response is still valid.
      ;; Deliver it before deciding whether connection death killed the child.
      (tramp-rpc--deliver-pending-output local-process)
      ;; Only this registration missed the transport teardown.  Others on the
      ;; transport were handled then; a PTY whose exit is recorded may still
      ;; be draining its final output.
      (unless (tramp-rpc-remote-process-remote-exited remote)
        (tramp-rpc--release-managed-process local-process remote)
        (tramp-rpc--forget-write-queue local-process remote)))))

(defun tramp-rpc--find-notification-process (connection pid)
  "Find the live relay for remote PID on exact CONNECTION generation in O(1)."
  (when (and (processp connection) (integerp pid))
    (let ((process (gethash (cons connection pid) tramp-rpc--pid-index)))
      (when (and process (process-live-p process))
        process))))

(defun tramp-rpc--handle-process-output-notification (local-process params)
  "Queue output PARAMS for pipe relay LOCAL-PROCESS."
  (when-let* ((info (gethash local-process tramp-rpc--managed-processes)))
    (let ((stdout (when-let* ((value (alist-get 'stdout params)))
                    (tramp-rpc--binary-bytes value)))
          (stderr (when-let* ((value (alist-get 'stderr params)))
                    (tramp-rpc--binary-bytes value))))
      (when (or stdout stderr)
        (tramp-rpc--queue-process-output
         local-process stdout stderr (tramp-rpc-remote-process-stderr-buffer info))))))

(defun tramp-rpc--deliver-process-output (local-process stdout stderr stderr-buffer)
  "Deliver STDOUT and STDERR to LOCAL-PROCESS.
Writes to the local cat relay process, which triggers proper I/O events
that satisfy function `accept-process-output'.
STDERR-BUFFER is the separate stderr buffer, or nil to mix with stdout."
  (when (and (processp local-process) (process-live-p local-process))
    ;; Feed cat directly, generating the real I/O events needed by
    ;; `accept-process-output' without redirecting nested client writes.
    (when (and stdout (> (length stdout) 0))
      (tramp-rpc--debug "DELIVER stdout %d bytes to %s" (length stdout) local-process)
      (tramp-rpc--send-local-relay-string local-process stdout))
    (when (and stderr (> (length stderr) 0))
      (tramp-rpc--debug "DELIVER stderr %d bytes" (length stderr))
      (let ((stderr-process
             (when stderr-buffer
               (when-let* ((remote (gethash local-process
                                            tramp-rpc--managed-processes)))
                 (tramp-rpc-remote-process-stderr-process remote)))))
        (tramp-rpc--send-local-relay-string
         (if (and stderr-process (process-live-p stderr-process))
             stderr-process
           local-process)
         stderr)))))

(defun tramp-rpc--take-pending (local-process key)
  "Remove and return the first item of LOCAL-PROCESS's KEY queue in O(1).
The :pending-exit slot holds a single item rather than a FIFO."
  (when-let* ((remote (gethash local-process tramp-rpc--managed-processes)))
    (pcase key
      (:pending-output
       (when-let* ((fifo (tramp-rpc-remote-process-pending-output remote)))
         (let ((item (caar fifo))
               (next (cdar fifo)))
           (setf (tramp-rpc-remote-process-pending-output remote)
                 (when next (cons next (cdr fifo))))
           item)))
      (:pending-exit
       (when-let* ((pending (tramp-rpc-remote-process-pending-exit remote)))
         (setf (tramp-rpc-remote-process-pending-exit remote) nil)
         pending)))))

(defun tramp-rpc--deliver-pending (local-process deliver finish)
  "Deliver LOCAL-PROCESS's queued output in order, then its exit.
DELIVER is called with each queued output chunk and FINISH with the queued
exit.  Writing a chunk to a full relay pipe lets Emacs run timers and
filters, which can queue newer chunks and call this again for the same
process.  Only the outermost call delivers: it takes one chunk at a time,
so newer chunks still follow older ones, and the exit, which closes the
relay, waits until every chunk is written."
  (when-let* ((remote (gethash local-process tramp-rpc--managed-processes))
              ((not (tramp-rpc-remote-process-delivering remote))))
    (setf (tramp-rpc-remote-process-delivering remote) t)
    (unwind-protect
        (let (item)
          (while (setq item (tramp-rpc--take-pending
                             local-process :pending-output))
            (funcall deliver item))
          (when (setq item (tramp-rpc--take-pending
                            local-process :pending-exit))
            (funcall finish item)))
      (when-let* ((current (gethash local-process tramp-rpc--managed-processes)))
        (setf (tramp-rpc-remote-process-delivering current) nil)))))

(defun tramp-rpc--deliver-pending-output (local-process)
  "Deliver queued output and then any exit for pipe or PTY LOCAL-PROCESS."
  (when-let* ((remote (gethash local-process tramp-rpc--managed-processes)))
    (if (tramp-rpc-remote-process-pty remote)
        (tramp-rpc--deliver-pending
         local-process
         (lambda (output)
           (when (process-live-p local-process)
             (tramp-rpc--send-local-relay-string local-process output)))
         (lambda (pending-exit)
           (apply #'tramp-rpc--handle-pty-exit local-process (cdr pending-exit))))
      (tramp-rpc--deliver-pending
       local-process
       (lambda (chunk)
         (apply #'tramp-rpc--deliver-process-output local-process chunk))
       (lambda (pending-exit)
         (accept-process-output local-process 0.01 nil t)
         (apply #'tramp-rpc--handle-process-exit local-process
                (cdr pending-exit)))))))

(defun tramp-rpc--queue-process-output (local-process stdout stderr stderr-buffer)
  "Queue one non-exit output chunk and schedule its exact delivery once.
LOCAL-PROCESS is the local relay process.
STDOUT is the process standard output.
STDERR is the process standard error output.
STDERR-BUFFER receives standard error output."
  (when-let* ((remote (gethash local-process tramp-rpc--managed-processes)))
    (setf (tramp-rpc-remote-process-pending-output remote)
          (tramp-rpc--fifo-push (tramp-rpc-remote-process-pending-output remote)
                                (list stdout stderr stderr-buffer)))
    (unless (tramp-rpc-remote-process-delivery-timer remote)
      (tramp-rpc--schedule-process-timer
       local-process #'tramp-rpc--deliver-pending-output local-process))))

(defun tramp-rpc--queue-process-exit (local-process exit-code &optional exit-signal)
  "Queue EXIT-CODE after all pending output for LOCAL-PROCESS.
EXIT-SIGNAL is the signal number that terminated the process, if any."
  (when-let* ((remote (gethash local-process tramp-rpc--managed-processes)))
    (setf (tramp-rpc-remote-process-pending-exit remote)
          (list 'exit (if (integerp exit-code) exit-code -1) exit-signal))
    (unless (tramp-rpc-remote-process-delivery-timer remote)
      (tramp-rpc--schedule-process-timer
       local-process #'tramp-rpc--deliver-pending-output local-process))))

(defun tramp-rpc--call-user-sentinel-once (process sentinel event)
  "Call SENTINEL for PROCESS once, recording that it was called."
  (when sentinel
    (let ((remote (tramp-rpc--ensure-remote-process process)))
      (unless (tramp-rpc-remote-process-user-sentinel-called remote)
        (setf (tramp-rpc-remote-process-user-sentinel-called remote) t)
        (funcall sentinel process event)))))

(defun tramp-rpc--pipe-process-sentinel (proc event &optional user-sentinel)
  "Sentinel for pipe relay PROC, preserving USER-SENTINEL exactly once.
The relay remains tracked while its sentinel runs; cleanup removes it after
that chain has completed.
EVENT is the process event string."
  (when-let* (((memq (process-status proc) '(exit signal)))
              (info (gethash proc tramp-rpc--managed-processes))
              ((not (tramp-rpc-remote-process-pty info))))
    (let ((vec (tramp-rpc-remote-process-vec info))
          (pid (tramp-rpc-remote-process-pid info))
          (connection (tramp-rpc--connection-transport
                       (tramp-rpc-remote-process-connection info))))
      (tramp-rpc--cancel-process-timers proc)
      ;; A relay killed or deleted locally never sees the remote exit,
      ;; which otherwise removes the queue.
      (tramp-rpc--forget-write-queue proc info)
      (let ((kill-remote
             (not (or (tramp-rpc-remote-process-exited info)
                      (tramp-rpc-remote-process-remote-exited info)
                      (tramp-rpc-remote-process-transport-cleanup info)
                      (tramp-rpc--transport-dead-p connection)))))
        ;; Mark the relay terminal before waiting for the remote kill, so a
        ;; sentinel call nested in that wait does not kill it again.
        (setf (tramp-rpc-remote-process-exited info) t)
        (when (and kill-remote vec pid)
          (tramp-rpc--best-effort
            (tramp-rpc--kill-remote-process
             vec pid 9 (tramp-rpc-remote-process-connection info)))))
      (when-let* ((stderr-process (tramp-rpc-remote-process-stderr-process info)))
        (when (process-live-p stderr-process)
          (tramp-rpc--best-effort (delete-process stderr-process))))
      (tramp-rpc--call-user-sentinel-once
       proc user-sentinel (tramp-rpc--remote-exit-event proc event))
      (tramp-rpc--clear-delivery-state proc)
      (tramp-rpc--unindex-managed-process proc info))))

(defun tramp-rpc--remote-exit-event (process fallback)
  "Return the sentinel event for the remote exit of PROCESS.
FALLBACK is used when the remote status is unknown.  Signal deaths use the
lowercase signal description, like local processes (\"killed\\n\")."
  (let* ((remote (tramp-rpc--remote-process process))
         (exit-signal (and remote (tramp-rpc-remote-process-exit-signal remote)))
         (exit-code (and remote (tramp-rpc-remote-process-exit-code remote))))
    (cond
     ((integerp exit-signal)
      (format "%s\n" (downcase (tramp-rpc-protocol-signal-description
                                 exit-signal
                                 (tramp-rpc-remote-process-remote-os remote)))))
     ((null exit-code) fallback)
     ((= exit-code 0) "finished\n")
     (t (format "exited abnormally with code %d\n" exit-code)))))

(defun tramp-rpc--handle-process-exit (local-process exit-code &optional exit-signal)
  "Handle exit of remote process associated with LOCAL-PROCESS.
Stores the remote exit code and sends EOF to the local cat relay so
it flushes remaining output and exits naturally.  The process sentinel
\(`tramp-rpc--pipe-process-sentinel') fires when cat exits, handles
cleanup, and calls the user's sentinel with the correct event string.

This design follows TRAMP's approach: let Emacs's process machinery
handle sentinel dispatch rather than fighting it with `delete-process'
+ deferred `run-at-time' sentinel calls.  Doing `delete-process'
before the cat relay drains its pipe causes a stale FD that makes
`input-pending-p' return t permanently, starving keyboard input.
EXIT-CODE is the process exit status and EXIT-SIGNAL the terminating signal
number, if any."
  (when-let* ((info (gethash local-process tramp-rpc--managed-processes))
              ((not (tramp-rpc-remote-process-pty info))))
    (tramp-rpc--cancel-process-timers local-process)
    (tramp-rpc--forget-write-queue local-process info)
    (setf (tramp-rpc-remote-process-exit-code info) (or exit-code 0)
          (tramp-rpc-remote-process-exit-signal info)
          (and (natnump exit-signal) exit-signal)
          (tramp-rpc-remote-process-remote-exited info) t)
    ;; Finish a separate stderr relay before closing stdout, so the public
    ;; process cannot report exit while final stderr bytes remain queued.
    (if-let* ((stderr-process (tramp-rpc-remote-process-stderr-process info))
              ((process-live-p stderr-process)))
        (let ((old-sentinel (process-sentinel stderr-process)))
          (set-process-sentinel
           stderr-process
           (lambda (process event)
             (when (memq (process-status process) '(exit signal))
               (tramp-rpc--close-local-relay local-process))
             (when old-sentinel
               (tramp-rpc--best-effort
                 (funcall old-sentinel process event)))))
          (condition-case nil
              (process-send-eof stderr-process)
            (error (tramp-rpc--close-local-relay local-process))))
      (tramp-rpc--close-local-relay local-process))))

(defun tramp-rpc--close-local-relay (local-process)
  "Send local EOF to LOCAL-PROCESS and let its output drain naturally."
  (when (process-live-p local-process)
    (tramp-rpc--best-effort
      (let ((tramp-rpc--bypass-next-relay-call local-process))
        (process-send-eof local-process)))))

;; ============================================================================
;; Sentinels
;; ============================================================================

(defun tramp-rpc--install-own-sentinel (process sentinel)
  "Install tramp-rpc's SENTINEL on relay PROCESS for its whole lifetime.
Callers such as `vc-do-command' and `compile' set the sentinel after the
process has started.  Once SENTINEL is installed, `set-process-sentinel' and
`process-sentinel' on PROCESS reach the caller's sentinel kept in its
remote-process state instead, which SENTINEL calls with the remote
exit status."
  (set-process-sentinel process sentinel)
  (setf (tramp-rpc-remote-process-own-sentinel (tramp-rpc--ensure-remote-process process)) sentinel))

(defun tramp-rpc--pipe-relay-sentinel (process event)
  "Sentinel of pipe relay PROCESS for local EVENT.
After the sentinel has run, delete the exited relay.  Otherwise
`get-buffer-process' keeps returning it, and `vc-dir-busy' reports an
update in progress."
  (tramp-rpc--pipe-process-sentinel
   process event (tramp-rpc--user-sentinel process))
  (when (memq (process-status process) '(exit signal))
    (run-at-time 0 nil (lambda ()
                         (unless (process-live-p process)
                           (tramp-rpc--best-effort (delete-process process)))))))

;; ============================================================================
;; Coding helper
;; ============================================================================

(defun tramp-rpc--coding-args (coding)
  "Return list of arguments for `set-process-coding-system' from CODING.
CODING is a non-nil `make-process' :coding value: either a symbol
\(used for both decoding and encoding) or a cons cell (DECODING .
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
\(DECODING . ENCODING).  Nil sides in a cons are replaced with the
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
option like `-l' precedes it (an invalid-option error).
VEC is the TRAMP connection vector."
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
             (equal base
                    (condition-case err
                        (file-name-nondirectory
                         (tramp-rpc--get-remote-login-shell vec))
                      ((file-error remote-file-error)
                       (tramp-rpc--debug "login shell probe failed: %s"
                                         (error-message-string err))
                       nil))))
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

A remote string `:stderr' value retains TRAMP's same-remote file semantics.
For compatibility, a non-remote string names an Emacs buffer."
  (let ((args (copy-sequence args)))
    (when (and (plist-member args :connection-type)
               (null (plist-get args :connection-type)))
      (setq args (plist-put args :connection-type 'pipe)))
    (when-let* ((stderr (plist-get args :stderr)))
      (when (and (stringp stderr)
                 (not (tramp-tramp-file-p stderr)))
        (setq args (plist-put args :stderr (get-buffer-create stderr)))))
    args))

(defun tramp-rpc--redirect-command-stderr (command stderr-file)
  "Return COMMAND wrapped to redirect stderr to remote STDERR-FILE safely."
  (append
   (list "/bin/sh" "-c"
         (format "exec \"$@\" 2>%s"
                 (tramp-shell-quote-argument stderr-file))
         "tramp-rpc-stderr")
   command))

(defun tramp-rpc-handle-make-process (&rest args)
  "Create an async process on the remote host.
ARGS are keyword arguments as per `make-process'.
Supports PTY allocation when :connection-type is \='pty or t,
or when `process-connection-type' is t.
For pipe mode, the server pushes output as it arrives.
Resolves program path and loads direnv environment from working directory."
  (let ((args (tramp-rpc--make-process-skeleton-args args)))
    ;; Reuse TRAMP's make-process skeleton for type checks, unique process names,
    ;; buffer setup, stderr validation, and TRAMP file-name parsing.  The body
    ;; below is only the RPC-specific process creation path.
    (tramp-skeleton-make-process args nil t
      ;; The skeleton checks the list, not its program.  RPC cannot create a
      ;; commandless PTY, even when Emacs's shell quoting accepts nil.
      (unless (stringp (car command))
        (signal 'wrong-type-argument (list #'stringp (car command))))
      ;; Unquote localname in case of file-name-quoted paths (e.g. /: prefix),
      ;; and treat an empty one as the remote home directory.
      (setq localname (tramp-rpc--process-directory localname))
      (let* ((sudo-command (and (car command)
                                (string= (file-name-nondirectory (car command))
                                         "sudo")))
             ;; The skeleton normalizes t to `pty'.  `pipe' and nil are both
             ;; non-PTY from the RPC process launcher's perspective.
             (use-pty (eq connection-type 'pty))
             (pipe-sudo (and sudo-command (not use-pty)))
             (stderr-file (when (stringp stderr)
                            (tramp-file-local-name
                             (expand-file-name stderr))))
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
          (let ((ssh-user (tramp-rpc--sudo-ssh-user v)))
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
        (when (and use-pty sudo-command stderr-file)
          (tramp-error
           v 'file-error
           "Cannot redirect stderr for an interactive PTY sudo process"))
        (when use-pty
          (setq command (tramp-rpc--maybe-login-shell-command v command)))
        ;; TRAMP string :stderr means a same-remote file, in both pipe and PTY
        ;; modes.  Redirect in one argv-safe shell wrapper; buffer stderr keeps
        ;; using the separate local relay below.  PTY sudo is rejected above
        ;; because redirecting its prompt would leave the user waiting blindly.
        (when stderr-file
          (setq command (tramp-rpc--redirect-command-stderr command stderr-file)
                stderr nil))
        ;; Use the same effective environment as synchronous and parallel RPC
        ;; processes, including configured PATH, direnv, and caller overrides.
        (let ((process-env (tramp-rpc--process-environment v localname)))
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
            (tramp-rpc--with-process-start
              (let* ((program (car command))
                     (program-args (cdr command))
                     (connection (tramp-rpc--ensure-connection v))
                     (remote-pid (tramp-rpc--start-remote-process
                                  v program program-args localname process-env
                                  connection))
                     (stderr-buffer (cond
                                     ((bufferp stderr) stderr)
                                     ((stringp stderr) (get-buffer-create stderr))
                                     (t nil)))
                     ;; Construct both local relays as one transaction.  If the
                     ;; second `cat' fails, tear down the first and the remote
                     ;; process rather than leaving either orphaned.
                     (relays
                      (tramp-rpc--start-cat-relays
                       (or name "tramp-rpc-async") buffer stderr-buffer
                       (lambda ()
                         (tramp-rpc--best-effort
                           (tramp-rpc--kill-remote-process
                            v remote-pid 9 connection)))))
                     (local-process (car relays))
                     (stderr-process (cdr relays)))

                ;; The public pair controls user input encoding.  The local
                ;; relay itself reads with the requested decoder and writes
                ;; binary remote bytes without another encoding pass.
                (tramp-rpc--configure-relay-coding local-process coding)
                (when stderr-process
                  (tramp-rpc--configure-relay-coding stderr-process coding))
                (set-process-query-on-exit-flag local-process (not noquery))

                (tramp-rpc--track-managed-process
                 local-process v remote-pid connection nil stderr-buffer stderr-process)

                ;; Feed sudo's stdin password before exposing the relay to callers;
                ;; subsequent writes use the same queue and stay ordered after it.
                (when sudo-password
                  (condition-case err
                      (tramp-rpc--write-remote-process
                       v remote-pid (tramp-rpc--encode-process-input
                                     local-process (concat sudo-password "\n"))
                       local-process)
                    ((error quit)
                     (tramp-rpc--forget-managed-process local-process)
                     (remhash (tramp-rpc-remote-process-write-queue-key
                               (tramp-rpc--remote-process local-process))
                              tramp-rpc--process-write-queues)
                     (dolist (relay (list (car relays) (cdr relays)))
                       (when (processp relay)
                         (set-process-sentinel relay #'ignore)
                         (tramp-rpc--best-effort (delete-process relay))))
                     (tramp-rpc--best-effort
                       (tramp-rpc--kill-remote-process v remote-pid 9 connection))
                     (signal (car err) (cdr err)))))
                (process-put local-process 'tramp-vector v)
                (process-put local-process 'remote-command orig-command)

                (when filter
                  (set-process-filter local-process filter))
                (setf (tramp-rpc-remote-process-user-sentinel
                       (tramp-rpc--ensure-remote-process local-process))
                      sentinel)
                (tramp-rpc--install-own-sentinel
                 local-process #'tramp-rpc--pipe-relay-sentinel)

                (tramp-rpc--debug
                  "MAKE-PROCESS created local=%s remote-pid=%s program=%s"
                  local-process remote-pid program)

                (tramp-rpc--register-managed-process local-process)

                local-process))))))))

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
                                         filter sentinel localname &optional process-env)
  "Create a PTY-based process for terminal emulators.
VEC is the tramp connection vector.
NAME, BUFFER, COMMAND, CODING, NOQUERY, FILTER, SENTINEL are process params.
LOCALNAME is the remote working directory.
PROCESS-ENV is the effective child environment alist built by
`tramp-rpc--process-environment' (configured PATH, direnv, caller overrides).

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
       vec name buffer command coding noquery filter sentinel localname process-env)
    ;; Use RPC-based PTY
    (tramp-rpc--make-rpc-pty-process
     vec name buffer command coding noquery filter sentinel localname process-env)))

(defun tramp-rpc--direct-ssh-pty-sentinel (process event)
  "Remove direct SSH PTY PROCESS and invoke its user sentinel once.
EVENT is the process event string."
  (when (memq (process-status process) '(exit signal))
    (tramp-rpc--unindex-managed-process process)
    (tramp-rpc--call-user-sentinel-once
     process (tramp-rpc--user-sentinel process) event)))

(defun tramp-rpc--make-direct-ssh-pty-process (vec name buffer command coding noquery
                                                    filter sentinel localname &optional process-env)
  "Create a PTY process using direct SSH connection.
This provides much lower latency than the RPC-based PTY by using a direct
SSH connection with `-t` for the terminal.  The SSH connection reuses the
existing ControlMaster socket, so authentication is already handled.

VEC is the tramp connection vector.
NAME, BUFFER, COMMAND, CODING, NOQUERY, FILTER, SENTINEL are process params.
LOCALNAME is the remote working directory.
PROCESS-ENV is the effective child environment alist built by
`tramp-rpc--process-environment'."
  (let* ((host (tramp-file-name-host vec))
         (program (car command))
         (program-args (cdr command))
         ;; Build environment exports for the remote command
         (env-exports (mapconcat
                       (lambda (pair)
                         (format "export %s=%s;"
                                 (car pair)
                                 (tramp-shell-quote-argument (cdr pair))))
                       (append process-env
                               `(("TERM" . ,(or (getenv "TERM") "xterm-256color"))))
                       " "))
         ;; Build the remote command - cd to dir, export env, exec program
         ;; Leave a leading "~" unquoted so the remote shell expands it.
         (remote-cmd (format "cd %s && %s exec %s %s"
                             (if (string-match
                                  (rx bos "~" (? (group "/" (* anychar))) eos)
                                  localname)
                                 (concat "~" (when (match-beginning 1)
                                               (tramp-shell-quote-argument
                                                (match-string 1 localname))))
                               (tramp-shell-quote-argument localname))
                             env-exports
                             (tramp-shell-quote-argument program)
                             (mapconcat #'tramp-shell-quote-argument program-args " ")))
         ;; Build SSH arguments via the shared transport builder so
         ;; ControlMaster, ProxyJump, and user SSH option precedence stay unified.
         (ssh-args (tramp-rpc--build-ssh-args
                    vec
                    ;; Keep SSH's "Shared connection ... closed" notice
                    ;; out of the terminal whatever the user's LogLevel.
                    :leading-options '("LogLevel=error")
                    :batch-mode tramp-rpc-use-controlmaster
                    :controlmaster (and tramp-rpc-use-controlmaster 'auto)
                    :extra-args (list "-t" "-t")
                    :remote-command (list remote-cmd)))
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

    ;; Normal exits must remove PTY tracking.
    (tramp-rpc--install-own-sentinel process #'tramp-rpc--direct-ssh-pty-sentinel)

    ;; A direct SSH PTY has no remote PID; its connection is only the
    ;; generation live at creation, used to scope transport cleanup.
    (tramp-rpc--track-managed-process
     process vec nil (tramp-rpc--get-connection vec) t nil nil t)
    (let ((remote (tramp-rpc--remote-process process)))
      (setf (tramp-rpc-remote-process-user-sentinel remote) sentinel
            (tramp-rpc-remote-process-command remote) command))
    ;; Standard tramp property expected by tests and upstream code
    (process-put process 'remote-command command)
    (process-put process 'tramp-vector vec)

    process))

(defun tramp-rpc--make-rpc-pty-process (vec name buffer command coding noquery
                                             filter sentinel localname &optional process-env)
  "Create a PTY-based process using the RPC server.
This is the fallback when direct SSH PTY is disabled.

VEC is the tramp connection vector.
NAME, BUFFER, COMMAND, CODING, NOQUERY, FILTER, SENTINEL are process params.
LOCALNAME is the remote working directory.
PROCESS-ENV is the effective child environment alist built by
`tramp-rpc--process-environment'."
  (tramp-rpc--with-process-start
    (let* ((program (car command))
           (program-args (cdr command))
           ;; Get terminal dimensions from buffer or use defaults
           (size (tramp-rpc--get-terminal-size buffer))
           (rows (cdr size))
           (cols (car size))
           ;; Build environment - add TERM after caller/remote env so it wins.
           (term-env (or (getenv "TERM") "xterm-256color"))
           (full-env (append process-env `(("TERM" . ,term-env))))
           ;; Capture before starting: the wait can invalidate or replace the
           ;; connection registered under VEC.
           (connection (tramp-rpc--ensure-connection vec))
           (result (tramp-rpc--call vec "process.start_pty"
                                    `((cmd . ,program)
                                      (args . ,(vconcat program-args))
                                      (cwd . ,(when localname
                                                (tramp-rpc--path-to-bin localname)))
                                      (rows . ,rows)
                                      (cols . ,cols)
                                      (env . ,full-env)) connection))
           (remote-pid (alist-get 'pid result))
           (tty-name (alist-get 'tty_name result))
           ;; Normalize buffer - it can be t, nil, a buffer, or a string
           (actual-buffer (cond
                           ((bufferp buffer) buffer)
                           ((stringp buffer) (get-buffer-create buffer))
                           ((eq buffer t) (current-buffer))
                           (t nil)))
           ;; Use a local cat relay so Emacs owns incremental decoding of raw
           ;; PTY bytes, exactly as it does for pipe process output.  If local
           ;; construction fails, close the already-created remote PTY through
           ;; the generation that created it.
           (relays
            (tramp-rpc--start-cat-relays
             (or name "tramp-rpc-pty") actual-buffer nil
             (lambda ()
               (tramp-rpc--best-effort
                 (tramp-rpc--call vec "process.close_pty"
                                  `((pid . ,remote-pid)) connection)))))
           (local-process (car relays)))

      ;; Configure the local relay process.  Its write side is binary; the
      ;; public coding pair is retained for process API and input encoding.
      (tramp-rpc--configure-relay-coding local-process coding)
      (set-process-filter local-process (or filter #'tramp-rpc--pty-default-filter))
      (tramp-rpc--install-own-sentinel local-process #'tramp-rpc--pty-sentinel)
      (set-process-query-on-exit-flag local-process (not noquery))

      ;; Store process info
      (tramp-rpc--track-managed-process local-process vec remote-pid connection t)
      (let ((remote (tramp-rpc--remote-process local-process)))
        (setf (tramp-rpc-remote-process-user-sentinel remote) sentinel
              (tramp-rpc-remote-process-command remote) command
              (tramp-rpc-remote-process-tty-name remote) tty-name))
      ;; Standard tramp property expected by tests and upstream code
      (process-put local-process 'remote-command command)
      (process-put local-process 'tramp-vector vec)

      ;; Set up window size adjustment function
      (process-put local-process 'adjust-window-size-function
                   #'tramp-rpc--adjust-pty-window-size)

      (tramp-rpc--register-managed-process local-process)

      local-process)))

(defun tramp-rpc--pty-default-filter (process output)
  "Default filter for PTY processes - insert output into process buffer.
PROCESS is the process being handled.
OUTPUT is the process output received."
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

(defun tramp-rpc--queue-pty-delivery
    (local-process &optional output exit-code exit-p exit-signal)
  "Queue PTY OUTPUT and optional EXIT-CODE for LOCAL-PROCESS.
EXIT-P marks a terminal delivery; EXIT-SIGNAL is the terminating signal."
  (when-let* ((remote (gethash local-process tramp-rpc--managed-processes)))
    (when output
      (setf (tramp-rpc-remote-process-pending-output remote)
            (tramp-rpc--fifo-push
             (tramp-rpc-remote-process-pending-output remote)
             output)))
    (when exit-p
      (setf (tramp-rpc-remote-process-pending-exit remote)
            (list 'exit (if (integerp exit-code) exit-code -1) exit-signal)))
    (unless (tramp-rpc-remote-process-delivery-timer remote)
      (tramp-rpc--schedule-process-timer
       local-process #'tramp-rpc--deliver-pending-output local-process))))

(defun tramp-rpc--handle-pty-exit (local-process exit-code &optional exit-signal)
  "Handle exit of PTY process associated with LOCAL-PROCESS.
EXIT-CODE is the process exit status and EXIT-SIGNAL the terminating signal
number, if any.  The server already released the remote PTY when it reported
the exit."
  (when-let* ((remote (gethash local-process tramp-rpc--managed-processes))
              ((tramp-rpc-remote-process-pty remote)))
    (tramp-rpc--cancel-process-timers local-process)
    ;; A terminal server response without a status is abnormal.  In
    ;; particular, do not translate a killed remote PTY into local success.
    (setf (tramp-rpc-remote-process-exit-code remote)
          (if (integerp exit-code) exit-code -1)
          (tramp-rpc-remote-process-exit-signal remote)
          (and (natnump exit-signal) exit-signal)
          (tramp-rpc-remote-process-remote-exited remote) t)
    ;; Close the relay's local stdin and let cat flush the final PTY bytes
    ;; before exiting naturally.  A zero-timeout `accept-process-output' after
    ;; `process-send-string' does not wait for the relay round trip, while
    ;; deleting it here discards the final output returned with EXITED.
    (tramp-rpc--close-local-relay local-process)))

(defun tramp-rpc--pty-sentinel (process event)
  "Sentinel for PTY relay PROCESS, preserving its user sentinel once.
EVENT is the process event string."
  (when (memq (process-status process) '(exit signal))
    (when-let* ((info (gethash process tramp-rpc--managed-processes))
                ((tramp-rpc-remote-process-pty info)))
      (tramp-rpc--cancel-process-timers process)
      ;; A transport cleanup already requested/closed the remote PTY.
      (let ((kill-remote
             (not (or (tramp-rpc-remote-process-exited info)
                      (tramp-rpc-remote-process-remote-exited info)
                      (tramp-rpc-remote-process-transport-cleanup info)
                      (tramp-rpc--transport-dead-p
                       (tramp-rpc--connection-transport
                        (tramp-rpc-remote-process-connection info)))))))
        ;; As for pipe relays, mark terminal before the synchronous kill wait
        ;; can re-enter this sentinel.
        (setf (tramp-rpc-remote-process-exited info) t)
        (when (and kill-remote
                   (tramp-rpc-remote-process-vec info)
                   (tramp-rpc-remote-process-pid info))
          (tramp-rpc--best-effort
            (tramp-rpc--call (tramp-rpc-remote-process-vec info) "process.kill_pty"
                             `((pid . ,(tramp-rpc-remote-process-pid info))
                               (signal . 9))
                             (tramp-rpc-remote-process-connection info)))))
      (tramp-rpc--call-user-sentinel-once
       process (tramp-rpc--user-sentinel process)
       (tramp-rpc--remote-exit-event process event))
      ;; Run after the wrapped/user sentinel has observed the exit.
      (tramp-rpc--clear-delivery-state process)
      (tramp-rpc--unindex-managed-process process info))))

;; ============================================================================
;; PTY window resize support (non-blocking)
;; ============================================================================

(defconst tramp-rpc--pty-resize-busy-retries 50
  "Number of times a PTY resize rejected as busy is sent again.")

(defun tramp-rpc--send-pty-resize (process remote cols rows &optional attempt)
  "Send one `process.resize_pty' RPC for PROCESS/REMOTE with COLS and ROWS.
Then drain any coalesced resize.  A resize the server rejects as busy did
not run; it is sent again, with the newest pending size, up to
`tramp-rpc--pty-resize-busy-retries' times.  ATTEMPT counts those retries."
  (let* ((vec (tramp-rpc-remote-process-vec remote))
         (pid (tramp-rpc-remote-process-pid remote))
         (connection (tramp-rpc-remote-process-connection remote))
         (transport (and connection (tramp-rpc-connection-process connection))))
    (if (not (and (process-live-p process)
                  (not (tramp-rpc-remote-process-exited remote))
                  (not (tramp-rpc-remote-process-remote-exited remote))
                  (processp transport)
                  (process-live-p transport)))
        (setf (tramp-rpc-remote-process-resize-in-flight remote) nil
              (tramp-rpc-remote-process-pending-resize remote) nil)
      (setf (tramp-rpc-remote-process-resize-in-flight remote) t)
      (condition-case err
          (tramp-rpc--call-async
           vec "process.resize_pty"
           `((pid . ,pid) (cols . ,cols) (rows . ,rows))
           (lambda (response)
             (if (and (eql (tramp-rpc-protocol-error-code response)
                           tramp-rpc-protocol-error-server-busy)
                      (< (or attempt 0) tramp-rpc--pty-resize-busy-retries))
                 ;; Stay in flight so newer sizes keep coalescing, and send
                 ;; whichever size is newest when the retry fires.
                 (run-at-time
                  tramp-rpc--server-busy-retry-delay nil
                  (lambda ()
                    (let ((size (or (tramp-rpc-remote-process-pending-resize remote)
                                    (cons cols rows))))
                      (setf (tramp-rpc-remote-process-pending-resize remote) nil)
                      (tramp-rpc--send-pty-resize
                       process remote (car size) (cdr size)
                       (1+ (or attempt 0))))))
               (when-let* ((rpc-err (plist-get response :error)))
                 (tramp-rpc--debug "PTY resize failed: %S" rpc-err))
               (setf (tramp-rpc-remote-process-resize-in-flight remote) nil)
               (when-let* ((next (tramp-rpc-remote-process-pending-resize remote)))
                 (setf (tramp-rpc-remote-process-pending-resize remote) nil)
                 (tramp-rpc--send-pty-resize
                  process remote (car next) (cdr next)))))
           connection)
        (error
         (setf (tramp-rpc-remote-process-resize-in-flight remote) nil
               (tramp-rpc-remote-process-pending-resize remote) nil)
         (tramp-rpc--debug "PTY resize send failed: %s"
                           (error-message-string err)))))))

(defun tramp-rpc--resize-pty-async (process remote cols rows)
  "Send an asynchronous `process.resize_pty' RPC for PROCESS/REMOTE.
COLS and ROWS are the new PTY dimensions.  Never blocks Emacs redisplay
or `window-size' adjustment hooks.  Keeps at
most one resize RPC in flight per PTY and coalesces newer dimensions into
`pending-resize' so out-of-order server execution cannot apply a stale size."
  (if (tramp-rpc-remote-process-resize-in-flight remote)
      (setf (tramp-rpc-remote-process-pending-resize remote) (cons cols rows))
    (tramp-rpc--send-pty-resize process remote cols rows)))

(defun tramp-rpc--adjust-pty-window-size (process _windows)
  "Adjust PTY window size after an Emacs window resize.
PROCESS is the local relay process, WINDOWS is the list of windows.
Returns nil to tell Emacs not to call `set-process-window-size' on
the local relay process (we handle resizing asynchronously via RPC)."
  (when (process-live-p process)
    (when-let* ((remote (tramp-rpc--remote-process process))
                ((tramp-rpc-remote-process-pty remote))
                ((tramp-rpc-remote-process-vec remote))
                ((tramp-rpc-remote-process-pid remote)))
      (let ((size (tramp-rpc--get-terminal-size (process-buffer process))))
        (tramp-rpc--resize-pty-async process remote (car size) (cdr size)))))
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
    (when-let* ((remote (tramp-rpc--remote-process process))
                ((tramp-rpc-remote-process-vec remote))
                ((tramp-rpc-remote-process-pid remote))
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
                (tramp-rpc--resize-pty-async process remote width height)
                ;; Let terminal-specific code update display
                (when display-updater
                  (funcall display-updater width height))
                (cons width height)))))))))

(defun tramp-rpc--vterm-window-adjust-advice (orig-fun process windows)
  "Resize the remote PTY of TRAMP-RPC PROCESS and update vterm's display.
Call ORIG-FUN, vterm's window adjust function, for other processes and
direct SSH PTYs, whose resize SSH handles.
WINDOWS is the list of windows displaying the process buffer."
  (cond
   ;; RPC-based PTY - resize via RPC
   ((tramp-rpc--rpc-pty-process-p process)
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
         (when-let* (((boundp 'vterm--term))
                     (term (symbol-value 'vterm--term))
                     ((fboundp 'vterm--set-size)))
           (funcall (symbol-function 'vterm--set-size)
                    term height width))))))
   (t (funcall orig-fun process windows))))

(defun tramp-rpc--eat-window-adjust-advice (orig-fun process windows)
  "Resize the remote PTY of TRAMP-RPC PROCESS and update eat's display.
Call ORIG-FUN, eat's window adjust function, for other processes and
direct SSH PTYs, whose resize SSH handles.
WINDOWS is the list of windows displaying the process buffer."
  (cond
   ;; RPC-based PTY - resize via RPC
   ((tramp-rpc--rpc-pty-process-p process)
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
   (t (funcall orig-fun process windows))))

;; ============================================================================
;; Process cleanup (unified for pipe and PTY relays)
;; ============================================================================

(defun tramp-rpc--terminate-managed-processes (vec connection-process connection)
  "Terminate remote pipe and RPC PTY processes for VEC and CONNECTION-PROCESS.
CONNECTION is the captured transport generation and remains usable after that
generation has been detached from global connection lookup.
Collect targets before issuing RPCs because processing a response may run
callbacks that mutate the process registry."
  (let ((target-key (tramp-rpc--connection-key vec))
        targets)
    (maphash
     (lambda (_local-process info)
       (when (and (not (tramp-rpc-remote-process-direct-ssh info))
                  (tramp-rpc-remote-process-pid info)
                  (equal (tramp-rpc--connection-key
                          (tramp-rpc-remote-process-vec info))
                         target-key)
                  (eq connection-process
                      (tramp-rpc--connection-transport
                       (tramp-rpc-remote-process-connection info))))
         (push (list (tramp-rpc-remote-process-vec info)
                     (tramp-rpc-remote-process-pid info)
                     (tramp-rpc-remote-process-pty info))
               targets)))
     tramp-rpc--managed-processes)
    (dolist (target targets)
      (pcase-let ((`(,tvec ,pid ,pty) target))
        (tramp-rpc--best-effort
          (tramp-rpc--call tvec (if pty "process.kill_pty" "process.kill")
                           `((pid . ,pid) (signal . 9)) connection))))))

(defun tramp-rpc--cleanup-process-write-queues (&optional vec connection-process)
  "Remove write queues for VEC and CONNECTION-PROCESS.
When CONNECTION-PROCESS is non-nil, remove only that exact connection
generation.  With VEC nil, remove all queues."
  (let ((target-key (and vec (tramp-rpc--connection-key vec)))
        stale)
    (maphash
     (lambda (key queue)
       (when (and (or (null target-key)
                      (equal (tramp-rpc--connection-key
                              (tramp-rpc-write-queue-vec queue))
                             target-key))
                  (or (null connection-process)
                      (eq (tramp-rpc-write-queue-connection-process queue)
                          connection-process)))
         (push key stale)))
     tramp-rpc--process-write-queues)
    (dolist (key stale)
      (remhash key tramp-rpc--process-write-queues))))

(defun tramp-rpc--cleanup-managed-processes
    (&optional vec connection-process remote-cleanup)
  "Clean up managed pipe and PTY processes for VEC and CONNECTION-PROCESS.
When REMOTE-CLEANUP is non-nil, request remote process termination while the
transport is live.  Independent direct SSH PTYs survive unexpected RPC
transport loss; explicit disconnect and global cleanup still remove them."
  (when (and remote-cleanup vec connection-process
             (process-live-p connection-process))
    (when-let* ((connection (tramp-rpc--get-connection vec)))
      (when (eq connection-process (tramp-rpc-connection-process connection))
        (tramp-rpc--terminate-managed-processes vec connection-process connection))))
  ;; Snapshot matching processes before mutating the hash tables or running
  ;; sentinels via `delete-process'.
  (let* ((target-key (and vec (tramp-rpc--connection-key vec)))
         (conn (and connection-process
                    (tramp-rpc--process-connection connection-process)))
         (reason (and conn (tramp-rpc-connection-cleanup-reason conn)))
         matches)
    (maphash
     (lambda (local-process info)
       (when (and (or (null target-key)
                      (equal (tramp-rpc--connection-key
                              (tramp-rpc-remote-process-vec info))
                             target-key))
                  (or (null connection-process)
                      (eq connection-process
                          (tramp-rpc--connection-transport
                           (tramp-rpc-remote-process-connection info)))))
         (let ((preserve-direct
                (and (tramp-rpc-remote-process-direct-ssh info)
                     (memq reason
                           '(:transport-death :timeout :protocol-error)))))
           (unless preserve-direct
             (push (cons local-process info) matches)))))
     tramp-rpc--managed-processes)
    (dolist (entry matches)
      (tramp-rpc--release-managed-process (car entry) (cdr entry))))
  (tramp-rpc--cleanup-process-write-queues vec connection-process))

(defun tramp-rpc--release-managed-process (local-process info)
  "Delete relay LOCAL-PROCESS and forget its remote state INFO."
  (tramp-rpc--cancel-process-timers local-process)
  (when-let* ((stderr-process (tramp-rpc-remote-process-stderr-process info)))
    (when (process-live-p stderr-process)
      (tramp-rpc--best-effort (delete-process stderr-process))))
  ;; Keep tracking while delete-process dispatches the wrapped sentinel.
  (setf (tramp-rpc-remote-process-transport-cleanup info) t)
  (when (process-live-p local-process)
    (delete-process local-process))
  (tramp-rpc--clear-delivery-state local-process)
  (tramp-rpc--unindex-managed-process local-process info))

;; ============================================================================
;; Unload support
;; ============================================================================

(defun tramp-rpc-process-unload-function ()
  "Unload function for tramp-rpc-process.
Cleans up managed pipe and PTY processes."
  (tramp-rpc--cleanup-managed-processes)
  ;; Return nil to allow normal unload to proceed
  nil)

(defun tramp-rpc--dispatch-process-notification (local-process method params)
  "Apply process notification METHOD with PARAMS to relay LOCAL-PROCESS."
  (when-let* ((info (gethash local-process tramp-rpc--managed-processes)))
    (let ((pty (tramp-rpc-remote-process-pty info)))
      (pcase method
        ("process.output"
         (if pty
             (when-let* ((output (alist-get 'stdout params)))
               (tramp-rpc--queue-pty-delivery
                local-process (tramp-rpc--binary-bytes output)))
           (tramp-rpc--handle-process-output-notification local-process params)))
        ("process.exit"
         (if pty
             (tramp-rpc--queue-pty-delivery
              local-process nil (alist-get 'exit_code params) t
              (alist-get 'signal params))
           (tramp-rpc--queue-process-exit
            local-process (alist-get 'exit_code params)
            (alist-get 'signal params))))))))

(defun tramp-rpc--handle-process-notification (process method params)
  "Dispatch process notification METHOD with PARAMS received on PROCESS.
Notifications for a PID whose start is still being registered are kept for
`tramp-rpc--register-managed-process'; others for unknown PIDs belong to
processes that are already gone."
  (when (member method '("process.output" "process.exit"))
    (let* ((pid (alist-get 'pid params))
           (local-process (tramp-rpc--find-notification-process process pid)))
      (cond
       (local-process
        (tramp-rpc--dispatch-process-notification local-process method params))
       ((> tramp-rpc--process-starts 0)
        (push (cl-list* process pid method params)
              tramp-rpc--early-process-notifications))))))

;; Relay teardown runs from the transport's generation cleanup: remote
;; children are signalled while the transport is still live, local relays
;; are removed once it is dead.  Register these internal callbacks even when
;; this module is loaded standalone, so relays cannot outlive the transport.
(add-hook 'tramp-rpc-transport-terminate-functions
          #'tramp-rpc--terminate-managed-processes t)
(add-hook 'tramp-rpc-transport-cleanup-functions
          #'tramp-rpc--cleanup-managed-processes t)
(add-hook 'tramp-rpc-notification-functions
          #'tramp-rpc--handle-process-notification t)

(provide 'tramp-rpc-process)
;;; tramp-rpc-process.el ends here
