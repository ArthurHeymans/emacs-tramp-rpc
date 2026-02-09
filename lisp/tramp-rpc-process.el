;;; tramp-rpc-process.el --- Async and PTY process support for TRAMP-RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Keywords: comm, processes
;; Package-Requires: ((emacs "30.1") (msgpack "0"))

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

;; Silence byte-compiler warnings for variables defined in vterm
(defvar vterm-copy-mode)
(defvar vterm-min-window-width)
(defvar vterm--term)

;; Functions from tramp-rpc.el (loaded before us)
(declare-function tramp-rpc--debug "tramp-rpc")
(declare-function tramp-rpc--ensure-connection "tramp-rpc")
(declare-function tramp-rpc--call "tramp-rpc")
(declare-function tramp-rpc--call-fast "tramp-rpc")
(declare-function tramp-rpc--call-async "tramp-rpc")
(declare-function tramp-rpc--resolve-executable "tramp-rpc")
(declare-function tramp-rpc--get-direnv-environment "tramp-rpc")
(declare-function tramp-rpc--decode-output "tramp-rpc")
(declare-function tramp-rpc--controlmaster-socket-path "tramp-rpc")
(declare-function tramp-rpc-file-name-p "tramp-rpc")

;; Variables from tramp-rpc.el
(defvar tramp-rpc-use-direct-ssh-pty)
(defvar tramp-rpc-use-controlmaster)
(defvar tramp-rpc-controlmaster-persist)
(defvar tramp-rpc-ssh-options)
(defvar tramp-rpc-ssh-args)
(defvar tramp-rpc-use-async-read)
(defvar tramp-rpc-async-read-timeout-ms)
(defvar tramp-rpc--delivering-output)

;; ============================================================================
;; Process tracking state
;; ============================================================================

(defvar tramp-rpc--async-processes (make-hash-table :test 'eq)
  "Hash table mapping local relay processes to their remote process info.
Value is a plist with :vec, :pid, :timer, :stderr-buffer.")

(defvar tramp-rpc--pty-processes (make-hash-table :test 'eq)
  "Hash table mapping local relay processes to their remote PTY process info.
Value is a plist with :vec, :pid.")

(defvar tramp-rpc--process-write-queues (make-hash-table :test 'eql)
  "Hash table mapping remote PIDs to write queue state.
Value is a plist with :pending (list of pending write data) and :writing (bool).")

;; ============================================================================
;; Polling configuration
;; ============================================================================

(defvar tramp-rpc--process-poll-interval-min 0.01
  "Minimum interval in seconds between polling remote process for output.
Used when process is actively producing output.")

(defvar tramp-rpc--process-poll-interval-max 0.5
  "Maximum interval in seconds between polling remote process for output.
Used when process has been idle for a while.")

(defvar tramp-rpc--process-poll-interval-initial 0.05
  "Initial interval in seconds for polling a new process.")

(defvar tramp-rpc--process-poll-backoff-factor 1.5
  "Factor by which to increase poll interval when no output is received.")

(defvar tramp-rpc--poll-in-progress nil
  "Non-nil while a poll is in progress (prevents reentrancy).")

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
                    (tramp-rpc--decode-output
                     s (alist-get 'stdout_encoding result)))
          :stderr (when-let* ((s (alist-get 'stderr result)))
                    (tramp-rpc--decode-output
                     s (alist-get 'stderr_encoding result)))
          :exited (alist-get 'exited result)
          :exit-code (alist-get 'exit_code result))))

(defun tramp-rpc--write-remote-process (vec pid data)
  "Write DATA to stdin of remote process PID on VEC.
Uses async RPC with queuing to ensure writes are serialized.
This prevents race conditions where later messages arrive before earlier ones."
  (let* ((queue-key pid)
         (queue (gethash queue-key tramp-rpc--process-write-queues))
         (pending (plist-get queue :pending))
         (writing (plist-get queue :writing)))
    ;; Add to pending queue
    (setq pending (append pending (list (list :vec vec :pid pid :data data))))
    (puthash queue-key (list :pending pending :writing writing)
             tramp-rpc--process-write-queues)
    ;; If not currently writing, start processing the queue
    (unless writing
      (tramp-rpc--process-write-queue queue-key))))

(defun tramp-rpc--process-write-queue (queue-key)
  "Process the next pending write for QUEUE-KEY (remote PID)."
  (let* ((queue (gethash queue-key tramp-rpc--process-write-queues))
         (pending (plist-get queue :pending)))
    (when pending
      (let* ((item (car pending))
             (vec (plist-get item :vec))
             (pid (plist-get item :pid))
             (data (plist-get item :data)))
        ;; Mark as writing and remove from pending
        (puthash queue-key (list :pending (cdr pending) :writing t)
                 tramp-rpc--process-write-queues)
        ;; Send the write - data must be binary for MessagePack
        (let ((data-bytes (if (multibyte-string-p data)
                              (encode-coding-string data 'utf-8-unix)
                            data)))
          (tramp-rpc--call-async vec "process.write"
                                 `((pid . ,pid)
                                   (data . ,(msgpack-bin-make data-bytes)))
                               (lambda (response)
                                 (when (plist-get response :error)
                                   (tramp-rpc--debug "WRITE-ERROR pid=%s: %s"
                                                    pid (plist-get response :error)))
                                 ;; Mark as not writing and process next item
                                 (let ((q (gethash queue-key tramp-rpc--process-write-queues)))
                                   (puthash queue-key
                                            (list :pending (plist-get q :pending) :writing nil)
                                            tramp-rpc--process-write-queues))
                                 (tramp-rpc--process-write-queue queue-key))))))))

(defun tramp-rpc--close-remote-stdin (vec pid)
  "Close stdin of remote process PID on VEC."
  (tramp-rpc--call vec "process.close_stdin" `((pid . ,pid))))

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
    ;; Set flag to bypass our advice - we're writing TO the local process,
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
        (cond
         ((bufferp stderr-buffer)
          (when (buffer-live-p stderr-buffer)
            (with-current-buffer stderr-buffer
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert stderr)))))
         ;; Mix with stdout if no separate stderr buffer - write to cat relay
         (t
          (process-send-string local-process stderr)))))))

(defun tramp-rpc--pipe-process-sentinel (proc event user-sentinel)
  "Sentinel for pipe relay processes.
PROC is the local cat process, EVENT is the event string.
USER-SENTINEL is the user's original sentinel function."
  ;; If cat died unexpectedly, clean up the remote process
  (when (and (memq (process-status proc) '(exit signal))
             (gethash proc tramp-rpc--async-processes))
    (let* ((info (gethash proc tramp-rpc--async-processes))
           (vec (plist-get info :vec))
           (pid (plist-get info :pid)))
      ;; Kill remote process if still running
      (when (and vec pid)
        (ignore-errors
          (tramp-rpc--kill-remote-process vec pid 9)))
      ;; Remove from tracking
      (remhash proc tramp-rpc--async-processes)))
  ;; Call user's sentinel if provided
  (when user-sentinel
    (funcall user-sentinel proc event)))

(defun tramp-rpc--handle-async-read-response (local-process response)
  "Handle async read response for LOCAL-PROCESS.
RESPONSE is the decoded RPC response plist."
  ;; Check process is still valid
  (when (and (processp local-process)
             (process-live-p local-process)
             (gethash local-process tramp-rpc--async-processes))
    (condition-case err
        (let* ((info (gethash local-process tramp-rpc--async-processes))
               (stderr-buffer (plist-get info :stderr-buffer))
               (result (plist-get response :result))
               (stdout (when-let* ((s (alist-get 'stdout result)))
                         (tramp-rpc--decode-output
                          s (alist-get 'stdout_encoding result))))
               (stderr (when-let* ((s (alist-get 'stderr result)))
                         (tramp-rpc--decode-output
                          s (alist-get 'stderr_encoding result))))
               (exited (alist-get 'exited result))
               (exit-code (alist-get 'exit_code result)))
          
          (tramp-rpc--debug "ASYNC-READ response: stdout=%s stderr=%s exited=%s"
                           (if stdout (length stdout) "nil")
                           (if stderr (length stderr) "nil")
                           exited)
          
          ;; Deliver output - use run-at-time to ensure event loop processes it
          ;; This is critical for accept-process-output to work correctly
          (when (or stdout stderr)
            (run-at-time 0 nil #'tramp-rpc--deliver-process-output
                         local-process stdout stderr stderr-buffer))
          
          ;; Handle process exit or chain next read
          (if exited
              (run-at-time 0 nil #'tramp-rpc--handle-process-exit local-process exit-code)
            ;; Chain another read - use run-at-time to avoid stack overflow
            (run-at-time 0 nil #'tramp-rpc--start-async-read local-process)))
      (error
       (tramp-rpc--debug "ASYNC-READ-ERROR: %S" err)
       ;; On error, clean up
       (run-at-time 0 nil #'tramp-rpc--handle-process-exit local-process -1)))))

(defun tramp-rpc--adjust-poll-interval (local-process had-output)
  "Adjust the poll interval for LOCAL-PROCESS based on whether it HAD-OUTPUT.
Returns the new interval."
  (let* ((info (gethash local-process tramp-rpc--async-processes))
         (current-interval (or (plist-get info :poll-interval)
                               tramp-rpc--process-poll-interval-initial))
         (new-interval
          (if had-output
              ;; Got output - use minimum interval for responsiveness
              tramp-rpc--process-poll-interval-min
            ;; No output - back off gradually
            (min (* current-interval tramp-rpc--process-poll-backoff-factor)
                 tramp-rpc--process-poll-interval-max))))
    ;; Update stored interval
    (when info
      (plist-put info :poll-interval new-interval))
    new-interval))

(defun tramp-rpc--reschedule-poll-timer (local-process new-interval)
  "Reschedule the poll timer for LOCAL-PROCESS with NEW-INTERVAL."
  (let ((info (gethash local-process tramp-rpc--async-processes)))
    (when info
      (when-let* ((old-timer (plist-get info :timer)))
        (cancel-timer old-timer))
      (let ((new-timer (run-with-timer
                        new-interval
                        new-interval
                        #'tramp-rpc--poll-process
                        local-process)))
        (plist-put info :timer new-timer)))))

(defun tramp-rpc--poll-process (local-process)
  "Poll for output from the remote process associated with LOCAL-PROCESS."
  ;; Guard against reentrancy (can happen during accept-process-output)
  ;; Also ensure quitting is allowed since timers run with inhibit-quit=t
  (unless tramp-rpc--poll-in-progress
    (let ((tramp-rpc--poll-in-progress t)
          (inhibit-quit nil))
      (when (process-live-p local-process)
        (let ((info (gethash local-process tramp-rpc--async-processes)))
          (when info
            (let* ((vec (plist-get info :vec))
                   (pid (plist-get info :pid))
                   (stderr-buffer (plist-get info :stderr-buffer))
                   (process-gone nil)
                   (result (condition-case err
                               (tramp-rpc--read-remote-process vec pid)
                             (error
                              ;; Check if this is a "process not found" error
                              ;; which means the remote process died unexpectedly
                              (if (string-match-p "Process not found" (error-message-string err))
                                  (progn
                                    (tramp-rpc--debug "POLL process %s gone (pid=%s)" local-process pid)
                                    (setq process-gone t)
                                    nil)
                                (message "tramp-rpc: Error polling process: %s" err)
                                nil))))
                   (had-output nil))
              ;; If the remote process disappeared, clean up and stop
              (when process-gone
                (tramp-rpc--handle-process-exit local-process -1))
              (when (and result (not process-gone))
            ;; Handle stdout - send to process filter/buffer
            (when-let* ((stdout (plist-get result :stdout)))
              (when (> (length stdout) 0)
                (setq had-output t)
                (if-let* ((filter (process-filter local-process)))
                    (funcall filter local-process stdout)
                  (when-let* ((buf (process-buffer local-process)))
                    (when (buffer-live-p buf)
                      (with-current-buffer buf
                        (let ((inhibit-read-only t))
                          (goto-char (point-max))
                          (insert stdout))))))))
            
            ;; Handle stderr - send to stderr buffer if specified
            (when-let* ((stderr (plist-get result :stderr)))
              (when (> (length stderr) 0)
                (setq had-output t)
                (cond
                 ((bufferp stderr-buffer)
                  (when (buffer-live-p stderr-buffer)
                    (with-current-buffer stderr-buffer
                      (let ((inhibit-read-only t))
                        (goto-char (point-max))
                        (insert stderr)))))
                 ;; If no stderr buffer, mix with stdout
                 (t
                  (if-let* ((filter (process-filter local-process)))
                      (funcall filter local-process stderr)
                    (when-let* ((buf (process-buffer local-process)))
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (let ((inhibit-read-only t))
                            (goto-char (point-max))
                            (insert stderr))))))))))
            
            ;; Adaptive polling: adjust interval based on output
            (let* ((current-interval (or (plist-get info :poll-interval)
                                         tramp-rpc--process-poll-interval-initial))
                   (new-interval (tramp-rpc--adjust-poll-interval
                                  local-process had-output)))
              ;; Only reschedule if interval changed significantly (>20%)
              (when (> (abs (- new-interval current-interval))
                       (* 0.2 current-interval))
                (tramp-rpc--reschedule-poll-timer local-process new-interval)))
            
            ;; Check if process exited
            (when (plist-get result :exited)
              (tramp-rpc--handle-process-exit
               local-process (plist-get result :exit-code)))))))))))

(defun tramp-rpc--handle-process-exit (local-process exit-code)
  "Handle exit of remote process associated with LOCAL-PROCESS."
  (let ((info (gethash local-process tramp-rpc--async-processes)))
    (when info
      ;; Cancel the timer (if using timer-based polling)
      (when-let* ((timer (plist-get info :timer)))
        (cancel-timer timer))
      ;; Remove from tracking
      (remhash local-process tramp-rpc--async-processes)
      ;; Store exit code and mark as exited
      (process-put local-process :tramp-rpc-exit-code (or exit-code 0))
      (process-put local-process :tramp-rpc-exited t)
      ;; Get sentinel before we modify anything
      (let ((sentinel (process-sentinel local-process))
            (event (if (and exit-code (= exit-code 0))
                       "finished\n"
                     (format "exited abnormally with code %d\n" (or exit-code -1)))))
        ;; Remove sentinel temporarily to prevent double-call
        (set-process-sentinel local-process nil)
        ;; Delete the process (changes status)
        (when (process-live-p local-process)
          (delete-process local-process))
        ;; Call sentinel in a separate thread to prevent blocking
        ;; This is critical because sentinels like vc-exec-after may do
        ;; blocking operations (accept-process-output) that would hang Emacs.
        ;; Using a thread allows the sentinel to block without freezing the UI,
        ;; since threads yield the global lock during I/O operations.
        (when sentinel
          (make-thread
           (lambda ()
             (condition-case err
                 (funcall sentinel local-process event)
               (error
                (tramp-rpc--debug "SENTINEL-ERROR: %S" err))))
           "tramp-rpc-sentinel"))))))

;; ============================================================================
;; make-process handler
;; ============================================================================

(defun tramp-rpc-handle-make-process (&rest args)
  "Create an async process on the remote host.
ARGS are keyword arguments as per `make-process'.
Supports PTY allocation when :connection-type is \\='pty or t,
or when `process-connection-type' is t.
For pipe mode, uses async polling for long-running processes.
Resolves program path and loads direnv environment from working directory."
  (let* ((name (plist-get args :name))
         (buffer (plist-get args :buffer))
         (command (plist-get args :command))
         (coding (plist-get args :coding))
         (noquery (plist-get args :noquery))
         (filter (plist-get args :filter))
         (sentinel (plist-get args :sentinel))
         (stderr (plist-get args :stderr))
         ;; file-handler is accepted but not used (we ARE the file handler)
         (_file-handler (plist-get args :file-handler))
         ;; Check both :connection-type arg and process-connection-type variable
         (connection-type (or (plist-get args :connection-type)
                              (when (boundp 'process-connection-type)
                                process-connection-type)))
         (program (car command))
         (program-args (cdr command))
         ;; Determine if PTY is requested
         (use-pty (memq connection-type '(pty t))))
    
    ;; Ensure we're in a remote directory
    (unless (tramp-tramp-file-p default-directory)
      (error "tramp-rpc-handle-make-process called without remote default-directory"))
    
    (with-parsed-tramp-file-name default-directory nil
      ;; Get direnv environment for this directory
      (let ((direnv-env (tramp-rpc--get-direnv-environment v localname)))
        (if use-pty
            ;; PTY mode - start async process with PTY
            (tramp-rpc--make-pty-process v name buffer command coding noquery
                                          filter sentinel localname direnv-env)
          ;; Pipe mode - use a local cat process as relay for proper I/O events
          ;; This is needed because accept-process-output waits for actual I/O,
          ;; not just filter calls
          (let* ((remote-program (tramp-rpc--resolve-executable v program))
                 (remote-pid (tramp-rpc--start-remote-process
                              v remote-program program-args localname direnv-env))
                 ;; Use a local cat process as relay - we write output to its stdin
                 ;; and it echoes to stdout, triggering proper I/O events
                 (local-process (let ((process-connection-type nil)) ; Use pipes, not PTY
                                  (start-process (or name "tramp-rpc-async")
                                                 buffer
                                                 "cat")))
                 (stderr-buffer (cond
                                 ((bufferp stderr) stderr)
                                 ((stringp stderr) (get-buffer-create stderr))
                                 (t nil))))
          
          ;; Configure the local relay process
          (when coding
            (set-process-coding-system local-process coding coding))
          (set-process-query-on-exit-flag local-process (not noquery))
          
          (process-put local-process :tramp-rpc-vec v)
          (process-put local-process :tramp-rpc-pid remote-pid)
          
          (when filter
            (set-process-filter local-process filter))
          (when sentinel
            ;; Wrap sentinel to handle our cleanup
            (set-process-sentinel local-process
                                  (lambda (proc event)
                                    (tramp-rpc--pipe-process-sentinel proc event sentinel))))
          
          ;; Store process info
          (puthash local-process
                   (list :vec v
                         :pid remote-pid
                         :stderr-buffer stderr-buffer)
                   tramp-rpc--async-processes)
          
          (tramp-rpc--debug "MAKE-PROCESS created local=%s remote-pid=%s program=%s"
                           local-process remote-pid remote-program)
          
          ;; Start async read loop
          (tramp-rpc--start-async-read local-process)
          
          local-process))))))

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
  (if tramp-rpc-use-direct-ssh-pty
      ;; Use direct SSH for low-latency PTY
      (tramp-rpc--make-direct-ssh-pty-process
       vec name buffer command coding noquery filter sentinel localname direnv-env)
    ;; Use RPC-based PTY
    (tramp-rpc--make-rpc-pty-process
     vec name buffer command coding noquery filter sentinel localname direnv-env)))

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
         (port (tramp-file-name-port vec))
         (program (car command))
         (program-args (cdr command))
         ;; Build environment exports for the remote command
         (env-exports (mapconcat
                       (lambda (pair)
                         (format "export %s=%s;"
                                 (car pair)
                                 (shell-quote-argument (cdr pair))))
                       (append direnv-env
                               `(("TERM" . ,(or (getenv "TERM") "xterm-256color"))))
                       " "))
         ;; Build the remote command - cd to dir, export env, exec program
         (remote-cmd (format "cd %s && %s exec %s %s"
                             (shell-quote-argument localname)
                             env-exports
                             (shell-quote-argument program)
                             (mapconcat #'shell-quote-argument program-args " ")))
         ;; Build SSH arguments for direct PTY connection
         (ssh-args (append
                    (list "ssh")
                    ;; Request PTY allocation
                    (list "-t" "-t")  ; Force PTY even without controlling terminal
                    ;; Reuse ControlMaster if enabled
                    (when tramp-rpc-use-controlmaster
                      (list "-o" "ControlMaster=auto"
                            "-o" (format "ControlPath=%s"
                                         (tramp-rpc--controlmaster-socket-path vec))
                            "-o" (format "ControlPersist=%s"
                                         tramp-rpc-controlmaster-persist)))
                    ;; Standard options
                    ;; Only use BatchMode=yes when ControlMaster handles auth;
                    ;; without it, BatchMode=yes prevents password prompts.
                    (when tramp-rpc-use-controlmaster
                      (list "-o" "BatchMode=yes"))
                    (list "-o" "StrictHostKeyChecking=accept-new")
                    ;; User-specified SSH options
                    (mapcan (lambda (opt) (list "-o" opt))
                            tramp-rpc-ssh-options)
                    ;; Raw SSH arguments
                    tramp-rpc-ssh-args
                    ;; Connection parameters
                    (when user (list "-l" user))
                    (when port (list "-p" (number-to-string port)))
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
      (set-process-coding-system process coding coding))
    (set-process-query-on-exit-flag process (not noquery))
    
    ;; Set up filter
    (when filter
      (set-process-filter process filter))
    
    ;; Set up sentinel
    (when sentinel
      (set-process-sentinel process sentinel))
    
    ;; Store tramp-rpc metadata for compatibility with other code
    (process-put process :tramp-rpc-pty t)
    (process-put process :tramp-rpc-direct-ssh t)
    (process-put process :tramp-rpc-vec vec)
    (process-put process :tramp-rpc-user-sentinel sentinel)
    (process-put process :tramp-rpc-command command)
    
    process))

(defun tramp-rpc--make-rpc-pty-process (vec name buffer command coding noquery
                                             filter sentinel localname &optional direnv-env)
  "Create a PTY-based process using the RPC server.
This is the fallback when direct SSH PTY is disabled.

VEC is the tramp connection vector.
NAME, BUFFER, COMMAND, CODING, NOQUERY, FILTER, SENTINEL are process params.
LOCALNAME is the remote working directory.
DIRENV-ENV is an optional alist of environment variables from direnv."
  (let* ((program (car command))
         (program-args (cdr command))
         (remote-program (tramp-rpc--resolve-executable vec program))
         ;; Get terminal dimensions from buffer or use defaults
         (size (tramp-rpc--get-terminal-size buffer))
         (rows (cdr size))
         (cols (car size))
         ;; Build environment - merge direnv env with TERM
         (term-env (or (getenv "TERM") "xterm-256color"))
         (full-env (append direnv-env `(("TERM" . ,term-env))))
         ;; Start the PTY process on remote
         (result (tramp-rpc--call vec "process.start_pty"
                                   `((cmd . ,remote-program)
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
         ;; Create a local pipe process as a relay
         ;; We use make-pipe-process for the local side - all actual I/O
         ;; goes through our PTY RPC calls, not through this process.
         (local-process (make-pipe-process
                         :name (or name "tramp-rpc-pty")
                         :buffer actual-buffer
                         :coding (or coding 'utf-8-unix)
                         :noquery t)))
    
    ;; Configure the local relay process
    (set-process-filter local-process (or filter #'tramp-rpc--pty-default-filter))
    (set-process-sentinel local-process #'tramp-rpc--pty-sentinel)
    (set-process-query-on-exit-flag local-process (not noquery))
    (when coding
      (set-process-coding-system local-process coding coding))
    
    ;; Store process info
    (process-put local-process :tramp-rpc-pty t)
    (process-put local-process :tramp-rpc-pid remote-pid)
    (process-put local-process :tramp-rpc-vec vec)
    (process-put local-process :tramp-rpc-user-sentinel sentinel)
    (process-put local-process :tramp-rpc-command command)
    (process-put local-process :tramp-rpc-tty-name tty-name)
    
    ;; Set up window size adjustment function
    (process-put local-process 'adjust-window-size-function
                 #'tramp-rpc--adjust-pty-window-size)
    
    ;; Track the PTY process
    (puthash local-process
             (list :vec vec :pid remote-pid)
             tramp-rpc--pty-processes)
    
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
               (output (when-let* ((o (alist-get 'output result)))
                         (tramp-rpc--decode-output
                          o (alist-get 'output_encoding result))))
               (exited (alist-get 'exited result))
               (exit-code (alist-get 'exit_code result)))
          
          ;; Deliver output via filter
          (when (and output (> (length output) 0))
            (if-let* ((filter (process-filter local-process)))
                (funcall filter local-process output)
              (when-let* ((buf (process-buffer local-process)))
                (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (goto-char (point-max))
                    (insert output))))))
          
          ;; Handle process exit or chain next read
          (if exited
              (tramp-rpc--handle-pty-exit local-process exit-code)
            ;; Chain another read immediately
            (tramp-rpc--pty-start-async-read local-process)))
      (error
       ;; On error, clean up
       (tramp-rpc--handle-pty-exit local-process nil)))))

(defun tramp-rpc--handle-pty-exit (local-process exit-code)
  "Handle exit of PTY process associated with LOCAL-PROCESS."
  ;; Clean up PTY on remote
  (when-let* ((vec (process-get local-process :tramp-rpc-vec))
             (pid (process-get local-process :tramp-rpc-pid)))
    (ignore-errors
      (tramp-rpc--call vec "process.close_pty" `((pid . ,pid)))))
  
  ;; Remove from tracking
  (remhash local-process tramp-rpc--pty-processes)
  
  ;; Store exit info
  (process-put local-process :tramp-rpc-exit-code (or exit-code 0))
  (process-put local-process :tramp-rpc-exited t)
  
  ;; Get user sentinel
  (let ((user-sentinel (process-get local-process :tramp-rpc-user-sentinel))
        (event (if (and exit-code (= exit-code 0))
                   "finished\n"
                 (format "exited abnormally with code %d\n" (or exit-code -1)))))
    ;; Delete the local process
    (when (process-live-p local-process)
      (delete-process local-process))
    ;; Call user sentinel
    (when user-sentinel
      (funcall user-sentinel local-process event))))

(defun tramp-rpc--pty-sentinel (process event)
  "Sentinel for PTY relay processes.
PROCESS is the local relay process, EVENT is the process event."
  ;; Handle local process termination (e.g., user killed it)
  (when (memq (process-status process) '(exit signal))
    ;; Clean up remote PTY if still tracked
    (when (gethash process tramp-rpc--pty-processes)
      (when-let* ((vec (process-get process :tramp-rpc-vec))
                 (pid (process-get process :tramp-rpc-pid)))
        (ignore-errors
          (tramp-rpc--call vec "process.kill_pty"
                           `((pid . ,pid) (signal . 9)))))
      ;; Remove from tracking
      (remhash process tramp-rpc--pty-processes))
    ;; Call user sentinel
    (when-let* ((user-sentinel (process-get process :tramp-rpc-user-sentinel)))
      (funcall user-sentinel process event))))

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

(defun tramp-rpc--vterm-window-size-advice (orig-fun process windows)
  "Advice for vterm's window adjust function for TRAMP-RPC PTY processes.
For tramp-rpc processes, resize the remote PTY and update vterm's display.
For direct SSH PTY, let the original function handle it (SSH handles resize)."
  (cond
   ;; Direct SSH PTY - let original function handle it
   ;; SSH client handles PTY resize automatically via SSH protocol
   ((and (processp process)
         (process-get process :tramp-rpc-direct-ssh))
    (funcall orig-fun process windows))
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
   (t (funcall orig-fun process windows))))

(defun tramp-rpc--eat-window-size-advice (orig-fun process windows)
  "Advice for eat's window adjust function for TRAMP-RPC PTY processes.
For tramp-rpc processes, resize the remote PTY and update eat's display.
For direct SSH PTY, let the original function handle it (SSH handles resize)."
  (cond
   ;; Direct SSH PTY - let original function handle it
   ;; SSH client handles PTY resize automatically via SSH protocol
   ((and (processp process)
         (process-get process :tramp-rpc-direct-ssh))
    (funcall orig-fun process windows))
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
   (t (funcall orig-fun process windows))))

;; ============================================================================
;; Process cleanup
;; ============================================================================

(defun tramp-rpc--cleanup-async-processes (&optional vec)
  "Clean up async processes, optionally only those for VEC."
  (maphash
   (lambda (local-process info)
     (when (or (null vec)
               (equal (tramp-rpc--connection-key (plist-get info :vec))
                      (tramp-rpc--connection-key vec)))
       ;; Cancel timer
       (when-let* ((timer (plist-get info :timer)))
         (cancel-timer timer))
       ;; Kill local process
       (when (process-live-p local-process)
         (delete-process local-process))
       ;; Remove from tracking
       (remhash local-process tramp-rpc--async-processes)))
   tramp-rpc--async-processes))

;; Forward declare for cleanup
(declare-function tramp-rpc--connection-key "tramp-rpc")

;; Install terminal emulator advice
(with-eval-after-load 'vterm
  (advice-add 'vterm--window-adjust-process-window-size :around
              #'tramp-rpc--vterm-window-size-advice))

(with-eval-after-load 'eat
  (advice-add 'eat--adjust-process-window-size :around
              #'tramp-rpc--eat-window-size-advice))

(provide 'tramp-rpc-process)
;;; tramp-rpc-process.el ends here
