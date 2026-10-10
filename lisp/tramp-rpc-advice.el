;;; tramp-rpc-advice.el --- Process handlers for TRAMP-RPC -*- lexical-binding: t; -*-

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

;; This file provides the advice and Tramp handlers that tramp-rpc
;; installs on Emacs built-in process functions:
;; - process-send-string / process-send-region (route to remote stdin)
;; - process-send-eof (close remote stdin or send Ctrl-D to PTY)
;; - signal-process (forward signals to remote PID)
;; - interrupt-process (interrupt the remote process, not its local relay)
;; - process-status / process-exit-status (return remote process state)
;; - process-command / process-tty-name (return stored metadata)
;; - set-process-sentinel / process-sentinel (keep tramp-rpc's own sentinel)
;; - vc-call-backend (ensure default-directory for remote VC files)
;; - vc-exec-after (handle native-compiled VC process state races)
;; - python-shell--tramp-with-environment (avoid sending shell commands to the RPC server)
;; - eglot--cmd (bypass shell wrapping for RPC connections)
;; - magit-start-process (force pipe mode when INPUT will be piped to the process)
;; - vc-dir-refresh (clean up stale processes)

;;; Code:

(require 'tramp)
(require 'msgpack)
(require 'tramp-rpc-protocol)
(require 'tramp-rpc-connection)
(require 'tramp-rpc-hops)
(require 'tramp-rpc-transport)
(require 'tramp-rpc-process)

;; Functions from tramp-rpc.el
(declare-function tramp-rpc-file-name-p "tramp-rpc")
(declare-function tramp-rpc--forget-managed-process "tramp-rpc-process" (process))

;; Variables from tramp-rpc.el / tramp-rpc-process.el

;; Functions from vc-dispatcher.el (used by vc-exec-after handler)
(declare-function vc-exec-after "vc-dispatcher")
(declare-function vc-set-mode-line-busy-indicator "vc-dispatcher")

;; Functions from python.el (used by python-shell integration)
(declare-function python-shell--tramp-with-environment "python")

;; Variables from vc-dir.el / tramp.el (used in integration handlers)
(defvar vc-dir-process-buffer)
(defvar tramp-remote-process-environment)
(defvar tramp-file-name-for-operation-external)

;; ============================================================================
;; Process coding state
;; ============================================================================

(defun tramp-rpc--process-coding-advised-p (process)
  "Return non-nil when PROCESS has a public relay coding state."
  (and (processp process)
       (process-get process :tramp-rpc-coding)))

(defun tramp-rpc--set-process-coding-system-advice
    (orig-fun process &optional decoding encoding)
  "Keep relay output binary while exposing the requested coding pair.
RPC relays receive raw bytes through their write side, so only their read side
is configured with DECODING.  ENCODING is retained for RPC stdin writes and
for `process-coding-system' callers.
ORIG-FUN is the original advised function.
PROCESS is the process being handled."
  (if (tramp-rpc--process-coding-advised-p process)
      (let ((effective-encoding (or encoding 'raw-text-unix)))
        ;; Validate both public sides before changing the relay.  Passing the
        ;; binary write side to Emacs would otherwise hide an invalid requested
        ;; encoding until the next process write.
        (check-coding-system decoding)
        (check-coding-system effective-encoding)
        (funcall orig-fun process decoding 'binary)
        (process-put process :tramp-rpc-coding
                     (cons decoding effective-encoding)))
    (funcall orig-fun process decoding encoding)))

(defun tramp-rpc--process-coding-system-advice (orig-fun process)
  "Return a relay's requested coding pair rather than its binary write side.
ORIG-FUN is the original advised function.
PROCESS is the process being handled."
  (or (and (tramp-rpc--process-coding-advised-p process)
           (process-get process :tramp-rpc-coding))
      (funcall orig-fun process)))

;; ============================================================================
;; Process routing
;; ============================================================================

;; Process primitives are routed by process ownership in outermost advice,
;; not through Tramp's file name handlers.  Ownership is a property of the
;; process object, and handler dispatch would run the native primitive under
;; `tramp-run-real-handler', whose inhibition leaks into filters and timers
;; run while a blocked write waits.  Internal relay plumbing bypasses the
;; advice per call with `tramp-rpc--native-call'.

(defun tramp-rpc--resolve-process (process)
  "Return the process object denoted by PROCESS, or nil."
  (cond
   ((processp process) process)
   ((null process) (get-buffer-process (current-buffer)))
   ((bufferp process) (get-buffer-process process))
   ((stringp process)
    (or (get-process process)
        (when-let* ((buffer (get-buffer process)))
          (get-buffer-process buffer))))))

(defun tramp-rpc--send-managed-process-string (proc string operation)
  "Send STRING to the remote stdin of RPC-managed PROC for OPERATION."
  (let ((vec (process-get proc :tramp-rpc-vec))
        (pid (process-get proc :tramp-rpc-pid))
        (data (tramp-rpc--encode-process-input proc string)))
    (if (process-get proc :tramp-rpc-pty)
        (progn
          (tramp-rpc--debug "%s PTY pid=%s len=%d" operation pid (length string))
          (tramp-rpc--call vec "process.write_pty"
                           `((pid . ,pid) (data . ,(msgpack-bin-make data)))
                           (process-get proc :tramp-rpc-connection)))
      (tramp-rpc--debug "%s pipe pid=%s len=%d" operation pid (length string))
      (tramp-rpc--write-remote-process vec pid data proc)
      (when tramp-rpc-synchronous-pipe-writes
        (tramp-rpc--drain-write-queue vec pid proc))))
  nil)

(defun tramp-rpc--managed-send-process (process)
  "Return PROCESS's RPC-managed process, or nil when native delivery is needed."
  (when-let* ((proc (tramp-rpc--resolve-process process)))
    (and (not (process-get proc :tramp-rpc-direct-ssh))
         (process-get proc :tramp-rpc-pid)
         (process-get proc :tramp-rpc-vec)
         proc)))

(defun tramp-rpc--process-send-string-advice (orig-fun process string)
  "Send STRING to RPC-managed PROCESS, otherwise call ORIG-FUN."
  (if-let* ((proc (tramp-rpc--managed-send-process process)))
      (tramp-rpc--send-managed-process-string proc string "SEND-STRING")
    (funcall orig-fun process string)))

(defun tramp-rpc--process-send-region-advice (orig-fun process start end)
  "Send region START to END to RPC-managed PROCESS, otherwise call ORIG-FUN."
  (if-let* ((proc (tramp-rpc--managed-send-process process)))
      (tramp-rpc--send-managed-process-string
       proc (buffer-substring-no-properties start end) "SEND-REGION")
    (funcall orig-fun process start end)))

(defun tramp-rpc--process-send-eof-advice (orig-fun &optional process)
  "Close the remote stdin of RPC-managed PROCESS, otherwise call ORIG-FUN."
  (if-let* ((proc (tramp-rpc--managed-send-process process)))
      (let ((pid (process-get proc :tramp-rpc-pid))
            (vec (process-get proc :tramp-rpc-vec)))
        ;; Short-lived processes (like git apply) may exit before the caller
        ;; sends EOF, which is fine: stdin was already closed on exit.
        (unless (or (process-get proc :tramp-rpc-exited)
                    (not (process-live-p proc)))
          (if (process-get proc :tramp-rpc-pty)
              ;; PTY processes: send Ctrl-D (EOF character) via the PTY.
              (tramp-rpc--call vec "process.write_pty"
                               `((pid . ,pid)
                                 (data . ,(msgpack-bin-make (string ?\C-d))))
                               (process-get proc :tramp-rpc-connection))
            ;; Pipe processes: a queue failure must reach the caller.
            (tramp-rpc--close-remote-stdin vec pid proc))))
    (apply orig-fun (and process (list process)))))

(defun tramp-rpc-handle-signal-process (process sigcode &optional remote current-group)
  "Handler for `signal-process' of TRAMP-RPC processes.
It will be added to `signal-process-functions'.
PROCESS is the process object or remote PID being handled.
SIGCODE identifies the signal to send.  REMOTE identifies the host when
PROCESS is a PID.  CURRENT-GROUP selects an RPC PTY's foreground job;
`lambda' skips the signal when the terminal's leader owns the foreground."
  (when (stringp process)
    (setq process
          (or (get-process process)
              (and (string-match-p (rx bol (+ digit) eol) process)
                   (string-to-number process)))))
  (let (pid vec rpc-process)
    (cond
     ((processp process)
      (setq pid (process-get process :tramp-rpc-pid)
            vec (process-get process :tramp-rpc-vec)
            rpc-process process))
     ((and (numberp process)
           (stringp remote)
           (tramp-rpc--managed-file-name-p remote))
      (setq pid process
            vec (tramp-dissect-file-name remote))))
    (when (and pid vec)
      (let ((connection (and rpc-process
                             (process-get rpc-process :tramp-rpc-connection))))
        (condition-case err
            (progn
              (cond
               ;; A numeric PID names an arbitrary remote OS process, not an
               ;; entry in the RPC server's managed-process registry.
               ((not rpc-process)
                (tramp-rpc--call vec "process.signal"
                                 `((pid . ,pid) (signal . ,sigcode))))
               ;; Use PTY kill for PTY processes, regular kill for pipes.
               ((process-get rpc-process :tramp-rpc-pty)
                (tramp-rpc--call vec "process.kill_pty"
                                 (append `((pid . ,pid) (signal . ,sigcode))
                                         (when current-group
                                           `((group . ,(if (eq current-group 'lambda)
                                                           "foreground_unless_leader"
                                                         "foreground")))))
                                 connection))
               (t
                (tramp-rpc--kill-remote-process
                 vec pid sigcode connection)))
              0) ; Return 0 for success.
          (error
           (message "tramp-rpc: Error signaling process: %s" err)
           -1))))))

(defun tramp-rpc-handle-interrupt-process (&optional process current-group)
  "Interrupt TRAMP-RPC PROCESS remotely, for `interrupt-process-functions'.
The default would signal the local relay, which ends it and, through its
sentinel, kills the remote process instead of interrupting it.  A direct SSH
PTY gets the interrupt character, like a terminal.  PROCESS can be a
process, a buffer, a process or buffer name, or nil for the current buffer.
CURRENT-GROUP selects the foreground job for RPC PTYs, as in
`interrupt-process'.  Return nil for other processes."
  (when-let* ((proc (tramp-rpc--resolve-process process))
              ((or (process-get proc :tramp-rpc-direct-ssh)
                   (process-get proc :tramp-rpc-pid))))
    (unless (process-live-p proc)
      (error "Process %s is not active" (process-name proc)))
    (if (process-get proc :tramp-rpc-direct-ssh)
        (process-send-string proc (string ?\C-c))
      (unless (eq 0 (tramp-rpc-handle-signal-process proc 2 nil current-group))
        (signal 'remote-file-error '("Failed to interrupt remote process"))))
    t))

;; ============================================================================
;; Process metadata
;; ============================================================================

(defun tramp-rpc--process-status-advice (orig-fun process)
  "Return the remote status of RPC-managed PROCESS, otherwise call ORIG-FUN."
  ;; Unlike other process primitives, a string names only a process here.
  (if-let* ((proc (if (stringp process)
                      (get-process process)
                    (tramp-rpc--resolve-process process)))
            ((process-get proc :tramp-rpc-pid)))
      (cond
       ;; Check local relay liveness with ORIG-FUN.  Do not perform
       ;; synchronous remote status RPCs here: callers such as mode-line
       ;; redisplay, Flymake, and LSP process management may ask for process
       ;; status while the user is typing.  The relay stays live until the
       ;; remote output has been delivered, so the remote exit status only
       ;; applies after that.
       ((and (not (process-get proc :tramp-rpc-exited))
             (memq (funcall orig-fun proc) '(run open listen connect)))
        'run)
       ;; A remote signal death is reported as `signal', matching local
       ;; processes.  Callers such as LSP and compile branch on this.
       ((integerp (process-get proc :tramp-rpc-exit-signal)) 'signal)
       (t 'exit))
    (funcall orig-fun process)))

(defun tramp-rpc--process-exit-status-advice (orig-fun process)
  "Return the remote exit status of RPC-managed PROCESS.
Signal deaths report the signal number, like local processes.  Call
ORIG-FUN for other processes."
  (if (and (processp process) (process-get process :tramp-rpc-pid))
      (or (process-get process :tramp-rpc-exit-signal)
          (process-get process :tramp-rpc-exit-code)
          0)
    (funcall orig-fun process)))

(defun tramp-rpc--process-command-advice (orig-fun process)
  "Return the remote command of RPC PROCESS, otherwise call ORIG-FUN."
  (or (and (processp process) (process-get process :tramp-rpc-command))
      (funcall orig-fun process)))

(defun tramp-rpc--process-tty-name-advice (orig-fun process &optional stream)
  "Return the remote TTY name of RPC PTY PROCESS, otherwise call ORIG-FUN.
Direct SSH PTYs use their local PTY name.  STREAM is passed to ORIG-FUN."
  (if (and (processp process)
           (process-get process :tramp-rpc-pty)
           (not (process-get process :tramp-rpc-direct-ssh)))
      (process-get process :tramp-rpc-tty-name)
    (funcall orig-fun process stream)))

(defun tramp-rpc--set-process-sentinel-advice (orig-fun process sentinel)
  "Store SENTINEL as the caller's sentinel of RPC PROCESS.
Replacing tramp-rpc's own sentinel would lose the remote exit status and the
remote cleanup, so its own sentinel calls SENTINEL instead.  Call ORIG-FUN
for other processes."
  (if (and (processp process) (process-get process :tramp-rpc-own-sentinel))
      (progn
        (process-put process :tramp-rpc-user-sentinel sentinel)
        sentinel)
    (funcall orig-fun process sentinel)))

(defun tramp-rpc--process-sentinel-advice (orig-fun process)
  "Return the caller's sentinel of RPC PROCESS, otherwise call ORIG-FUN.
Return `ignore' when there is none: tramp-rpc never runs the default
sentinel."
  (if (and (processp process) (process-get process :tramp-rpc-own-sentinel))
      (or (process-get process :tramp-rpc-user-sentinel) #'ignore)
    (funcall orig-fun process)))

;; ============================================================================
;; VC integration handler
;; ============================================================================

;; VC backends like vc-git-state use process-file internally, but they don't
;; set default-directory to the remote file's directory.  This means
;; process-file runs locally instead of going through our tramp handler.  We
;; fix this by advising vc-call-backend to set default-directory when the
;; file is remote.

(defconst tramp-rpc--vc-file-operations
  '(registered state state-heuristic dir-status-files working-revision
    previous-revision next-revision responsible-p)
  "VC backend operations that take a file and may call `process-file'.")

(defun tramp-rpc--vc-call-backend-advice (orig-fun backend function-name
                                                   &rest args)
  "Run VC FUNCTION-NAME for a TRAMP-RPC file in that file's directory.
VC backends run `process-file' in `default-directory', so set it to the
directory of the file argument.  ORIG-FUN is `vc-call-backend', called with
BACKEND, FUNCTION-NAME and ARGS."
  (let ((file (car args)))
    (if (and (memq function-name tramp-rpc--vc-file-operations)
             (stringp file)
             (tramp-rpc--managed-file-name-p file))
        (let ((default-directory (file-name-directory file)))
          (apply orig-fun backend function-name args))
      (apply orig-fun backend function-name args))))

(defun tramp-rpc--vc-exec-after-managed (code okstatus proc)
  "Run CODE once TRAMP-RPC relay PROC is done, using its logical state.
OKSTATUS is as for `vc-exec-after'."
  (if (or (process-get proc :tramp-rpc-exited)
          (not (memq (process-status proc) '(run open listen connect))))
      (progn
        ;; Match `vc-exec-after': drain pending output before the next VC
        ;; stage.  Use zero-timeout accepts so we drain what is immediately
        ;; available without blocking callers such as Dired/diff-hl that
        ;; run with `inhibit-quit' bound; Emacs 30 warns about blocking
        ;; `accept-process-output' in that context.
        (while (accept-process-output proc 0 nil t))
        (when (cond
               ((null okstatus) t)
               ((processp okstatus) (zerop (process-exit-status okstatus))) ; Emacs 30
               ((integerp okstatus) (<= (process-exit-status proc) okstatus))) ; Emacs 31+
          (if (functionp code) (funcall code) (eval code t))))
    (vc-set-mode-line-busy-indicator)
    (letrec ((fun (lambda (p _msg)
                    (remove-function (process-sentinel p) fun)
                    (when-let* ((buf (process-buffer p))
                                ((buffer-live-p buf)))
                      (with-current-buffer buf
                        (tramp-rpc--vc-exec-after-managed code okstatus p))))))
      (add-function :after (process-sentinel proc) fun)))
  nil)

(defun tramp-rpc--vc-exec-after-advice (orig-fun code &rest args)
  "Run CODE after a TRAMP-RPC relay using its logical process state.

Some native-compiled VC functions can observe the raw local relay process
state instead of the logical state provided by the `process-status' advice.
A short-lived remote command can leave the local cat relay in a non-`run' and
non-`exit' state while TRAMP-RPC has already recorded the remote exit.  The
stock `vc-exec-after' then signals \"Unexpected process state\".  For
TRAMP-RPC processes, reproduce `vc-exec-after' using the logical process
state.  ORIG-FUN is `vc-exec-after'; ARGS are its OKSTATUS and PROC."
  (let ((proc (or (nth 1 args) (get-buffer-process (current-buffer)))))
    (if (and (processp proc) (process-get proc :tramp-rpc-pid))
        (tramp-rpc--vc-exec-after-managed code (car args) proc)
      (apply orig-fun code args))))

;; ============================================================================
;; Python shell integration
;; ============================================================================

(defun tramp-rpc-handle-python-shell--tramp-with-environment
    (_vec extraenv bodyfun)
  "Run Python shell BODYFUN without tramp-sh environment refresh for RPC.

`python-shell--tramp-with-environment' assumes that every TRAMP connection
has an interactive shell and refreshes environment variables by calling
`tramp-send-command'.  A tramp-rpc connection process is the MessagePack RPC
server, not a shell, so sending shell snippets to it can block commands such as
`run-python'.  For rpc connections, keep the requested environment in a
dynamic `tramp-remote-process-environment' binding and let tramp-rpc's process
handlers pass it directly to the server.
EXTRAENV is a list of extra environment bindings."
  (let ((tramp-remote-process-environment
         (append extraenv tramp-remote-process-environment)))
    (funcall bodyfun)))

(defun tramp-rpc-handle-python-shell--tramp-with-environment-compat
    (orig-fun vec extraenv bodyfun)
  "Compatibility advice for older Tramp versions.

Tramp 2.8.2 adds `tramp-file-name' as ARG-TYPE for
`tramp-add-external-operation', which lets
`python-shell--tramp-with-environment' be registered as a regular Tramp
external operation.  Until tramp-rpc can require Tramp 2.8.2, keep this
fallback so released Tramp versions do not fail when python.el is loaded.
ORIG-FUN is the original advised function.
VEC is the TRAMP connection vector.
EXTRAENV is a list of extra environment bindings.
BODYFUN performs the wrapped operation."
  (if (tramp-rpc-file-name-p vec)
      (tramp-rpc-handle-python-shell--tramp-with-environment
       vec extraenv bodyfun)
    (funcall orig-fun vec extraenv bodyfun)))

(defun tramp-rpc--python-tramp-file-name-external-operation-p ()
  "Return non-nil if Tramp supports `tramp-file-name' external ARG-TYPE."
  (let ((tramp-file-name-for-operation-external
         (cons '(tramp-rpc--external-operation-probe . tramp-file-name)
               tramp-file-name-for-operation-external))
        (debug-ignored-errors (cons 'remote-file-error debug-ignored-errors)))
    (condition-case nil
        (stringp
         (tramp-file-name-for-operation
          'tramp-rpc--external-operation-probe
          (make-tramp-file-name :method "rpc" :host "host" :localname "/")))
      (remote-file-error nil))))

;; ============================================================================
;; Eglot integration
;; ============================================================================

;; Eglot wraps remote commands with `/bin/sh -c "stty raw > /dev/null; cmd"`
;; to disable line buffering. This doesn't work with tramp-rpc because:
;; 1. Our pipe processes don't have a TTY, so stty fails
;; 2. We don't need this workaround - our RPC handles binary data correctly
;;
;; This handler bypasses the shell wrapper for tramp-rpc connections.

(defun tramp-rpc--eglot--cmd-advice (orig-fun contact &rest args)
  "Return CONTACT unwrapped for TRAMP-RPC, otherwise call ORIG-FUN.
TRAMP-RPC uses pipes, not PTYs, and handles binary data correctly, so the
shell wrapper is not needed.  ARGS are passed to ORIG-FUN."
  (if (tramp-rpc-file-name-p default-directory)
      contact
    (apply orig-fun contact args)))

;; ============================================================================
;; Magit: force pipe mode for stdin piping
;; ============================================================================

;; When `magit-tramp-pipe-stty-settings' is `pty', magit forces PTY mode for
;; all remote processes, including `git apply' which reads a patch from stdin.
;; On a PTY, `process-send-eof' sends Ctrl-D instead of closing the pipe.
;; Ctrl-D only signals EOF when the line buffer is empty; if the patch data
;; doesn't end at a line boundary the first Ctrl-D just flushes the buffer
;; and git waits for more input — hanging Emacs.
;;
;; The `pty' workaround exists for tramp-sh (#4720, #5220) where pipe stty
;; settings broke hunk staging.  tramp-rpc doesn't need it: stdin data goes
;; via RPC (pipe processes) or direct SSH pipes, both of which handle EOF
;; correctly.  This handler forces pipe mode only for tramp-rpc connections
;; when input will be piped, leaving other TRAMP methods untouched.

;; Declared special so the dynamic let-binding in the handler below
;; is not flagged as an unused lexical variable by the byte-compiler.
(defvar magit-tramp-pipe-stty-settings)

(defun tramp-rpc--magit-start-process-advice (orig-fun program
                                                      &optional input
                                                      &rest args)
  "Force pipe mode for TRAMP-RPC when INPUT will be piped to the process.
PTY mode breaks stdin piping because `process-send-eof' sends Ctrl-D
which does not close the pipe — git waits for more input forever.
ORIG-FUN is `magit-start-process', called with PROGRAM, INPUT and ARGS."
  (if (and input (tramp-rpc--managed-file-name-p default-directory))
      ;; magit-start-process uses a pipe when this is "".
      (let ((magit-tramp-pipe-stty-settings ""))
        (apply orig-fun program input args))
    (apply orig-fun program input args)))

;; ============================================================================
;; vc-dir stale-process guard
;; ============================================================================

;; `vc-dir-busy' tests (get-buffer-process vc-dir-process-buffer).
;; In Emacs, `get-buffer-process' returns ANY process associated with the
;; buffer -- including exited ones -- as long as `delete-process' has not
;; been called.  Normally `tramp-rpc--pipe-relay-sentinel' deletes the exited
;; relay, but if its timer hasn't fired yet (or if the cat relay got stuck),
;; the stale process causes "Another update process is in progress".
;; This handler acts as a safety net: before `vc-dir-refresh' checks the
;; busy flag, we delete any exited tramp-rpc relay process from the buffer.

(defun tramp-rpc--vc-dir-refresh-advice (orig-fun &rest args)
  "Delete an exited TRAMP-RPC relay of the `vc-dir' buffer, then ORIG-FUN.
The relay would otherwise make the refresh report a busy update process.
ARGS are passed to ORIG-FUN."
  (when-let* (((bound-and-true-p vc-dir-process-buffer))
              ((buffer-live-p vc-dir-process-buffer))
              (proc (get-buffer-process vc-dir-process-buffer))
              ((process-get proc :tramp-rpc-pid))
              ((or (process-get proc :tramp-rpc-exited)
                   (not (process-live-p proc)))))
    (tramp-rpc--forget-managed-process proc)
    (delete-process proc))
  (apply orig-fun args))

;; ============================================================================
;; Install and uninstall handler
;; ============================================================================

(defconst tramp-rpc--advice
  '(;; Process primitives, routed by process ownership.
    (process-send-string . tramp-rpc--process-send-string-advice)
    (process-send-region . tramp-rpc--process-send-region-advice)
    (process-send-eof . tramp-rpc--process-send-eof-advice)
    (process-status . tramp-rpc--process-status-advice)
    (process-exit-status . tramp-rpc--process-exit-status-advice)
    (process-command . tramp-rpc--process-command-advice)
    (process-tty-name . tramp-rpc--process-tty-name-advice)
    (set-process-sentinel . tramp-rpc--set-process-sentinel-advice)
    (process-sentinel . tramp-rpc--process-sentinel-advice)
    (set-process-coding-system . tramp-rpc--set-process-coding-system-advice)
    (process-coding-system . tramp-rpc--process-coding-system-advice)
    (vterm--window-adjust-process-window-size
     . tramp-rpc--vterm-window-adjust-advice)
    (eat--adjust-process-window-size . tramp-rpc--eat-window-adjust-advice)
    ;; Integrations, which wrap the original call for TRAMP-RPC files.
    (vc-call-backend . tramp-rpc--vc-call-backend-advice)
    (vc-exec-after . tramp-rpc--vc-exec-after-advice)
    (vc-dir-refresh . tramp-rpc--vc-dir-refresh-advice)
    (eglot--cmd . tramp-rpc--eglot--cmd-advice)
    (magit-start-process . tramp-rpc--magit-start-process-advice))
  "Functions tramp-rpc advises, with their advice.
Advice, unlike a Tramp external operation, calls the original function
directly: Tramp runs it under `tramp-run-real-handler', which disables the
handler for nested calls of the same operation, including callbacks run while
it waits.  Advising a function that is not loaded yet takes effect when it
is defined.")

(defun tramp-rpc-handler-install ()
  "Install all process handler for tramp-rpc."
  (pcase-dolist (`(,function . ,advice) tramp-rpc--advice)
    ;; Outermost, so ownership is checked before any Tramp dispatch.
    (advice-add function :around advice '((depth . -10))))
  ;; This must be before `tramp-signal-process'.  Since tramp.el is
  ;; required, this is guaranteed.
  (add-hook 'signal-process-functions #'tramp-rpc-handle-signal-process)
  ;; Likewise before `tramp-interrupt-process'.
  (add-hook 'interrupt-process-functions #'tramp-rpc-handle-interrupt-process))

(defun tramp-rpc-advice-install-optional-handlers ()
  "Install handlers for loaded optional integration packages."
  (when (featurep 'python)
      (if (tramp-rpc--python-tramp-file-name-external-operation-p)
          (condition-case err
              (tramp-rpc--add-external-operation
               'python-shell--tramp-with-environment
               #'tramp-rpc-handle-python-shell--tramp-with-environment
               'tramp-rpc 'tramp-file-name)
            (remote-file-error
             (signal (car err) (cdr err))))
        ;; Compatibility fallback for Tramp < 2.8.2.  Remove this advice
        ;; path once tramp-rpc can require a Tramp release supporting
        ;; `tramp-file-name' ARG-TYPE for external operations.
        (unless (advice-member-p
                 #'tramp-rpc-handle-python-shell--tramp-with-environment-compat
                 'python-shell--tramp-with-environment)
          (advice-add
           'python-shell--tramp-with-environment
           :around
           #'tramp-rpc-handle-python-shell--tramp-with-environment-compat)))))

(defun tramp-rpc-handler-remove ()
  "Remove all process handler installed by tramp-rpc."
  (pcase-dolist (`(,function . ,advice) tramp-rpc--advice)
    (advice-remove function advice))
  (remove-hook 'signal-process-functions #'tramp-rpc-handle-signal-process)
  (remove-hook 'interrupt-process-functions #'tramp-rpc-handle-interrupt-process)
  (tramp-rpc--remove-external-operation
   'python-shell--tramp-with-environment 'tramp-rpc)
  (when (fboundp 'python-shell--tramp-with-environment)
    (advice-remove
     'python-shell--tramp-with-environment
     #'tramp-rpc-handle-python-shell--tramp-with-environment-compat)))


;; ============================================================================
;; Unload support
;; ============================================================================

(defun tramp-rpc-advice-unload-function ()
  "Unload function for tramp-rpc-advice.
Removes handler."
  ;; Remove all handler.
  (tramp-rpc-handler-remove)
  ;; Return nil to allow normal unload to proceed
  nil)

(provide 'tramp-rpc-advice)
;;; tramp-rpc-advice.el ends here
