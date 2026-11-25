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
(require 'tramp-rpc-cache)
(require 'tramp-rpc-process)

;; Functions from tramp-rpc-process.el
(declare-function tramp-rpc--forget-managed-process "tramp-rpc-process" (process))

;; Variables from tramp-rpc-process.el
(defvar tramp-rpc--bypass-next-relay-call)

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
  (when-let* ((remote (tramp-rpc--remote-process process)))
    (tramp-rpc-remote-process-coding remote)))

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
        (setf (tramp-rpc-remote-process-coding
               (tramp-rpc--remote-process process))
              (cons decoding effective-encoding)))
    (funcall orig-fun process decoding encoding)))

(defun tramp-rpc--process-coding-system-advice (orig-fun process)
  "Return a relay's requested coding pair rather than its binary write side.
ORIG-FUN is the original advised function.
PROCESS is the process being handled."
  (or (tramp-rpc--process-coding-advised-p process)
      (funcall orig-fun process)))

;; ============================================================================
;; Process routing
;; ============================================================================

;; Process primitives are routed by process ownership in outermost advice,
;; not through Tramp's file name handlers.  Ownership is a property of the
;; process object, and handler dispatch would run the native primitive under
;; `tramp-run-real-handler', whose inhibition leaks into filters and timers
;; run while a blocked write waits.  Internal relay plumbing bypasses the
;; advice per call via the one-shot `tramp-rpc--bypass-next-relay-call' token.

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
  (let* ((remote (tramp-rpc--remote-process proc))
         (vec (tramp-rpc-remote-process-vec remote))
         (pid (tramp-rpc-remote-process-pid remote))
         (data (tramp-rpc--encode-process-input proc string)))
    (if (tramp-rpc-remote-process-pty remote)
        (progn
          (tramp-rpc--debug "%s PTY pid=%s len=%d" operation pid (length string))
          (tramp-rpc--call vec "process.write_pty"
                           `((pid . ,pid) (data . ,(msgpack-bin-make data)))
                           (tramp-rpc-remote-process-connection remote)))
      (tramp-rpc--debug "%s pipe pid=%s len=%d" operation pid (length string))
      (tramp-rpc--write-remote-process vec pid data proc)
      (when tramp-rpc-synchronous-pipe-writes
        (tramp-rpc--drain-write-queue vec pid proc))))
  nil)

(defun tramp-rpc--managed-send-process (process)
  "Return PROCESS's RPC-managed process, or nil when native delivery is needed."
  (when-let* ((proc (tramp-rpc--resolve-process process))
              (remote (tramp-rpc--remote-process proc)))
    (and (not (tramp-rpc-remote-process-direct-ssh remote))
         (tramp-rpc-remote-process-pid remote)
         (tramp-rpc-remote-process-vec remote)
         proc)))

(defun tramp-rpc--process-send-string-advice (orig-fun process string)
  "Send STRING to RPC-managed PROCESS, otherwise call ORIG-FUN."
  (if (and tramp-rpc--bypass-next-relay-call
           (eq process tramp-rpc--bypass-next-relay-call))
      (let ((tramp-rpc--bypass-next-relay-call nil))
        (funcall orig-fun process string))
    (if-let* ((proc (tramp-rpc--managed-send-process process)))
        (tramp-rpc--send-managed-process-string proc string "SEND-STRING")
      (funcall orig-fun process string))))

(defun tramp-rpc--process-send-region-advice (orig-fun process start end)
  "Send region START to END to RPC-managed PROCESS, otherwise call ORIG-FUN."
  (if-let* ((proc (tramp-rpc--managed-send-process process)))
      (tramp-rpc--send-managed-process-string
       proc (buffer-substring-no-properties start end) "SEND-REGION")
    (funcall orig-fun process start end)))

(defun tramp-rpc--process-send-eof-advice (orig-fun &optional process)
  "Close the remote stdin of RPC-managed PROCESS, otherwise call ORIG-FUN."
  (if (and tramp-rpc--bypass-next-relay-call
           (eq process tramp-rpc--bypass-next-relay-call))
      (let ((tramp-rpc--bypass-next-relay-call nil))
        (apply orig-fun (and process (list process))))
    (if-let* ((proc (tramp-rpc--managed-send-process process)))
        (let* ((remote (tramp-rpc--remote-process proc))
               (pid (tramp-rpc-remote-process-pid remote))
               (vec (tramp-rpc-remote-process-vec remote)))
          ;; Short-lived processes (like git apply) may exit before the caller
          ;; sends EOF, which is fine: stdin was already closed on exit.
          (unless (or (tramp-rpc-remote-process-exited remote)
                      (not (process-live-p proc)))
            (if (tramp-rpc-remote-process-pty remote)
                ;; PTY processes: send Ctrl-D (EOF character) via the PTY.
                (tramp-rpc--call vec "process.write_pty"
                                 `((pid . ,pid)
                                   (data . ,(msgpack-bin-make (string ?\C-d))))
                                 (tramp-rpc-remote-process-connection remote))
              ;; Pipe processes: a queue failure must reach the caller.
              (tramp-rpc--close-remote-stdin vec pid proc))))
      (apply orig-fun (and process (list process))))))

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
  (let (pid vec rpc-process remote-state)
    (cond
     ((processp process)
      (setq remote-state (tramp-rpc--remote-process process)
            pid (and remote-state (tramp-rpc-remote-process-pid remote-state))
            vec (and remote-state (tramp-rpc-remote-process-vec remote-state))
            rpc-process process))
     ((and (numberp process)
           (stringp remote)
           (tramp-rpc--managed-file-name-p remote))
      (setq pid process
            vec (tramp-dissect-file-name remote))))
    (when (and pid vec)
      (let ((connection (and remote-state
                             (tramp-rpc-remote-process-connection remote-state))))
        (condition-case err
            (progn
              (cond
               ;; A numeric PID names an arbitrary remote OS process, not an
               ;; entry in the RPC server's managed-process registry.
               ((not rpc-process)
                (tramp-rpc--call vec "process.signal"
                                 `((pid . ,pid) (signal . ,sigcode))))
               ;; Use PTY kill for PTY processes, regular kill for pipes.
               ((tramp-rpc-remote-process-pty remote-state)
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
              (remote (tramp-rpc--remote-process proc))
              ((or (tramp-rpc-remote-process-direct-ssh remote)
                   (tramp-rpc-remote-process-pid remote))))
    (unless (process-live-p proc)
      (error "Process %s is not active" (process-name proc)))
    (if (tramp-rpc-remote-process-direct-ssh remote)
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
            (remote (tramp-rpc--remote-process proc))
            ((tramp-rpc-remote-process-pid remote)))
      (cond
       ;; Check local relay liveness with ORIG-FUN.  Do not perform
       ;; synchronous remote status RPCs here: callers such as mode-line
       ;; redisplay, Flymake, and LSP process management may ask for process
       ;; status while the user is typing.  The relay stays live until the
       ;; remote output has been delivered, so the remote exit status only
       ;; applies after that.
       ((and (not (tramp-rpc-remote-process-exited remote))
             (memq (funcall orig-fun proc) '(run open listen connect)))
        'run)
       ;; A remote signal death is reported as `signal', matching local
       ;; processes.  Callers such as LSP and compile branch on this.
       ((integerp (tramp-rpc-remote-process-exit-signal remote)) 'signal)
       (t 'exit))
    (funcall orig-fun process)))

(defun tramp-rpc--process-exit-status-advice (orig-fun process)
  "Return the remote exit status of RPC-managed PROCESS.
Signal deaths report the signal number, like local processes.  Call
ORIG-FUN for other processes."
  (if-let* ((remote (tramp-rpc--remote-process process))
            ((tramp-rpc-remote-process-pid remote)))
      (or (tramp-rpc-remote-process-exit-signal remote)
          (tramp-rpc-remote-process-exit-code remote)
          0)
    (funcall orig-fun process)))

(defun tramp-rpc--process-command-advice (orig-fun process)
  "Return the remote command of RPC PROCESS, otherwise call ORIG-FUN."
  (or (when-let* ((remote (tramp-rpc--remote-process process)))
        (tramp-rpc-remote-process-command remote))
      (funcall orig-fun process)))

(defun tramp-rpc--process-tty-name-advice (orig-fun process &optional stream)
  "Return the remote TTY name of RPC PTY PROCESS, otherwise call ORIG-FUN.
Direct SSH PTYs use their local PTY name.  STREAM is passed to ORIG-FUN."
  (if (tramp-rpc--rpc-pty-process-p process)
      (tramp-rpc-remote-process-tty-name
       (tramp-rpc--remote-process process))
    (funcall orig-fun process stream)))

(defun tramp-rpc--set-process-sentinel-advice (orig-fun process sentinel)
  "Store SENTINEL as the caller's sentinel of RPC PROCESS.
Replacing tramp-rpc's own sentinel would lose the remote exit status and the
remote cleanup, so its own sentinel calls SENTINEL instead.  Call ORIG-FUN
for other processes."
  (if-let* ((remote (tramp-rpc--remote-process process))
            ((tramp-rpc-remote-process-own-sentinel remote)))
      (progn
        (setf (tramp-rpc-remote-process-user-sentinel remote) sentinel)
        sentinel)
    (funcall orig-fun process sentinel)))

(defun tramp-rpc--process-sentinel-advice (orig-fun process)
  "Return the caller's sentinel of RPC PROCESS, otherwise call ORIG-FUN.
Return `ignore' when there is none: tramp-rpc never runs the default
sentinel."
  (if-let* ((remote (tramp-rpc--remote-process process))
            ((tramp-rpc-remote-process-own-sentinel remote)))
      (or (tramp-rpc-remote-process-user-sentinel remote) #'ignore)
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
  (if (or (when-let* ((remote (tramp-rpc--remote-process proc)))
            (tramp-rpc-remote-process-exited remote))
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
               ((processp okstatus) ; Emacs 30
                (and (eq (process-status okstatus) 'exit)
                     (zerop (process-exit-status okstatus))))
               ((integerp okstatus) ; Emacs 31+
                (and (eq (process-status proc) 'exit)
                     (<= (process-exit-status proc) okstatus))))
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
    (if (when-let* ((remote (tramp-rpc--remote-process proc)))
          (tramp-rpc-remote-process-pid remote))
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
  (if (tramp-rpc--managed-file-name-p vec)
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
  (if (tramp-rpc--managed-file-name-p default-directory)
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
              (remote (tramp-rpc--remote-process proc))
              ((tramp-rpc-remote-process-pid remote))
              ((or (tramp-rpc-remote-process-exited remote)
                   (not (process-live-p proc)))))
    (tramp-rpc--forget-managed-process proc)
    (delete-process proc))
  (apply orig-fun args))

;; ============================================================================
;; Route-preservation advice for multi-path file operations
;; ============================================================================

(defun tramp-rpc--matching-proxy-for-vec (vec proxies)
  "Return (t . PROXY) for the first rule in PROXIES matching VEC, or nil."
  (when (listp proxies)
    (let ((host-port (or (tramp-file-name-host-port vec) ""))
          (user-domain (or (tramp-file-name-user-domain vec) ""))
          (choices proxies)
          matched)
      (while (and choices (not matched))
        (let* ((item (pop choices))
               (host-re (or (eval (nth 0 item) t) ""))
               (user-re (or (eval (nth 1 item) t) "")))
          (when (and (string-match-p host-re host-port)
                     (string-match-p user-re user-domain))
            (let ((proxy (eval (nth 2 item) t)))
              (when (stringp proxy)
                ;; Same expansion as `tramp-format-spec', which package-lint
                ;; attributes to Emacs 31: keep "%" in USER%DOMAIN literal.
                (setq proxy
                      (format-spec
                       (replace-regexp-in-string
                        (rx "%" (group (= 2 alnum))) "%%\\1" proxy)
                       (format-spec-make
                        ?u (or (tramp-file-name-user vec) "")
                        ?h (or (tramp-file-name-host vec) "")))))
              (setq matched (cons t proxy))))))
      matched)))

(defun tramp-rpc--strip-conflicting-proxy-entries (vec proxies)
  "Return a copy of PROXIES without entries matching VEC's explicit hop targets.
Removing both conflicting rules and buried duplicates guarantees that
`tramp-add-hops' prepends VEC's explicit hops at the head of the alist."
  (if (not (listp proxies))
      proxies
    (let ((result (copy-sequence proxies))
          (hops (tramp-file-name-hop vec))
          (item vec))
      (when hops
        (dolist (proxy (reverse (split-string hops tramp-postfix-hop-regexp 'omit)))
          (let* ((host-port (tramp-file-name-host-port item))
                 (host-rx (and (stringp host-port)
                               (rx bol (literal host-port) eol)))
                 (user-domain (tramp-file-name-user-domain item))
                 (user-rx (and (stringp user-domain)
                               (rx bol (literal user-domain) eol)))
                 (proxy-str
                  (concat tramp-prefix-format proxy tramp-postfix-host-format)))
            (setq result
                  (cl-remove-if
                   (lambda (entry)
                     (and (consp entry)
                          (equal host-rx (car entry))
                          (equal user-rx (cadr entry))))
                   result))
            (setq item (tramp-dissect-file-name proxy-str)))))
      result)))

(defun tramp-rpc--native-effective-route (vec)
  "Return canonical hop-vector list for native TRAMP VEC without side effects.
Resolves VEC's explicit hops (if any) and `tramp-default-proxies-alist'
without calling `tramp-compute-multi-hops' or `tramp-add-hops'.  Returns
`cycle' when proxy resolution encounters a loop."
  (when (and (tramp-file-name-p vec)
             (listp tramp-default-proxies-alist)
             (not (tramp-rpc--managed-file-name-p vec))
             (tramp-sh-file-name-handler-p vec))
    (let* ((proxies (if (and tramp-rpc--inhibit-add-hops
                             (tramp-file-name-hop vec))
                        (tramp-rpc--strip-conflicting-proxy-entries
                         vec tramp-default-proxies-alist)
                      (tramp-rpc--copy-proxies-alist)))
           (target-alist (list (tramp-file-name-unify vec))))
      (when-let* ((hops (tramp-file-name-hop vec)))
        (let ((item vec))
          (dolist (proxy (reverse (split-string hops tramp-postfix-hop-regexp 'omit)))
            (let* ((host-port (tramp-file-name-host-port item))
                   (host-rx (and (stringp host-port)
                                 (rx bol (literal host-port) eol)))
                   (user-domain (tramp-file-name-user-domain item))
                   (user-rx (and (stringp user-domain)
                                 (rx bol (literal user-domain) eol)))
                   (proxy-str
                    (concat tramp-prefix-format proxy tramp-postfix-host-format))
                   (entry (list host-rx user-rx proxy-str)))
              (when (and (not tramp-rpc--inhibit-add-hops)
                         tramp-show-ad-hoc-proxies)
                ;; Match native `tramp-add-hops': remove only entries whose
                ;; proxy string differs, leaving an identical entry in place.
                (setq proxies
                      (cl-remove-if
                       (lambda (e)
                         (and (consp e)
                              (equal host-rx (car e))
                              (equal user-rx (cadr e))
                              (not (equal proxy-str (caddr e)))))
                       proxies)))
              (unless (member entry proxies)
                (push entry proxies))
              (setq item (tramp-dissect-file-name proxy-str))))))
      (let ((done nil))
        (while (not done)
          (let ((match (tramp-rpc--matching-proxy-for-vec
                        (car target-alist) proxies)))
            (if (or (null match) (null (cdr match)))
                (setq done t)
              (let ((proxy-vec (tramp-file-name-unify
                                (tramp-dissect-file-name (cdr match)))))
                (if (member proxy-vec target-alist)
                    ;; Signal cycle to caller so any live session is torn down
                    ;; and `tramp-compute-multi-hops' reports the error.
                    (setq target-alist 'cycle done t)
                  (push proxy-vec target-alist)))))))
      target-alist)))

(defvar tramp-rpc--tracked-native-routes (make-hash-table :test 'equal)
  "Map unified native TRAMP connection keys to their effective hop route.
Populated when a native `tramp-sh' connection opens or caches state so
subsequent operations cannot reuse a live session or stale cache from a
different route, even after the process exits.")

(defvar tramp-rpc--ensuring-native-route nil
  "Non-nil while `tramp-rpc--ensure-native-connection-route' is running.")

(defvar tramp-rpc--skip-native-route-keys nil
  "List of unified native TRAMP keys whose route checks are temporarily skipped.")

(defvar tramp-rpc--captured-native-route nil
  "Cons `(VEC . ROUTE)' capturing `tramp-compute-multi-hops' during connect.")

(defconst tramp-rpc--non-session-native-ops
  '(;; Comparisons need no connection by themselves; the lookups they make
    ;; (`file-truename', `file-attributes') are aligned when they run, so a
    ;; comparison across hosts never disconnects a session it does not use.
    file-equal-p
    file-in-directory-p
    file-newer-than-file-p
    directory-file-name
    dired-uncache
    expand-file-name
    file-name-as-directory
    file-name-directory
    file-name-nondirectory
    file-name-sans-versions
    file-remote-p
    find-backup-file-name
    make-auto-save-file-name
    make-lock-file-name
    substitute-in-file-name
    unhandled-file-name-directory)
  "`tramp-sh' ops that need no route check of their own.
They are syntactic, flush caches, or compare files through other
operations that are checked when they run.  Tilde expansion inside
`expand-file-name' still aligns routes via
`tramp-rpc--get-home-directory-advice'.")

(defun tramp-rpc--connection-property-is-default-p (key-name prop val)
  "Return non-nil if PROP/VAL still matches `tramp-connection-properties'.
KEY-NAME is the connection name matched against the configured rules.
Emacs `puthash' with `:test equal' keeps the original key string (and its
`tramp-default' text property) when a seeded property is later overwritten,
so we also verify that VAL still equals the configured default."
  (when (and (stringp prop)
             (get-text-property 0 'tramp-default prop))
    (let ((found nil))
      (dolist (elt tramp-connection-properties)
        (when (and (equal (nth 1 elt) prop)
                   (string-match-p (or (nth 0 elt) "") key-name))
          (setq found (cons t (nth 2 elt)))))
      (and (consp found)
           (equal val (cdr found))))))

(defun tramp-rpc--native-connection-has-cache-p (key)
  "Return non-nil if KEY has live cached entries in `tramp-cache-data'.
Ignores empty hash tables created on lookup, transient process slots, and
untouched default entries seeded from `tramp-connection-properties'."
  (and (hash-table-p tramp-cache-data)
       (catch 'tramp-rpc--found
         (maphash
          (lambda (cache-key table)
            (when (and (tramp-file-name-p cache-key)
                       (hash-table-p table)
                       (> (hash-table-count table) 0)
                       (tramp-file-name-equal-p cache-key key))
              (if (tramp-file-name-localname cache-key)
                  (throw 'tramp-rpc--found t)
                (let ((key-name nil))
                  (maphash
                   (lambda (prop val)
                     (unless (or (member prop '(" process-name" " process-buffer"))
                                 (and (stringp prop)
                                      (get-text-property 0 'tramp-default prop)
                                      (tramp-rpc--connection-property-is-default-p
                                       (or key-name
                                           (setq key-name
                                                 (let ((tramp-default-proxies-alist
                                                        tramp-cache-undefined))
                                                   (tramp-make-tramp-file-name
                                                    cache-key 'noloc))))
                                       prop val)))
                       (throw 'tramp-rpc--found t)))
                   table)))))
          tramp-cache-data)
         nil)))

(defun tramp-rpc--forget-tracked-native-route (key)
  "Forget any tracked native route for KEY once no live process or cache stays."
  (when (and (tramp-file-name-p key)
             (> (hash-table-count tramp-rpc--tracked-native-routes) 0))
    (let ((unified (tramp-file-name-unify key)))
      (when (and (gethash unified tramp-rpc--tracked-native-routes)
                 (not (process-live-p (tramp-get-connection-process unified)))
                 (not (tramp-rpc--native-connection-has-cache-p unified)))
        (remhash unified tramp-rpc--tracked-native-routes)))))

(defun tramp-rpc--ensure-native-connection-route (vec)
  "Ensure any live native TRAMP session or cache for VEC matches its route.
Returns VEC's desired canonical route when tracking applies, or nil.
Cleans up a live session and cached properties whose route differs from
VEC's requested route (or when the requested route is cyclic), and records
matching routes so subsequent operations know which route the live process
and caches belong to.  State without a recorded route never matches: the
current proxy rules need not describe how an older session was opened.
When `non-essential' is non-nil, a mismatch throws `non-essential' instead
of disconnecting, so a probe takes TRAMP's fallback for such calls."
  (when (and (not tramp-rpc--ensuring-native-route)
             (or tramp-rpc--inhibit-add-hops
                 (> (hash-table-count tramp-rpc--tracked-native-routes) 0))
             (tramp-file-name-p vec)
             (not (tramp-rpc--managed-file-name-p vec))
             (tramp-sh-file-name-handler-p vec))
    (let* ((tramp-rpc--ensuring-native-route t)
           (key (tramp-file-name-unify vec)))
      (unless (cl-member key tramp-rpc--skip-native-route-keys
                         :test #'tramp-file-name-equal-p)
        (let* ((proc (tramp-get-connection-process vec))
               (live-p (process-live-p proc))
               (proc-route (and live-p (process-get proc 'tramp-rpc-native-route)))
               (tracked-route (gethash key tramp-rpc--tracked-native-routes))
               (recorded-route (or proc-route tracked-route))
               (has-hop (and (tramp-file-name-hop vec) t)))
          (when (or (and tramp-rpc--inhibit-add-hops has-hop)
                    recorded-route)
            (let ((desired-route (tramp-rpc--native-effective-route vec)))
              (when desired-route
                (let ((current-route recorded-route))
                  (if (and (consp desired-route)
                           (equal current-route desired-route))
                      (progn
                        (when (and live-p (not proc-route))
                          (process-put proc 'tramp-rpc-native-route desired-route))
                        (when (and tramp-rpc--inhibit-add-hops has-hop)
                          (puthash key desired-route tramp-rpc--tracked-native-routes)))
                    (when (or live-p (tramp-rpc--native-connection-has-cache-p key))
                      ;; A probe cannot reopen the requested route, so keep
                      ;; the other route's session and caches intact.
                      (when non-essential
                        (throw 'non-essential 'non-essential))
                      (let ((tramp-rpc--inhibit-add-hops t))
                        (tramp-cleanup-connection
                         vec 'keep-debug 'keep-password 'keep-processes))
                      (when (process-live-p proc)
                        (ignore-errors (delete-process proc))))
                    (remhash key tramp-rpc--tracked-native-routes)
                    (when (and tramp-rpc--inhibit-add-hops has-hop
                               (consp desired-route))
                      (puthash key desired-route tramp-rpc--tracked-native-routes)))))
              (and (consp desired-route) desired-route))))))))

(defun tramp-rpc--inhibit-add-hops-advice (orig-fun &rest args)
  "Call ORIG-FUN with ARGS unless `tramp-add-hops' is suppressed.
Suppression applies while inhibited or skipped for the connection of the
file name vector in ARGS."
  (let ((vec (car args)))
    (unless (or tramp-rpc--inhibit-add-hops
                (and tramp-rpc--skip-native-route-keys
                     (tramp-file-name-p vec)
                     (cl-member vec tramp-rpc--skip-native-route-keys
                                :test #'tramp-file-name-equal-p)))
      (apply orig-fun args))))

(defun tramp-rpc--compute-multi-hops-advice (orig-fun vec)
  "Allow native TRAMP `tramp-compute-multi-hops' to expand VEC's explicit hops.
ORIG-FUN is the original `tramp-compute-multi-hops'.  While
`tramp-rpc--inhibit-add-hops' is active, native TRAMP methods (such as
`ssh' and `scpx') still rely on `tramp-add-hops' to build their hop chain.
Run that expansion against an isolated temporary copy of
`tramp-default-proxies-alist' stripped of conflicting entries for VEC's
explicit hops so jump hosts are preserved without mutating or redirecting
any other route."
  (let ((result
         (if (and tramp-rpc--inhibit-add-hops
                  (tramp-file-name-p vec)
                  (tramp-file-name-hop vec)
                  (not (tramp-rpc--managed-file-name-p vec)))
             (let ((tramp-rpc--inhibit-add-hops nil)
                   (tramp-show-ad-hoc-proxies nil)
                   (tramp-save-ad-hoc-proxies nil)
                   (tramp-default-proxies-alist
                    (tramp-rpc--strip-conflicting-proxy-entries
                     vec tramp-default-proxies-alist)))
               (funcall orig-fun vec))
           (funcall orig-fun vec))))
    (when (and (consp tramp-rpc--captured-native-route)
               (tramp-file-name-p vec)
               (tramp-file-name-equal-p vec (car tramp-rpc--captured-native-route))
               (listp result)
               result)
      (setcdr tramp-rpc--captured-native-route
              (mapcar #'tramp-file-name-unify result)))
    result))

(defun tramp-rpc--maybe-open-connection-advice (orig-fun vec)
  "Align native TRAMP connections with VEC's effective hop route.
ORIG-FUN is the original `tramp-maybe-open-connection'.  Records the
computed route on every newly opened native process and in
`tramp-rpc--tracked-native-routes' so repeated operations reuse the session
while subsequent hopless, cyclic, or different-route operations cannot reuse
a mismatched live session or stale cache after disconnect."
  (let* ((native-p (and (tramp-file-name-p vec)
                        (not (tramp-rpc--managed-file-name-p vec))
                        (tramp-sh-file-name-handler-p vec)))
         (old-proc (and native-p (tramp-get-connection-process vec)))
         (desired-route (and native-p
                             (tramp-rpc--ensure-native-connection-route vec)))
         (tramp-rpc--captured-native-route (and native-p (cons vec nil))))
    (unwind-protect
        (funcall orig-fun vec)
      (when native-p
        (let* ((new-proc (tramp-get-connection-process vec))
               (new-live-p (and (process-live-p new-proc)
                                (not (eq new-proc old-proc))))
               (key (tramp-file-name-unify vec))
               (captured (cdr tramp-rpc--captured-native-route)))
          (when (or new-live-p
                    (and captured
                         (tramp-rpc--native-connection-has-cache-p key)))
            (let ((opened-route
                   (or captured
                       (and (consp desired-route) desired-route)
                       (let ((eff (tramp-rpc--native-effective-route vec)))
                         (and (consp eff) eff)))))
              (when opened-route
                (when new-live-p
                  (process-put new-proc 'tramp-rpc-native-route opened-route))
                (puthash key opened-route tramp-rpc--tracked-native-routes)))))))))

(defun tramp-rpc--get-home-directory-advice (orig-fun vec &optional user)
  "Align live native TRAMP sessions for VEC before ORIG-FUN resolves `~'.
USER is passed to ORIG-FUN.  When `non-essential' is non-nil and VEC's
session belongs to another route, return nil: the home directory is
unknown, and neither that route's cache nor its session is touched.
This can run outside TRAMP's dispatcher, which otherwise catches the
`non-essential' throw."
  (unless (eq (catch 'non-essential
                (tramp-rpc--ensure-native-connection-route vec)
                'aligned)
              'non-essential)
    (funcall orig-fun vec user)))

(defun tramp-rpc--dissect-target-no-hops (target &optional allow-relative)
  "Dissect remote filename or `tramp-file-name' TARGET without mutating proxies.
When ALLOW-RELATIVE is non-nil, resolve relative TARGET against
`default-directory'."
  (cond
   ((tramp-file-name-p target) target)
   ((stringp target)
    (let ((effective (if (or (not allow-relative)
                             (file-name-absolute-p target))
                         target
                       default-directory)))
      (when (stringp effective)
        (let ((tramp-default-proxies-alist tramp-cache-undefined))
          (let ((unquoted (if (file-name-quoted-p effective)
                              (file-name-unquote effective)
                            effective)))
            (when (and (stringp unquoted)
                       (tramp-tramp-file-p unquoted))
              (ignore-errors (tramp-dissect-file-name unquoted))))))))))

(defun tramp-rpc--ensure-native-route-for-target (target)
  "Ensure native TRAMP route alignment for filename or `tramp-file-name' TARGET."
  (when-let* ((vec (tramp-rpc--dissect-target-no-hops target 'allow-relative)))
    (tramp-rpc--ensure-native-connection-route vec)))

(defun tramp-rpc--sh-file-name-handler-advice (orig-fun operation &rest args)
  "Align live native TRAMP sessions and caches, then call ORIG-FUN.
OPERATION and ARGS are the `tramp-sh' file name handler arguments."
  (let ((skip-keys tramp-rpc--skip-native-route-keys))
    (when (and (not tramp-rpc--ensuring-native-route)
               (not (memq operation tramp-rpc--non-session-native-ops)))
      (let ((primary (unless (tramp-file-name-p (car args))
                       (ignore-errors
                         (apply #'tramp-file-name-for-operation
                                operation args)))))
        (when (eq operation 'make-symbolic-link)
          ;; `tramp-skeleton-make-symbolic-link' calls `expand-file-name' on
          ;; TARGET while flushing cache on LINKNAME's host.  Skip route
          ;; checks and `tramp-add-hops' on TARGET's host when it differs from
          ;; LINKNAME's host so a literal cross-host target (with `~' or
          ;; explicit hops) cannot mutate proxies or kill an unrelated session.
          (let* ((target (nth 0 args))
                 (target-vec (and (stringp target)
                                  (let ((non-essential t)
                                        (tramp-default-proxies-alist
                                         tramp-cache-undefined))
                                    (and (not (file-name-quoted-p target))
                                         (tramp-tramp-file-p target)
                                         (ignore-errors
                                           (tramp-dissect-file-name target))))))
                 (primary-vec (tramp-rpc--dissect-target-no-hops
                               primary 'allow-relative)))
            (when (and target-vec
                       (or (not primary-vec)
                           (not (tramp-file-name-equal-p target-vec
                                                         primary-vec))))
              (push (tramp-file-name-unify target-vec) skip-keys))))
        (when (or tramp-rpc--inhibit-add-hops
                  (> (hash-table-count tramp-rpc--tracked-native-routes) 0))
          (if (tramp-file-name-p (car args))
              (tramp-rpc--ensure-native-connection-route (car args))
            (tramp-rpc--ensure-native-route-for-target primary)
            (when (memq operation
                        '(copy-file rename-file copy-directory add-name-to-file))
              (dolist (arg (list (nth 0 args) (nth 1 args)))
                (when (and (stringp arg) (not (equal arg primary)))
                  (tramp-rpc--ensure-native-route-for-target arg))))))))
    (let ((tramp-rpc--skip-native-route-keys skip-keys))
      (apply orig-fun operation args))))

(defun tramp-rpc--path-managed-by-rpc-p (path)
  "Return non-nil when PATH (or `default-directory') is managed by tramp-rpc."
  (when (stringp path)
    (let ((effective (if (file-name-absolute-p path) path default-directory)))
      (and (stringp effective)
           (tramp-tramp-file-p effective)
           (tramp-rpc--managed-file-name-p effective)))))

(defun tramp-rpc--preserve-two-path-routes-advice (orig-fun file1 file2 &rest rest)
  "Preserve distinct explicit and hidden TRAMP-RPC proxy routes across ORIG-FUN.
Emacs primitives such as `copy-file' expand FILE1 and FILE2 sequentially before
invoking file-name handlers; without this guard, expanding an explicit-hop path
invokes `tramp-add-hops' and can redirect the other path onto the wrong route.
REST holds the remaining arguments of ORIG-FUN."
  (if (or tramp-rpc--inhibit-add-hops
          (not (and (stringp file1) (stringp file2)))
          (not (or (tramp-rpc--path-has-explicit-hop-p file1)
                   (tramp-rpc--path-has-explicit-hop-p file2)))
          (not (or (tramp-rpc--path-managed-by-rpc-p file1)
                   (tramp-rpc--path-managed-by-rpc-p file2))))
      (apply orig-fun file1 file2 rest)
    (let ((m1 (tramp-rpc--materialize-route file1))
          (m2 (tramp-rpc--materialize-route file2)))
      (let ((tramp-show-ad-hoc-proxies t)
            (tramp-save-ad-hoc-proxies nil)
            (tramp-rpc--inhibit-add-hops t)
            (tramp-default-proxies-alist (tramp-rpc--copy-proxies-alist)))
        (apply orig-fun m1 m2 rest)))))

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
    ;; Route preservation across explicit/hidden proxy hops.
    (tramp-add-hops . tramp-rpc--inhibit-add-hops-advice)
    (tramp-compute-multi-hops . tramp-rpc--compute-multi-hops-advice)
    (tramp-maybe-open-connection . tramp-rpc--maybe-open-connection-advice)
    (tramp-get-home-directory . tramp-rpc--get-home-directory-advice)
    (tramp-sh-file-name-handler . tramp-rpc--sh-file-name-handler-advice)
    (copy-file . tramp-rpc--preserve-two-path-routes-advice)
    (rename-file . tramp-rpc--preserve-two-path-routes-advice)
    (copy-directory . tramp-rpc--preserve-two-path-routes-advice)
    (add-name-to-file . tramp-rpc--preserve-two-path-routes-advice)
    (file-newer-than-file-p . tramp-rpc--preserve-two-path-routes-advice)
    (file-equal-p . tramp-rpc--preserve-two-path-routes-advice)
    (file-in-directory-p . tramp-rpc--preserve-two-path-routes-advice)
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

(defconst tramp-rpc--cache-sync-advice
  '((tramp-flush-file-properties . tramp-rpc--sync-tramp-flush-file)
    (tramp-flush-directory-properties . tramp-rpc--sync-tramp-flush-directory)
    (tramp-flush-connection-properties . tramp-rpc--sync-tramp-flush-connection)
    (tramp-flush-connection-properties . tramp-rpc--forget-tracked-native-route))
  "TRAMP cache-flush functions advised `:after' to keep RPC caches coherent.")

(defvar tramp-rpc--python-external-operation-installed nil
  "Non-nil when Python's TRAMP external operation has been installed.")

(defun tramp-rpc-handler-install ()
  "Install all process handler for tramp-rpc."
  (pcase-dolist (`(,function . ,advice) tramp-rpc--advice)
    ;; Outermost, so ownership is checked before any Tramp dispatch.
    (advice-add function :around advice '((depth . -10))))
  (pcase-dolist (`(,function . ,advice) tramp-rpc--cache-sync-advice)
    (advice-add function :after advice))
  ;; This must be before `tramp-signal-process'.  Since tramp.el is
  ;; required, this is guaranteed.
  (add-hook 'signal-process-functions #'tramp-rpc-handle-signal-process)
  ;; Likewise before `tramp-interrupt-process'.
  (add-hook 'interrupt-process-functions #'tramp-rpc-handle-interrupt-process))

(defun tramp-rpc-advice-install-optional-handlers ()
  "Install handlers for loaded optional integration packages."
  (when (featurep 'python)
    (if (tramp-rpc--python-tramp-file-name-external-operation-p)
        (unless tramp-rpc--python-external-operation-installed
          (condition-case err
              (progn
                (tramp-rpc--add-external-operation
                 'python-shell--tramp-with-environment
                 #'tramp-rpc-handle-python-shell--tramp-with-environment
                 'tramp-rpc 'tramp-file-name)
                (setq tramp-rpc--python-external-operation-installed t))
            (remote-file-error
             (signal (car err) (cdr err)))))
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
  (pcase-dolist (`(,function . ,advice) tramp-rpc--cache-sync-advice)
    (advice-remove function advice))
  (clrhash tramp-rpc--tracked-native-routes)
  (remove-hook 'signal-process-functions #'tramp-rpc-handle-signal-process)
  (remove-hook 'interrupt-process-functions #'tramp-rpc-handle-interrupt-process)
  (tramp-rpc--remove-external-operation
   'python-shell--tramp-with-environment 'tramp-rpc)
  (setq tramp-rpc--python-external-operation-installed nil)
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
