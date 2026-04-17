;;; tramp-rpc-advice.el --- Process handlers for TRAMP-RPC -*- lexical-binding: t; -*-

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

;; This file provides all the own Tramp handlers that tramp-rpc
;; installs on Emacs built-in process functions:
;; - process-send-string / process-send-region (route to remote stdin)
;; - process-send-eof (close remote stdin or send Ctrl-D to PTY)
;; - signal-process (forward signals to remote PID)
;; - process-status / process-exit-status (return remote process state)
;; - process-command / process-tty-name (return stored metadata)
;; - vc-call-backend (ensure default-directory for remote VC files)
;; - eglot--cmd (bypass shell wrapping for RPC connections)
;; - hack-dir-local-variables (enable dir-locals for RPC remotes)

;;; Code:

(require 'tramp)
(require 'msgpack)

;; Functions from tramp-rpc-process.el
(declare-function tramp-rpc--write-remote-process "tramp-rpc-process")
(declare-function tramp-rpc--close-remote-stdin "tramp-rpc-process")
(declare-function tramp-rpc--kill-remote-process "tramp-rpc-process")
(declare-function tramp-rpc--cleanup-async-processes "tramp-rpc-process")

;; Functions from tramp-rpc.el
(declare-function tramp-rpc--debug "tramp-rpc")
(declare-function tramp-rpc--call "tramp-rpc")
(declare-function tramp-rpc--call-async "tramp-rpc")
(declare-function tramp-rpc-file-name-p "tramp-rpc")

;; Variables from tramp-rpc.el / tramp-rpc-process.el
(defvar tramp-rpc--delivering-output)
(defvar tramp-rpc--closing-local-relay)
(defvar tramp-rpc--pty-processes)
(defvar tramp-rpc--async-processes)

;; Variables from vc-dir.el (used in vc-dir-refresh handler)
(defvar vc-dir-process-buffer)

;; ============================================================================
;; Process I/O handler
;; ============================================================================

(defun tramp-rpc-handle-process-send-string (process string)
  "Handler for `process-send-string' for TRAMP-RPC processes."
  ;; If we're delivering output to the local relay, bypass this handler
  (if tramp-rpc--delivering-output
      (tramp-run-real-handler #'process-send-string (list process string))
    ;; process-send-string can receive a buffer/buffer-name instead of process
    (let ((proc (cond
                 ((processp process) process)
                 ((or (bufferp process) (stringp process))
                  (get-buffer-process (get-buffer process)))
                 (t nil))))
      (cond
       ;; Direct SSH PTY - use normal process-send-string (low latency)
       ((and proc (process-get proc :tramp-rpc-direct-ssh))
        (tramp-run-real-handler #'process-send-string (list process string)))
       ;; RPC-based PTY process - use PTY write (async, fire-and-forget)
       ((and proc (process-get proc :tramp-rpc-pty)
             (process-get proc :tramp-rpc-pid))
        (let ((vec (process-get proc :tramp-rpc-vec))
              (pid (process-get proc :tramp-rpc-pid)))
          (tramp-rpc--debug "SEND-STRING PTY pid=%s len=%d" pid (length string))
          ;; Send write request asynchronously - data must be binary for MessagePack
          (let ((data-bytes (if (multibyte-string-p string)
                                (encode-coding-string string 'utf-8-unix)
                              string)))
            (tramp-rpc--call-async vec "process.write_pty"
                                   `((pid . ,pid)
                                     (data . ,(msgpack-bin-make data-bytes)))
                                   #'ignore))  ; Ignore the response
          nil))
       ;; Regular async RPC process (pipe-based)
       ((and proc
             (process-get proc :tramp-rpc-pid)
             (process-get proc :tramp-rpc-vec)
             (not (process-get proc :tramp-rpc-pty)))
        (tramp-rpc--debug "SEND-STRING pipe pid=%s len=%d"
                         (process-get proc :tramp-rpc-pid) (length string))
        (condition-case err
            (tramp-rpc--write-remote-process
           (process-get proc :tramp-rpc-vec)
           (process-get proc :tramp-rpc-pid)
           string)
        (error
         (message "tramp-rpc: Error writing to process: %s" err))))
       ;; Not an RPC process, use original function
       (t (tramp-run-real-handler #'process-send-string (list process string)))))))

(defun tramp-rpc-handle-process-send-region (process start end)
  "Handler for `process-send-region' for TRAMP-RPC processes."
  ;; If we're delivering output to the local relay, bypass this handler
  (if tramp-rpc--delivering-output
      (tramp-run-real-handler #'process-send-region (list process start end))
    (let ((proc (cond
                 ((processp process) process)
                 ((or (bufferp process) (stringp process))
                  (get-buffer-process (get-buffer process)))
                 (t nil))))
      (cond
       ;; Direct SSH PTY - use normal process-send-region (low latency)
       ((and proc (process-get proc :tramp-rpc-direct-ssh))
        (tramp-run-real-handler #'process-send-region (list process start end)))
       ;; RPC-based PTY process - use PTY write
       ((and proc (process-get proc :tramp-rpc-pty)
             (process-get proc :tramp-rpc-pid))
        (let ((vec (process-get proc :tramp-rpc-vec))
              (pid (process-get proc :tramp-rpc-pid))
              (string (buffer-substring-no-properties start end)))
          (tramp-rpc--debug "SEND-REGION PTY pid=%s len=%d" pid (length string))
          (let ((data-bytes (if (multibyte-string-p string)
                                (encode-coding-string string 'utf-8-unix)
                              string)))
            (tramp-rpc--call-async vec "process.write_pty"
                                   `((pid . ,pid)
                                     (data . ,(msgpack-bin-make data-bytes)))
                                   #'ignore))
          nil))
       ;; Regular async RPC process (pipe-based)
       ((and proc
             (process-get proc :tramp-rpc-pid)
             (process-get proc :tramp-rpc-vec)
             (not (process-get proc :tramp-rpc-pty)))
        (let ((string (buffer-substring-no-properties start end)))
          (tramp-rpc--debug "SEND-REGION pipe pid=%s len=%d"
                           (process-get proc :tramp-rpc-pid) (length string))
          (condition-case err
              (tramp-rpc--write-remote-process
               (process-get proc :tramp-rpc-vec)
               (process-get proc :tramp-rpc-pid)
               string)
            (error
             (message "tramp-rpc: Error writing to process: %s" err)))))
       ;; Not an RPC process, use original function
       (t (tramp-run-real-handler #'process-send-region (list process start end)))))))

(defun tramp-rpc-handle-process-send-eof (&optional process)
  "Handler for `process-send-eof' for TRAMP-RPC processes."
  ;; When closing a local cat relay, bypass this handler entirely so
  ;; the EOF reaches the local process rather than the remote one.
  (if tramp-rpc--closing-local-relay
      (tramp-run-real-handler #'process-send-eof (and process (list process)))
    (let ((proc (or process (get-buffer-process (current-buffer)))))
      (cond
       ;; Direct SSH PTY - use normal process-send-eof
       ((and proc (process-get proc :tramp-rpc-direct-ssh))
         (tramp-run-real-handler #'process-send-eof (and process (list process))))
       ;; RPC-managed process
       ((and proc
             (process-get proc :tramp-rpc-pid)
             (process-get proc :tramp-rpc-vec))
        (let ((pid (process-get proc :tramp-rpc-pid))
              (vec (process-get proc :tramp-rpc-vec)))
          ;; Only try to send EOF if the process hasn't already exited.
          ;; Short-lived processes (like git apply) may exit before we call
          ;; process-send-eof, which is fine - stdin was already closed on exit.
          (unless (or (process-get proc :tramp-rpc-exited)
                      (not (process-live-p proc)))
            (condition-case err
                (if (process-get proc :tramp-rpc-pty)
                    ;; PTY processes: send Ctrl-D (EOF character) via the PTY
                    (let ((eof-char (string ?\C-d))) ; ASCII 4 = Ctrl-D
                      (tramp-rpc--call-async vec "process.write_pty"
                                             `((pid . ,pid)
                                               (data . ,(msgpack-bin-make eof-char)))
                                             #'ignore))
                  ;; Pipe processes: close the stdin pipe
                  (tramp-rpc--close-remote-stdin vec pid))
              (error
               ;; Ignore "Process not found" errors - they just mean the process
               ;; exited before we could close stdin, which is expected for
               ;; short-lived processes like git apply in magit hunk staging.
               (unless (string-match-p "Process not found" (error-message-string err))
                 (message "tramp-rpc: Error closing stdin: %s" err)))))))
       ;; Not a tramp-rpc process
       (t (tramp-run-real-handler #'process-send-eof (and process (list process))))))))

(defun tramp-rpc-handle-signal-process (process sigcode &optional _remote)
  "Handler for `signal-process' of TRAMP-RPC processes.
It will be added to `signal-process-functions'."
  (when-let* ((pid (and (processp process)
			(process-get process :tramp-rpc-pid)))
	      (vec (process-get process :tramp-rpc-vec)))
    (condition-case err
        (progn
          ;; Use PTY kill for PTY processes, regular kill for pipe processes
          (if (process-get process :tramp-rpc-pty)
              (tramp-rpc--call vec "process.kill_pty"
                               `((pid . ,pid) (signal . ,sigcode)))
            (tramp-rpc--kill-remote-process vec pid sigcode))
          0) ; Return 0 for success
      (error
       (message "tramp-rpc: Error signaling process: %s" err)
       -1))))

;; ============================================================================
;; Process metadata handlers
;; ============================================================================

(defun tramp-rpc-handle-process-status (process)
  "Handler for `process-status' for TRAMP-RPC processes."
  (if (and (processp process) (process-get process :tramp-rpc-pid))
      (cond
       ((process-get process :tramp-rpc-exited) 'exit)
       ;; Use orig-fun to check live status, not process-live-p (which would recurse)
       ((memq (tramp-run-real-handler #'process-status (list process))
	      '(run open listen connect))
	'run)
       (t 'exit))
    (tramp-run-real-handler #'process-status (list process))))

(defun tramp-rpc-handle-process-exit-status (process)
  "Handler for `process-exit-status' for TRAMP-RPC processes."
  (if (and (processp process) (process-get process :tramp-rpc-pid))
      (or (process-get process :tramp-rpc-exit-code) 0)
    (tramp-run-real-handler #'process-exit-status (list process))))

(defun tramp-rpc-handle-process-command (process)
  "Handler for `process-command' to return stored command for PTY processes."
  (if (and (processp process) (process-get process :tramp-rpc-command))
      (process-get process :tramp-rpc-command)
    (tramp-run-real-handler #'process-command (list process))))

(defun tramp-rpc-handle-process-tty-name (process &optional stream)
  "Handler for `process-tty-name' to return stored TTY name for PTY processes.
For TRAMP-RPC PTY processes, return the remote TTY name stored during creation.
For direct SSH PTY processes, use the original function (returns local PTY)."
  (if (and (processp process)
           (process-get process :tramp-rpc-pty)
           (not (process-get process :tramp-rpc-direct-ssh)))
      (process-get process :tramp-rpc-tty-name)
    (tramp-run-real-handler #'process-tty-name (list process stream))))

;; ============================================================================
;; VC integration handler
;; ============================================================================

;; VC backends like vc-git-state use process-file internally, but they don't
;; set default-directory to the remote file's directory. This means process-file
;; runs locally instead of going through our tramp handler. We fix this by
;; advising vc-call-backend to set default-directory when the file is remote.

(defun tramp-rpc--vc-call-backend-file-name-for-operation
    (_operation _backend function-name &rest args)
  "Helper function for `vc-call-backend' handler."
  (or (and ;; Operations that take a file and may call process-file
           (memq function-name '(registered state state-heuristic dir-status-files
                                 working-revision previous-revision next-revision
                                 responsible-p))
	   (stringp (car args)) (car args))
      ""))

(defun tramp-rpc-handle-vc-call-backend (backend function-name &rest args)
  "Handler for `vc-call-backend' for TRAMP files correctly.
When FUNCTION-NAME is an operation that takes a file argument and that file is
a TRAMP path, ensure `default-directory' is set to the file's directory so that
process-file calls are routed through the TRAMP handler."
  (let ((default-directory (file-name-directory (car args))))
    (tramp-run-real-handler
     #'vc-call-backend (append `(,backend ,function-name) args))))

;; ============================================================================
;; Privilege elevation integration
;; ============================================================================
;; Eglot integration
;; ============================================================================

;; Eglot wraps remote commands with `/bin/sh -c "stty raw > /dev/null; cmd"`
;; to disable line buffering. This doesn't work with tramp-rpc because:
;; 1. Our pipe processes don't have a TTY, so stty fails
;; 2. We don't need this workaround - our RPC handles binary data correctly
;;
;; This handler bypasses the shell wrapper for tramp-rpc connections.

(defun tramp-rpc-handle-eglot--cmd (contact)
  "Handler for `eglot--cmd' to avoid shell wrapping for tramp-rpc.
For tramp-rpc connections, return CONTACT directly without wrapping
in a shell command.  This is safe because tramp-rpc uses pipes (not PTYs)
and handles binary data correctly."
  (if (tramp-rpc-file-name-p default-directory)
      contact
    (tramp-run-real-handler 'eglot--cmd (list contact))))

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

(defun tramp-rpc-handle-magit-start-process (program &optional input &rest args)
  "Force pipe mode for tramp-rpc when INPUT will be piped to the process.
PTY mode breaks stdin piping because `process-send-eof' sends Ctrl-D
which does not close the pipe — git waits for more input forever."
  (if input
      ;; Let-bind magit-tramp-pipe-stty-settings to "" so that
      ;; magit-start-process sets process-connection-type to nil (pipe).
      (let ((magit-tramp-pipe-stty-settings ""))
	(tramp-run-real-handler
	 'magit-start-process (append `(,program ,input) args)))
    (tramp-run-real-handler
     'magit-start-process (append `(,program ,input) args))))

;; ============================================================================
;; vc-dir stale-process guard
;; ============================================================================

;; `vc-dir-busy' tests (get-buffer-process vc-dir-process-buffer).
;; In Emacs, `get-buffer-process' returns ANY process associated with the
;; buffer -- including exited ones -- as long as `delete-process' has not
;; been called.  Normally our deferred `tramp-rpc--install-process-cleanup'
;; handles this, but if the timer hasn't fired yet (or if the cat relay got
;; stuck), the stale process causes "Another update process is in progress".
;; This handler acts as a safety net: before `vc-dir-refresh' checks the
;; busy flag, we delete any exited tramp-rpc relay process from the buffer.

(defun tramp-rpc--vc-dir-refresh-file-name-for-operation
    (_operation)
  "Helper function for `vc-dir-refresh' handler."
  (if (and (bound-and-true-p vc-dir-process-buffer)
           (buffer-live-p vc-dir-process-buffer))
      (tramp-get-default-directory vc-dir-process-buffer)
      ""))

(defun tramp-rpc-handle-vc-dir-refresh ()
  "Handler for `vc-dir-refresh' to clean up stale TRAMP-RPC relay processes.
If the vc-dir process buffer has a tramp-rpc cat relay that has already
exited (remote side finished), delete it so the refresh can proceed."
  (let ((proc (get-buffer-process vc-dir-process-buffer)))
    (when (and proc
               (process-get proc :tramp-rpc-pid)
               (or (process-get proc :tramp-rpc-exited)
                   (not (process-live-p proc))))
      (remhash proc tramp-rpc--async-processes)
      (ignore-errors (delete-process proc))))
  (tramp-run-real-handler 'vc-dir-refresh nil))

;; ============================================================================
;; Dir-locals advice
;; ============================================================================

;; Emacs's `enable-remote-dir-locals' defaults to nil because looking for
;; .dir-locals.el on remote hosts can be slow for traditional TRAMP methods.
;; TRAMP-RPC uses a fast binary protocol and dedicated high-level operations,
;; so enable this only for buffers using the rpc method.
(defun tramp-rpc--hack-dir-local-variables-advice (orig-fun)
  "Enable remote dir-locals in `hack-dir-local-variables' for RPC files."
  (let ((enable-remote-dir-locals
         (or enable-remote-dir-locals
             (when-let* ((file (or (buffer-file-name) default-directory)))
               (tramp-rpc-file-name-p file)))))
    (funcall orig-fun)))

;; ============================================================================
;; Install and uninstall handler
;; ============================================================================

(defun tramp-rpc-handler-install ()
  "Install all process handler for tramp-rpc."
  (with-eval-after-load 'tramp-rpc
    (tramp-add-external-operation
     'process-send-string
     #'tramp-rpc-handle-process-send-string 'tramp-rpc 'process)
    (tramp-add-external-operation
     'process-send-region
     #'tramp-rpc-handle-process-send-region 'tramp-rpc 'process)
    (tramp-add-external-operation
     'process-send-eof
     #'tramp-rpc-handle-process-send-eof 'tramp-rpc 'process))
  ;; This must be before `tramp-signal-process'.  Since tramp.el is
  ;; required, this is guaranteed.
  (add-hook 'signal-process-functions #'tramp-rpc-handle-signal-process)
  (with-eval-after-load 'tramp-rpc
    (tramp-add-external-operation
     'process-status
     #'tramp-rpc-handle-process-status 'tramp-rpc 'process)
    (tramp-add-external-operation
     'process-exit-status
     #'tramp-rpc-handle-process-exit-status 'tramp-rpc 'process)
    (tramp-add-external-operation
     'process-command
     #'tramp-rpc-handle-process-command 'tramp-rpc 'process)
    (tramp-add-external-operation
     'process-tty-name
     #'tramp-rpc-handle-process-tty-name 'tramp-rpc 'process))
  (with-eval-after-load 'tramp-rpc
    (tramp-add-external-operation
     'vc-call-backend
     #'tramp-rpc-handle-vc-call-backend 'tramp-rpc
     #'tramp-rpc--vc-call-backend-file-name-for-operation))
  (with-eval-after-load 'eglot
    (tramp-add-external-operation
     'eglot--cmd
     #'tramp-rpc-handle-eglot--cmd 'tramp-rpc 'default-directory))
  (with-eval-after-load 'magit-process
    (tramp-add-external-operation
     'magit-start-process
     #'tramp-rpc-handle-magit-start-process 'tramp-rpc 'default-directory))
  (with-eval-after-load 'tramp-rpc
    (tramp-add-external-operation
     'vc-dir-refresh
     #'tramp-rpc-handle-vc-dir-refresh 'tramp-rpc
     #'tramp-rpc--vc-dir-refresh-file-name-for-operation))
  (advice-add 'hack-dir-local-variables :around
              #'tramp-rpc--hack-dir-local-variables-advice))

(defun tramp-rpc-handler-remove ()
  "Remove all process handler installed by tramp-rpc."
  (tramp-remove-external-operation 'process-send-string 'tramp-rpc)
  (tramp-remove-external-operation 'process-send-region 'tramp-rpc)
  (tramp-remove-external-operation 'process-send-eof 'tramp-rpc)
  (remove-hook 'signal-process-functions #'tramp-rpc-handle-signal-process)
  (tramp-remove-external-operation 'process-status 'tramp-rpc)
  (tramp-remove-external-operation 'process-exit-status 'tramp-rpc)
  (tramp-remove-external-operation 'process-command 'tramp-rpc)
  (tramp-remove-external-operation 'process-tty-name 'tramp-rpc)
  (tramp-remove-external-operation 'vc-call-backend 'tramp-rpc)
  (tramp-remove-external-operation 'eglot--cmd 'tramp-rpc)
  (tramp-remove-external-operation 'magit-start-process 'tramp-rpc)
  (tramp-remove-external-operation 'vc-dir-refresh 'tramp-rpc)
  (advice-remove 'hack-dir-local-variables #'tramp-rpc--hack-dir-local-variables-advice))

;; FIXME: Do we need this?
(define-obsolete-variable-alias
  'tramp-rpc-install-advice-on-load 'tramp-rpc-install-handler-on-load "0.8.0")

(defcustom tramp-rpc-install-handler-on-load t
  "Whether to install process handler when tramp-rpc-advice is loaded.
Set to nil before loading to prevent automatic handler installation."
  :type 'boolean
  :group 'tramp-rpc)

;; Install handler when loaded (if enabled)
(when tramp-rpc-install-handler-on-load
  (tramp-rpc-handler-install))

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

(add-hook 'tramp-rpc-unload-hook
	  (lambda ()
	    ;; When Emacs is configured --with-native-compilation,
	    ;; `load-history' contains `--anonymous-lambda' defun
	    ;; entries for tramp-rpc-advice.el.  This raises an
	    ;; `cl--assertion-failed' error when unloading.  Emacs
	    ;; bug#80446.
	    (dolist (entry load-history)
	      (when (string-match-p
		     (rx "tramp-rpc-advice" (| ".el" ".elc") eos) (car entry))
		(setcdr entry
			(seq-remove
			 (lambda (item)
			   (equal item '(defun . --anonymous-lambda)))
			 (cdr entry)))))
	    (unload-feature 'tramp-rpc-advice 'force)))

(provide 'tramp-rpc-advice)
;;; tramp-rpc-advice.el ends here
