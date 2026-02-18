;;; tramp-rpc-advice.el --- Process advice for TRAMP-RPC -*- lexical-binding: t; -*-

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

;; This file provides all the :around advice functions that tramp-rpc
;; installs on Emacs built-in process functions:
;; - process-send-string / process-send-region (route to remote stdin)
;; - process-send-eof (close remote stdin or send Ctrl-D to PTY)
;; - signal-process (forward signals to remote PID)
;; - process-status / process-exit-status (return remote process state)
;; - process-command / process-tty-name (return stored metadata)
;; - vc-call-backend (ensure default-directory for remote VC files)
;; - eglot--cmd (bypass shell wrapping for RPC connections)

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

;; ============================================================================
;; Process I/O advice
;; ============================================================================

(defun tramp-rpc--process-send-string-advice (orig-fun process string)
  "Advice for `process-send-string' to handle TRAMP-RPC processes."
  ;; If we're delivering output to the local relay, bypass this advice
  (if tramp-rpc--delivering-output
      (funcall orig-fun process string)
    ;; process-send-string can receive a buffer/buffer-name instead of process
    (let ((proc (cond
                 ((processp process) process)
                 ((or (bufferp process) (stringp process))
                  (get-buffer-process (get-buffer process)))
                 (t nil))))
      (cond
       ;; Direct SSH PTY - use normal process-send-string (low latency)
       ((and proc (process-get proc :tramp-rpc-direct-ssh))
        (funcall orig-fun process string))
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
       (t (funcall orig-fun process string))))))

(defun tramp-rpc--process-send-region-advice (orig-fun process start end)
  "Advice for `process-send-region' to handle TRAMP-RPC processes."
  ;; If we're delivering output to the local relay, bypass this advice
  (if tramp-rpc--delivering-output
      (funcall orig-fun process start end)
    (let ((proc (cond
                 ((processp process) process)
                 ((or (bufferp process) (stringp process))
                  (get-buffer-process (get-buffer process)))
                 (t nil))))
      (cond
       ;; Direct SSH PTY - use normal process-send-region (low latency)
       ((and proc (process-get proc :tramp-rpc-direct-ssh))
        (funcall orig-fun process start end))
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
       (t (funcall orig-fun process start end))))))

(defun tramp-rpc--process-send-eof-advice (orig-fun &optional process)
  "Advice for `process-send-eof' to handle TRAMP-RPC processes."
  ;; When closing a local cat relay, bypass this advice entirely so
  ;; the EOF reaches the local process rather than the remote one.
  (if tramp-rpc--closing-local-relay
      (funcall orig-fun process)
    (let ((proc (or process (get-buffer-process (current-buffer)))))
      (cond
       ;; Direct SSH PTY - use normal process-send-eof
       ((and proc (process-get proc :tramp-rpc-direct-ssh))
        (funcall orig-fun process))
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
                    (let ((eof-char (string 4))) ; ASCII 4 = Ctrl-D
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
       (t (funcall orig-fun process))))))

(defun tramp-rpc--signal-process-advice (orig-fun process sigcode &optional remote)
  "Advice for `signal-process' to handle TRAMP-RPC processes."
  (if-let* ((pid (and (processp process)
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
         -1))
    (funcall orig-fun process sigcode remote)))

;; ============================================================================
;; Process metadata advice
;; ============================================================================

(defun tramp-rpc--process-status-advice (orig-fun process)
  "Advice for `process-status' to handle TRAMP-RPC processes."
  (if (and (processp process) (process-get process :tramp-rpc-pid))
      (cond
       ((process-get process :tramp-rpc-exited) 'exit)
       ;; Use orig-fun to check live status, not process-live-p (which would recurse)
       ((memq (funcall orig-fun process) '(run open listen connect)) 'run)
       (t 'exit))
    (funcall orig-fun process)))

(defun tramp-rpc--process-exit-status-advice (orig-fun process)
  "Advice for `process-exit-status' to handle TRAMP-RPC processes."
  (if (and (processp process) (process-get process :tramp-rpc-pid))
      (or (process-get process :tramp-rpc-exit-code) 0)
    (funcall orig-fun process)))

(defun tramp-rpc--process-command-advice (orig-fun process)
  "Advice for `process-command' to return stored command for PTY processes."
  (if (and (processp process) (process-get process :tramp-rpc-command))
      (process-get process :tramp-rpc-command)
    (funcall orig-fun process)))

(defun tramp-rpc--process-tty-name-advice (orig-fun process &optional stream)
  "Advice for `process-tty-name' to return stored TTY name for PTY processes.
For TRAMP-RPC PTY processes, return the remote TTY name stored during creation.
For direct SSH PTY processes, use the original function (returns local PTY)."
  (if (and (processp process)
           (process-get process :tramp-rpc-pty)
           (not (process-get process :tramp-rpc-direct-ssh)))
      (process-get process :tramp-rpc-tty-name)
    (funcall orig-fun process stream)))

;; ============================================================================
;; VC integration advice
;; ============================================================================

;; VC backends like vc-git-state use process-file internally, but they don't
;; set default-directory to the remote file's directory. This means process-file
;; runs locally instead of going through our tramp handler. We fix this by
;; advising vc-call-backend to set default-directory when the file is remote.

(defun tramp-rpc--vc-call-backend-advice (orig-fun backend function-name &rest args)
  "Advice for `vc-call-backend' to handle TRAMP files correctly.
When FUNCTION-NAME is an operation that takes a file argument and that file is
a TRAMP path, ensure `default-directory' is set to the file's directory so that
process-file calls are routed through the TRAMP handler."
  (let* ((file (car args))
         (should-set-dir (and file
                              (stringp file)
                              (tramp-tramp-file-p file)
                              ;; Operations that take a file and may call process-file
                              (memq function-name '(state state-heuristic dir-status-files
                                                    working-revision previous-revision next-revision
                                                    responsible-p)))))
    (if should-set-dir
        (let ((default-directory (file-name-directory file)))
          (apply orig-fun backend function-name args))
      (apply orig-fun backend function-name args))))

;; ============================================================================
;; Eglot integration
;; ============================================================================

;; Eglot wraps remote commands with `/bin/sh -c "stty raw > /dev/null; cmd"`
;; to disable line buffering. This doesn't work with tramp-rpc because:
;; 1. Our pipe processes don't have a TTY, so stty fails
;; 2. We don't need this workaround - our RPC handles binary data correctly
;;
;; This advice bypasses the shell wrapper for tramp-rpc connections.

(defun tramp-rpc--eglot-cmd-advice (orig-fun contact)
  "Advice for `eglot--cmd' to avoid shell wrapping for tramp-rpc.
For tramp-rpc connections, return CONTACT directly without wrapping
in a shell command. This is safe because tramp-rpc uses pipes (not PTYs)
and handles binary data correctly."
  (if (tramp-rpc-file-name-p default-directory)
      contact
    (funcall orig-fun contact)))

;; ============================================================================
;; vc-dir stale-process guard
;; ============================================================================

;; `vc-dir-busy' tests (get-buffer-process vc-dir-process-buffer).
;; In Emacs, `get-buffer-process' returns ANY process associated with the
;; buffer -- including exited ones -- as long as `delete-process' has not
;; been called.  Normally our deferred `tramp-rpc--install-process-cleanup'
;; handles this, but if the timer hasn't fired yet (or if the cat relay got
;; stuck), the stale process causes "Another update process is in progress".
;; This advice acts as a safety net: before `vc-dir-refresh' checks the
;; busy flag, we delete any exited tramp-rpc relay process from the buffer.

(defun tramp-rpc--vc-dir-refresh-advice (orig-fun)
  "Advice for `vc-dir-refresh' to clean up stale TRAMP-RPC relay processes.
If the vc-dir process buffer has a tramp-rpc cat relay that has already
exited (remote side finished), delete it so the refresh can proceed."
  (when (and (bound-and-true-p vc-dir-process-buffer)
             (buffer-live-p vc-dir-process-buffer))
    (let ((proc (get-buffer-process vc-dir-process-buffer)))
      (when (and proc
                 (process-get proc :tramp-rpc-pid)
                 (or (process-get proc :tramp-rpc-exited)
                     (not (process-live-p proc))))
        (remhash proc tramp-rpc--async-processes)
        (ignore-errors (delete-process proc)))))
  (funcall orig-fun))

;; ============================================================================
;; Install and uninstall advice
;; ============================================================================

(defun tramp-rpc-advice-install ()
  "Install all process advice for tramp-rpc."
  (advice-add 'process-send-string :around #'tramp-rpc--process-send-string-advice)
  (advice-add 'process-send-region :around #'tramp-rpc--process-send-region-advice)
  (advice-add 'process-send-eof :around #'tramp-rpc--process-send-eof-advice)
  (advice-add 'signal-process :around #'tramp-rpc--signal-process-advice)
  (advice-add 'process-status :around #'tramp-rpc--process-status-advice)
  (advice-add 'process-exit-status :around #'tramp-rpc--process-exit-status-advice)
  (advice-add 'process-command :around #'tramp-rpc--process-command-advice)
  (advice-add 'process-tty-name :around #'tramp-rpc--process-tty-name-advice)
  (advice-add 'vc-call-backend :around #'tramp-rpc--vc-call-backend-advice)
  (with-eval-after-load 'eglot
    (advice-add 'eglot--cmd :around #'tramp-rpc--eglot-cmd-advice))
  (with-eval-after-load 'vc-dir
    (advice-add 'vc-dir-refresh :around #'tramp-rpc--vc-dir-refresh-advice)))

(defun tramp-rpc-advice-remove ()
  "Remove all process advice installed by tramp-rpc."
  (advice-remove 'process-send-string #'tramp-rpc--process-send-string-advice)
  (advice-remove 'process-send-region #'tramp-rpc--process-send-region-advice)
  (advice-remove 'process-send-eof #'tramp-rpc--process-send-eof-advice)
  (advice-remove 'signal-process #'tramp-rpc--signal-process-advice)
  (advice-remove 'process-status #'tramp-rpc--process-status-advice)
  (advice-remove 'process-exit-status #'tramp-rpc--process-exit-status-advice)
  (advice-remove 'process-command #'tramp-rpc--process-command-advice)
  (advice-remove 'process-tty-name #'tramp-rpc--process-tty-name-advice)
  (advice-remove 'vc-call-backend #'tramp-rpc--vc-call-backend-advice)
  (advice-remove 'eglot--cmd #'tramp-rpc--eglot-cmd-advice)
  (advice-remove 'vc-dir-refresh #'tramp-rpc--vc-dir-refresh-advice))

(defcustom tramp-rpc-install-advice-on-load t
  "Whether to install process advice when tramp-rpc-advice is loaded.
Set to nil before loading to prevent automatic advice installation."
  :type 'boolean
  :group 'tramp-rpc)

;; Install advice when loaded (if enabled)
(when tramp-rpc-install-advice-on-load
  (tramp-rpc-advice-install))

(provide 'tramp-rpc-advice)
;;; tramp-rpc-advice.el ends here
