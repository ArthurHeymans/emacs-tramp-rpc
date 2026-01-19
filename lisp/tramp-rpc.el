;;; tramp-rpc.el --- TRAMP backend using RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Version: 0.2.0
;; Keywords: comm, processes, files
;; Package-Requires: ((emacs "30.1"))

;; This file is part of tramp-rpc.

;; tramp-rpc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This package provides a TRAMP backend that uses a custom RPC server
;; instead of parsing shell command output.  This significantly improves
;; performance for remote file operations.
;;
;; To use, add to your init.el:
;;   (require 'tramp-rpc)
;;
;; Then access files using the "rpc" method:
;;   /rpc:user@host:/path/to/file
;;
;; FEATURES:
;; - Fast file operations via binary RPC protocol
;; - Async process support (make-process, start-file-process)
;; - VC mode integration works (git, etc.)
;;
;; HOW ASYNC PROCESSES WORK:
;; Remote processes are started via RPC and polled periodically for output.
;; A local pipe process serves as a relay to provide Emacs process semantics.
;; Process filters, sentinels, and signals all work as expected.
;;
;; OPTIONAL CONFIGURATION:
;; If you experience issues with diff-hl in dired, you can disable it:
;;   (setq diff-hl-disable-on-remote t)

;;; Code:

(require 'tramp)
(require 'tramp-sh)
(require 'tramp-rpc-protocol)
(require 'tramp-rpc-deploy)

;; Silence byte-compiler warnings for functions defined elsewhere

;; Silence byte-compiler warnings for variables defined in vterm
(defvar vterm-copy-mode)
(defvar vterm-min-window-width)
(defvar vterm--term)

(defgroup tramp-rpc nil
  "TRAMP backend using RPC."
  :group 'tramp)

(defcustom tramp-rpc-use-controlmaster t
  "Whether to use SSH ControlMaster for connection sharing.
When enabled, multiple connections to the same host share a single
SSH connection, significantly reducing connection overhead.

The control socket is stored in `tramp-rpc-controlmaster-path'."
  :type 'boolean
  :group 'tramp-rpc)

(defcustom tramp-rpc-controlmaster-path "~/.ssh/tramp-rpc/%C"
  "Path template for SSH ControlMaster socket.
Use SSH escape sequences: %r=remote user, %h=host, %p=port, %C=connection hash.
The %C token (available in OpenSSH 6.7+) creates a unique hash from
%l%h%p%r (local host, remote host, port, user), avoiding path length issues.
For older OpenSSH versions, use: ~/.ssh/tramp-rpc-%r@%h:%p
The directory must exist and be writable."
  :type 'string
  :group 'tramp-rpc)

(defcustom tramp-rpc-controlmaster-persist 600
  "How long (in seconds) to keep ControlMaster connections alive.
Set to 0 to close immediately when last connection exits.
Set to \"yes\" to keep alive indefinitely."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Indefinitely" "yes"))
  :group 'tramp-rpc)

(defcustom tramp-rpc-debug nil
  "When non-nil, log debug messages to *tramp-rpc-debug* buffer.
Set to t to enable debugging for hang diagnosis."
  :type 'boolean
  :group 'tramp-rpc)

(defun tramp-rpc--debug (format-string &rest args)
  "Log a debug message to *tramp-rpc-debug* buffer if debugging is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (when tramp-rpc-debug
    (with-current-buffer (get-buffer-create "*tramp-rpc-debug*")
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S.%3N] ")
              (apply #'format format-string args)
              "\n"))))

(defconst tramp-rpc-method "rpc"
  "TRAMP method for RPC-based remote access.")

;; ============================================================================
;; Connection management
;; ============================================================================

(defvar tramp-rpc--connections (make-hash-table :test 'equal)
  "Hash table mapping connection keys to RPC process info.
Key is (host user port), value is a plist with :process and :buffer.")

(defvar tramp-rpc--async-processes (make-hash-table :test 'eq)
  "Hash table mapping local relay processes to their remote process info.
Value is a plist with :vec, :pid, :timer, :stderr-buffer.")

(defvar tramp-rpc--pty-processes (make-hash-table :test 'eq)
  "Hash table mapping local relay processes to their remote PTY process info.
Value is a plist with :vec, :pid.")

(defvar tramp-rpc--async-callbacks (make-hash-table :test 'eql)
  "Hash table mapping request IDs to callback functions for async RPC calls.")

(defvar tramp-rpc--process-write-queues (make-hash-table :test 'eql)
  "Hash table mapping remote PIDs to write queue state.
Value is a plist with :pending (list of pending write data) and :writing (bool).")

;; ============================================================================
;; Direnv environment caching for process execution
;; ============================================================================

(defvar tramp-rpc--direnv-cache (make-hash-table :test 'equal)
  "Cache of direnv environments keyed by (connection-key . directory).
Value is a plist with :env (alist) and :timestamp.")

(defvar tramp-rpc--direnv-available-cache (make-hash-table :test 'equal)
  "Cache tracking whether direnv is available on each connection.
Value is :available, :unavailable, or nil (unknown).")

(defcustom tramp-rpc-use-direnv t
  "Whether to load direnv environment for remote processes.
When enabled, runs `direnv export json` to get project-specific
environment variables. Set to nil to disable for better performance."
  :type 'boolean
  :group 'tramp-rpc)

(defcustom tramp-rpc-direnv-cache-timeout 300
  "Seconds to cache direnv environment before re-fetching.
Set to 0 to disable caching (not recommended)."
  :type 'integer
  :group 'tramp-rpc)

(defun tramp-rpc--direnv-cache-key (vec directory)
  "Generate cache key for direnv environment on VEC in DIRECTORY."
  (cons (tramp-rpc--connection-key vec) directory))

(defun tramp-rpc--get-direnv-environment (vec directory)
  "Get direnv environment for DIRECTORY on VEC.
Returns alist of (VAR . VALUE) pairs, or nil if direnv unavailable/disabled.
Results are cached for `tramp-rpc-direnv-cache-timeout' seconds."
  (when tramp-rpc-use-direnv
    (let* ((conn-key (tramp-rpc--connection-key vec))
           (direnv-status (gethash conn-key tramp-rpc--direnv-available-cache)))
      ;; Skip if we already know direnv is unavailable on this host
      (unless (eq direnv-status :unavailable)
        (let* ((cache-key (tramp-rpc--direnv-cache-key vec directory))
               (cached (gethash cache-key tramp-rpc--direnv-cache))
               (now (float-time)))
          ;; Check if cache is valid
          (if (and cached
                   (< (- now (plist-get cached :timestamp))
                      tramp-rpc-direnv-cache-timeout))
              (plist-get cached :env)
            ;; Need to fetch fresh
            (let ((env (tramp-rpc--fetch-direnv-environment vec directory)))
              ;; Cache the result (even if nil, to avoid repeated failures)
              (puthash cache-key
                       (list :env env :timestamp now)
                       tramp-rpc--direnv-cache)
              env)))))))

(defcustom tramp-rpc-direnv-essential-vars
  '("PATH" "LD_LIBRARY_PATH" "LIBRARY_PATH"
    "CARGO_HOME" "RUSTUP_HOME" "RUST_SRC_PATH"
    "CC" "CXX" "PKG_CONFIG_PATH"
    "NIX_CC" "NIX_CFLAGS_COMPILE" "NIX_LDFLAGS"
    "GOPATH" "GOROOT"
    "PYTHONPATH" "VIRTUAL_ENV"
    "NODE_PATH" "NPM_CONFIG_PREFIX")
  "Environment variables to extract from direnv.
Only these variables are passed to remote processes to avoid
performance issues with large environments."
  :type '(repeat string)
  :group 'tramp-rpc)

(defun tramp-rpc--fetch-direnv-environment (vec directory)
  "Fetch direnv environment for DIRECTORY on VEC.
Returns alist of (VAR . VALUE) pairs for essential variables only.
See `tramp-rpc-direnv-essential-vars' for the list of variables."
  (condition-case nil
      (let* ((result (tramp-rpc--call vec "process.run"
                                       `((cmd . "/bin/sh")
                                         (args . ["-l" "-c"
                                                  ,(concat "cd " (shell-quote-argument directory)
                                                           " && direnv export json 2>/dev/null")])
                                         (cwd . "/"))))
             (exit-code (alist-get 'exit_code result))
             (stdout (tramp-rpc--decode-output
                      (alist-get 'stdout result)
                      (alist-get 'stdout_encoding result))))
        (if (and (eq exit-code 0)
                 (> (length stdout) 0))
            ;; Parse JSON output into alist, filter to essential vars
            (condition-case nil
                (let* ((json-object-type 'alist)
                       (json-key-type 'string)
                       (full-env (json-read-from-string stdout)))
                  ;; Filter to only essential variables
                  (cl-loop for var in tramp-rpc-direnv-essential-vars
                           for pair = (assoc var full-env)
                           when pair collect pair))
              (error nil))
          ;; If exit code is 127 (command not found), mark direnv as unavailable
          (when (eq exit-code 127)
            (puthash (tramp-rpc--connection-key vec)
                     :unavailable
                     tramp-rpc--direnv-available-cache))
          nil))
    ;; If any error, return nil silently
    (error nil)))

(defvar tramp-rpc--executable-cache (make-hash-table :test 'equal)
  "Cache of executable paths keyed by (connection-key . program).
Value is the full path or :not-found.")

(defun tramp-rpc--resolve-executable (vec program)
  "Resolve PROGRAM to its full path on VEC.
Returns the full path if found, otherwise the original PROGRAM.
Results are cached per connection."
  (if (file-name-absolute-p program)
      program
    (let* ((cache-key (cons (tramp-rpc--connection-key vec) program))
           (cached (gethash cache-key tramp-rpc--executable-cache)))
      (cond
       ((stringp cached) cached)  ; Cached full path
       ((eq cached :not-found) program)  ; Known not found, use original
       (t  ; Not cached, look it up
        (let ((found (tramp-rpc--find-executable vec program)))
          (puthash cache-key (or found :not-found) tramp-rpc--executable-cache)
          (or found program)))))))

(defun tramp-rpc--connection-key (vec)
  "Generate a connection key for VEC."
  (list (tramp-file-name-host vec)
        (tramp-file-name-user vec)
        (or (tramp-file-name-port vec) 22)))

(defun tramp-rpc--get-connection (vec)
  "Get the RPC connection for VEC, or nil if not connected."
  (gethash (tramp-rpc--connection-key vec) tramp-rpc--connections))

(defun tramp-rpc--set-connection (vec process buffer)
  "Store the RPC connection for VEC."
  (puthash (tramp-rpc--connection-key vec)
           (list :process process :buffer buffer)
           tramp-rpc--connections))

(defun tramp-rpc--remove-connection (vec)
  "Remove the RPC connection for VEC."
  (remhash (tramp-rpc--connection-key vec) tramp-rpc--connections))

(defun tramp-rpc--ensure-connection (vec)
  "Ensure we have an active RPC connection to VEC.
Returns the connection plist."
  (let ((conn (tramp-rpc--get-connection vec)))
    (if (and conn
             (process-live-p (plist-get conn :process)))
        conn
      ;; Need to establish connection
      (tramp-rpc--connect vec))))

(defun tramp-rpc--ensure-controlmaster-directory ()
  "Ensure the ControlMaster socket directory exists.
Creates the directory from `tramp-rpc-controlmaster-path' if needed."
  (when tramp-rpc-use-controlmaster
    (let* ((path (expand-file-name tramp-rpc-controlmaster-path))
           (dir (file-name-directory path)))
      (when (and dir (not (file-directory-p dir)))
        (make-directory dir t)
        ;; Set restrictive permissions for security
        (set-file-modes dir #o700)))))

(defun tramp-rpc--connect (vec)
  "Establish an RPC connection to VEC."
  ;; Ensure ControlMaster directory exists
  (tramp-rpc--ensure-controlmaster-directory)
  ;; First, ensure the binary is deployed using shell-based tramp
  (let* ((binary-path (tramp-rpc-deploy-ensure-binary vec))
         (host (tramp-file-name-host vec))
         (user (tramp-file-name-user vec))
         (port (tramp-file-name-port vec))
         ;; Build SSH command to run the RPC server
         (ssh-args (append
                    (list "ssh")
                    (when user (list "-l" user))
                    (when port (list "-p" (number-to-string port)))
                    (list "-o" "BatchMode=yes")
                    (list "-o" "StrictHostKeyChecking=accept-new")
                    ;; ControlMaster options for connection sharing
                    (when tramp-rpc-use-controlmaster
                      (list "-o" "ControlMaster=auto"
                            "-o" (format "ControlPath=%s"
                                         tramp-rpc-controlmaster-path)
                            "-o" (format "ControlPersist=%s"
                                         tramp-rpc-controlmaster-persist)))
                    (list host binary-path)))
         ;; Use TRAMP's standard naming so tramp-get-connection-process works
         (process-name (tramp-get-connection-name vec))
         (buffer-name (tramp-buffer-name vec))
         (buffer (get-buffer-create buffer-name))
         process)

    ;; Clear buffer - use multibyte to handle UTF-8 JSON properly
    (with-current-buffer buffer
      (erase-buffer)
      (set-buffer-multibyte t))

    ;; Start the process with pipe connection (not PTY)
    ;; PTY has line buffering and ~4KB line length limits that break large JSON-RPC requests
    (let ((process-connection-type nil))  ; Use pipes, not PTY
      (setq process (apply #'start-process process-name buffer ssh-args)))

    ;; Configure process
    (set-process-query-on-exit-flag process nil)
    (set-process-coding-system process 'utf-8 'utf-8)
    
    ;; Set up filter for async response handling
    (set-process-filter process #'tramp-rpc--connection-filter)

    ;; Store connection
    (tramp-rpc--set-connection vec process buffer)

    ;; Wait for server to be ready by sending a ping
    (let ((response (tramp-rpc--call vec "system.info" nil)))
      (unless response
        (tramp-rpc--remove-connection vec)
        (error "Failed to connect to RPC server on %s" host)))

    ;; Mark as connected for TRAMP's connectivity checks (used by projectile, etc.)
    (tramp-set-connection-property process "connected" t)

    (tramp-rpc--get-connection vec)))

(defun tramp-rpc--disconnect (vec)
  "Disconnect the RPC connection to VEC."
  ;; First, clean up any async processes for this connection
  (tramp-rpc--cleanup-async-processes vec)
  (let ((conn (tramp-rpc--get-connection vec)))
    (when conn
      (let ((process (plist-get conn :process)))
        (when (process-live-p process)
          (delete-process process)))
      (tramp-rpc--remove-connection vec))))

(defun tramp-rpc--cleanup-async-processes (&optional vec)
  "Clean up async processes, optionally only those for VEC."
  (maphash
   (lambda (local-process info)
     (when (or (null vec)
               (equal (tramp-rpc--connection-key (plist-get info :vec))
                      (tramp-rpc--connection-key vec)))
       ;; Cancel timer
       (when-let ((timer (plist-get info :timer)))
         (cancel-timer timer))
       ;; Kill local process
       (when (process-live-p local-process)
         (delete-process local-process))
       ;; Remove from tracking
       (remhash local-process tramp-rpc--async-processes)))
   tramp-rpc--async-processes))

;; ============================================================================
;; RPC communication
;; ============================================================================

(defun tramp-rpc--connection-filter (process output)
  "Filter for RPC connection PROCESS receiving OUTPUT.
Handles async responses by dispatching to registered callbacks."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Append output to buffer
        (goto-char (point-max))
        (insert output)
        (tramp-rpc--debug "FILTER received %d bytes, buffer-size=%d"
                         (length output) (buffer-size))
        ;; Process complete lines for async callbacks
        (goto-char (point-min))
        (while (search-forward "\n" nil t)
          (let* ((line-end (point))
                 ;; Point is after \n, so go back to get start of the line we just passed
                 (line-start (save-excursion
                               (goto-char (1- line-end))
                               (line-beginning-position)))
                 (line (buffer-substring line-start (1- line-end))))
            (when (> (length line) 0)
              (condition-case err
                  (let* ((response (tramp-rpc-protocol-decode-response line))
                         (id (plist-get response :id))
                         (callback (gethash id tramp-rpc--async-callbacks)))
                    (if callback
                        (progn
                          (tramp-rpc--debug "FILTER dispatching async id=%s" id)
                          ;; Remove this line from buffer - it's handled async
                          (delete-region line-start line-end)
                          (goto-char (point-min))
                          ;; Remove callback and call it
                          (remhash id tramp-rpc--async-callbacks)
                          (funcall callback response))
                      ;; Not an async response - leave for sync code
                      (tramp-rpc--debug "FILTER leaving sync response id=%s in buffer" id)))
                (error
                 (tramp-rpc--debug "FILTER parse error: %S line=%S" err line))))))))))

(defun tramp-rpc--call-async (vec method params callback)
  "Call METHOD with PARAMS asynchronously on the RPC server for VEC.
CALLBACK is called with the response plist when it arrives.
Returns the request ID."
  (let* ((conn (tramp-rpc--ensure-connection vec))
         (process (plist-get conn :process))
         (id-and-request (tramp-rpc-protocol-encode-request-with-id method params))
         (id (car id-and-request))
         (request (cdr id-and-request)))
    (tramp-rpc--debug "SEND-ASYNC id=%s method=%s" id method)
    ;; Register callback
    (puthash id callback tramp-rpc--async-callbacks)
    ;; Send request
    (process-send-string process (concat request "\n"))
    id))

(defun tramp-rpc--call (vec method params)
  "Call METHOD with PARAMS on the RPC server for VEC.
Returns the result or signals an error."
  (tramp-rpc--call-with-timeout vec method params 30 0.1))

(defun tramp-rpc--call-fast (vec method params)
  "Call METHOD with PARAMS with shorter timeout for low-latency ops.
Returns the result or signals an error.
Uses 5s total timeout with 10ms polling."
  (tramp-rpc--call-with-timeout vec method params 5 0.01))

(defun tramp-rpc--find-response-by-id (expected-id)
  "Search current buffer for a JSON-RPC response with EXPECTED-ID.
Returns (LINE-START . LINE-END) if found, nil otherwise.
Does not modify the buffer."
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (while (search-forward "\n" nil t)
        (let* ((line-end (point))
               (line-start (save-excursion
                             (goto-char (1- line-end))
                             (line-beginning-position)))
               (line (buffer-substring line-start (1- line-end))))
          (when (> (length line) 0)
            (condition-case nil
                (let* ((response (tramp-rpc-protocol-decode-response line))
                       (id (plist-get response :id)))
                  (when (equal id expected-id)
                    (throw 'found (cons line-start line-end))))
              (error nil)))))
      nil)))

(defun tramp-rpc--call-with-timeout (vec method params total-timeout poll-interval)
  "Call METHOD with PARAMS on the RPC server for VEC.
TOTAL-TIMEOUT is maximum seconds to wait.
POLL-INTERVAL is seconds between accept-process-output checks.
Returns the result or signals an error."
  (let* ((conn (tramp-rpc--ensure-connection vec))
         (process (plist-get conn :process))
         (buffer (plist-get conn :buffer))
         (id-and-request (tramp-rpc-protocol-encode-request-with-id method params))
         (expected-id (car id-and-request))
         (request (cdr id-and-request)))

    (tramp-rpc--debug "SEND id=%s method=%s" expected-id method)

    ;; Send request
    (process-send-string process (concat request "\n"))

    ;; Wait for response with matching ID
    (with-current-buffer buffer
      (let ((timeout total-timeout)
            response-line
            response-bounds)
        ;; Wait for a response with the correct ID
        (while (and (not response-line)
                    (> timeout 0)
                    (process-live-p process))
          ;; Use same pattern as tramp-accept-process-output:
          ;; - poll-interval timeout to avoid spinning
          ;; - JUST-THIS-ONE=t to only accept from this process (Bug#12145)
          ;; - with-local-quit to allow C-g, returns t on success
          ;; - Propagate quit if user pressed C-g
          (if (with-local-quit
                (accept-process-output process poll-interval nil t)
                t)
              ;; Check if our response arrived (search by ID)
              (when (setq response-bounds (tramp-rpc--find-response-by-id expected-id))
                (setq response-line (buffer-substring
                                     (car response-bounds)
                                     (1- (cdr response-bounds))))
                ;; Clear only our response from the buffer
                (delete-region (car response-bounds) (cdr response-bounds))
                (tramp-rpc--debug "RECV id=%s (found)" expected-id))
            ;; User quit - propagate it
            (tramp-rpc--debug "QUIT id=%s (user interrupted)" expected-id)
            (keyboard-quit))
          (cl-decf timeout poll-interval))

        (unless response-line
          (tramp-rpc--debug "TIMEOUT id=%s method=%s buffer-contents=%S"
                           expected-id method (buffer-string))
          (error "Timeout waiting for RPC response from %s (id=%s, method=%s)"
                 (tramp-file-name-host vec) expected-id method))

        (let ((response (tramp-rpc-protocol-decode-response response-line)))
          (if (tramp-rpc-protocol-error-p response)
              (let ((code (tramp-rpc-protocol-error-code response))
                    (msg (tramp-rpc-protocol-error-message response)))
                (tramp-rpc--debug "ERROR id=%s code=%s msg=%s" expected-id code msg)
                (cond
                 ((= code tramp-rpc-protocol-error-file-not-found)
                  (signal 'file-missing (list "RPC" "No such file" msg)))
                 ((= code tramp-rpc-protocol-error-permission-denied)
                  (signal 'file-error (list "RPC" "Permission denied" msg)))
                 (t
                  (error "RPC error: %s" msg))))
            (plist-get response :result)))))))

(defun tramp-rpc--call-batch (vec requests)
  "Execute multiple RPC REQUESTS in a single round-trip for VEC.
REQUESTS is a list of (METHOD . PARAMS) cons cells.
Returns a list of results (or error plists) in the same order.

Example:
  (tramp-rpc--call-batch vec
    \\='((\"file.exists\" . ((path . \"/foo\")))
      (\"file.stat\" . ((path . \"/bar\")))
      (\"process.run\" . ((cmd . \"git\") (args . [\"status\"])))))

Returns:
  (t                          ; file.exists result
   ((type . \"file\") ...)    ; file.stat result  
   (:error -32001 :message \"...\"))  ; or error plist"
  (let* ((conn (tramp-rpc--ensure-connection vec))
         (process (plist-get conn :process))
         (buffer (plist-get conn :buffer))
         (id-and-request (tramp-rpc-protocol-encode-batch-request-with-id requests))
         (expected-id (car id-and-request))
         (request (cdr id-and-request)))

    (tramp-rpc--debug "SEND-BATCH id=%s count=%d" expected-id (length requests))

    ;; Send batch request
    (process-send-string process (concat request "\n"))

    ;; Wait for response with matching ID
    (with-current-buffer buffer
      (let ((timeout 30)
            response-line
            response-bounds)
        (while (and (not response-line)
                    (> timeout 0)
                    (process-live-p process))
          (if (with-local-quit
                (accept-process-output process 0.1 nil t)
                t)
              ;; Check if our response arrived (search by ID)
              (when (setq response-bounds (tramp-rpc--find-response-by-id expected-id))
                (setq response-line (buffer-substring
                                     (car response-bounds)
                                     (1- (cdr response-bounds))))
                (delete-region (car response-bounds) (cdr response-bounds))
                (tramp-rpc--debug "RECV-BATCH id=%s (found)" expected-id))
            (tramp-rpc--debug "QUIT-BATCH id=%s (user interrupted)" expected-id)
            (keyboard-quit))
          (cl-decf timeout 0.1))

        (unless response-line
          (tramp-rpc--debug "TIMEOUT-BATCH id=%s buffer-contents=%S"
                           expected-id (buffer-string))
          (error "Timeout waiting for batch RPC response from %s (id=%s)"
                 (tramp-file-name-host vec) expected-id))

        (let ((response (tramp-rpc-protocol-decode-response response-line)))
          (if (tramp-rpc-protocol-error-p response)
              (progn
                (tramp-rpc--debug "ERROR-BATCH id=%s msg=%s"
                                 expected-id (tramp-rpc-protocol-error-message response))
                (error "Batch RPC error: %s"
                       (tramp-rpc-protocol-error-message response)))
            (tramp-rpc-protocol-decode-batch-response response)))))))

;; ============================================================================
;; Request pipelining support
;; ============================================================================

(defun tramp-rpc--send-requests (vec requests)
  "Send multiple REQUESTS to the RPC server for VEC without waiting.
REQUESTS is a list of (METHOD . PARAMS) cons cells.
Returns a list of request IDs in the same order."
  (let* ((conn (tramp-rpc--ensure-connection vec))
         (process (plist-get conn :process))
         ids)
    (dolist (req requests)
      (let* ((id-and-json (tramp-rpc-protocol-encode-request-with-id
                           (car req) (cdr req)))
             (id (car id-and-json))
             (json (cdr id-and-json)))
        (tramp-rpc--debug "SEND-PIPE id=%s method=%s" id (car req))
        (push id ids)
        (process-send-string process (concat json "\n"))))
    (nreverse ids)))

(defun tramp-rpc--receive-responses (vec ids &optional timeout)
  "Receive responses for request IDS from the RPC server for VEC.
Returns an alist mapping each ID to its response plist.
TIMEOUT is the maximum time to wait in seconds (default 30)."
  (let* ((conn (tramp-rpc--ensure-connection vec))
         (process (plist-get conn :process))
         (buffer (plist-get conn :buffer))
         (timeout (or timeout 30))
         (remaining-ids (copy-sequence ids))
         (responses (make-hash-table :test 'eql)))
    (tramp-rpc--debug "RECV-PIPE waiting for %d responses: %S" (length ids) ids)
    (with-current-buffer buffer
      (while (and remaining-ids
                  (> timeout 0)
                  (process-live-p process))
        (if (with-local-quit
              (accept-process-output process 0.1 nil t)
              t)
            ;; Check for each remaining ID
            (dolist (id remaining-ids)
              (let ((bounds (tramp-rpc--find-response-by-id id)))
                (when bounds
                  (let* ((line (buffer-substring (car bounds) (1- (cdr bounds))))
                         (response (tramp-rpc-protocol-decode-response line)))
                    (tramp-rpc--debug "RECV-PIPE found id=%s" id)
                    ;; Store response by ID
                    (puthash id response responses)
                    ;; Remove from remaining
                    (setq remaining-ids (delete id remaining-ids))
                    ;; Remove from buffer
                    (delete-region (car bounds) (cdr bounds))))))
          (tramp-rpc--debug "RECV-PIPE quit (user interrupted)")
          (keyboard-quit))
        (cl-decf timeout 0.1)))
    (when remaining-ids
      (tramp-rpc--debug "RECV-PIPE timeout, missing ids: %S" remaining-ids))
    ;; Convert hash table to alist in original order
    (mapcar (lambda (id)
              (cons id (gethash id responses)))
            ids)))

(defun tramp-rpc--call-pipelined (vec requests)
  "Execute multiple REQUESTS in a pipelined fashion for VEC.
REQUESTS is a list of (METHOD . PARAMS) cons cells.
Returns a list of results in the same order as REQUESTS.
Each result is either the actual result or an error plist.

Unlike `tramp-rpc--call-batch', this sends each request as a separate
RPC call, allowing the server to process them concurrently.
This is more efficient when the server has async support."
  (let* ((ids (tramp-rpc--send-requests vec requests))
         (responses (tramp-rpc--receive-responses vec ids)))
    ;; Process responses in order and extract results
    (mapcar (lambda (id-response)
              (let ((response (cdr id-response)))
                (if (tramp-rpc-protocol-error-p response)
                    (let ((code (tramp-rpc-protocol-error-code response))
                          (msg (tramp-rpc-protocol-error-message response)))
                      (list :error code :message msg))
                  (plist-get response :result))))
            responses)))

;; ============================================================================
;; Batch context for automatic operation batching
;; ============================================================================

(defvar tramp-rpc--batch-context nil
  "When non-nil, a plist with :vec, :requests, :results for batch collection.")

(defmacro with-tramp-rpc-batch (vec &rest body)
  "Execute BODY with automatic batching of RPC operations for VEC.
Operations within BODY that would normally make individual RPC calls
are collected and executed together when the batch context ends.

This is useful for optimizing code that makes many small RPC calls,
such as iterating over files.

Example:
  (with-tramp-rpc-batch vec
    (dolist (file files)
      (when (file-exists-p file)
        (push (file-attributes file) attrs))))"
  (declare (indent 1))
  `(let ((tramp-rpc--batch-context
          (list :vec ,vec :requests nil :results nil)))
     (unwind-protect
         (progn ,@body)
       ;; Flush any pending requests
       (tramp-rpc--flush-batch-context))))

(defun tramp-rpc--flush-batch-context ()
  "Execute any pending requests in the current batch context."
  (when tramp-rpc--batch-context
    (let ((requests (plist-get tramp-rpc--batch-context :requests)))
      (when requests
        (let* ((vec (plist-get tramp-rpc--batch-context :vec))
               (results (tramp-rpc--call-pipelined vec (nreverse requests))))
          (plist-put tramp-rpc--batch-context :results results)
          (plist-put tramp-rpc--batch-context :requests nil))))))

;; ============================================================================
;; Output decoding helper
;; ============================================================================

(defun tramp-rpc--decode-output (data encoding)
  "Decode DATA according to ENCODING.
ENCODING is either \"text\" (raw UTF-8) or \"base64\" (base64-encoded).
If ENCODING is nil, defaults to \"base64\" for backward compatibility."
  (cond
   ((null data) "")
   ((equal encoding "text") data)
   (t (base64-decode-string data))))

;; ============================================================================
;; File name handler operations
;; ============================================================================

(defun tramp-rpc-handle-file-exists-p (filename)
  "Like `file-exists-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (tramp-rpc--call v "file.exists" `((path . ,localname)))))

(defun tramp-rpc-handle-file-readable-p (filename)
  "Like `file-readable-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (tramp-rpc--call v "file.readable" `((path . ,localname)))))

(defun tramp-rpc-handle-file-writable-p (filename)
  "Like `file-writable-p' for TRAMP-RPC files.
Optimized to use pipelined requests for better performance."
  (with-parsed-tramp-file-name filename nil
    (let* ((parent (file-name-directory (directory-file-name localname)))
           ;; Send both requests in parallel
           (results (tramp-rpc--call-pipelined
                     v
                     `(("file.exists" . ((path . ,localname)))
                       ("file.writable" . ((path . ,localname)))
                       ("file.writable" . ((path . ,parent))))))
           (exists (nth 0 results))
           (file-writable (nth 1 results))
           (parent-writable (nth 2 results)))
      (if exists
          ;; File exists - check if it's writable
          (and (not (plist-get file-writable :error)) file-writable)
        ;; File doesn't exist - check if parent is writable
        (and (not (plist-get parent-writable :error)) parent-writable)))))

(defun tramp-rpc-handle-file-executable-p (filename)
  "Like `file-executable-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (tramp-rpc--call v "file.executable" `((path . ,localname)))))

(defun tramp-rpc-handle-file-directory-p (filename)
  "Like `file-directory-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (condition-case nil
        (let ((result (tramp-rpc--call v "file.stat" `((path . ,localname)))))
          (equal (alist-get 'type result) "directory"))
      (file-missing nil))))

(defun tramp-rpc-handle-file-regular-p (filename)
  "Like `file-regular-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (condition-case nil
        (let ((result (tramp-rpc--call v "file.stat" `((path . ,localname)))))
          (equal (alist-get 'type result) "file"))
      (file-missing nil))))

(defun tramp-rpc-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (condition-case nil
        (let ((result (tramp-rpc--call v "file.stat" `((path . ,localname) (lstat . t)))))
          (when (equal (alist-get 'type result) "symlink")
            (alist-get 'link_target result)))
      (file-missing nil))))

(defun tramp-rpc-handle-file-truename (filename)
  "Like `file-truename' for TRAMP-RPC files.
If the file doesn't exist, return FILENAME unchanged
\(like local `file-truename')."
  (with-parsed-tramp-file-name filename nil
    (condition-case nil
        (let ((result (tramp-rpc--call v "file.truename" `((path . ,localname)))))
          (tramp-make-tramp-file-name v result))
      ;; If file doesn't exist, return the filename unchanged
      (file-missing filename))))

(defun tramp-rpc-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (condition-case nil
        (let ((result (tramp-rpc--call v "file.stat" `((path . ,localname) (lstat . t)))))
          (tramp-rpc--convert-file-attributes result id-format))
      (file-missing nil))))

(defun tramp-rpc--convert-file-attributes (stat id-format)
  "Convert STAT result to Emacs file-attributes format.
ID-FORMAT specifies whether to use numeric or string IDs."
  (let* ((type-str (alist-get 'type stat))
         (type (pcase type-str
                 ("file" nil)
                 ("directory" t)
                 ("symlink" (alist-get 'link_target stat))
                 (_ nil)))
         (nlinks (alist-get 'nlinks stat))
         (uid (alist-get 'uid stat))
         (gid (alist-get 'gid stat))
         (uname (alist-get 'uname stat))
         (gname (alist-get 'gname stat))
         (atime (seconds-to-time (alist-get 'atime stat)))
         (mtime (seconds-to-time (alist-get 'mtime stat)))
         (ctime (seconds-to-time (alist-get 'ctime stat)))
         (size (alist-get 'size stat))
         (mode (tramp-rpc--mode-to-string (alist-get 'mode stat) type-str))
         (inode (alist-get 'inode stat))
         (dev (alist-get 'dev stat)))
    ;; Return in file-attributes format
    (list type nlinks
          (if (eq id-format 'string) (or uname (number-to-string uid)) uid)
          (if (eq id-format 'string) (or gname (number-to-string gid)) gid)
          atime mtime ctime
          size mode nil inode dev)))

(defun tramp-rpc--mode-to-string (mode type)
  "Convert numeric MODE to a string like \"drwxr-xr-x\".
TYPE is the file type string."
  (let ((type-char (pcase type
                     ("directory" ?d)
                     ("symlink" ?l)
                     ("file" ?-)
                     ("chardevice" ?c)
                     ("blockdevice" ?b)
                     ("fifo" ?p)
                     ("socket" ?s)
                     (_ ?-))))
    (format "%c%c%c%c%c%c%c%c%c%c"
            type-char
            (if (> (logand mode #o400) 0) ?r ?-)
            (if (> (logand mode #o200) 0) ?w ?-)
            (if (> (logand mode #o4000) 0)
                (if (> (logand mode #o100) 0) ?s ?S)
              (if (> (logand mode #o100) 0) ?x ?-))
            (if (> (logand mode #o040) 0) ?r ?-)
            (if (> (logand mode #o020) 0) ?w ?-)
            (if (> (logand mode #o2000) 0)
                (if (> (logand mode #o010) 0) ?s ?S)
              (if (> (logand mode #o010) 0) ?x ?-))
            (if (> (logand mode #o004) 0) ?r ?-)
            (if (> (logand mode #o002) 0) ?w ?-)
            (if (> (logand mode #o1000) 0)
                (if (> (logand mode #o001) 0) ?t ?T)
              (if (> (logand mode #o001) 0) ?x ?-)))))

(defun tramp-rpc-handle-file-modes (filename &optional _flag)
  "Like `file-modes' for TRAMP-RPC files."
  (let ((attrs (tramp-rpc-handle-file-attributes filename)))
    (when attrs
      (tramp-mode-string-to-int (nth 8 attrs)))))

(defun tramp-rpc-handle-set-file-modes (filename mode &optional _flag)
  "Like `set-file-modes' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (tramp-rpc--call v "file.set_modes" `((path . ,localname) (mode . ,mode)))))

(defun tramp-rpc-handle-set-file-times (filename &optional timestamp _flag)
  "Like `set-file-times' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (let ((mtime (floor (float-time (or timestamp (current-time))))))
      (tramp-rpc--call v "file.set_times" `((path . ,localname) (mtime . ,mtime))))))

(defun tramp-rpc-handle-file-newer-than-file-p (file1 file2)
  "Like `file-newer-than-file-p' for TRAMP-RPC files."
  (cond
   ((not (file-exists-p file1)) nil)
   ((not (file-exists-p file2)) t)
   (t
    ;; Both files exist, compare mtimes
    (let ((mtime1 (float-time (file-attribute-modification-time
                               (file-attributes file1))))
          (mtime2 (float-time (file-attribute-modification-time
                               (file-attributes file2)))))
      (> mtime1 mtime2)))))

;; ============================================================================
;; Directory operations
;; ============================================================================

(defun tramp-rpc-handle-directory-files (directory &optional full match nosort count)
  "Like `directory-files' for TRAMP-RPC files."
  (with-parsed-tramp-file-name (expand-file-name directory) nil
    (let* ((result (tramp-rpc--call v "dir.list"
                                    `((path . ,localname)
                                      (include_attrs . :json-false)
                                      (include_hidden . t))))
           (files (mapcar (lambda (entry) (alist-get 'name entry)) result)))
      ;; Filter by match pattern
      (when match
        (setq files (cl-remove-if-not
                     (lambda (f) (string-match-p match f))
                     files)))
      ;; Add full path if requested
      (when full
        (setq files (mapcar
                     (lambda (f)
                       (tramp-make-tramp-file-name
                        v (expand-file-name f localname)))
                     files)))
      ;; Sort unless nosort
      (unless nosort
        (setq files (sort files #'string<)))
      ;; Limit count
      (when count
        (setq files (seq-take files count)))
      files)))

(defun tramp-rpc-handle-directory-files-and-attributes
    (directory &optional full match nosort id-format count)
  "Like `directory-files-and-attributes' for TRAMP-RPC files."
  (with-parsed-tramp-file-name (expand-file-name directory) nil
    (let* ((result (tramp-rpc--call v "dir.list"
                                    `((path . ,localname)
                                      (include_attrs . t)
                                      (include_hidden . t))))
           (entries (mapcar
                     (lambda (entry)
                       (let* ((name (alist-get 'name entry))
                              (attrs (alist-get 'attrs entry))
                              (full-name (if full
                                             (tramp-make-tramp-file-name
                                              v (expand-file-name name localname))
                                           name)))
                         (cons full-name
                               (when attrs
                                 (tramp-rpc--convert-file-attributes attrs id-format)))))
                     result)))
      ;; Filter by match pattern
      (when match
        (setq entries (cl-remove-if-not
                       (lambda (e) (string-match-p match (car e)))
                       entries)))
      ;; Sort unless nosort
      (unless nosort
        (setq entries (sort entries (lambda (a b) (string< (car a) (car b))))))
      ;; Limit count
      (when count
        (setq entries (seq-take entries count)))
      entries)))

(defun tramp-rpc-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for TRAMP-RPC files."
  (tramp-skeleton-file-name-all-completions filename directory
    (with-parsed-tramp-file-name (expand-file-name directory) nil
      (when (and (not (string-search "/" filename))
                 (tramp-connectable-p v))
        (all-completions
         filename
         ;; Get all entries in the directory
         (let* ((result (tramp-rpc--call v "dir.list"
                                         `((path . ,localname)
                                           (include_attrs . :json-false)
                                           (include_hidden . t))))
                ;; Convert vector to list if needed
                (entries (if (vectorp result) (append result nil) result)))
           ;; Build list of names with trailing / for directories
           (mapcar (lambda (entry)
                     (let ((name (alist-get 'name entry))
                           (file-type (alist-get 'type entry)))
                       (if (equal file-type "directory")
                           (concat name "/")
                         name)))
                   entries)))))))

(defun tramp-rpc-handle-make-directory (dir &optional parents)
  "Like `make-directory' for TRAMP-RPC files."
  (with-parsed-tramp-file-name dir nil
    (tramp-rpc--call v "dir.create"
                     `((path . ,localname)
                       (parents . ,(if parents t :json-false))))))

(defun tramp-rpc-handle-delete-directory (directory &optional recursive trash)
  "Like `delete-directory' for TRAMP-RPC files."
  (tramp-skeleton-delete-directory directory recursive trash
    (tramp-rpc--call v "dir.remove"
                     `((path . ,localname)
                       (recursive . ,(if recursive t :json-false))))))

(defun tramp-rpc-handle-insert-directory
    (filename switches &optional _wildcard _full-directory-p)
  "Like `insert-directory' for TRAMP-RPC files.
Produces ls-like output for dired."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (let* ((result (tramp-rpc--call v "dir.list"
                                    `((path . ,localname)
                                      (include_attrs . t)
                                      (include_hidden . ,(if (string-match-p "a" (or switches "")) t :json-false)))))
           ;; Convert vector to list if needed
           (result-list (if (vectorp result) (append result nil) result))
           (entries (sort result-list (lambda (a b)
                                        (string< (alist-get 'name a)
                                                 (alist-get 'name b))))))
      ;; Insert header
      (insert (format "  %s:\n" filename))
      (insert "  total 0\n")  ; We don't calculate total blocks

      ;; Insert each entry
      (dolist (entry entries)
        (let* ((name (alist-get 'name entry))
               (attrs (alist-get 'attrs entry))
               (type (alist-get 'type attrs))
               (mode (alist-get 'mode attrs))
               (nlinks (or (alist-get 'nlinks attrs) 1))
               (uid (or (alist-get 'uid attrs) 0))
               (gid (or (alist-get 'gid attrs) 0))
               (uname (or (alist-get 'uname attrs) (number-to-string uid)))
               (gname (or (alist-get 'gname attrs) (number-to-string gid)))
               (size (or (alist-get 'size attrs) 0))
               (mtime (alist-get 'mtime attrs))
               (link-target (alist-get 'link_target attrs))
               (mode-str (tramp-rpc--mode-to-string (or mode 0) (or type "file")))
               (time-str (if mtime
                             (format-time-string "%b %e %H:%M" (seconds-to-time mtime))
                           "Jan  1 00:00")))
          ;; Skip . and .. unless -a is given
          (unless (and (member name '("." ".."))
                       (not (string-match-p "a" (or switches ""))))
            (insert (format "  %s %3d %-8s %-8s %8d %s %s"
                            mode-str nlinks uname gname size time-str name))
            (when (and link-target (equal type "symlink"))
              (insert (format " -> %s" link-target)))
            (insert "\n")))))))

;; ============================================================================
;; File I/O operations
;; ============================================================================

(defun tramp-rpc-handle-insert-file-contents
    (filename &optional visit beg end replace)
  "Like `insert-file-contents' for TRAMP-RPC files."
  (barf-if-buffer-read-only)
  (with-parsed-tramp-file-name filename nil
    (let* ((params `((path . ,localname)))
           (_ (when beg (push `(offset . ,beg) params)))
           (_ (when end (push `(length . ,(- end (or beg 0))) params)))
           (result (tramp-rpc--call v "file.read" params))
           (content (base64-decode-string (alist-get 'content result)))
           (size (length content)))

      (when replace
        (delete-region (point-min) (point-max)))

      (insert content)
      (when visit
        (setq buffer-file-name filename)
        (set-visited-file-modtime)
        (set-buffer-modified-p nil))

      (list filename size))))

(defun tramp-rpc-handle-write-region
    (start end filename &optional append visit _lockname mustbenew)
  "Like `write-region' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    ;; If START is a string, write it directly; otherwise extract from buffer
    ;; Note: START and END can be nil (meaning entire buffer) when called from save-buffer
    (let* ((content (if (stringp start)
                        start
                      (buffer-substring-no-properties
                       (or start (point-min))
                       (or end (point-max)))))
           (encoded (base64-encode-string content t))
           (params `((path . ,localname)
                     (content . ,encoded)
                     (append . ,(if append t :json-false)))))

      ;; Check mustbenew
      (when mustbenew
        (when (file-exists-p filename)
          (if (eq mustbenew 'excl)
              (signal 'file-already-exists (list filename))
            (unless (yes-or-no-p
                     (format "File %s exists; overwrite? " filename))
              (signal 'file-already-exists (list filename))))))

      (tramp-rpc--call v "file.write" params)

      ;; Handle visit
      (when (or (eq visit t) (stringp visit))
        (setq buffer-file-name filename)
        (set-visited-file-modtime)
        (set-buffer-modified-p nil))
      (when (stringp visit)
        (setq buffer-file-name visit))

      nil)))

(defun tramp-rpc-handle-copy-file
    (filename newname &optional ok-if-already-exists keep-time
              preserve-uid-gid preserve-permissions)
  "Like `copy-file' for TRAMP-RPC files."
  (setq filename (expand-file-name filename)
        newname (expand-file-name newname))
  (let ((source-remote (tramp-tramp-file-p filename))
        (dest-remote (tramp-tramp-file-p newname)))
    (cond
     ;; Both on same remote host using RPC - use server-side copy
     ((and source-remote dest-remote
           (tramp-equal-remote filename newname))
      (with-parsed-tramp-file-name filename v1
        (with-parsed-tramp-file-name newname v2
          (unless ok-if-already-exists
            (when (file-exists-p newname)
              (signal 'file-already-exists (list newname))))
          (tramp-rpc--call v1 "file.copy"
                           `((src . ,v1-localname)
                             (dest . ,v2-localname)
                             (preserve . ,(if (or keep-time preserve-permissions) t :json-false)))))))
     ;; Remote source, local dest - read via RPC, write locally
     ((and source-remote (not dest-remote))
      (unless ok-if-already-exists
        (when (file-exists-p newname)
          (signal 'file-already-exists (list newname))))
      ;; Use file-local-copy to get a temp local copy, then rename
      (let ((tmpfile (file-local-copy filename)))
        (unwind-protect
            (progn
              (rename-file tmpfile newname ok-if-already-exists)
              (when (or keep-time preserve-permissions)
                (set-file-times newname (file-attribute-modification-time
                                         (file-attributes filename)))))
          (when (file-exists-p tmpfile)
            (delete-file tmpfile)))))
     ;; Local source, remote dest - read locally, write via RPC
     ((and (not source-remote) dest-remote)
      (unless ok-if-already-exists
        (when (file-exists-p newname)
          (signal 'file-already-exists (list newname))))
      ;; Read local file and write to remote
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally filename)
        (write-region (point-min) (point-max) newname nil 'nomessage))
      (when (or keep-time preserve-permissions)
        (set-file-times newname (file-attribute-modification-time
                                 (file-attributes filename)))))
     ;; Both local or different remote hosts - use default
     (t
      (tramp-run-real-handler
       #'copy-file
       (list filename newname ok-if-already-exists keep-time
             preserve-uid-gid preserve-permissions))))))

(defun tramp-rpc-handle-rename-file (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for TRAMP-RPC files."
  (let ((source-remote (tramp-tramp-file-p filename))
        (dest-remote (tramp-tramp-file-p newname)))
    (cond
     ;; Both on same remote host using RPC
     ((and source-remote dest-remote
           (tramp-equal-remote filename newname))
      (with-parsed-tramp-file-name filename v1
        (with-parsed-tramp-file-name newname v2
          (tramp-rpc--call v1 "file.rename"
                           `((src . ,v1-localname)
                             (dest . ,v2-localname)
                             (overwrite . ,(if ok-if-already-exists t :json-false)))))))
     ;; Different hosts, copy then delete
     (t
      (copy-file filename newname ok-if-already-exists t t t)
      (delete-file filename)))))

(defun tramp-rpc-handle-delete-file (filename &optional trash)
  "Like `delete-file' for TRAMP-RPC files."
  (tramp-skeleton-delete-file filename trash
    (tramp-rpc--call v "file.delete" `((path . ,localname)))))

(defun tramp-rpc-handle-make-symbolic-link (target linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for TRAMP-RPC files."
  (with-parsed-tramp-file-name linkname nil
    (unless ok-if-already-exists
      (when (file-exists-p linkname)
        (signal 'file-already-exists (list linkname))))
    (when (file-exists-p linkname)
      (delete-file linkname))
    (tramp-rpc--call v "file.make_symlink"
                     `((target . ,(if (tramp-tramp-file-p target)
                                      (tramp-file-local-name target)
                                    target))
                       (link_path . ,localname)))))

(defun tramp-rpc-handle-add-name-to-file (filename newname &optional ok-if-already-exists)
  "Like `add-name-to-file' for TRAMP-RPC files.
Creates a hard link from NEWNAME to FILENAME."
  (unless (tramp-equal-remote filename newname)
    (with-parsed-tramp-file-name
        (if (tramp-tramp-file-p filename) filename newname) nil
      (tramp-error
       v 'file-error
       "add-name-to-file: %s"
       "only implemented for same method, same user, same host")))
  (with-parsed-tramp-file-name (expand-file-name filename) v1
    (with-parsed-tramp-file-name (expand-file-name newname) v2
      ;; Handle the 'confirm if exists' thing
      (when (file-exists-p newname)
        (if (or (null ok-if-already-exists)
                (and (numberp ok-if-already-exists)
                     (not (yes-or-no-p
                           (format "File %s already exists; make it a link anyway?"
                                   v2-localname)))))
            (tramp-error v2 'file-already-exists newname)
          (delete-file newname)))
      (tramp-rpc--call v1 "file.make_hardlink"
                       `((src . ,v1-localname)
                         (dest . ,v2-localname))))))

(defun tramp-rpc-handle-set-file-uid-gid (filename &optional uid gid)
  "Like `tramp-set-file-uid-gid' for TRAMP-RPC files.
Set the ownership of FILENAME to UID and GID.
Either UID or GID can be nil or -1 to leave that unchanged."
  (with-parsed-tramp-file-name filename nil
    (let ((uid (or (and (natnump uid) uid)
                   (tramp-rpc-handle-get-remote-uid v 'integer)))
          (gid (or (and (natnump gid) gid)
                   (tramp-rpc-handle-get-remote-gid v 'integer))))
      (tramp-rpc--call v "file.chown"
                       `((path . ,localname)
                         (uid . ,uid)
                         (gid . ,gid))))))

(defun tramp-rpc-handle-file-system-info (filename)
  "Like `file-system-info' for TRAMP-RPC files.
Returns a list of (TOTAL FREE AVAILABLE) bytes for the filesystem
containing FILENAME."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (condition-case nil
        (let ((result (tramp-rpc--call v "system.statvfs" `((path . ,localname)))))
          (list (alist-get 'total result)
                (alist-get 'free result)
                (alist-get 'available result)))
      (error nil))))

(defun tramp-rpc-handle-get-remote-groups (vec id-format)
  "Return remote groups using RPC.
ID-FORMAT specifies whether to return integer GIDs or string names."
  (condition-case nil
      (let ((result (tramp-rpc--call vec "system.groups" nil)))
        (mapcar (lambda (g)
                  (if (eq id-format 'integer)
                      (alist-get 'gid g)
                    (or (alist-get 'name g)
                        (number-to-string (alist-get 'gid g)))))
                result))
    (error nil)))

;; ============================================================================
;; ACL Support
;; ============================================================================

(defun tramp-rpc--acl-enabled-p (vec)
  "Check if ACL is available on the remote host VEC.
Caches the result for efficiency."
  ;; Check if getfacl exists and works
  (condition-case nil
      (let ((result (tramp-rpc--call vec "process.run"
                                     `((cmd . "getfacl")
                                       (args . ["--version"])
                                       (cwd . "/")))))
        (zerop (alist-get 'exit_code result)))
    (error nil)))

(defun tramp-rpc-handle-file-acl (filename)
  "Like `file-acl' for TRAMP-RPC files.
Returns the ACL string for FILENAME, or nil if ACLs are not supported."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (when (tramp-rpc--acl-enabled-p v)
      (let ((result (tramp-rpc--call v "process.run"
                                     `((cmd . "getfacl")
                                       (args . ["-ac" ,localname])
                                       (cwd . "/")))))
        (when (zerop (alist-get 'exit_code result))
          (let ((output (tramp-rpc--decode-output
                         (alist-get 'stdout result)
                         (alist-get 'stdout_encoding result))))
            ;; Return nil if output is empty or only whitespace
            (when (string-match-p "[^ \t\n]" output)
              (string-trim output))))))))

(defun tramp-rpc-handle-set-file-acl (filename acl-string)
  "Like `set-file-acl' for TRAMP-RPC files.
Set the ACL of FILENAME to ACL-STRING.
Returns t on success, nil on failure."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (when (and (stringp acl-string)
               (tramp-rpc--acl-enabled-p v))
      ;; Use setfacl with --set-file=- to read ACL from stdin
      (let* ((encoded-acl (base64-encode-string acl-string t))
             (result (tramp-rpc--call v "process.run"
                                      `((cmd . "setfacl")
                                        (args . ["--set-file=-" ,localname])
                                        (cwd . "/")
                                        (stdin . ,encoded-acl)))))
        (zerop (alist-get 'exit_code result))))))

;; ============================================================================
;; SELinux Support
;; ============================================================================

(defun tramp-rpc--selinux-enabled-p (vec)
  "Check if SELinux is enabled on the remote host VEC."
  (condition-case nil
      (let ((result (tramp-rpc--call vec "process.run"
                                     `((cmd . "selinuxenabled")
                                       (args . [])
                                       (cwd . "/")))))
        (zerop (alist-get 'exit_code result)))
    (error nil)))

(defun tramp-rpc-handle-file-selinux-context (filename)
  "Like `file-selinux-context' for TRAMP-RPC files.
Returns a list of (USER ROLE TYPE RANGE), or (nil nil nil nil) if not available."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (let ((context '(nil nil nil nil)))
      (when (tramp-rpc--selinux-enabled-p v)
        (let ((result (tramp-rpc--call v "process.run"
                                       `((cmd . "ls")
                                         (args . ["-d" "-Z" ,localname])
                                         (cwd . "/")))))
          (when (zerop (alist-get 'exit_code result))
            (let ((output (tramp-rpc--decode-output
                           (alist-get 'stdout result)
                           (alist-get 'stdout_encoding result))))
              ;; Parse SELinux context from ls -Z output
              ;; Format: user:role:type:range filename
              (when (string-match
                     "\\([^:]+\\):\\([^:]+\\):\\([^:]+\\):\\([^ \t\n]+\\)"
                     output)
                (setq context (list (match-string 1 output)
                                    (match-string 2 output)
                                    (match-string 3 output)
                                    (match-string 4 output))))))))
      context)))

(defun tramp-rpc-handle-set-file-selinux-context (filename context)
  "Like `set-file-selinux-context' for TRAMP-RPC files.
Set the SELinux context of FILENAME to CONTEXT.
CONTEXT is a list of (USER ROLE TYPE RANGE).
Returns t on success, nil on failure."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (when (and (consp context)
               (tramp-rpc--selinux-enabled-p v))
      (let* ((user (and (stringp (nth 0 context)) (nth 0 context)))
             (role (and (stringp (nth 1 context)) (nth 1 context)))
             (type (and (stringp (nth 2 context)) (nth 2 context)))
             (range (and (stringp (nth 3 context)) (nth 3 context)))
             (args (append
                    (when user (list (format "--user=%s" user)))
                    (when role (list (format "--role=%s" role)))
                    (when type (list (format "--type=%s" type)))
                    (when range (list (format "--range=%s" range)))
                    (list localname)))
             (result (tramp-rpc--call v "process.run"
                                      `((cmd . "chcon")
                                        (args . ,(vconcat args))
                                        (cwd . "/")))))
        (zerop (alist-get 'exit_code result))))))

;; ============================================================================
;; Process operations
;; ============================================================================

(defun tramp-rpc-handle-process-file
    (program &optional infile destination _display &rest args)
  "Like `process-file' for TRAMP-RPC files.
Resolves PROGRAM path and loads direnv environment from working directory."
  (with-parsed-tramp-file-name default-directory nil
    (let* ((resolved-program (tramp-rpc--resolve-executable v program))
           (direnv-env (tramp-rpc--get-direnv-environment v localname))
           (stdin-content (when (and infile (not (eq infile t)))
                            (with-temp-buffer
                              (insert-file-contents infile)
                              (base64-encode-string (buffer-string) t))))
           (result (tramp-rpc--call v "process.run"
                                    `((cmd . ,resolved-program)
                                      (args . ,(vconcat args))
                                      (cwd . ,localname)
                                      ,@(when direnv-env
                                          `((env . ,direnv-env)))
                                      ,@(when stdin-content
                                          `((stdin . ,stdin-content))))))
           (exit-code (alist-get 'exit_code result))
           (stdout (tramp-rpc--decode-output
                    (alist-get 'stdout result)
                    (alist-get 'stdout_encoding result)))
           (stderr (tramp-rpc--decode-output
                    (alist-get 'stderr result)
                    (alist-get 'stderr_encoding result))))

      ;; Handle destination
      (cond
       ((null destination) nil)
       ((eq destination t)
        (insert stdout))
       ((stringp destination)
        (with-temp-file destination
          (insert stdout)))
       ((bufferp destination)
        (with-current-buffer destination
          (insert stdout)))
       ((consp destination)
        (let ((stdout-dest (car destination))
              (stderr-dest (cadr destination)))
          (when stdout-dest
            (cond
             ((eq stdout-dest t) (insert stdout))
             ((stringp stdout-dest)
              (with-temp-file stdout-dest (insert stdout)))
             ((bufferp stdout-dest)
              (with-current-buffer stdout-dest (insert stdout)))))
          (when stderr-dest
            (cond
             ((stringp stderr-dest)
              (with-temp-file stderr-dest (insert stderr)))
             ((bufferp stderr-dest)
              (with-current-buffer stderr-dest (insert stderr))))))))

      exit-code)))

(defun tramp-rpc-handle-shell-command (command &optional output-buffer error-buffer)
  "Like `shell-command' for TRAMP-RPC files."
  (with-parsed-tramp-file-name default-directory nil
    (let* ((result (tramp-rpc--call v "process.run"
                                    `((cmd . "/bin/sh")
                                      (args . ["-c" ,command])
                                      (cwd . ,localname))))
           (exit-code (alist-get 'exit_code result))
           (stdout (tramp-rpc--decode-output
                    (alist-get 'stdout result)
                    (alist-get 'stdout_encoding result)))
           (stderr (tramp-rpc--decode-output
                    (alist-get 'stderr result)
                    (alist-get 'stderr_encoding result))))

      ;; Handle output-buffer: t means current buffer, buffer/string means that buffer
      (cond
       ((eq output-buffer t)
        (insert stdout))
       ((or (bufferp output-buffer) (stringp output-buffer))
        (with-current-buffer (get-buffer-create output-buffer)
          (erase-buffer)
          (insert stdout))))

      ;; Handle error-buffer
      (when (or (bufferp error-buffer) (stringp error-buffer))
        (with-current-buffer (get-buffer-create error-buffer)
          (erase-buffer)
          (insert stderr)))

      exit-code)))

(defun tramp-rpc-handle-vc-registered (file)
  "Like `vc-registered' for TRAMP-RPC files.
Since tramp-rpc supports `process-file', VC backends can run their
commands (git, svn, hg) directly via RPC.

We set `default-directory' to the file's directory to ensure that
process-file calls from VC backends are routed through our tramp handler."
  (when vc-handled-backends
    (with-parsed-tramp-file-name file nil
      ;; Set default-directory to the file's remote directory so that
      ;; process-file calls from VC are handled by our tramp handler.
      (let ((default-directory (file-name-directory file))
            process-file-side-effects)
        (tramp-run-real-handler #'vc-registered (list file))))))

(defun tramp-rpc-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for TRAMP-RPC files.
Handles tilde expansion by looking up the remote home directory."
  (let ((dir (or dir default-directory)))
    (cond
     ;; Absolute path with tramp prefix - parse and expand
     ((tramp-tramp-file-p name)
      (with-parsed-tramp-file-name name nil
        ;; Make sure localname is absolute
        (unless (tramp-run-real-handler #'file-name-absolute-p (list localname))
          (setq localname (concat "/" localname)))
        ;; Handle tilde expansion
        (when (string-prefix-p "~" localname)
          (setq localname (tramp-rpc--expand-tilde v localname)))
        ;; Do normal expand-file-name for "./" and "../"
        (let ((default-directory tramp-compat-temporary-file-directory))
          (tramp-make-tramp-file-name
           v (tramp-drop-volume-letter
              (tramp-run-real-handler #'expand-file-name (list localname)))))))
     ;; Absolute local path - make it remote
     ((file-name-absolute-p name)
      (with-parsed-tramp-file-name dir nil
        (tramp-make-tramp-file-name v name)))
     ;; Tilde path - expand relative to remote home
     ((string-prefix-p "~" name)
      (with-parsed-tramp-file-name dir nil
        (let ((expanded (tramp-rpc--expand-tilde v name)))
          (let ((default-directory tramp-compat-temporary-file-directory))
            (tramp-make-tramp-file-name
             v (tramp-drop-volume-letter
                (tramp-run-real-handler #'expand-file-name (list expanded))))))))
     ;; Relative path
     (t
      (tramp-make-tramp-file-name
       (tramp-dissect-file-name dir)
       (expand-file-name name (tramp-file-local-name dir)))))))

(defun tramp-rpc--expand-tilde (vec localname)
  "Expand tilde in LOCALNAME for remote connection VEC.
Returns the expanded path, or LOCALNAME unchanged if expansion fails."
  (if (string-match (rx bos "~" (group (* (not "/"))) (group (* nonl)) eos) localname)
      (let ((uname (match-string 1 localname))
            (fname (match-string 2 localname))
            hname)
        ;; Empty username means current user
        (when (string-empty-p uname)
          (setq uname (tramp-file-name-user vec)))
        ;; Get home directory
        (if (setq hname (tramp-get-home-directory vec uname))
            (concat hname fname)
          ;; Can't expand - return as-is (will error later)
          localname))
    localname))

;; ============================================================================
;; Additional handlers to avoid shell dependency
;; ============================================================================

(defvar tramp-rpc--exec-path-cache (make-hash-table :test 'equal)
  "Cache of remote exec-path keyed by connection-key.")

(defun tramp-rpc-handle-exec-path ()
  "Return remote exec-path using RPC.
Caches the result per connection."
  (with-parsed-tramp-file-name default-directory nil
    (let* ((key (tramp-rpc--connection-key v))
           (cached (gethash key tramp-rpc--exec-path-cache)))
      (or cached
          (let ((path (tramp-rpc--fetch-remote-exec-path v)))
            (puthash key path tramp-rpc--exec-path-cache)
            path)))))

(defun tramp-rpc--fetch-remote-exec-path (vec)
  "Fetch the remote PATH from VEC and split into directories."
  (condition-case nil
      (let* ((result (tramp-rpc--call vec "process.run"
                                       `((cmd . "/bin/sh")
                                         (args . ["-l" "-c" "echo $PATH"])
                                         (cwd . "/"))))
             (exit-code (alist-get 'exit_code result))
             (stdout (tramp-rpc--decode-output
                      (alist-get 'stdout result)
                      (alist-get 'stdout_encoding result))))
        (if (and (eq exit-code 0) (> (length stdout) 0))
            (split-string (string-trim stdout) ":" t)
          ;; Fallback to default paths
          '("/usr/local/bin" "/usr/bin" "/bin" "/usr/local/sbin" "/usr/sbin" "/sbin")))
    (error
     ;; On error, return default paths
     '("/usr/local/bin" "/usr/bin" "/bin" "/usr/local/sbin" "/usr/sbin" "/sbin"))))

(defun tramp-rpc-handle-file-local-copy (filename)
  "Create a local copy of remote FILENAME using RPC."
  (with-parsed-tramp-file-name filename nil
    (let* ((result (tramp-rpc--call v "file.read" `((path . ,localname))))
           (content (base64-decode-string (alist-get 'content result)))
           (tmpfile (make-temp-file "tramp-rpc.")))
      (with-temp-file tmpfile
        (set-buffer-multibyte nil)
        (insert content))
      tmpfile)))

(defun tramp-rpc-handle-get-home-directory (vec &optional user)
  "Return home directory for USER on remote host VEC using RPC.
If USER is nil or matches the connection user, returns the current user's
home directory from system.info.  For other users, looks up via getent."
  (let* ((conn-user (tramp-file-name-user vec))
         (target-user (or user conn-user)))
    (if (or (null target-user)
            (string-empty-p target-user)
            (equal target-user conn-user))
        ;; Current user - use system.info
        (let ((result (tramp-rpc--call vec "system.info" nil)))
          (alist-get 'home result))
      ;; Different user - look up via getent passwd
      (condition-case nil
          (let* ((result (tramp-rpc--call vec "process.run"
                                          `((cmd . "getent")
                                            (args . ["passwd" ,target-user])
                                            (cwd . "/"))))
                 (exit-code (alist-get 'exit_code result))
                 (stdout (tramp-rpc--decode-output
                          (alist-get 'stdout result)
                          (alist-get 'stdout_encoding result))))
            (when (and (eq exit-code 0) (> (length stdout) 0))
              ;; getent passwd format: name:x:uid:gid:gecos:home:shell
              (let ((fields (split-string (string-trim stdout) ":")))
                (when (>= (length fields) 6)
                  (nth 5 fields)))))
        (error nil)))))

(defun tramp-rpc-handle-get-remote-uid (vec id-format)
  "Return remote UID using RPC."
  (let ((result (tramp-rpc--call vec "system.info" nil)))
    (let ((uid (alist-get 'uid result)))
      (if (eq id-format 'integer)
          uid
        (number-to-string uid)))))

(defun tramp-rpc-handle-get-remote-gid (vec id-format)
  "Return remote GID using RPC."
  (let ((result (tramp-rpc--call vec "system.info" nil)))
    (let ((gid (alist-get 'gid result)))
      (if (eq id-format 'integer)
          gid
        (number-to-string gid)))))

(defun tramp-rpc-handle-file-ownership-preserved-p (filename &optional group)
  "Like `file-ownership-preserved-p' for TRAMP-RPC files.
Check if file ownership would be preserved when creating FILENAME.
If GROUP is non-nil, also check that group would be preserved."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (let ((attributes (file-attributes filename 'integer)))
      ;; Return t if the file doesn't exist, since it's true that no
      ;; information would be lost by an (attempted) delete and create.
      (or (null attributes)
          (and
           (= (file-attribute-user-id attributes)
              (tramp-rpc-handle-get-remote-uid v 'integer))
           (or (not group)
               (= (file-attribute-group-id attributes)
                  (tramp-rpc-handle-get-remote-gid v 'integer))))))))

(defun tramp-rpc-handle-access-file (filename string)
  "Like `access-file' for TRAMP-RPC files.
Check if FILENAME is accessible, signaling an error if not.
STRING is used in error messages."
  (setq filename (file-truename filename))
  (if (file-exists-p filename)
      (unless (funcall
               (if (file-directory-p filename)
                   #'file-accessible-directory-p #'file-readable-p)
               filename)
        (tramp-error
         (tramp-dissect-file-name filename)
         'file-error
         (format "%s: Permission denied, %s" string filename)))
    (tramp-error
     (tramp-dissect-file-name filename)
     'file-missing
     (format "%s: No such file or directory, %s" string filename))))

(defun tramp-rpc-handle-file-name-case-insensitive-p (_filename)
  "Like `file-name-case-insensitive-p' for TRAMP-RPC files.
Returns nil since most remote systems (Linux) are case-sensitive."
  ;; For simplicity, assume case-sensitive (most common for remote servers).
  ;; A more thorough implementation would check the remote filesystem type.
  nil)

(defun tramp-rpc-handle-file-name-as-directory (file)
  "Like `file-name-as-directory' for TRAMP-RPC files."
  (with-parsed-tramp-file-name file nil
    (tramp-make-tramp-file-name
     v (if (tramp-string-empty-or-nil-p localname)
           "/"
         (file-name-as-directory localname)))))

;; ============================================================================
;; Async Process Support
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

(defcustom tramp-rpc-use-async-read t
  "Whether to use async callback-based reads for pipe processes.
When t (default), uses fast async reads with server-side blocking.
When nil, uses timer-based polling (slower, for debugging)."
  :type 'boolean
  :group 'tramp-rpc)

(defcustom tramp-rpc-async-read-timeout-ms 100
  "Timeout in milliseconds for async process reads.
The server will block for this long waiting for data before returning.
Lower values mean more responsive but higher CPU usage."
  :type 'integer
  :group 'tramp-rpc)

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
    (list :stdout (when-let ((s (alist-get 'stdout result)))
                    (tramp-rpc--decode-output
                     s (alist-get 'stdout_encoding result)))
          :stderr (when-let ((s (alist-get 'stderr result)))
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
        ;; Send the write
        (tramp-rpc--call-async vec "process.write"
                               `((pid . ,pid)
                                 (data . ,(base64-encode-string data t)))
                               (lambda (response)
                                 (when (plist-get response :error)
                                   (tramp-rpc--debug "WRITE-ERROR pid=%s: %s"
                                                    pid (plist-get response :error)))
                                 ;; Mark as not writing and process next item
                                 (let ((q (gethash queue-key tramp-rpc--process-write-queues)))
                                   (puthash queue-key
                                            (list :pending (plist-get q :pending) :writing nil)
                                            tramp-rpc--process-write-queues))
                                 (tramp-rpc--process-write-queue queue-key)))))))

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

(defvar tramp-rpc--delivering-output nil
  "Non-nil when we're delivering output to the local relay process.
Used to bypass the process-send-string advice.")

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
               (stdout (when-let ((s (alist-get 'stdout result)))
                         (tramp-rpc--decode-output
                          s (alist-get 'stdout_encoding result))))
               (stderr (when-let ((s (alist-get 'stderr result)))
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
      (when-let ((old-timer (plist-get info :timer)))
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
            (when-let ((stdout (plist-get result :stdout)))
              (when (> (length stdout) 0)
                (setq had-output t)
                (if-let ((filter (process-filter local-process)))
                    (funcall filter local-process stdout)
                  (when-let ((buf (process-buffer local-process)))
                    (when (buffer-live-p buf)
                      (with-current-buffer buf
                        (let ((inhibit-read-only t))
                          (goto-char (point-max))
                          (insert stdout))))))))
            
            ;; Handle stderr - send to stderr buffer if specified
            (when-let ((stderr (plist-get result :stderr)))
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
                  (if-let ((filter (process-filter local-process)))
                      (funcall filter local-process stderr)
                    (when-let ((buf (process-buffer local-process)))
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
      (when-let ((timer (plist-get info :timer)))
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

(defun tramp-rpc--find-executable (vec program)
  "Find PROGRAM in the remote PATH on VEC.
Returns the absolute path or nil.
Uses `command -v` for efficient lookup via login shell."
  (condition-case nil
      (let* ((result (tramp-rpc--call vec "process.run"
                                       `((cmd . "/bin/sh")
                                         (args . ["-l" "-c" ,(concat "command -v " program)])
                                         (cwd . "/"))))
             (exit-code (alist-get 'exit_code result))
             (stdout (tramp-rpc--decode-output
                      (alist-get 'stdout result)
                      (alist-get 'stdout_encoding result))))
        (when (and (eq exit-code 0) (> (length stdout) 0))
          (string-trim stdout)))
    (error nil)))

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
  (when-let ((buf (process-buffer process)))
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
               (output (when-let ((o (alist-get 'output result)))
                         (tramp-rpc--decode-output
                          o (alist-get 'output_encoding result))))
               (exited (alist-get 'exited result))
               (exit-code (alist-get 'exit_code result)))
          
          ;; Deliver output via filter
          (when (and output (> (length output) 0))
            (if-let ((filter (process-filter local-process)))
                (funcall filter local-process output)
              (when-let ((buf (process-buffer local-process)))
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
  (when-let ((vec (process-get local-process :tramp-rpc-vec))
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
      (when-let ((vec (process-get process :tramp-rpc-vec))
                 (pid (process-get process :tramp-rpc-pid)))
        (ignore-errors
          (tramp-rpc--call vec "process.kill_pty"
                           `((pid . ,pid) (signal . 9)))))
      ;; Remove from tracking
      (remhash process tramp-rpc--pty-processes))
    ;; Call user sentinel
    (when-let ((user-sentinel (process-get process :tramp-rpc-user-sentinel)))
      (funcall user-sentinel process event))))

(defun tramp-rpc--adjust-pty-window-size (process _windows)
  "Adjust PTY window size when Emacs window size changes.
PROCESS is the local relay process, WINDOWS is the list of windows.
Returns nil to tell Emacs not to call `set-process-window-size' on
the local relay process (we handle resizing via RPC to the remote)."
  (when (and (process-live-p process)
             (process-get process :tramp-rpc-pty))
    (when-let ((vec (process-get process :tramp-rpc-vec))
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
    (when-let ((vec (process-get process :tramp-rpc-vec))
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

(defun tramp-rpc--vterm-window-adjust-process-window-size-advice (orig-fun process windows)
  "Advice for vterm's window adjust function to handle TRAMP-RPC PTY processes.
For tramp-rpc processes, resize the remote PTY and update vterm's display."
  (if (and (processp process)
           (process-get process :tramp-rpc-pty))
      ;; This is a tramp-rpc PTY process
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
             (vterm--set-size vterm--term height width)))))
    ;; Not our process, call original
    (funcall orig-fun process windows)))

(defun tramp-rpc--eat-adjust-process-window-size-advice (orig-fun process windows)
  "Advice for eat's window adjust function to handle TRAMP-RPC PTY processes.
For tramp-rpc processes, resize the remote PTY and update eat's display."
  (if (and (processp process)
           (process-get process :tramp-rpc-pty))
      ;; This is a tramp-rpc PTY process
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
           ('eshell-mode (run-hooks 'eat-eshell-update-hook)))))
    ;; Not our process, call original
    (funcall orig-fun process windows)))

;; ============================================================================
;; Advice for process operations
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
       ;; PTY process - use PTY write (async, fire-and-forget)
       ((and proc (process-get proc :tramp-rpc-pty))
        (let ((vec (process-get proc :tramp-rpc-vec))
              (pid (process-get proc :tramp-rpc-pid)))
          (tramp-rpc--debug "SEND-STRING PTY pid=%s len=%d" pid (length string))
          ;; Send write request asynchronously - don't wait for response
          (tramp-rpc--call-async vec "process.write_pty"
                                 `((pid . ,pid)
                                   (data . ,(base64-encode-string string t)))
                                 #'ignore)  ; Ignore the response
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

(defun tramp-rpc--process-send-eof-advice (orig-fun &optional process)
  "Advice for `process-send-eof' to handle TRAMP-RPC processes."
  (let ((proc (or process (get-buffer-process (current-buffer)))))
    (if-let ((pid (process-get proc :tramp-rpc-pid))
             (vec (process-get proc :tramp-rpc-vec)))
        (condition-case err
            (tramp-rpc--close-remote-stdin vec pid)
          (error
           (message "tramp-rpc: Error closing stdin: %s" err)))
      (funcall orig-fun process))))

(defun tramp-rpc--signal-process-advice (orig-fun process sigcode &optional remote)
  "Advice for `signal-process' to handle TRAMP-RPC processes."
  (if-let ((pid (and (processp process)
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

(defun tramp-rpc--process-status-advice (orig-fun process)
  "Advice for `process-status' to handle TRAMP-RPC processes."
  (if (process-get process :tramp-rpc-pid)
      (cond
       ((process-get process :tramp-rpc-exited) 'exit)
       ;; Use orig-fun to check live status, not process-live-p (which would recurse)
       ((memq (funcall orig-fun process) '(run open listen connect)) 'run)
       (t 'exit))
    (funcall orig-fun process)))

(defun tramp-rpc--process-exit-status-advice (orig-fun process)
  "Advice for `process-exit-status' to handle TRAMP-RPC processes."
  (if (process-get process :tramp-rpc-pid)
      (or (process-get process :tramp-rpc-exit-code) 0)
    (funcall orig-fun process)))

(defun tramp-rpc--process-command-advice (orig-fun process)
  "Advice for `process-command' to return stored command for PTY processes."
  (or (process-get process :tramp-rpc-command)
      (funcall orig-fun process)))

(defun tramp-rpc--process-tty-name-advice (orig-fun process &optional stream)
  "Advice for `process-tty-name' to return stored TTY name for PTY processes.
For TRAMP-RPC PTY processes, return the remote TTY name stored during creation."
  (if (and (processp process)
           (process-get process :tramp-rpc-pty))
      (process-get process :tramp-rpc-tty-name)
    (funcall orig-fun process stream)))

;; Install advice
(advice-add 'process-send-string :around #'tramp-rpc--process-send-string-advice)
(advice-add 'process-send-eof :around #'tramp-rpc--process-send-eof-advice)
(advice-add 'signal-process :around #'tramp-rpc--signal-process-advice)
(advice-add 'process-status :around #'tramp-rpc--process-status-advice)
(advice-add 'process-exit-status :around #'tramp-rpc--process-exit-status-advice)
(advice-add 'process-command :around #'tramp-rpc--process-command-advice)
(advice-add 'process-tty-name :around #'tramp-rpc--process-tty-name-advice)

;; Advice vterm's window resize function for tramp-rpc PTY support
(with-eval-after-load 'vterm
  (advice-add 'vterm--window-adjust-process-window-size :around
              #'tramp-rpc--vterm-window-adjust-process-window-size-advice))

;; Advice eat's window resize function for tramp-rpc PTY support
(with-eval-after-load 'eat
  (advice-add 'eat--adjust-process-window-size :around
              #'tramp-rpc--eat-adjust-process-window-size-advice))

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
  (if (and (file-remote-p default-directory)
           (string-match-p "^/rpc:" default-directory))
      contact
    (funcall orig-fun contact)))

(with-eval-after-load 'eglot
  (advice-add 'eglot--cmd :around #'tramp-rpc--eglot-cmd-advice))

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

(advice-add 'vc-call-backend :around #'tramp-rpc--vc-call-backend-advice)

;; ============================================================================
;; File name handler registration
;; ============================================================================

(defconst tramp-rpc-file-name-handler-alist
  '(;; =========================================================================
    ;; RPC-based file attribute operations
    ;; =========================================================================
    (file-exists-p . tramp-rpc-handle-file-exists-p)
    (file-readable-p . tramp-rpc-handle-file-readable-p)
    (file-writable-p . tramp-rpc-handle-file-writable-p)
    (file-executable-p . tramp-rpc-handle-file-executable-p)
    (file-directory-p . tramp-rpc-handle-file-directory-p)
    (file-regular-p . tramp-rpc-handle-file-regular-p)
    (file-symlink-p . tramp-rpc-handle-file-symlink-p)
    (file-truename . tramp-rpc-handle-file-truename)
    (file-attributes . tramp-rpc-handle-file-attributes)
    (file-modes . tramp-rpc-handle-file-modes)
    (file-newer-than-file-p . tramp-rpc-handle-file-newer-than-file-p)
    (file-ownership-preserved-p . tramp-rpc-handle-file-ownership-preserved-p)
    (file-system-info . tramp-rpc-handle-file-system-info)

    ;; =========================================================================
    ;; RPC-based file modification operations
    ;; =========================================================================
    (set-file-modes . tramp-rpc-handle-set-file-modes)
    (set-file-times . tramp-rpc-handle-set-file-times)
    (tramp-set-file-uid-gid . tramp-rpc-handle-set-file-uid-gid)

    ;; =========================================================================
    ;; RPC-based directory operations
    ;; =========================================================================
    (directory-files . tramp-rpc-handle-directory-files)
    (directory-files-and-attributes . tramp-rpc-handle-directory-files-and-attributes)
    (file-name-all-completions . tramp-rpc-handle-file-name-all-completions)
    (make-directory . tramp-rpc-handle-make-directory)
    (delete-directory . tramp-rpc-handle-delete-directory)
    (insert-directory . tramp-rpc-handle-insert-directory)

    ;; =========================================================================
    ;; RPC-based file I/O operations
    ;; =========================================================================
    (insert-file-contents . tramp-rpc-handle-insert-file-contents)
    (write-region . tramp-rpc-handle-write-region)
    (copy-file . tramp-rpc-handle-copy-file)
    (rename-file . tramp-rpc-handle-rename-file)
    (delete-file . tramp-rpc-handle-delete-file)
    (make-symbolic-link . tramp-rpc-handle-make-symbolic-link)
    (add-name-to-file . tramp-rpc-handle-add-name-to-file)
    (file-local-copy . tramp-rpc-handle-file-local-copy)

    ;; =========================================================================
    ;; RPC-based process operations
    ;; =========================================================================
    (process-file . tramp-rpc-handle-process-file)
    (shell-command . tramp-rpc-handle-shell-command)
    (make-process . tramp-rpc-handle-make-process)
    (start-file-process . tramp-rpc-handle-start-file-process)

    ;; =========================================================================
    ;; RPC-based system information
    ;; =========================================================================
    (tramp-get-home-directory . tramp-rpc-handle-get-home-directory)
    (tramp-get-remote-uid . tramp-rpc-handle-get-remote-uid)
    (tramp-get-remote-gid . tramp-rpc-handle-get-remote-gid)
    (tramp-get-remote-groups . tramp-rpc-handle-get-remote-groups)
    (exec-path . tramp-rpc-handle-exec-path)

    ;; =========================================================================
    ;; RPC-based extended attributes (ACL/SELinux via process.run)
    ;; =========================================================================
    (file-acl . tramp-rpc-handle-file-acl)
    (set-file-acl . tramp-rpc-handle-set-file-acl)
    (file-selinux-context . tramp-rpc-handle-file-selinux-context)
    (set-file-selinux-context . tramp-rpc-handle-set-file-selinux-context)

    ;; =========================================================================
    ;; RPC-based path and VC operations
    ;; =========================================================================
    (expand-file-name . tramp-rpc-handle-expand-file-name)
    (vc-registered . tramp-rpc-handle-vc-registered)

    ;; =========================================================================
    ;; Generic TRAMP handlers (work with any backend, no remote I/O needed)
    ;; These use tramp-handle-* functions that operate on cached data or
    ;; delegate to our RPC handlers internally.
    ;; =========================================================================
    (access-file . tramp-rpc-handle-access-file)
    (directory-file-name . tramp-handle-directory-file-name)
    (dired-uncache . tramp-handle-dired-uncache)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-name-as-directory . tramp-rpc-handle-file-name-as-directory)
    (file-name-case-insensitive-p . tramp-rpc-handle-file-name-case-insensitive-p)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    (file-remote-p . tramp-handle-file-remote-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    (load . tramp-handle-load)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)

    ;; =========================================================================
    ;; Generic TRAMP handlers for local Emacs state (locking, modtime, temp files)
    ;; =========================================================================
    (file-locked-p . tramp-handle-file-locked-p)
    (lock-file . tramp-handle-lock-file)
    (unlock-file . tramp-handle-unlock-file)
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (temporary-file-directory . tramp-handle-temporary-file-directory)

    ;; =========================================================================
    ;; Generic TRAMP handlers for file notifications
    ;; =========================================================================
    (file-notify-add-watch . tramp-handle-file-notify-add-watch)
    (file-notify-rm-watch . tramp-handle-file-notify-rm-watch)
    (file-notify-valid-p . tramp-handle-file-notify-valid-p)

    ;; =========================================================================
    ;; Intentionally ignored (not applicable or handled elsewhere)
    ;; =========================================================================
    (byte-compiler-base-file-name . ignore)  ; Not needed for remote files
    (diff-latest-backup-file . ignore)       ; Backup handling is local
    (make-directory-internal . ignore)       ; We implement make-directory
    (unhandled-file-name-directory . ignore) ; Should return nil for TRAMP
    )
  "Alist of handler functions for TRAMP-RPC method.")

(defun tramp-rpc-file-name-handler (operation &rest args)
  "Invoke TRAMP-RPC file name handler for OPERATION with ARGS."
  (if-let ((handler (assq operation tramp-rpc-file-name-handler-alist)))
      (save-match-data (apply (cdr handler) args))
    (tramp-run-real-handler operation args)))

;; ============================================================================
;; Method registration
;; ============================================================================

;;;###autoload
(tramp-register-foreign-file-name-handler
 #'tramp-rpc-file-name-p #'tramp-rpc-file-name-handler)

(defun tramp-rpc-file-name-p (vec-or-filename)
  "Check if VEC-OR-FILENAME is handled by TRAMP-RPC.
VEC-OR-FILENAME can be either a tramp-file-name struct or a filename string."
  (let ((method (cond
                 ((tramp-file-name-p vec-or-filename)
                  (tramp-file-name-method vec-or-filename))
                 ((stringp vec-or-filename)
                  (and (tramp-tramp-file-p vec-or-filename)
                       (tramp-file-name-method (tramp-dissect-file-name vec-or-filename))))
                 (t nil))))
    (string= method tramp-rpc-method)))

;;;###autoload
(add-to-list 'tramp-methods
             `(,tramp-rpc-method
               ;; Minimal method entry - no shell setup needed
               ;; The foreign file name handler handles everything
               ))

;; ============================================================================
;; Unload support
;; ============================================================================

(defun tramp-rpc-unload-function ()
  "Unload function for tramp-rpc.
Removes advice and cleans up async processes."
  ;; Remove advice
  (advice-remove 'process-send-string #'tramp-rpc--process-send-string-advice)
  (advice-remove 'process-send-eof #'tramp-rpc--process-send-eof-advice)
  (advice-remove 'signal-process #'tramp-rpc--signal-process-advice)
  (advice-remove 'process-status #'tramp-rpc--process-status-advice)
  (advice-remove 'process-exit-status #'tramp-rpc--process-exit-status-advice)
  (advice-remove 'process-command #'tramp-rpc--process-command-advice)
  (advice-remove 'vc-call-backend #'tramp-rpc--vc-call-backend-advice)
  ;; Clean up all async processes
  (tramp-rpc--cleanup-async-processes)
  ;; Clean up PTY processes
  (maphash (lambda (proc info)
             (when-let ((timer (plist-get info :timer)))
               (cancel-timer timer))
             (when (process-live-p proc)
               (delete-process proc)))
           tramp-rpc--pty-processes)
  (clrhash tramp-rpc--pty-processes)
  ;; Return nil to allow normal unload to proceed
  nil)

(provide 'tramp-rpc)
;;; tramp-rpc.el ends here
