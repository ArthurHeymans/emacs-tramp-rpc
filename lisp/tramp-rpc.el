;;; tramp-rpc.el --- TRAMP backend using RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Version: 0.4.0
;; Keywords: comm, processes, files
;; Package-Requires: ((emacs "30.1") (msgpack "0"))

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
;; Once installed, just access files using the "rpc" method:
;;   /rpc:user@host:/path/to/file
;;
;; The package autoloads automatically - no (require 'tramp-rpc) needed.
;;
;; FEATURES:
;; - Fast file operations via binary RPC protocol
;; - Async process support (make-process, start-file-process)
;; - VC mode integration works (git, etc.)
;; - Hybrid PTY mode: direct SSH for interactive terminals (vterm, shell)
;;
;; HYBRID PTY MODE (DEFAULT):
;; For interactive terminal use (vterm, shell-mode, term-mode), tramp-rpc
;; uses a direct SSH connection with PTY allocation instead of going through
;; the RPC server.  This provides the same low-latency experience as using
;; SSH directly with vterm, while file operations still benefit from RPC.
;;
;; The direct SSH connection reuses the ControlMaster socket, so there's
;; no additional authentication needed.  To disable this and use RPC for
;; all PTY operations, set:
;;   (setq tramp-rpc-use-direct-ssh-pty nil)
;;
;; HOW ASYNC PROCESSES WORK:
;; Remote processes are started via RPC and polled periodically for output.
;; A local pipe process serves as a relay to provide Emacs process semantics.
;; Process filters, sentinels, and signals all work as expected.
;;
;; OPTIONAL CONFIGURATION:
;; If you experience issues with diff-hl in dired, you can disable it:
;;   (setq diff-hl-disable-on-remote t)
;;
;; AUTHENTICATION:
;; When ControlMaster is enabled (default), tramp-rpc establishes the SSH
;; ControlMaster connection first, which supports both key-based and password
;; authentication.  If your SSH key isn't available, you'll be prompted for
;; a password.  Subsequent operations reuse this connection without prompting.

;;; Code:

;; Autoload support - these forms are extracted to tramp-rpc-autoloads.el
;; and run at package-initialize time, before the full file is loaded.

;;;###autoload
(defconst tramp-rpc-method "rpc"
  "TRAMP method for RPC-based remote access.")

;;;###autoload
(with-eval-after-load 'tramp
  ;; Register the method
  (add-to-list 'tramp-methods `(,tramp-rpc-method))

  ;; Define the predicate inline (as defsubst) so it's available without
  ;; loading tramp-rpc.el.  This avoids recursive autoloading: TRAMP calls
  ;; the predicate to decide which handler to use, and if it were an
  ;; autoload stub it would load tramp-rpc.el which `(require 'tramp)'.
  ;; Reference TRAMP uses the same pattern (defsubst in tramp-loaddefs.el).
  (defsubst tramp-rpc-file-name-p (vec-or-filename)
    "Check if VEC-OR-FILENAME is handled by TRAMP-RPC."
    (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename)))
      (string= (tramp-file-name-method vec) tramp-rpc-method)))

  ;; Register the foreign handler directly in the alist.  We cannot use
  ;; `tramp-register-foreign-file-name-handler' here because it tries to
  ;; read `tramp-rpc-file-name-handler-alist' (defined in the full file),
  ;; which isn't loaded yet.  The handler function itself is an autoload
  ;; stub that triggers loading of tramp-rpc.el on first use.
  (add-to-list 'tramp-foreign-file-name-handler-alist
               '(tramp-rpc-file-name-p . tramp-rpc-file-name-handler)))

;; Now the actual implementation
(require 'cl-lib)
(require 'tramp)
(require 'tramp-sh)
(require 'tramp-rpc-protocol)
(require 'tramp-rpc-deploy)

;; Silence byte-compiler warnings for functions defined elsewhere

;; Silence byte-compiler warnings for variables defined in vterm
(defvar vterm-copy-mode)
(defvar vterm-min-window-width)
(defvar vterm--term)

;; Silence byte-compiler warnings for variables defined later in this file
(defvar tramp-rpc-magit--prefetch-directory)
(defvar tramp-rpc-magit--debug)
(defvar tramp-rpc-magit--status-cache)
(defvar tramp-rpc-magit--ancestors-cache)
(defvar tramp-rpc--file-exists-cache)
(defvar tramp-rpc--file-truename-cache)
(defvar tramp-rpc--cache-ttl)
(defvar tramp-rpc--cache-max-size)

;; Silence byte-compiler warnings for external functions
(declare-function projectile-dir-files-alien "projectile")
(declare-function projectile-time-seconds "projectile")
(declare-function magit-status-setup-buffer "magit-status")
(declare-function magit-get-mode-buffer "magit-mode")

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

(defcustom tramp-rpc-ssh-options nil
  "Additional SSH options to pass when connecting.
This is a list of strings, each of which is passed as an SSH -o option.
For example, to disable strict host key checking:
  (setq tramp-rpc-ssh-options \\='(\"StrictHostKeyChecking=no\"
                                 \"UserKnownHostsFile=/dev/null\"))

Note: The following options are always passed by default:
  - BatchMode=yes (for RPC connection; ControlMaster handles auth first)
  - StrictHostKeyChecking=accept-new (accept new keys, reject changed)
  - ControlMaster/ControlPath/ControlPersist (if `tramp-rpc-use-controlmaster')

Set this variable to override or supplement these defaults."
  :type '(repeat string)
  :group 'tramp-rpc)

(defcustom tramp-rpc-ssh-args nil
  "Raw SSH arguments to pass when connecting.
This is a list of strings that are passed directly to SSH.
For example: \\='(\"-v\" \"-F\" \"/path/to/config\")

Unlike `tramp-rpc-ssh-options' which adds -o options, this allows
passing any SSH command-line arguments."
  :type '(repeat string)
  :group 'tramp-rpc)

(defcustom tramp-rpc-debug nil
  "When non-nil, log debug messages to *tramp-rpc-debug* buffer.
Set to t to enable debugging for hang diagnosis."
  :type 'boolean
  :group 'tramp-rpc)

(defcustom tramp-rpc-use-direct-ssh-pty t
  "When non-nil, use direct SSH for PTY processes instead of RPC.
This provides much lower latency for interactive terminal use (vterm, shell)
by creating a direct SSH connection with PTY rather than going through
the RPC server.  File operations still use the efficient RPC protocol.

The direct SSH connection reuses the ControlMaster socket established
by tramp-rpc, so authentication is already handled.

Set to nil to use the RPC-based PTY implementation instead."
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

(defvar tramp-rpc--pending-responses (make-hash-table :test 'eq)
  "Hash table mapping buffers to their pending response hash tables.
Each buffer has its own hash table mapping request IDs to response plists.")

(defun tramp-rpc--get-pending-responses (buffer)
  "Get the pending responses hash table for BUFFER, creating if needed."
  (or (gethash buffer tramp-rpc--pending-responses)
      (puthash buffer (make-hash-table :test 'eql) tramp-rpc--pending-responses)))

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

(defun tramp-rpc--get-system-info (vec)
  "Return cached system.info for VEC.
The result is cached at connection time and reused for all subsequent
calls to avoid redundant RPC round-trips for get-home-directory,
get-remote-uid, and get-remote-gid."
  (let ((conn (tramp-rpc--get-connection vec)))
    (or (and conn (plist-get conn :system-info))
        ;; Fallback: call the server if cache miss (shouldn't happen
        ;; normally since we cache at connection time)
        (let ((result (tramp-rpc--call vec "system.info" nil)))
          (when (and result conn)
            (plist-put conn :system-info result))
          result))))

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

(defvar tramp-rpc--password-prompt-regexp
  (rx (or "password:" "Password:" "Password for" "passphrase"
          (seq "Enter passphrase for")))
  "Regexp matching SSH password/passphrase prompts.")

(defun tramp-rpc--controlmaster-socket-path (vec)
  "Return the ControlMaster socket path for VEC.
Expands SSH escape sequences in `tramp-rpc-controlmaster-path'."
  (let* ((host (tramp-file-name-host vec))
         (user (or (tramp-file-name-user vec) (user-login-name)))
         (port (or (tramp-file-name-port vec) 22))
         (path tramp-rpc-controlmaster-path))
    ;; Expand common SSH escape sequences
    ;; %h = host, %r = remote user, %p = port
    ;; %C = hash of %l%h%p%r (we approximate this)
    (setq path (replace-regexp-in-string "%h" host path t t))
    (setq path (replace-regexp-in-string "%r" user path t t))
    (setq path (replace-regexp-in-string "%p" (number-to-string port) path t t))
    ;; For %C, use a simple hash approximation
    (setq path (replace-regexp-in-string
                "%C"
                (md5 (format "%s%s%s%s" (system-name) host port user))
                path t t))
    (expand-file-name path)))

(defun tramp-rpc--controlmaster-active-p (vec)
  "Return non-nil if a ControlMaster connection is active for VEC."
  (let ((socket-path (tramp-rpc--controlmaster-socket-path vec))
        (host (tramp-file-name-host vec))
        (user (tramp-file-name-user vec))
        (port (tramp-file-name-port vec)))
    (and (file-exists-p socket-path)
         ;; Check if the socket is actually usable via ssh -O check
         (zerop (apply #'call-process "ssh" nil nil nil
                       (append
                        (when user (list "-l" user))
                        (when port (list "-p" (number-to-string port)))
                        (list "-o" (format "ControlPath=%s" socket-path)
                              "-O" "check"
                              host)))))))

(cl-defun tramp-rpc--establish-controlmaster (vec)
  "Establish a ControlMaster connection for VEC.
This creates an interactive SSH connection (without BatchMode) that can
prompt for passwords if needed, then keeps it running as a ControlMaster.
Subsequent BatchMode connections reuse this socket.
Returns non-nil on success."
  ;; Check if already connected
  (when (tramp-rpc--controlmaster-active-p vec)
    (tramp-rpc--debug "ControlMaster already active for %s" (tramp-file-name-host vec))
    (cl-return-from tramp-rpc--establish-controlmaster t))
  (tramp-rpc--ensure-controlmaster-directory)
  (let* ((host (tramp-file-name-host vec))
         (user (tramp-file-name-user vec))
         (port (tramp-file-name-port vec))
         (socket-path (tramp-rpc--controlmaster-socket-path vec))
         (process-name (format "*tramp-rpc-auth %s*" host))
         (buffer (get-buffer-create (format " *tramp-rpc-auth %s*" host)))
         (ssh-args (append
                    (list "ssh")
                    tramp-rpc-ssh-args
                    (when user (list "-l" user))
                    (when port (list "-p" (number-to-string port)))
                    ;; NO BatchMode - allow password prompts
                    (list "-o" "StrictHostKeyChecking=accept-new")
                    ;; ControlMaster options
                    (list "-o" "ControlMaster=yes"
                          "-o" (format "ControlPath=%s" socket-path)
                          "-o" (format "ControlPersist=%s"
                                       tramp-rpc-controlmaster-persist))
                    ;; Connect and immediately exit, leaving ControlMaster running
                    (list "-N" host)))
         process)
    (with-current-buffer buffer
      (erase-buffer))
    ;; Start SSH with PTY for interactive password prompt
    (let ((process-connection-type t))  ; Use PTY for password prompts
      (setq process (apply #'start-process process-name buffer ssh-args)))
    (set-process-query-on-exit-flag process nil)
    ;; Handle password prompts and wait for connection
    (let ((start-time (current-time))
          (timeout 60))  ; 60 second timeout for authentication
      (while (and (process-live-p process)
                  (not (file-exists-p socket-path))
                  (< (float-time (time-subtract (current-time) start-time)) timeout))
        (accept-process-output process 0.1)
        (with-current-buffer buffer
          (goto-char (point-min))
          (when (re-search-forward tramp-rpc--password-prompt-regexp nil t)
            ;; Password prompt detected - ask user
            (let ((password (read-passwd
                             (format "Password for %s@%s: "
                                     (or user (user-login-name)) host))))
              (when password
                (process-send-string process (concat password "\n"))
                ;; Clear the buffer to avoid re-matching the same prompt
                (erase-buffer)))))))
    ;; Check if authentication succeeded (socket was created)
    (if (file-exists-p socket-path)
        (progn
          ;; Give it a moment to stabilize
          (sleep-for 0.1)
          t)
      ;; Authentication failed
      (when (process-live-p process)
        (delete-process process))
      (let ((output (with-current-buffer buffer (buffer-string))))
        (error "Failed to establish SSH connection to %s: %s" host output)))))

(defun tramp-rpc--connect (vec)
  "Establish an RPC connection to VEC."
  ;; Ensure ControlMaster directory exists
  (tramp-rpc--ensure-controlmaster-directory)
  ;; When ControlMaster is enabled, establish it first.
  ;; This handles both key-based and password authentication:
  ;; - Key-based: connects silently
  ;; - Password: prompts user, then subsequent connections reuse it
  (when tramp-rpc-use-controlmaster
    (tramp-rpc--establish-controlmaster vec))
  ;; Ensure the binary is deployed using shell-based tramp
  (let* ((binary-path (tramp-rpc-deploy-ensure-binary vec))
         (host (tramp-file-name-host vec))
         (user (tramp-file-name-user vec))
         (port (tramp-file-name-port vec))
         ;; Build SSH command to run the RPC server
         (ssh-args (append
                    (list "ssh")
                    ;; Raw SSH arguments (e.g., -v, -F config)
                    tramp-rpc-ssh-args
                    (when user (list "-l" user))
                    (when port (list "-p" (number-to-string port)))
                    ;; Default options
                    (list "-o" "BatchMode=yes")
                    (list "-o" "StrictHostKeyChecking=accept-new")
                    ;; User-specified SSH options
                    (mapcan (lambda (opt) (list "-o" opt))
                            tramp-rpc-ssh-options)
                    ;; ControlMaster options for connection sharing
                    ;; Use the expanded socket path to match what establish-controlmaster created
                    (when tramp-rpc-use-controlmaster
                      (list "-o" "ControlMaster=auto"
                            "-o" (format "ControlPath=%s"
                                         (tramp-rpc--controlmaster-socket-path vec))
                            "-o" (format "ControlPersist=%s"
                                         tramp-rpc-controlmaster-persist)))
                    (list host binary-path)))
         ;; Use TRAMP's standard naming so tramp-get-connection-process works
         (process-name (tramp-get-connection-name vec))
         (buffer-name (tramp-buffer-name vec))
         (buffer (get-buffer-create buffer-name))
         process)

    ;; Clear buffer - use unibyte for binary MessagePack framing
    (with-current-buffer buffer
      (erase-buffer)
      (set-buffer-multibyte nil))

    ;; Start the process with pipe connection (not PTY)
    ;; PTY has line buffering and ~4KB line length limits that break large JSON-RPC requests
    (let ((process-connection-type nil))  ; Use pipes, not PTY
      (setq process (apply #'start-process process-name buffer ssh-args)))

    ;; Configure process
    (set-process-query-on-exit-flag process nil)
    (set-process-coding-system process 'binary 'binary)
    
    ;; Set up filter for async response handling
    (set-process-filter process #'tramp-rpc--connection-filter)

    ;; Store connection
    (tramp-rpc--set-connection vec process buffer)

    ;; Wait for server to be ready by sending a ping, and cache system.info
    (let ((response (tramp-rpc--call vec "system.info" nil)))
      (unless response
        (tramp-rpc--remove-connection vec)
        (error "Failed to connect to RPC server on %s" host))
      ;; Cache system.info in the connection plist (avoids repeated calls
      ;; for get-home-directory, get-remote-uid, get-remote-gid)
      (let ((conn (tramp-rpc--get-connection vec)))
        (when conn
          (plist-put conn :system-info response))))

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
       (when-let* ((timer (plist-get info :timer)))
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
Handles async responses by dispatching to registered callbacks.
Uses length-prefixed binary framing: <4-byte BE length><msgpack payload>."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Append output to buffer
        (goto-char (point-max))
        (insert output)
        (tramp-rpc--debug "FILTER received %d bytes, buffer-size=%d"
                         (length output) (buffer-size))
        ;; Process complete messages
        (goto-char (point-min))
        (let ((result t))
          (while result
            (setq result (tramp-rpc-protocol-try-read-message
                          (buffer-substring (point-min) (point-max))))
            (when result
              (let ((response (car result))
                    (remaining (cdr result)))
                ;; Replace buffer contents with remaining data
                (erase-buffer)
                (insert remaining)
                (goto-char (point-min))
                ;; Handle the response
                (let* ((id (plist-get response :id))
                       (callback (gethash id tramp-rpc--async-callbacks)))
                  (if callback
                      (progn
                        (tramp-rpc--debug "FILTER dispatching async id=%s" id)
                        (remhash id tramp-rpc--async-callbacks)
                        (funcall callback response))
                    ;; Not an async response - store for sync code
                    (tramp-rpc--debug "FILTER storing sync response id=%s" id)
                    (puthash id response (tramp-rpc--get-pending-responses buffer))))))))))))

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
    ;; Send request (binary data with length prefix, no newline)
    (process-send-string process request)
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
  "Check pending responses for EXPECTED-ID.
Returns the response plist if found and removes it from pending, nil otherwise."
  (let* ((pending (tramp-rpc--get-pending-responses (current-buffer)))
         (response (gethash expected-id pending)))
    (when response
      (remhash expected-id pending)
      response)))

(defun tramp-rpc--process-accessible-p (process)
  "Return t if PROCESS can be accessed from the current thread.
Returns nil if the process is locked to a different thread."
  (let ((locked-thread (process-thread process)))
    (or (null locked-thread)
        (eq locked-thread (current-thread)))))

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

    ;; Send request (binary data with length prefix, no newline)
    (process-send-string process request)

    ;; Wait for response with matching ID
    (with-current-buffer buffer
      (let ((timeout total-timeout)
            response)
        ;; Wait for a response with the correct ID
        (while (and (not response)
                    (> timeout 0)
                    (process-live-p process))
          ;; Check if process is locked to another thread before trying to accept
          (if (not (tramp-rpc--process-accessible-p process))
              (progn
                ;; Process locked - if non-essential, bail out; otherwise sleep and retry
                (if non-essential
                    (progn
                      (tramp-rpc--debug "LOCKED id=%s method=%s (non-essential, bailing)"
                                       expected-id method)
                      (throw 'non-essential 'non-essential))
                  ;; Sleep briefly - other thread may receive our response
                  (sleep-for poll-interval)
                  ;; Check if other thread already got our response
                  (setq response (tramp-rpc--find-response-by-id expected-id))))
            ;; Process is accessible - proceed with accept-process-output
            ;; Use same pattern as tramp-accept-process-output:
            ;; - poll-interval timeout to avoid spinning
            ;; - JUST-THIS-ONE=t to only accept from this process (Bug#12145)
            ;; - with-local-quit to allow C-g, returns t on success
            ;; - Propagate quit if user pressed C-g
            (if (with-local-quit
                  (accept-process-output process poll-interval nil t)
                  t)
                ;; Check if our response arrived in pending responses
                (setq response (tramp-rpc--find-response-by-id expected-id))
              ;; User quit - propagate it
              (tramp-rpc--debug "QUIT id=%s (user interrupted)" expected-id)
              (keyboard-quit)))
          (cl-decf timeout poll-interval))

        (unless response
          (tramp-rpc--debug "TIMEOUT id=%s method=%s buffer-size=%d"
                           expected-id method (buffer-size))
          (error "Timeout waiting for RPC response from %s (id=%s, method=%s)"
                 (tramp-file-name-host vec) expected-id method))

        (tramp-rpc--debug "RECV id=%s (found)" expected-id)
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
          (plist-get response :result))))))

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

    ;; Send batch request (binary data with length prefix, no newline)
    (process-send-string process request)

    ;; Wait for response with matching ID
    (with-current-buffer buffer
      (let ((timeout 30)
            response)
        (while (and (not response)
                    (> timeout 0)
                    (process-live-p process))
          ;; Check if process is locked to another thread before trying to accept
          (if (not (tramp-rpc--process-accessible-p process))
              ;; Process locked - if non-essential, bail out; otherwise sleep and retry
              (if non-essential
                  (progn
                    (tramp-rpc--debug "LOCKED-BATCH id=%s (non-essential, bailing)" expected-id)
                    (throw 'non-essential 'non-essential))
                ;; Sleep briefly - other thread may receive our response
                (sleep-for 0.1)
                ;; Check if other thread already got our response
                (setq response (tramp-rpc--find-response-by-id expected-id)))
            ;; Process is accessible
            (if (with-local-quit
                  (accept-process-output process 0.1 nil t)
                  t)
                ;; Check if our response arrived in pending responses
                (setq response (tramp-rpc--find-response-by-id expected-id))
              (tramp-rpc--debug "QUIT-BATCH id=%s (user interrupted)" expected-id)
              (keyboard-quit)))
          (cl-decf timeout 0.1))

        (unless response
          (tramp-rpc--debug "TIMEOUT-BATCH id=%s buffer-size=%d"
                           expected-id (buffer-size))
          (error "Timeout waiting for batch RPC response from %s (id=%s)"
                 (tramp-file-name-host vec) expected-id))

        (tramp-rpc--debug "RECV-BATCH id=%s (found)" expected-id)
        (if (tramp-rpc-protocol-error-p response)
            (progn
              (tramp-rpc--debug "ERROR-BATCH id=%s msg=%s"
                               expected-id (tramp-rpc-protocol-error-message response))
              (error "Batch RPC error: %s"
                     (tramp-rpc-protocol-error-message response)))
          (tramp-rpc-protocol-decode-batch-response response))))))

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
      (let* ((id-and-bytes (tramp-rpc-protocol-encode-request-with-id
                            (car req) (cdr req)))
             (id (car id-and-bytes))
             (bytes (cdr id-and-bytes)))
        (tramp-rpc--debug "SEND-PIPE id=%s method=%s" id (car req))
        (push id ids)
        ;; Send binary data with length prefix, no newline
        (process-send-string process bytes)))
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
        ;; Check if process is locked to another thread before trying to accept
        (if (not (tramp-rpc--process-accessible-p process))
            ;; Process locked - if non-essential, bail out; otherwise sleep and retry
            (if non-essential
                (progn
                  (tramp-rpc--debug "LOCKED-PIPE (non-essential, bailing)")
                  (throw 'non-essential 'non-essential))
              ;; Sleep briefly - other thread may receive our responses
              (sleep-for 0.1)
              ;; Check if other thread already got any of our responses
              (dolist (id remaining-ids)
                (let ((response (tramp-rpc--find-response-by-id id)))
                  (when response
                    (tramp-rpc--debug "RECV-PIPE found id=%s (after sleep)" id)
                    (puthash id response responses)
                    (setq remaining-ids (delete id remaining-ids))))))
          ;; Process is accessible
          (if (with-local-quit
                (accept-process-output process 0.1 nil t)
                t)
              ;; Check for each remaining ID in pending responses
              (dolist (id remaining-ids)
                (let ((response (tramp-rpc--find-response-by-id id)))
                  (when response
                    (tramp-rpc--debug "RECV-PIPE found id=%s" id)
                    ;; Store response by ID
                    (puthash id response responses)
                    ;; Remove from remaining
                    (setq remaining-ids (delete id remaining-ids)))))
            (tramp-rpc--debug "RECV-PIPE quit (user interrupted)")
            (keyboard-quit)))
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
;; Batched process-file support for magit and similar tools
;; ============================================================================

;;;###autoload
(defun tramp-rpc-run-git-commands (directory commands)
  "Run multiple git COMMANDS in DIRECTORY using pipelined RPC.
COMMANDS is a list of lists, where each sublist is arguments to git.
For example: ((\"rev-parse\" \"HEAD\") (\"status\" \"--porcelain\"))

Returns a list of plists, each containing:
  :exit-code - the exit code of the command
  :stdout    - standard output as a string
  :stderr    - standard error as a string

This is much faster than running each command sequentially over TRAMP
because all commands are sent in a single network round-trip."
  (with-parsed-tramp-file-name directory nil
    (let* ((requests
            (mapcar (lambda (args)
                      (cons "process.run"
                            `((cmd . "git")
                              (args . ,(vconcat args))
                              (cwd . ,localname))))
                    commands))
           (results (tramp-rpc--call-pipelined v requests)))
      ;; Convert results to a more convenient format
      (mapcar (lambda (result)
                (if (plist-get result :error)
                    (list :exit-code -1
                          :stdout ""
                          :stderr (or (plist-get result :message) "RPC error"))
                  (list :exit-code (alist-get 'exit_code result)
                        :stdout (tramp-rpc--decode-output
                                 (alist-get 'stdout result)
                                 (alist-get 'stdout_encoding result))
                        :stderr (tramp-rpc--decode-output
                                 (alist-get 'stderr result)
                                 (alist-get 'stderr_encoding result)))))
              results))))

;;;###autoload
(defun tramp-rpc-run-commands (directory commands)
  "Run multiple shell COMMANDS in DIRECTORY using pipelined RPC.
COMMANDS is a list of (PROGRAM . ARGS) cons cells.
For example: ((\"git\" \"rev-parse\" \"HEAD\") (\"ls\" \"-la\"))

Returns a list of plists, each containing:
  :exit-code - the exit code of the command
  :stdout    - standard output as a string
  :stderr    - standard error as a string

This is much faster than running each command sequentially over TRAMP
because all commands are sent in a single network round-trip."
  (with-parsed-tramp-file-name directory nil
    (let* ((requests
            (mapcar (lambda (cmd)
                      (let ((program (car cmd))
                            (args (cdr cmd)))
                        (cons "process.run"
                              `((cmd . ,program)
                                (args . ,(vconcat args))
                                (cwd . ,localname)))))
                    commands))
           (results (tramp-rpc--call-pipelined v requests)))
      ;; Convert results to a more convenient format
      (mapcar (lambda (result)
                (if (plist-get result :error)
                    (list :exit-code -1
                          :stdout ""
                          :stderr (or (plist-get result :message) "RPC error"))
                  (list :exit-code (alist-get 'exit_code result)
                        :stdout (tramp-rpc--decode-output
                                 (alist-get 'stdout result)
                                 (alist-get 'stdout_encoding result))
                        :stderr (tramp-rpc--decode-output
                                 (alist-get 'stderr result)
                                 (alist-get 'stderr_encoding result)))))
              results))))

;; ============================================================================
;; Output decoding helper
;; ============================================================================

(defun tramp-rpc--decode-string (data)
  "Decode DATA from raw bytes to multibyte UTF-8 string.
With MessagePack, strings come as raw bytes (unibyte string).
We decode them as UTF-8 to get proper multibyte strings.
Returns nil if DATA is nil, empty string if DATA is empty."
  (cond
   ((null data) nil)
   ((and (stringp data) (> (length data) 0))
    (decode-coding-string data 'utf-8-unix))
   (t data)))

(defun tramp-rpc--decode-output (data _encoding)
  "Decode DATA from raw bytes to multibyte UTF-8 string.
With MessagePack, data comes as raw bytes (unibyte string).
We decode it as UTF-8 to get a proper multibyte string.
ENCODING is ignored (kept for API compatibility)."
  (or (tramp-rpc--decode-string data) ""))

(defun tramp-rpc--decode-filename (entry)
  "Get filename from directory ENTRY.
With MessagePack, filenames come as raw bytes - decode to UTF-8."
  (tramp-rpc--decode-string (alist-get 'name entry)))

(defun tramp-rpc--path-to-bytes (path)
  "Convert PATH to a unibyte string for MessagePack transmission.
Handles both multibyte UTF-8 strings and unibyte byte strings."
  (if (multibyte-string-p path)
      (encode-coding-string path 'utf-8-unix)
    path))

(defun tramp-rpc--encode-path (path)
  "Encode PATH for transmission to the server.
With MessagePack, paths are sent directly as strings/binary.
Returns an alist with path."
  `((path . ,(tramp-rpc--path-to-bytes path))))

;; ============================================================================
;; File name handler operations
;; ============================================================================

(defun tramp-rpc-handle-file-exists-p (filename)
  "Like `file-exists-p' for TRAMP-RPC files.
Uses magit caches when available for better performance.
Also caches RPC results to avoid repeated calls for the same path."
  (let ((expanded (expand-file-name filename)))
    (catch 'result
      ;; Check if this is the prefetched directory itself
      (when (and tramp-rpc-magit--prefetch-directory
                 (string= expanded tramp-rpc-magit--prefetch-directory))
        (when tramp-rpc-magit--debug
          (message "file-exists-p HIT (prefetch-dir): %s -> t" filename))
        (throw 'result t))
      ;; Check state files cache (git internal files)
      (when tramp-rpc-magit--status-cache
        (let ((cached (tramp-rpc-magit--state-file-exists-p filename)))
          (unless (eq cached 'not-cached)
            (when tramp-rpc-magit--debug
              (message "file-exists-p HIT (state): %s -> %s" filename cached))
            (throw 'result cached))))
      ;; Check ancestors cache (marker files like .git, .projectile)
      (when tramp-rpc-magit--ancestors-cache
        (let ((cached (tramp-rpc-magit--file-exists-p filename)))
          (unless (eq cached 'not-cached)
            (when tramp-rpc-magit--debug
              (message "file-exists-p HIT (ancestors): %s -> %s" filename cached))
            (throw 'result cached))))
      ;; Check general file-exists cache (avoids repeated RPC calls)
      (let ((cached (tramp-rpc--cache-get tramp-rpc--file-exists-cache expanded)))
        (unless (eq cached 'not-found)
          (when tramp-rpc-magit--debug
            (message "file-exists-p HIT (rpc-cache): %s -> %s" filename cached))
          (throw 'result cached)))
      ;; Cache miss - make RPC call and cache the result
      (when tramp-rpc-magit--debug
        (message "file-exists-p MISS: %s" filename))
      (with-parsed-tramp-file-name filename nil
        (let ((result (tramp-rpc--call v "file.exists" (tramp-rpc--encode-path localname))))
          ;; Cache the result
          (tramp-rpc--cache-put tramp-rpc--file-exists-cache expanded result)
          result)))))

(defun tramp-rpc-handle-file-readable-p (filename)
  "Like `file-readable-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (tramp-rpc--call v "file.readable" (tramp-rpc--encode-path localname))))

(defun tramp-rpc-handle-file-writable-p (filename)
  "Like `file-writable-p' for TRAMP-RPC files.
Optimized to use pipelined requests for better performance."
  (with-parsed-tramp-file-name filename nil
    (let* ((parent (file-name-directory (directory-file-name localname)))
           (localname-encoded (tramp-rpc--encode-path localname))
           (parent-encoded (tramp-rpc--encode-path parent))
           ;; Send both requests in parallel
           (results (tramp-rpc--call-pipelined
                     v
                     `(("file.exists" . ,localname-encoded)
                       ("file.writable" . ,localname-encoded)
                       ("file.writable" . ,parent-encoded))))
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
    (tramp-rpc--call v "file.executable" (tramp-rpc--encode-path localname))))

(defun tramp-rpc--call-file-stat (vec localname &optional lstat)
  "Call file.stat for LOCALNAME on VEC, returning nil if file doesn't exist.
If LSTAT is non-nil, don't follow symlinks."
  (let* ((conn (tramp-rpc--ensure-connection vec))
         (process (plist-get conn :process))
         (buffer (plist-get conn :buffer))
         (params (append (tramp-rpc--encode-path localname)
                         (when lstat '((lstat . t)))))
         (id-and-request (tramp-rpc-protocol-encode-request-with-id "file.stat" params))
         (expected-id (car id-and-request))
         (request (cdr id-and-request)))
    (tramp-rpc--debug "SEND id=%s method=file.stat" expected-id)
    (process-send-string process request)
    (with-current-buffer buffer
      (let ((timeout 30)
            response)
        (while (and (not response)
                    (> timeout 0)
                    (process-live-p process))
          ;; Check if process is locked to another thread before trying to accept
          (if (not (tramp-rpc--process-accessible-p process))
              ;; Process locked - if non-essential, bail out; otherwise sleep and retry
              (if non-essential
                  (progn
                    (tramp-rpc--debug "LOCKED file.stat (non-essential, bailing)")
                    (throw 'non-essential 'non-essential))
                ;; Sleep briefly - other thread may receive our response
                (sleep-for 0.1)
                ;; Check if other thread already got our response
                (setq response (tramp-rpc--find-response-by-id expected-id)))
            ;; Process is accessible
            (if (with-local-quit
                  (accept-process-output process 0.1 nil t)
                  t)
                (setq response (tramp-rpc--find-response-by-id expected-id))
              (keyboard-quit)))
          (cl-decf timeout 0.1))
        (unless response
          (error "Timeout waiting for file.stat response"))
        (tramp-rpc--debug "RECV id=%s (found)" expected-id)
        (if (tramp-rpc-protocol-error-p response)
            (let ((code (tramp-rpc-protocol-error-code response)))
              ;; Return nil for file-not-found, signal for other errors
              (if (= code tramp-rpc-protocol-error-file-not-found)
                  nil
                (let ((msg (tramp-rpc-protocol-error-message response)))
                  (if (= code tramp-rpc-protocol-error-permission-denied)
                      (signal 'file-error (list "RPC" "Permission denied" msg))
                    (error "RPC error: %s" msg)))))
          (plist-get response :result))))))

(defun tramp-rpc-handle-file-directory-p (filename)
  "Like `file-directory-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (let ((result (tramp-rpc--call-file-stat v localname)))
      (and result (equal (alist-get 'type result) "directory")))))

(defun tramp-rpc-handle-file-regular-p (filename)
  "Like `file-regular-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (let ((result (tramp-rpc--call-file-stat v localname)))
      (and result (equal (alist-get 'type result) "file")))))

(defun tramp-rpc-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (let ((result (tramp-rpc--call-file-stat v localname t)))  ; lstat=t
      (when (and result (equal (alist-get 'type result) "symlink"))
        (tramp-rpc--decode-string (alist-get 'link_target result))))))

(defun tramp-rpc-handle-file-truename (filename)
  "Like `file-truename' for TRAMP-RPC files.
If the file doesn't exist, return FILENAME unchanged
\(like local `file-truename').
Results are cached to avoid repeated RPC calls."
  (let* ((expanded (expand-file-name filename))
         (cached (tramp-rpc--cache-get tramp-rpc--file-truename-cache expanded)))
    (if (not (eq cached 'not-found))
        ;; Cache hit
        (progn
          (when tramp-rpc-magit--debug
            (message "file-truename HIT: %s -> %s" filename cached))
          cached)
      ;; Cache miss - make RPC call
      (when tramp-rpc-magit--debug
        (message "file-truename MISS: %s" filename))
      (with-parsed-tramp-file-name filename nil
        (condition-case nil
            (let* ((result (tramp-rpc--call v "file.truename" (tramp-rpc--encode-path localname)))
                   ;; With MessagePack, path comes as raw bytes - decode to UTF-8
                   (path (tramp-rpc--decode-string
                          (if (stringp result)
                              result  ; Direct binary result
                            (alist-get 'path result))))  ; Or in result object
                   (truename (tramp-make-tramp-file-name v path)))
              ;; Cache the result
              (tramp-rpc--cache-put tramp-rpc--file-truename-cache expanded truename)
              truename)
          ;; If file doesn't exist, return the filename unchanged and cache it
          (file-missing
           (tramp-rpc--cache-put tramp-rpc--file-truename-cache expanded filename)
           filename))))))

(defun tramp-rpc-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for TRAMP-RPC files.
Also populates the file-exists cache as a side effect — if file.stat
succeeds, the file obviously exists."
  (with-parsed-tramp-file-name filename nil
    (let ((result (tramp-rpc--call-file-stat v localname t)))  ; lstat=t
      ;; Populate file-exists cache as a side effect
      (tramp-rpc--cache-put tramp-rpc--file-exists-cache
                            (expand-file-name filename)
                            (if result t nil))
      (when result
        (tramp-rpc--convert-file-attributes result id-format)))))

(defun tramp-rpc--convert-file-attributes (stat id-format)
  "Convert STAT result to Emacs file-attributes format.
ID-FORMAT specifies whether to use numeric or string IDs."
  (let* ((type-str (alist-get 'type stat))
         (type (pcase type-str
                 ("file" nil)
                 ("directory" t)
                 ("symlink" (tramp-rpc--decode-string (alist-get 'link_target stat)))
                 (_ nil)))
         (nlinks (alist-get 'nlinks stat))
         (uid (alist-get 'uid stat))
         (gid (alist-get 'gid stat))
         (uname (tramp-rpc--decode-string (alist-get 'uname stat)))
         (gname (tramp-rpc--decode-string (alist-get 'gname stat)))
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
    (tramp-rpc--call v "file.set_modes"
                     (append (tramp-rpc--encode-path localname)
                             `((mode . ,mode))))))

(defun tramp-rpc-handle-set-file-times (filename &optional timestamp _flag)
  "Like `set-file-times' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (let ((mtime (floor (float-time (or timestamp (current-time))))))
      (tramp-rpc--call v "file.set_times"
                       (append (tramp-rpc--encode-path localname)
                               `((mtime . ,mtime)))))))

(defun tramp-rpc-handle-file-newer-than-file-p (file1 file2)
  "Like `file-newer-than-file-p' for TRAMP-RPC files.
When both files are on the same remote host, uses the dedicated
`file.newer_than' RPC method for a single round-trip instead of
up to 4 separate calls (2x file-exists-p + 2x file-attributes)."
  (let ((remote1 (file-remote-p file1))
        (remote2 (file-remote-p file2)))
    (if (and remote1 remote2 (string= remote1 remote2))
        ;; Both files on same remote - use single RPC call
        (with-parsed-tramp-file-name file1 f1
          (with-parsed-tramp-file-name file2 f2
            (tramp-rpc--call f1 "file.newer_than"
                             `((file1 . ,f1-localname)
                               (file2 . ,f2-localname)))))
      ;; Mixed local/remote or different remotes - fall back to generic
      (cond
       ((not (file-exists-p file1)) nil)
       ((not (file-exists-p file2)) t)
       (t
        (let ((mtime1 (float-time (file-attribute-modification-time
                                   (file-attributes file1))))
              (mtime2 (float-time (file-attribute-modification-time
                                   (file-attributes file2)))))
          (> mtime1 mtime2)))))))

;; ============================================================================
;; Directory operations
;; ============================================================================

(defun tramp-rpc-handle-directory-files (directory &optional full match nosort count)
  "Like `directory-files' for TRAMP-RPC files."
  (with-parsed-tramp-file-name (expand-file-name directory) nil
    (let* ((result (tramp-rpc--call v "dir.list"
                                    (append (tramp-rpc--encode-path localname)
                                            '((include_attrs . :msgpack-false)
                                              (include_hidden . t)))))
           (files (mapcar #'tramp-rpc--decode-filename result)))
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
                                    (append (tramp-rpc--encode-path localname)
                                            '((include_attrs . t)
                                              (include_hidden . t)))))
           (entries (mapcar
                     (lambda (entry)
                       (let* ((name (tramp-rpc--decode-filename entry))
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
                                          (append (tramp-rpc--encode-path localname)
                                                  '((include_attrs . :msgpack-false)
                                                    (include_hidden . t)))))
                ;; Convert vector to list if needed
                (entries (if (vectorp result) (append result nil) result)))
           ;; Build list of names with trailing / for directories
           (mapcar (lambda (entry)
                     (let ((name (tramp-rpc--decode-filename entry))
                           (file-type (alist-get 'type entry)))
                       (if (equal file-type "directory")
                           (concat name "/")
                         name)))
                   entries)))))))

(defun tramp-rpc-handle-make-directory (dir &optional parents)
  "Like `make-directory' for TRAMP-RPC files."
  (with-parsed-tramp-file-name dir nil
    (tramp-rpc--call v "dir.create"
                     (append (tramp-rpc--encode-path localname)
                             `((parents . ,(if parents t :msgpack-false))))))
  ;; Invalidate caches for the new directory
  (tramp-rpc--invalidate-cache-for-path dir))

(defun tramp-rpc-handle-delete-directory (directory &optional recursive trash)
  "Like `delete-directory' for TRAMP-RPC files."
  (tramp-skeleton-delete-directory directory recursive trash
    (tramp-rpc--call v "dir.remove"
                     (append (tramp-rpc--encode-path localname)
                             `((recursive . ,(if recursive t :msgpack-false))))))
  ;; Invalidate caches for the deleted directory
  (tramp-rpc--invalidate-cache-for-path directory))

(defun tramp-rpc--parse-dired-switches (switches)
  "Parse SWITCHES string into a plist of options.
Supported switches:
  -a: show hidden files
  -t: sort by modification time
  -S: sort by size
  -r: reverse sort order
  -h: human-readable sizes
  --group-directories-first: directories before files"
  (let ((sw (or switches "")))
    (list
     :show-hidden (string-match-p "a" sw)
     :sort-by-time (string-match-p "t" sw)
     :sort-by-size (string-match-p "S" sw)
     :reverse (string-match-p "r" sw)
     :human-readable (string-match-p "h" sw)
     :group-directories-first (string-match-p "group-directories-first" sw))))

(defun tramp-rpc--format-size (size human-readable)
  "Format SIZE for display.
If HUMAN-READABLE is non-nil, use human-readable format (K, M, G).
Uses a width sufficient for large files (up to petabytes)."
  (if human-readable
      (cond
       ((>= size 1073741824) (format "%5.1fG" (/ size 1073741824.0)))
       ((>= size 1048576) (format "%5.1fM" (/ size 1048576.0)))
       ((>= size 1024) (format "%5.1fK" (/ size 1024.0)))
       (t (format "%6d" size)))
    (format "%13d" size)))

(defun tramp-rpc--format-time (mtime)
  "Format MTIME (Unix timestamp) for ls-like display.
Uses the standard ls -l convention:
- Files modified within the last 6 months: \"Mon DD HH:MM\"
- Files modified more than 6 months ago: \"Mon DD  YYYY\"
Returns a fixed-width 12-character string."
  (if (null mtime)
      "Jan  1  1970"
    (let* ((time (seconds-to-time mtime))
           (now (current-time))
           ;; 6 months in seconds (approximately 182.5 days)
           (six-months-ago (time-subtract now (days-to-time 182))))
      (if (time-less-p time six-months-ago)
          ;; Old file: show year instead of time
          (format-time-string "%b %e  %Y" time)
        ;; Recent file: show time
        (format-time-string "%b %e %H:%M" time)))))

(defun tramp-rpc-handle-insert-directory
    (filename switches &optional _wildcard _full-directory-p)
  "Like `insert-directory' for TRAMP-RPC files.
Produces ls-like output for dired.
Supported switches: -a -t -S -r -h --group-directories-first."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (let* ((opts (tramp-rpc--parse-dired-switches switches))
           (result (tramp-rpc--call v "dir.list"
                                    (append (tramp-rpc--encode-path localname)
                                            `((include_attrs . t)
                                              (include_hidden . ,(if (plist-get opts :show-hidden) t :msgpack-false))))))
           ;; Convert vector to list if needed
           (result-list (if (vectorp result) (append result nil) result))
           ;; Pre-decode names and extract sort keys
           (entries-with-data
            (mapcar (lambda (entry)
                      (let* ((name (tramp-rpc--decode-filename entry))
                             (attrs (alist-get 'attrs entry))
                             (type (alist-get 'type attrs))
                             (size (or (alist-get 'size attrs) 0))
                             (mtime (or (alist-get 'mtime attrs) 0))
                             (is-dir (equal type "directory")))
                        (list :name name :entry entry :size size
                              :mtime mtime :is-dir is-dir)))
                    result-list))
           ;; Sort entries based on switches
           (sorted-entries
            (let* ((sort-fn
                    (cond
                     ((plist-get opts :sort-by-time)
                      (lambda (a b) (> (plist-get a :mtime) (plist-get b :mtime))))
                     ((plist-get opts :sort-by-size)
                      (lambda (a b) (> (plist-get a :size) (plist-get b :size))))
                     (t
                      (lambda (a b) (string< (plist-get a :name) (plist-get b :name))))))
                   ;; Apply --group-directories-first
                   (sorted (if (plist-get opts :group-directories-first)
                               (append
                                (sort (cl-remove-if-not (lambda (e) (plist-get e :is-dir))
                                                        entries-with-data)
                                      sort-fn)
                                (sort (cl-remove-if (lambda (e) (plist-get e :is-dir))
                                                    entries-with-data)
                                      sort-fn))
                             (sort entries-with-data sort-fn))))
              ;; Apply reverse if requested
              (if (plist-get opts :reverse)
                  (nreverse sorted)
                sorted))))
      ;; Insert header
      (insert (format "  %s:\n" filename))
      (insert "  total 0\n")  ; We don't calculate total blocks

      ;; Insert each entry
      (dolist (entry-data sorted-entries)
        (let* ((name (plist-get entry-data :name))
               (entry (plist-get entry-data :entry))
               (attrs (alist-get 'attrs entry))
               (type (alist-get 'type attrs))
               (mode (alist-get 'mode attrs))
               (nlinks (or (alist-get 'nlinks attrs) 1))
               (uid (or (alist-get 'uid attrs) 0))
               (gid (or (alist-get 'gid attrs) 0))
               (uname (or (tramp-rpc--decode-string (alist-get 'uname attrs))
                          (number-to-string uid)))
               (gname (or (tramp-rpc--decode-string (alist-get 'gname attrs))
                          (number-to-string gid)))
               (size (plist-get entry-data :size))
               (mtime (alist-get 'mtime attrs))
               (link-target (tramp-rpc--decode-string (alist-get 'link_target attrs)))
               (mode-str (tramp-rpc--mode-to-string (or mode 0) (or type "file")))
               (size-str (tramp-rpc--format-size size (plist-get opts :human-readable)))
               (time-str (tramp-rpc--format-time mtime)))
          ;; Skip . and .. unless -a is given
          (unless (and (member name '("." ".."))
                       (not (plist-get opts :show-hidden)))
            (insert (format "  %s %3d %-8s %-8s %s %s %s"
                            mode-str nlinks uname gname size-str time-str name))
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
    (let* ((params (tramp-rpc--encode-path localname))
           (_ (when beg (push `(offset . ,beg) params)))
           (_ (when end (push `(length . ,(- end (or beg 0))) params)))
           (result (tramp-rpc--call v "file.read" params))
           ;; With MessagePack, content is already raw bytes (unibyte string)
           (content (alist-get 'content result))
           (size (length content))
           (point-before (point)))

      (when replace
        (delete-region (point-min) (point-max))
        (setq point-before (point-min)))

      ;; Insert raw bytes and let Emacs detect/decode the encoding
      ;; This matches what standard TRAMP does for proper encoding handling
      (let ((coding-system-for-read 'undecided))
        (insert content)
        ;; Decode the inserted region using Emacs' coding system detection
        ;; This properly handles UTF-8, Chinese, and other encodings
        (decode-coding-inserted-region point-before (point) filename visit beg end replace))

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
           ;; With MessagePack, send content as binary directly
           (content-bytes (encode-coding-string content 'utf-8-unix))
           (params (append (tramp-rpc--encode-path localname)
                           `((content . ,(msgpack-bin-make content-bytes))
                             (append . ,(if append t :msgpack-false))))))

      ;; Check mustbenew
      (when mustbenew
        (when (file-exists-p filename)
          (if (eq mustbenew 'excl)
              (signal 'file-already-exists (list filename))
            (unless (yes-or-no-p
                     (format "File %s exists; overwrite? " filename))
              (signal 'file-already-exists (list filename))))))

      (tramp-rpc--call v "file.write" params)

      ;; Invalidate caches for the written file
      (tramp-rpc--invalidate-cache-for-path filename)

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
                             (preserve . ,(if (or keep-time preserve-permissions) t :msgpack-false)))))))
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
             preserve-uid-gid preserve-permissions))))
    ;; Invalidate caches for the destination file (if remote)
    (when dest-remote
      (tramp-rpc--invalidate-cache-for-path newname))))

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
                             (overwrite . ,(if ok-if-already-exists t :msgpack-false)))))))
     ;; Different hosts, copy then delete
     (t
      (copy-file filename newname ok-if-already-exists t t t)
      (delete-file filename)))
    ;; Invalidate caches for both source (no longer exists) and destination
    (when source-remote
      (tramp-rpc--invalidate-cache-for-path filename))
    (when dest-remote
      (tramp-rpc--invalidate-cache-for-path newname))))

(defun tramp-rpc-handle-delete-file (filename &optional trash)
  "Like `delete-file' for TRAMP-RPC files."
  (tramp-skeleton-delete-file filename trash
    (tramp-rpc--call v "file.delete" (tramp-rpc--encode-path localname)))
  ;; Invalidate caches for the deleted file
  (tramp-rpc--invalidate-cache-for-path filename))

(defun tramp-rpc-handle-make-symbolic-link (target linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for TRAMP-RPC files."
  (with-parsed-tramp-file-name linkname nil
    (unless ok-if-already-exists
      (when (file-exists-p linkname)
        (signal 'file-already-exists (list linkname))))
    (when (file-exists-p linkname)
      (delete-file linkname))
    (let* ((target-path (if (tramp-tramp-file-p target)
                            (tramp-file-local-name target)
                          target))
           (link-path-params (tramp-rpc--encode-path localname))
           ;; Rename 'path' to 'link_path' in the encoded params
           (params (mapcar (lambda (p)
                             (if (eq (car p) 'path)
                                 (cons 'link_path (cdr p))
                               (if (eq (car p) 'path_encoding)
                                   (cons 'link_path_encoding (cdr p))
                                 p)))
                           link-path-params)))
      (tramp-rpc--call v "file.make_symlink"
                       (append `((target . ,target-path)) params))))
  ;; Invalidate caches for the new symlink
  (tramp-rpc--invalidate-cache-for-path linkname))

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
                       (append (tramp-rpc--encode-path localname)
                               `((uid . ,uid)
                                 (gid . ,gid)))))))

(defun tramp-rpc-handle-file-system-info (filename)
  "Like `file-system-info' for TRAMP-RPC files.
Returns a list of (TOTAL FREE AVAILABLE) bytes for the filesystem
containing FILENAME."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (condition-case nil
        (let ((result (tramp-rpc--call v "system.statvfs" (tramp-rpc--encode-path localname))))
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
                    (or (tramp-rpc--decode-string (alist-get 'name g))
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
      ;; stdin must be binary for MessagePack
      (let* ((acl-bytes (encode-coding-string acl-string 'utf-8-unix))
             (result (tramp-rpc--call v "process.run"
                                      `((cmd . "setfacl")
                                        (args . ["--set-file=-" ,localname])
                                        (cwd . "/")
                                        (stdin . ,(msgpack-bin-make acl-bytes))))))
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
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally infile)
                              (buffer-string))))
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

      ;; Handle destination (side effects only, we return exit-code below)
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
      ;; Always return exit code
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
    (let* ((result (tramp-rpc--call v "file.read" (tramp-rpc--encode-path localname)))
           ;; With MessagePack, content is already raw bytes
           (content (alist-get 'content result))
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
        ;; Current user - use cached system.info
        (let ((result (tramp-rpc--get-system-info vec)))
          (tramp-rpc--decode-string (alist-get 'home result)))
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
  "Return remote UID using cached system.info."
  (let* ((result (tramp-rpc--get-system-info vec))
         (uid (alist-get 'uid result)))
    (when uid
      (if (eq id-format 'integer)
          uid
        (number-to-string uid)))))

(defun tramp-rpc-handle-get-remote-gid (vec id-format)
  "Return remote GID using cached system.info."
  (let* ((result (tramp-rpc--get-system-info vec))
         (gid (alist-get 'gid result)))
    (when gid
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
Uses `command -v` for efficient lookup via login shell.
Uses a unique marker to separate MOTD/banner text from actual output,
following the pattern used by standard TRAMP."
  (condition-case nil
      (let* (;; Use a unique marker (MD5 hash) to delimit output from MOTD text
             ;; This is the same approach used by tramp-sh.el
             (marker (md5 (format "tramp-rpc-%s-%s" program (float-time))))
             (result (tramp-rpc--call vec "process.run"
                                       `((cmd . "/bin/sh")
                                         (args . ["-l" "-c"
                                                  ,(format "echo %s; command -v %s"
                                                           marker program)])
                                         (cwd . "/"))))
             (exit-code (alist-get 'exit_code result))
             (stdout (tramp-rpc--decode-output
                      (alist-get 'stdout result)
                      (alist-get 'stdout_encoding result))))
        (when (and (eq exit-code 0) (> (length stdout) 0))
          ;; Find the marker and extract the path after it
          (when (string-match (concat (regexp-quote marker) "\n\\([^\n]+\\)") stdout)
            (let ((path (string-trim (match-string 1 stdout))))
              (when (string-prefix-p "/" path)
                path)))))
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
     (t (funcall orig-fun process)))))

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
For TRAMP-RPC PTY processes, return the remote TTY name stored during creation.
For direct SSH PTY processes, use the original function (returns local PTY)."
  (if (and (processp process)
           (process-get process :tramp-rpc-pty)
           (not (process-get process :tramp-rpc-direct-ssh)))  ; Not direct SSH
      (process-get process :tramp-rpc-tty-name)
    (funcall orig-fun process stream)))

;; Install advice
(advice-add 'process-send-string :around #'tramp-rpc--process-send-string-advice)
(advice-add 'process-send-region :around #'tramp-rpc--process-send-region-advice)
(advice-add 'process-send-eof :around #'tramp-rpc--process-send-eof-advice)
(advice-add 'signal-process :around #'tramp-rpc--signal-process-advice)
(advice-add 'process-status :around #'tramp-rpc--process-status-advice)
(advice-add 'process-exit-status :around #'tramp-rpc--process-exit-status-advice)
(advice-add 'process-command :around #'tramp-rpc--process-command-advice)
(advice-add 'process-tty-name :around #'tramp-rpc--process-tty-name-advice)

;; Advice vterm's window resize function for tramp-rpc PTY support
(with-eval-after-load 'vterm
  (advice-add 'vterm--window-adjust-process-window-size :around
              #'tramp-rpc--vterm-window-size-advice))

;; Advice eat's window resize function for tramp-rpc PTY support
(with-eval-after-load 'eat
  (advice-add 'eat--adjust-process-window-size :around
              #'tramp-rpc--eat-window-size-advice))

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

;;;###autoload
(defun tramp-rpc-file-name-handler (operation &rest args)
  "Invoke TRAMP-RPC file name handler for OPERATION with ARGS."
  (if-let* ((handler (assq operation tramp-rpc-file-name-handler-alist)))
      (save-match-data (apply (cdr handler) args))
    (tramp-run-real-handler operation args)))

;; ============================================================================
;; Method predicate and handler registration
;; ============================================================================

;; `tramp-rpc-file-name-p' is defined as defsubst in the with-eval-after-load
;; block above (extracted into autoloads).  Re-define it here as defun for
;; the full-load case so it gets proper byte-compilation.
(defun tramp-rpc-file-name-p (vec-or-filename)
  "Check if VEC-OR-FILENAME is handled by TRAMP-RPC.
VEC-OR-FILENAME can be either a tramp-file-name struct or a filename string."
  (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename)))
    (string= (tramp-file-name-method vec) tramp-rpc-method)))

;; Re-register with the full defun now that the file is loaded.
;; (Already registered via with-eval-after-load, but this ensures the
;; byte-compiled defun version is used.)
(tramp-register-foreign-file-name-handler
 #'tramp-rpc-file-name-p #'tramp-rpc-file-name-handler)

;; ============================================================================
;; Batched file operations for performance optimization
;; ============================================================================

(defun tramp-rpc-batch-file-exists (directory paths)
  "Check existence of multiple PATHS in DIRECTORY in a single RPC call.
PATHS should be local paths (without the remote prefix).
Returns an alist of (path . exists-p)."
  (with-parsed-tramp-file-name directory nil
    (let* ((requests
            (mapcar (lambda (path)
                      (cons "file.exists" `((path . ,path))))
                    paths))
           (results (tramp-rpc--call-pipelined v requests)))
      (cl-loop for path in paths
               for result in results
               collect (cons path (if (plist-get result :error) nil result))))))

(defun tramp-rpc-batch-file-attributes (directory paths &optional id-format)
  "Get file attributes for multiple PATHS in DIRECTORY in a single RPC call.
PATHS should be local paths (without the remote prefix).
ID-FORMAT specifies whether to use numeric or string IDs (default: integer).
Returns an alist of (path . attributes) in file-attributes format."
  (with-parsed-tramp-file-name directory nil
    (let* ((requests
            (mapcar (lambda (path)
                      (cons "file.stat" `((path . ,path))))
                    paths))
           (results (tramp-rpc--call-pipelined v requests)))
      (cl-loop for path in paths
               for result in results
               collect (cons path
                             (if (or (plist-get result :error)
                                     (not result))
                                 nil
                               (tramp-rpc--convert-file-attributes
                                result (or id-format 'integer))))))))

;; ============================================================================
;; Magit integration - server-side optimized status
;; ============================================================================

;; This uses the server-side magit.status RPC which returns all data needed
;; for magit-status in a single call, eliminating ~60 individual git commands.

(defvar tramp-rpc-magit--status-cache nil
  "Cached magit status data from server-side RPC.
Populated by `tramp-rpc-magit--prefetch', used by process-file advice.")

(defvar tramp-rpc-magit--ancestors-cache nil
  "Cached ancestor scan data from server-side RPC.
This is populated by `tramp-rpc-magit--prefetch' for file existence checks.")

(defvar tramp-rpc-magit--prefetch-directory nil
  "The directory that was prefetched.
Used to answer file-exists-p queries for the directory itself.")

(defvar tramp-rpc-magit--debug nil
  "When non-nil, log cache hits/misses for debugging.")

(defvar tramp-rpc--file-exists-cache (make-hash-table :test 'equal)
  "Cache for file-exists-p results.
Maps absolute file paths to (timestamp . result) pairs.
This persists across calls to avoid repeated RPC calls for the same path.")

(defvar tramp-rpc--file-truename-cache (make-hash-table :test 'equal)
  "Cache for file-truename results.
Maps absolute file paths to (timestamp . truename) pairs.
This persists across calls to avoid repeated RPC calls for the same path.")

(defvar tramp-rpc--cache-ttl 300
  "Time-to-live in seconds for file-exists-p and file-truename caches.
Default is 300 seconds (5 minutes).  Set to nil to disable TTL.")

(defvar tramp-rpc--cache-max-size 10000
  "Maximum number of entries in each cache.
When exceeded, the cache is cleared.  Set to nil for no limit.")

(defun tramp-rpc--cache-get (cache key)
  "Get value from CACHE for KEY, respecting TTL.
Returns the value if found and not expired, otherwise `not-found'."
  (let ((entry (gethash key cache 'not-found)))
    (if (eq entry 'not-found)
        'not-found
      ;; Entry is (timestamp . value)
      (let ((timestamp (car entry))
            (value (cdr entry)))
        (if (and tramp-rpc--cache-ttl
                 (> (- (float-time) timestamp) tramp-rpc--cache-ttl))
            ;; Expired - remove and return not-found
            (progn
              (remhash key cache)
              'not-found)
          value)))))

(defun tramp-rpc--cache-put (cache key value)
  "Put VALUE into CACHE for KEY with current timestamp.
Clears cache if it exceeds `tramp-rpc--cache-max-size'."
  ;; Check size limit
  (when (and tramp-rpc--cache-max-size
             (>= (hash-table-count cache) tramp-rpc--cache-max-size))
    (clrhash cache))
  ;; Store (timestamp . value)
  (puthash key (cons (float-time) value) cache))

;;;###autoload
(defun tramp-rpc-magit-status (directory)
  "Get complete magit status for DIRECTORY using server-side RPC.
Returns an alist with all data needed for magit-status:
  toplevel, gitdir, head, upstream, push, state, staged, unstaged,
  untracked, stashes, recent_commits, tags, remotes, config, state_files.

This is much faster than magit's default behavior because the server
runs all git commands locally and returns the results in one response."
  (when (and (file-remote-p directory)
             (tramp-rpc-file-name-p directory))
    (with-parsed-tramp-file-name directory nil
      (let ((result (tramp-rpc--call v "magit.status"
                                     `((directory . ,localname)))))
        (when result
          ;; Decode string fields
          (dolist (key '(toplevel gitdir state))
            (when-let* ((val (alist-get key result)))
              (when (stringp val)
                (setf (alist-get key result) (decode-coding-string val 'utf-8)))))
          ;; Decode nested head info
          (when-let* ((head (alist-get 'head result)))
            (dolist (key '(hash short branch message))
              (when-let* ((val (alist-get key head)))
                (when (stringp val)
                  (setf (alist-get key head) (decode-coding-string val 'utf-8))))))
          ;; Decode diff content
          (dolist (section '(staged unstaged))
            (when-let* ((data (alist-get section result)))
              (when-let* ((diff (alist-get 'diff data)))
                (when (stringp diff)
                  (setf (alist-get 'diff data) (decode-coding-string diff 'utf-8))))))
          result)))))

;;;###autoload
(defun tramp-rpc-ancestors-scan (directory markers &optional max-depth)
  "Scan ancestor directories of DIRECTORY for MARKERS using server-side RPC.
MARKERS is a list of file/directory names to look for (e.g., \".git\" \".svn\").
MAX-DEPTH limits how far up the tree to search (default 10).

Returns an alist of (marker . found-directory) where found-directory is
the closest ancestor containing that marker, or nil if not found.

This is much faster than checking each ancestor individually because
the server scans the entire tree in one operation."
  (when (and (file-remote-p directory)
             (tramp-rpc-file-name-p directory))
    (with-parsed-tramp-file-name directory nil
      (let ((result (tramp-rpc--call v "ancestors.scan"
                                     `((directory . ,localname)
                                       (markers . ,(vconcat markers))
                                       (max_depth . ,(or max-depth 10))))))
        ;; Convert result to alist with string keys and decoded paths
        ;; Keys come back as symbols from msgpack (e.g., .git instead of ".git")
        ;; so we convert them to strings for easier lookup
        (mapcar (lambda (pair)
                  (let ((key (car pair))
                        (val (cdr pair)))
                    (cons (if (symbolp key) (symbol-name key) key)
                          (when val
                            (decode-coding-string val 'utf-8)))))
                result)))))

(defun tramp-rpc-magit--prefetch (directory)
  "Prefetch magit status and ancestor data for DIRECTORY.
Populates `tramp-rpc-magit--status-cache' and ancestors cache."
  (when (and (file-remote-p directory)
             (tramp-rpc-file-name-p directory))
    ;; Remember the directory we prefetched for
    (setq tramp-rpc-magit--prefetch-directory (expand-file-name directory))
    ;; Fetch magit status
    (setq tramp-rpc-magit--status-cache (tramp-rpc-magit-status directory))
    ;; Fetch ancestor markers for project/VC detection
    (setq tramp-rpc-magit--ancestors-cache
          (tramp-rpc-ancestors-scan directory
                                    '(".git" ".svn" ".hg" ".bzr" "_darcs"
                                      ".projectile" ".project" ".dir-locals.el"
                                      ".editorconfig")))
    (when tramp-rpc-magit--debug
      (message "tramp-rpc-magit: prefetched status and ancestors for %s" directory))))

(defun tramp-rpc-magit--clear-status-cache ()
  "Clear only the status cache (git state that changes frequently)."
  (setq tramp-rpc-magit--status-cache nil))

(defun tramp-rpc-magit--clear-cache ()
  "Clear all magit-related caches."
  (setq tramp-rpc-magit--status-cache nil)
  (setq tramp-rpc-magit--ancestors-cache nil)
  (setq tramp-rpc-magit--prefetch-directory nil))

(defun tramp-rpc-clear-file-exists-cache ()
  "Clear the file-exists-p result cache.
Call this after making changes to the remote filesystem."
  (interactive)
  (clrhash tramp-rpc--file-exists-cache)
  (when (called-interactively-p 'any)
    (message "tramp-rpc file-exists cache cleared")))

(defun tramp-rpc-clear-file-truename-cache ()
  "Clear the file-truename result cache.
Call this after making changes to symlinks on the remote filesystem."
  (interactive)
  (clrhash tramp-rpc--file-truename-cache)
  (when (called-interactively-p 'any)
    (message "tramp-rpc file-truename cache cleared")))

(defun tramp-rpc--invalidate-cache-for-path (filename)
  "Invalidate caches for FILENAME and its parent directory.
Called after file modifications to ensure cache consistency."
  (when filename
    (let ((expanded (expand-file-name filename)))
      ;; Remove the file itself from caches
      (remhash expanded tramp-rpc--file-exists-cache)
      (remhash expanded tramp-rpc--file-truename-cache)
      ;; Also invalidate parent directory (for directory listings)
      (let ((parent (file-name-directory (directory-file-name expanded))))
        (when parent
          (remhash parent tramp-rpc--file-exists-cache)
          (remhash parent tramp-rpc--file-truename-cache))))))

(defun tramp-rpc-clear-all-caches ()
  "Clear all tramp-rpc caches.
Call this after making significant changes to the remote filesystem."
  (interactive)
  (tramp-rpc-magit--clear-cache)
  (tramp-rpc-clear-file-exists-cache)
  (tramp-rpc-clear-file-truename-cache)
  (when (called-interactively-p 'any)
    (message "All tramp-rpc caches cleared")))

(defun tramp-rpc-magit--file-exists-p (filename)
  "Check if FILENAME exists using cached ancestor data.
Returns t, nil, or \='not-cached if not in cache.
Only uses the cache if FILENAME is under the prefetched directory."
  (if (and tramp-rpc-magit--ancestors-cache
           tramp-rpc-magit--prefetch-directory)
      ;; First check if this file is under the prefetched directory
      (let* ((expanded (expand-file-name filename))
             (prefetch-local (tramp-file-local-name tramp-rpc-magit--prefetch-directory))
             (file-local (tramp-file-local-name expanded)))
        (if (string-prefix-p prefetch-local file-local)
            ;; File is under the prefetched directory - cache applies
            (let ((basename (file-name-nondirectory filename)))
              ;; Check if this is one of the markers we scanned for
              (if-let* ((entry (assoc basename tramp-rpc-magit--ancestors-cache)))
                  (if (cdr entry)
                      ;; Marker was found - check if at right location
                      (let ((found-dir (cdr entry))
                            (file-dir (file-name-directory file-local)))
                        (if (string-prefix-p found-dir file-dir)
                            t
                          nil))
                    nil)
                'not-cached))
          ;; File is outside the prefetched directory - cache doesn't apply
          'not-cached))
    'not-cached))

(defun tramp-rpc-magit--state-file-exists-p (filename)
  "Check if git state FILENAME exists using cached status data.
Returns t, nil, or \='not-cached if not in cache.
Only uses the cache if FILENAME is under the prefetched directory."
  (if (and tramp-rpc-magit--status-cache
           tramp-rpc-magit--prefetch-directory
           (alist-get 'state_files tramp-rpc-magit--status-cache))
      ;; Check if file is under the prefetched directory
      (let* ((expanded (expand-file-name filename))
             (prefetch-local (tramp-file-local-name tramp-rpc-magit--prefetch-directory))
             (file-local (tramp-file-local-name expanded)))
        (if (string-prefix-p prefetch-local file-local)
            ;; File is under the prefetched directory - cache applies
            (let* ((state-files (alist-get 'state_files tramp-rpc-magit--status-cache))
                   ;; Extract the path relative to .git/
                   (basename (if (string-match "/\\.git/\\(.+\\)$" file-local)
                                 (match-string 1 file-local)
                               (file-name-nondirectory filename))))
              (if-let* ((entry (assoc (intern basename) state-files)))
                  (if (cdr entry) t nil)
                'not-cached))
          ;; File is outside the prefetched directory - cache doesn't apply
          'not-cached))
    'not-cached))

(defun tramp-rpc-magit--advice-setup-buffer (orig-fun directory &rest args)
  "Advice around `magit-status-setup-buffer' to prefetch data."
  (when (and (file-remote-p directory)
             (tramp-rpc-file-name-p directory))
    (tramp-rpc-magit--prefetch directory))
  (unwind-protect
      (apply orig-fun directory args)
    ;; Only clear status cache - ancestors/prefetch-dir stay for other packages
    (tramp-rpc-magit--clear-status-cache)))

(defun tramp-rpc-magit--advice-refresh-buffer (orig-fun &rest args)
  "Advice around `magit-status-refresh-buffer' to prefetch data."
  (when (and (file-remote-p default-directory)
             (tramp-rpc-file-name-p default-directory)
             (null tramp-rpc-magit--status-cache))
    (tramp-rpc-magit--prefetch default-directory))
  (unwind-protect
      (apply orig-fun args)
    ;; Only clear status cache - ancestors/prefetch-dir stay for other packages
    (tramp-rpc-magit--clear-status-cache)))

;;;###autoload
(defun tramp-rpc-magit-enable ()
  "Enable tramp-rpc magit optimizations.
This uses server-side RPCs to dramatically speed up magit-status
on remote repositories by fetching all data in a single call."
  (interactive)
  (advice-add 'magit-status-setup-buffer :around
              #'tramp-rpc-magit--advice-setup-buffer)
  (advice-add 'magit-status-refresh-buffer :around
              #'tramp-rpc-magit--advice-refresh-buffer)
  ;; Note: file-exists-p caching is handled in tramp-rpc-handle-file-exists-p
  ;; via the TRAMP handler mechanism, no global advice needed
  (message "tramp-rpc magit optimizations enabled"))

;;;###autoload
(defun tramp-rpc-magit-disable ()
  "Disable tramp-rpc magit optimizations."
  (interactive)
  (advice-remove 'magit-status-setup-buffer
                 #'tramp-rpc-magit--advice-setup-buffer)
  (advice-remove 'magit-status-refresh-buffer
                 #'tramp-rpc-magit--advice-refresh-buffer)
  (tramp-rpc-magit--clear-cache)
  (message "tramp-rpc magit optimizations disabled"))

;;;###autoload
(defun tramp-rpc-magit-enable-debug ()
  "Enable debug logging for tramp-rpc magit."
  (interactive)
  (setq tramp-rpc-magit--debug t)
  (message "tramp-rpc magit debug enabled"))

;;;###autoload
(defun tramp-rpc-magit-disable-debug ()
  "Disable debug logging for tramp-rpc magit."
  (interactive)
  (setq tramp-rpc-magit--debug nil)
  (message "tramp-rpc magit debug disabled"))

;; Auto-enable when magit is loaded
(with-eval-after-load 'magit
  (tramp-rpc-magit-enable))

;; ============================================================================
;; Projectile optimizations
;; ============================================================================

(defun tramp-rpc-projectile--advice-get-ext-command (orig-fun vcs)
  "Advice to disable fd for remote directories.
Projectile checks if fd is available using `executable-find' which
checks the LOCAL machine, but fd may not be available on the REMOTE.
This forces git ls-files for remote directories."
  (if (and (file-remote-p default-directory)
           (tramp-rpc-file-name-p default-directory)
           (eq vcs 'git)
           (boundp 'projectile-git-command))
      ;; For remote RPC directories, always use git ls-files
      projectile-git-command
    ;; Otherwise, use the original function
    (funcall orig-fun vcs)))

(defun tramp-rpc-projectile--advice-dir-files (orig-fun directory)
  "Advice to use alien indexing for remote directories.
Projectile's hybrid indexing calls `file-relative-name' for each file
which is slow over TRAMP.  For remote directories, we use alien indexing
directly since git ls-files already returns relative paths."
  (if (and (file-remote-p directory)
           (tramp-rpc-file-name-p directory))
      ;; For remote RPC directories, use alien indexing directly
      (projectile-dir-files-alien directory)
    ;; Otherwise, use the original function
    (funcall orig-fun directory)))

(defun tramp-rpc-projectile--advice-project-files (orig-fun project-root)
  "Advice to use alien indexing for remote project files.
This bypasses the expensive `file-relative-name' calls in hybrid mode."
  (if (and (file-remote-p project-root)
           (tramp-rpc-file-name-p project-root))
      ;; For remote RPC directories, use alien indexing directly
      ;; This skips the cl-mapcan + file-relative-name overhead
      (let ((files nil))
        ;; Check cache first (like projectile-project-files does)
        ;; Guard against projectile not being loaded
        (when (and (bound-and-true-p projectile-enable-caching)
                   (boundp 'projectile-projects-cache))
          (setq files (gethash project-root projectile-projects-cache)))
        ;; If not cached, fetch and cache
        (unless files
          (setq files (projectile-dir-files-alien project-root))
          (when (and (bound-and-true-p projectile-enable-caching)
                     (boundp 'projectile-projects-cache)
                     (boundp 'projectile-projects-cache-time)
                     (fboundp 'projectile-time-seconds))
            (puthash project-root files projectile-projects-cache)
            (puthash project-root (projectile-time-seconds) projectile-projects-cache-time)))
        files)
    ;; Otherwise, use the original function
    (funcall orig-fun project-root)))

;;;###autoload
(defun tramp-rpc-projectile-enable ()
  "Enable tramp-rpc projectile optimizations.
This ensures fd is not used for remote directories where it may not
be available, and uses alien indexing for better performance."
  (interactive)
  (advice-add 'projectile-get-ext-command :around
              #'tramp-rpc-projectile--advice-get-ext-command)
  (advice-add 'projectile-dir-files :around
              #'tramp-rpc-projectile--advice-dir-files)
  (advice-add 'projectile-project-files :around
              #'tramp-rpc-projectile--advice-project-files)
  (message "tramp-rpc projectile optimizations enabled"))

;;;###autoload
(defun tramp-rpc-projectile-disable ()
  "Disable tramp-rpc projectile optimizations."
  (interactive)
  (advice-remove 'projectile-get-ext-command
                 #'tramp-rpc-projectile--advice-get-ext-command)
  (advice-remove 'projectile-dir-files
                 #'tramp-rpc-projectile--advice-dir-files)
  (advice-remove 'projectile-project-files
                 #'tramp-rpc-projectile--advice-project-files)
  (message "tramp-rpc projectile optimizations disabled"))

;; Auto-enable when projectile is loaded
(with-eval-after-load 'projectile
  (tramp-rpc-projectile-enable))

;; Unload support
;; ============================================================================

(defun tramp-rpc-unload-function ()
  "Unload function for tramp-rpc.
Removes advice and cleans up async processes."
  ;; Remove advice
  (advice-remove 'process-send-string #'tramp-rpc--process-send-string-advice)
  (advice-remove 'process-send-region #'tramp-rpc--process-send-region-advice)
  (advice-remove 'process-send-eof #'tramp-rpc--process-send-eof-advice)
  (advice-remove 'signal-process #'tramp-rpc--signal-process-advice)
  (advice-remove 'process-status #'tramp-rpc--process-status-advice)
  (advice-remove 'process-exit-status #'tramp-rpc--process-exit-status-advice)
  (advice-remove 'process-command #'tramp-rpc--process-command-advice)
  (advice-remove 'vc-call-backend #'tramp-rpc--vc-call-backend-advice)
  ;; Remove magit advice
  (advice-remove 'magit-status-setup-buffer #'tramp-rpc-magit--advice-setup-buffer)
  (advice-remove 'magit-status-refresh-buffer #'tramp-rpc-magit--advice-refresh-buffer)
  ;; Remove projectile advice
  (advice-remove 'projectile-get-ext-command #'tramp-rpc-projectile--advice-get-ext-command)
  (advice-remove 'projectile-dir-files #'tramp-rpc-projectile--advice-dir-files)
  (advice-remove 'projectile-project-files #'tramp-rpc-projectile--advice-project-files)
  ;; Clean up all async processes
  (tramp-rpc--cleanup-async-processes)
  ;; Clean up PTY processes
  (maphash (lambda (proc info)
             (when-let* ((timer (plist-get info :timer)))
               (cancel-timer timer))
             (when (process-live-p proc)
               (delete-process proc)))
           tramp-rpc--pty-processes)
  (clrhash tramp-rpc--pty-processes)
  ;; Return nil to allow normal unload to proceed
  nil)

(provide 'tramp-rpc)
;;; tramp-rpc.el ends here
