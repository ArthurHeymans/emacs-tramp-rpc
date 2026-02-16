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
  (add-to-list 'tramp-methods
               `(,tramp-rpc-method
                 ;; Direct async process support: tramp-rpc uses direct SSH
                 ;; PTY connections for async processes, which means stderr
                 ;; is mixed with stdout (normal PTY behavior).  Setting
                 ;; tramp-direct-async lets upstream tests know to skip
                 ;; stderr-separation assertions for async shell-command.
                 (tramp-direct-async t)))

  ;; Enable direct-async-process for the rpc method.
  ;; This tells upstream tramp that our async processes are "direct"
  ;; (i.e., they use a direct SSH PTY connection rather than piping
  ;; through the control channel).  As a consequence, stderr cannot
  ;; be separated from stdout in async processes.
  (connection-local-set-profile-variables
   'tramp-rpc-connection-local-default-profile
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   `(:application tramp :protocol ,tramp-rpc-method)
   'tramp-rpc-connection-local-default-profile)

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
               '(tramp-rpc-file-name-p . tramp-rpc-file-name-handler))

  ;; Configure user and host name completion.
  (tramp-set-completion-function "rpc" tramp-completion-function-alist-ssh)

  ;; Allow the "rpc" method in multi-hop filename syntax.
  ;; TRAMP's `tramp-multi-hop-p' only returns t for tramp-sh methods,
  ;; which would cause `tramp-dissect-file-name' to reject filenames like
  ;; /rpc:hop|rpc:target:/path.  We advise it to also accept "rpc".
  (defun tramp-rpc--multi-hop-advice (orig-fun vec)
    "Allow the rpc method in multi-hop chains."
    (or (string= (tramp-file-name-method vec) tramp-rpc-method)
        (funcall orig-fun vec)))
  (advice-add 'tramp-multi-hop-p :around #'tramp-rpc--multi-hop-advice))

;; Now the actual implementation
(require 'cl-lib)
(require 'tramp)
(require 'tramp-sh)
(require 'tramp-rpc-protocol)

;; Silence byte-compiler warning for function defined in with-eval-after-load
(declare-function tramp-rpc--multi-hop-advice "tramp-rpc")
(require 'tramp-rpc-deploy)

;; Silence byte-compiler warnings for functions defined elsewhere
;; (vterm variables are declared in tramp-rpc-process.el)

;; Forward declarations for cache/watch functions (tramp-rpc-magit.el)
(defvar tramp-rpc--file-exists-cache)
(defvar tramp-rpc--file-truename-cache)
(defvar tramp-rpc--suppress-fs-notifications)
(defvar tramp-rpc--watched-directories)
(declare-function tramp-rpc--cache-get "tramp-rpc-magit")
(declare-function tramp-rpc--cache-put "tramp-rpc-magit")
(declare-function tramp-rpc--invalidate-cache-for-path "tramp-rpc-magit")
(declare-function tramp-rpc--directory-watched-p "tramp-rpc-magit")
(declare-function tramp-rpc--handle-notification "tramp-rpc-magit")
(declare-function tramp-rpc-clear-file-exists-cache "tramp-rpc-magit")
(declare-function tramp-rpc-clear-file-truename-cache "tramp-rpc-magit")
(declare-function tramp-rpc--cleanup-watches-for-connection "tramp-rpc-magit")
(declare-function tramp-rpc-magit--process-cache-lookup "tramp-rpc-magit")
(declare-function tramp-rpc-magit--file-exists-p "tramp-rpc-magit")
(declare-function tramp-rpc-magit--clear-cache "tramp-rpc-magit")
(defvar tramp-rpc-magit--debug)
(defvar tramp-rpc-magit--process-caches)

(defvar tramp-rpc--readonly-programs
  '("git" "ls" "cat" "find" "grep" "rg" "test" "stat" "head" "tail"
    "wc" "sort" "uniq" "diff" "comm" "file" "readlink" "realpath"
    "which" "whereis" "id" "whoami" "hostname" "uname" "env" "printenv"
    "date" "du" "df" "free" "uptime" "ps" "top" "awk" "sed" "tr"
    "cut" "paste" "join" "tee" "xargs" "basename" "dirname" "sha256sum"
    "md5sum" "true" "false" "echo" "printf")
  "Programs known to not modify the filesystem.
Used to skip cache invalidation in `tramp-rpc-handle-process-file'.")

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

(defcustom tramp-rpc-use-direct-ssh-pty t
  "Whether to use direct SSH connections for PTY processes.
When non-nil, interactive terminal processes (vterm, shell-mode, term-mode)
use a direct SSH connection with `-t` for the PTY, providing much lower
latency than the RPC-based PTY.  The SSH connection reuses the existing
ControlMaster socket, so authentication is already handled.

Note: `signal-process' on direct SSH PTY sends signal to the local SSH
process, which may not propagate to the remote process in all cases."
  :type 'boolean
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

;; ============================================================================
;; Connection management
;; ============================================================================

(defvar tramp-rpc--connections (make-hash-table :test 'equal)
  "Hash table mapping connection keys to RPC process info.
Key is (host user port hop), value is a plist with :process and :buffer.")

;; tramp-rpc--async-processes and tramp-rpc--pty-processes are defined in
;; tramp-rpc-process.el (loaded via require below)

(defvar tramp-rpc--async-callbacks (make-hash-table :test 'eql)
  "Hash table mapping request IDs to callback functions for async RPC calls.")

(defvar tramp-rpc--pending-responses (make-hash-table :test 'eq)
  "Hash table mapping buffers to their pending response hash tables.
Each buffer has its own hash table mapping request IDs to response plists.")

(defun tramp-rpc--get-pending-responses (buffer)
  "Get the pending responses hash table for BUFFER, creating if needed."
  (or (gethash buffer tramp-rpc--pending-responses)
      (puthash buffer (make-hash-table :test 'eql) tramp-rpc--pending-responses)))

;; tramp-rpc--process-write-queues is defined in tramp-rpc-process.el

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

(defun tramp-rpc--clear-executable-cache (&optional vec)
  "Clear the executable cache.
If VEC is provided, only clear entries for that connection.
Otherwise clear all entries."
  (if vec
      (let ((conn-key (tramp-rpc--connection-key vec))
            (keys-to-remove nil))
        ;; Collect keys first (can't modify hash table during maphash)
        (maphash (lambda (key _value)
                   (when (equal (car key) conn-key)
                     (push key keys-to-remove)))
                 tramp-rpc--executable-cache)
        ;; Now remove them
        (dolist (key keys-to-remove)
          (remhash key tramp-rpc--executable-cache)))
    (clrhash tramp-rpc--executable-cache)))

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
                                                           marker (shell-quote-argument program))])
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

(defun tramp-rpc--connection-key (vec)
  "Generate a connection key for VEC.
Includes the hop chain so that different multi-hop routes to the
same host produce distinct connections."
  (list (tramp-file-name-host vec)
        (tramp-file-name-user vec)
        (or (tramp-file-name-port vec) 22)
        (tramp-file-name-hop vec)))

(defun tramp-rpc--get-connection (vec)
  "Get the RPC connection for VEC, or nil if not connected."
  (gethash (tramp-rpc--connection-key vec) tramp-rpc--connections))

(defun tramp-rpc--set-connection (vec process buffer)
  "Store the RPC connection for VEC."
  (puthash (tramp-rpc--connection-key vec)
           (list :process process :buffer buffer)
           tramp-rpc--connections))

(defun tramp-rpc--remove-connection (vec)
  "Remove the RPC connection for VEC.
Also clears the executable cache for this connection."
  (remhash (tramp-rpc--connection-key vec) tramp-rpc--connections)
  (tramp-rpc--clear-executable-cache vec))

(defun tramp-rpc--ensure-connection (vec)
  "Ensure we have an active RPC connection to VEC.
Returns the connection plist."
  (let ((conn (tramp-rpc--get-connection vec)))
    (if (and conn
             (process-live-p (plist-get conn :process))
             (buffer-live-p (plist-get conn :buffer)))
        conn
      ;; Stale connection - remove it before reconnecting
      (when conn
        (tramp-rpc--remove-connection vec))
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

;;; ============================================================================
;;; Multi-hop support
;;; ============================================================================

(defun tramp-rpc--hops-to-proxyjump (vec)
  "Convert VEC's hop chain to an SSH ProxyJump (-J) string.
Parses the TRAMP hop field (e.g. \"rpc:user@gateway|\") and converts
each hop to the SSH ProxyJump format (e.g. \"user@gateway\").
Returns nil if there are no hops.

Supports mixed methods: both \"rpc:\" and \"ssh:\" hops are accepted
since ProxyJump only needs host connectivity."
  (when-let* ((hops (tramp-file-name-hop vec)))
    (mapconcat
     (lambda (hop-str)
       (let* ((hop-name (concat tramp-prefix-format hop-str
                                tramp-postfix-host-format))
              (hop-vec (tramp-dissect-file-name hop-name 'nodefault)))
         (concat
          (when (tramp-file-name-user hop-vec)
            (concat (tramp-file-name-user hop-vec) "@"))
          (tramp-file-name-host hop-vec)
          (when-let* ((port (tramp-file-name-port hop-vec)))
            (concat ":" (if (numberp port) (number-to-string port) port))))))
     (split-string hops tramp-postfix-hop-regexp 'omit)
     ",")))

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
    ;; Include the hop chain so different multi-hop routes get different sockets
    (setq path (replace-regexp-in-string
                "%C"
                (md5 (format "%s%s%s%s%s" (system-name) host port user
                             (or (tramp-file-name-hop vec) "")))
                path t t))
    (expand-file-name path)))

(defun tramp-rpc--controlmaster-active-p (vec)
  "Return non-nil if a ControlMaster connection is active for VEC."
  (let ((socket-path (tramp-rpc--controlmaster-socket-path vec))
        (host (tramp-file-name-host vec))
        (user (tramp-file-name-user vec))
        (port (tramp-file-name-port vec))
        (proxyjump (tramp-rpc--hops-to-proxyjump vec)))
    (and (file-exists-p socket-path)
         ;; Check if the socket is actually usable via ssh -O check
         (zerop (apply #'call-process "ssh" nil nil nil
                       (append
                        (when user (list "-l" user))
                        (when port (list "-p" (number-to-string port)))
                        (when proxyjump (list "-J" proxyjump))
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
         (proxyjump (tramp-rpc--hops-to-proxyjump vec))
         (socket-path (tramp-rpc--controlmaster-socket-path vec))
         (process-name (format "*tramp-rpc-auth %s*" host))
         (buffer (get-buffer-create (format " *tramp-rpc-auth %s*" host)))
         (ssh-args (append
                    (list "ssh")
                    tramp-rpc-ssh-args
                    (when user (list "-l" user))
                    (when port (list "-p" (number-to-string port)))
                    ;; Multi-hop via ProxyJump
                    (when proxyjump (list "-J" proxyjump))
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
        (signal
	 'remote-file-error
	 (list "Failed to establish SSH connection to %s: %s" host output))))))

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
         (proxyjump (tramp-rpc--hops-to-proxyjump vec))
         ;; Build SSH command to run the RPC server
         (ssh-args (append
                    (list "ssh")
                    ;; Raw SSH arguments (e.g., -v, -F config)
                    tramp-rpc-ssh-args
                    (when user (list "-l" user))
                    (when port (list "-p" (number-to-string port)))
                    ;; Multi-hop via ProxyJump
                    (when proxyjump (list "-J" proxyjump))
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

    ;; Store vec on the process so notifications can identify the connection
    (process-put process :tramp-rpc-vec vec)

    ;; Wait for server to be ready by sending a ping
    (let ((response (tramp-rpc--call vec "system.info" nil)))
      (unless response
        (tramp-rpc--remove-connection vec)
        (signal 'remote-file-error (list "Failed to connect to RPC server on %s" host))))

    ;; Mark as connected for TRAMP's connectivity checks (used by projectile, etc.)
    (tramp-set-connection-property process "connected" t)

    (tramp-rpc--get-connection vec)))

(defun tramp-rpc--disconnect (vec)
  "Disconnect the RPC connection to VEC."
  ;; First, clean up any async and PTY processes for this connection
  (tramp-rpc--cleanup-async-processes vec)
  (tramp-rpc--cleanup-pty-processes vec)
  ;; Clean up watched directory entries for this connection
  (tramp-rpc--cleanup-watches-for-connection vec)
  (let ((conn (tramp-rpc--get-connection vec)))
    (when conn
      (let ((process (plist-get conn :process)))
        (when (process-live-p process)
          (delete-process process)))
      (tramp-rpc--remove-connection vec)))
  ;; Flush TRAMP caches so a reconnect gets fresh data (home dir, uid, etc.)
  (tramp-flush-directory-properties vec "/")
  (tramp-flush-connection-properties vec))

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
                ;; Check for server-initiated notification (no id, has method)
                (if (plist-get response :notification)
                    (tramp-rpc--handle-notification
                     process
                     (plist-get response :method)
                     (plist-get response :params))
                  ;; Handle normal response
                  (let* ((id (plist-get response :id))
                         (callback (gethash id tramp-rpc--async-callbacks)))
                    (if callback
                        (progn
                          (tramp-rpc--debug "FILTER dispatching async id=%s" id)
                          (remhash id tramp-rpc--async-callbacks)
                          (funcall callback response))
                      ;; Not an async response - store for sync code
                      (tramp-rpc--debug "FILTER storing sync response id=%s" id)
                      (puthash id response (tramp-rpc--get-pending-responses buffer)))))))))))))

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
            ;; - with-tramp-suspended-timers to prevent deferred process
            ;;   sentinels (scheduled via run-at-time 0) from firing
            ;;   inside accept-process-output and blocking this call.
            ;;   The sentinels will run when control returns to the
            ;;   command loop.  (Mirrors tramp-accept-process-output.)
            (if (with-tramp-suspended-timers
                  (with-local-quit
                    (accept-process-output process poll-interval nil t)
                    t))
                ;; Check if our response arrived in pending responses
                (setq response (tramp-rpc--find-response-by-id expected-id))
              ;; User quit - propagate it
              (tramp-rpc--debug "QUIT id=%s (user interrupted)" expected-id)
              (keyboard-quit)))
          (cl-decf timeout poll-interval))

        (unless response
          (tramp-rpc--debug "TIMEOUT id=%s method=%s buffer-size=%d"
                           expected-id method (buffer-size))
          (signal
	   'remote-file-error
	   (list "Timeout waiting for RPC response from %s (id=%s, method=%s)"
                 (tramp-file-name-host vec) expected-id method)))

        (tramp-rpc--debug "RECV id=%s (found)" expected-id)
        (if (tramp-rpc-protocol-error-p response)
            (let ((code (tramp-rpc-protocol-error-code response))
                  (msg (tramp-rpc-protocol-error-message response))
                  (os-errno (tramp-rpc-protocol-error-errno response)))
              (tramp-rpc--debug "ERROR id=%s code=%s msg=%s errno=%s"
                               expected-id code msg os-errno)
              (cond
               ((= code tramp-rpc-protocol-error-file-not-found)
                (signal 'file-missing (list "RPC" "No such file" msg)))
               ((= code tramp-rpc-protocol-error-permission-denied)
                (signal 'permission-denied (list "RPC" "Permission denied" msg)))
               ;; Map OS errno values to appropriate Emacs error symbols.
               ;; The server includes the raw errno in the error data field.
               ((eql os-errno 17) ;; EEXIST
                (signal 'file-already-exists (list "RPC" msg)))
               ((eql os-errno 39) ;; ENOTEMPTY
                (signal 'file-error (list "RPC" "Directory not empty" msg)))
               ((eql os-errno 20) ;; ENOTDIR
                (signal 'file-error (list "RPC" "Not a directory" msg)))
               ((eql os-errno 21) ;; EISDIR
                (signal 'file-error (list "RPC" "Is a directory" msg)))
               ((eql os-errno 40) ;; ELOOP
                (signal 'file-error (list "RPC" "Too many levels of symbolic links" msg)))
               ;; All other IO errors also signal file-error so callers
               ;; can catch them uniformly with condition-case.
               ((= code tramp-rpc-protocol-error-io)
                (signal 'remote-file-error (list "RPC" msg)))
               (t
                (signal 'remote-file-error (list "RPC error: %s" msg)))))
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
          (signal
	   'remote-file-error
	   (list "Timeout waiting for batch RPC response from %s (id=%s)"
		 (tramp-file-name-host vec) expected-id)))

        (tramp-rpc--debug "RECV-BATCH id=%s (found)" expected-id)
        (if (tramp-rpc-protocol-error-p response)
            (progn
              (tramp-rpc--debug "ERROR-BATCH id=%s msg=%s"
                               expected-id (tramp-rpc-protocol-error-message response))
              (signal
	       'remote-file-error
	       (list "Batch RPC error: %s"
                     (tramp-rpc-protocol-error-message response))))
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
Handles both multibyte UTF-8 strings and unibyte byte strings.
Strips Emacs file-name quoting (the /: prefix) before sending to
the server, since the remote side does not understand it."
  (let ((unquoted (file-name-unquote path)))
    (if (multibyte-string-p unquoted)
        (encode-coding-string unquoted 'utf-8-unix)
      unquoted)))

(defun tramp-rpc--encode-path (path)
  "Encode PATH for transmission to the server.
With MessagePack, paths are sent directly as strings/binary.
Strips any Emacs file-name quoting (/:) before encoding.
Returns an alist with path."
  `((path . ,(tramp-rpc--path-to-bytes path))))

;; ============================================================================
;; File name handler operations
;; ============================================================================

(defun tramp-rpc-handle-file-executable-p (filename)
  "Like `file-executable-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (tramp-rpc--call v "file.executable" (tramp-rpc--encode-path localname))))

(defun tramp-rpc--call-file-stat (vec localname &optional lstat)
  "Call file.stat for LOCALNAME on VEC, returning nil if file doesn't exist.
If LSTAT is non-nil, don't follow symlinks.
Uses `tramp-rpc--call' internally but converts file-missing and
ELOOP errors to nil (the file effectively doesn't exist for stat)."
  (let ((params (append (tramp-rpc--encode-path localname)
                        (when lstat '((lstat . t))))))
    (condition-case err
        (tramp-rpc--call vec "file.stat" params)
      (file-missing nil)
      (file-error
       ;; Return nil for ELOOP (symlink loop) - file can't be resolved,
       ;; so it effectively doesn't exist for stat purposes.
       (if (string-match-p "Too many levels of symbolic links" (cadr err))
           nil
         (signal (car err) (cdr err)))))))



(defun tramp-rpc-handle-file-truename (filename)
  "Like `file-truename' for TRAMP-RPC files.
Resolves symlinks in the path.  For non-existing files, returns the
path unchanged (after resolving any symlinks in parent directories)."
  ;; Use tramp-skeleton-file-truename which handles:
  ;; - Caching via with-tramp-file-property
  ;; - Proper filename expansion and unquoting
  ;; - Preserving trailing "/" and requoting
  ;; The BODY must return a localname, which the skeleton wraps with
  ;; tramp-make-tramp-file-name.
  (tramp-skeleton-file-truename filename
    ;; Try RPC first for existing files (fast path)
    (condition-case nil
        (let* ((result (tramp-rpc--call v "file.truename"
                                        (tramp-rpc--encode-path localname)))
               ;; With MessagePack, path comes as raw bytes - decode to UTF-8
               (path (tramp-rpc--decode-string
                      (if (stringp result)
                          result
                        (alist-get 'path result)))))
          (or path localname))
      ;; If file doesn't exist or has a symlink loop, fall back to
      ;; symlink-chasing approach (same as tramp-handle-file-truename).
      ;; ELOOP (symlink loop) maps to file-error, not file-missing.
      (file-error
       (let ((result (directory-file-name localname))
             (numchase 0)
             (numchase-limit 20)
             symlink-target)
         (while (and (setq symlink-target
                           (file-symlink-p (tramp-make-tramp-file-name v result)))
                     (< numchase numchase-limit))
           (setq numchase (1+ numchase)
                 result
                 (if (tramp-tramp-file-p symlink-target)
                     (file-name-quote symlink-target 'top)
                   (tramp-drop-volume-letter
                    (expand-file-name
                     symlink-target (file-name-directory result)))))
           (when (>= numchase numchase-limit)
             (tramp-error
              v 'file-error
              "Maximum number (%d) of symlinks exceeded" numchase-limit)))
         (directory-file-name result))))))

(defun tramp-rpc-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property
        v localname (format "file-attributes-%s" id-format)
      (let ((result (tramp-rpc--call-file-stat v localname t)))  ; lstat=t
        ;; Populate file-exists cache as side effect
        (let ((expanded (expand-file-name filename)))
          (tramp-rpc--cache-put tramp-rpc--file-exists-cache
                                expanded (if result t nil)))
        (when result
          (tramp-rpc--convert-file-attributes result id-format))))))

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



;; ============================================================================
;; Directory operations
;; ============================================================================

(defun tramp-rpc-handle-directory-files (directory &optional full match nosort count)
  "Like `directory-files' for TRAMP-RPC files."
  (tramp-skeleton-directory-files directory full match nosort count
    (let* ((result (tramp-rpc--call v "dir.list"
                                    (append (tramp-rpc--encode-path localname)
                                            '((include_attrs . :msgpack-false)
                                              (include_hidden . t)))))
           (files (mapcar #'tramp-rpc--decode-filename result)))
      ;; Ensure "." and ".." are present (some storage systems omit them)
      (unless (member "." files) (push "." files))
      (unless (member ".." files) (push ".." files))
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
  (tramp-skeleton-make-directory dir parents
    (tramp-rpc--call v "dir.create"
                     (append (tramp-rpc--encode-path localname)
                             ;; Don't pass parents here - the skeleton
                             ;; handles the recursive parent creation.
                             `((parents . :msgpack-false)
                               (mode . ,(default-file-modes)))))
    ;; Flush parent directory properties so file-exists-p sees the new dir.
    (tramp-flush-directory-properties v (file-name-directory localname))
    (tramp-rpc--invalidate-cache-for-path dir)))

(defun tramp-rpc-handle-delete-directory (directory &optional recursive trash)
  "Like `delete-directory' for TRAMP-RPC files."
  (tramp-skeleton-delete-directory directory recursive trash
    (tramp-rpc--call v "dir.remove"
                     (append (tramp-rpc--encode-path localname)
                             `((recursive . ,(if recursive t :msgpack-false))))))
  (tramp-rpc--invalidate-cache-for-path directory))

;; ============================================================================
;; File I/O operations
;; ============================================================================



(defun tramp-rpc-handle-write-region
    (start end filename &optional append visit lockname mustbenew)
  "Like `write-region' for TRAMP-RPC files."
  (tramp-skeleton-write-region
      start end filename append visit lockname mustbenew
    ;; If START is a string, write it directly; otherwise extract from buffer.
    ;; When APPEND is an integer, it is a file offset for writing.
    (let* ((content (if (stringp start)
                        start
                      (buffer-substring-no-properties
                       (or start (point-min))
                       (or end (point-max)))))
           ;; Encode using buffer's coding system or default to utf-8
           (coding (or (and (not (stringp start))
                            buffer-file-coding-system)
                       'utf-8-unix))
           (content-bytes (encode-coding-string content coding))
           ;; When APPEND is an integer, it's a file offset.
           ;; Read the existing file content first, then splice.
           (real-append (cond
                         ((integerp append)
                          ;; Offset write: read file, truncate at offset, append new
                          (let* ((existing (condition-case nil
                                              (let ((r (tramp-rpc--call
                                                        v "file.read"
                                                        (tramp-rpc--encode-path localname))))
                                                (alist-get 'content r))
                                            (file-missing nil)))
                                 (prefix (if existing
                                             (substring existing 0 (min append (length existing)))
                                           "")))
                            ;; Combine prefix + new content
                            (setq content-bytes (concat prefix content-bytes))
                            ;; Not an append anymore, full overwrite
                            nil))
                         (append t)
                         (t nil)))
           (params (append (tramp-rpc--encode-path localname)
                           `((content . ,(msgpack-bin-make content-bytes))
                             (append . ,(if real-append t :msgpack-false))))))

      (let ((tramp-rpc--suppress-fs-notifications t))
        (tramp-rpc--call v "file.write" params))

      ;; Invalidate caches for the written file
      (tramp-rpc--invalidate-cache-for-path filename)

      ;; Tell the skeleton which coding system we used.
      ;; `encode-coding-string' sets `last-coding-system-used', but
      ;; the skeleton shadows it with a local `let', so use the value
      ;; from our `coding' variable instead.
      (setq coding-system-used coding))))

(defun tramp-rpc-handle-copy-file
    (filename newname &optional ok-if-already-exists keep-time
              preserve-uid-gid preserve-permissions)
  "Like `copy-file' for TRAMP-RPC files."
  (setq filename (expand-file-name filename)
        newname (expand-file-name newname))
  ;; When NEWNAME is a directory name (trailing /), copy INTO it.
  (when (and (directory-name-p newname)
             (file-directory-p newname))
    (setq newname (expand-file-name
                   (file-name-nondirectory filename) newname)))
  ;; Common checks before dispatching by host combination.
  (unless ok-if-already-exists
    (when (file-exists-p newname)
      (signal 'file-already-exists (list newname))))
  (when (and (file-directory-p newname)
             (not (directory-name-p newname)))
    (signal 'file-error (list "File is a directory" newname)))
  (let ((source-remote (tramp-tramp-file-p filename))
        (dest-remote (tramp-tramp-file-p newname)))
    (cond
     ;; Directory source: delegate to copy-directory.
     ((file-directory-p filename)
      (copy-directory filename newname keep-time t))

     ;; Symlink source: recreate the symlink at the destination rather
     ;; than copying the target file contents (matches upstream tramp).
     ((file-symlink-p filename)
      (make-symbolic-link
       (file-symlink-p filename) newname ok-if-already-exists))

     ;; Both on same remote host using RPC - use server-side copy
     ((and source-remote dest-remote
           (tramp-equal-remote filename newname))
      (with-parsed-tramp-file-name filename v1
        (with-parsed-tramp-file-name newname v2
          (tramp-rpc--call v1 "file.copy"
                           `((src . ,(tramp-rpc--path-to-bytes
                                      (file-name-unquote v1-localname)))
                             (dest . ,(tramp-rpc--path-to-bytes
                                       (file-name-unquote v2-localname)))
                             (preserve . ,(if (or keep-time preserve-permissions) t :msgpack-false)))))))
     ;; Remote source, local dest - read via RPC, write locally
     ((and source-remote (not dest-remote))
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
      ;; Read local file and write to remote
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally filename)
        (write-region (point-min) (point-max) newname nil 'nomessage))
      (when (or keep-time preserve-permissions)
        (set-file-times newname (file-attribute-modification-time
                                 (file-attributes filename)))))
     ;; Both remote, different hosts - copy via local Emacs buffer.
     ;; This is the universal fallback matching upstream tramp's
     ;; `tramp-do-copy-or-rename-file-via-buffer': read source via its
     ;; handler, write destination via its handler.
     ((and source-remote dest-remote)
      (abort-if-file-too-large
       (file-attribute-size (file-attributes (file-truename filename)))
       "copy" filename)
      (let ((coding-system-for-read 'binary)
            (coding-system-for-write 'binary)
            (jka-compr-inhibit t))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally filename)
          (write-region (point-min) (point-max) newname nil 'nomessage)))
      (when (or keep-time preserve-permissions)
        (set-file-times newname (file-attribute-modification-time
                                 (file-attributes filename)))))
     ;; Neither remote - should not reach this handler, but be safe.
     (t
      (tramp-run-real-handler
       #'copy-file
       (list filename newname ok-if-already-exists keep-time
             preserve-uid-gid preserve-permissions))))
    ;; Flush tramp file property cache for source and destination
    (when source-remote
      (with-parsed-tramp-file-name filename v1
        (tramp-flush-file-properties v1 v1-localname)))
    (when dest-remote
      (with-parsed-tramp-file-name newname v2
        (tramp-flush-file-properties v2 v2-localname)
        (tramp-flush-directory-properties v2 v2-localname))
      (tramp-rpc--invalidate-cache-for-path newname))))

(defun tramp-rpc-handle-rename-file (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for TRAMP-RPC files."
  (setq filename (expand-file-name filename)
        newname (expand-file-name newname))
  ;; Check ok-if-already-exists BEFORE any directory rewriting.
  (when (file-exists-p newname)
    (unless ok-if-already-exists
      (signal 'file-already-exists (list newname)))
    ;; Even with ok-if-already-exists, can't rename a file onto a directory.
    (when (and (file-directory-p newname)
               (not (directory-name-p newname))
               (not (file-directory-p filename)))
      (signal 'file-error (list "File is a directory" newname))))
  ;; If newname is a directory (with trailing slash), rename INTO it.
  (when (and (file-directory-p newname)
             (directory-name-p newname))
    (setq newname (expand-file-name (file-name-nondirectory filename) newname)))
  (let ((source-remote (tramp-tramp-file-p filename))
        (dest-remote (tramp-tramp-file-p newname)))
    (cond
     ;; Both on same remote host using RPC
     ((and source-remote dest-remote
           (tramp-equal-remote filename newname))
      (with-parsed-tramp-file-name filename v1
        (with-parsed-tramp-file-name newname v2
          (tramp-rpc--call v1 "file.rename"
                           `((src . ,(tramp-rpc--path-to-bytes
                                      (file-name-unquote v1-localname)))
                             (dest . ,(tramp-rpc--path-to-bytes
                                       (file-name-unquote v2-localname)))
                             (overwrite . ,(if ok-if-already-exists t :msgpack-false)))))))
     ;; Different hosts, copy then delete
     (t
      (copy-file filename newname ok-if-already-exists t t t)
      (if (file-directory-p filename)
          (delete-directory filename 'recursive)
        (delete-file filename))))
    ;; Flush tramp file property cache for source and destination
    (when source-remote
      (with-parsed-tramp-file-name filename v1
        (tramp-flush-file-properties v1 v1-localname))
      (tramp-rpc--invalidate-cache-for-path filename))
    (when dest-remote
      (with-parsed-tramp-file-name newname v2
        (tramp-flush-file-properties v2 v2-localname)
        (tramp-flush-directory-properties v2 v2-localname))
      (tramp-rpc--invalidate-cache-for-path newname))))

(defun tramp-rpc-handle-delete-file (filename &optional trash)
  "Like `delete-file' for TRAMP-RPC files."
  (tramp-skeleton-delete-file filename trash
    (tramp-rpc--call v "file.delete" (tramp-rpc--encode-path localname)))
  (tramp-rpc--invalidate-cache-for-path filename))

(defun tramp-rpc-handle-make-symbolic-link (target linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for TRAMP-RPC files."
  (tramp-skeleton-make-symbolic-link target linkname ok-if-already-exists
    (let* ((target-path (file-name-unquote target))
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
                       (append `((target . ,(tramp-rpc--path-to-bytes target-path))) params)))
    (tramp-rpc--invalidate-cache-for-path linkname)))

(defun tramp-rpc-handle-add-name-to-file (filename newname &optional ok-if-already-exists)
  "Like `add-name-to-file' for TRAMP-RPC files.
Creates a hard link from NEWNAME to FILENAME."
  ;; When newname is a directory-name (trailing /), create the link inside it.
  (when (and (directory-name-p newname)
             (file-directory-p newname))
    (setq newname (expand-file-name (file-name-nondirectory filename) newname)))
  (unless (tramp-equal-remote filename newname)
    (with-parsed-tramp-file-name
        (if (tramp-tramp-file-p filename) filename newname) nil
      (tramp-error
       v 'remote-file-error
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
      (tramp-flush-file-properties v2 v2-localname)
      (tramp-rpc--call v1 "file.make_hardlink"
                       `((src . ,(tramp-rpc--path-to-bytes
                                  (file-name-unquote v1-localname)))
                         (dest . ,(tramp-rpc--path-to-bytes
                                   (file-name-unquote v2-localname))))))))

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
    (setq localname (file-name-unquote localname))
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

(defun tramp-rpc--route-process-file-output (destination stdout &optional stderr)
  "Route process-file STDOUT and STDERR according to DESTINATION.
DESTINATION follows the `process-file' convention:
  nil       - discard
  t         - insert into current buffer
  string    - write to file
  buffer    - insert into buffer
  (stdout-dest . stderr-dest) - cons for separate handling"
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
      (when (and stderr-dest stderr)
        (cond
         ((stringp stderr-dest)
          (with-temp-file stderr-dest (insert stderr)))
         ((bufferp stderr-dest)
          (with-current-buffer stderr-dest (insert stderr)))))))))

(defun tramp-rpc--get-signal-strings (vec)
  "Strings to return by `process-file' in case of signals on VEC.
Runs `kill -l' on the remote host to get signal names, then maps
signal numbers to human-readable strings like \"Interrupt\" or
\"Signal 2\".  The result is cached per connection."
  (with-tramp-connection-property vec "rpc-signal-strings"
    (let* ((result (tramp-rpc--call vec "process.run"
                                    `((cmd . "/bin/sh")
                                      (args . ["-c" "kill -l"])
                                      (cwd . "/"))))
           (exit-code (alist-get 'exit_code result))
           (stdout (tramp-rpc--decode-output
                    (alist-get 'stdout result)
                    (alist-get 'stdout_encoding result)))
           (raw-signals (when (and (eq exit-code 0) (> (length stdout) 0))
                          (split-string (string-trim stdout) nil 'omit)))
           ;; Prepend a placeholder 0 for signal 0 so that (nth 1 signals)
           ;; corresponds to signal 1 (HUP), (nth 2 signals) to signal 2 (INT), etc.
           (signals (cons 0 raw-signals))
           (vec-strings (make-vector 128 nil)))
      ;; Sanity: remove duplicate leading "0" entry if kill -l included one
      (when (and (stringp (cadr signals)) (string-equal (cadr signals) "0"))
        (setcdr signals (cddr signals)))
      ;; Map signal names to human-readable strings
      (dotimes (i 128)
        (let ((sig (nth i signals)))
          (aset vec-strings i
                (cond
                 ((zerop i) 0)
                 ((null sig) (format "Signal %d" i))
                 ((string-equal sig "HUP") "Hangup")
                 ((string-equal sig "INT") "Interrupt")
                 ((string-equal sig "QUIT") "Quit")
                 ((string-equal sig "STOP") "Stopped (signal)")
                 ((string-equal sig "TSTP") "Stopped")
                 ((string-equal sig "TTIN") "Stopped (tty input)")
                 ((string-equal sig "TTOU") "Stopped (tty output)")
                 (t (format "Signal %d" i))))))
      vec-strings)))

(defun tramp-rpc-handle-process-file
    (program &optional infile destination _display &rest args)
  "Like `process-file' for TRAMP-RPC files.
Resolves PROGRAM path and loads direnv environment from working directory.
When `tramp-rpc-magit--process-caches' is populated (during magit
refresh), git commands are served from the prefetch cache when possible."
  (with-parsed-tramp-file-name default-directory nil
    ;; Unquote localname in case of file-name-quoted paths (e.g. /: prefix).
    (setq localname (file-name-unquote localname))
    ;; Try serving from magit prefetch cache first (no RPC needed)
    (let ((cached (when (null infile)  ; no stdin redirection
                    (tramp-rpc-magit--process-cache-lookup program args))))
      (if cached
          ;; Cache hit - serve from prefetch
          (let ((exit-code (car cached))
                (stdout (cdr cached)))
            (tramp-rpc--route-process-file-output destination stdout)
            exit-code)
        ;; Cache miss - make actual RPC call
        (let* ((resolved-program (tramp-rpc--resolve-executable v program))
               (direnv-env (tramp-rpc--get-direnv-environment v localname))
               (stdin-content (when (and infile (not (eq infile t)))
                                (with-temp-buffer
                                  (set-buffer-multibyte nil)
                                  (insert-file-contents-literally infile)
                                  (buffer-string))))
               (result (condition-case _err
                           (tramp-rpc--call v "process.run"
                                            `((cmd . ,resolved-program)
                                              (args . ,(vconcat args))
                                              (cwd . ,localname)
                                              ,@(when direnv-env
                                                  `((env . ,direnv-env)))
                                              ,@(when stdin-content
                                                  `((stdin . ,stdin-content)))))
                         ;; When the binary doesn't exist or can't be
                         ;; spawned, return exit code 127 (command not
                         ;; found) instead of signaling an error.
                         (remote-file-error nil))))
          (if result
              (let ((exit-code (alist-get 'exit_code result))
                    (stdout (tramp-rpc--decode-output
                             (alist-get 'stdout result)
                             (alist-get 'stdout_encoding result)))
                    (stderr (tramp-rpc--decode-output
                             (alist-get 'stderr result)
                             (alist-get 'stderr_encoding result))))

                ;; Handle destination
                (tramp-rpc--route-process-file-output destination stdout stderr)

                ;; Invalidate caches if the program might modify the filesystem
                ;; and the directory isn't being watched (watched dirs get
                ;; server-pushed invalidation)
                (let ((program-name (file-name-nondirectory program)))
                  (unless (or (member program-name tramp-rpc--readonly-programs)
                              (tramp-rpc--directory-watched-p localname v))
                    (tramp-rpc--invalidate-cache-for-path default-directory)))

                ;; Handle signal strings: when
                ;; `process-file-return-signal-string' is non-nil and exit
                ;; code >= 128, return the signal name string instead.
                (if (and (bound-and-true-p process-file-return-signal-string)
                         (natnump exit-code) (>= exit-code 128))
                    (let ((strings (tramp-rpc--get-signal-strings v)))
                      (aref strings (- exit-code 128)))
                  exit-code))
            ;; Process spawn failed - return 127 (command not found)
            127))))))

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



;; ============================================================================
;; Additional handlers to avoid shell dependency
;; ============================================================================

(defvar tramp-rpc--exec-path-cache (make-hash-table :test 'equal)
  "Cache of remote exec-path keyed by connection-key.")

(defun tramp-rpc-handle-exec-path ()
  "Return remote exec-path using RPC.
Appends the remote working directory as the last element (the equivalent
of `exec-directory'), matching `tramp-sh-handle-exec-path' behavior.
Caches the PATH portion per connection."
  (with-parsed-tramp-file-name default-directory nil
    (let* ((key (tramp-rpc--connection-key v))
           (cached (gethash key tramp-rpc--exec-path-cache))
           (remote-path (or cached
                            (let ((path (tramp-rpc--fetch-remote-exec-path v)))
                              (puthash key path tramp-rpc--exec-path-cache)
                              path))))
      ;; Append localname of default-directory as last element,
      ;; the equivalent to `exec-directory'.
      (append remote-path
              (list (tramp-file-local-name
                     (expand-file-name default-directory)))))))

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
  (tramp-skeleton-file-local-copy filename
    (let* ((result (tramp-rpc--call v "file.read" (tramp-rpc--encode-path localname)))
           ;; With MessagePack, content is already raw bytes
           (content (alist-get 'content result)))
      (with-temp-file tmpfile
        (set-buffer-multibyte nil)
        (insert content)))))

(defun tramp-rpc-handle-get-home-directory (vec &optional user)
  "Return home directory for USER on remote host VEC using RPC.
If USER is nil or matches the connection user, returns the current user's
home directory from system.info.  For other users, looks up via getent.
Signals an error rather than returning nil, so that
`tramp-get-home-directory' does not cache a nil result."
  (let* ((conn-user (tramp-file-name-user vec))
         (target-user (or user conn-user)))
    (if (or (null target-user)
            (string-empty-p target-user)
            (equal target-user conn-user))
        ;; Current user - use system.info (errors propagate, not cached)
        (or (tramp-rpc--decode-string
             (alist-get 'home (tramp-rpc--call vec "system.info" nil)))
            (tramp-error vec 'file-error
                         "Remote home directory not available"))
      ;; Different user - look up via getent passwd
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
              (nth 5 fields))))))))

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



(defun tramp-rpc-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for TRAMP-RPC files.
Delegates to `tramp-handle-expand-file-name'.  If tilde expansion
fails because the connection is not available (e.g. during
`tramp-cleanup-all-connections'), retries with `tramp-tolerate-tilde'
so the path is returned with the tilde unexpanded rather than
signalling an error."
  (condition-case nil
      (tramp-handle-expand-file-name name dir)
    (file-error
     (let ((tramp-tolerate-tilde t))
       (tramp-handle-expand-file-name name dir)))))

;; ============================================================================
;; Process and advice modules (extracted)
;; ============================================================================

;; Process support, advice functions, and magit integration are now in
;; separate modules for better organization and maintainability.
(require 'tramp-rpc-process)
(require 'tramp-rpc-advice)
(require 'tramp-rpc-magit)

(defvar tramp-rpc--delivering-output nil
  "Non-nil while delivering process output to the local relay.
Used by advice functions to bypass interception during output delivery.")

(defcustom tramp-rpc-use-async-read t
  "Whether to use async callback-based reads for pipe processes.
When t (default), uses fast async reads with server-side blocking.
When nil, uses timer-based polling (slower, for debugging)."
  :type 'boolean
  :group 'tramp-rpc)

(defcustom tramp-rpc-async-read-timeout-ms 200
  "Timeout in milliseconds for async process reads.
The server will block for this long waiting for data before returning.
Lower values mean more responsive but higher CPU usage.
Also controls process exit detection latency."
  :type 'integer
  :group 'tramp-rpc)


;; ============================================================================
;; File name handler registration
;; ============================================================================

(defconst tramp-rpc-file-name-handler-alist
  '(;; =========================================================================
    ;; RPC-based file attribute operations
    ;; =========================================================================
    ;; RPC-based file attribute operations
    ;; =========================================================================
    (file-exists-p . tramp-handle-file-exists-p)
    (file-readable-p . tramp-handle-file-readable-p)
    (file-writable-p . tramp-handle-file-writable-p)
    (file-executable-p . tramp-rpc-handle-file-executable-p)
    (file-directory-p . tramp-handle-file-directory-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-truename . tramp-rpc-handle-file-truename)
    (file-attributes . tramp-rpc-handle-file-attributes)
    (file-modes . tramp-handle-file-modes)
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
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
    (insert-directory . tramp-handle-insert-directory)
    (copy-directory . tramp-handle-copy-directory)

    ;; =========================================================================
    ;; RPC-based file I/O operations
    ;; =========================================================================
    (insert-file-contents . tramp-handle-insert-file-contents)
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
    (shell-command . tramp-handle-shell-command)
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
    (list-system-processes . tramp-handle-list-system-processes)
    (process-attributes . tramp-handle-process-attributes)

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
    (abbreviate-file-name . tramp-handle-abbreviate-file-name)
    (file-group-gid . tramp-handle-file-group-gid)
    (file-user-uid . tramp-handle-file-user-uid)
    (memory-info . tramp-handle-memory-info)
    (access-file . tramp-handle-access-file)
    (directory-file-name . tramp-handle-directory-file-name)
    (dired-uncache . tramp-handle-dired-uncache)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p)
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
;; Unload support
;; ============================================================================

(defun tramp-rpc-unload-function ()
  "Unload function for tramp-rpc.
Removes advice and cleans up async processes."
  ;; Remove all advice (from tramp-rpc-advice module)
  (tramp-rpc-advice-remove)
  ;; Clean up all async processes (from tramp-rpc-process module)
  (tramp-rpc--cleanup-async-processes)
  ;; Clean up PTY processes (from tramp-rpc-process module)
  (tramp-rpc--cleanup-pty-processes)
  ;; Return nil to allow normal unload to proceed
  nil)

(provide 'tramp-rpc)
;;; tramp-rpc.el ends here
