;;; tramp-rpc.el --- TRAMP backend using RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Version: 0.7.0
;; Keywords: comm, processes, files
;; Package-Requires: ((emacs "30.1") (msgpack "0") (tramp "2.8.1.3"))

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
  ;; Check, that `tramp-rpc-method' is still bound.  It isn't after
  ;; unloading `tramp-rpc', but this body still exists as compiled
  ;; function in `after-load-alist'.
  (when (boundp 'tramp-rpc-method)
  ;; Register the method
  (add-to-list 'tramp-methods
               `(,tramp-rpc-method
                 ;; Declare that the rpc method uses the host name.
                 ;; tramp-compute-multi-hops validates that methods without
                 ;; "%h" in tramp-login-args use a host matching the previous
                 ;; hop.  Since rpc IS host-directed (it SSH-connects to the
                 ;; specified host), advertising "%h" here lets rpc appear
                 ;; as a proxy hop in chains like /rpc:server|sudo:root@server:/path
                 ;; without triggering the "host does not match" error.
                 ;; The actual tramp-login-args value is never used for login
                 ;; because rpc is a foreign (non-tramp-sh) file handler.
                 (tramp-login-args (("%h")))
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

  ;; Detect privilege elevation paths with rpc hops, e.g.
  ;; /rpc:user@host|sudo:root@host:/path.  These are handled by the
  ;; tramp-rpc handler which starts the RPC server via sudo.
  (defsubst tramp-rpc--sudo-file-name-p (vec-or-filename)
    "Check if VEC-OR-FILENAME is a privilege elevation with an rpc hop."
    (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename))
                (hop (tramp-file-name-hop vec)))
      (and (string-match-p "rpc:" hop)
           (member (tramp-file-name-method vec)
                   '("sudo" "su" "doas" "sg" "run0" "ksu")))))

  ;; Register the foreign handler directly in the alist.  We cannot use
  ;; `tramp-register-foreign-file-name-handler' here because it tries to
  ;; read `tramp-rpc-file-name-handler-alist' (defined in the full file),
  ;; which isn't loaded yet.  The handler function itself is an autoload
  ;; stub that triggers loading of tramp-rpc.el on first use.
  (add-to-list 'tramp-foreign-file-name-handler-alist
               '(tramp-rpc-file-name-p . tramp-rpc-file-name-handler))
  ;; sudo+rpc handler must be checked, maps to the same handler.
  (add-to-list 'tramp-foreign-file-name-handler-alist
               '(tramp-rpc--sudo-file-name-p . tramp-rpc-file-name-handler))

  ;; Configure user and host name completion.
  (tramp-set-completion-function "rpc" tramp-completion-function-alist-ssh)

  ;; Allow the "rpc" method in multi-hop filename syntax.
  ;; TRAMP's `tramp-multi-hop-p' only returns t for tramp-sh methods,
  ;; which would cause `tramp-dissect-file-name' to reject filenames like
  ;; /rpc:hop|rpc:target:/path.  We advise it to also accept "rpc".
  (defun tramp-rpc--multi-hop-advice (orig-fun vec)
    "Allow the rpc method and rpc+sudo paths in multi-hop chains."
    (or (string= (tramp-file-name-method vec) tramp-rpc-method)
        ;; Also allow privilege elevation methods when the hop contains rpc
        (when-let* ((hop (tramp-file-name-hop vec)))
          (and (string-match-p "rpc:" hop)
               (member (tramp-file-name-method vec)
                       '("sudo" "su" "doas" "sg" "run0" "ksu"))))
        (funcall orig-fun vec)))
  (advice-add 'tramp-multi-hop-p :around #'tramp-rpc--multi-hop-advice)))

;; Now the actual implementation
(require 'cl-lib)
(require 'tramp)
(require 'tramp-sh)
(require 'tramp-rpc-protocol)

;; Check for minimum Tramp version.  The Package-Requires header declares
;; (tramp "2.8.1.3") but that is only enforced by package.el at install
;; time.  Guard at load time so that manual installations fail clearly.
(when (version< tramp-version "2.8.1.3")
  (error "tramp-rpc requires Tramp >= 2.8.1.3, but %s is loaded"
         tramp-version))

;; Give the rpc method all ssh connection parameters so it can serve
;; as a hop in tramp-sh multi-hop chains (e.g.
;; /rpc:host|sudo:root@host:/path).  For single-hop rpc, the foreign
;; handler takes over and these parameters are never used.  This is
;; future-proof: if ssh's parameters change in future TRAMP versions,
;; rpc automatically inherits the updates.
;; Suggested by Michael Albinus.
(when-let* ((ssh-params (alist-get "ssh" tramp-methods nil nil #'equal))
            (rpc-entry (assoc tramp-rpc-method tramp-methods)))
  (setcdr rpc-entry ssh-params))

;; Silence byte-compiler warnings for functions defined in with-eval-after-load
(declare-function tramp-rpc-advice-remove "tramp-rpc-advice")
(declare-function tramp-add-external-operation "tramp")
(declare-function tramp-remove-external-operation "tramp")
(declare-function tramp-rpc--multi-hop-advice "tramp-rpc")
(declare-function tramp-rpc--sudo-file-name-p "tramp-rpc")

;; ============================================================================
;; Sudo-via-RPC: detect privilege elevation from hop chains
;; ============================================================================

(defun tramp-rpc--detect-sudo-elevation (vec)
  "Return the SSH user if VEC needs sudo elevation via RPC, or nil.
Detects when the hop chain has an rpc hop to the same host as the
target but with a different user, indicating privilege elevation.
For /rpc:user@host|sudo:root@host:/path, returns \"user\"."
  (when-let* ((hop (tramp-file-name-hop vec))
              (target-host (tramp-file-name-host vec)))
    (let ((hops (split-string hop tramp-postfix-hop-regexp 'omit))
          result)
      ;; Walk hops in reverse (closest to target first) looking for
      ;; an rpc hop to the same host.
      (dolist (hop-str (reverse hops))
        (unless result
          (let* ((hop-name (concat tramp-prefix-format hop-str
                                   tramp-postfix-host-format))
                 (hop-vec (tramp-dissect-file-name hop-name 'nodefault)))
            (when (and (string= (tramp-file-name-method hop-vec) "rpc")
                       (string= (tramp-file-name-host hop-vec) target-host))
              (setq result (or (tramp-file-name-user hop-vec)
                               (user-login-name)))))))
      result)))

(defun tramp-rpc--proxy-hop-string (vec)
  "Return VEC's hop string with same-host sudo hops removed.
For /rpc:gw|rpc:user@host|sudo:root@host:/path, returns \"rpc:gw|\".
Returns nil if no proxy hops remain."
  (when-let* ((hop (tramp-file-name-hop vec))
              (target-host (tramp-file-name-host vec)))
    (let ((hops (split-string hop tramp-postfix-hop-regexp 'omit))
          proxy-hops)
      (dolist (hop-str hops)
        (let* ((hop-name (concat tramp-prefix-format hop-str
                                 tramp-postfix-host-format))
               (hop-vec (tramp-dissect-file-name hop-name 'nodefault)))
          ;; Keep hops that are genuine proxies (different host)
          (unless (and (string= (tramp-file-name-method hop-vec) "rpc")
                       (string= (tramp-file-name-host hop-vec) target-host))
            (push hop-str proxy-hops))))
      (when proxy-hops
        (concat (mapconcat #'identity (nreverse proxy-hops)
                           tramp-postfix-hop-format)
                tramp-postfix-hop-format)))))

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
(declare-function tramp-rpc--clear-file-caches-for-connection "tramp-rpc-magit")
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

(defcustom tramp-rpc-compress-file-read (fboundp 'zlib-decompress-region)
  "When non-nil, use compression for file reads to enable faster transfers."
  :type 'boolean
  :group 'tramp-rpc)

(defconst tramp-rpc-own-remote-path 'tramp-rpc-own-remote-path
  "Placeholder in `tramp-rpc-remote-path'.
Replaced by the PATH from the user's login shell on the remote host.
The login shell is looked up from system.info (via getpwuid) or
`getent passwd', and invoked as a login shell to capture PATH.
This works correctly for bash, zsh, fish, and other shells.")

(defcustom tramp-rpc-remote-path
  `(tramp-rpc-own-remote-path
    "/usr/local/bin" "/usr/bin" "/bin" "/usr/local/sbin" "/usr/sbin" "/sbin")
  "List of directories to search for executables on remote hosts.
Each element is either a directory string or the symbol
`tramp-rpc-own-remote-path'.  The symbol is replaced at runtime by
the directories from the user's login shell PATH on the remote host.

The default value uses the login shell PATH and appends common
system directories as fallbacks.  Customize this to prepend
directories like \"~/.local/bin\" or to remove the login shell
lookup entirely."
  :type '(repeat (choice (string :tag "Directory")
                         (const :tag "Login shell PATH" tramp-rpc-own-remote-path)))
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

(defun tramp-rpc--extract-file-read-content (rpc-result)
  "Extract and optionally decompress content from FILE.READ RPC-RESULT.
Signals `remote-file-error' on compressed payload decode failures."
  (let ((content (if (stringp rpc-result)
                     rpc-result
                   (alist-get 'content rpc-result))))
    (if (and (not (stringp rpc-result))
             (alist-get 'compressed rpc-result))
        (let ((compression (or (alist-get 'compression rpc-result) "zlib")))
          (cond
           ((and (string= compression "zlib")
                 (fboundp 'zlib-decompress-region))
            (condition-case err
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert content)
                  (zlib-decompress-region (point-min) (point-max))
                  (buffer-string))
              (error
               (signal 'remote-file-error
                       (list "RPC"
                             (format "zlib decompression failed: %s" err))))))
           (t
            (signal 'remote-file-error
                    (list "RPC"
                          (format "Unsupported file.read compression: %s" compression))))))
      content)))

(defun tramp-rpc--file-read-params (localname &optional force-uncompressed)
  "Build params for `file.read' on LOCALNAME.
When `tramp-rpc-compress-file-read' is non-nil, request compression unless
FORCE-UNCOMPRESSED is non-nil."
  (let ((params (tramp-rpc--encode-path localname)))
    (when (and tramp-rpc-compress-file-read
               (not force-uncompressed))
      (push '(compress . t) params))
    params))

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
  "Generate cache key for direnv environment on VEC in DIRECTORY.
Normalizes DIRECTORY via `expand-file-name' so that ~ and the expanded
home path map to the same cache key."
  (cons (tramp-rpc--connection-key vec)
        (tramp-file-local-name
         (expand-file-name
          (tramp-make-tramp-file-name vec directory)))))

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
  (condition-case err
      (let* ((result (tramp-rpc--call vec "process.run"
                                       `((cmd . "/bin/sh")
                                         (args . ["-l" "-c"
                                                  ,(concat "cd " (tramp-shell-quote-argument directory)
                                                           " && direnv export json 2>/dev/null")])
                                         (cwd . "/"))))
             (exit-code (alist-get 'exit_code result))
             (stdout (tramp-rpc--decode-output
                      (alist-get 'stdout result)
                      (alist-get 'stdout_encoding result))))
        (if (and (eq exit-code 0)
                 (> (length stdout) 0))
            ;; Parse JSON output into alist, filter to essential vars
            (condition-case err
                (let* ((json-object-type 'alist)
                       (json-key-type 'string)
                       (full-env (json-read-from-string stdout)))
                  ;; Filter to only essential variables
                  (cl-loop for var in tramp-rpc-direnv-essential-vars
                           for pair = (assoc var full-env)
                           when pair collect pair))
              (error
               (tramp-rpc--debug "direnv JSON parse failed: %S" err)
               nil))
          ;; If exit code is 127 (command not found), mark direnv as unavailable
          (when (eq exit-code 127)
            (puthash (tramp-rpc--connection-key vec)
                     :unavailable
                     tramp-rpc--direnv-available-cache))
          nil))
    (error
     (tramp-rpc--debug "direnv fetch failed: %S" err)
     nil)))

(defun tramp-rpc--clear-direnv-cache (&optional vec)
  "Clear the direnv caches.
If VEC is provided, only clear entries for that connection.
Otherwise clear all entries."
  (if vec
      (let ((conn-key (tramp-rpc--connection-key vec)))
        ;; Clear environment cache entries for this connection
        (let ((keys-to-remove nil))
          (maphash (lambda (key _value)
                     (when (equal (car key) conn-key)
                       (push key keys-to-remove)))
                   tramp-rpc--direnv-cache)
          (dolist (key keys-to-remove)
            (remhash key tramp-rpc--direnv-cache)))
        ;; Clear availability cache for this connection
        (remhash conn-key tramp-rpc--direnv-available-cache))
    (clrhash tramp-rpc--direnv-cache)
    (clrhash tramp-rpc--direnv-available-cache)))

(defvar tramp-rpc--executable-cache (make-hash-table :test 'equal)
  "Cache of executable paths keyed by (connection-key . program).
Value is the full path or :not-found.")

;; Forward-declare caches used by tramp-rpc--remove-connection (defined
;; later in the exec-path section).  The byte-compiler needs to see
;; these defvars before their first reference.
(defvar tramp-rpc--exec-path-cache (make-hash-table :test 'equal)
  "Cache of remote exec-path keyed by connection-key.")

(defvar tramp-rpc--login-shell-cache (make-hash-table :test 'equal)
  "Cache of remote login shell keyed by connection-key.")

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

(defun tramp-rpc--ensure-inside-emacs-env (env)
  "Ensure INSIDE_EMACS is set in environment alist ENV.
ENV is an alist of (KEY . VALUE) string pairs, or nil.
If INSIDE_EMACS is not already present, it is added with the value
from `tramp-inside-emacs'.  Returns the (possibly augmented) alist."
  (if (assoc "INSIDE_EMACS" env)
      env
    (cons (cons "INSIDE_EMACS" (tramp-inside-emacs)) env)))

(defun tramp-rpc--caller-environment ()
  "Extract environment variable overrides from `process-environment'.
Emacs packages dynamically bind env vars via `with-environment-variables'
or `setenv' (e.g. magit sets GIT_INDEX_FILE for temp-index operations).
These additions/changes land in `process-environment' but are not forwarded
by `tramp-rpc-handle-process-file' unless we explicitly extract them.

Compares the current `process-environment' against the toplevel default.
Entries that are only present in the current dynamic scope (e.g. added
by `with-environment-variables') are returned as an alist of
\(NAME . VALUE) pairs."
  (let ((toplevel (default-toplevel-value 'process-environment))
        (env nil))
    (dolist (elt process-environment)
      (when (and (stringp elt)
                 (not (member elt toplevel))
                 (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" elt))
        (push (cons (match-string 1 elt) (match-string 2 elt)) env)))
    (nreverse env)))

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
Uses `command -v` via the user's login shell for lookup, so that
executables in shell-specific PATH entries are found.
Uses a unique marker to separate MOTD/banner text from actual output,
following the pattern used by standard TRAMP."
  (condition-case err
      (let* (;; Use a unique marker (MD5 hash) to delimit output from MOTD text
             ;; This is the same approach used by tramp-sh.el
             (marker (md5 (format "tramp-rpc-%s-%s" program (float-time))))
             (shell (tramp-rpc--get-remote-login-shell vec))
             (result (tramp-rpc--call vec "process.run"
                                       `((cmd . ,shell)
                                         (args . ["-l" "-c"
                                                  ,(format "echo %s; command -v %s"
                                                            marker (tramp-shell-quote-argument program))])
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
    (error
     (tramp-rpc--debug "find-executable failed for %s: %S" program err)
     nil)))

(defsubst tramp-rpc--port-to-string (port)
  "Normalize PORT to a string, or return nil.
PORT may be a number (from defaults), a string (from filename
parsing via `tramp-dissect-file-name'), or nil (when unset).
Upstream TRAMP always stores port as a string in the
`tramp-file-name' struct, but defensive handling of numbers
avoids breakage if callers supply numeric defaults."
  (cond ((stringp port) port)
        ((numberp port) (number-to-string port))
        (t nil)))

(defun tramp-rpc--connection-key (vec)
  "Generate a connection key for VEC.
Includes the hop chain so that different multi-hop routes to the
same host produce distinct connections."
  (list (tramp-file-name-host vec)
        (tramp-file-name-user vec)
        (or (tramp-rpc--port-to-string (tramp-file-name-port vec)) "22")
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
Also clears the executable, exec-path, and login shell caches."
  (let ((key (tramp-rpc--connection-key vec)))
    (remhash key tramp-rpc--connections)
    (remhash key tramp-rpc--exec-path-cache)
    (remhash key tramp-rpc--login-shell-cache))
  (tramp-rpc--clear-executable-cache vec))

(defun tramp-rpc--ensure-connection (vec)
  "Ensure we have an active RPC connection to VEC.
Returns the connection plist.
When `non-essential' is non-nil and no live connection exists,
throws `non-essential' instead of opening a new connection.
This prevents background operations (timers, fontification,
completion) from blocking on unreachable hosts."
  (let ((conn (tramp-rpc--get-connection vec)))
    (if (and conn
             (process-live-p (plist-get conn :process))
             (buffer-live-p (plist-get conn :buffer)))
        conn
      ;; Stale connection - remove it before reconnecting
      (when conn
        (tramp-rpc--remove-connection vec))
      ;; During non-essential operations, don't open new connections.
      ;; This mirrors the (unless (tramp-connectable-p vec)
      ;; (throw 'non-essential 'non-essential)) pattern used by every
      ;; standard TRAMP backend in their maybe-open-connection functions.
      (unless (tramp-connectable-p vec)
        (throw 'non-essential 'non-essential))
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

;;; ============================================================================
;;; Authentication via tramp-process-actions
;;; ============================================================================

;; Reuse upstream TRAMP's `tramp-process-actions' state machine for all
;; interactive authentication (SSH passwords, sudo, host-key prompts,
;; OTP, security keys).  This gives us auth-source integration, password
;; caching, wrong-password detection, and locale-aware prompt matching
;; for free, instead of reimplementing with a custom regexp + loop.

(defvar tramp-rpc--controlmaster-socket-path nil
  "Dynamically bound socket path during ControlMaster establishment.
Used by `tramp-rpc--action-controlmaster-established'.")

(defun tramp-rpc--action-controlmaster-established (proc _vec)
  "Succeed when the ControlMaster socket file appears, fail on process death.
The target socket path is read from the dynamic variable
`tramp-rpc--controlmaster-socket-path'."
  (cond
   ((file-exists-p tramp-rpc--controlmaster-socket-path)
    (throw 'tramp-action 'ok))
   ((not (process-live-p proc))
    (while (tramp-accept-process-output proc))
    (throw 'tramp-action 'process-died))))

(defconst tramp-rpc--controlmaster-actions
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-yesno-prompt-regexp tramp-action-yesno)
    (tramp-yn-prompt-regexp tramp-action-yn)
    (tramp-process-alive-regexp tramp-rpc--action-controlmaster-established))
  "Actions for SSH ControlMaster establishment.
Handles password prompts, host-key verification, and detects the
ControlMaster socket file appearing as the success condition.")

(defun tramp-rpc--action-sudo-complete (proc _vec)
  "Succeed when `sudo -v' exits with code 0, fail otherwise."
  (unless (process-live-p proc)
    (while (tramp-accept-process-output proc))
    (throw 'tramp-action
           (if (zerop (process-exit-status proc)) 'ok 'permission-denied))))

(defconst tramp-rpc--sudo-actions
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-process-alive-regexp tramp-rpc--action-sudo-complete))
  "Actions for `sudo -v' pre-authentication.
Handles password prompts and detects sudo exit as success.")

;;; ============================================================================
;;; Multi-hop support
;;; ============================================================================

(defun tramp-rpc--hops-to-proxyjump (vec)
  "Convert VEC's hop chain to an SSH ProxyJump (-J) string.
Parses the TRAMP hop field (e.g. \"rpc:user@gateway|\") and converts
each hop to the SSH ProxyJump format (e.g. \"user@gateway\").
Returns nil if there are no hops.

Same-host rpc hops are skipped because they represent sudo elevation,
not proxy jumps.  Supports mixed methods: both \"rpc:\" and \"ssh:\"
hops are accepted since ProxyJump only needs host connectivity."
  (when-let* ((hops (tramp-file-name-hop vec))
              (target-host (tramp-file-name-host vec)))
    (let (proxy-parts)
      (dolist (hop-str (split-string hops tramp-postfix-hop-regexp 'omit))
        (let* ((hop-name (concat tramp-prefix-format hop-str
                                 tramp-postfix-host-format))
               (hop-vec (tramp-dissect-file-name hop-name 'nodefault))
               (hop-host (tramp-file-name-host hop-vec)))
          ;; Skip same-host rpc hops (sudo elevation, not proxy)
          (unless (and (string= (tramp-file-name-method hop-vec) "rpc")
                       (string= hop-host target-host))
            (push (concat
                   (when (tramp-file-name-user hop-vec)
                     (concat (tramp-file-name-user hop-vec) "@"))
                   hop-host
                   (when-let* ((port (tramp-rpc--port-to-string
                                      (tramp-file-name-port hop-vec))))
                     (concat ":" port)))
                  proxy-parts))))
      (when proxy-parts
        (mapconcat #'identity (nreverse proxy-parts) ",")))))

(defun tramp-rpc--controlmaster-socket-path (vec)
  "Return the ControlMaster socket path for VEC.
Expands SSH escape sequences in `tramp-rpc-controlmaster-path'.
For sudo-via-RPC paths, uses the SSH user and excludes the sudo
hop so the socket is shared with the normal rpc connection."
  (let* ((sudo-ssh-user (tramp-rpc--detect-sudo-elevation vec))
         (host (tramp-file-name-host vec))
         (user (or sudo-ssh-user (tramp-file-name-user vec) (user-login-name)))
         (port (or (tramp-rpc--port-to-string (tramp-file-name-port vec)) "22"))
         ;; For sudo, use only proxy hops (exclude the same-host sudo hop)
         (hop (if sudo-ssh-user
                  (tramp-rpc--proxy-hop-string vec)
                (tramp-file-name-hop vec)))
         (path tramp-rpc-controlmaster-path))
    ;; Expand common SSH escape sequences
    ;; %h = host, %r = remote user, %p = port
    ;; %C = hash of %l%h%p%r (we approximate this)
    (setq path (replace-regexp-in-string "%h" host path t t))
    (setq path (replace-regexp-in-string "%r" user path t t))
    (setq path (replace-regexp-in-string "%p" port path t t))
    ;; For %C, use a simple hash approximation
    ;; Include the hop chain so different multi-hop routes get different sockets
    (setq path (replace-regexp-in-string
                "%C"
                (md5 (format "%s%s%s%s%s" (system-name) host port user
                             (or hop "")))
                path t t))
    (expand-file-name path)))

(defun tramp-rpc--controlmaster-active-p (vec)
  "Return non-nil if a ControlMaster connection is active for VEC."
  (let* ((sudo-ssh-user (tramp-rpc--detect-sudo-elevation vec))
         (socket-path (tramp-rpc--controlmaster-socket-path vec))
         (host (tramp-file-name-host vec))
         (user (or sudo-ssh-user (tramp-file-name-user vec)))
         (port (tramp-rpc--port-to-string (tramp-file-name-port vec)))
         (proxyjump (tramp-rpc--hops-to-proxyjump vec)))
    (and (file-exists-p socket-path)
         ;; Check if the socket is actually usable via ssh -O check
         (zerop (apply #'call-process "ssh" nil nil nil
                       (append
                        (when user (list "-l" user))
                        (when port (list "-p" port))
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
  (let* ((sudo-ssh-user (tramp-rpc--detect-sudo-elevation vec))
         (host (tramp-file-name-host vec))
         (user (or sudo-ssh-user (tramp-file-name-user vec)))
         (port (tramp-rpc--port-to-string (tramp-file-name-port vec)))
         (proxyjump (tramp-rpc--hops-to-proxyjump vec))
         (socket-path (tramp-rpc--controlmaster-socket-path vec))
         (process-name (format "*tramp-rpc-auth %s*" host))
         (buffer (get-buffer-create (format " *tramp-rpc-auth %s*" host)))
         (ssh-args (append
                    (list "ssh")
                    tramp-rpc-ssh-args
                    (when user (list "-l" user))
                    (when port (list "-p" port))
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
    (set-process-sentinel process #'ignore)
    ;; Set up process properties for tramp-process-actions / tramp-read-passwd.
    ;; pw-vector tells auth-source where to look up credentials.
    (process-put process 'tramp-vector vec)
    (tramp-set-connection-property process "hop-vector" vec)
    (tramp-set-connection-property
     process "pw-vector"
     (make-tramp-file-name :method "ssh" :user user :host host))
    ;; Use upstream tramp-process-actions for password/host-key handling.
    ;; The custom action checks for the ControlMaster socket appearing.
    (let ((tramp-rpc--controlmaster-socket-path socket-path))
      (tramp-process-actions process vec nil
                             tramp-rpc--controlmaster-actions 60))
    ;; tramp-process-actions throws on failure; reaching here means success.
    (sleep-for 0.1)
    t))

(defun tramp-rpc--sudo-authenticate (vec ssh-user)
  "Pre-authenticate sudo on the remote host for VEC.
Uses the ControlMaster to run `sudo -v' interactively, caching
credentials so the server can be started with sudo via pipes.
SSH-USER is the user for the SSH connection (from the rpc hop).

Uses `tramp-process-actions' with `tramp-rpc--sudo-actions' for
password handling, giving auth-source integration and password
caching for free."
  (let* ((host (tramp-file-name-host vec))
         (port (tramp-rpc--port-to-string (tramp-file-name-port vec)))
         (proxyjump (tramp-rpc--hops-to-proxyjump vec))
         (socket-path (tramp-rpc--controlmaster-socket-path vec))
         (process-name (format "*tramp-rpc-sudo %s*" host))
         (buffer (get-buffer-create (format " *tramp-rpc-sudo %s*" host)))
         (ssh-args (append
                    (list "ssh")
                    tramp-rpc-ssh-args
                    (when ssh-user (list "-l" ssh-user))
                    (when port (list "-p" port))
                    ;; Multi-hop via ProxyJump
                    (when proxyjump (list "-J" proxyjump))
                    ;; Reuse ControlMaster for the SSH layer
                    (when (and tramp-rpc-use-controlmaster
                               (file-exists-p socket-path))
                      (list "-o" "ControlMaster=auto"
                            "-o" (format "ControlPath=%s" socket-path)))
                    (list "-o" "StrictHostKeyChecking=accept-new")
                    ;; Run sudo -v to cache credentials, then exit
                    (list host "sudo" "-v")))
         process)
    (with-current-buffer buffer (erase-buffer))
    ;; Use PTY for the sudo password prompt
    (let ((process-connection-type t))
      (setq process (apply #'start-process process-name buffer ssh-args)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process #'ignore)
    ;; Set up process properties for tramp-process-actions / tramp-read-passwd.
    ;; pw-vector points to the sudo user so auth-source can look up
    ;; credentials via e.g. "machine host login user port sudo".
    (process-put process 'tramp-vector vec)
    (tramp-set-connection-property process "hop-vector" vec)
    (tramp-set-connection-property
     process "pw-vector"
     (make-tramp-file-name :method "sudo" :user ssh-user :host host))
    ;; Use upstream tramp-process-actions for password handling.
    ;; tramp-rpc--action-sudo-complete throws 'ok on exit 0,
    ;; 'permission-denied otherwise.  tramp-process-actions handles
    ;; timeout, wrong-password cleanup, and error signaling.
    (tramp-process-actions process vec nil tramp-rpc--sudo-actions 60)))

(defun tramp-rpc--start-server-process (vec binary-path)
  "Start the RPC server on VEC at BINARY-PATH and verify it responds.
BINARY-PATH is the remote localname of the server binary (may contain ~).
For sudo-via-RPC paths, the server is started via sudo.
Returns the connection plist.  Signals `remote-file-error' on failure."
  (let* ((sudo-ssh-user (tramp-rpc--detect-sudo-elevation vec))
         (host (tramp-file-name-host vec))
         (user (or sudo-ssh-user (tramp-file-name-user vec)))
         (port (tramp-rpc--port-to-string (tramp-file-name-port vec)))
         (proxyjump (tramp-rpc--hops-to-proxyjump vec))
         ;; Build SSH command to run the RPC server
         (ssh-args (append
                    (list "ssh")
                    ;; Raw SSH arguments (e.g., -v, -F config)
                    tramp-rpc-ssh-args
                    (when user (list "-l" user))
                    (when port (list "-p" port))
                    ;; Multi-hop via ProxyJump
                    (when proxyjump (list "-J" proxyjump))
                    ;; Only use BatchMode=yes when ControlMaster handles auth;
                    ;; without it, BatchMode=yes prevents password prompts.
                    (when tramp-rpc-use-controlmaster
                      (list "-o" "BatchMode=yes"))
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
                    ;; For sudo elevation, wrap the binary in sudo
                    (if sudo-ssh-user
                        (list host "sudo" "-n"
                              "-u" (tramp-file-name-user vec)
                              binary-path)
                      (list host binary-path))))
         ;; Use TRAMP's standard naming so tramp-get-connection-process works
         (process-name (tramp-get-connection-name vec))
         (buffer-name (tramp-buffer-name vec))
         (buffer (get-buffer-create buffer-name))
         process)

    ;; Clear buffer - use unibyte for binary MessagePack framing
    (with-current-buffer buffer
      (erase-buffer)
      (set-buffer-multibyte nil)
      (set-marker (mark-marker) (point-min)))

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
        (signal 'remote-file-error (list "Failed to connect to RPC server on" host)))

      ;; Store remote uname so `tramp-check-remote-uname' works.
      ;; The server returns "linux" or "macos"; map to the kernel name
      ;; that tramp-sh expects ("Linux", "Darwin", etc.).
      (let ((os (alist-get 'os response)))
        (when os
          (tramp-set-connection-property
           vec "uname"
           (pcase os
             ("macos" "Darwin")
             ("linux" "Linux")
             (_ os))))))

    ;; Set connection-local variables in the connection buffer.
    ;; Every TRAMP backend must call this after establishing the connection
    ;; so that connection-local variable profiles (registered via
    ;; `connection-local-set-profiles') are applied.  This enables variables
    ;; like `tramp-direct-async-process', `shell-file-name', `path-separator'
    ;; etc. to take effect in the connection buffer.
    (tramp-set-connection-local-variables vec)

    ;; Mark as connected for TRAMP's connectivity checks (used by projectile, etc.)
    (tramp-set-connection-property process "connected" t)

    ;; Mark as connected on the vec so `tramp-list-connections' finds
    ;; this connection and `tramp-cleanup-connection' can offer it
    ;; interactively.  The value is the connection buffer, matching the
    ;; convention in `tramp-get-buffer'.
    ;; Emacs 30.x uses "process-buffer"; newer TRAMP (31+) uses " connected".
    ;; Set both for compatibility.
    (tramp-set-connection-property vec "process-buffer" buffer)
    (tramp-set-connection-property vec " connected" buffer)

    (tramp-rpc--get-connection vec)))

(defun tramp-rpc--cleanup-failed-connection (vec)
  "Clean up a failed connection attempt for VEC.
Kills the process if still alive and removes the connection entry."
  (let ((conn (tramp-rpc--get-connection vec)))
    (when conn
      (let ((proc (plist-get conn :process)))
        (when (process-live-p proc)
          (delete-process proc)))
      (tramp-rpc--remove-connection vec))))

(defun tramp-rpc--cleanup-bootstrap-connection (vec)
  "Close the scpx/scp bootstrap connection for VEC if it exists.
The bootstrap connection is only needed during deploy and should be
closed afterward to prevent other packages (vc, diff-hl) from
accidentally routing file operations through tramp-sh."
  (let* ((bootstrap-vec (tramp-rpc-deploy--bootstrap-vec vec))
         (proc (tramp-get-connection-process bootstrap-vec)))
    (when (and proc (process-live-p proc))
      (delete-process proc))))

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
  ;; For sudo-via-RPC, pre-authenticate sudo so the server can be
  ;; started with `sudo -n' (non-interactive) via pipes.
  (when-let* ((sudo-ssh-user (tramp-rpc--detect-sudo-elevation vec)))
    (tramp-rpc--sudo-authenticate vec sudo-ssh-user))
  (if tramp-rpc-deploy-never-deploy
      ;; Never-deploy mode: use the configured path directly, no fallback.
      (let ((binary-path (tramp-rpc-deploy-ensure-binary vec)))
        (condition-case err
            (tramp-rpc--start-server-process vec binary-path)
          (remote-file-error
           (tramp-rpc--cleanup-failed-connection vec)
           (signal 'remote-file-error
                   (list (format
			  "tramp-rpc-server not found at \"%s\" on %s (never-deploy is set, no deployment attempted). Set `tramp-rpc-deploy-remote-binary-path' to the correct path. Original error: %s"
                          binary-path (tramp-file-name-host vec)
                          (error-message-string err)))))))
    ;; Normal mode: try expected path first, deploy on failure.
    ;; This avoids opening a bootstrap (scpx) connection just to run
    ;; `test -x binary', which takes ~6s for tramp-sh to establish the
    ;; shell.  If the binary exists (the common case after first deploy),
    ;; this connects directly.  If it doesn't exist (first time or after
    ;; version bump), SSH exits immediately, we catch the error, deploy
    ;; via scpx, and retry.
    (condition-case nil
        (tramp-rpc--start-server-process
         vec (tramp-rpc-deploy-expected-binary-localname))
      (remote-file-error
       ;; Connection failed - binary likely missing.  Clean up and deploy.
       (tramp-rpc--cleanup-failed-connection vec)
       (let ((binary-path (tramp-rpc-deploy-ensure-binary vec)))
         ;; Close the bootstrap connection - it's no longer needed and
         ;; leaving it alive can cause vc/diff-hl sentinels to route
         ;; file operations through tramp-sh instead of tramp-rpc.
         (tramp-rpc--cleanup-bootstrap-connection vec)
         (tramp-rpc--start-server-process vec binary-path))))))

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

(defun tramp-rpc--cleanup-controlmaster (vec)
  "Clean up the ControlMaster process and socket for VEC.
Sends an SSH -O exit command to gracefully close the ControlMaster
socket, then kills the auth process and buffer."
  (when tramp-rpc-use-controlmaster
    (let* ((host (tramp-file-name-host vec))
           (user (tramp-file-name-user vec))
           (port (tramp-rpc--port-to-string (tramp-file-name-port vec)))
           (proxyjump (tramp-rpc--hops-to-proxyjump vec))
           (socket-path (tramp-rpc--controlmaster-socket-path vec))
           (auth-process-name (format "*tramp-rpc-auth %s*" host))
           (auth-buffer-name (format " *tramp-rpc-auth %s*" host))
           (auth-process (get-process auth-process-name))
           (auth-buffer (get-buffer auth-buffer-name)))
      ;; Close the ControlMaster socket gracefully via ssh -O exit.
      ;; This is a local control message (no network round-trip), so fast.
      (when (file-exists-p socket-path)
        (ignore-errors
          (apply #'call-process "ssh" nil nil nil
                 (append
                  (when user (list "-l" user))
                  (when port (list "-p" port))
                  (when proxyjump (list "-J" proxyjump))
                  (list "-o" (format "ControlPath=%s" socket-path)
                        "-O" "exit" host)))))
      ;; Kill the auth process.
      (when (and auth-process (process-live-p auth-process))
        (delete-process auth-process))
      ;; Kill the auth buffer.
      (when (buffer-live-p auth-buffer)
        (kill-buffer auth-buffer)))))

;; ============================================================================
;; RPC communication
;; ============================================================================

(defun tramp-rpc--connection-filter (process output)
  "Filter for RPC connection PROCESS receiving OUTPUT.
Handles async responses by dispatching to registered callbacks.
Uses length-prefixed binary framing: <4-byte BE length><msgpack payload>."
  (let ((buffer (process-buffer process))
	response)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Append output to buffer
        (goto-char (point-max))
        (insert output)
        (tramp-rpc--debug "FILTER received %d bytes, buffer-size=%d"
                          (length output) (buffer-size))
        ;; Process complete messages
        (goto-char (point-min))
        (while (setq response (tramp-rpc-protocol-try-read-message buffer))
          ;; Replace buffer contents with remaining data
          (delete-region (point-min) (mark-marker))
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
                (puthash id response (tramp-rpc--get-pending-responses buffer))))))))))

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

    ;; Wait for response with matching ID using wall-clock deadline.
    ;; NOTE: We use (float-time) instead of decrementing a counter because
    ;; accept-process-output can return early (e.g. async process output
    ;; arrives), and decrementing by poll-interval each iteration would
    ;; cause premature timeouts when there is concurrent I/O traffic.
    (with-current-buffer buffer
      (let ((start-time (float-time))
            (deadline (+ (float-time) total-timeout))
            response)
        ;; Wait for a response with the correct ID
        (while (and (not response)
                    (< (float-time) deadline)
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
              (keyboard-quit))))

        (unless response
          (let ((elapsed (- (float-time) start-time)))
            (tramp-rpc--debug
             "TIMEOUT id=%s method=%s elapsed=%.1fs buffer-size=%d process-live=%s"
             expected-id method elapsed (buffer-size) (process-live-p process))
            (signal
	     'remote-file-error
	     (list (format
		    "Timeout waiting for RPC response from %s (id=%s, method=%s, waited %.1fs)"
                    (tramp-file-name-host vec) expected-id method elapsed)))))

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
                (signal 'remote-file-error (list "RPC error" msg)))))
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

    ;; Wait for response with matching ID using wall-clock deadline
    (with-current-buffer buffer
      (let ((start-time (float-time))
            (deadline (+ (float-time) 30))
            response)
        (while (and (not response)
                    (< (float-time) deadline)
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
            (if (with-tramp-suspended-timers
                  (with-local-quit
                    (accept-process-output process 0.1 nil t)
                    t))
                ;; Check if our response arrived in pending responses
                (setq response (tramp-rpc--find-response-by-id expected-id))
              (tramp-rpc--debug "QUIT-BATCH id=%s (user interrupted)" expected-id)
              (keyboard-quit))))

        (unless response
          (let ((elapsed (- (float-time) start-time)))
            (tramp-rpc--debug
             "TIMEOUT-BATCH id=%s elapsed=%.1fs buffer-size=%d process-live=%s"
             expected-id elapsed (buffer-size) (process-live-p process))
            (signal
	     'remote-file-error
	     (list (format
		    "Timeout waiting for batch RPC response from %s (id=%s, waited %.1fs)"
		    (tramp-file-name-host vec) expected-id elapsed)))))

        (tramp-rpc--debug "RECV-BATCH id=%s (found)" expected-id)
        (if (tramp-rpc-protocol-error-p response)
            (progn
              (tramp-rpc--debug "ERROR-BATCH id=%s msg=%s"
                               expected-id (tramp-rpc-protocol-error-message response))
              (signal
	       'remote-file-error
	       (list "Batch RPC error"
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
         (deadline (+ (float-time) (or timeout 30)))
         (remaining-ids (copy-sequence ids))
         (responses (make-hash-table :test 'eql)))
    (tramp-rpc--debug "RECV-PIPE waiting for %d responses: %S" (length ids) ids)
    (with-current-buffer buffer
      (while (and remaining-ids
                  (< (float-time) deadline)
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
          (if (with-tramp-suspended-timers
                (with-local-quit
                  (accept-process-output process 0.1 nil t)
                  t))
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
            (keyboard-quit)))))
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
Strips any Emacs file-name quoting (\"/:\") before encoding.
Returns an alist with path."
  `((path . ,(tramp-rpc--path-to-bytes path))))

;; ============================================================================
;; File name handler operations
;; ============================================================================

(defun tramp-rpc-handle-file-executable-p (filename)
  "Like `file-executable-p' for TRAMP-RPC files.
Checks execute permission from `file-attributes' mode string and
the remote uid/gid.  No dedicated RPC call needed.
For symlinks, follows through to the target (like
`tramp-handle-file-readable-p' does)."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-executable-p"
      (when-let* ((attrs (file-attributes filename 'integer)))
        (if (stringp (file-attribute-type attrs))
            ;; Symlink: follow it and check the target.
            (file-executable-p (file-truename filename))
          ;; Regular file or directory: check mode bits.
          (when-let* ((mode-string (file-attribute-modes attrs))
                      (remote-uid (tramp-get-remote-uid v 'integer))
                      (remote-gid (tramp-get-remote-gid v 'integer)))
            (or
             ;; World executable.
             (memq (aref mode-string 9) '(?x ?t))
             ;; Owner executable and we are owner (or root).
             (and (memq (aref mode-string 3) '(?x ?s))
                  (or (equal remote-uid tramp-root-id-integer)
                      (equal remote-uid (file-attribute-user-id attrs))))
             ;; Group executable and we are in that group.
             (and (memq (aref mode-string 6) '(?x ?s))
                  (or (equal remote-gid (file-attribute-group-id attrs))
                      (member (file-attribute-group-id attrs)
                              (tramp-get-remote-groups v 'integer)))))))))))

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
       ;; Return nil for ELOOP (symlink loop) and ENOTDIR (path component
       ;; is not a directory, e.g. "file.py/.editorconfig") - the file
       ;; can't be resolved, so it effectively doesn't exist for stat purposes.
       (if (or (string-match-p "Too many levels of symbolic links" (cadr err))
               (string-match-p "Not a directory" (cadr err)))
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
         (mode (tramp-file-mode-from-int (alist-get 'mode stat)))
         (inode (alist-get 'inode stat))
         (dev (alist-get 'dev stat)))
    ;; Return in file-attributes format
    (list type nlinks
          (if (eq id-format 'string) (or uname (number-to-string uid)) uid)
          (if (eq id-format 'string) (or gname (number-to-string gid)) gid)
          atime mtime ctime
          size mode nil inode dev)))




(defun tramp-rpc-handle-set-file-modes (filename mode &optional _flag)
  "Like `set-file-modes' for TRAMP-RPC files."
  (tramp-skeleton-set-file-modes-times-uid-gid filename
    (tramp-rpc--call v "file.set_modes"
                     (append (tramp-rpc--encode-path localname)
                             `((mode . ,mode))))))

(defun tramp-rpc-handle-set-file-times (filename &optional timestamp _flag)
  "Like `set-file-times' for TRAMP-RPC files."
  (tramp-skeleton-set-file-modes-times-uid-gid filename
    (let ((mtime (floor (float-time (or timestamp (current-time))))))
      (tramp-rpc--call v "file.set_times"
                       (append (tramp-rpc--encode-path localname)
                               `((mtime . ,mtime)))))))


;; ============================================================================
;; High-level operations
;; ============================================================================

(defun tramp-rpc--dir-locals-candidate-files (&optional base-el-only)
  "Return dir-locals candidate file names.
When BASE-EL-ONLY is non-nil, return only `dir-locals-file'."
  (let ((file-1 dir-locals-file)
        (file-2 (and (string-match "\\.el\\'" dir-locals-file)
                     (replace-match "-2.el" t nil dir-locals-file))))
    (if base-el-only
        (list file-1)
      (delq nil (list file-1 file-2)))))

(defun tramp-rpc--quote-localname (original-localname new-localname)
  "Return NEW-LOCALNAME with ORIGINAL-LOCALNAME quoting style.
If ORIGINAL-LOCALNAME is file-name-quoted, quote NEW-LOCALNAME too."
  (if (file-name-quoted-p original-localname)
      (file-name-quote new-localname)
    new-localname))

(defun tramp-rpc--parent-directory (directory)
  "Return parent directory for DIRECTORY, or nil at filesystem root."
  (let* ((current (directory-file-name directory))
         (parent (file-name-directory current)))
    (when parent
      (let ((parent (directory-file-name parent)))
        (unless (equal parent current)
          parent)))))

(defun tramp-rpc--locate-search-directory (path)
  "Return lexical search directory for locate-dominating PATH."
  (if (string-suffix-p "/" path)
      (directory-file-name path)
    (let ((normalized (directory-file-name path)))
      (or (and (file-name-directory normalized)
               (directory-file-name (file-name-directory normalized)))
          normalized))))

(defun tramp-rpc--locate-dominating-before-stop-p (search-path dominating-dir)
  "Return non-nil when DOMINATING-DIR is reachable without crossing stop regexp.
SEARCH-PATH and DOMINATING-DIR must use the same pathname form (remote/local)
that `locate-dominating-stop-dir-regexp' is expected to match."
  (let ((stop locate-dominating-stop-dir-regexp))
    (if (or (null stop) (equal stop ""))
        t
      (let ((current (tramp-rpc--locate-search-directory search-path))
            (target (directory-file-name dominating-dir))
            (blocked nil))
        (while (and current (not blocked) (not (equal current target)))
          (when (string-match-p stop (file-name-as-directory current))
            (setq blocked t))
          (setq current (tramp-rpc--parent-directory current)))
        (and (not blocked)
             (equal current target))))))

(defun tramp-rpc-handle-dir-locals--all-files (directory &optional base-el-only)
  "Like `dir-locals--all-files' for TRAMP-RPC files.
Return readable dir-locals files in DIRECTORY in increasing priority order."
  (with-parsed-tramp-file-name
      (if (file-name-absolute-p directory)
          directory
        (file-name-concat default-directory directory))
      nil
    ;; Unquote file names (e.g. /: prefix) before sending to server.
    (let* ((quoted-localname localname)
           (localdir (directory-file-name (file-name-unquote localname)))
           (names (tramp-rpc--dir-locals-candidate-files base-el-only))
           (result (tramp-rpc--call
                    v "highlevel.test_files_in_dir"
                    `((directory . ,(tramp-rpc--path-to-bytes localdir))
                      (names . ,(vconcat names))))))
      (mapcar (lambda (path)
                (tramp-make-tramp-file-name
                 v
                 (tramp-rpc--quote-localname
                  quoted-localname
                  (tramp-rpc--decode-string path))))
              result))))

(defun tramp-rpc-handle-locate-dominating-file (file name)
  "Like `locate-dominating-file' for TRAMP-RPC files.
For string/list NAME, uses a high-level RPC call.  Predicate NAME falls back
to the built-in implementation."
  (if (functionp name)
      (tramp-run-real-handler #'locate-dominating-file (list file name))
    (with-parsed-tramp-file-name
        (if (file-name-absolute-p file)
            file
          (file-name-concat default-directory file))
        nil
      ;; Unquote file names (e.g. /: prefix) before sending to server.
      (let* ((quoted-localname localname)
             (localname (file-name-unquote localname))
             (names (ensure-list name))
             (result (tramp-rpc--call
                      v "highlevel.locate_dominating_file_multi"
                      `((file . ,(tramp-rpc--path-to-bytes localname))
                        (names . ,(vconcat names))))))
        (when-let* ((marker (car result))
                    (marker-path (tramp-rpc--decode-string marker)))
          (let* ((dominating-dir (file-name-directory marker-path))
                 (search-remote
                  (tramp-make-tramp-file-name
                   v
                   (tramp-rpc--quote-localname quoted-localname localname)))
                 (dominating-remote
                  (tramp-make-tramp-file-name
                   v
                   (tramp-rpc--quote-localname quoted-localname dominating-dir))))
            (when (tramp-rpc--locate-dominating-before-stop-p
                   search-remote dominating-remote)
              dominating-remote)))))))

(defun tramp-rpc--dir-locals-cache-update (file cache)
  "Call RPC helper for `dir-locals-find-file' update using FILE and CACHE."
  (with-parsed-tramp-file-name
      (if (file-name-absolute-p file)
          file
        (file-name-concat default-directory file))
      nil
    ;; Unquote file names (e.g. /: prefix) before sending to server.
    (let* ((localname (file-name-unquote localname))
           (file-connection (file-remote-p file))
           (names (tramp-rpc--dir-locals-candidate-files nil))
           (cache-dirs
            (seq-uniq
             (cl-loop
              for cache-entry in cache
              for cache-dir = (car cache-entry)
              when (string= file-connection (file-remote-p cache-dir))
              collect (file-name-unquote (file-local-name cache-dir))))))
      (tramp-rpc--call
       v "highlevel.dir_locals_find_file_cache_update"
       `((file . ,(tramp-rpc--path-to-bytes localname))
         (names . ,(vconcat names))
         (cache_dirs . ,(vconcat cache-dirs)))))))

(defun tramp-rpc--dir-locals-latest-mtime (files)
  "Return latest mtime from FILES alist data as a Lisp time value."
  (let ((latest 0))
    (dolist (f files latest)
      (let ((f-time (seconds-to-time (alist-get 'mtime f))))
        (when (time-less-p latest f-time)
          (setq latest f-time))))))

(defun tramp-rpc--dir-locals-cache-covers-p (locals-dir cache-dir)
  "Return non-nil when CACHE-DIR is at or below LOCALS-DIR."
  (let ((locals (directory-file-name locals-dir))
        (cache (directory-file-name cache-dir)))
    (or (equal locals cache)
        (file-in-directory-p cache locals))))

(defun tramp-rpc-handle-dir-locals-find-file (file)
  "Like `dir-locals-find-file' for TRAMP-RPC files."
  (let* ((file (if (file-name-absolute-p file)
                   file
                 (file-name-concat default-directory file)))
         (file-connection (file-remote-p file))
         (cache-update (tramp-rpc--dir-locals-cache-update file dir-locals-directory-cache))
         (locals-dir-update (alist-get 'locals cache-update))
         (locals-dir (when locals-dir-update
                       (file-name-as-directory
                        (concat file-connection
                                (tramp-rpc--decode-string
                                 (alist-get 'dir locals-dir-update))))))
         (cache-dir-update (alist-get 'cache cache-update))
         (cache-dir (when cache-dir-update
                      (file-name-as-directory
                       (concat file-connection
                               (tramp-rpc--decode-string
                                (alist-get 'dir cache-dir-update))))))
         (dir-elt (when cache-dir
                    (seq-find (lambda (elt) (string= (car elt) cache-dir))
                              dir-locals-directory-cache))))
    (if (and dir-elt
             (or (null locals-dir)
                 (tramp-rpc--dir-locals-cache-covers-p locals-dir (car dir-elt))))
        ;; Potential cache hit, verify mtimes.
        (if (or (null (nth 2 dir-elt))
                (let ((cached-files (alist-get 'files cache-dir-update)))
                  (and cached-files
                       (time-equal-p
                        (nth 2 dir-elt)
                        (tramp-rpc--dir-locals-latest-mtime cached-files)))))
            dir-elt
          (progn
            ;; Cache entry invalid, clear and return discovered locals dir.
            (setq dir-locals-directory-cache
                  (delq dir-elt dir-locals-directory-cache))
            locals-dir))
      ;; No cache entry.
      locals-dir)))

;; ============================================================================
;; Directory operations
;; ============================================================================

(defun tramp-rpc-handle-directory-files (directory &optional full match nosort count)
  "Like `directory-files' for TRAMP-RPC files."
  (tramp-skeleton-directory-files directory full match nosort count
    (let* ((result (tramp-rpc--call v "dir.list"
                                    (append (tramp-rpc--encode-path localname)
                                            '((include_attrs . :msgpack-false)
                                              (include_hidden . t))))))
      (mapcar #'tramp-rpc--decode-filename result))))

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

;; Declared in Tramp 2.8.1.3+; forward-declare so byte compiler treats it as dynamic.
(defvar tramp-fnac-add-trailing-slash)

(defun tramp-rpc-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for TRAMP-RPC files."
  ;; Suppress check for trailing slash in `tramp-skeleton-file-name-all-completions'.
  (let (tramp-fnac-add-trailing-slash)
    (tramp-skeleton-file-name-all-completions filename directory
      (with-parsed-tramp-file-name (expand-file-name directory) nil
	;; Get all entries in the directory. Convert vector to list if needed.
	(let ((entries
	       (append (tramp-rpc--call v "dir.list"
				       (append (tramp-rpc--encode-path localname)
					       '((include_attrs . :msgpack-false)
                                                 (include_hidden . t))))
		       nil)))
          ;; Build list of names with trailing / for directories
          (mapcar (lambda (entry)
                    (let ((name (tramp-rpc--decode-filename entry))
                          (file-type (alist-get 'type entry)))
                      (if (equal file-type "directory")
                          (concat name "/")
			name)))
                  entries))))))

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
              (when keep-time
                (set-file-times newname (file-attribute-modification-time
                                         (file-attributes filename))))
              (when preserve-permissions
                (set-file-extended-attributes newname (file-extended-attributes
						       filename))))
          (when (file-exists-p tmpfile)
            (delete-file tmpfile)))))
     ;; Local source, remote dest - read locally, write via RPC
     ((and (not source-remote) dest-remote)
      ;; Read local file and write to remote
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally filename)
        (write-region (point-min) (point-max) newname nil 'nomessage))
      (when keep-time
        (set-file-times newname (file-attribute-modification-time
                                 (file-attributes filename))))
      (when preserve-permissions
        (set-file-extended-attributes newname (file-extended-attributes
					       filename))))
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
      (when keep-time
        (set-file-times newname (file-attribute-modification-time
                                 (file-attributes filename))))
      (when preserve-permissions
        (set-file-extended-attributes newname (file-extended-attributes
					      filename))))
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
  (tramp-skeleton-set-file-modes-times-uid-gid filename
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
  (with-parsed-tramp-file-name (expand-file-name (file-name-unquote filename)) nil
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
	      ;; By convention, the result string has a trailing
	      ;; newline.  Don't let tests fail.
	      (concat (string-trim output) "\n"))))))))

(defun tramp-rpc-handle-set-file-acl (filename acl-string)
  "Like `set-file-acl' for TRAMP-RPC files.
Set the ACL of FILENAME to ACL-STRING.
Returns t on success, nil on failure."
  (with-parsed-tramp-file-name (expand-file-name (file-name-unquote filename)) nil
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
  (with-parsed-tramp-file-name (expand-file-name (file-name-unquote filename)) nil
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
  (with-parsed-tramp-file-name (expand-file-name (file-name-unquote filename)) nil
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
               (env (tramp-rpc--ensure-inside-emacs-env
                     ;; Merge: direnv base + caller overrides (e.g. GIT_INDEX_FILE).
                     ;; Caller overrides take priority -- append last so
                     ;; duplicate keys resolve to the caller's value when
                     ;; the server iterates the map.
                     (append (tramp-rpc--get-direnv-environment v localname)
                             (tramp-rpc--caller-environment))))
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
                                              (env . ,env)
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

(defun tramp-rpc-handle-exec-path ()
  "Return remote exec-path using RPC.
Resolves `tramp-rpc-remote-path' by expanding the placeholder
`tramp-rpc-own-remote-path' with the user's actual login shell PATH.
Appends the remote working directory as the last element (the equivalent
of `exec-directory'), matching `tramp-sh-handle-exec-path' behavior.
Caches the PATH portion per connection."
  (with-parsed-tramp-file-name default-directory nil
    (let* ((key (tramp-rpc--connection-key v))
           (cached (gethash key tramp-rpc--exec-path-cache))
           (remote-path (or cached
                            (let ((path (tramp-rpc--compute-remote-path v)))
                              (puthash key path tramp-rpc--exec-path-cache)
                              path))))
      ;; Append localname of default-directory as last element,
      ;; the equivalent to `exec-directory'.
      (append remote-path
              (list (tramp-file-local-name
                     (expand-file-name default-directory)))))))

(defun tramp-rpc--compute-remote-path (vec)
  "Compute the remote exec-path for VEC from `tramp-rpc-remote-path'.
Replaces `tramp-rpc-own-remote-path' with the directories from the
user's login shell PATH.  Removes duplicates."
  (let ((own-path nil)
        (result nil))
    (dolist (entry tramp-rpc-remote-path)
      (if (eq entry 'tramp-rpc-own-remote-path)
          ;; Expand the placeholder: fetch login shell PATH (once)
          (progn
            (unless own-path
              (setq own-path (or (tramp-rpc--fetch-remote-exec-path vec)
                                 '())))
            (dolist (dir own-path)
              (unless (member dir result)
                (push dir result))))
        ;; Literal directory string
        (unless (member entry result)
          (push entry result))))
    (nreverse result)))

(defun tramp-rpc--get-remote-login-shell (vec)
  "Return the login shell for the remote user on VEC.
Tries the `shell' field from system.info (populated via getpwuid on
the server).  Falls back to looking up the user via `getent passwd'
and extracting field 7.  Returns \"/bin/sh\" if all lookups fail.
Result is cached per connection."
  (let* ((key (tramp-rpc--connection-key vec))
         (cached (gethash key tramp-rpc--login-shell-cache)))
    (or cached
        (let ((shell
               (condition-case nil
                   (let* ((info (tramp-rpc--call vec "system.info" nil))
                          (sh (alist-get 'shell info)))
                     (if (and sh (stringp sh) (> (length sh) 0))
                         sh
                       (tramp-rpc--get-remote-login-shell-via-getent vec)))
                 (error (tramp-rpc--get-remote-login-shell-via-getent vec)))))
          (puthash key shell tramp-rpc--login-shell-cache)
          shell))))

(defun tramp-rpc--get-remote-login-shell-via-getent (vec)
  "Look up the login shell for the remote user on VEC via getent.
Returns \"/bin/sh\" if the lookup fails."
  (condition-case nil
      (let* ((user (or (tramp-file-name-user vec) ""))
             ;; If no user in the vec, fall back to system.info user
             (target-user (if (string-empty-p user)
                              (tramp-rpc--decode-string
                               (alist-get 'user
                                          (tramp-rpc--call vec "system.info" nil)))
                            user))
             (result (tramp-rpc--call vec "process.run"
                                       `((cmd . "getent")
                                         (args . ["passwd" ,target-user])
                                         (cwd . "/"))))
             (exit-code (alist-get 'exit_code result))
             (stdout (tramp-rpc--decode-output
                      (alist-get 'stdout result)
                      (alist-get 'stdout_encoding result))))
        (if (and (eq exit-code 0) (> (length stdout) 0))
            ;; getent passwd format: name:x:uid:gid:gecos:home:shell
            (let* ((fields (split-string (string-trim stdout) ":"))
                   (shell (and (>= (length fields) 7) (nth 6 fields))))
              (if (and shell (> (length shell) 0))
                  shell
                "/bin/sh"))
          "/bin/sh"))
    (error "/bin/sh")))

(defun tramp-rpc--fetch-remote-exec-path (vec)
  "Fetch the remote PATH from VEC using the user's login shell.
Invokes the login shell with `-l' to source shell configuration
files, and uses `printenv PATH' for shell-agnostic output (works
correctly with bash, zsh, fish, and other shells)."
  (condition-case nil
      (let* ((shell (tramp-rpc--get-remote-login-shell vec))
             (result (tramp-rpc--call vec "process.run"
                                       `((cmd . ,shell)
                                         (args . ["-l" "-c" "printenv PATH"])
                                         (cwd . "/"))))
             (exit-code (alist-get 'exit_code result))
             (stdout (tramp-rpc--decode-output
                      (alist-get 'stdout result)
                      (alist-get 'stdout_encoding result))))
        (if (and (eq exit-code 0) (> (length stdout) 0))
            (split-string (string-trim stdout) ":" t)
          nil))
    (error nil)))

(defun tramp-rpc-handle-file-local-copy (filename)
  "Create a local copy of remote FILENAME using RPC."
  (tramp-skeleton-file-local-copy filename
    (let* ((params (tramp-rpc--file-read-params localname))
           (result (tramp-rpc--call v "file.read" params))
           (content (tramp-rpc--extract-file-read-content result)))
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
If GROUP is non-nil, also check that group would be preserved.
Uses cached `file-attributes' and connection-cached remote uid/gid,
so this typically requires no RPC calls."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property
        v localname
        (format "file-ownership-preserved-p%s" (if group "-group" ""))
      (let ((attributes (file-attributes filename 'integer)))
        ;; Return t if the file doesn't exist, since it's true that no
        ;; information would be lost by an (attempted) delete and create.
        (or (null attributes)
            (and
             (= (file-attribute-user-id attributes)
                (tramp-get-remote-uid v 'integer))
             (or (not group)
                 ;; On BSD-derived systems files always inherit the
                 ;; parent directory's group, so skip the group-gid test.
                 (tramp-check-remote-uname v tramp-bsd-unames)
                 (= (file-attribute-group-id attributes)
                    (tramp-get-remote-gid v 'integer)))))))))

(defun tramp-rpc-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for TRAMP-RPC files.
Delegates to `tramp-handle-expand-file-name'.  If tilde expansion
fails because the connection is not available (e.g. during
`tramp-cleanup-all-connections'), retries with `tramp-tolerate-tilde'
so the path is returned with the tilde unexpanded rather than
signalling an error.
`tramp-verbose' is suppressed during the first attempt because
`tramp-error' logs a level-1 message before signalling, which
would otherwise flood the echo area with \"Cannot expand tilde\"."
  ;; The generic `tramp-handle-expand-file-name' defaults non-absolute
  ;; localnames to "/" (root), but the ssh handler
  ;; (`tramp-sh-handle-expand-file-name') defaults to "~/" instead.
  ;; Match that behavior: empty localnames get "~", and non-absolute
  ;; localnames (e.g. ".config/") get "~/" prepended so they resolve
  ;; relative to the home directory rather than the filesystem root.
  ;; Guard with `tramp-connectable-p' so that the tilde substitution is
  ;; skipped during completion when no connection exists, avoiding a
  ;; blocking connection attempt when `non-essential' is t.  When not
  ;; connectable the generic handler falls through to "/" (root) rather
  ;; than the home directory — acceptable for the completion case.
  ;; Use `tramp-dissect-file-name' and `tramp-make-tramp-file-name'
  ;; instead of `file-remote-p' to avoid re-entering expand-file-name.
  (when (tramp-tramp-file-p name)
    (let ((v (tramp-dissect-file-name name)))
      (when (tramp-connectable-p v)
        (let ((localname (tramp-file-name-localname v)))
          (cond
           ;; Empty localname (e.g. "/rpc:host:") -> expand to home.
           ((tramp-string-empty-or-nil-p localname)
            (setq name (tramp-make-tramp-file-name v "~")))
           ;; Non-absolute localname (e.g. ".config/") -> make relative
           ;; to home, matching tramp-sh-handle-expand-file-name behavior.
           ;; Without this, the generic handler prepends "/" (root).
           ((not (tramp-run-real-handler
                  #'file-name-absolute-p (list localname)))
            (setq name (tramp-make-tramp-file-name
                        v (concat "~/" localname)))))))))
  (condition-case nil
      (let ((tramp-verbose 0))
        (tramp-handle-expand-file-name name dir))
    (file-error
     (let ((tramp-tolerate-tilde t))
       (tramp-handle-expand-file-name name dir)))))

;; ============================================================================
;; Process and advice modules (extracted)
;; ============================================================================

(defvar tramp-rpc--delivering-output nil
  "Non-nil while delivering process output to the local relay.
Used by advice functions to bypass interception during output delivery.")

(defvar tramp-rpc--closing-local-relay nil
  "Non-nil while sending EOF to a local cat relay process.
Tells the `process-send-eof' advice to call the original function
instead of routing to the remote process.")

(defcustom tramp-rpc-async-read-timeout-ms 200
  "Timeout in milliseconds for async process reads.
The server will block for this long waiting for data before returning.
Lower values mean more responsive but higher CPU usage.
Also controls process exit detection latency."
  :type 'integer
  :group 'tramp-rpc)

;; Process support, advice functions, and magit integration are now in
;; separate modules for better organization and maintainability.
(require 'tramp-rpc-process)
;; Loading tramp-rpc-advice while this file is being byte-compiled can
;; recurse on some Emacs/TRAMP combinations.  Advice is still loaded at
;; runtime when `tramp-rpc' is required normally.
(unless (bound-and-true-p byte-compile-current-file)
  (require 'tramp-rpc-advice))
(require 'tramp-rpc-magit)

;; ============================================================================
;; File name handler registration
;; ============================================================================

(defconst tramp-rpc-file-name-handler-alist
  '(;; =========================================================================
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
    (locate-dominating-file . tramp-rpc-handle-locate-dominating-file)
    (dir-locals--all-files . tramp-rpc-handle-dir-locals--all-files)
    (dir-locals-find-file . tramp-rpc-handle-dir-locals-find-file)
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

;; Defer registration until tramp-rpc is fully loaded so
;; `tramp-add-external-operation' can safely `(require 'tramp-rpc)'.
(with-eval-after-load 'tramp-rpc
  (tramp-add-external-operation 'locate-dominating-file 'tramp-rpc-handle-locate-dominating-file 'tramp-rpc)
  (tramp-add-external-operation 'dir-locals--all-files 'tramp-rpc-handle-dir-locals--all-files 'tramp-rpc)
  (tramp-add-external-operation 'dir-locals-find-file 'tramp-rpc-handle-dir-locals-find-file 'tramp-rpc))

;;;###autoload
(defun tramp-rpc-file-name-handler (operation &rest args)
  "Invoke TRAMP-RPC file name handler for OPERATION with ARGS.
Falls back to the local handler when `non-essential' is non-nil and
a backend function throws `non-essential' (e.g. because no connection
exists and opening one would block).  This mirrors the catch/throw
pattern in `tramp-file-name-handler'."
  ;; `file-remote-p' is called for everything, even for symbolic
  ;; links which look remote.  We don't want to get an error.
  (let ((non-essential (or non-essential (eq operation 'file-remote-p))))
    (if-let* ((handler (assq operation tramp-rpc-file-name-handler-alist)))
        (let ((result (catch 'non-essential
                        (save-match-data (apply (cdr handler) args)))))
          (if (eq result 'non-essential)
              (tramp-run-real-handler operation args)
            result))
      (tramp-run-real-handler operation args))))

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

(defun tramp-rpc--multi-hop-advice-remove ()
  "Remove multi-hop advice."
  (advice-remove 'tramp-multi-hop-p #'tramp-rpc--multi-hop-advice))

;; ============================================================================
;; Recentf integration
;; ============================================================================



;; ============================================================================
;; Connection cleanup support
;; ============================================================================

(defun tramp-rpc-cleanup-connection (vec)
  "Clean up TRAMP-RPC resources for connection VEC.
This is called from `tramp-cleanup-connection-hook' after TRAMP's
generic cleanup has already run (passwords cleared, timers cancelled,
connection buffer killed, TRAMP caches flushed).

Handles RPC-specific state: the connection hash table, async/PTY
processes, file watches, ControlMaster process/socket, pending RPC
responses, and RPC-specific caches (direnv, executable, file-exists,
file-truename)."
  (when (tramp-rpc-file-name-p vec)
    ;; Save buffer reference before disconnect removes the connection
    ;; entry.  The buffer is already killed by TRAMP's generic cleanup,
    ;; but we need the object to remove its pending-responses hash entry.
    (let ((conn-buffer (when-let* ((conn (tramp-rpc--get-connection vec)))
                         (plist-get conn :buffer))))
      ;; Delegate to disconnect for the common cleanup: async/PTY
      ;; processes, watches, connection hash, executable cache.
      ;; The redundant tramp-flush-* calls in disconnect are harmless.
      (tramp-rpc--disconnect vec)
      ;; Clean up pending responses keyed by the (now-dead) buffer.
      (when conn-buffer
        (remhash conn-buffer tramp-rpc--pending-responses)))
    ;; Clear RPC-specific caches for this connection.
    (tramp-rpc--clear-direnv-cache vec)
    (tramp-rpc--clear-file-caches-for-connection vec)
    ;; Clean up ControlMaster SSH process and socket.
    (tramp-rpc--cleanup-controlmaster vec)
    ;; Note: recentf cleanup is handled by `tramp-recentf-cleanup' from
    ;; tramp-integration.el, which is registered on the same
    ;; `tramp-cleanup-connection-hook'.
    ))

(defun tramp-rpc-cleanup-all-connections ()
  "Clean up all TRAMP-RPC connections.
Called from `tramp-cleanup-all-connections-hook' after TRAMP's generic
cleanup of all connections has run."
  ;; Collect vecs before clearing connections hash so we can close
  ;; their ControlMaster sockets afterward.
  (let ((vecs nil))
    (maphash (lambda (_key conn)
               (when-let* ((proc (plist-get conn :process))
                           (v (process-get proc :tramp-rpc-vec)))
                 (push v vecs)))
             tramp-rpc--connections)
    ;; Clean up all async and PTY processes (no vec = all connections).
    (tramp-rpc--cleanup-async-processes)
    (tramp-rpc--cleanup-pty-processes)
    ;; Clean up all filesystem watches.
    (clrhash tramp-rpc--watched-directories)
    ;; Kill any remaining RPC server processes and clear connections hash.
    (maphash (lambda (_key conn)
               (let ((process (plist-get conn :process)))
                 (when (process-live-p process)
                   (delete-process process))))
             tramp-rpc--connections)
    (clrhash tramp-rpc--connections)
    ;; Close ControlMaster sockets and kill auth processes/buffers.
    (dolist (vec vecs)
      (tramp-rpc--cleanup-controlmaster vec))
    ;; Also kill any orphaned auth buffers not associated with a
    ;; tracked connection (e.g. from a failed connection attempt).
    (dolist (buf (buffer-list))
      (when (string-match-p "\\` \\*tramp-rpc-auth " (buffer-name buf))
        (when-let* ((proc (get-buffer-process buf)))
          (when (process-live-p proc)
            (delete-process proc)))
        (kill-buffer buf))))
  ;; Clear all RPC-specific caches.
  (clrhash tramp-rpc--pending-responses)
  (clrhash tramp-rpc--async-callbacks)
  (clrhash tramp-rpc--executable-cache)
  (tramp-rpc--clear-direnv-cache)
  (tramp-rpc-clear-file-exists-cache)
  (tramp-rpc-clear-file-truename-cache)
  ;; Note: recentf cleanup is handled by `tramp-recentf-cleanup-all'
  ;; from tramp-integration.el, registered on the same
  ;; `tramp-cleanup-all-connections-hook'.
  )

;; Register cleanup hooks.
(add-hook 'tramp-cleanup-connection-hook #'tramp-rpc-cleanup-connection)
(add-hook 'tramp-cleanup-all-connections-hook #'tramp-rpc-cleanup-all-connections)

;; ============================================================================
;; Unload support
;; ============================================================================

(defun tramp-rpc-unload-function ()
  "Unload function for tramp-rpc.
Removes advice and cleans up async processes."
  ;; Remove high-level external operations from tramp-rpc core.
  (tramp-remove-external-operation 'locate-dominating-file 'tramp-rpc)
  (tramp-remove-external-operation 'dir-locals--all-files 'tramp-rpc)
  (tramp-remove-external-operation 'dir-locals-find-file 'tramp-rpc)
  ;; Remove all advice (from tramp-rpc-advice module)
  (tramp-rpc-advice-remove)
  ;; Remove legacy multi-hop advice and cleanup hooks.
  (advice-remove 'tramp-multi-hop-p #'tramp-rpc--multi-hop-advice)
  (remove-hook 'tramp-cleanup-connection-hook #'tramp-rpc-cleanup-connection)
  (remove-hook 'tramp-cleanup-all-connections-hook #'tramp-rpc-cleanup-all-connections)
  ;; Clean up all async processes (from tramp-rpc-process module)
  (tramp-rpc--cleanup-async-processes)
  ;; Clean up PTY processes (from tramp-rpc-process module)
  (tramp-rpc--cleanup-pty-processes)
  ;; Remove method registrations.
  (setq tramp-methods (delete (assoc tramp-rpc-method tramp-methods) tramp-methods))
  (setq tramp-foreign-file-name-handler-alist
	(delete (assoc 'tramp-rpc--sudo-file-name-p
			tramp-foreign-file-name-handler-alist)
		tramp-foreign-file-name-handler-alist))
  (setq tramp-foreign-file-name-handler-alist
	(delete (assoc 'tramp-rpc-file-name-p
			tramp-foreign-file-name-handler-alist)
		tramp-foreign-file-name-handler-alist))
  ;; Return nil to allow normal unload to proceed
  nil)

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-rpc 'force)))

(provide 'tramp-rpc)
;;; tramp-rpc.el ends here
