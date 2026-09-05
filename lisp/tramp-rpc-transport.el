;;; tramp-rpc-transport.el --- RPC transport layer for TRAMP-RPC -*- lexical-binding: t; -*-

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

;; This file owns everything needed to reach a remote RPC server and talk
;; to it, independent of any TRAMP file operation:
;; - Hop-chain analysis, including sudo-via-RPC privilege elevation
;; - SSH ControlMaster and multi-hop (ProxyJump) setup, authentication
;;   through `tramp-process-actions', and server process startup
;; - Connection generations: lookup, replacement, and cleanup
;; - The RPC call primitives: synchronous calls, batches, pipelines and
;;   asynchronous calls with callbacks
;; - The effective environment handed to remote processes (PATH, direnv)
;;
;; Every other tramp-rpc module builds on this one.  It must not depend
;; on the file handlers in tramp-rpc.el; the few callbacks it needs from
;; higher layers are declared below.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'tramp)
(require 'tramp-sh)
(require 'tramp-rpc-protocol)
(require 'tramp-rpc-connection)
(require 'tramp-rpc-hops)
(require 'tramp-rpc-deploy)

;; Emitted inside the autoload form in tramp-rpc.el.
(defvar tramp-rpc-method)
(declare-function tramp-rpc--sudo-file-name-p "tramp-rpc")

;; ============================================================================
;; Hooks into higher layers
;; ============================================================================

;; The transport does not know about caches, relays, watches or file
;; notifications.  Higher modules register on these hooks at load time; the
;; order in which tramp-rpc.el requires them fixes the call order.

(defvar tramp-rpc-connection-invalidate-functions nil
  "Functions called with VEC when VEC's connection state must be forgotten.
Run before a new generation becomes visible and when a generation is
retired, so caches keyed by the TRAMP connection spelling can be dropped.")

(defvar tramp-rpc-transport-terminate-functions nil
  "Functions called with VEC, PROCESS and CONNECTION before a transport dies.
The transport PROCESS is still live and CONNECTION is its generation, so
functions may still send final RPCs through it, for example to kill
remote children.")

(defvar tramp-rpc-transport-cleanup-functions nil
  "Functions called with VEC and PROCESS once a transport generation is dead.
No RPC can be sent any more; functions remove local state tied to PROCESS.")

(defvar tramp-rpc-notification-functions nil
  "Functions called with PROCESS, METHOD and PARAMS for server notifications.
METHOD is the notification name, for example \"fs.events\", and PARAMS its
decoded parameters.  Notifications nobody handles are discarded.")

(define-error 'tramp-rpc-server-unavailable
  "TRAMP-RPC server binary is unavailable" 'remote-file-error)

(defcustom tramp-rpc-call-timeout 30
  "Maximum seconds to wait for a synchronous RPC call to complete.
The value must be a positive number."
  :type 'number
  :group 'tramp-rpc)

(defcustom tramp-rpc-poll-interval 0.1
  "Seconds between synchronous RPC response polls.
The value must be a positive number."
  :type 'number
  :group 'tramp-rpc)

(defun tramp-rpc--configured-call-timeout ()
  "Return the validated synchronous RPC call timeout."
  (unless (and (numberp tramp-rpc-call-timeout)
               (> tramp-rpc-call-timeout 0))
    (user-error "`tramp-rpc-call-timeout' must be a positive number"))
  tramp-rpc-call-timeout)

(defun tramp-rpc--configured-poll-interval ()
  "Return the validated synchronous RPC poll interval."
  (unless (and (numberp tramp-rpc-poll-interval)
               (> tramp-rpc-poll-interval 0))
    (user-error "`tramp-rpc-poll-interval' must be a positive number"))
  tramp-rpc-poll-interval)

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

(defvar tramp-rpc--owned-controlmasters (make-hash-table :test 'equal)
  "Map owned ControlMaster socket paths to their establishing processes.
An entry remains valid after its process exits because `ControlPersist' can
leave the master running in the background.")

(defcustom tramp-rpc-controlmaster-persist 600
  "How long (in seconds) to keep ControlMaster connections alive.
Set to 0 to close immediately when last connection exits.
Set to \"yes\" to keep alive indefinitely."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Indefinitely" "yes"))
  :group 'tramp-rpc)

(defcustom tramp-rpc-server-alive-interval 30
  "SSH ServerAliveInterval in seconds for RPC connections, or nil to disable.
Server-pushed process notifications generate no traffic while remote processes
are idle, so keepalives prevent firewalls and NAT routers from silently
discarding the connection."
  :type '(choice (integer :tag "Interval (seconds)")
                 (const :tag "Disabled" nil))
  :group 'tramp-rpc)

(defcustom tramp-rpc-server-alive-count-max 3
  "Number of unanswered SSH keepalives before an RPC connection is dead."
  :type 'integer
  :group 'tramp-rpc)

(defun tramp-rpc--server-alive-args ()
  "Return SSH keepalive arguments configured for RPC connections."
  (when tramp-rpc-server-alive-interval
    (list "-o" (format "ServerAliveInterval=%d"
                       tramp-rpc-server-alive-interval)
          "-o" (format "ServerAliveCountMax=%d"
                       tramp-rpc-server-alive-count-max))))

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
When non-nil, interactive terminal processes (`vterm', `shell-mode',
`term-mode') use a direct SSH connection with `-t` for the PTY.  This provides
much lower latency than the RPC-based PTY.  The SSH connection reuses the
existing ControlMaster socket, so authentication is already handled.

Note: `signal-process' on direct SSH PTY sends signal to the local SSH
process, which may not propagate to the remote process in all cases."
  :type 'boolean
  :group 'tramp-rpc)

(defconst tramp-rpc-own-remote-path 'tramp-rpc-own-remote-path
  "Deprecated placeholder in `tramp-rpc-remote-path'.
Use TRAMP's `tramp-own-remote-path' in `tramp-remote-path' instead.
This symbol is still accepted for backward compatibility and is treated
like `tramp-own-remote-path'.")

(defcustom tramp-rpc-remote-path nil
  "Deprecated tramp-rpc-specific remote executable search path.
When nil, tramp-rpc uses TRAMP's standard `tramp-remote-path'.  When
non-nil, this value overrides `tramp-remote-path' for compatibility with
older tramp-rpc configurations.

Prefer customizing `tramp-remote-path'.  This compatibility variable
accepts directory strings plus the standard TRAMP placeholders
`tramp-default-remote-path' and `tramp-own-remote-path'.  The old
tramp-rpc placeholder `tramp-rpc-own-remote-path' is also accepted and is
treated like `tramp-own-remote-path'."
  :type '(choice
          (const :tag "Use `tramp-remote-path'" nil)
          (repeat :tag "Compatibility override"
                  (choice (string :tag "Directory")
                          (const :tag "Default Directories" tramp-default-remote-path)
                          (const :tag "Private Directories" tramp-own-remote-path)
                          (const :tag "Deprecated tramp-rpc private directories"
                                 tramp-rpc-own-remote-path))))
  :group 'tramp-rpc)


;; ============================================================================
;; Connection management
;; ============================================================================

(defvar tramp-rpc--connections (make-hash-table :test 'equal)
  "Hash table mapping normalized connection keys to `tramp-rpc-connection'.
Keys include target method/user/host/port plus the effective route (explicit
or hidden TRAMP ad-hoc proxy hops).")

(defvar tramp-rpc--connection-lifecycle-mutexes (make-hash-table :test 'equal)
  "Mutexes serializing connection replacement and ControlMaster teardown.")

;; tramp-rpc--async-processes and tramp-rpc--pty-processes are defined in
;; tramp-rpc-process.el (loaded via require below).  Per-request state
;; (pending IDs, buffered responses, async callbacks) lives on each
;; `tramp-rpc-connection' generation; see tramp-rpc-connection.el.

(defvar tramp-rpc--process-timer-recorder nil
  "Function called for process timers created during a suspended RPC wait.")

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
environment variables.  Set to nil to disable for better performance."
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
                      (alist-get 'stdout result))))
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

;; Forward-declare caches used by tramp-rpc--remove-connection (defined
;; later in the exec-path section).  The byte-compiler needs to see
;; these defvars before their first reference.
(defvar tramp-rpc--exec-path-cache (make-hash-table :test 'equal)
  "Cache of remote variable `exec-path' keyed by connection-key.")

(defvar tramp-rpc--login-shell-cache (make-hash-table :test 'equal)
  "Cache of remote login shell keyed by connection-key.")

(defconst tramp-rpc--generic-route-connection-properties
  '("uname" "uid-integer" "uid-string" "gid-integer" "gid-string" "~")
  "Generic TRAMP properties populated by tramp-rpc that depend on the route.
Home-directory properties named ~USER are route-sensitive as well.")

(defconst tramp-rpc--owned-route-connection-properties
  '("tramp-rpc-login-path" "rpc-signal-strings"
    " rpc-acl-enabled" " rpc-selinux-enabled" "tramp-rpc-system-info")
  "Project-specific route-aware TRAMP connection properties.")

(defvar tramp-rpc--route-property-access nil
  "Non-nil while accessing an explicitly route-qualified TRAMP property.")

(defun tramp-rpc--generic-route-connection-property-p (vec property)
  "Return non-nil when PROPERTY on VEC needs route-aware TRAMP storage."
  (and (tramp-file-name-p vec)
       (or (string= (tramp-file-name-method vec) tramp-rpc-method)
           ;; The sudo predicate consults TRAMP method/connection metadata.
           ;; Suppress this advice during that probe to avoid recursive
           ;; property qualification.
           (let ((tramp-rpc--route-property-access t))
             (tramp-rpc--sudo-file-name-p vec)))
       (stringp property)
       (or (member property tramp-rpc--generic-route-connection-properties)
           (string-prefix-p "~" property))))

(defun tramp-rpc--route-property-name (vec property)
  "Return route-aware TRAMP connection PROPERTY name for VEC."
  (format "%s:%s" property
          (secure-hash 'sha1 (prin1-to-string (tramp-rpc--connection-key vec)))))

(defun tramp-rpc--route-generic-connection-property-advice
    (original vec property &rest args)
  "Call ORIGINAL with route-aware PROPERTY when VEC is an RPC target.
ARGS contains the remaining arguments of the advised TRAMP property function."
  (apply original vec
         (if (and (not tramp-rpc--route-property-access)
                  (tramp-rpc--generic-route-connection-property-p vec property))
             (tramp-rpc--route-property-name vec property)
           property)
         args))

(defmacro tramp-rpc--with-route-connection-property (vec property &rest body)
  "Evaluate BODY once and cache it under route-aware PROPERTY for VEC."
  (declare (indent 2) (debug t))
  (let ((cached-vec (make-symbol "vec"))
        (cached-property (make-symbol "property"))
        (missing (make-symbol "missing"))
        (value (make-symbol "value")))
    `(let* ((,cached-vec ,vec)
            (,cached-property ,property)
            (,missing (make-symbol "missing"))
            (,value (tramp-rpc--get-route-connection-property
                     ,cached-vec ,cached-property ,missing)))
       (if (eq ,value ,missing)
           (let ((,value (progn ,@body)))
             (tramp-rpc--set-route-connection-property
              ,cached-vec ,cached-property ,value)
             ,value)
         ,value))))

(defun tramp-rpc--get-route-connection-property (vec property default)
  "Return route-aware connection PROPERTY for VEC, or DEFAULT."
  (let ((tramp-rpc--route-property-access t))
    (tramp-get-connection-property
     vec (tramp-rpc--route-property-name vec property) default)))

(defun tramp-rpc--set-route-connection-property (vec property value)
  "Set route-aware connection PROPERTY for VEC to VALUE."
  (let ((tramp-rpc--route-property-access t))
    (tramp-set-connection-property
     vec (tramp-rpc--route-property-name vec property) value)))

(defun tramp-rpc--flush-route-connection-property (vec property)
  "Flush route-aware connection PROPERTY for VEC."
  (let ((tramp-rpc--route-property-access t))
    (tramp-flush-connection-property
     vec (tramp-rpc--route-property-name vec property))))

(defun tramp-rpc--flush-owned-route-connection-properties (vec)
  "Flush every route-dependent connection property owned for VEC."
  (dolist (property (append tramp-rpc--owned-route-connection-properties
                            tramp-rpc--generic-route-connection-properties
                            (when-let* ((user (tramp-file-name-user vec)))
                              (unless (string-empty-p user)
                                (list (concat "~" user))))))
    (tramp-rpc--flush-route-connection-property vec property)))

(defun tramp-rpc--environment-with (env key value)
  "Return ENV with KEY set to VALUE.
ENV is an alist of (KEY . VALUE) string pairs.  If KEY already exists,
its value is replaced in-place in the returned list; otherwise a new
entry is appended."
  (if-let* ((cell (assoc key env)))
      (progn
        (setcdr cell value)
        env)
    (append env (list (cons key value)))))

(defun tramp-rpc--ensure-inside-emacs-env (env)
  "Ensure INSIDE_EMACS is set in environment alist ENV.
ENV is an alist of (KEY . VALUE) string pairs, or nil.
If INSIDE_EMACS is not already present, it is added with the value
from `tramp-inside-emacs'.  Returns the (possibly augmented) alist."
  (if-let* ((ie (cdr (assoc "INSIDE_EMACS" env)))
	    ((string-match-p "tramp" ie)))
      env
    (tramp-rpc--environment-with env "INSIDE_EMACS" (tramp-inside-emacs))))

(defun tramp-rpc--merge-environments (&rest environments)
  "Merge ENVIRONMENTS alists with later entries overriding earlier ones.
Duplicate variable names are removed before the alist is sent over RPC.
This avoids relying on duplicate MessagePack map key ordering on the Rust
server side."
  (let (merged)
    (dolist (env environments)
      (dolist (pair env)
        (when (and (consp pair)
                   (stringp (car pair))
                   (stringp (cdr pair)))
          (setq merged
                (tramp-rpc--environment-with merged (car pair) (cdr pair))))))
    merged))

(defun tramp-rpc--cached-remote-path (vec)
  "Return cached remote PATH directories for VEC, computing them if needed."
  (let* ((key (tramp-rpc--connection-key vec))
         (cached (gethash key tramp-rpc--exec-path-cache)))
    (or cached
        (let ((path (tramp-rpc--compute-remote-path vec)))
          (puthash key path tramp-rpc--exec-path-cache)
          path))))

(defun tramp-rpc--remote-path-environment (vec)
  "Return the configured PATH environment entry for VEC.
Uses `tramp-remote-path' by default.  A non-nil deprecated
`tramp-rpc-remote-path' overrides it for compatibility."
  (let ((remote-path (tramp-rpc--cached-remote-path vec)))
    (when remote-path
      `(("PATH" . ,(mapconcat #'identity remote-path ":"))))))

(defun tramp-rpc--cached-login-path (vec)
  "Return the login shell PATH directories for VEC, caching the result."
  (tramp-rpc--with-route-connection-property vec "tramp-rpc-login-path"
    (or (tramp-rpc--fetch-remote-exec-path vec) '())))

(defun tramp-rpc--process-path-environment (vec)
  "Return the PATH entry used for shell child processes on VEC.
Configured `tramp-remote-path' entries keep precedence.  Append missing
entries from the remote login shell PATH so shell commands behave like
`tramp-sh', whose persistent login shell retains its own PATH."
  (let ((path (copy-sequence (tramp-rpc--cached-remote-path vec))))
    (dolist (entry (tramp-rpc--cached-login-path vec))
      (unless (member entry path)
        (setq path (append path (list entry)))))
    (when path
      `(("PATH" . ,(mapconcat #'identity path ":"))))))

(defvar tramp-remote-process-environment)

(defun tramp-rpc--environment-list-to-alist (environment)
  "Convert ENVIRONMENT strings of the form NAME=VALUE to an alist.
Entries without an equals sign are ignored because they request unsetting a
variable in Emacs process APIs, while tramp-rpc sends an explicit environment
map to the remote server."
  (let (alist)
    (dolist (elt environment)
      (when (and (stringp elt)
                 (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" elt))
        (push (cons (match-string 1 elt) (match-string 2 elt)) alist)))
    (nreverse alist)))

(defun tramp-rpc--tramp-remote-process-environment ()
  "Return dynamic `tramp-remote-process-environment' entries as an alist.
This includes dynamic bindings made by packages such as python.el for remote
processes.  The baseline `tramp-remote-process-environment' entries are shell
setup snippets for tramp-sh (for example, LC_CTYPE set to two single quotes),
not a direct exec environment.  Do not pass those defaults to RPC child
processes, because they can leak shell-only quoting and produce warnings on
stdout/stderr."
  (when (boundp 'tramp-remote-process-environment)
    (let ((baseline (default-toplevel-value 'tramp-remote-process-environment))
          dynamic)
      (dolist (elt tramp-remote-process-environment)
        (unless (member elt baseline)
          (push elt dynamic)))
      (tramp-rpc--environment-list-to-alist (nreverse dynamic)))))

(defun tramp-rpc--emacsclient-tramp-environment (vec)
  "Return an EMACSCLIENT_TRAMP entry for VEC.
Depends on `tramp-propagate-emacsclient-tramp' being non-nil."
  ;; `tramp-propagate-emacsclient-tramp' exists since Tramp 2.8.1.5.
  (when (bound-and-true-p tramp-propagate-emacsclient-tramp)
    `(("EMACSCLIENT_TRAMP" . ,(tramp-make-tramp-file-name vec 'noloc)))))

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

(defun tramp-rpc--process-environment (vec localname &optional login-path)
  "Return the effective child environment for LOCALNAME on VEC.
The configured remote PATH is the baseline.  When LOGIN-PATH is non-nil,
append entries from the remote login shell PATH; this matches `tramp-sh' for
commands run through `shell-file-name'.  Dynamic TRAMP environment,
EMACSCLIENT_TRAMP, direnv, and caller overrides are merged in that order, so
later and more specific values replace earlier ones."
  (tramp-rpc--ensure-inside-emacs-env
   (tramp-rpc--merge-environments
    (if login-path
        (tramp-rpc--process-path-environment vec)
      (tramp-rpc--remote-path-environment vec))
    (tramp-rpc--tramp-remote-process-environment)
    (tramp-rpc--emacsclient-tramp-environment vec)
    (tramp-rpc--get-direnv-environment vec localname)
    (tramp-rpc--caller-environment))))

(defun tramp-rpc--connection-key-route-hop (hop-vec)
  "Return normalized route identity for HOP-VEC."
  (list (tramp-rpc--hop-component-string (tramp-file-name-method hop-vec))
        (tramp-rpc--hop-component-string (tramp-file-name-user hop-vec))
        (tramp-rpc--hop-component-string (tramp-file-name-domain hop-vec))
        (tramp-rpc--hop-component-string (tramp-file-name-host hop-vec))
        (or (tramp-rpc--port-to-string (tramp-file-name-port hop-vec)) "22")))

(defun tramp-rpc--connection-key (vec)
  "Generate a connection key for VEC.
Includes the effective hop chain, including hidden TRAMP ad-hoc proxy hops, so
that different multi-hop routes to the same host produce distinct connections.
Hidden and explicit rpc+sudo spellings for the same route share a key, while a
direct root rpc connection does not collide with sudo over an unprivileged rpc
hop."
  (list :method (tramp-rpc--hop-component-string (tramp-file-name-method vec))
        :host (tramp-rpc--hop-component-string (tramp-file-name-host vec))
        :user (tramp-rpc--hop-component-string (tramp-file-name-user vec))
        :port (or (tramp-rpc--port-to-string (tramp-file-name-port vec)) "22")
        :route (mapcar (lambda (hop)
                         (tramp-rpc--connection-key-route-hop (cdr hop)))
                       (tramp-rpc--hop-pairs vec))))

(defun tramp-rpc--get-connection (vec)
  "Get the RPC connection for VEC, or nil if not connected."
  (gethash (tramp-rpc--connection-key vec) tramp-rpc--connections))

(defun tramp-rpc--connection-lifecycle-mutex (vec)
  "Return the connection lifecycle mutex for VEC."
  (let ((key (tramp-rpc--connection-key vec)))
    (or (gethash key tramp-rpc--connection-lifecycle-mutexes)
        (puthash key (make-mutex "tramp-rpc connection lifecycle")
                 tramp-rpc--connection-lifecycle-mutexes))))

(defun tramp-rpc--set-connection (vec process buffer &optional stderr-buffer)
  "Store the RPC connection for VEC.
Caches keyed only by the TRAMP connection spelling are invalidated before a
new generation is made visible.
PROCESS is the RPC transport process.
BUFFER receives the RPC response stream.
STDERR-BUFFER receives the transport's standard error output."
  (tramp-rpc--clear-direnv-cache vec)
  (run-hook-with-args 'tramp-rpc-connection-invalidate-functions vec)
  (let ((connection
         (tramp-rpc--make-connection
          :process process :buffer buffer :stderr-buffer stderr-buffer
          :vec vec)))
    ;; Retain the exact generation and vector on its transport so late cleanup
    ;; cannot accidentally route RPCs through a replacement connection.
    (tramp-rpc--attach-connection connection)
    (puthash (tramp-rpc--connection-key vec) connection
             tramp-rpc--connections)))

(defun tramp-rpc--remove-connection (vec &optional process)
  "Remove VEC's connection, optionally only when PROCESS is current.
Also clears the executable, variable `exec-path', and login-shell caches."
  (let* ((key (tramp-rpc--connection-key vec))
         (current (gethash key tramp-rpc--connections)))
    (when (and current
               (or (null process) (eq process (tramp-rpc-connection-process current))))
      (when-let* ((transport (tramp-rpc-connection-process current)))
        (tramp-rpc-protocol--clear-deferred-polls-for-target transport))
      (remhash key tramp-rpc--connections)
      (tramp-rpc--flush-owned-route-connection-properties vec)
      (remhash key tramp-rpc--exec-path-cache)
      (remhash key tramp-rpc--login-shell-cache))))

(defun tramp-rpc--connection-error-response (vec event)
  "Return an RPC error response for a transport failure on VEC.
EVENT is the process event string."
  (list :error
        (list :code -32098
              :type 'remote-file-error
              :message (format "RPC transport closed for %s%s"
                               (tramp-file-name-host vec)
                               (if event (format " (%s)" (string-trim event)) "")))))

(defun tramp-rpc--track-pending-request (conn id)
  "Record synchronous request ID ID as pending on generation CONN."
  (setf (tramp-rpc-connection-pending-ids conn)
        (cons id (delete id (tramp-rpc-connection-pending-ids conn)))))

(defun tramp-rpc--release-pending-requests (conn ids)
  "Untrack IDS on generation CONN and drop their buffered responses.
This is idempotent so it can run from every synchronous wait exit path."
  (setf (tramp-rpc-connection-pending-ids conn)
        (seq-remove (lambda (id) (memql id ids))
                    (tramp-rpc-connection-pending-ids conn)))
  (dolist (id ids)
    (remhash id (tramp-rpc-connection-pending-responses conn))))

(defmacro tramp-rpc--with-pending-requests (spec &rest body)
  "Run BODY, releasing unresolved request IDS on every exit.
SPEC is (CONN IDS [VEC EVENT]).  When it includes VEC and EVENT, retire the
captured generation on user quit."
  (declare (indent 1) (debug t))
  (let ((conn (nth 0 spec))
        (ids (nth 1 spec))
        (vec (nth 2 spec))
        (event (nth 3 spec)))
    `(unwind-protect
         (condition-case interrupted
             (progn ,@body)
           (quit
            ,(when vec
               `(tramp-rpc--invalidate-interrupted-connection
                 (tramp-rpc-connection-process ,conn) ,vec ,event))
            (signal (car interrupted) (cdr interrupted))))
       (tramp-rpc--release-pending-requests ,conn ,ids))))

(defun tramp-rpc--claim-connection-generation (process vec event reason)
  "Claim PROCESS as a dead generation and detach it from VEC's connection table.
Marks the generation as being cleaned up and, when PROCESS is still the
current connection for VEC, removes it so a replacement can take over.
Returns the claimed `tramp-rpc-connection', or nil when PROCESS has no
generation or it was already claimed.
EVENT is the process event string.
REASON describes why the process ended."
  (when (processp process)
    (let* ((current-connection (tramp-rpc--get-connection vec))
           (current-p (or (null current-connection)
                          (eq process (tramp-rpc-connection-process
                                       current-connection))))
           (conn (or (tramp-rpc--process-connection process)
                     (and current-p current-connection))))
      (when (and conn (not (tramp-rpc-connection-cleanup-started conn)))
        (setf (tramp-rpc-connection-cleanup-started conn) t
              (tramp-rpc-connection-cleanup-reason conn) reason
              (tramp-rpc-connection-cleanup-event conn) event)
        ;; Detach the generation before any relay sentinel can reenter
        ;; connection lookup.  Explicit cleanup retains CONN locally for its
        ;; final kill RPCs.
        (when current-p
          (tramp-rpc--clear-direnv-cache vec)
          (run-hook-with-args 'tramp-rpc-connection-invalidate-functions vec)
          (tramp-rpc--remove-connection vec process))
        conn))))

(defun tramp-rpc--finish-connection-generation-cleanup
    (process vec event conn &optional remote-cleanup defer-callbacks)
  "Complete cleanup of claimed generation CONN whose transport is PROCESS.
VEC is the TRAMP vector CONN served.  REMOTE-CLEANUP requests termination
before local relay deletion when PROCESS is still live.  When
DEFER-CALLBACKS is non-nil, this returns (ERROR-RESPONSE . CALLBACKS)
instead of invoking the callbacks.  EVENT is the process event string."
  (let ((error-response (tramp-rpc--connection-error-response vec event))
        (callback-table (tramp-rpc-connection-async-callbacks conn))
        callbacks)
    ;; Kill acknowledgements must still be accepted by the live transport.
    ;; Mark it dead only after these captured-generation calls complete.
    (when (and remote-cleanup (process-live-p process))
      (run-hook-with-args 'tramp-rpc-transport-terminate-functions
                          vec process conn))
    (setf (tramp-rpc-connection-transport-cleaned conn) t
          (tramp-rpc-connection-transport-dead conn) t)
    ;; Wake synchronous callers after remote cleanup.  The injected errors
    ;; stay in the generation until their waiters consume them.
    (dolist (id (tramp-rpc-connection-pending-ids conn))
      (puthash id error-response (tramp-rpc-connection-pending-responses conn)))
    ;; Detach callbacks before invoking them, so callback code cannot observe
    ;; or recreate a callback belonging to this dead generation.
    (maphash (lambda (_id callback) (push callback callbacks)) callback-table)
    (clrhash callback-table)
    ;; Cleanup functions keep local relays tracked through delete-process so
    ;; their wrapped sentinels can preserve the user's sentinel.  Remote
    ;; termination was completed above using the captured connection.
    (run-hook-with-args 'tramp-rpc-transport-cleanup-functions vec process)
    (unless defer-callbacks
      (dolist (callback callbacks)
        (condition-case callback-error
            (funcall callback error-response)
          (error
           (tramp-rpc--debug "transport cleanup callback failed: %S"
                             callback-error)))))
    ;; Explicit disconnect owns transport deletion; unexpected death is
    ;; already dispatched by Emacs and this is harmlessly idempotent.
    (when (and remote-cleanup (process-live-p process))
      (delete-process process))
    (when defer-callbacks
      (cons error-response callbacks))))

(defun tramp-rpc--cleanup-connection-generation
    (process vec event reason &optional remote-cleanup defer-callbacks)
  "Clean up one PROCESS generation for VEC.
REASON and EVENT are retained on the generation and used for both explicit
disconnect and unexpected transport death.  REMOTE-CLEANUP requests
termination before local relay deletion when PROCESS is still live.  When
DEFER-CALLBACKS is non-nil, callback invocation is left to the caller and
this returns (ERROR-RESPONSE . CALLBACKS)."
  (when-let* ((conn (tramp-rpc--claim-connection-generation
                     process vec event reason)))
    (tramp-rpc--finish-connection-generation-cleanup
     process vec event conn remote-cleanup defer-callbacks)))

(defun tramp-rpc--connection-transport-death (process vec event)
  "Clean up unexpected death of PROCESS generation for VEC.
EVENT is the process event string."
  (tramp-rpc--cleanup-connection-generation
   process vec event :transport-death nil))

(defun tramp-rpc--install-connection-sentinel (process vec)
  "Install the transport sentinel on PROCESS for VEC's generation.
PROCESS must already be attached to its generation, which records that the
sentinel is in place so a second call cannot wrap it twice."
  (let ((conn (tramp-rpc--process-connection process)))
    (unless (tramp-rpc-connection-sentinel-installed conn)
      (setf (tramp-rpc-connection-sentinel-installed conn) t)
      (let ((sentinel (process-sentinel process)))
        (set-process-sentinel
         process
         (lambda (proc event)
           (when sentinel
             (funcall sentinel proc event))
           (when (memq (process-status proc) '(exit signal))
             (tramp-rpc--connection-transport-death proc vec event))))))))

(defun tramp-rpc--ensure-connection (vec)
  "Ensure we have an active RPC connection to VEC.
Returns the connection plist.
When `non-essential' is non-nil and no live connection exists,
throws `non-essential' instead of opening a new connection.
This prevents background operations (timers, fontification,
completion) from blocking on unreachable hosts."
  (let ((conn (tramp-rpc--get-connection vec)))
    (if (and conn
             (process-live-p (tramp-rpc-connection-process conn))
             (buffer-live-p (tramp-rpc-connection-buffer conn)))
        conn
      (with-mutex (tramp-rpc--connection-lifecycle-mutex vec)
        ;; Another thread may have reconnected while this one waited.
        (setq conn (tramp-rpc--get-connection vec))
        (if (and conn
                 (process-live-p (tramp-rpc-connection-process conn))
                 (buffer-live-p (tramp-rpc-connection-buffer conn)))
            conn
          ;; Stale connection - remove it before reconnecting.
          (when conn
            (tramp-rpc--remove-connection vec))
          ;; During non-essential operations, don't open new connections.
          ;; This mirrors the (unless (tramp-connectable-p vec)
          ;; (throw 'non-essential 'non-essential)) pattern used by every
          ;; standard TRAMP backend in their maybe-open-connection functions.
          (unless (tramp-connectable-p vec)
            (throw 'non-essential 'non-essential))
          ;; Need to establish connection.
          (tramp-rpc--connect vec))))))

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

;; ============================================================================
;;; Authentication via tramp-process-actions
;; ============================================================================

;; Reuse upstream TRAMP's `tramp-process-actions' state machine for all
;; interactive authentication (SSH passwords, sudo, host-key prompts,
;; OTP, security keys).  This gives us auth-source integration, password
;; caching, wrong-password detection, and locale-aware prompt matching
;; for free, instead of reimplementing with a custom regexp + loop.

(defvar tramp-rpc--controlmaster-socket-path nil
  "Dynamically bound socket path during ControlMaster establishment.
Used by `tramp-rpc--action-controlmaster-established'.")

(defvar tramp-rpc--controlmaster-socket-grace-retries 50
  "Number of checks for a late ControlMaster socket after ssh exits.")

(defvar tramp-rpc--controlmaster-socket-grace-delay 0.02
  "Seconds between checks for a late ControlMaster socket.")

(defun tramp-rpc--action-controlmaster-established (proc _vec)
  "Succeed when the ControlMaster socket file appears, fail on process death.
The target socket path is read from the dynamic variable
`tramp-rpc--controlmaster-socket-path'.
PROC is the process being handled.
With ControlPersist, the ssh parent exits as soon as it forks the
persistent master into the background, and the socket can become visible a
few milliseconds after that exit.  Give a dead process a short grace period
for the socket to appear before declaring the attempt dead."
  (cond
   ((file-exists-p tramp-rpc--controlmaster-socket-path)
    (throw 'tramp-action 'ok))
   ((not (process-live-p proc))
    (while (tramp-accept-process-output proc))
    (dotimes (_ tramp-rpc--controlmaster-socket-grace-retries)
      (when (file-exists-p tramp-rpc--controlmaster-socket-path)
        (throw 'tramp-action 'ok))
      (sleep-for tramp-rpc--controlmaster-socket-grace-delay))
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

;; ============================================================================
;;; Multi-hop support
;; ============================================================================

(defun tramp-rpc--hops-to-proxyjump (vec)
  "Convert VEC's hop chain to an SSH ProxyJump (-J) string.
Parses the TRAMP hop field (for example, `rpc:user@gateway|') and converts
each hop to the SSH ProxyJump format (for example, `user@gateway').
Returns nil if there are no hops.

For sudo-via-RPC paths, the same-host rpc hop carrying the SSH details is
not a ProxyJump; other same-host rpc hops are preserved.  Supports mixed
methods: both `rpc:' and `ssh:' hops are accepted since ProxyJump only
needs host connectivity."
  (when-let* ((hop-pairs (tramp-rpc--hop-pairs vec)))
    (let ((sudo-hop (tramp-rpc--same-host-rpc-hop vec 'return-string))
          proxy-parts)
      (dolist (hop hop-pairs)
        (let* ((hop-vec (cdr hop)))
          ;; Skip only the rpc hop that represents sudo-via-RPC.
          (unless (and sudo-hop (tramp-rpc--same-hop-p (cdr hop) (cdr sudo-hop)))
            (push (concat
                   (when (tramp-file-name-user hop-vec)
                     (concat (tramp-file-name-user hop-vec) "@"))
                   (tramp-file-name-host hop-vec)
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
  (let* ((sudo-hop (tramp-rpc--sudo-rpc-hop-vec vec))
         (host (tramp-file-name-host vec))
         (user (or (and sudo-hop
                         (or (tramp-file-name-user sudo-hop)
                             (user-login-name)))
                   (tramp-file-name-user vec)
                   (user-login-name)))
         (port (or (tramp-rpc--port-to-string
                    (if sudo-hop
                        (tramp-file-name-port sudo-hop)
                      (tramp-file-name-port vec)))
                   "22"))
         ;; For sudo, use only proxy hops (exclude the same-host sudo hop)
         (hop (if sudo-hop
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

(defun tramp-rpc--ssh-identity-args (user port proxyjump)
  "Return SSH -l/-p/-J arguments for USER, PORT, and PROXYJUMP.
Each of USER, PORT, and PROXYJUMP may be nil, in which case the
corresponding argument is omitted."
  (append (when user (list "-l" user))
          (when port (list "-p" port))
          (when proxyjump (list "-J" proxyjump))))

(defun tramp-rpc--controlmaster-active-p (vec)
  "Return non-nil if a ControlMaster connection is active for VEC."
  (let* ((socket-path (tramp-rpc--controlmaster-socket-path vec))
         (host (tramp-file-name-host vec))
         (user (tramp-rpc--ssh-detail-user vec))
         (port (tramp-rpc--port-to-string
                (tramp-rpc--ssh-detail-port vec)))
         (proxyjump (tramp-rpc--hops-to-proxyjump vec)))
    (and (file-exists-p socket-path)
         ;; Check if the socket is actually usable via ssh -O check
         (zerop (apply #'call-process "ssh" nil nil nil
                       (append
                        (tramp-rpc--ssh-identity-args user port proxyjump)
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
         (user (tramp-rpc--ssh-detail-user vec))
         (port (tramp-rpc--port-to-string
                (tramp-rpc--ssh-detail-port vec)))
         (proxyjump (tramp-rpc--hops-to-proxyjump vec))
         (socket-path (tramp-rpc--controlmaster-socket-path vec))
         (process-name (format "*tramp-rpc-auth %s*" host))
         (buffer (get-buffer-create (format " *tramp-rpc-auth %s*" host)))
         (ssh-args (append
                    (list "ssh")
                    ;; Never let this master ask for a remote
                    ;; pseudo-terminal.  The request stays ahead of
                    ;; user-supplied arguments on purpose: OpenSSH
                    ;; resolves repeated options to their first value, so
                    ;; neither `tramp-rpc-ssh-args' nor ssh_config
                    ;; (`RequestTTY yes', `-t') can make the session-less
                    ;; master (`-N', below, which also implies
                    ;; RequestTTY=no) ask for a remote pseudo-terminal,
                    ;; say on a ProxyJump hop (see #213).  The master
                    ;; must stay a plain connection multiplexer.
                    (list "-o" "RequestTTY=no")
                    tramp-rpc-ssh-args
                    (tramp-rpc--ssh-identity-args user port proxyjump)
                    ;; NO BatchMode - allow password prompts
                    (list "-o" "StrictHostKeyChecking=accept-new")
                    ;; ControlMaster options
                    (list "-o" "ControlMaster=yes"
                          "-o" (format "ControlPath=%s" socket-path)
                          "-o" (format "ControlPersist=%s"
                                       tramp-rpc-controlmaster-persist))
                    (tramp-rpc--server-alive-args)
                    ;; Connect and immediately exit, leaving ControlMaster running
                    (list "-N" host)))
         process)
    ;; If the socket file exists but `tramp-rpc--controlmaster-active-p' did
    ;; not accept it, it is stale.  OpenSSH exits immediately when asked to
    ;; create a ControlMaster on top of a stale ControlPath, which later shows
    ;; up as a generic "Tramp failed to connect" during unrelated file ops.
    (when (file-exists-p socket-path)
      (remhash socket-path tramp-rpc--owned-controlmasters)
      (delete-file socket-path))
    (with-current-buffer buffer
      (erase-buffer))
    (let (success)
      (unwind-protect
          (progn
            ;; Start SSH with a local PTY.  OpenSSH writes password and
            ;; passphrase prompts to, and reads the reply from, its
            ;; controlling terminal; `tramp-process-actions' matches those
            ;; prompts in the process buffer and answers via stdin, which
            ;; with a PTY is that same terminal.  A controlling terminal is
            ;; therefore required for authentication: a pipe leaves ssh
            ;; without one (it will not fall back to reading stdin), and a
            ;; separate stderr buffer would hide the prompts from the
            ;; regexp actions (see the review of #213).  Only the local
            ;; side gets a terminal; the `RequestTTY=no' above keeps any
            ;; remote tty request out of the ControlMaster itself.
            (let ((process-connection-type t))
              (setq process (apply #'start-process process-name buffer ssh-args)))
            (set-process-query-on-exit-flag process nil)
            (set-process-sentinel process #'ignore)
            ;; Set up process properties for tramp-process-actions /
            ;; tramp-read-passwd.  pw-vector tells auth-source where to look
            ;; up credentials.
            (process-put process 'tramp-vector vec)
            (tramp-set-connection-property process "hop-vector" vec)
            (tramp-set-connection-property
             process "pw-vector"
             (make-tramp-file-name
              :method "ssh" :user user :host host :port port))
            ;; Use upstream tramp-process-actions for password/host-key
            ;; handling.  The custom action checks for the ControlMaster
            ;; socket appearing.
            (let ((tramp-rpc--controlmaster-socket-path socket-path))
              (tramp-process-actions process vec nil
                                     tramp-rpc--controlmaster-actions 60))
            ;; tramp-process-actions throws on failure; reaching here means
            ;; the persistent master owns PROCESS and BUFFER.
            (sleep-for 0.1)
            (puthash socket-path process tramp-rpc--owned-controlmasters)
            (setq success t))
        (unless success
          (remhash socket-path tramp-rpc--owned-controlmasters)
          (when (and process (process-live-p process))
            (delete-process process))
          (when (buffer-live-p buffer)
            (kill-buffer buffer))))
      success)))

(defun tramp-rpc--server-binary-unavailable-p (process)
  "Return non-nil when PROCESS reports a remote exec failure.
A missing or non-executable remote binary is reported by the remote shell as
127 or 126, which ssh propagates.  Do not infer this from free-form stderr:
wrappers and dynamic loaders can print the same text for unrelated failures."
  (and (not (process-live-p process))
       (memq (process-exit-status process) '(126 127))))

(defun tramp-rpc--connection-sentinel (process _event)
  "Discard deferred protocol state when RPC connection PROCESS closes."
  (unless (process-live-p process)
    (tramp-rpc-protocol--clear-deferred-polls-for-target process)))

(defun tramp-rpc--start-server-process (vec binary-path &optional sudo-password)
  "Start the RPC server on VEC at BINARY-PATH and verify it responds.
BINARY-PATH is the remote localname of the server binary (may contain ~).
For sudo-via-RPC paths, the server is started via sudo.  If SUDO-PASSWORD
is non-nil, feed it to sudo -S before the RPC server starts reading stdin.
Returns the connection plist.  Signals `remote-file-error' on failure."
  (let* ((sudo-ssh-user (tramp-rpc--detect-sudo-elevation vec))
         (host (tramp-file-name-host vec))
         (user (or sudo-ssh-user (tramp-file-name-user vec)))
         (port (tramp-rpc--port-to-string
                (tramp-rpc--ssh-detail-port vec)))
         (proxyjump (tramp-rpc--hops-to-proxyjump vec))
         ;; Build SSH command to run the RPC server
         (ssh-args (append
                    (list "ssh")
                    ;; Raw SSH arguments (e.g., -v, -F config)
                    tramp-rpc-ssh-args
                    (tramp-rpc--ssh-identity-args user port proxyjump)
                    ;; Only use BatchMode=yes when ControlMaster handles auth;
                    ;; without it, BatchMode=yes prevents password prompts.
                    (when tramp-rpc-use-controlmaster
                      (list "-o" "BatchMode=yes"))
                    (list "-o" "StrictHostKeyChecking=accept-new")
                    ;; User-specified SSH options
                    (mapcan (lambda (opt) (list "-o" opt))
                            tramp-rpc-ssh-options)
                    (tramp-rpc--server-alive-args)
                    ;; ControlMaster options for connection sharing
                    ;; Use the expanded socket path to match what establish-controlmaster created
                    (when tramp-rpc-use-controlmaster
                      (list "-o" "ControlMaster=auto"
                            "-o" (format "ControlPath=%s"
                                         (tramp-rpc--controlmaster-socket-path vec))
                            "-o" (format "ControlPersist=%s"
                                         tramp-rpc-controlmaster-persist)))
                    ;; For sudo elevation, wrap the binary in sudo.  This is
                    ;; the RPC analogue of TRAMP's native sudo method: open an
                    ;; elevated long-lived backend connection, then run process
                    ;; operations inside that connection.
                    (if sudo-ssh-user
                        (append
                         (list host "sudo")
                         ;; Do not use an empty sudo prompt here.  OpenSSH
                         ;; builds the remote command from argv and drops the
                         ;; empty argument, making sudo parse "-u" as the
                         ;; prompt string instead of the user option.
                         (if sudo-password
                             '("-k" "-S" "-p" "Password:")
                           '("-n"))
                         ;; Match TRAMP's sudo method: run with the target
                         ;; user's home environment rather than preserving the
                         ;; SSH user's HOME.
                         (list "-u" (tramp-file-name-user vec) "-H"
                               binary-path))
                      (list host binary-path))))
         ;; Use TRAMP's standard naming so tramp-get-connection-process works
         (process-name (tramp-get-connection-name vec))
         (buffer-name (tramp-buffer-name vec))
         (buffer (get-buffer-create buffer-name))
         (stderr-buffer (get-buffer-create (concat buffer-name " stderr")))
         process)

    ;; Clear buffers.  The main buffer must be unibyte for binary MessagePack
    ;; framing.  Keep SSH stderr in a separate buffer: OpenSSH diagnostics (for
    ;; example ControlMaster mux messages) are not part of the RPC protocol, and
    ;; if they are mixed into the stdout buffer the length-prefixed reader loses
    ;; framing and every call waits for the full RPC timeout.
    (with-current-buffer buffer
      ;; This internal transport buffer is continuously appended to and
      ;; drained by the process filter.  Recording those binary changes in an
      ;; undo list wastes memory and can exceed `undo-outer-limit'.  Disable
      ;; undo before clearing a reused buffer so that erase is not recorded.
      (buffer-disable-undo)
      (erase-buffer)
      (set-buffer-multibyte nil)
      (set-marker (mark-marker) (point-min)))
    (with-current-buffer stderr-buffer
      (buffer-disable-undo)
      (erase-buffer))

    ;; Start the process with pipe connection (not PTY).  PTYs have line
    ;; buffering and ~4KB line length limits that break large MessagePack
    ;; requests.  Use `make-process' rather than `start-process' so local SSH
    ;; stderr can be separated from the binary stdout protocol stream.
    (setq process
          (make-process
           :name process-name
           :buffer buffer
           :command ssh-args
           :connection-type 'pipe
           :coding 'binary
           :noquery t
           :stderr stderr-buffer
           :filter #'tramp-rpc--connection-filter
           :sentinel #'tramp-rpc--connection-sentinel))

    (condition-case start-error
        (progn
          ;; If sudo is reading the password from stdin, send it before the RPC
          ;; server starts.  sudo consumes this line; after exec, the same pipe
          ;; carries MessagePack-RPC frames to the server.  Keep this inside the
          ;; startup cleanup region because the transport can exit before the
          ;; password write is accepted.
          (when sudo-password
            (process-send-string process (concat sudo-password "\n")))

          ;; Store connection.
          (tramp-rpc--set-connection vec process buffer stderr-buffer)

          ;; Install the generation sentinel before the first RPC can be sent.
          (process-put process 'tramp-vector vec)
          (tramp-rpc--install-connection-sentinel process vec)

          ;; Wait for server to be ready by sending a ping, and seed the
          ;; connection-local system.info cache for later uid/gid/home/shell
          ;; lookups.  Tear down a failed transport before retrying.
          (let ((response (tramp-rpc--cache-system-info
                           vec (tramp-rpc--call vec "system.info" nil))))
            (unless response
              (signal 'remote-file-error
                      (list "Failed to connect to RPC server on" host))))

          ;; Set connection-local variables in the connection buffer.
          ;; Every TRAMP backend must call this after establishing the
          ;; connection so that connection-local variable profiles
          ;; (registered via `connection-local-set-profiles') are applied.
          ;; This enables variables like `tramp-direct-async-process',
          ;; `shell-file-name', `path-separator' etc. to take effect in the
          ;; connection buffer.
          (tramp-set-connection-local-variables vec)

          ;; Mark as connected for TRAMP's connectivity checks (used by
          ;; projectile, etc.)
          (tramp-set-connection-property process "connected" t)

          ;; Mark as connected on the vec so `tramp-list-connections' finds
          ;; this connection and `tramp-cleanup-connection' can offer it
          ;; interactively.  The value is the connection buffer, matching the
          ;; convention in `tramp-get-buffer'.
          ;; Keep TRAMP's private leading-space connection properties.
          (tramp-set-connection-property vec " process-buffer" buffer)
          (tramp-set-connection-property vec " connected" buffer)

          (tramp-rpc--get-connection vec))
      ((quit error)
       (let ((binary-unavailable
              (tramp-rpc--server-binary-unavailable-p process))
             (sudo-auth-rejected
              (and sudo-password
                   (tramp-rpc--sudo-auth-rejected-p stderr-buffer))))
         ;; Missing binaries and other transport failures also reach this
         ;; handler.  Forget the password only when sudo explicitly rejected
         ;; it, so the next attempt prompts for a fresh password.
         (when sudo-auth-rejected
           (tramp-rpc--clear-sudo-password vec))
         ;; The generation may already have detached itself after a timeout or
         ;; protocol error, so clean up directly from the local handles.
         (tramp-rpc--remove-connection vec process)
         (when (process-live-p process)
           (delete-process process))
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (when (buffer-live-p stderr-buffer)
           (kill-buffer stderr-buffer))
         (cond (sudo-auth-rejected
                (signal 'tramp-rpc-sudo-auth-rejected (cdr start-error)))
               (binary-unavailable
                (signal 'tramp-rpc-server-unavailable (cdr start-error)))
               (t
                (signal (car start-error) (cdr start-error)))))))))

(defun tramp-rpc--cleanup-failed-connection (vec)
  "Clean up a failed connection attempt for VEC.
Kills the process if still alive and removes the connection entry."
  (let ((conn (tramp-rpc--get-connection vec)))
    (when conn
      (let ((proc (tramp-rpc-connection-process conn))
            (buffer (tramp-rpc-connection-buffer conn))
            (stderr-buffer (tramp-rpc-connection-stderr-buffer conn)))
        (when (process-live-p proc)
          (delete-process proc))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (when (buffer-live-p stderr-buffer)
          (kill-buffer stderr-buffer)))
      (tramp-rpc--remove-connection vec))))

(defun tramp-rpc--cleanup-bootstrap-connection (vec)
  "Remove the scpx/scp bootstrap connection for VEC if it has state.
The bootstrap connection is only needed during deployment.  Leaving its
TRAMP cache entries behind lets background packages keep using tramp-sh for
the same host while tramp-rpc is active.  In particular, a tramp-sh liveness
probe can then interleave with RPC startup and corrupt the protocol stream."
  (let* ((bootstrap-vec (tramp-rpc-deploy--bootstrap-vec vec))
         (proc (tramp-get-connection-process bootstrap-vec)))
    (when (or (process-live-p proc)
              (tramp-connection-property-p bootstrap-vec " process-buffer")
              (tramp-connection-property-p bootstrap-vec " connected"))
      ;; Preserve authentication and unrelated asynchronous processes while
      ;; removing the bootstrap shell, buffers, timers, and cached connection
      ;; properties.
      (tramp-cleanup-connection
       bootstrap-vec 'keep-debug 'keep-password 'keep-processes))))

(defun tramp-rpc--connect (vec)
  "Establish an RPC connection to VEC."
  ;; Ensure ControlMaster directory exists
  (tramp-rpc--ensure-controlmaster-directory)
  ;; When ControlMaster is enabled, establish it first.
  ;; This handles both key-based and password authentication:
  ;; - Key-based: connects silently
  ;; - Password: prompts user, then subsequent connections reuse it
  (when tramp-rpc-use-controlmaster
    (condition-case err
        (tramp-rpc--establish-controlmaster vec)
      ((file-error remote-file-error)
       ;; A stale ControlMaster socket can make OpenSSH exit immediately while
       ;; TRAMP reports only a generic connection failure.  Remove the socket
       ;; and retry once before surfacing the error.  Retry likewise when no
       ;; socket exists at all: the first attempt may have died transiently
       ;; before creating one.  Only a socket that still answers ControlMaster
       ;; checks must be left alone.
       (let ((socket-path (tramp-rpc--controlmaster-socket-path vec)))
         (if (tramp-rpc--controlmaster-active-p vec)
             ;; Do not tear down a socket that still answers ControlMaster
             ;; checks merely because authentication failed for another reason.
             (signal (car err) (cdr err))
           (condition-case nil
               (delete-file socket-path)
             (file-missing nil))
           (sleep-for 0.1)
           (condition-case nil
               (tramp-rpc--establish-controlmaster vec)
             ((file-error remote-file-error)
              (signal (car err) (cdr err)))))))))
  (let* ((sudo-ssh-user (tramp-rpc--detect-sudo-elevation vec))
         ;; TRAMP's sudo method opens an elevated backend connection.  For the
         ;; RPC backend that means starting the server via sudo.  Prefer sudo
         ;; -n when a ticket is already valid; otherwise read the password with
         ;; TRAMP's auth machinery and feed it to sudo -S during server start.
         (sudo-password (when (and sudo-ssh-user
                                   (tramp-rpc--sudo-password-required-p vec))
                          (tramp-rpc--sudo-read-password vec sudo-ssh-user))))
    (if tramp-rpc-deploy-never-deploy
      ;; Never-deploy mode: use the configured path directly, no fallback.
      (let ((binary-path (tramp-rpc-deploy-ensure-binary vec)))
        (condition-case err
            (progn
              (tramp-rpc--cleanup-bootstrap-connection vec)
              (tramp-rpc--start-server-process vec binary-path sudo-password))
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
    (condition-case err
        (progn
          ;; Remove bootstrap state left by an earlier deployment before the
          ;; startup probe begins exchanging MessagePack frames.
          (tramp-rpc--cleanup-bootstrap-connection vec)
          (tramp-rpc--start-server-process
           vec (tramp-rpc-deploy-expected-binary-localname) sudo-password))
      ((tramp-rpc-server-unavailable tramp-rpc-sudo-auth-rejected)
       ;; Binary missing or sudo rejected the password.  Clean up and deploy.
       (tramp-rpc--cleanup-failed-connection vec)
       ;; Deployment uses a bootstrap TRAMP connection.  Remove all of its
       ;; state before retrying RPC startup, and also when deployment or the
       ;; retry itself fails.
       (unwind-protect
           (let ((binary-path (tramp-rpc-deploy-ensure-binary vec)))
             (tramp-rpc--cleanup-bootstrap-connection vec)
             ;; Re-prompt only when sudo explicitly rejected the supplied
             ;; password.  Missing-binary fallback keeps using the still-valid
             ;; credential.
             (when (and sudo-ssh-user
                        (eq (car err) 'tramp-rpc-sudo-auth-rejected))
               (setq sudo-password
                     (when (tramp-rpc--sudo-password-required-p vec)
                       (tramp-rpc--sudo-read-password vec sudo-ssh-user))))
             (tramp-rpc--start-server-process
              vec binary-path sudo-password))
         (tramp-rpc--cleanup-bootstrap-connection vec)))))))

(defun tramp-rpc--disconnect (vec)
  "Disconnect the RPC connection to VEC explicitly."
  (when-let* ((conn (tramp-rpc--get-connection vec))
              (connection-process (tramp-rpc-connection-process conn)))
    (tramp-rpc--cleanup-connection-generation
     connection-process vec "explicit disconnect\n" :explicit-disconnect t))
  ;; Flush TRAMP caches so a reconnect gets fresh data (home dir, uid, etc.).
  (tramp-flush-directory-properties vec "/")
  (tramp-flush-connection-properties vec))

(defun tramp-rpc--controlmaster-socket-shared-p (vec &optional except-process)
  "Return non-nil when another live connection shares VEC's ControlMaster socket.
A connection other than EXCEPT-PROCESS is considered to share the socket when
its process is live and its ControlMaster socket path equals VEC's.  Tearing
down VEC's ControlMaster in that case would disrupt the still-live connection."
  (let ((socket-path (tramp-rpc--controlmaster-socket-path vec)))
    (catch 'shared
      (maphash
       (lambda (_key conn)
         (let ((other-process (tramp-rpc-connection-process conn))
               (other-vec (tramp-rpc-connection-vec conn)))
           (when (and other-process
                      other-vec
                      (not (eq other-process except-process))
                      (process-live-p other-process)
                      (equal socket-path
                             (tramp-rpc--controlmaster-socket-path other-vec)))
             (throw 'shared t))))
       tramp-rpc--connections)
      nil)))

(defun tramp-rpc--cleanup-controlmaster-unlocked (vec)
  "Clean up VEC's owned ControlMaster while holding its lifecycle mutex.
A socket reused from another Emacs process is not owned here and must not
receive `ssh -O exit', which would disconnect that other session."
  (when tramp-rpc-use-controlmaster
    (let* ((host (tramp-file-name-host vec))
           (user (tramp-rpc--ssh-detail-user vec))
           (port (tramp-rpc--port-to-string
                  (tramp-rpc--ssh-detail-port vec)))
           (proxyjump (tramp-rpc--hops-to-proxyjump vec))
           (socket-path (tramp-rpc--controlmaster-socket-path vec))
           (auth-process
            (gethash socket-path tramp-rpc--owned-controlmasters))
           (auth-buffer (and (processp auth-process)
                             (process-buffer auth-process)))
           (owned (processp auth-process)))
      ;; Close the ControlMaster socket gracefully via ssh -O exit.
      ;; This is a local control message (no network round-trip), so fast.
      (when (and owned (file-exists-p socket-path))
        (condition-case err
            (apply #'call-process "ssh" nil nil nil
                   (append
                    (tramp-rpc--ssh-identity-args user port proxyjump)
                    (list "-o" (format "ControlPath=%s" socket-path)
                          "-O" "exit" host)))
          (file-error
           (tramp-rpc--debug "ControlMaster cleanup failed: %s"
                             (error-message-string err)))))
      ;; Kill the auth process.
      (when (and auth-process (process-live-p auth-process))
        (delete-process auth-process))
      ;; Kill the auth buffer.
      (when (buffer-live-p auth-buffer)
        (kill-buffer auth-buffer))
      (remhash socket-path tramp-rpc--owned-controlmasters))))

(defun tramp-rpc--cleanup-controlmaster (vec &optional expected-process)
  "Clean up the ControlMaster process and socket for VEC.
Sends an SSH -O exit command to gracefully close the ControlMaster socket, then
kills the auth process and buffer.  Skip cleanup when EXPECTED-PROCESS is
non-nil and a newer RPC connection has replaced that process, or when another
live connection shares VEC's ControlMaster socket."
  (with-mutex (tramp-rpc--connection-lifecycle-mutex vec)
    (let ((current (tramp-rpc--get-connection vec)))
      (when (and (or (null expected-process)
                     (null current)
                     (eq expected-process (tramp-rpc-connection-process current)))
                 (not (tramp-rpc--controlmaster-socket-shared-p
                       vec expected-process)))
        (tramp-rpc--cleanup-controlmaster-unlocked vec)))))

(defun tramp-rpc--invalidate-timed-out-connection (process vec event)
  "Discard PROCESS and its ControlMaster after a timeout on VEC.
EVENT describes the timed-out operation for transport diagnostics.  A timed-out
stream cannot safely be reused because its response may still arrive later and
the underlying SSH ControlMaster may be half-open after a network interruption."
  (let ((claimed
         (with-mutex (tramp-rpc--connection-lifecycle-mutex vec)
           (prog1 (tramp-rpc--claim-connection-generation
                   process vec event :timeout)
             ;; Tear down the ControlMaster while holding the lifecycle mutex
             ;; and only when no replacement connection is already current, so
             ;; a reconnect cannot reuse the half-open socket between removal
             ;; and teardown.
             (let ((current (tramp-rpc--get-connection vec)))
               (when (and (or (null current)
                              (eq process (tramp-rpc-connection-process current)))
                          (not (tramp-rpc--controlmaster-socket-shared-p
                                vec process)))
                 (tramp-rpc--cleanup-controlmaster-unlocked vec)))))))
    (when claimed
      (let ((deferred (tramp-rpc--finish-connection-generation-cleanup
                       process vec event claimed nil t)))
        (when (process-live-p process)
          (delete-process process))
        ;; Invoke the generation's callbacks only after the protected lifecycle
        ;; transition has completed and the transport is fully detached.
        (dolist (callback (cdr deferred))
          (condition-case callback-error
              (funcall callback (car deferred))
            (error
             (tramp-rpc--debug "transport cleanup callback failed: %S"
                               callback-error))))))))

(defun tramp-rpc--invalidate-interrupted-connection (process vec event)
  "Retire PROCESS after an interrupted synchronous request on VEC.
EVENT describes the interrupted operation.  Cleanup errors are logged so the
original user quit is always re-signalled."
  (condition-case cleanup-error
      (tramp-rpc--invalidate-timed-out-connection process vec event)
    (error
     (tramp-rpc--debug "interrupted transport cleanup failed: %S"
                       cleanup-error))))

(defun tramp-rpc--sudo-password-required-p (vec)
  "Return non-nil when sudo on VEC needs a password for non-PTY use."
  (let* ((auth-vec (or (tramp-rpc--sudo-auth-vec vec) vec))
         (result (tramp-rpc--call auth-vec "process.run"
                                  '((cmd . "sudo")
                                    (args . ["-n" "-v"])
                                    (cwd . "/"))))
         (exit-code (alist-get 'exit_code result)))
    (not (eq exit-code 0))))

;; ============================================================================
;; RPC error signalling
;; ============================================================================

(defconst tramp-rpc--invalid-params-error-code -32602
  "JSON-RPC error code for invalid request parameters.")

(defun tramp-rpc--batch-error-p (value)
  "Return non-nil when VALUE is an error object from `tramp-rpc--call-batch'."
  (and (consp value) (plist-get value :error)))

(defun tramp-rpc--batch-error-errno (error)
  "Return OS errno from batched RPC ERROR, or nil."
  (when-let* ((data (plist-get error :data)))
    (alist-get 'os_errno data)))

(defun tramp-rpc--error-args (operation detail message filename &optional data)
  "Build signal args for OPERATION, DETAIL, MESSAGE, FILENAME, and DATA."
  (append
   (cond
    ((and filename detail) (list operation detail filename message))
    (filename (list operation filename message))
    (detail (list operation detail message))
    (t (list operation message)))
   (when data (list data))))

(defun tramp-rpc--signal-rpc-error
    (operation message code os-errno &optional filename data)
  "Signal RPC error CODE/OS-ERRNO and optional structured DATA for OPERATION.
MESSAGE describes the error."
  (cond
   ((= code tramp-rpc-protocol-error-file-not-found)
    (signal 'file-missing
            (tramp-rpc--error-args operation "No such file" message filename data)))
   ((= code tramp-rpc-protocol-error-permission-denied)
    (signal 'permission-denied
            (tramp-rpc--error-args operation "Permission denied" message filename data)))
   ;; process.run can report ENOENT for its executable or its cwd.  Only the
   ;; server's explicit marker represents command-not-found status 127.
   ((and (= code tramp-rpc-protocol-error-process)
         (eq (alist-get 'spawn_not_found data) t))
    (signal 'file-missing
            (tramp-rpc--error-args operation "No such file" message filename data)))
   ((and (not (= code tramp-rpc-protocol-error-process))
         (eql os-errno 2)) ; ENOENT
    (signal 'file-missing
            (tramp-rpc--error-args operation "No such file" message filename data)))
   ((eql os-errno 17) ; EEXIST
    (signal 'file-already-exists
            (tramp-rpc--error-args operation nil message filename data)))
   ((eql os-errno 39) ; ENOTEMPTY
    (signal 'file-error
            (tramp-rpc--error-args operation "Directory not empty" message filename data)))
   ((eql os-errno 20) ; ENOTDIR
    (signal 'file-error
            (tramp-rpc--error-args operation "Not a directory" message filename data)))
   ((eql os-errno 21) ; EISDIR
    (signal 'file-error
            (tramp-rpc--error-args operation "Is a directory" message filename data)))
   ((eql os-errno 40) ; ELOOP
    (signal 'file-error
            (tramp-rpc--error-args
             operation "Too many levels of symbolic links" message filename data)))
   (t
    (signal 'remote-file-error
            (tramp-rpc--error-args operation nil message filename data)))))

(defun tramp-rpc--signal-batch-failure (operation filename error)
  "Signal ERROR returned by a batched RPC for OPERATION on FILENAME."
  (tramp-rpc--signal-rpc-error
   operation
   (or (plist-get error :message) "RPC batch subrequest failed")
   (plist-get error :error)
   (tramp-rpc--batch-error-errno error)
   filename
   (plist-get error :data)))

(defun tramp-rpc--batch-result-or-signal (operation filename result)
  "Return batched RESULT, or signal its embedded error for FILENAME.
OPERATION names the attempted operation."
  (if (tramp-rpc--batch-error-p result)
      (tramp-rpc--signal-batch-failure operation filename result)
    result))

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
        (let ((tramp-rpc-protocol--message-target process))
          (while
              (condition-case protocol-error
                  (setq response
                        (tramp-rpc-protocol-try-read-message buffer))
                (error
                 ;; A malformed or incorrectly framed response makes stream
                 ;; reuse unsafe.  Fail the generation immediately instead of
                 ;; allowing an error in the process filter to strand pending
                 ;; callers.
                 (let* ((vec (process-get process :tramp-rpc-vec))
                        (event (format "RPC protocol error: %s"
                                       (error-message-string protocol-error))))
                   (tramp-rpc--debug "%s" event)
                   (when vec
                     (tramp-rpc--cleanup-connection-generation
                      process vec event :protocol-error nil))
                   (when (process-live-p process)
                     (delete-process process)))
                 nil))
                ;; Replace buffer contents with remaining data.
                (delete-region (point-min) (mark-marker))
                (goto-char (point-min))
                ;; Check for server-initiated notification (no id, has method).
                ;; Isolated: a throwing notification handler must not strand
                ;; already-buffered frames behind it in the process filter.
                (if (plist-get response :notification)
                    (condition-case notify-error
                        (run-hook-with-args 'tramp-rpc-notification-functions
                                            process
                                            (plist-get response :method)
                                            (plist-get response :params))
                      (error
                       (tramp-rpc--debug
                        "notification handler failed for %s: %S"
                        (plist-get response :method) notify-error)))
                  ;; A cleaned generation may still receive buffered output.
                  ;; Its injected transport-death errors belong to live waiters
                  ;; and must not be overwritten by late normal responses.  A
                  ;; process without a generation has nobody waiting.
                  (when-let* ((conn (tramp-rpc--process-connection process))
                              ((not (tramp-rpc-connection-transport-cleaned conn)))
                              ((not (tramp-rpc-connection-transport-dead conn))))
                    (let* ((id (plist-get response :id))
                           (callbacks (tramp-rpc-connection-async-callbacks conn))
                           (callback (gethash id callbacks)))
                      (if callback
                          (progn
                            (tramp-rpc--debug
                             "FILTER dispatching async id=%s" id)
                            (remhash id callbacks)
                            (condition-case callback-error
                                (funcall callback response)
                              (error
                               ;; One client callback must not strand complete
                               ;; responses already buffered behind its frame.
                               (tramp-rpc--debug
                                "async response callback failed for id=%s: %S"
                                id callback-error))))
                        ;; Store only responses for this generation's live
                        ;; waiters.  Late responses from an abandoned
                        ;; generation are discarded.
                        (when (memql id (tramp-rpc-connection-pending-ids conn))
                          (tramp-rpc--debug
                           "FILTER storing sync response id=%s" id)
                          (puthash id response
                                   (tramp-rpc-connection-pending-responses
                                    conn)))))))))))))

(defun tramp-rpc--call-async (vec method params callback &optional connection)
  "Call METHOD with PARAMS asynchronously on the RPC server for VEC.
CALLBACK is called with the response plist when it arrives.  CONNECTION, when
non-nil, is the captured connection generation to use.
Returns the request ID."
  (let* ((conn (or connection (tramp-rpc--ensure-connection vec)))
         (process (tramp-rpc-connection-process conn))
         (id-and-request (let ((tramp-rpc-protocol--message-target process))
                           (tramp-rpc-protocol-encode-request-with-id
                            method params)))
         (id (car id-and-request))
         (request (cdr id-and-request)))
    (tramp-rpc--debug "SEND-ASYNC id=%s method=%s" id method)
    ;; Register callback with its exact transport generation.  Roll registration
    ;; back if the transport rejects the send; no response can arrive for a
    ;; request that was never accepted by the process object.
    (puthash id callback (tramp-rpc-connection-async-callbacks conn))
    (let (sent)
      (unwind-protect
          (prog1
              (progn
                ;; Send request (binary data with length prefix, no newline)
                (process-send-string process request)
                id)
            (setq sent t))
        ;; Cover errors, user quits, and any other non-local exit.
        (unless sent
          (remhash id (tramp-rpc-connection-async-callbacks conn)))))))

(defun tramp-rpc--call (vec method params &optional connection)
  "Call METHOD with PARAMS on the RPC server for VEC.
CONNECTION, when non-nil, is the captured connection generation to use.
Returns the result or signals an error."
  (tramp-rpc--call-with-timeout
   vec method params (tramp-rpc--configured-call-timeout)
   (tramp-rpc--configured-poll-interval) connection))

(defun tramp-rpc--call-fast (vec method params)
  "Call METHOD with PARAMS with shorter timeout for low-latency ops.
Returns the result or signals an error.
Uses 5s total timeout with 10ms polling.
VEC is the TRAMP connection vector."
  (tramp-rpc--call-with-timeout vec method params 5 0.01))

(defun tramp-rpc--find-response-by-id (conn expected-id)
  "Check generation CONN's pending responses for EXPECTED-ID.
Returns the response plist if found and removes it from pending, nil
otherwise."
  (when-let* ((response (gethash expected-id
                                 (tramp-rpc-connection-pending-responses conn))))
    (tramp-rpc--release-pending-requests conn (list expected-id))
    response))

(defun tramp-rpc--process-accessible-p (process)
  "Return t if PROCESS can be accessed from the current thread.
Returns nil if the process is locked to a different thread."
  (let ((locked-thread (process-thread process)))
    (or (null locked-thread)
        (eq locked-thread (current-thread)))))

(defun tramp-rpc--drain-connection-stderr (conn)
  "Drain pending stderr output for CONN's SSH process.
`make-process' with `:stderr' creates a separate stderr process.  The RPC
wait loops accept output with JUST-THIS-ONE for the stdout protocol process,
so explicitly service stderr as well to prevent the stderr pipe from filling
and blocking SSH or the remote server."
  (when-let* ((stderr-buffer (tramp-rpc-connection-stderr-buffer conn))
              ((buffer-live-p stderr-buffer))
              (stderr-process (get-buffer-process stderr-buffer))
              ((tramp-rpc--process-accessible-p stderr-process)))
    (while (accept-process-output stderr-process 0 nil t))))

(defun tramp-rpc--connection-stderr-tail (conn &optional max-bytes)
  "Return a diagnostic tail from CONN's stderr buffer, or nil.
MAX-BYTES limits the number of bytes returned."
  (when-let* ((stderr-buffer (tramp-rpc-connection-stderr-buffer conn))
              ((buffer-live-p stderr-buffer)))
    (with-current-buffer stderr-buffer
      (when (> (buffer-size) 0)
        (buffer-substring-no-properties
         (max (point-min) (- (point-max) (or max-bytes 1024)))
         (point-max))))))

(defmacro tramp-rpc--with-suspended-timers-preserving-process-timers (&rest body)
  "Run BODY with external timers suspended, preserving new process timers.
Reactivation assumes no enclosing `with-tramp-suspended-timers' is still
in effect when BODY exits, because the restored timers would land in that
outer binding of `timer-list' and be discarded again.  The RPC wait loops
satisfy this: they accept output with JUST-THIS-ONE, so no other
connection's filter, and therefore no nested wait, can run inside BODY."
  (declare (indent 0) (debug t))
  (let ((recorded (make-symbol "recorded-process-timers")))
    `(let (,recorded)
       (unwind-protect
           (let ((tramp-rpc--process-timer-recorder
                  (lambda (&rest timer-state)
                    (push timer-state ,recorded))))
             (with-tramp-suspended-timers
               ,@body))
         (dolist (timer-state ,recorded)
           (pcase-let ((`(,timer ,table ,process ,timer-key) timer-state))
             (when (and (timerp timer)
                        (not (memq timer timer-list))
                        (when-let* ((info (gethash process table)))
                          (eq timer (plist-get info timer-key))))
               (timer-activate timer))))))))

(defun tramp-rpc--wait-for-response-ids (conn ids timeout poll-interval label)
  "Wait for IDS from generation CONN and return response state.
TIMEOUT is a wall-clock limit, and POLL-INTERVAL controls output polling.
LABEL is used only for debug messages.  The returned plist contains
:responses, :remaining, :elapsed, and :process-live."
  (let ((process (tramp-rpc-connection-process conn))
        (buffer (tramp-rpc-connection-buffer conn))
        (start-time (float-time))
        (deadline (+ (float-time) timeout))
        (remaining (copy-sequence ids))
        (responses (make-hash-table :test 'eql)))
    (cl-labels
        ((collect ()
           (dolist (id (copy-sequence remaining))
             (when-let* ((response (tramp-rpc--find-response-by-id conn id)))
               (puthash id response responses)
               (setq remaining (delete id remaining))))))
      (with-current-buffer buffer
        (collect)
        (while (and remaining
                    (< (float-time) deadline)
                    (process-live-p process))
          (if (not (tramp-rpc--process-accessible-p process))
              (if non-essential
                  (progn
                    (tramp-rpc--debug "LOCKED-%s (non-essential, bailing)" label)
                    (throw 'non-essential 'non-essential))
                (sleep-for poll-interval)
                (collect))
            (if (tramp-rpc--with-suspended-timers-preserving-process-timers
                  (with-local-quit
                    (tramp-rpc--drain-connection-stderr conn)
                    (accept-process-output process poll-interval nil t)
                    (tramp-rpc--drain-connection-stderr conn)
                    t))
                (collect)
              (tramp-rpc--debug "QUIT-%s (user interrupted)" label)
              (keyboard-quit))))))
    (list :responses responses
          :remaining remaining
          :elapsed (- (float-time) start-time)
          :process-live (process-live-p process))))

(defun tramp-rpc--call-with-timeout (vec method params total-timeout poll-interval
                                         &optional connection)
  "Call METHOD with PARAMS on the RPC server for VEC.
TOTAL-TIMEOUT is maximum seconds to wait.
POLL-INTERVAL is seconds between `accept-process-output' checks.
CONNECTION, when non-nil, is the captured connection generation to use.
Returns the result or signals an error."
  (let* ((conn (or connection (tramp-rpc--ensure-connection vec)))
         (process (tramp-rpc-connection-process conn))
         (id-and-request (let ((tramp-rpc-protocol--message-target process))
                           (tramp-rpc-protocol-encode-request-with-id
                            method params)))
         (expected-id (car id-and-request))
         (request (cdr id-and-request)))

    (tramp-rpc--debug "SEND id=%s method=%s" expected-id method)
    (tramp-rpc--track-pending-request conn expected-id)

    (tramp-rpc--with-pending-requests
        (conn (list expected-id) vec
              (format "RPC interrupted while waiting for %s\n" method))
      ;; Send request (binary data with length prefix, no newline)
      (process-send-string process request)

      (let* ((state (tramp-rpc--wait-for-response-ids
                     conn (list expected-id) total-timeout
                     poll-interval "CALL"))
             (response (gethash expected-id (plist-get state :responses))))

        (unless response
          (if (or (tramp-rpc-connection-transport-dead conn)
                  (not (plist-get state :process-live)))
              (signal 'remote-file-error
                      (list (format "RPC transport disconnected from %s"
                                    (tramp-file-name-host vec))))
            (let ((elapsed (plist-get state :elapsed))
                  (stderr-tail (tramp-rpc--connection-stderr-tail conn)))
              (tramp-rpc--debug
               "TIMEOUT id=%s method=%s elapsed=%.1fs buffer-size=%d process-live=%s stderr-tail=%S"
               expected-id method elapsed
               (buffer-size (tramp-rpc-connection-buffer conn))
               (process-live-p process) stderr-tail)
              (tramp-rpc--invalidate-timed-out-connection
               process vec (format "RPC timeout waiting for %s\n" method))
              (signal
               'remote-file-error
               (list (concat
                      (format
                       "Timeout waiting for RPC response from %s (id=%s, method=%s, waited %.1fs)"
                       (tramp-file-name-host vec) expected-id method elapsed)
                      (when stderr-tail
                        (format "; SSH stderr: %s" stderr-tail))))))))

        (tramp-rpc--debug "RECV id=%s (found)" expected-id)
        (if (tramp-rpc-protocol-error-p response)
            (let ((code (tramp-rpc-protocol-error-code response))
                  (msg (tramp-rpc-protocol-error-message response))
                  (data (tramp-rpc-protocol-error-data response))
                  (os-errno (tramp-rpc-protocol-error-errno response)))
              (tramp-rpc--debug "ERROR id=%s code=%s msg=%s errno=%s"
				expected-id code msg os-errno)
              (tramp-rpc--signal-rpc-error "RPC" msg code os-errno nil data))
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
  (let* ((timeout (tramp-rpc--configured-call-timeout))
         (poll-interval (tramp-rpc--configured-poll-interval))
         (conn (tramp-rpc--ensure-connection vec))
         (process (tramp-rpc-connection-process conn))
         (id-and-request (let ((tramp-rpc-protocol--message-target process))
                           (tramp-rpc-protocol-encode-batch-request-with-id
                            requests)))
         (expected-id (car id-and-request))
         (request (cdr id-and-request)))
    (tramp-rpc--debug "SEND-BATCH id=%s count=%d" expected-id (length requests))
    (tramp-rpc--track-pending-request conn expected-id)
    (tramp-rpc--with-pending-requests
        (conn (list expected-id) vec "Batch RPC interrupted\n")
      (process-send-string process request)
      (let* ((state (tramp-rpc--wait-for-response-ids
                     conn (list expected-id) timeout poll-interval "BATCH"))
             (response (gethash expected-id (plist-get state :responses))))
        (unless response
          (if (or (tramp-rpc-connection-transport-dead conn)
                  (not (plist-get state :process-live)))
              (signal 'remote-file-error
                      (list (format "RPC transport disconnected from %s"
                                    (tramp-file-name-host vec))))
            (let ((elapsed (plist-get state :elapsed))
                  (stderr-tail (tramp-rpc--connection-stderr-tail conn)))
              (tramp-rpc--debug
               "TIMEOUT-BATCH id=%s elapsed=%.1fs buffer-size=%d process-live=%s stderr-tail=%S"
               expected-id elapsed
               (buffer-size (tramp-rpc-connection-buffer conn))
               (plist-get state :process-live) stderr-tail)
              (tramp-rpc--invalidate-timed-out-connection
               process vec "Batch RPC timeout\n")
              (signal
               'remote-file-error
               (list (concat
                      (format
                       "Timeout waiting for batch RPC response from %s (id=%s, waited %.1fs)"
                       (tramp-file-name-host vec) expected-id elapsed)
                      (when stderr-tail
                        (format "; SSH stderr: %s" stderr-tail))))))))
        (tramp-rpc--debug "RECV-BATCH id=%s (found)" expected-id)
        (if (tramp-rpc-protocol-error-p response)
            (progn
              (tramp-rpc--debug "ERROR-BATCH id=%s msg=%s"
                                expected-id
                                (tramp-rpc-protocol-error-message response))
              (signal 'remote-file-error
		      (list "Batch RPC error"
			    (tramp-rpc-protocol-error-message response))))
          (tramp-rpc-protocol-decode-batch-response response))))))

;; ============================================================================
;; Request pipelining support
;; ============================================================================

(defun tramp-rpc--send-requests (vec requests &optional connection)
  "Send multiple REQUESTS to the RPC server for VEC without waiting.
REQUESTS is a list of (METHOD . PARAMS) cons cells.
CONNECTION, when non-nil, is the captured connection generation to use.
Returns a list of request IDs in the same order."
  (let* ((conn (or connection (tramp-rpc--ensure-connection vec)))
         (process (tramp-rpc-connection-process conn))
         ids completed dispatch-attempted)
    (unwind-protect
        (condition-case interrupted
            (progn
              (dolist (req requests)
                (let* ((id-and-bytes
                        (let ((tramp-rpc-protocol--message-target process))
                          (tramp-rpc-protocol-encode-request-with-id
                           (car req) (cdr req))))
                       (id (car id-and-bytes))
                       (bytes (cdr id-and-bytes)))
                  (tramp-rpc--debug "SEND-PIPE id=%s method=%s" id (car req))
                  (push id ids)
                  (tramp-rpc--track-pending-request conn id)
                  ;; Once a transport write is attempted, a quit leaves frame
                  ;; delivery ambiguous and this generation cannot be reused.
                  (setq dispatch-attempted t)
                  (process-send-string process bytes)))
              (setq completed t)
              (nreverse ids))
          (quit
           (when dispatch-attempted
             (tramp-rpc--invalidate-interrupted-connection
              process vec "Pipelined RPC interrupted while sending\n"))
           (signal (car interrupted) (cdr interrupted))))
      (unless completed
        (tramp-rpc--release-pending-requests conn ids)))))

(defun tramp-rpc--receive-responses (vec ids &optional timeout connection)
  "Receive responses for request IDS from the RPC server for VEC.
Returns an alist mapping each ID to its response plist.
TIMEOUT is the maximum time to wait in seconds.
When nil, `tramp-rpc-call-timeout' is used.  CONNECTION, when non-nil, is the
captured connection generation to use."
  (let* ((timeout (or timeout (tramp-rpc--configured-call-timeout)))
         (poll-interval (tramp-rpc--configured-poll-interval))
         (conn (or connection (tramp-rpc--ensure-connection vec)))
         (process (tramp-rpc-connection-process conn)))
    (tramp-rpc--debug "RECV-PIPE waiting for %d responses: %S" (length ids) ids)
    (tramp-rpc--with-pending-requests
        (conn ids vec "Pipelined RPC interrupted\n")
      (let* ((state (tramp-rpc--wait-for-response-ids
                     conn ids timeout poll-interval "PIPE"))
             (remaining-ids (plist-get state :remaining))
             (responses (plist-get state :responses)))
        (when remaining-ids
          (tramp-rpc--debug "RECV-PIPE missing ids: %S" remaining-ids)
          (let ((process-live (plist-get state :process-live))
                (stderr-tail (tramp-rpc--connection-stderr-tail conn)))
            (when process-live
              (tramp-rpc--invalidate-timed-out-connection
               process vec "Pipelined RPC timeout\n"))
            (signal
             'remote-file-error
             (list
              (if process-live
                  (concat
                   (format
                    "Timeout waiting for pipelined RPC responses from %s (missing ids: %S)"
                    (tramp-file-name-host vec) remaining-ids)
                   (when stderr-tail
                     (format "; SSH stderr: %s" stderr-tail)))
                (format
                 "RPC process died waiting for pipelined responses from %s (missing ids: %S)"
                 (tramp-file-name-host vec) remaining-ids))))))
        (mapcar (lambda (id) (cons id (gethash id responses))) ids)))))

(defun tramp-rpc--call-pipelined (vec requests)
  "Execute multiple REQUESTS in a pipelined fashion for VEC.
REQUESTS is a list of (METHOD . PARAMS) cons cells.
Returns a list of results in the same order as REQUESTS.
Each result is either the actual result or an error plist.

Unlike `tramp-rpc--call-batch', this sends each request as a separate
RPC call, allowing the server to process them concurrently.
This is more efficient when the server has async support."
  (let* ((timeout (tramp-rpc--configured-call-timeout))
         (connection (tramp-rpc--ensure-connection vec))
         (ids (tramp-rpc--send-requests vec requests connection))
         (responses (tramp-rpc--receive-responses
                     vec ids timeout connection)))
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
;; Remote PATH and login shell
;; ============================================================================

(defun tramp-rpc--effective-remote-path-spec (vec)
  "Return the remote PATH specification used by tramp-rpc on VEC.
Connection-local values are honored, matching `tramp-get-remote-path'."
  (condition-case nil
      (with-current-buffer (tramp-get-connection-buffer vec)
        (tramp-set-connection-local-variables vec)
        (copy-tree (or tramp-rpc-remote-path tramp-remote-path)))
    (error
     (copy-tree (or tramp-rpc-remote-path tramp-remote-path)))))

(defun tramp-rpc--append-path-entries (entries result)
  "Append string ENTRIES to RESULT, preserving order and removing duplicates."
  (dolist (dir entries result)
    (when (and (stringp dir)
               (not (string-empty-p dir))
               (not (member dir result)))
      (setq result (append result (list dir))))))

(defun tramp-rpc--expand-remote-path-entry (vec entry)
  "Expand one remote PATH ENTRY for VEC when necessary."
  (if (and (stringp entry)
           (string-match "\\`~\\([^/]*\\)\\(/.*\\)?\\'" entry))
      (let* ((user (match-string 1 entry))
             (suffix (or (match-string 2 entry) ""))
             (home (tramp-get-home-directory
                    vec (unless (string-empty-p user) user))))
        (concat (directory-file-name home) suffix))
    entry))

(defun tramp-rpc--fetch-default-remote-path (vec)
  "Fetch the POSIX default PATH for VEC, falling back to /bin:/usr/bin."
  (condition-case nil
      (let* ((result (tramp-rpc--call vec "process.run"
                                      `((cmd . "/bin/sh")
                                        (args . ["-c" "getconf PATH 2>/dev/null"])
                                        (cwd . "/"))))
             (exit-code (alist-get 'exit_code result))
             (stdout (tramp-rpc--decode-output
                      (alist-get 'stdout result))))
        (if (and (eq exit-code 0) (> (length stdout) 0))
            (split-string (string-trim stdout) ":" t)
          '("/bin" "/usr/bin")))
    (error '("/bin" "/usr/bin"))))

(defun tramp-rpc--compute-remote-path (vec)
  "Compute remote variable `exec-path' for VEC from `tramp-remote-path'.
A non-nil deprecated `tramp-rpc-remote-path' overrides
`tramp-remote-path'.  Supports the standard TRAMP placeholders
`tramp-default-remote-path' and `tramp-own-remote-path'.  The old
`tramp-rpc-own-remote-path' placeholder is treated like
`tramp-own-remote-path'.  Duplicate, unsupported, and nonexistent
entries are removed."
  (let ((own-path nil)
        (default-path nil)
        (result nil))
    (dolist (entry (tramp-rpc--effective-remote-path-spec vec))
      (setq entry (tramp-rpc--expand-remote-path-entry vec entry))
      (cond
       ((eq entry 'tramp-default-remote-path)
        (unless default-path
          (setq default-path (tramp-rpc--fetch-default-remote-path vec)))
        (setq result (tramp-rpc--append-path-entries default-path result)))
       ((memq entry '(tramp-own-remote-path tramp-rpc-own-remote-path))
        (unless own-path
          (setq own-path (or (tramp-rpc--fetch-remote-exec-path vec) '())))
        (setq result (tramp-rpc--append-path-entries own-path result)))
       ((stringp entry)
        (setq result (tramp-rpc--append-path-entries (list entry) result)))
       (t
        (tramp-rpc--debug "Ignoring unsupported remote PATH entry: %S" entry))))
    ;; Remove non-existing directories (matches tramp-sh behavior).
    (delq nil (mapcar (lambda (x)
                        (and (stringp x)
                             (file-directory-p
                              (tramp-make-tramp-file-name vec x))
                             x))
                      result))))

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
                   (let* ((info (tramp-rpc--system-info vec))
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
                               (alist-get 'user (tramp-rpc--system-info vec)))
                            user))
             (result (tramp-rpc--call vec "process.run"
                                       `((cmd . "getent")
                                         (args . ["passwd" ,target-user])
                                         (cwd . "/"))))
             (exit-code (alist-get 'exit_code result))
             (stdout (tramp-rpc--decode-output
                      (alist-get 'stdout result))))
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
Invokes the login shell with `-l' to source shell configuration files.
A marker separates shell startup output, MOTD text, or banners from the
actual PATH line, matching the robustness of upstream TRAMP."
  (condition-case nil
      (let* ((marker (md5 (format "tramp-rpc-path-%s" (float-time))))
             (shell (tramp-rpc--get-remote-login-shell vec))
             (result (tramp-rpc--call vec "process.run"
                                       `((cmd . ,shell)
                                         (args . ["-l" "-c"
                                                  ,(format "echo %s; printenv PATH" marker)])
                                         (cwd . "/"))))
             (exit-code (alist-get 'exit_code result))
             (stdout (tramp-rpc--decode-output
                      (alist-get 'stdout result))))
        (when (and (eq exit-code 0) (> (length stdout) 0)
                   (string-match
                    (concat (regexp-quote marker) "\r?\n\\([^\r\n]+\\)")
                    stdout))
          (split-string (string-trim (match-string 1 stdout)) ":" t)))
    (error nil)))

;; ============================================================================
;; system.info
;; ============================================================================

(defconst tramp-rpc--system-info-property "tramp-rpc-system-info"
  "TRAMP connection property storing the cached system.info response.")

(defcustom tramp-rpc--watcher-unavailable-ttl 30
  "TTL cap in seconds for caches when push notifications are unavailable.
When the server reports `watcher_available' as false, `fs.events'
notifications are not running and caches are TTL-only.  Capping to a short
TTL bounds staleness instead of serving up to `tramp-rpc--cache-ttl'
seconds of stale metadata."
  :type 'number
  :group 'tramp-rpc)

(defvar tramp-rpc--watcher-degraded nil
  "Non-nil when any known connection lacks push notifications.
Set from `system.info' `watcher_available'.  Once set, metadata and Magit
process caches use `tramp-rpc--watcher-unavailable-ttl' as a cap.  This is
global and conservative, one degraded host shortens TTLs for all, because
cache validity checks do not carry connection context.")

(defun tramp-rpc--note-watcher-availability (info)
  "Update `tramp-rpc--watcher-degraded' from system.info INFO.
INFO is the decoded response alist.  Missing `watcher_available' means an
old server that predates the field; assume available to preserve behavior.
An explicit non-t value marks degraded."
  (when info
    (let ((cell (assq 'watcher_available info)))
      (when (and cell (not (eq (cdr cell) t)))
        (setq tramp-rpc--watcher-degraded t)))))

(defun tramp-rpc--cache-system-info (vec info)
  "Store system.info INFO for VEC and seed related TRAMP properties."
  (when info
    (tramp-rpc--note-watcher-availability info)
    (tramp-rpc--set-route-connection-property
     vec tramp-rpc--system-info-property info)
    ;; Store remote uname so `tramp-check-remote-uname' works.  The server
    ;; returns "linux" or "macos"; map to the kernel names tramp-sh expects.
    (when-let* ((os (alist-get 'os info)))
      (tramp-set-connection-property
       vec "uname"
       (pcase os
         ("macos" "Darwin")
         ("linux" "Linux")
         (_ os))))
    ;; Match this backend's existing string uid/gid behavior: string format is
    ;; the numeric id rendered as text, not the login/group name.
    (when-let* ((uid (alist-get 'uid info)))
      (tramp-set-connection-property vec "uid-integer" uid)
      (tramp-set-connection-property vec "uid-string" (number-to-string uid)))
    (when-let* ((gid (alist-get 'gid info)))
      (tramp-set-connection-property vec "gid-integer" gid)
      (tramp-set-connection-property vec "gid-string" (number-to-string gid)))
    (when-let* ((home (tramp-rpc--decode-string (alist-get 'home info))))
      ;; `tramp-get-home-directory' caches under "~" when USER is nil and
      ;; under "~USER" when USER is explicit.  Seed both current-user forms.
      (tramp-set-connection-property vec "~" home)
      (when-let* ((user (tramp-file-name-user vec)))
        (unless (string-empty-p user)
          (tramp-set-connection-property vec (concat "~" user) home)))))
  info)

(defun tramp-rpc--cached-system-info (vec)
  "Return the system.info cached for VEC, or nil.  Never issues an RPC."
  (tramp-rpc--get-route-connection-property
   vec tramp-rpc--system-info-property nil))

(defun tramp-rpc--system-info (vec)
  "Return cached system.info for VEC, fetching it at most once per connection."
  (or (tramp-rpc--cached-system-info vec)
      (progn
        ;; Establishing a new connection already performs and caches a
        ;; `system.info' call as its readiness ping.  Re-check the property after
        ;; connection setup so a cold caller does not immediately send a second
        ;; identical RPC.
        (tramp-rpc--ensure-connection vec)
        (or (tramp-rpc--get-route-connection-property
             vec tramp-rpc--system-info-property nil)
            (tramp-rpc--cache-system-info
             vec (tramp-rpc--call vec "system.info" nil))))))

(provide 'tramp-rpc-transport)
;;; tramp-rpc-transport.el ends here
