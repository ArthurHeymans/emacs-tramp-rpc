;;; tramp-rpc-environment.el --- Remote process environment for TRAMP-RPC -*- lexical-binding: t; -*-

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

;; This file computes the effective environment for processes run on a
;; remote host: the remote PATH (from `tramp-remote-path' and the login
;; shell), TRAMP's remote process environment, the caller's environment,
;; and direnv variables for the process directory.  `tramp-rpc--process-environment'
;; merges these layers into the alist sent with process-running RPCs.
;;
;; The results are cached per connection (exec-path, login shell, and
;; direnv availability/environments).  The transport does not know about
;; these caches; this module drops them by registering on
;; `tramp-rpc-connection-invalidate-functions', which the transport runs
;; whenever a connection generation is retired or replaced.

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 'tramp-rpc-protocol)
(require 'tramp-rpc-connection)
(require 'tramp-rpc-transport)

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

;; ============================================================================
;; Remote exec-path and login-shell caches
;; ============================================================================

(defvar tramp-rpc--exec-path-cache (make-hash-table :test 'equal)
  "Cache of remote variable `exec-path' keyed by connection-key.")

(defvar tramp-rpc--login-shell-cache (make-hash-table :test 'equal)
  "Cache of remote login shell keyed by connection-key.")

;; ============================================================================
;; Environment merging
;; ============================================================================

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
INSIDE_EMACS is set to `tramp-inside-emacs' unless its current value
already names tramp; an unrelated value such as \\=`31.1,comint' is
replaced, as in `tramp-sh'.  Returns the (possibly augmented) alist."
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
  "Return cached remote PATH directories for VEC, computing them if needed.
Return incomplete results without caching them."
  (let* ((key (tramp-rpc--connection-key vec))
         (cached (gethash key tramp-rpc--exec-path-cache)))
    (or cached
        (let ((computed (tramp-rpc--compute-remote-path vec)))
          (when (cdr computed)
            (puthash key (car computed) tramp-rpc--exec-path-cache))
          (car computed)))))

(defun tramp-rpc--remote-path-environment (vec)
  "Return the configured PATH environment entry for VEC.
Uses `tramp-remote-path' by default.  A non-nil deprecated
`tramp-rpc-remote-path' overrides it for compatibility."
  (let ((remote-path (tramp-rpc--cached-remote-path vec)))
    (when remote-path
      `(("PATH" . ,(mapconcat #'identity remote-path ":"))))))

(defun tramp-rpc--cached-login-path (vec)
  "Return the login shell PATH directories for VEC, caching the result.
A failed fetch is not cached."
  (condition-case nil
      (tramp-rpc--with-route-connection-property vec "tramp-rpc-login-path"
        (or (tramp-rpc--fetch-remote-exec-path vec) '()))
    (error '())))

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
  "Fetch the POSIX default PATH for VEC, falling back to /bin:/usr/bin.
Signals when VEC cannot be reached."
  (let* ((result (tramp-rpc--call vec "process.run"
                                  `((cmd . "/bin/sh")
                                    (args . ["-c" "getconf PATH 2>/dev/null"])
                                    (cwd . "/"))))
         (exit-code (alist-get 'exit_code result))
         (stdout (tramp-rpc--decode-output
                  (alist-get 'stdout result))))
    (if (and (eq exit-code 0) (> (length stdout) 0))
        (split-string (string-trim stdout) ":" t)
      '("/bin" "/usr/bin"))))

(defun tramp-rpc--compute-remote-path (vec)
  "Compute remote variable `exec-path' for VEC from `tramp-remote-path'.
A non-nil deprecated `tramp-rpc-remote-path' overrides
`tramp-remote-path'.  Supports the standard TRAMP placeholders
`tramp-default-remote-path' and `tramp-own-remote-path'.  The old
`tramp-rpc-own-remote-path' placeholder is treated like
`tramp-own-remote-path'.  Duplicate, unsupported, and nonexistent
entries are removed.
Return (DIRECTORIES . COMPLETE), with COMPLETE non-nil if every fetch
succeeds."
  (let ((own-path nil)
        (default-path nil)
        (complete t)
        (result nil))
    (dolist (entry (tramp-rpc--effective-remote-path-spec vec))
      (setq entry (tramp-rpc--expand-remote-path-entry vec entry))
      (cond
       ((eq entry 'tramp-default-remote-path)
        (unless default-path
          (setq default-path
                (condition-case nil
                    (tramp-rpc--fetch-default-remote-path vec)
                  (error (setq complete nil) '("/bin" "/usr/bin")))))
        (setq result (tramp-rpc--append-path-entries default-path result)))
       ((memq entry '(tramp-own-remote-path tramp-rpc-own-remote-path))
        (unless own-path
          (setq own-path
                (condition-case nil
                    (or (tramp-rpc--fetch-remote-exec-path vec) '())
                  (error (setq complete nil) '()))))
        (setq result (tramp-rpc--append-path-entries own-path result)))
       ((stringp entry)
        (setq result (tramp-rpc--append-path-entries (list entry) result)))
       (t
        (tramp-rpc--debug "Ignoring unsupported remote PATH entry: %S" entry))))
    ;; Remove non-existing directories (matches tramp-sh behavior), checking
    ;; all candidates in one round trip.  If that fails, keep every
    ;; candidate and report the result as incomplete.
    (let* ((candidates (seq-filter #'stringp result))
           (directories
            (when candidates
              (condition-case nil
                  (cl-loop for dir in candidates
                           for stat in (tramp-rpc--call-batch
                                        vec (mapcar (lambda (dir)
                                                      (cons "file.stat"
                                                            (tramp-rpc--encode-path dir)))
                                                    candidates))
                           when (and (not (tramp-rpc--batch-error-p stat))
                                     (equal (alist-get 'type stat) "directory"))
                           collect dir)
                (error (setq complete nil) candidates)))))
      (cons directories complete))))

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
actual PATH line, matching the robustness of upstream TRAMP.
Signals when VEC cannot be reached."
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
      (split-string (string-trim (match-string 1 stdout)) ":" t))))


;; ============================================================================
;; Cache invalidation
;; ============================================================================

(defun tramp-rpc--clear-environment-caches (&optional vec)
  "Clear cached remote environments, exec-paths and login shells.
With VEC, clear only that connection's entries; without it, clear all."
  (tramp-rpc--clear-direnv-cache vec)
  (if vec
      (let ((key (tramp-rpc--connection-key vec)))
        (remhash key tramp-rpc--exec-path-cache)
        (remhash key tramp-rpc--login-shell-cache))
    (clrhash tramp-rpc--exec-path-cache)
    (clrhash tramp-rpc--login-shell-cache)))

;; ============================================================================
;; Unload support
;; ============================================================================

(defun tramp-rpc-environment-unload-function ()
  "Unload function for tramp-rpc-environment.
Removes its connection-invalidation callback."
  (remove-hook 'tramp-rpc-connection-invalidate-functions
               #'tramp-rpc--clear-environment-caches)
  ;; Return nil to allow normal unload to proceed
  nil)

;; The cached PATH, login shell and direnv state describe a connection's
;; remote host as last probed; a retired or replaced generation must not
;; hand them to its successor.
(add-hook 'tramp-rpc-connection-invalidate-functions
          #'tramp-rpc--clear-environment-caches t)

(provide 'tramp-rpc-environment)
;;; tramp-rpc-environment.el ends here
