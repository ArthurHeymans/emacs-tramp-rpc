;;; tramp-rpc-magit.el --- Magit/Projectile integration for TRAMP-RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Keywords: comm, processes, vc
;; Package-Requires: ((emacs "30.1") (msgpack "0"))

;; This file is part of tramp-rpc.

;; tramp-rpc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This file provides magit and projectile integration for tramp-rpc:
;; - Prefetch git commands via commands.run_parallel for fast magit-status
;; - Process-file cache for serving git commands from prefetched data
;; - Ancestor directory scanning for project/VC detection
;; - File-exists and file-truename caching with TTL
;; - Filesystem watching (inotify) for automatic cache invalidation
;; - Projectile optimizations for remote directories

;;; Code:

(require 'cl-lib)
(require 'tramp)

;; Functions from tramp-rpc.el
(declare-function tramp-rpc--debug "tramp-rpc")
(declare-function tramp-rpc--call "tramp-rpc")
(declare-function tramp-rpc--call-fast "tramp-rpc")
(declare-function tramp-rpc--call-pipelined "tramp-rpc")
(declare-function tramp-rpc--connection-key "tramp-rpc")
(declare-function tramp-rpc--decode-output "tramp-rpc")
(declare-function tramp-rpc--decode-string "tramp-rpc")
(declare-function tramp-rpc--encode-path "tramp-rpc")
(declare-function tramp-rpc-file-name-p "tramp-rpc")

;; Silence byte-compiler warnings for external functions
(declare-function projectile-dir-files-alien "projectile")
(declare-function projectile-time-seconds "projectile")
(declare-function magit-status-setup-buffer "magit-status")
(declare-function magit-get-mode-buffer "magit-mode")

;; ============================================================================
;; Caching infrastructure
;; ============================================================================

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

;;;###autoload
(defun tramp-rpc-clear-file-exists-cache ()
  "Clear the file-exists-p result cache.
Call this after making changes to the remote filesystem."
  (interactive)
  (clrhash tramp-rpc--file-exists-cache)
  (when (called-interactively-p 'any)
    (message "tramp-rpc file-exists cache cleared")))

;;;###autoload
(defun tramp-rpc-clear-file-truename-cache ()
  "Clear the file-truename result cache.
Call this after making changes to symlinks on the remote filesystem."
  (interactive)
  (clrhash tramp-rpc--file-truename-cache)
  (when (called-interactively-p 'any)
    (message "tramp-rpc file-truename cache cleared")))

;;;###autoload
(defun tramp-rpc-clear-all-caches ()
  "Clear all tramp-rpc caches.
Call this after making significant changes to the remote filesystem."
  (interactive)
  (tramp-rpc-magit--clear-cache)
  (tramp-rpc-clear-file-exists-cache)
  (tramp-rpc-clear-file-truename-cache)
  (when (called-interactively-p 'any)
    (message "All tramp-rpc caches cleared")))

;; ============================================================================
;; Server notification handling (inotify-based cache invalidation)
;; ============================================================================

(defvar tramp-rpc--suppress-fs-notifications nil
  "When non-nil, suppress fs.changed cache invalidation.
Bound during magit refresh to prevent inotify notifications from
clearing caches mid-refresh (git commands touch .git/index etc.).")

(defvar tramp-rpc--watched-directories (make-hash-table :test 'equal)
  "Hash table of directories being watched via inotify.
Keys are \"connection-key:path\" strings, values are t.")

(defvar tramp-rpc--watch-debug nil
  "When non-nil, log filesystem watch events.")

(defun tramp-rpc--connection-key-string (vec)
  "Return a string representation of VEC's connection key for use in hash keys."
  (let ((key (tramp-rpc--connection-key vec)))
    (format "%s" key)))

(defun tramp-rpc--directory-watched-p (localname vec)
  "Return non-nil if LOCALNAME (or a parent) is being watched for VEC.
Checks if any watched directory is a prefix of LOCALNAME."
  (let ((conn-key (tramp-rpc--connection-key-string vec))
        (found nil))
    (maphash (lambda (key _val)
               (when (string-match (format "^%s:\\(.+\\)$" (regexp-quote conn-key)) key)
                 (let ((watched-path (match-string 1 key)))
                   (when (string-prefix-p watched-path localname)
                     (setq found t)))))
             tramp-rpc--watched-directories)
    found))

(defun tramp-rpc--handle-notification (process method params)
  "Handle a server-initiated notification.
PROCESS is the RPC connection process.
METHOD is the notification method name.
PARAMS is the notification parameters."
  (pcase method
    ("fs.changed"
     (tramp-rpc--handle-fs-changed process params))
    (_
     (when tramp-rpc--watch-debug
       (message "tramp-rpc: unknown notification method: %s" method)))))

(defun tramp-rpc--handle-fs-changed (_process params)
  "Handle an fs.changed notification by invalidating caches.
PARAMS is an alist with a `paths' key containing a list of changed paths.
When `tramp-rpc--suppress-fs-notifications' is non-nil (e.g. during
magit refresh), notifications are suppressed.  The advice unwind-protect
flushes file-exists and file-truename caches after the refresh completes."
  (let ((paths (alist-get 'paths params)))
    (when tramp-rpc--watch-debug
      (message "tramp-rpc: fs.changed notification, %d paths%s"
               (length paths)
               (if tramp-rpc--suppress-fs-notifications " (suppressed)" "")))
    (unless tramp-rpc--suppress-fs-notifications
      ;; Clear all caches that could be stale.
      (clrhash tramp-rpc--file-exists-cache)
      (clrhash tramp-rpc--file-truename-cache)
      ;; Also clear the magit process-file cache since git state may have changed.
      (tramp-rpc-magit--clear-status-cache))))

;;;###autoload
(defun tramp-rpc-watch-directory (directory &optional non-recursive)
  "Start watching DIRECTORY on the remote server for filesystem changes.
When changes are detected, local caches are automatically invalidated.
By default, subdirectories are watched recursively.
If NON-RECURSIVE is non-nil, only the directory itself is watched.

This uses inotify (Linux) or kqueue (macOS) on the remote server."
  (interactive
   (list (read-directory-name "Watch directory: " nil nil t)
         nil))
  (when (and (file-remote-p directory)
             (tramp-rpc-file-name-p directory))
    (with-parsed-tramp-file-name directory nil
      (let* ((recursive (not non-recursive))
             (result (tramp-rpc--call v "watch.add"
                                      `((path . ,localname)
                                        (recursive . ,(if recursive t :json-false))))))
        (when result
          (let ((key (format "%s:%s" (tramp-rpc--connection-key-string v) localname)))
            (puthash key t tramp-rpc--watched-directories))
          (when (or tramp-rpc--watch-debug (called-interactively-p 'any))
            (message "tramp-rpc: watching %s%s"
                     localname
                     (if recursive " (recursive)" ""))))))))

;;;###autoload
(defun tramp-rpc-unwatch-directory (directory)
  "Stop watching DIRECTORY on the remote server."
  (interactive
   (list (read-directory-name "Unwatch directory: " nil nil t)))
  (when (and (file-remote-p directory)
             (tramp-rpc-file-name-p directory))
    (with-parsed-tramp-file-name directory nil
      (let ((result (tramp-rpc--call v "watch.remove"
                                     `((path . ,localname)))))
        (when result
          (let ((key (format "%s:%s" (tramp-rpc--connection-key-string v) localname)))
            (remhash key tramp-rpc--watched-directories))
          (when (or tramp-rpc--watch-debug (called-interactively-p 'any))
            (message "tramp-rpc: unwatched %s" localname)))))))

;;;###autoload
(defun tramp-rpc-list-watches ()
  "List currently active filesystem watches on the remote server."
  (interactive)
  (if (not (file-remote-p default-directory))
      (message "Not in a remote directory")
    (with-parsed-tramp-file-name default-directory nil
      (let ((result (tramp-rpc--call v "watch.list" nil)))
        (if (and result (> (length result) 0))
            (message "Active watches:\n%s"
                     (mapconcat (lambda (w)
                                  (format "  %s%s"
                                          (alist-get 'path w)
                                          (if (eq (alist-get 'recursive w) t)
                                              " (recursive)" "")))
                                result "\n"))
          (message "No active watches"))))))

(defun tramp-rpc--auto-watch-git-worktree (vec toplevel)
  "Automatically watch a git worktree after prefetch.
VEC is the TRAMP connection vector.  TOPLEVEL is the local path
of the git worktree root on the remote."
  (when toplevel
    (let ((key (format "%s:%s" (tramp-rpc--connection-key-string vec) toplevel)))
      (unless (gethash key tramp-rpc--watched-directories)
        ;; Not yet watching this worktree - start watching
        (condition-case err
            (let ((result (tramp-rpc--call vec "watch.add"
                                           `((path . ,toplevel)
                                             (recursive . t)))))
              (when result
                (puthash key t tramp-rpc--watched-directories)
                (when tramp-rpc--watch-debug
                  (message "tramp-rpc: auto-watching git worktree %s" toplevel))))
          (error
           (when tramp-rpc--watch-debug
             (message "tramp-rpc: failed to auto-watch %s: %s"
                      toplevel (error-message-string err)))))))))

;; ============================================================================
;; Magit integration - client-side parallel prefetch
;; ============================================================================

;; The prefetch sends all git commands magit will need via a single
;; commands.run_parallel RPC call.  The server runs them in parallel
;; using OS threads and returns {key: {exit_code, stdout, stderr}}.
;; The results are stored directly as the process-file cache —
;; no reconstruction or key normalization needed.

(defvar tramp-rpc-magit--process-file-cache nil
  "Hash table mapping git arg keys to cached results.
Each value is (exit-code . stdout-string).  Built directly from
the commands.run_parallel response during prefetch.")

(defvar tramp-rpc-magit--ancestors-cache nil
  "Cached ancestor scan data from server-side RPC.
This is populated by `tramp-rpc-magit--prefetch' for file existence checks.")

(defvar tramp-rpc-magit--prefetch-directory nil
  "The directory that was prefetched.
Used to answer file-exists-p queries for the directory itself.")

(defvar tramp-rpc-magit--debug nil
  "When non-nil, log cache hits/misses for debugging.")

(defun tramp-rpc-magit--process-cache-key (&rest args)
  "Build a cache key from git ARGS.
Joins args with | as separator for hash table lookup."
  (mapconcat #'identity args "|"))

(defconst tramp-rpc-magit--state-files
  '("MERGE_HEAD" "REVERT_HEAD" "CHERRY_PICK_HEAD" "ORIG_HEAD"
    "FETCH_HEAD" "AUTO_MERGE" "SQUASH_MSG"
    "BISECT_LOG" "BISECT_CMD_OUTPUT" "BISECT_TERMS"
    "rebase-merge" "rebase-merge/git-rebase-todo"
    "rebase-merge/done" "rebase-merge/onto"
    "rebase-merge/orig-head" "rebase-merge/head-name"
    "rebase-merge/amend" "rebase-merge/stopped-sha"
    "rebase-merge/rewritten-pending"
    "rebase-apply" "rebase-apply/onto"
    "rebase-apply/head-name" "rebase-apply/applying"
    "rebase-apply/original-commit" "rebase-apply/rewritten"
    "sequencer" "sequencer/todo" "sequencer/head"
    "HEAD" "config" "index" "refs/stash"
    "info/exclude" "NOTES_MERGE_WORKTREE")
  "Git state files that magit checks for existence under .git/.
These are checked speculatively during prefetch (assuming .git as
the gitdir) and the results are cached in `tramp-rpc--file-exists-cache'.")

(defun tramp-rpc-magit--prefetch-git-commands (directory)
  "Build the list of git commands to prefetch for DIRECTORY.
Returns a vector of command entries for commands.run_parallel.
Each entry has key, cmd, args, and cwd fields.  Git command keys
match what `tramp-rpc-magit--process-cache-lookup' will look up.
State file checks use \"state_file:PATH\" keys."
  (let ((cmds nil)
        (gitdir (concat (file-name-as-directory directory) ".git")))
    (cl-flet ((add-git (&rest args)
                (push `((key . ,(apply #'tramp-rpc-magit--process-cache-key args))
                        (cmd . "git")
                        (args . ,(vconcat args))
                        (cwd . ,directory))
                      cmds))
              (add-state-file (relative-path)
                (let ((full-path (concat (file-name-as-directory gitdir)
                                         relative-path)))
                  (push `((key . ,(concat "state_file:" full-path))
                          (cmd . "test")
                          (args . ["-e" ,full-path]))
                        cmds))))
      ;; State file existence checks (speculative, assuming .git gitdir)
      (dolist (sf tramp-rpc-magit--state-files)
        (add-state-file sf))

      ;; Basic repo info
      (add-git "rev-parse" "--show-toplevel")
      (add-git "rev-parse" "--git-dir")

      ;; HEAD info
      (add-git "rev-parse" "HEAD")
      (add-git "rev-parse" "--short" "HEAD")
      (add-git "symbolic-ref" "--short" "HEAD")
      (add-git "log" "-1" "--format=%s" "HEAD")
      (add-git "rev-parse" "--verify" "HEAD")
      (add-git "symbolic-ref" "HEAD")

      ;; Upstream / push
      (add-git "rev-parse" "--abbrev-ref" "@{upstream}")
      (add-git "rev-list" "--count" "--left-right" "@{upstream}...HEAD")
      (add-git "rev-parse" "--abbrev-ref" "@{push}")
      (add-git "rev-list" "--count" "--left-right" "@{push}...HEAD")

      ;; Diffs
      (add-git "diff" "--ita-visible-in-index" "--no-ext-diff" "--no-prefix" "--")
      (add-git "diff" "--ita-visible-in-index" "--cached" "--no-ext-diff" "--no-prefix" "--")
      (add-git "diff" "--cached" "--stat" "--no-color")
      (add-git "diff" "--stat" "--no-color")

      ;; Untracked files
      (add-git "ls-files" "--others" "--exclude-standard" "--directory" "--no-empty-directory")

      ;; Tags
      (add-git "describe" "--tags" "--exact-match" "HEAD")
      (add-git "describe" "--tags" "--abbrev=0")
      (add-git "describe" "--long" "--tags")
      (add-git "describe" "--contains" "HEAD")

      ;; Remotes
      (add-git "remote")
      (add-git "remote" "get-url" "origin")

      ;; Config
      (add-git "config" "user.name")
      (add-git "config" "user.email")
      (add-git "config" "remote.origin.url")
      (add-git "config" "--bool" "--default" "false" "core.bare")
      (add-git "config" "--list" "-z")
      (add-git "config" "--local" "-z" "--get-all" "--include" "status.showUntrackedFiles")

      ;; Porcelain status
      (add-git "status" "-z" "--porcelain" "--untracked-files=normal" "--")
      (add-git "status" "--porcelain" "--branch")

      ;; Index refresh (read-only, safe to prefetch)
      (add-git "update-index" "--refresh")

      ;; Bare repo check
      (add-git "rev-parse" "--is-bare-repository")

      ;; Stash
      (add-git "rev-parse" "--verify" "refs/stash")
      (add-git "reflog" "--format=%gd%x00%aN%x00%at%x00%gs" "refs/stash")

      ;; Parent commits
      (add-git "rev-parse" "--short" "HEAD~")
      (add-git "rev-parse" "--verify" "HEAD~10")

      ;; Recent log with decorations
      (add-git "log" "--format=%h%x0c%D%x0c%x0c%aN%x0c%at%x0c%x0c%s"
               "--decorate=full" "-n10" "--use-mailmap" "--no-prefix" "--")

      ;; Log for header line
      (add-git "log" "--no-walk" "--format=%h %s" "HEAD^{commit}" "--"))

    (vconcat (nreverse cmds))))

(defun tramp-rpc-magit--strip-git-prefix-args (args)
  "Strip magit's prefix flags from git ARGS to get the core command.
Magit prepends --no-pager, --literal-pathspecs, and -c key=value
before the actual subcommand.  We strip these to normalize the key."
  (let ((rest (append args nil)))  ; copy list from vector if needed
    (while (and rest
                (let ((arg (car rest)))
                  (cond
                   ;; Skip --no-pager, --literal-pathspecs, etc.
                   ((string-prefix-p "--no-pager" arg) t)
                   ((string-prefix-p "--literal-pathspecs" arg) t)
                   ((string-prefix-p "--glob-pathspecs" arg) t)
                   ((string-prefix-p "--noglob-pathspecs" arg) t)
                   ;; Skip -c key=value (two args: -c then key=value)
                   ((string= "-c" arg)
                    (setq rest (cdr rest))  ; skip the value too
                    t)
                   ;; Skip -C dir (two args)
                   ((string= "-C" arg)
                    (setq rest (cdr rest))
                    t)
                   (t nil))))
      (setq rest (cdr rest)))
    rest))

(defun tramp-rpc-magit--process-cache-lookup (program args)
  "Look up PROGRAM ARGS in the process-file cache.
Returns (exit-code . stdout) if found, nil otherwise.
Only matches git commands.  Strips magit's prefix flags
\(--no-pager, -c key=value, etc.) to normalize the key."
  (when (and tramp-rpc-magit--process-file-cache
             ;; Only intercept git commands
             (or (string-suffix-p "/git" program)
                 (string= "git" program)))
    (let* ((core-args (tramp-rpc-magit--strip-git-prefix-args args))
           (key (apply #'tramp-rpc-magit--process-cache-key core-args))
           (result (gethash key tramp-rpc-magit--process-file-cache)))
      (when tramp-rpc-magit--debug
        (if result
            (message "process-file HIT (prefetch): git %s -> exit %d"
                     key (car result))
          (message "process-file MISS (prefetch): git %s" key)))
      result)))

(defun tramp-rpc-magit--prefetch (directory)
  "Prefetch magit status and ancestor data for DIRECTORY.
Sends all git commands magit will need via a single
commands.run_parallel RPC call, then stores the results directly
as the process-file cache.  Also fetches ancestor markers."
  (when (and (file-remote-p directory)
             (tramp-rpc-file-name-p directory))
    ;; Suppress fs.changed notifications during prefetch.
    ;; The git commands we run on the server touch .git/index etc.,
    ;; triggering inotify events that would clear the cache we're building.
    (let ((tramp-rpc--suppress-fs-notifications t))
      ;; Remember the directory we prefetched for
      (setq tramp-rpc-magit--prefetch-directory (expand-file-name directory))
      (with-parsed-tramp-file-name directory nil
        ;; Build command list and run in parallel on server
        (let* ((commands (tramp-rpc-magit--prefetch-git-commands localname))
               (results (tramp-rpc--call v "commands.run_parallel"
                                         `((commands . ,commands)))))
          (when results
            ;; Build process-file cache directly from results.
            ;; Each result entry is (key . {exit_code, stdout, stderr}).
            ;; Git command results are stored as (exit-code . decoded-stdout).
            ;; State file results (key starts with "state_file:") are stored
            ;; in the file-exists cache instead.
            (let ((cache (make-hash-table :test 'equal))
                  (remote-prefix (file-remote-p directory)))
              (dolist (entry results)
                (let* ((key (if (symbolp (car entry))
                                (symbol-name (car entry))
                              (car entry)))
                       (data (cdr entry))
                       (exit-code (alist-get 'exit_code data)))
                  (if (string-prefix-p "state_file:" key)
                      ;; State file check: exit 0 = exists, non-zero = doesn't
                      (let* ((remote-path (substring key (length "state_file:")))
                             (tramp-path (concat remote-prefix remote-path)))
                        (tramp-rpc--cache-put tramp-rpc--file-exists-cache
                                              tramp-path
                                              (= exit-code 0)))
                    ;; Git command: store in process-file cache
                    (let* ((stdout-raw (alist-get 'stdout data))
                           (stdout (tramp-rpc--decode-output stdout-raw nil)))
                      (puthash key (cons exit-code stdout) cache)))))
              (setq tramp-rpc-magit--process-file-cache cache))
            ;; Auto-watch the git worktree
            (let* ((toplevel-key (tramp-rpc-magit--process-cache-key
                                  "rev-parse" "--show-toplevel"))
                   (toplevel-entry (gethash toplevel-key
                                            tramp-rpc-magit--process-file-cache))
                   (toplevel (when (and toplevel-entry (= 0 (car toplevel-entry)))
                               (string-trim (cdr toplevel-entry)))))
              (when toplevel
                (tramp-rpc--auto-watch-git-worktree v toplevel)))))
        ;; Fetch ancestor markers for project/VC detection
        (setq tramp-rpc-magit--ancestors-cache
              (tramp-rpc-ancestors-scan directory
                                        '(".git" ".svn" ".hg" ".bzr" "_darcs"
                                          ".projectile" ".project" ".dir-locals.el"
                                          ".editorconfig")))
        (when tramp-rpc-magit--debug
          (message "tramp-rpc-magit: prefetched %d commands + ancestors for %s"
                   (if tramp-rpc-magit--process-file-cache
                       (hash-table-count tramp-rpc-magit--process-file-cache)
                     0)
                   directory))))))

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
        (mapcar (lambda (pair)
                  (let ((key (car pair))
                        (val (cdr pair)))
                    (cons (if (symbolp key) (symbol-name key) key)
                          (when val
                            (decode-coding-string val 'utf-8)))))
                result)))))

(defun tramp-rpc-magit--clear-status-cache ()
  "Clear only the status cache (git state that changes frequently)."
  (setq tramp-rpc-magit--process-file-cache nil))

(defun tramp-rpc-magit--clear-cache ()
  "Clear all magit-related caches."
  (setq tramp-rpc-magit--process-file-cache nil)
  (setq tramp-rpc-magit--ancestors-cache nil)
  (setq tramp-rpc-magit--prefetch-directory nil))

;; ============================================================================
;; Magit ancestor cache lookups
;; ============================================================================

(defun tramp-rpc-magit--file-exists-p (filename)
  "Check if FILENAME exists using cached ancestor data.
Returns t, nil, or \\='not-cached if not in cache.
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

;; ============================================================================
;; Magit advice
;; ============================================================================

(defun tramp-rpc-magit--advice-setup-buffer (orig-fun directory &rest args)
  "Advice around `magit-status-setup-buffer' to prefetch data.
Suppresses fs.changed notifications during refresh to prevent
inotify events (from git commands touching .git/index etc.) from
clearing caches mid-refresh."
  (when (and (file-remote-p directory)
             (tramp-rpc-file-name-p directory))
    (tramp-rpc-magit--prefetch directory))
  (let ((tramp-rpc--suppress-fs-notifications t))
    (unwind-protect
        (apply orig-fun directory args)
      ;; Clear process-file cache - ancestors/prefetch-dir stay for other packages
      (tramp-rpc-magit--clear-status-cache)
      ;; Flush file caches since we suppressed fs.changed during the refresh
      (clrhash tramp-rpc--file-exists-cache)
      (clrhash tramp-rpc--file-truename-cache))))

(defun tramp-rpc-magit--advice-refresh-buffer (orig-fun &rest args)
  "Advice around `magit-status-refresh-buffer' to prefetch data.
Suppresses fs.changed notifications during refresh to prevent
inotify events from clearing caches mid-refresh."
  (when (and (file-remote-p default-directory)
             (tramp-rpc-file-name-p default-directory)
             (null tramp-rpc-magit--process-file-cache))
    (tramp-rpc-magit--prefetch default-directory))
  (let ((tramp-rpc--suppress-fs-notifications t))
    (unwind-protect
        (apply orig-fun args)
      ;; Clear process-file cache - ancestors/prefetch-dir stay for other packages
      (tramp-rpc-magit--clear-status-cache)
      ;; Flush file caches since we suppressed fs.changed during the refresh
      (clrhash tramp-rpc--file-exists-cache)
      (clrhash tramp-rpc--file-truename-cache))))

;;;###autoload
(defun tramp-rpc-magit-enable ()
  "Enable tramp-rpc magit optimizations.
This uses parallel command prefetching to dramatically speed up
magit-status on remote repositories."
  (interactive)
  (advice-add 'magit-status-setup-buffer :around
              #'tramp-rpc-magit--advice-setup-buffer)
  (advice-add 'magit-status-refresh-buffer :around
              #'tramp-rpc-magit--advice-refresh-buffer)
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
      (let ((files nil))
        ;; Check cache first (like projectile-project-files does)
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

(provide 'tramp-rpc-magit)
;;; tramp-rpc-magit.el ends here
