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
;; - Server-side magit.status RPC for fast magit-status on remote repos
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
      ;; This is the simplest correct approach: the caches refill on demand
      ;; in a single RPC round-trip each. More granular invalidation
      ;; (matching individual paths) can be added later if needed.
      (clrhash tramp-rpc--file-exists-cache)
      (clrhash tramp-rpc--file-truename-cache)
      ;; Also clear the magit status cache since git state files or
      ;; worktree contents may have changed.
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

(defun tramp-rpc--auto-watch-git-worktree (directory status-data)
  "Automatically watch a git worktree after magit.status returns.
DIRECTORY is the TRAMP directory. STATUS-DATA is the magit.status result."
  (when-let* ((toplevel (alist-get 'toplevel status-data)))
    (with-parsed-tramp-file-name directory nil
      (let ((key (format "%s:%s" (tramp-rpc--connection-key-string v) toplevel)))
        (unless (gethash key tramp-rpc--watched-directories)
          ;; Not yet watching this worktree - start watching
          (condition-case err
              (let ((result (tramp-rpc--call v "watch.add"
                                             `((path . ,toplevel)
                                               (recursive . t)))))
                (when result
                  (puthash key t tramp-rpc--watched-directories)
                  (when tramp-rpc--watch-debug
                    (message "tramp-rpc: auto-watching git worktree %s" toplevel))))
            (error
             (when tramp-rpc--watch-debug
               (message "tramp-rpc: failed to auto-watch %s: %s"
                        toplevel (error-message-string err))))))))))

;; ============================================================================
;; Magit integration - server-side optimized status
;; ============================================================================

;; This uses the server-side magit.status RPC which returns all data needed
;; for magit-status in a single call, eliminating ~60 individual git commands.

(defvar tramp-rpc-magit--status-cache nil
  "Cached magit status data from server-side RPC.
Populated by `tramp-rpc-magit--prefetch', used by process-file advice.")

(defvar tramp-rpc-magit--process-file-cache nil
  "Hash table mapping git arg keys to cached results.
Each value is (exit-code . stdout).  Built from the magit.status
prefetch by `tramp-rpc-magit--build-process-cache'.")

(defvar tramp-rpc-magit--ancestors-cache nil
  "Cached ancestor scan data from server-side RPC.
This is populated by `tramp-rpc-magit--prefetch' for file existence checks.")

(defvar tramp-rpc-magit--prefetch-directory nil
  "The directory that was prefetched.
Used to answer file-exists-p queries for the directory itself.")

(defvar tramp-rpc-magit--debug nil
  "When non-nil, log cache hits/misses for debugging.")

;;;###autoload
(defun tramp-rpc-magit-status (directory)
  "Get complete magit status for DIRECTORY using server-side RPC.
Returns an alist with all data needed for magit-status:
  toplevel, gitdir, head, upstream, push, state, staged, unstaged,
  untracked, tags, remotes, config, state_files, config_list,
  describe_long, describe_contains, status_porcelain, config_untracked,
  stash_reflog, head_parent_short, head_parent_10, recent_decorated.

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

(defun tramp-rpc-magit--process-cache-key (&rest args)
  "Build a cache key from git ARGS.
Joins args with | as separator for hash table lookup."
  (mapconcat #'identity args "|"))

(defun tramp-rpc-magit--build-process-cache (status-data)
  "Build a process-file cache from STATUS-DATA.
Returns a hash table mapping git arg keys to (exit-code . stdout).
The server's magit.status already ran all these commands in parallel;
this lets us serve the results without additional RPCs.

NOTE: Cache keys must match the exact argument patterns that magit
sends after stripping prefix flags (--no-pager, --literal-pathspecs,
-c key=value).  These patterns may change across magit versions.
Enable `tramp-rpc-magit--debug' to log cache misses."
  (let ((cache (make-hash-table :test 'equal))
        (toplevel (alist-get 'toplevel status-data))
        (gitdir (alist-get 'gitdir status-data))
        (head (alist-get 'head status-data))
        (upstream (alist-get 'upstream status-data))
        (push-info (alist-get 'push status-data))
        (staged (alist-get 'staged status-data))
        (unstaged (alist-get 'unstaged status-data))
        (untracked (alist-get 'untracked status-data))
        (tags (alist-get 'tags status-data))
        (remotes (alist-get 'remotes status-data))
        (state (alist-get 'state status-data))
        (config (alist-get 'config status-data))
        ;; Raw output fields matching magit's exact invocations
        (config-list (alist-get 'config_list status-data))
        (describe-long (alist-get 'describe_long status-data))
        (describe-contains (alist-get 'describe_contains status-data))
        (status-porcelain (alist-get 'status_porcelain status-data))
        (config-untracked (alist-get 'config_untracked status-data))
        (stash-reflog (alist-get 'stash_reflog status-data))
        (head-parent-short (alist-get 'head_parent_short status-data))
        (head-parent-10 (alist-get 'head_parent_10 status-data))
        (recent-decorated (alist-get 'recent_decorated status-data)))
    ;; Helper: put an entry; exit-code 0 for Some, 1 for None
    (cl-flet ((put-result (key value)
                (puthash key
                         (if value (cons 0 (concat value "\n")) (cons 1 ""))
                         cache))
              (put-exact (key exit-code stdout)
                (puthash key (cons exit-code stdout) cache)))
      ;; Basic repo info
      (put-result (tramp-rpc-magit--process-cache-key "rev-parse" "--show-toplevel")
                  toplevel)
      (put-result (tramp-rpc-magit--process-cache-key "rev-parse" "--git-dir")
                  gitdir)

      ;; HEAD info
      (when head
        (put-result (tramp-rpc-magit--process-cache-key "rev-parse" "HEAD")
                    (alist-get 'hash head))
        (put-result (tramp-rpc-magit--process-cache-key "rev-parse" "--short" "HEAD")
                    (alist-get 'short head))
        (put-result (tramp-rpc-magit--process-cache-key "symbolic-ref" "--short" "HEAD")
                    (alist-get 'branch head))
        (put-result (tramp-rpc-magit--process-cache-key "log" "-1" "--format=%s" "HEAD")
                    (alist-get 'message head))
        (put-result (tramp-rpc-magit--process-cache-key "rev-parse" "--verify" "HEAD")
                    (alist-get 'hash head))
        ;; symbolic-ref HEAD (full) - derive from branch
        (when (alist-get 'branch head)
          (put-result (tramp-rpc-magit--process-cache-key "symbolic-ref" "HEAD")
                      (concat "refs/heads/" (alist-get 'branch head)))))

      ;; Upstream info
      (when upstream
        (put-result (tramp-rpc-magit--process-cache-key
                     "rev-parse" "--abbrev-ref" "@{upstream}")
                    (alist-get 'branch upstream))
        (let ((ahead (alist-get 'ahead upstream))
              (behind (alist-get 'behind upstream)))
          (when (or ahead behind)
            (put-exact (tramp-rpc-magit--process-cache-key
                        "rev-list" "--count" "--left-right" "@{upstream}...HEAD")
                       0
                       (format "%s\t%s\n"
                               (or behind 0) (or ahead 0))))))

      ;; Push remote info
      (when push-info
        (put-result (tramp-rpc-magit--process-cache-key
                     "rev-parse" "--abbrev-ref" "@{push}")
                    (alist-get 'branch push-info))
        (let ((ahead (alist-get 'ahead push-info))
              (behind (alist-get 'behind push-info)))
          (when (or ahead behind)
            (put-exact (tramp-rpc-magit--process-cache-key
                        "rev-list" "--count" "--left-right" "@{push}...HEAD")
                       0
                       (format "%s\t%s\n"
                               (or behind 0) (or ahead 0))))))

      ;; Diff: magit uses --ita-visible-in-index --no-ext-diff --no-prefix
      ;; (server now runs these exact flags)
      ;; Unstaged diff
      (let ((diff (when unstaged (alist-get 'diff unstaged))))
        (put-exact (tramp-rpc-magit--process-cache-key
                    "diff" "--ita-visible-in-index"
                    "--no-ext-diff" "--no-prefix" "--")
                   0 (or diff "")))
      ;; Staged (cached) diff
      (let ((diff (when staged (alist-get 'diff staged))))
        (put-exact (tramp-rpc-magit--process-cache-key
                    "diff" "--ita-visible-in-index" "--cached"
                    "--no-ext-diff" "--no-prefix" "--")
                   0 (or diff "")))

      ;; Diff stat (magit may still use --stat --no-color variants)
      (when staged
        (let ((stat (alist-get 'stat staged)))
          (when stat
            (put-result (tramp-rpc-magit--process-cache-key
                         "diff" "--cached" "--stat" "--no-color")
                        stat))))
      (when unstaged
        (let ((stat (alist-get 'stat unstaged)))
          (when stat
            (put-result (tramp-rpc-magit--process-cache-key
                         "diff" "--stat" "--no-color")
                        stat))))

      ;; Untracked files
      (when untracked
        (put-exact (tramp-rpc-magit--process-cache-key
                    "ls-files" "--others" "--exclude-standard"
                    "--directory" "--no-empty-directory")
                   0
                   (if (listp untracked)
                       (concat (mapconcat #'identity untracked "\n") "\n")
                     (concat untracked "\n"))))

      ;; Tags
      (when tags
        (put-result (tramp-rpc-magit--process-cache-key
                     "describe" "--tags" "--exact-match" "HEAD")
                    (alist-get 'at_head tags))
        (put-result (tramp-rpc-magit--process-cache-key
                     "describe" "--tags" "--abbrev=0")
                    (alist-get 'latest tags)))

      ;; Remotes
      (when remotes
        (put-exact (tramp-rpc-magit--process-cache-key "remote")
                   0
                   (if (listp remotes)
                       (concat (mapconcat #'identity remotes "\n") "\n")
                     (concat remotes "\n"))))

      ;; Repo state (empty string = no special state)
      (when state
        (put-exact (tramp-rpc-magit--process-cache-key
                    "status" "--porcelain" "--branch")
                   0 ""))

      ;; Config values (individual key lookups)
      (when config
        (dolist (pair config)
          (let ((key (if (symbolp (car pair))
                         (symbol-name (car pair))
                       (car pair)))
                (val (cdr pair)))
            (when val
              (put-result (tramp-rpc-magit--process-cache-key "config" key)
                          val)))))

      ;; update-index --refresh: safe to return success (read-only refresh)
      (put-exact (tramp-rpc-magit--process-cache-key "update-index" "--refresh")
                 0 "")

      ;; rev-parse --is-bare-repository: derive from config core.bare
      (when config
        (let ((bare (cdr (or (assoc 'core.bare config)
                             (assoc "core.bare" config)))))
          (put-exact (tramp-rpc-magit--process-cache-key
                      "rev-parse" "--is-bare-repository")
                     0
                     (concat (or bare "false") "\n"))))

      ;; HEAD log variants that magit uses
      (when head
        (let ((short (alist-get 'short head))
              (message (alist-get 'message head)))
          (when (and short message)
            (put-exact (tramp-rpc-magit--process-cache-key
                        "log" "--no-walk" "--format=%h %s"
                        "HEAD^{commit}" "--")
                       0 (format "%s %s\n" short message)))))

      ;; Stash ref verification (derive from stash_reflog presence)
      (if stash-reflog
          (put-exact (tramp-rpc-magit--process-cache-key
                      "rev-parse" "--verify" "refs/stash")
                     0 "refs/stash\n")
        (put-exact (tramp-rpc-magit--process-cache-key
                    "rev-parse" "--verify" "refs/stash")
                   1 ""))

      ;; remote get-url origin: derive from config
      (when config
        (let ((url (cdr (or (assoc 'remote.origin.url config)
                            (assoc "remote.origin.url" config)))))
          (when url
            (put-result (tramp-rpc-magit--process-cache-key
                         "remote" "get-url" "origin")
                        url))))

      ;; === Raw output fields matching magit's exact invocations ===

      ;; config --list -z (raw NUL-separated output)
      (when config-list
        (put-exact (tramp-rpc-magit--process-cache-key "config" "--list" "-z")
                   0 (if (stringp config-list)
                         config-list
                       (decode-coding-string config-list 'utf-8-unix))))

      ;; describe --long --tags
      (put-result (tramp-rpc-magit--process-cache-key
                   "describe" "--long" "--tags")
                  (when (stringp describe-long)
                    (decode-coding-string describe-long 'utf-8-unix)))

      ;; describe --contains HEAD
      (put-result (tramp-rpc-magit--process-cache-key
                   "describe" "--contains" "HEAD")
                  (when (stringp describe-contains)
                    (decode-coding-string describe-contains 'utf-8-unix)))

      ;; status -z --porcelain --untracked-files=normal --
      (put-exact (tramp-rpc-magit--process-cache-key
                  "status" "-z" "--porcelain"
                  "--untracked-files=normal" "--")
                 0 (cond
                    ((stringp status-porcelain) status-porcelain)
                    (status-porcelain (decode-coding-string status-porcelain 'utf-8-unix))
                    (t "")))

      ;; config --local -z --get-all --include status.showUntrackedFiles
      (put-exact (tramp-rpc-magit--process-cache-key
                  "config" "--local" "-z" "--get-all"
                  "--include" "status.showUntrackedFiles")
                 (if config-untracked 0 1)
                 (if (stringp config-untracked)
                     (decode-coding-string config-untracked 'utf-8-unix)
                   ""))

      ;; reflog --format=%gd%x00%aN%x00%at%x00%gs refs/stash
      (put-exact (tramp-rpc-magit--process-cache-key
                  "reflog" "--format=%gd%x00%aN%x00%at%x00%gs"
                  "refs/stash")
                 (if stash-reflog 0 128)
                 (cond
                  ((stringp stash-reflog) stash-reflog)
                  (stash-reflog (decode-coding-string stash-reflog 'utf-8-unix))
                  (t "")))

      ;; rev-parse --short HEAD~
      (put-result (tramp-rpc-magit--process-cache-key
                   "rev-parse" "--short" "HEAD~")
                  (when (stringp head-parent-short)
                    (decode-coding-string head-parent-short 'utf-8-unix)))

      ;; rev-parse --verify HEAD~10
      (put-result (tramp-rpc-magit--process-cache-key
                   "rev-parse" "--verify" "HEAD~10")
                  (when (stringp head-parent-10)
                    (decode-coding-string head-parent-10 'utf-8-unix)))

      ;; log --format=... --decorate=full -n10 --use-mailmap --no-prefix --
      (when recent-decorated
        (put-exact (tramp-rpc-magit--process-cache-key
                    "log"
                    "--format=%h%x0c%D%x0c%x0c%aN%x0c%at%x0c%x0c%s"
                    "--decorate=full" "-n10"
                    "--use-mailmap" "--no-prefix" "--")
                   0 (if (stringp recent-decorated)
                         recent-decorated
                       (decode-coding-string recent-decorated 'utf-8-unix)))))

    (when tramp-rpc-magit--debug
      (message "tramp-rpc-magit: built process cache with %d entries"
               (hash-table-count cache)))
    cache))

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
Populates `tramp-rpc-magit--status-cache' and ancestors cache.
Suppresses fs.changed notifications during RPC calls to prevent
the server's git commands from triggering inotify events that
would clear our caches while we're still populating them."
  (when (and (file-remote-p directory)
             (tramp-rpc-file-name-p directory))
    ;; Suppress fs.changed notifications during prefetch.
    ;; The magit.status RPC runs git commands on the server which touch
    ;; .git/index etc., triggering inotify events that arrive as
    ;; fs.changed notifications during accept-process-output.  Without
    ;; suppression, these would clear the cache we're building.
    (let ((tramp-rpc--suppress-fs-notifications t))
      ;; Remember the directory we prefetched for
      (setq tramp-rpc-magit--prefetch-directory (expand-file-name directory))
      ;; Fetch magit status
      (setq tramp-rpc-magit--status-cache (tramp-rpc-magit-status directory))
      ;; Build process-file cache from status data
      (setq tramp-rpc-magit--process-file-cache
            (when tramp-rpc-magit--status-cache
              (tramp-rpc-magit--build-process-cache tramp-rpc-magit--status-cache)))
      ;; Auto-watch the git worktree for cache invalidation
      (when tramp-rpc-magit--status-cache
        (tramp-rpc--auto-watch-git-worktree directory tramp-rpc-magit--status-cache))
      ;; Fetch ancestor markers for project/VC detection
      (setq tramp-rpc-magit--ancestors-cache
            (tramp-rpc-ancestors-scan directory
                                      '(".git" ".svn" ".hg" ".bzr" "_darcs"
                                        ".projectile" ".project" ".dir-locals.el"
                                        ".editorconfig")))
      (when tramp-rpc-magit--debug
        (message "tramp-rpc-magit: prefetched status and ancestors for %s" directory)))))

(defun tramp-rpc-magit--clear-status-cache ()
  "Clear only the status cache (git state that changes frequently)."
  (setq tramp-rpc-magit--status-cache nil)
  (setq tramp-rpc-magit--process-file-cache nil))

(defun tramp-rpc-magit--clear-cache ()
  "Clear all magit-related caches."
  (setq tramp-rpc-magit--status-cache nil)
  (setq tramp-rpc-magit--process-file-cache nil)
  (setq tramp-rpc-magit--ancestors-cache nil)
  (setq tramp-rpc-magit--prefetch-directory nil))

;; ============================================================================
;; Magit ancestor / state file cache lookups
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

(defun tramp-rpc-magit--state-file-exists-p (filename)
  "Check if git state FILENAME exists using cached status data.
Returns t, nil, or \\='not-cached if not in cache.
Uses the gitdir from status-cache to match paths correctly."
  (if (and tramp-rpc-magit--status-cache
           tramp-rpc-magit--prefetch-directory
           (alist-get 'state_files tramp-rpc-magit--status-cache))
      (let* ((expanded (expand-file-name filename))
             (file-local (tramp-file-local-name expanded))
             (state-files (alist-get 'state_files tramp-rpc-magit--status-cache))
             ;; Get gitdir from status cache (could be relative like ".git"
             ;; or absolute like "/path/to/.git")
             (gitdir-raw (alist-get 'gitdir tramp-rpc-magit--status-cache))
             (toplevel (alist-get 'toplevel tramp-rpc-magit--status-cache))
             (abs-gitdir (when gitdir-raw
                           (if (file-name-absolute-p gitdir-raw)
                               gitdir-raw
                             (concat (file-name-as-directory toplevel)
                                     gitdir-raw))))
             (gitdir-prefix (when abs-gitdir
                              (file-name-as-directory abs-gitdir))))
        (if (and gitdir-prefix
                 (string-prefix-p gitdir-prefix file-local))
            ;; File is under the gitdir - extract relative path
            (let* ((relative (substring file-local (length gitdir-prefix)))
                   ;; Try symbol key (msgpack-key-type 'symbol) then string
                   (entry (or (assoc (intern relative) state-files)
                              (assoc relative state-files))))
              (if entry
                  (if (cdr entry) t nil)
                'not-cached))
          ;; Not under gitdir - try fallback regex for unusual layouts
          (if (string-match "/\\.git/\\(.+\\)$" file-local)
              (let* ((relative (match-string 1 file-local))
                     (entry (or (assoc (intern relative) state-files)
                                (assoc relative state-files))))
                (if entry
                    (if (cdr entry) t nil)
                  'not-cached))
            'not-cached)))
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
      ;; Clear status/process caches - ancestors/prefetch-dir stay for other packages
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
             (null tramp-rpc-magit--status-cache))
    (tramp-rpc-magit--prefetch default-directory))
  (let ((tramp-rpc--suppress-fs-notifications t))
    (unwind-protect
        (apply orig-fun args)
      ;; Clear status/process caches - ancestors/prefetch-dir stay for other packages
      (tramp-rpc-magit--clear-status-cache)
      ;; Flush file caches since we suppressed fs.changed during the refresh
      (clrhash tramp-rpc--file-exists-cache)
      (clrhash tramp-rpc--file-truename-cache))))

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

(provide 'tramp-rpc-magit)
;;; tramp-rpc-magit.el ends here
