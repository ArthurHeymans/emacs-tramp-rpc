;;; tramp-rpc-magit.el --- Caching and watch support for TRAMP-RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Keywords: comm, processes, vc
;; Package-Requires: ((emacs "30.1") (msgpack "0.1.1"))

;; This file is part of tramp-rpc.

;; tramp-rpc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This file provides caching infrastructure, filesystem watching, and
;; magit/projectile integration for tramp-rpc.  It includes:
;; - TTL-based file-exists and file-truename caches
;; - Cache invalidation via server push notifications (fs.events)
;; - Watch management (add/remove/list watched directories)
;; - Notification dispatch for filesystem change events
;; - Parallel git command prefetch for fast magit-status
;; - Process-file cache for serving git commands from prefetched data
;; - Ancestor directory scanning for project/VC detection
;; - Projectile optimizations for remote directories

;;; Code:

(require 'cl-lib)
(require 'tramp)

;; Functions from tramp.el
(declare-function tramp-add-external-operation "tramp")
(declare-function tramp-remove-external-operation "tramp")
(declare-function tramp-message "tramp-message")

;; Functions from tramp-rpc.el
(declare-function tramp-rpc--debug "tramp-rpc")
(declare-function tramp-rpc--call "tramp-rpc")
(declare-function tramp-rpc--call-batch "tramp-rpc")
(declare-function tramp-rpc--connection-key "tramp-rpc")
(declare-function tramp-rpc--decode-output "tramp-rpc")
(declare-function tramp-rpc--decode-string "tramp-rpc")
(declare-function tramp-rpc--encode-path "tramp-rpc")
(declare-function tramp-rpc--convert-file-attributes "tramp-rpc")
(declare-function tramp-rpc-file-name-p "tramp-rpc")
(declare-function tramp-rpc--canonical-watch-active-p "tramp-rpc")
(declare-function tramp-rpc--file-notify-alias-paths "tramp-rpc")
(declare-function tramp-rpc--file-notify-dispatch "tramp-rpc")
(declare-function tramp-rpc--watch-entry-canonical-directory "tramp-rpc")

;; Functions from tramp-cache.el.
(declare-function tramp-flush-file-properties "tramp-cache")

;; Functions from magit-section.el.
(declare-function magit-section-show "magit-section")

;; Silence byte-compiler warnings for external functions
(declare-function projectile-dir-files-alien "projectile")
(declare-function projectile-time-seconds "projectile")

;; Variables from magit-diff.el.
(defvar magit-diff-adjust-tab-width)

;; ============================================================================
;; Cache infrastructure
;; ============================================================================

(defvar tramp-rpc--cache-ttl 300
  "Time-to-live for cache entries in seconds.")

(defvar tramp-rpc--cache-max-size 10000
  "Maximum number of entries per cache before eviction.")

(defvar tramp-rpc--file-exists-cache (make-hash-table :test 'equal)
  "Cache for file-exists-p results.
Keys are expanded filenames, values are (TIMESTAMP . RESULT).")

(defvar tramp-rpc--file-truename-cache (make-hash-table :test 'equal)
  "Cache for file-truename results.
Keys are expanded filenames, values are (TIMESTAMP . TRUENAME).")

(defvar tramp-rpc--file-stat-cache (make-hash-table :test 'equal)
  "Cache for file.stat results.
Keys are (EXPANDED-FILENAME . LSTAT), values are (TIMESTAMP . STAT).
STAT may be nil, which records a missing file.")

(defun tramp-rpc--cache-get (cache key)
  "Get value for KEY from CACHE if not expired.
Returns the cached value, or nil if not found or expired."
  (when-let* ((entry (gethash key cache)))
    (let ((timestamp (car entry))
          (value (cdr entry)))
      (if (< (- (float-time) timestamp) tramp-rpc--cache-ttl)
          value
        ;; Expired, remove it
        (remhash key cache)
        nil))))

(defun tramp-rpc--cache-put (cache key value)
  "Store VALUE for KEY in CACHE with current timestamp.
Evicts oldest 25%% of entries when cache exceeds max size."
  ;; Check if eviction is needed
  (when (>= (hash-table-count cache) tramp-rpc--cache-max-size)
    (tramp-rpc--cache-evict cache))
  (puthash key (cons (float-time) value) cache))

(defun tramp-rpc--cache-lookup (cache key)
  "Return cached value for KEY in CACHE, or `not-cached'.
Unlike `tramp-rpc--cache-get', this preserves cached nil values."
  (let ((entry (gethash key cache)))
    (if (not entry)
        'not-cached
      (let ((timestamp (car entry))
            (value (cdr entry)))
        (if (< (- (float-time) timestamp) tramp-rpc--cache-ttl)
            value
          (remhash key cache)
          'not-cached)))))

(defun tramp-rpc--file-stat-cache-key (vec localname lstat)
  "Return file.stat cache key for VEC, LOCALNAME, and LSTAT."
  (cons (expand-file-name (tramp-make-tramp-file-name vec localname))
        (and lstat t)))

(defun tramp-rpc--cache-file-stat-result (vec localname stat &optional lstat)
  "Cache file.stat STAT for LOCALNAME on VEC.
When LSTAT is non-nil and STAT is not a symlink, also cache the following-stat
spelling because both variants return the same attributes.  A following stat
cannot safely seed lstat: a symlink to a regular file follows to file type but
must still report symlink type for lstat."
  (let ((keys (list (tramp-rpc--file-stat-cache-key vec localname lstat))))
    (when (and lstat stat (not (equal (alist-get 'type stat) "symlink")))
      (push (tramp-rpc--file-stat-cache-key vec localname (not lstat)) keys))
    (dolist (key (delete-dups keys))
      (tramp-rpc--cache-put tramp-rpc--file-stat-cache key stat))))

(defun tramp-rpc--cache-evict (cache)
  "Evict oldest 25%% of entries from CACHE."
  (let ((entries nil))
    ;; Collect all entries with timestamps
    (maphash (lambda (key value)
               (push (cons key (car value)) entries))
             cache)
    ;; Sort by timestamp (oldest first)
    (setq entries (sort entries (lambda (a b) (< (cdr a) (cdr b)))))
    ;; Remove oldest 25%
    (let ((to-remove (/ (length entries) 4)))
      (dotimes (_ to-remove)
        (when entries
          (remhash (caar entries) cache)
          (setq entries (cdr entries)))))))

(defun tramp-rpc--invalidate-cache-for-path (filename)
  "Invalidate cache entries for FILENAME."
  (cl-labels ((drop (candidate)
                (remhash candidate tramp-rpc--file-exists-cache)
                (remhash candidate tramp-rpc--file-truename-cache)
                (remhash (cons candidate nil) tramp-rpc--file-stat-cache)
                (remhash (cons candidate t) tramp-rpc--file-stat-cache))
              (flush-tramp-properties (candidate)
                (when (tramp-tramp-file-p candidate)
                  (ignore-errors
                    (with-parsed-tramp-file-name candidate nil
                      (tramp-flush-file-properties v localname)))))
              (flush-tramp-directory-properties (candidate)
                (when (tramp-tramp-file-p candidate)
                  (ignore-errors
                    (with-parsed-tramp-file-name candidate nil
                      (tramp-flush-directory-properties v localname)))))
              (spellings (path)
                (delete-dups
                 (list path
                       (directory-file-name path)
                       (file-name-as-directory
                        (directory-file-name path))))))
    (let ((expanded (expand-file-name filename)))
      (dolist (candidate (spellings expanded))
        (drop candidate)
        (flush-tramp-properties candidate)
        (flush-tramp-directory-properties candidate))
      ;; Also invalidate parent directory.
      (let ((dir (file-name-directory expanded)))
        (when dir
          (dolist (candidate (spellings dir))
            (drop candidate)
            (flush-tramp-properties candidate)
            (flush-tramp-directory-properties candidate)))))))

(defun tramp-rpc--invalidate-cache-for-subtree (directory)
  "Invalidate cache entries for DIRECTORY and all cached descendants."
  (let* ((expanded-dir (file-name-as-directory (expand-file-name directory)))
         (expanded-file (directory-file-name expanded-dir)))
    (tramp-rpc--invalidate-cache-for-path expanded-file)
    (cl-labels ((flush-tramp-properties (candidate)
                  (when (tramp-tramp-file-p candidate)
                    (ignore-errors
                      (with-parsed-tramp-file-name candidate nil
                        (tramp-flush-file-properties v localname)
                        (tramp-flush-directory-properties v localname)))))
                (drop-string-prefix (cache)
                  (let (keys)
                    (maphash (lambda (key _value)
                               (when (and (stringp key)
                                          (string-prefix-p expanded-dir key))
                                 (push key keys)))
                             cache)
                    (dolist (key keys)
                      (remhash key cache)
                      (flush-tramp-properties key))))
                (drop-stat-prefix ()
                  (let (keys)
                    (maphash (lambda (key _value)
                               (when (and (consp key)
                                          (stringp (car key))
                                          (string-prefix-p expanded-dir (car key)))
                                 (push key keys)))
                             tramp-rpc--file-stat-cache)
                    (dolist (key keys)
                      (remhash key tramp-rpc--file-stat-cache)
                      (flush-tramp-properties (car key))))))
      (drop-string-prefix tramp-rpc--file-exists-cache)
      (drop-string-prefix tramp-rpc--file-truename-cache)
      (drop-stat-prefix))))

(defun tramp-rpc-clear-file-exists-cache ()
  "Clear the file-exists-p cache."
  (interactive)
  (clrhash tramp-rpc--file-exists-cache))

(defun tramp-rpc-clear-file-truename-cache ()
  "Clear the file-truename cache."
  (interactive)
  (clrhash tramp-rpc--file-truename-cache))

(defun tramp-rpc-clear-file-stat-cache ()
  "Clear the file.stat cache."
  (interactive)
  (clrhash tramp-rpc--file-stat-cache))

(defun tramp-rpc--clear-file-metadata-caches ()
  "Clear cached file metadata."
  (clrhash tramp-rpc--file-exists-cache)
  (clrhash tramp-rpc--file-truename-cache)
  (clrhash tramp-rpc--file-stat-cache))

(defun tramp-rpc-clear-all-caches ()
  "Clear all tramp-rpc caches."
  (interactive)
  (tramp-rpc-magit--clear-cache)
  (tramp-rpc--clear-file-metadata-caches))

(defun tramp-rpc--clear-file-caches-for-connection (vec)
  "Clear file-exists and file-truename cache entries for connection VEC.
Entries are keyed by expanded TRAMP filenames; this removes those
matching the remote prefix of VEC."
  (let ((prefix (tramp-make-tramp-file-name vec "/")))
    ;; Match the prefix up to the colon-slash that starts the localname.
    ;; e.g. "/rpc:user@host:/" -- any key starting with this belongs to VEC.
    (dolist (cache (list tramp-rpc--file-exists-cache
                        tramp-rpc--file-truename-cache))
      (let ((keys-to-remove nil))
        (maphash (lambda (key _value)
                   (when (string-prefix-p prefix key)
                     (push key keys-to-remove)))
                 cache)
        (dolist (key keys-to-remove)
          (remhash key cache))))
    (let ((keys-to-remove nil))
      (maphash (lambda (key _value)
                 (when (and (consp key)
                            (string-prefix-p prefix (car key)))
                   (push key keys-to-remove)))
               tramp-rpc--file-stat-cache)
      (dolist (key keys-to-remove)
        (remhash key tramp-rpc--file-stat-cache)))))

;; ============================================================================
;; Filesystem watching
;; ============================================================================

(defvar tramp-rpc--watched-directories (make-hash-table :test 'equal)
  "Hash table of watched directories.
Keys are \"conn-key:path\" strings, values are plists with watch metadata.")

(defvar tramp-rpc--file-notify-watch-counts)

(defvar tramp-rpc--suppress-fs-notifications nil
  "When non-nil, suppress cache handling of fs.events notifications.
Used during operations that will invalidate caches themselves.")

(defun tramp-rpc--connection-key-string (vec)
  "Return a string key for connection VEC, suitable for hash table keys."
  (let ((key (tramp-rpc--connection-key vec)))
    (format "%S" key)))

(defun tramp-rpc--directory-watched-p (localname vec)
  "Return non-nil if LOCALNAME on VEC is being watched."
  (let ((conn-key (tramp-rpc--connection-key-string vec)))
    (gethash (format "%s:%s" conn-key localname)
             tramp-rpc--watched-directories)))

(defun tramp-rpc--watch-entry-recursive-p (entry)
  "Return non-nil if watched-directory ENTRY is recursive."
  (and (consp entry) (plist-get entry :recursive)))

(defun tramp-rpc--handle-notification (process method params)
  "Handle a server-initiated notification.
PROCESS is the connection, METHOD is the notification method,
PARAMS is the notification parameters."
  (cond
   ((string= method "fs.events")
    (tramp-rpc--handle-fs-events process params))
   (t
    (tramp-rpc--debug "Unknown notification: %s" method))))

(defun tramp-rpc--fs-event-path (vec event key)
  "Return EVENT's KEY path as a TRAMP file name on VEC, or nil."
  (when-let* ((path (tramp-rpc--decode-string (alist-get key event)))
              ((stringp path)))
    (if (tramp-tramp-file-p path)
        path
      (tramp-make-tramp-file-name vec path))))

(defun tramp-rpc--watch-canonical-directory (vec result)
  "Return canonical TRAMP directory from watch.add RESULT on VEC."
  (when-let* ((canonical-localname (and (listp result)
                                        (tramp-rpc--decode-string
                                         (alist-get 'path result))))
              ((stringp canonical-localname)))
    (if (tramp-tramp-file-p canonical-localname)
        canonical-localname
      (tramp-make-tramp-file-name vec canonical-localname))))

(defun tramp-rpc--path-under-directory-relative (directory file-name)
  "Return FILE-NAME relative to DIRECTORY, or nil.
DIRECTORY itself returns the empty string.  Descendants can contain slashes."
  (let* ((dir (file-name-as-directory (directory-file-name directory)))
         (file (directory-file-name file-name)))
    (cond
     ((string= (directory-file-name dir) file) "")
     ((string-prefix-p dir file)
      (substring file (length dir))))))

(defun tramp-rpc--watched-directory-alias-paths (path)
  "Return explicit watch spellings equivalent to canonical PATH."
  (let (aliases)
    (when (hash-table-p tramp-rpc--watched-directories)
      (maphash
       (lambda (_key entry)
         (let* ((canonical-directory (plist-get entry :canonical-directory))
                (directory (plist-get entry :directory))
                (relative (and canonical-directory directory
                               (tramp-rpc--path-under-directory-relative
                                canonical-directory path))))
           (when relative
             (let ((alias (if (string-empty-p relative)
                              (directory-file-name directory)
                            (expand-file-name relative directory))))
               (unless (string= alias path)
                 (cl-pushnew alias aliases :test #'string=))))))
       tramp-rpc--watched-directories))
    aliases))

(defun tramp-rpc--invalidate-event-path (path)
  "Invalidate caches for PATH and equivalent original watch spellings."
  (dolist (candidate (append (list path)
                             (tramp-rpc--watched-directory-alias-paths path)
                             (tramp-rpc--file-notify-alias-paths path)))
    (tramp-rpc--invalidate-cache-for-path candidate)))

(defun tramp-rpc--handle-fs-events (process params)
  "Handle an fs.events notification from PROCESS with PARAMS."
  (let ((events (alist-get 'events params)))
    (when events
      (tramp-rpc--debug "fs.events: %d events" (length events))
      (tramp-message process 6 "%s" events)
      (when-let* ((vec (process-get process :tramp-rpc-vec)))
        (unless tramp-rpc--suppress-fs-notifications
          ;; Also clear the magit process-file cache since git state may have changed.
          (tramp-rpc-magit--clear-status-cache))
        (let (renamed-pairs)
          ;; Linux/inotify can report the same rename as both a combined pair
          ;; and as cookie-tracked from/to events in one debounce batch.  Emacs'
          ;; filenotify tests expect one public `renamed' action, so suppress
          ;; the from/to half when an equivalent combined event is present.
          (dolist (event events)
            (let ((action (alist-get 'action event)))
              (when (string= action "renamed")
                (when-let* ((path (tramp-rpc--fs-event-path vec event 'path))
                            (path1 (tramp-rpc--fs-event-path vec event 'path1)))
                  (push (cons path path1) renamed-pairs)))))
          (dolist (event events)
            (let* ((action (alist-get 'action event))
                   (path (tramp-rpc--fs-event-path vec event 'path))
                   (path1 (tramp-rpc--fs-event-path vec event 'path1))
                   (cookie (alist-get 'cookie event))
                   (duplicate-tracked-rename
                    (and (member action '("renamed-from" "renamed-to"))
                         path
                         (cl-some
                          (lambda (pair)
                            (string= path (if (string= action "renamed-from")
                                              (car pair)
                                            (cdr pair))))
                          renamed-pairs))))
              (when (and (stringp action) (not duplicate-tracked-rename))
                (if (string= action "rescan")
                    (unless tramp-rpc--suppress-fs-notifications
                      (tramp-rpc--clear-file-caches-for-connection vec))
                  (when path
                    ;; File notifications are deliberately not suppressed by
                    ;; `tramp-rpc--suppress-fs-notifications': that variable only
                    ;; suppresses cache/status work during operations that
                    ;; invalidate caches themselves.
                    (unless tramp-rpc--suppress-fs-notifications
                      (tramp-rpc--invalidate-event-path path)
                      (when path1
                        (tramp-rpc--invalidate-event-path path1)))
                    (tramp-rpc--file-notify-dispatch action path path1 cookie)))))))))))

(defun tramp-rpc-watch-directory (directory &optional recursive)
  "Start watching DIRECTORY for filesystem changes.
When RECURSIVE is non-nil, watch subdirectories too."
  (interactive "DDirectory to watch: ")
  (with-parsed-tramp-file-name directory nil
    (let* ((watch-key (format "%s:%s" (tramp-rpc--connection-key-string v)
                              localname))
           (entry (gethash watch-key tramp-rpc--watched-directories))
           (file-notify-entry (and (boundp 'tramp-rpc--file-notify-watch-counts)
                                   (gethash watch-key
                                            tramp-rpc--file-notify-watch-counts))))
      (if (and recursive
               (not (tramp-rpc--watch-entry-recursive-p entry))
               file-notify-entry
               (plist-get file-notify-entry :owned))
          ;; Upgrade a file-notify-owned direct watch by relying on the server's
          ;; atomic non-recursive-to-recursive upgrade path.  Do not remove the
          ;; existing watch first; if the recursive add fails, the server rolls
          ;; back and our file-notify ownership state remains unchanged.
          (let* ((result (tramp-rpc--call
                          v "watch.add"
                          `((path . ,localname) (recursive . t))))
                 (canonical-directory
                  (tramp-rpc--watch-canonical-directory v result)))
            (plist-put file-notify-entry :owned nil)
            (puthash watch-key
                     (list :recursive t
                           :directory directory
                           :canonical-directory canonical-directory)
                     tramp-rpc--watched-directories))
        (let* ((result (tramp-rpc--call
                        v "watch.add"
                        `((path . ,localname)
                          (recursive . ,(if recursive t :msgpack-false)))))
               (canonical-directory
                (tramp-rpc--watch-canonical-directory v result)))
          (puthash watch-key
                   (list :recursive (or recursive
                                        (tramp-rpc--watch-entry-recursive-p entry))
                         :directory directory
                         :canonical-directory canonical-directory)
                   tramp-rpc--watched-directories))))
    (tramp-rpc--debug "Watching: %s (recursive=%s)" localname recursive)))

(defun tramp-rpc-unwatch-directory (directory)
  "Stop watching DIRECTORY for filesystem changes."
  (interactive "DDirectory to unwatch: ")
  (with-parsed-tramp-file-name directory nil
    (let* ((watch-key (format "%s:%s" (tramp-rpc--connection-key-string v)
                              localname))
           (entry (gethash watch-key tramp-rpc--watched-directories))
           (canonical-directory
            (tramp-rpc--watch-entry-canonical-directory entry))
           (file-notify-entry (and (boundp 'tramp-rpc--file-notify-watch-counts)
                                   (gethash watch-key
                                            tramp-rpc--file-notify-watch-counts))))
      (remhash watch-key tramp-rpc--watched-directories)
      ;; If file-notify was relying on the watch we just removed, restore its
      ;; direct non-recursive watch.  This applies to both recursive and
      ;; non-recursive explicit watches; otherwise a still-valid file-notify
      ;; descriptor could be left without any server watch underneath it.
      (when (and file-notify-entry
                 (not (plist-get file-notify-entry :owned)))
        (let ((result (tramp-rpc--call v "watch.add"
                                       `((path . ,localname)
                                         (recursive . :msgpack-false)))))
          (when-let* ((restored-canonical-directory
                       (tramp-rpc--watch-canonical-directory v result)))
            (setq canonical-directory restored-canonical-directory)
            (plist-put file-notify-entry :canonical-directory canonical-directory)))
        (plist-put file-notify-entry :owned t))
      (unless (tramp-rpc--canonical-watch-active-p canonical-directory)
        (tramp-rpc--call
         v "watch.remove"
         `((path . ,(if (stringp canonical-directory)
                        (tramp-file-local-name canonical-directory)
                      localname))))))
    (tramp-rpc--debug "Unwatched: %s" localname)))

(defun tramp-rpc--cleanup-watches-for-connection (vec)
  "Remove all watched directory entries for connection VEC."
  (let ((conn-key (tramp-rpc--connection-key-string vec))
        (keys-to-remove nil))
    (maphash (lambda (key _value)
               (when (string-prefix-p (concat conn-key ":") key)
                 (push key keys-to-remove)))
             tramp-rpc--watched-directories)
    (dolist (key keys-to-remove)
      (remhash key tramp-rpc--watched-directories))
    (when keys-to-remove
      (tramp-rpc--debug "Cleaned up %d watches for %s"
                        (length keys-to-remove) conn-key))))

(defun tramp-rpc-list-watches ()
  "List currently watched directories."
  (interactive)
  (let ((watches nil))
    (maphash (lambda (key _value)
               (push key watches))
             tramp-rpc--watched-directories)
    (if watches
        (message "Watched directories:\n%s"
                 (mapconcat #'identity watches "\n"))
      (message "No directories being watched."))))

;; ============================================================================
;; Magit integration - client-side parallel prefetch
;; ============================================================================

;; The prefetch sends all git commands magit will need via a single
;; commands.run_parallel RPC call.  The server runs them in parallel
;; using OS threads and returns {key: {exit_code, stdout, stderr}}.
;; The results are stored directly as the process-file cache --
;; no reconstruction or key normalization needed.

(defcustom tramp-rpc-magit-optimize t
  "Whether to enable magit prefetch optimizations.
When non-nil, tramp-rpc will automatically install handlers on
`magit-status-setup-buffer' and `magit-status-refresh-buffer' to
prefetch git commands in parallel, dramatically speeding up
magit-status on remote repositories."
  :type 'boolean
  :group 'tramp-rpc)

(defvar tramp-rpc-magit--process-caches (make-hash-table :test 'equal)
  "Hash table mapping (conn-key . directory) to process-file cache entries.
Each value stores a timestamp and a hash table mapping git arg keys to
(exit-code . stdout-string).  Keyed per connection and per directory to
support multiple remotes and repos.")

(defcustom tramp-rpc-magit-process-cache-ttl 120
  "Seconds to keep prefetched Magit git command output.

Magit creates some expensive status sections lazily.  For example,
pressing TAB on the Unstaged changes or Untracked files section
can run git after the initial status refresh has completed.  Keeping the
prefetched command output briefly lets those lazy expansions reuse the
same batched status snapshot instead of making another remote round-trip.

Filesystem watch notifications and the next status refresh still clear or
replace this cache; this TTL is only a backstop for unwatched changes."
  :type 'number
  :group 'tramp-rpc)

(defcustom tramp-rpc-magit-disable-remote-diff-tab-width-detection t
  "Whether remote Magit status expansion should skip per-file tab-width probing.

Magit can inspect each changed file while washing diffs to derive its
buffer-local `tab-width'.  On TRAMP-RPC remotes that may open many files
and trigger many serial `file.stat', `file.truename', and process RPCs
when pressing TAB in `magit-status'.  When this option is non-nil,
TRAMP-RPC binds `magit-diff-adjust-tab-width' to nil while refreshing or
expanding remote Magit status sections, avoiding those round-trips and
using the current `tab-width' instead."
  :type 'boolean
  :group 'tramp-rpc)

(defvar tramp-rpc-magit--ancestors-cache nil
  "Cached ancestor scan data from server-side RPC.
This is populated by `tramp-rpc-magit--prefetch' for file existence checks.")

(defconst tramp-rpc-magit--ancestor-marker-names
  '(".git" ".svn" ".hg" ".bzr" "_darcs"
    ".fslckout" "_FOSSIL_" ".pijul" ".sl" ".jj"
    ".projectile" ".project" ".dir-locals.el" ".editorconfig")
  "Marker names checked by project, VC, Projectile, and editorconfig code.")

(defvar tramp-rpc-magit--ancestor-scan-caches (make-hash-table :test 'equal)
  "Cached ancestor scans keyed by remote search directory.")

(defvar tramp-rpc-magit--prefetch-directory nil
  "The directory that was prefetched.
Used to answer file-exists-p queries for the directory itself.")

(defvar tramp-rpc-magit--debug nil
  "When non-nil, log cache hits/misses for debugging.")

(defvar tramp-rpc-magit--allow-process-cache nil
  "Non-nil when prefetched git output may satisfy `process-file'.

This is dynamically bound while Magit is refreshing or lazily expanding a
status section.  Outside those windows, exact git command matches should
run normally instead of using a possibly stale status snapshot.")

(defvar tramp-rpc-magit--status-setup-prefetch-active nil
  "Non-nil while a status setup prefetch covers nested refresh calls.")


(defun tramp-rpc-magit--get-cache-key (vec directory)
  "Build a cache key for VEC and DIRECTORY.
Returns a cons cell (connection-key . directory) for hash table lookups."
  (cons (tramp-rpc--connection-key-string vec)
        (expand-file-name directory)))

(defun tramp-rpc-magit--valid-process-cache (key)
  "Return the non-expired process cache for KEY, or nil."
  (let* ((entry (gethash key tramp-rpc-magit--process-caches))
         ;; Older sessions may still contain the pre-TTL representation.
         (cache (if (hash-table-p entry)
                    entry
                  (plist-get entry :cache)))
         (time (and (not (hash-table-p entry))
                    (plist-get entry :time))))
    (cond
     ((not cache) nil)
     ((and time
           tramp-rpc-magit-process-cache-ttl
           (> (- (float-time) time)
              tramp-rpc-magit-process-cache-ttl))
      (remhash key tramp-rpc-magit--process-caches)
      nil)
     (t cache))))

(defun tramp-rpc-magit--get-process-cache ()
  "Get the process-file cache for the current `default-directory'.
Returns the cache hash table, or nil if none."
  (when (file-remote-p default-directory)
    (with-parsed-tramp-file-name default-directory nil
      (or (tramp-rpc-magit--valid-process-cache
           (tramp-rpc-magit--get-cache-key v default-directory))
          ;; Magit sometimes temporarily sets `default-directory' to a file's
          ;; subdirectory while washing a status diff.  Reuse the repository
          ;; root prefetch cache for those subdirectories; otherwise exact
          ;; prefetched git commands miss solely because the cwd changed.
          (when (and tramp-rpc-magit--prefetch-directory
                     (equal (file-remote-p default-directory)
                            (file-remote-p tramp-rpc-magit--prefetch-directory))
                     (string-prefix-p
                      (file-name-as-directory
                       (tramp-file-local-name tramp-rpc-magit--prefetch-directory))
                      (tramp-file-local-name (expand-file-name default-directory))))
            (tramp-rpc-magit--valid-process-cache
             (tramp-rpc-magit--get-cache-key
              v tramp-rpc-magit--prefetch-directory)))))))

(defun tramp-rpc-magit--set-process-cache (vec directory cache)
  "Set the process-file CACHE for VEC and DIRECTORY."
  (let ((key (tramp-rpc-magit--get-cache-key vec directory)))
    (puthash key (list :time (float-time) :cache cache)
             tramp-rpc-magit--process-caches)))


(defun tramp-rpc-magit--process-cache-key (&rest args)
  "Build a cache key from git ARGS.
Use a printed argv list rather than joining with a separator, so pathspecs
containing characters such as `|' cannot collide with a different argv vector."
  (prin1-to-string (mapcar (lambda (arg)
                             (if (stringp arg)
                                 (substring-no-properties arg)
                               arg))
                           args)))

(defconst tramp-rpc-magit--ignorable-git-global-config
  (append '("core.preloadIndex=true"
            "log.showSignature=false"
            "color.ui=false"
            "color.diff=false"
            "diff.noPrefix=false")
          (when (eq system-type 'windows-nt)
            '("i18n.logOutputEncoding=UTF-8")))
  "Git `-c' assignments that Magit adds and prefetch reproduces.")

(defconst tramp-rpc-magit--git-prefetch-prefix-args
  (append '("--no-pager" "--literal-pathspecs")
          (apply #'append
                 (mapcar (lambda (assignment) (list "-c" assignment))
                         tramp-rpc-magit--ignorable-git-global-config)))
  "Global git arguments used for prefetched git commands.")

(defconst tramp-rpc-magit--uncacheable-git-subcommands
  '("update-index")
  "Git subcommands that must never be served from the Magit process cache.")

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

(defun tramp-rpc-magit--state-file-entry (gitdir relative-path)
  "Return a commands.run_parallel entry testing RELATIVE-PATH under GITDIR."
  (let ((full-path (concat (file-name-as-directory gitdir) relative-path)))
    `((key . ,(concat "state_file:" full-path))
      (cmd . "test")
      (args . ["-e" ,full-path]))))

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
                        (args . ,(vconcat (append tramp-rpc-magit--git-prefetch-prefix-args
                                                   args)))
                        (cwd . ,directory))
                      cmds))
              (add-state-file (relative-path)
                (push (tramp-rpc-magit--state-file-entry gitdir relative-path)
                      cmds)))
      ;; State file existence checks (speculative, assuming .git gitdir)
      (dolist (sf tramp-rpc-magit--state-files)
        (add-state-file sf))

      ;; Basic repo info
      (add-git "rev-parse" "--show-toplevel")
      (add-git "rev-parse" "--git-dir")

      ;; HEAD info
      (add-git "rev-parse" "HEAD")
      (add-git "rev-parse" "--short" "HEAD")
      (add-git "rev-parse" "--short=9" "HEAD")
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
      (add-git "status" "-z" "--porcelain" "--untracked-files=all" "--")

      ;; File-list sections are inserted lazily when expanding collapsed
      ;; Magit status sections.  Prefetch their common commands so TAB can be
      ;; served from the same batched snapshot.
      (add-git "ls-files" "-z" "--full-name")
      (add-git "ls-files" "-z" "--full-name" "--cached")
      (add-git "ls-files" "-z" "--full-name" "--others" "--ignored" "--exclude-standard")
      (add-git "ls-files" "-z" "--full-name" "-t")
      (add-git "ls-files" "-z" "--full-name" "-v")

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
      (add-git "config" "-z" "--get-all" "--include" "core.abbrev")
      (add-git "config" "-z" "--get-all" "--include" "forge.remote")
      (add-git "config" "--get" "remote.upstream.url")
      (add-git "config" "--get" "remote.origin.url")

      ;; Revision/name formatting used while washing expanded file sections.
      (add-git "for-each-ref" "--format=%(symref)\f%(refname)" "refs/")
      (add-git "for-each-ref" "--format=%(symref)\f%(refname:short)" "refs/")
      (add-git "symbolic-ref" "refs/remotes/origin/HEAD")

      ;; Porcelain status
      (add-git "status" "-z" "--porcelain" "--untracked-files=normal" "--")
      (add-git "status" "--porcelain" "--branch")

      ;; Bare repo check
      (add-git "rev-parse" "--is-bare-repository")

      ;; Stash
      (add-git "rev-parse" "--verify" "refs/stash")
      (add-git "reflog" "--format=%gd%x00%aN%x00%at%x00%gs" "refs/stash")

      ;; Parent commits
      (add-git "rev-parse" "--short" "HEAD~")
      (add-git "rev-parse" "--short=9" "HEAD~")
      (add-git "rev-parse" "--verify" "HEAD~10")

      ;; Recent log with decorations
      (add-git "log" "--format=%h%x0c%D%x0c%x0c%aN%x0c%at%x0c%x0c%s"
               "--decorate=full" "-n10" "--use-mailmap" "--no-prefix" "--")

      ;; Log for header line
      (add-git "log" "--no-walk" "--format=%h %s" "HEAD^{commit}" "--"))

    (vconcat (nreverse cmds))))

(defun tramp-rpc-magit--git-command-entry (directory args)
  "Return a commands.run_parallel entry for git ARGS in DIRECTORY."
  `((key . ,(apply #'tramp-rpc-magit--process-cache-key args))
    (cmd . "git")
    (args . ,(vconcat (append tramp-rpc-magit--git-prefetch-prefix-args args)))
    (cwd . ,directory)))

(defun tramp-rpc-magit--store-command-results (vec directory results &optional replace)
  "Merge commands.run_parallel RESULTS into DIRECTORY's process cache.
When REPLACE is non-nil, build a fresh cache instead of merging into an
existing one."
  (when results
    (let* ((key (tramp-rpc-magit--get-cache-key vec directory))
           (cache (if replace
                      (make-hash-table :test 'equal)
                    (or (tramp-rpc-magit--valid-process-cache key)
                        (make-hash-table :test 'equal))))
           (remote-prefix (file-remote-p directory)))
      (dolist (entry results)
        (let* ((cmd-key (if (symbolp (car entry))
                            (symbol-name (car entry))
                          (car entry)))
               (data (cdr entry))
               (exit-code (alist-get 'exit_code data)))
          (if (string-prefix-p "state_file:" cmd-key)
              (let* ((remote-path (substring cmd-key (length "state_file:")))
                     (tramp-path (concat remote-prefix remote-path)))
                (tramp-rpc--cache-put tramp-rpc--file-exists-cache
                                      tramp-path
                                      (= exit-code 0)))
            (let* ((stdout-raw (alist-get 'stdout data))
                   (stdout (tramp-rpc--decode-output stdout-raw nil)))
              (puthash cmd-key (cons exit-code stdout) cache)))))
      (tramp-rpc-magit--set-process-cache vec directory cache)
      cache)))

(defconst tramp-rpc-magit--run-parallel-command-limit 200
  "Maximum commands per Magit `commands.run_parallel' RPC.
The server currently rejects batches above 256; keep headroom so dynamic
prefetch growth does not trip that hard limit.")

(defun tramp-rpc-magit--run-command-entries (vec directory commands)
  "Run COMMANDS in chunks and merge their results into DIRECTORY's cache."
  (let ((remaining (append commands nil))
        cache)
    (while remaining
      (let ((chunk nil)
            (count 0))
        (while (and remaining
                    (< count tramp-rpc-magit--run-parallel-command-limit))
          (push (pop remaining) chunk)
          (setq count (1+ count)))
        (setq chunk (nreverse chunk))
        (setq cache
              (tramp-rpc-magit--store-command-results
               vec directory
               (tramp-rpc--call vec "commands.run_parallel"
                                `((commands . ,(vconcat chunk))))))))
    cache))

(defun tramp-rpc-magit--cached-git-stdout (cache &rest args)
  "Return cached stdout for git ARGS in CACHE, or nil on miss/error.
If ARGS ends with `:raw', preserve stdout exactly instead of trimming
whitespace."
  (let ((raw (eq (car (last args)) :raw)))
    (when raw
      (setq args (butlast args)))
    (when-let* ((entry (gethash (apply #'tramp-rpc-magit--process-cache-key args)
                                cache)))
      (when (= 0 (car entry))
        (if raw
            (cdr entry)
          (string-trim (cdr entry)))))))

(defun tramp-rpc-magit--status-files-from-porcelain (porcelain)
  "Extract touched files from NUL-delimited git PORCELAIN status output."
  (let ((records (split-string (or porcelain "") "\0" t))
        files)
    (while records
      (let ((record (pop records)))
        (when (>= (length record) 4)
          (let ((x (aref record 0))
                (y (aref record 1))
                (path (substring record 3)))
            (cond
             ;; Rename/copy porcelain v1 -z uses the next NUL record as the
             ;; original path.  The first path is the one Magit displays in
             ;; status and later expands.
             ((or (memq x '(?R ?C)) (memq y '(?R ?C)))
              (push path files)
              (when records (pop records)))
             ((not (string-empty-p path))
              (push path files)))))))
    (delete-dups (nreverse files))))

(defun tramp-rpc-magit--remote-path (vec localname)
  "Return the TRAMP filename for LOCALNAME on VEC."
  (tramp-make-tramp-file-name vec localname))

(defun tramp-rpc-magit--cache-file-stat (vec localname stat)
  "Populate TRAMP/file-exists caches for LOCALNAME from STAT."
  (let ((localnames (list localname)))
    ;; TRAMP distinguishes directory spellings with and without trailing slash
    ;; in its property keys.  Magit tends to ask for the slash spelling later,
    ;; while our metadata prefetch naturally de-duplicates to the canonical
    ;; no-slash spelling, so populate both.
    (when (and stat (equal (alist-get 'type stat) "directory"))
      (push (file-name-as-directory (directory-file-name localname))
            localnames))
    (dolist (ln (delete-dups localnames))
      (let ((filename (tramp-rpc-magit--remote-path vec ln))
            (symlink-p (and stat (equal (alist-get 'type stat) "symlink"))))
        ;; This metadata batch uses lstat.  A symlink lstat does not tell us
        ;; whether following the symlink succeeds, so don't populate
        ;; `file-exists-p' or follow-stat from it.
        (unless symlink-p
          (tramp-rpc--cache-put tramp-rpc--file-exists-cache filename (if stat t nil)))
        (tramp-rpc--cache-file-stat-result vec ln stat t)
        (when stat
          (tramp-set-file-property
           vec ln "file-attributes-nil"
           (tramp-rpc--convert-file-attributes stat nil))
          (tramp-set-file-property
           vec ln "file-attributes-integer"
           (tramp-rpc--convert-file-attributes stat 'integer))
          (pcase (alist-get 'type stat)
            ("directory" (tramp-set-file-property vec ln "file-directory-p" t))
            ((or "file" (pred null))
             (tramp-set-file-property vec ln "file-directory-p" nil))))))))

(defun tramp-rpc-magit--ref-short-names (cache)
  "Return short ref names from cached `for-each-ref' output in CACHE."
  (when-let* ((entry (gethash (tramp-rpc-magit--process-cache-key
                               "for-each-ref"
                               "--format=%(symref)\f%(refname:short)" "refs/")
                              cache))
              ((= 0 (car entry))))
    (let ((sep (string ?\f)))
      (delq nil
            (mapcar (lambda (line)
                      (let ((parts (split-string line (regexp-quote sep))))
                        (cadr parts)))
                    (split-string (cdr entry) "\n" t))))))

(defun tramp-rpc-magit--remote-branch-candidates (cache branch)
  "Return remote branch names in CACHE likely related to BRANCH."
  (when (and branch (not (string-empty-p branch)))
    (let* ((remotes (when-let* ((remote-output
                                 (tramp-rpc-magit--cached-git-stdout
                                  cache "remote")))
                      (split-string remote-output "\n" t)))
           (from-remotes (mapcar (lambda (remote)
                                   (concat remote "/" branch))
                                 remotes))
           (suffix (concat "/" branch))
           (from-refs
            (cl-remove-if-not
             (lambda (name)
               (and (string-match-p "/" name)
                    (string-suffix-p suffix name)))
             (tramp-rpc-magit--ref-short-names cache))))
      (delete-dups (append from-remotes from-refs)))))

(defun tramp-rpc-magit--cache-file-truename (vec localname result)
  "Populate `file-truename' cache for LOCALNAME from RESULT."
  (when result
    (let* ((truename-local (tramp-rpc--decode-string
                            (if (stringp result)
                                result
                              (alist-get 'path result))))
           (filename (tramp-rpc-magit--remote-path vec localname))
           (truename (tramp-rpc-magit--remote-path vec truename-local)))
      (tramp-rpc--cache-put tramp-rpc--file-truename-cache filename truename))))

(defun tramp-rpc-magit--prefetch-file-metadata (vec files)
  "Batch file.stat/file.truename for local FILES on VEC and cache them."
  (let (items requests)
    (dolist (file files)
      (let ((local-file (directory-file-name file))
            (local-dir (directory-file-name (file-name-directory file))))
        (dolist (path (list local-file local-dir))
          (when (and path (not (member (list 'stat path) items)))
            (push (list 'stat path) items)
            (push (cons "file.stat"
                        (append (tramp-rpc--encode-path path) '((lstat . t))))
                  requests)))
        (unless (member (list 'truename local-file) items)
          (push (list 'truename local-file) items)
          (push (cons "file.truename" (tramp-rpc--encode-path local-file))
                requests))))
    (when requests
      (let ((results (tramp-rpc--call-batch vec (nreverse requests))))
        (cl-mapc
         (lambda (item result)
           (unless (and (consp result) (plist-get result :error))
             (pcase (car item)
               ('stat (tramp-rpc-magit--cache-file-stat vec (cadr item) result))
               ('truename (tramp-rpc-magit--cache-file-truename
                           vec (cadr item) result)))))
         (nreverse items)
         results)))))

(defun tramp-rpc-magit--prefetch-dynamic-status (vec directory root-local)
  "Prefetch status data that depends on initial git output.
This second-stage batch covers worktree-specific gitdirs, current branch and
upstream names, and commands used to wash already-expanded file sections."
  (when-let* ((cache (tramp-rpc-magit--get-process-cache)))
    (let ((commands nil)
          (expanded-files (list (directory-file-name root-local))))
      (cl-labels
          ((cached (&rest args)
             (apply #'tramp-rpc-magit--cached-git-stdout cache args))
           (add-git (&rest args)
             (let ((key (apply #'tramp-rpc-magit--process-cache-key args)))
               (unless (gethash key cache)
                 (push (tramp-rpc-magit--git-command-entry root-local args)
                       commands))))
           (add-state-dir (gitdir)
             (dolist (sf tramp-rpc-magit--state-files)
               (push (tramp-rpc-magit--state-file-entry gitdir sf) commands)))
           (add-log-range (range)
             (add-git "log" "--format=%h%x0c%D%x0c%x0c%aN%x0c%at%x0c%x0c%s"
                      "--decorate=full" "-n256" "--use-mailmap"
                      "--no-prefix" range "--"))
           (add-ref-name (name)
             (when (and name (not (string-empty-p name)))
               (add-git "rev-parse" "--verify" name)
               (add-git "rev-parse" "--verify" "--abbrev-ref" name)
               (add-git "rev-parse" "--verify" (concat "refs/tags/" name)))))
        ;; Magit checks state files in the real gitdir.  In linked worktrees,
        ;; that is not WORKTREE/.git, so use the prefetched rev-parse result.
        (when-let* ((gitdir (cached "rev-parse" "--git-dir")))
          (add-state-dir (if (file-name-absolute-p gitdir)
                             gitdir
                           (expand-file-name gitdir root-local))))

        ;; Current branch/upstream/ref-name probes.
        (let* ((branch (cached "symbolic-ref" "--short" "HEAD"))
               (upstream (cached "rev-parse" "--abbrev-ref" "@{upstream}"))
               (origin-head-ref (cached "symbolic-ref" "refs/remotes/origin/HEAD"))
               (origin-head-short (and origin-head-ref
                                       (string-remove-prefix
                                        "refs/remotes/" origin-head-ref)))
               (origin-head-branch (and origin-head-short
                                        (file-name-nondirectory origin-head-short)))
               (names (delete-dups
                       (delq nil (append
                                  (list branch
                                        (and branch (concat branch "@{upstream}"))
                                        upstream
                                        "origin/HEAD"
                                        origin-head-short
                                        origin-head-branch)
                                  (tramp-rpc-magit--remote-branch-candidates
                                   cache branch))))))
          (dolist (name names)
            (add-ref-name name)
            (when (and name (not (string-suffix-p "@{upstream}" name)))
              (add-log-range (concat name ".."))
              (add-log-range (concat ".." name)))))

        ;; File-section wash commands for files already expanded in status.
        (let* ((status (or (cached "status" "-z" "--porcelain"
                                   "--untracked-files=normal" "--" :raw)
                           (cached "status" "-z" "--porcelain"
                                   "--untracked-files=all" "--" :raw)))
               (files (tramp-rpc-magit--status-files-from-porcelain status))
               (head (or (cached "rev-parse" "--short=9" "HEAD")
                         (cached "rev-parse" "HEAD"))))
          (when head
            (add-git "rev-parse" "--short=9" head)
            (add-git "cat-file" "-t" head)
            (add-git "rev-parse" "--verify" (concat head "^{commit}")))
          (dolist (file files)
            (let ((abs-file (expand-file-name file root-local)))
              (push abs-file expanded-files)
              (add-git "diff" "--quiet" "--cached" "--submodule=short"
                       "--" file)
              (add-git "ls-files" "-c" "-z" "--" file)
              (when head
                (add-git "ls-tree" "--full-tree" head "--" abs-file)
                (add-git "cat-file" "-p" (format "%s:%s" head file))))))

        (when commands
          (tramp-rpc-magit--run-command-entries
           vec directory (nreverse commands)))
        (when expanded-files
          (tramp-rpc-magit--prefetch-file-metadata
           vec (delete-dups (nreverse expanded-files))))))))

(defun tramp-rpc-magit--prefetch-file-section (section)
  "Prefetch the git commands needed to expand Magit file SECTION.
This is intentionally much smaller than the full status prefetch and is used
when the status cache has expired but TAB is expanding a single file section."
  (when-let* ((file (tramp-rpc-magit--section-slot section 'value))
              ((stringp file))
              (directory (or tramp-rpc-magit--prefetch-directory
                             default-directory))
              ((file-remote-p directory))
              ((tramp-rpc-file-name-p directory)))
    (with-parsed-tramp-file-name directory nil
      (let* ((root-local (file-name-as-directory localname))
             (rel-file (if (file-name-absolute-p file)
                           (file-relative-name file root-local)
                         file))
             (abs-file (expand-file-name rel-file root-local))
             (cache (tramp-rpc-magit--get-process-cache))
             (diff-key (tramp-rpc-magit--process-cache-key
                        "diff" "--quiet" "--cached" "--submodule=short"
                        "--" rel-file)))
        (unless (and cache (gethash diff-key cache))
          ;; First ensure we know the full HEAD object name; several Magit
          ;; helpers subsequently ask about that exact object, not the symbol
          ;; HEAD, so the cache key must contain the resolved object name.
          (unless (and cache (gethash (tramp-rpc-magit--process-cache-key
                                       "rev-parse" "HEAD")
                                      cache))
            (tramp-rpc-magit--store-command-results
             v directory
             (tramp-rpc--call
              v "commands.run_parallel"
              `((commands . ,(vector
                              (tramp-rpc-magit--git-command-entry
                               root-local '("rev-parse" "HEAD"))))))))
          (setq cache (tramp-rpc-magit--get-process-cache))
          (let* ((head-entry (and cache
                                  (gethash (tramp-rpc-magit--process-cache-key
                                            "rev-parse" "HEAD")
                                           cache)))
                 (head (and head-entry (= 0 (car head-entry))
                            (string-trim (cdr head-entry))))
                 (commands nil))
            (cl-labels ((add (&rest args)
                          (push (tramp-rpc-magit--git-command-entry
                                 root-local args)
                                commands)))
              (add "diff" "--quiet" "--cached" "--submodule=short"
                   "--" rel-file)
              (add "ls-files" "-c" "-z" "--" rel-file)
              (add "config" "-z" "--get-all" "--include" "core.abbrev")
              (add "for-each-ref" "--format=%(symref)\f%(refname)" "refs/")
              (add "for-each-ref" "--format=%(symref)\f%(refname:short)" "refs/")
              (when head
                (add "rev-parse" "--short=9" head)
                (add "cat-file" "-t" head)
                (add "rev-parse" "--verify" (concat head "^{commit}"))
                (add "ls-tree" "--full-tree" head "--" abs-file)
                (add "cat-file" "-p" (format "%s:%s" head rel-file))))
            (tramp-rpc-magit--store-command-results
             v directory
             (tramp-rpc--call v "commands.run_parallel"
                              `((commands . ,(vconcat (nreverse commands))))))))))))

(defun tramp-rpc-magit--strip-git-prefix-args (args)
  "Strip cache-neutral Magit git prefix flags from ARGS.
Return nil if ARGS contain semantic global flags that are not represented by
our prefetch command/key."
  (let ((rest (mapcar (lambda (arg)
                        (if (stringp arg) (substring-no-properties arg) arg))
                      (append args nil)))
        (safe t))
    (while (and safe rest
                (let ((arg (car rest)))
                  (cond
                   ;; `--literal-pathspecs' is cache-neutral for the prefetched
                   ;; commands we serve: commands with real pathspecs are
                   ;; prefetched in the literal form Magit uses, and commands
                   ;; without pathspecs are unaffected by the flag.
                   ((member arg '("--no-pager" "--literal-pathspecs")) t)
                   ((string= "-c" arg)
                    (let ((assignment (cadr rest)))
                      (if (member assignment
                                  tramp-rpc-magit--ignorable-git-global-config)
                          (progn (setq rest (cdr rest)) t)
                        (setq safe nil)
                        nil)))
                   ;; These global arguments change pathspec/repository
                   ;; semantics and are not modeled by the cache key.
                   ((member arg '("--glob-pathspecs" "--noglob-pathspecs" "-C"))
                    (setq safe nil)
                    nil)
                   (t nil))))
      (setq rest (cdr rest)))
    (and safe rest)))

(defun tramp-rpc-magit--git-cacheable-args-p (args)
  "Return non-nil if normalized git ARGS may be cached."
  (let ((subcommand (car args)))
    (and subcommand
         (not (string-prefix-p "-" subcommand))
         (not (member subcommand tramp-rpc-magit--uncacheable-git-subcommands)))))

(defun tramp-rpc-magit--git-cache-safe-environment-p ()
  "Return non-nil if the dynamic environment is safe for git cache reuse."
  (let ((baseline (default-toplevel-value 'process-environment))
        (safe t))
    (dolist (entry process-environment safe)
      (when (and safe
                 (stringp entry)
                 (not (member entry baseline))
                 (string-match-p "\\`GIT_[^=]*=" entry))
        (setq safe nil)))))

(defun tramp-rpc-magit--process-cache-lookup (program args)
  "Look up PROGRAM ARGS in the process-file cache.
Returns (exit-code . stdout) if found, nil otherwise."
  (when-let* (((bound-and-true-p tramp-rpc-magit--allow-process-cache))
              ((tramp-rpc-magit--git-cache-safe-environment-p))
              (cache (tramp-rpc-magit--get-process-cache)))
    (when (or (string-suffix-p "/git" program)
              (string= "git" program))
      (let* ((core-args (tramp-rpc-magit--strip-git-prefix-args args))
             (key (and (tramp-rpc-magit--git-cacheable-args-p core-args)
                       (apply #'tramp-rpc-magit--process-cache-key core-args)))
             (result (and key (gethash key cache))))
        (when tramp-rpc-magit--debug
          (if result
              (tramp-rpc--debug "process-file HIT (prefetch): git %s -> exit %d"
                                key (car result))
            (tramp-rpc--debug "process-file MISS (prefetch): git %s" key)))
        result))))

(defun tramp-rpc-magit--process-cache-store (program args exit-code stdout)
  "Store a just-run git PROGRAM ARGS result in the active Magit cache.
EXIT-CODE and STDOUT are the values returned by `process-file'."
  (when-let* (((bound-and-true-p tramp-rpc-magit--allow-process-cache))
              ((tramp-rpc-magit--git-cache-safe-environment-p))
              (cache (tramp-rpc-magit--get-process-cache)))
    (when (or (string-suffix-p "/git" program)
              (string= "git" program))
      (let* ((core-args (tramp-rpc-magit--strip-git-prefix-args args))
             (key (and (tramp-rpc-magit--git-cacheable-args-p core-args)
                       (apply #'tramp-rpc-magit--process-cache-key core-args))))
        (when key
          (puthash key (cons exit-code stdout) cache))))))

(defun tramp-rpc-magit--prefetch (directory)
  "Prefetch magit status and ancestor data for DIRECTORY.
Sends all git commands magit will need via a single
commands.run_parallel RPC call, then stores the results directly
as the process-file cache.  Also fetches ancestor markers."
  (when (and (file-remote-p directory)
             (tramp-rpc-file-name-p directory))
    ;; Suppress fs.events cache handling during prefetch.  The git commands
    ;; we run on the server touch .git/index etc., triggering inotify events
    ;; that would clear the cache we're building.
    (let ((tramp-rpc--suppress-fs-notifications t))
      ;; Remember the directory we prefetched for
      (setq tramp-rpc-magit--prefetch-directory (expand-file-name directory))
      (with-parsed-tramp-file-name directory nil
        ;; Build command list and run in parallel on server.  `update-index
        ;; --refresh' is intentionally not part of this prefetch; it is run by
        ;; Magit in the real refresh sequence, and `tramp-rpc-handle-process-file'
        ;; triggers this prefetch immediately after that command completes.
        (let* ((commands (tramp-rpc-magit--prefetch-git-commands localname))
               (results (tramp-rpc--call v "commands.run_parallel"
                                         `((commands . ,commands)))))
          (when results
            ;; Each result entry is (key . {exit_code, stdout, stderr}).  Git
            ;; command results are stored as (exit-code . decoded-stdout), while
            ;; state file checks (key starts with "state_file:") populate the
            ;; file-exists cache.
            (tramp-rpc-magit--store-command-results v directory results t)
            ;; Second-stage prefetch for data whose names are only known after
            ;; the first batch (real gitdir for worktrees, branch/upstream
            ;; names, and the files reported by status porcelain).
            (tramp-rpc-magit--prefetch-dynamic-status v directory localname)
            ;; Auto-watch the git worktree
            (let* ((cache (tramp-rpc-magit--get-process-cache))
                   (toplevel-key (tramp-rpc-magit--process-cache-key
                                  "rev-parse" "--show-toplevel"))
                   (toplevel-entry (when cache
                                     (gethash toplevel-key cache)))
                   (toplevel (when (and toplevel-entry (= 0 (car toplevel-entry)))
                               (string-trim (cdr toplevel-entry)))))
              (when toplevel
                (tramp-rpc--auto-watch-git-worktree v toplevel)))))
        ;; Fetch ancestor markers for project/VC detection
        (setq tramp-rpc-magit--ancestors-cache
              (tramp-rpc-ancestors-scan
               directory tramp-rpc-magit--ancestor-marker-names))
        (when tramp-rpc-magit--debug
          (let ((cache (tramp-rpc-magit--get-process-cache)))
            (tramp-rpc--debug "tramp-rpc-magit: prefetched %d commands + ancestors for %s"
                              (if cache (hash-table-count cache) 0)
                              directory)))))))

(defun tramp-rpc--auto-watch-git-worktree (vec toplevel)
  "Automatically watch a git worktree after prefetch.
VEC is the TRAMP connection vector.  TOPLEVEL is the local path
of the git worktree root on the remote."
  (when toplevel
    (let* ((key (format "%s:%s" (tramp-rpc--connection-key-string vec) toplevel))
           (entry (gethash key tramp-rpc--watched-directories)))
      (unless (tramp-rpc--watch-entry-recursive-p entry)
        ;; Not yet watching this worktree recursively - start watching.
        (condition-case err
            (progn
              (tramp-rpc-watch-directory
               (tramp-make-tramp-file-name vec toplevel) t)
              (tramp-rpc--debug "auto-watching git worktree %s" toplevel))
          (error
           (tramp-rpc--debug "failed to auto-watch %s: %s"
                             toplevel (error-message-string err))))))))

;; ============================================================================
;; Ancestor directory scanning
;; ============================================================================

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

(defun tramp-rpc-magit--ancestor-scan-cache-key (directory)
  "Return cache key for ancestor scan rooted at DIRECTORY."
  (cons (file-remote-p directory) (tramp-file-local-name directory)))

(defun tramp-rpc-magit--ancestor-scan-for-directory (directory)
  "Return cached ancestor marker scan for DIRECTORY, fetching it if needed."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (key (tramp-rpc-magit--ancestor-scan-cache-key directory)))
    (or (gethash key tramp-rpc-magit--ancestor-scan-caches)
        (puthash key
                 (tramp-rpc-ancestors-scan
                  directory tramp-rpc-magit--ancestor-marker-names)
                 tramp-rpc-magit--ancestor-scan-caches))))

(defun tramp-rpc-magit--ancestor-cache-covers-p (scan-directory candidate-dir)
  "Return non-nil if SCAN-DIRECTORY's ancestor scan covers CANDIDATE-DIR."
  (let ((scan (file-name-as-directory (directory-file-name scan-directory)))
        (candidate (file-name-as-directory (directory-file-name candidate-dir))))
    (string-prefix-p candidate scan)))

(defun tramp-rpc-magit--file-exists-in-ancestor-scan (filename scan)
  "Return FILENAME existence using ancestor SCAN, or `not-cached'."
  (let* ((expanded (expand-file-name filename))
         (basename (file-name-nondirectory expanded))
         (entry (assoc basename scan)))
    (if entry
        (if (cdr entry)
            (let ((found-dir (directory-file-name (cdr entry)))
                  (candidate-dir (directory-file-name
                                  (file-name-directory
                                   (tramp-file-local-name expanded)))))
              (cond
               ((string= found-dir candidate-dir) t)
               ;; The closest hit proves there is no matching marker below
               ;; FOUND-DIR in the scanned ancestor chain, but it says nothing
               ;; about ancestors above FOUND-DIR.  Let those lookups fall
               ;; through to a direct stat instead of caching a false nil.
               ((string-prefix-p
                 (file-name-as-directory found-dir)
                 (file-name-as-directory candidate-dir))
                nil)
               (t 'not-cached)))
          nil)
      'not-cached)))

(defun tramp-rpc-magit--file-exists-p (filename)
  "Check if FILENAME exists using cached ancestor data.
Returns t, nil, or \\='not-cached if not in cache."
  (let* ((expanded (expand-file-name filename))
         (basename (file-name-nondirectory expanded))
         (file-dir (file-name-as-directory
                    (directory-file-name
                     (or (file-name-directory (tramp-file-local-name expanded))
                         (tramp-file-local-name expanded)))))
         (answer 'not-cached))
    (when (member basename tramp-rpc-magit--ancestor-marker-names)
      ;; Reuse any dynamic scan whose root is below this candidate directory;
      ;; ancestor scans cover all parents of their search root.
      (maphash
       (lambda (key scan)
         (when (and (eq answer 'not-cached)
                    (equal (car key) (file-remote-p expanded))
                    (tramp-rpc-magit--ancestor-cache-covers-p
                     (cdr key) file-dir))
           (setq answer
                 (tramp-rpc-magit--file-exists-in-ancestor-scan
                  expanded scan))))
       tramp-rpc-magit--ancestor-scan-caches)
      (when (and (eq answer 'not-cached)
                 tramp-rpc-magit--ancestors-cache
                 tramp-rpc-magit--prefetch-directory
                 (tramp-rpc-magit--ancestor-cache-covers-p
                  (tramp-file-local-name tramp-rpc-magit--prefetch-directory)
                  file-dir))
        (setq answer
              (tramp-rpc-magit--file-exists-in-ancestor-scan
               expanded tramp-rpc-magit--ancestors-cache)))
      ;; If this is a marker under the prefetched repository, one high-level
      ;; ancestor scan from the queried directory replaces dozens of serial
      ;; file.stat calls as project.el/Projectile walk upward.  Preserve nil as
      ;; a real cached answer, not as "try the next fallback".
      (when (and (eq answer 'not-cached)
                 tramp-rpc-magit--prefetch-directory
                 (equal (file-remote-p expanded)
                        (file-remote-p tramp-rpc-magit--prefetch-directory))
                 (string-prefix-p
                  (file-name-as-directory
                   (directory-file-name
                    (tramp-file-local-name tramp-rpc-magit--prefetch-directory)))
                  (tramp-file-local-name expanded)))
        (setq answer
              (tramp-rpc-magit--file-exists-in-ancestor-scan
               expanded
               (tramp-rpc-magit--ancestor-scan-for-directory
                (file-name-directory expanded))))))
    answer))

;; ============================================================================
;; Cache clearing
;; ============================================================================

(defun tramp-rpc-magit--clear-status-cache ()
  "Clear only the status cache (git state that changes frequently)."
  (clrhash tramp-rpc-magit--process-caches))

(defun tramp-rpc-magit--clear-cache ()
  "Clear all magit-related caches."
  (clrhash tramp-rpc-magit--process-caches)
  (clrhash tramp-rpc-magit--ancestor-scan-caches)
  (setq tramp-rpc-magit--ancestors-cache nil)
  (setq tramp-rpc-magit--prefetch-directory nil))

;; ============================================================================
;; Lazy Magit section expansion
;; ============================================================================

(defconst tramp-rpc-magit--lazy-status-section-types
  '(unstaged staged untracked tracked ignored skip-worktree assume-unchanged file)
  "Magit status section types whose bodies may run git lazily on expansion.")

(defun tramp-rpc-magit--section-slot (section slot)
  "Return SECTION's SLOT value, or nil if unavailable."
  (ignore-errors (eieio-oref section slot)))

(defun tramp-rpc-magit--maybe-prefetch-for-section (section)
  "Ensure batched data exists before expanding lazy Magit SECTION."
  (when (and tramp-rpc-magit-optimize
             (derived-mode-p 'magit-status-mode)
             (file-remote-p default-directory)
             (tramp-rpc-file-name-p default-directory)
             (tramp-rpc-magit--section-slot section 'hidden)
             (memq (tramp-rpc-magit--section-slot section 'type)
                   tramp-rpc-magit--lazy-status-section-types))
    (if (eq (tramp-rpc-magit--section-slot section 'type) 'file)
        (tramp-rpc-magit--prefetch-file-section section)
      (when (null (tramp-rpc-magit--get-process-cache))
        (tramp-rpc-magit--prefetch default-directory)))))

(defun tramp-rpc-magit--section-show-advice (orig section)
  "Advice around `magit-section-show' for lazy remote status sections."
  (let ((tramp-rpc-magit--allow-process-cache t))
    (tramp-rpc-magit--maybe-prefetch-for-section section)
    (let ((magit-diff-adjust-tab-width
           (if (and tramp-rpc-magit-disable-remote-diff-tab-width-detection
                    (derived-mode-p 'magit-status-mode)
                    (file-remote-p default-directory)
                    (tramp-rpc-file-name-p default-directory))
               nil
             (and (boundp 'magit-diff-adjust-tab-width)
                  magit-diff-adjust-tab-width))))
      (funcall orig section))))

;; ============================================================================
;; Magit handlers
;; ============================================================================

(defun tramp-rpc-handle-magit-status-setup-buffer (&optional directory)
  "Handler for `magit-status-setup-buffer' to prefetch data.
Suppresses fs.events cache handling during refresh to prevent
inotify events (from git commands touching .git/index etc.) from
clearing caches mid-refresh."
  (let* ((directory (or directory default-directory))
         (tramp-rpc--suppress-fs-notifications t)
         (tramp-rpc-magit--allow-process-cache t)
         (tramp-rpc-magit--status-setup-prefetch-active t)
         (magit-diff-adjust-tab-width
          (if (and tramp-rpc-magit-disable-remote-diff-tab-width-detection
                   (file-remote-p directory)
                   (tramp-rpc-file-name-p directory))
              nil
            (and (boundp 'magit-diff-adjust-tab-width)
                 magit-diff-adjust-tab-width))))
    (tramp-rpc-magit--clear-status-cache)
    ;; Drop stale metadata before refresh.  Fresh metadata prefetched during the
    ;; refresh must survive so lazy Magit section expansion can reuse it.
    (let ((default-directory (if (directory-name-p directory)
                                 directory
                               (concat directory "/"))))
      (tramp-rpc--clear-file-metadata-caches))
    (condition-case err
        (tramp-run-real-handler 'magit-status-setup-buffer (list directory))
      (error
       (let ((default-directory (if (directory-name-p directory)
                                    directory
                                  (concat directory "/"))))
         (tramp-rpc--clear-file-metadata-caches))
       (signal (car err) (cdr err))))))

(defun tramp-rpc-handle-magit-status-refresh-buffer ()
  "Handler for `magit-status-refresh-buffer' to prefetch data.
Suppresses fs.events cache handling during refresh to prevent
inotify events from clearing caches mid-refresh."
  (unless tramp-rpc-magit--status-setup-prefetch-active
    (tramp-rpc-magit--clear-status-cache))
  (let ((tramp-rpc--suppress-fs-notifications t)
        (tramp-rpc-magit--allow-process-cache t)
        (magit-diff-adjust-tab-width
         (if (and tramp-rpc-magit-disable-remote-diff-tab-width-detection
                  (file-remote-p default-directory)
                  (tramp-rpc-file-name-p default-directory))
             nil
           (and (boundp 'magit-diff-adjust-tab-width)
                magit-diff-adjust-tab-width))))
    ;; Drop stale metadata before refresh.  Fresh metadata prefetched during the
    ;; refresh must survive so lazy Magit section expansion can reuse it.
    (tramp-rpc--clear-file-metadata-caches)
    (condition-case err
        (tramp-run-real-handler 'magit-status-refresh-buffer nil)
      (error
       (tramp-rpc--clear-file-metadata-caches)
       (signal (car err) (cdr err))))))

;;;###autoload
(defun tramp-rpc-magit-enable ()
  "Enable tramp-rpc magit optimizations.
This uses parallel command prefetching to dramatically speed up
magit-status on remote repositories."
  (interactive)
  ;; Make repeated enable calls idempotent.
  (advice-remove 'magit-section-show #'tramp-rpc-magit--section-show-advice)
  (when (fboundp 'magit-section-show)
    (advice-add 'magit-section-show :around #'tramp-rpc-magit--section-show-advice))
  (with-eval-after-load 'magit-section
    (advice-remove 'magit-section-show #'tramp-rpc-magit--section-show-advice)
    (advice-add 'magit-section-show :around #'tramp-rpc-magit--section-show-advice))
  (tramp-add-external-operation
   'magit-status-setup-buffer
   #'tramp-rpc-handle-magit-status-setup-buffer 'tramp-rpc)
  (tramp-add-external-operation
   'magit-status-refresh-buffer
   #'tramp-rpc-handle-magit-status-refresh-buffer 'tramp-rpc)
  (message "tramp-rpc magit optimizations enabled"))

;;;###autoload
(defun tramp-rpc-magit-disable ()
  "Disable tramp-rpc magit optimizations."
  (interactive)
  (advice-remove 'magit-section-show #'tramp-rpc-magit--section-show-advice)
  (tramp-remove-external-operation 'magit-status-setup-buffer 'tramp-rpc)
  (tramp-remove-external-operation 'magit-status-refresh-buffer 'tramp-rpc)
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

;; Fix #m4: Auto-enable behind defcustom gate
(with-eval-after-load 'magit
  (with-eval-after-load 'tramp-rpc
    (when tramp-rpc-magit-optimize
      (tramp-rpc-magit-enable))))

;; ============================================================================
;; Projectile optimizations
;; ============================================================================

(defvar projectile-projects-cache)
(defvar projectile-projects-cache-time)
(defvar projectile-git-use-fd)

(defun tramp-rpc-handle-projectile-dir-files (directory)
  "Handler to use alien indexing for remote project files.
`projectile-dir-files-alien' (via `projectile-get-ext-command') checks
fd availability via `executable-find' on the LOCAL machine, but fd may
not be available on the REMOTE.  Binding `projectile-git-use-fd' to nil
forces git ls-files instead."
  (let ((projectile-git-use-fd nil))
    (projectile-dir-files-alien directory)))

(defun tramp-rpc-handle-projectile-project-files (project-root)
  "Handler to use alien indexing for remote project files.
This bypasses the expensive `file-relative-name' calls in hybrid mode."
  ;; For remote RPC directories, use alien indexing directly
  (let ((files nil))
    ;; Check cache first (like projectile-project-files does)
    (when (and (bound-and-true-p projectile-enable-caching)
               (boundp 'projectile-projects-cache))
      (setq files (gethash project-root projectile-projects-cache)))
    ;; If not cached, fetch and cache
    (unless files
      (setq files (tramp-rpc-handle-projectile-dir-files project-root))
      (when (and (bound-and-true-p projectile-enable-caching)
                 (boundp 'projectile-projects-cache)
                 (boundp 'projectile-projects-cache-time)
                 (fboundp 'projectile-time-seconds))
        (puthash project-root files projectile-projects-cache)
        (puthash project-root (projectile-time-seconds) projectile-projects-cache-time)))
    files))

;;;###autoload
(defun tramp-rpc-projectile-enable ()
  "Enable tramp-rpc projectile optimizations.
This ensures fd is not used for remote directories where it may not
be available, and uses alien indexing for better performance."
  (interactive)
  (tramp-add-external-operation
   'projectile-dir-files
   #'tramp-rpc-handle-projectile-dir-files 'tramp-rpc)
  (tramp-add-external-operation
   'projectile-project-files
   #'tramp-rpc-handle-projectile-project-files 'tramp-rpc)
  (message "tramp-rpc projectile optimizations enabled"))

;;;###autoload
(defun tramp-rpc-projectile-disable ()
  "Disable tramp-rpc projectile optimizations."
  (interactive)
  (tramp-remove-external-operation 'projectile-dir-files 'tramp-rpc)
  (tramp-remove-external-operation 'projectile-project-files 'tramp-rpc)
  (message "tramp-rpc projectile optimizations disabled"))

;; Auto-enable when projectile is loaded
(with-eval-after-load 'projectile
  (with-eval-after-load 'tramp-rpc
    (tramp-rpc-projectile-enable)))

;; ============================================================================
;; Unload support
;; ============================================================================

(defun tramp-rpc-magit-unload-function ()
  "Unload function for tramp-rpc-magit.
Removes handlers."
  ;; Remove all handlers.
  (tramp-rpc-magit-disable)
  (tramp-rpc-projectile-disable)
  ;; Return nil to allow normal unload to proceed
  nil)

(add-hook 'tramp-rpc-unload-hook
	  (lambda ()
	    (when (featurep 'tramp-rpc-magit)
	      (unload-feature 'tramp-rpc-magit 'force))))

(provide 'tramp-rpc-magit)
;;; tramp-rpc-magit.el ends here
