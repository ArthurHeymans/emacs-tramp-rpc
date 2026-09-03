;;; tramp-rpc-cache.el --- Metadata caches and fs watching for TRAMP-RPC -*- lexical-binding: t; -*-

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

;; This file provides the client-side metadata caches and the filesystem
;; watch machinery that keeps them fresh:
;; - TTL-based `file-exists-p', `file-truename' and file.stat caches
;; - Cache invalidation for paths, subtrees and whole connections
;; - Watch management (add/remove/list watched directories)
;; - Dispatch of server-initiated fs.events notifications to cache
;;   invalidation and to public file-notify descriptors
;;
;; Magit-specific status prefetching lives in tramp-rpc-magit.el and hooks
;; into this module through the connection-scoped clearing functions.

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 'tramp-cache)
(require 'tramp-rpc-protocol)
(require 'tramp-rpc-connection)

;; Functions from tramp-rpc.el
(declare-function tramp-rpc--call "tramp-rpc")
(declare-function tramp-rpc--get-connection "tramp-rpc" (vec))
(declare-function tramp-rpc--connection-key "tramp-rpc")
(declare-function tramp-rpc--decode-string "tramp-rpc")
(declare-function tramp-rpc--canonical-watch-active-p "tramp-rpc")
(declare-function tramp-rpc--file-notify-alias-paths "tramp-rpc")
(declare-function tramp-rpc--file-notify-dispatch "tramp-rpc")
(declare-function tramp-rpc--file-notify-dispatch-rescan "tramp-rpc")
(declare-function tramp-rpc--watch-entry-canonical-directory "tramp-rpc")
(declare-function tramp-rpc--clear-direnv-cache "tramp-rpc" (&optional vec))
(declare-function tramp-rpc--flush-owned-route-connection-properties
                  "tramp-rpc" (vec))

;; Functions from tramp-rpc-magit.el
(declare-function tramp-rpc-magit--clear-cache "tramp-rpc-magit")
(declare-function tramp-rpc-magit--clear-ancestor-caches "tramp-rpc-magit")
(declare-function tramp-rpc-magit--clear-ancestor-caches-for-connection
                  "tramp-rpc-magit" (vec))
(declare-function tramp-rpc-magit--clear-status-cache-for-connection
                  "tramp-rpc-magit" (vec))

(defvar tramp-rpc--connections)
(defvar tramp-rpc--exec-path-cache)
(defvar tramp-rpc--login-shell-cache)
(defvar tramp-rpc--watcher-degraded)
(defvar tramp-rpc--watcher-unavailable-ttl)

;; ============================================================================
;; Cache infrastructure
;; ============================================================================

(defvar tramp-rpc--cache-ttl 300
  "Time-to-live for cache entries in seconds.")

(defvar tramp-rpc--cache-max-size 10000
  "Maximum number of entries per cache before eviction.")

(defvar tramp-rpc--file-exists-cache (make-hash-table :test 'equal)
  "Cache for `file-exists-p' results.
Keys are expanded filenames, values are (TIMESTAMP . RESULT).")

(defvar tramp-rpc--file-truename-cache (make-hash-table :test 'equal)
  "Cache for `file-truename' results.
Keys are expanded filenames, values are (TIMESTAMP . TRUENAME).")

(defvar tramp-rpc--file-stat-cache (make-hash-table :test 'equal)
  "Cache for file.stat results.
Keys are (EXPANDED-FILENAME . LSTAT), values are (TIMESTAMP . STAT).
STAT may be nil, which records a missing file.")

(defun tramp-rpc--effective-cache-ttl ()
  "Return the current TTL for TRAMP-RPC metadata caches.
`remote-file-name-inhibit-cache' t disables caching, while a numeric value
caps the project-specific TTL.  Nil retains the explicit TRAMP-RPC TTL.
When push notifications are unavailable (`tramp-rpc--watcher-degraded'),
caches are TTL-only, so cap to `tramp-rpc--watcher-unavailable-ttl'."
  (let ((ttl (cond
                ((eq remote-file-name-inhibit-cache t) 0)
                ((numberp remote-file-name-inhibit-cache)
                 (min tramp-rpc--cache-ttl (max 0 remote-file-name-inhibit-cache)))
                (t tramp-rpc--cache-ttl))))
    (if (and (boundp 'tramp-rpc--watcher-degraded)
             tramp-rpc--watcher-degraded)
        (min ttl tramp-rpc--watcher-unavailable-ttl)
      ttl)))

(defun tramp-rpc--cache-entry-valid-p (timestamp)
  "Return non-nil when a cache entry created at TIMESTAMP is reusable."
  (let ((age (- (float-time) timestamp))
        (ttl (tramp-rpc--effective-cache-ttl)))
    (cond
     ((eq remote-file-name-inhibit-cache t) nil)
     ((numberp remote-file-name-inhibit-cache)
      (< age ttl))
     ;; TRAMP also binds this to `current-time' to invalidate entries older
     ;; than the start of a compound operation.
     ((consp remote-file-name-inhibit-cache)
      (and (not (time-less-p (seconds-to-time timestamp)
                             remote-file-name-inhibit-cache))
           (< age ttl)))
     (t (< age ttl)))))

(defun tramp-rpc--cache-get (cache key)
  "Get value for KEY from CACHE if not expired.
Returns the cached value, or nil if not found or expired."
  (when-let* ((entry (gethash key cache)))
    (let ((timestamp (car entry))
          (value (cdr entry)))
      (if (tramp-rpc--cache-entry-valid-p timestamp)
          value
        ;; Expired, remove it
        (remhash key cache)
        nil))))

(defun tramp-rpc--cache-put (cache key value)
  "Store VALUE for KEY in CACHE with current timestamp.
Evicts the oldest 25% of entries when cache reaches the maximum size."
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
        (if (tramp-rpc--cache-entry-valid-p timestamp)
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
  "Evict the oldest 25% of entries from CACHE."
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

(defun tramp-rpc--hash-remove-if (predicate table &optional callback)
  "Remove TABLE entries satisfying PREDICATE without mutating during `maphash'.
PREDICATE receives key and value.  CALLBACK, when non-nil, is called with each
removed key and value.  Return the number of removed entries."
  (let (entries)
    (maphash (lambda (key value)
               (when (funcall predicate key value)
                 (push (cons key value) entries)))
             table)
    (dolist (entry entries)
      (remhash (car entry) table)
      (when callback
        (funcall callback (car entry) (cdr entry))))
    (length entries)))

(defun tramp-rpc--invalidate-cache-for-path (filename)
  "Invalidate cache entries for FILENAME."
  ;; Ancestor scans are keyed by connection.  A mutation on one remote must
  ;; not evict Magit's repository discovery results for every other remote.
  (when (tramp-tramp-file-p filename)
    (with-parsed-tramp-file-name filename nil
      (tramp-rpc-magit--clear-ancestor-caches-for-connection v)))
  (cl-labels ((drop (candidate)
                (remhash candidate tramp-rpc--file-exists-cache)
                (remhash candidate tramp-rpc--file-truename-cache)
                (remhash (cons candidate nil) tramp-rpc--file-stat-cache)
                (remhash (cons candidate t) tramp-rpc--file-stat-cache))
              (flush-tramp-properties (candidate)
                (when (tramp-tramp-file-p candidate)
                  (with-parsed-tramp-file-name candidate nil
                    (tramp-flush-file-properties v localname))))
              (flush-tramp-directory-properties (candidate)
                (when (tramp-tramp-file-p candidate)
                  (with-parsed-tramp-file-name candidate nil
                    (tramp-flush-directory-properties v localname))))
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
                    (with-parsed-tramp-file-name candidate nil
                      (tramp-flush-file-properties v localname)
                      (tramp-flush-directory-properties v localname))))
                (drop-string-prefix (cache)
                  (tramp-rpc--hash-remove-if
                   (lambda (key _value)
                     (and (stringp key)
                          (string-prefix-p expanded-dir key)))
                   cache
                   (lambda (key _value)
                     (flush-tramp-properties key))))
                (drop-stat-prefix ()
                  (tramp-rpc--hash-remove-if
                   (lambda (key _value)
                     (and (consp key)
                          (stringp (car key))
                          (string-prefix-p expanded-dir (car key))))
                   tramp-rpc--file-stat-cache
                   (lambda (key _value)
                     (flush-tramp-properties (car key))))))
      (drop-string-prefix tramp-rpc--file-exists-cache)
      (drop-string-prefix tramp-rpc--file-truename-cache)
      (drop-stat-prefix))))

(defun tramp-rpc-clear-file-exists-cache ()
  "Clear the `file-exists-p' cache."
  (interactive)
  (clrhash tramp-rpc--file-exists-cache))

(defun tramp-rpc-clear-file-truename-cache ()
  "Clear the `file-truename' cache."
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
  (clrhash tramp-rpc--file-stat-cache)
  (tramp-rpc-magit--clear-ancestor-caches))

(defun tramp-rpc-clear-all-caches ()
  "Clear all project-owned caches and route-aware connection properties."
  (interactive)
  (tramp-rpc-magit--clear-cache)
  (tramp-rpc--clear-file-metadata-caches)
  (tramp-rpc--clear-direnv-cache)
  (clrhash tramp-rpc--exec-path-cache)
  (clrhash tramp-rpc--login-shell-cache)
  (maphash
   (lambda (_key connection)
     (when-let* ((vec (tramp-rpc-connection-vec connection)))
       (tramp-flush-directory-properties vec "/")
       (tramp-rpc--flush-owned-route-connection-properties vec)))
   tramp-rpc--connections))

(defun tramp-rpc--clear-file-caches-for-connection (vec)
  "Clear file-exists and `file-truename' cache entries for connection VEC.
Entries are keyed by expanded TRAMP filenames; this removes those
matching the remote prefix of VEC."
  (tramp-rpc-magit--clear-ancestor-caches-for-connection vec)
  (tramp-flush-directory-properties vec "/")
  (let ((prefix (tramp-make-tramp-file-name vec "/")))
    ;; Match the prefix up to the colon-slash that starts the localname.
    ;; e.g. "/rpc:user@host:/" -- any key starting with this belongs to VEC.
    (dolist (cache (list tramp-rpc--file-exists-cache
                         tramp-rpc--file-truename-cache))
      (tramp-rpc--hash-remove-if
       (lambda (key _value) (string-prefix-p prefix key)) cache))
    (tramp-rpc--hash-remove-if
     (lambda (key _value)
       (and (consp key) (string-prefix-p prefix (car key))))
     tramp-rpc--file-stat-cache)))

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


(defun tramp-rpc--watch-entry-recursive-p (entry)
  "Return non-nil if watched-directory ENTRY is recursive."
  (and (consp entry) (plist-get entry :recursive)))

(defun tramp-rpc--handle-notification (process method params)
  "Handle a server-initiated notification.
PROCESS is the connection, METHOD is the notification method,
PARAMS is the notification parameters."
  (condition-case notify-error
      (cond
       ((string= method "fs.events")
        (tramp-rpc--handle-fs-events process params))
       (t
        (tramp-rpc--debug "Unknown notification: %s" method)))
    (error
     (tramp-rpc--debug "notification %s failed: %S" method notify-error))))

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
      (when-let* ((vec (process-get process :tramp-rpc-vec))
                  (connection (tramp-rpc--get-connection vec))
                  ((eq process (tramp-rpc-connection-process connection))))
        (when (or (not tramp-rpc--suppress-fs-notifications)
                  (cl-some (lambda (event)
                             (equal (alist-get 'action event) "rescan"))
                           events))
          ;; Git state changed on this transport, not every remote connection.
          ;; A rescan means concrete events were lost, so it must invalidate
          ;; status state even while ordinary notification handling is suppressed.
          (tramp-rpc-magit--clear-status-cache-for-connection vec))
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
                    (progn
                      ;; A rescan means concrete paths were dropped, including
                      ;; potentially unrelated changes that the suppressed
                      ;; operation will not invalidate itself.
                      (tramp-rpc--clear-file-caches-for-connection vec)
                      ;; Public file-notify consumers still need a conservative
                      ;; event for the dropped paths.
                      (tramp-rpc--file-notify-dispatch-rescan process))
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
  "Start watching DIRECTORY for filesystem change events.
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
                         :canonical-directory canonical-directory
                         :connection-process (tramp-rpc--connection-transport (tramp-rpc--get-connection v)))
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
                           :canonical-directory canonical-directory
                           :connection-process (tramp-rpc--connection-transport (tramp-rpc--get-connection v)))
                   tramp-rpc--watched-directories))))
    (tramp-rpc--debug "Watching: %s (recursive=%s)" localname recursive)))

(defun tramp-rpc-unwatch-directory (directory)
  "Stop watching DIRECTORY for filesystem change events."
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

(defun tramp-rpc--cleanup-watches-for-connection (vec &optional connection-process)
  "Remove watched directory entries for VEC's CONNECTION-PROCESS."
  (let* ((conn-key (tramp-rpc--connection-key-string vec))
         (removed
          (tramp-rpc--hash-remove-if
           (lambda (key value)
             (and (string-prefix-p (concat conn-key ":") key)
                  (or (null connection-process)
                      (null (plist-get value :connection-process))
                      (eq connection-process
                          (plist-get value :connection-process)))))
           tramp-rpc--watched-directories)))
    (when (> removed 0)
      (tramp-rpc--debug "Cleaned up %d watches for %s"
                        removed conn-key))))

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

(provide 'tramp-rpc-cache)
;;; tramp-rpc-cache.el ends here
