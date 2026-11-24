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
;; - TTL-based `file-exists-p', `file-truename' and file.stat caches, and
;;   the file.stat to `file-attributes' conversion (with nanosecond times)
;; - Cache invalidation for paths, subtrees and whole connections
;; - Unified watch lease management (`tramp-rpc-watch-lease') shared
;;   between explicit directory watches and `tramp-rpc-notify.el'
;; - Dispatch of server-initiated fs.events notifications to cache
;;   invalidation and to registered hooks (e.g., `tramp-rpc-notify.el')
;;
;; Higher layers (`tramp-rpc-notify.el', `tramp-rpc-magit.el') subscribe
;; via the hooks defined below; this module never calls into them.

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 'tramp-cache)
(require 'tramp-rpc-protocol)
(require 'tramp-rpc-connection)
(require 'tramp-rpc-hops)
(require 'tramp-rpc-transport)
(require 'tramp-rpc-environment)

;; ============================================================================
;; Hooks into higher layers
;; ============================================================================

(defvar tramp-rpc-cache-invalidate-functions nil
  "Functions called with VEC when cached file metadata for VEC is dropped.
Run for path, subtree and connection-wide invalidation so modules deriving
state from file metadata can drop their entries for that connection.  VEC
is nil when every cache is cleared, regardless of connection.")

(defvar tramp-rpc-fs-events-functions nil
  "Functions called with VEC and PATHS for filesystem events on VEC.
PATHS lists the changed TRAMP file names, or is nil when the changes are
unknown because the server lost events.  Run before the event paths are
invalidated.  Events in `tramp-rpc--expected-fs-events' are left out, and
the hook is not run when only such events remain.")

(defvar tramp-rpc-fs-event-dispatch-functions nil
  "Functions called with (ACTION PATH PATH1 COOKIE) for each fs.events item.
Unlike `tramp-rpc-fs-events-functions', this runs even for expected events
so `file-notify' consumers receive every filesystem notification.")

(defvar tramp-rpc-fs-rescan-functions nil
  "Functions called with (PROCESS) when the server reports an fs.events rescan.")

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

(defvar tramp-rpc--in-cache-invalidation nil
  "Non-nil while TRAMP-RPC is flushing TRAMP properties.
Prevents re-entrancy with `:after' advice on `tramp-flush-*'.")

(defun tramp-rpc--effective-cache-ttl (&optional vec)
  "Return the current TTL for TRAMP-RPC metadata caches on VEC.
`remote-file-name-inhibit-cache' t disables caching, while a numeric value
caps the project-specific TTL.  Nil retains the explicit TRAMP-RPC TTL.
When push notifications are unavailable on VEC, caches are TTL-only, so
cap to `tramp-rpc-watcher-unavailable-ttl'."
  (let ((ttl (cond
              ((eq remote-file-name-inhibit-cache t) 0)
              ((numberp remote-file-name-inhibit-cache)
               (min tramp-rpc--cache-ttl (max 0 remote-file-name-inhibit-cache)))
              (t tramp-rpc--cache-ttl))))
    (if (and vec (not (tramp-rpc--watcher-available-p vec)))
        (min ttl tramp-rpc-watcher-unavailable-ttl)
      ttl)))

(defun tramp-rpc--cache-entry-valid-p (timestamp &optional vec)
  "Return non-nil when a cache entry created at TIMESTAMP on VEC is reusable."
  (let ((age (- (float-time) timestamp))
        (ttl (tramp-rpc--effective-cache-ttl vec)))
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

(defun tramp-rpc--cache-get (cache key &optional vec)
  "Get value for KEY from CACHE on VEC if not expired.
Returns the cached value, or nil if not found or expired."
  (when-let* ((entry (gethash key cache)))
    (let ((timestamp (car entry))
          (value (cdr entry)))
      (if (tramp-rpc--cache-entry-valid-p timestamp vec)
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

(defun tramp-rpc--cache-lookup (cache key &optional vec)
  "Return cached value for KEY in CACHE on VEC, or `not-cached'.
Unlike `tramp-rpc--cache-get', this preserves cached nil values."
  (let ((entry (gethash key cache)))
    (if (not entry)
        'not-cached
      (let ((timestamp (car entry))
            (value (cdr entry)))
        (if (tramp-rpc--cache-entry-valid-p timestamp vec)
            value
          (remhash key cache)
          'not-cached)))))

(defun tramp-rpc--file-property-key (vec localname property)
  "Return a route-aware TRAMP cache property for VEC, LOCALNAME and PROPERTY.
TRAMP unifies file keys by stripping hops and trailing slashes.  Include
those distinctions in the property instead, retaining TRAMP's normal
expiry, subtree invalidation and connection cleanup."
  (tramp-rpc--route-property-name
   vec (format "rpc-file:%s:%s" property (string-suffix-p "/" localname))))

(defun tramp-rpc--vec-route-tag (vec)
  "Return the route hash string for VEC, or nil if VEC has no proxy hops."
  (when (and (tramp-file-name-p vec)
             (plist-get (tramp-rpc--connection-key vec) :route))
    (tramp-rpc--route-hash vec)))

(defun tramp-rpc--file-cache-key (vec expanded)
  "Return a route-aware cache key for EXPANDED on VEC.
Direct routes (no proxy hops) key directly on EXPANDED; proxied routes
\(including hidden routes configured via `tramp-default-proxies-alist')
qualify EXPANDED with VEC's resolved route hash so distinct routes to the
same user@host never collide."
  (let* ((v (or (and (tramp-file-name-p vec) vec)
                (and (stringp expanded)
                     (tramp-tramp-file-p expanded)
                     (let ((tramp-default-proxies-alist
                            (tramp-rpc--copy-proxies-alist))
                           (tramp-show-ad-hoc-proxies t))
                       (tramp-dissect-file-name
                        (if (file-name-quoted-p expanded)
                            (file-name-unquote expanded)
                          expanded))))))
         (route-tag (tramp-rpc--vec-route-tag v)))
    (if route-tag
        (cons route-tag expanded)
      expanded)))

(defun tramp-rpc--file-cache-key-path (key)
  "Extract the expanded filename string from file cache KEY."
  (if (consp key) (cdr key) key))

(defun tramp-rpc--file-cache-key-route (key)
  "Extract the route hash tag from file cache KEY, or nil for direct routes."
  (when (consp key) (car key)))

(defun tramp-rpc--file-stat-cache-key (vec localname lstat)
  "Return route-aware file.stat cache key for VEC, LOCALNAME, and LSTAT."
  (let ((expanded
         (let ((tramp-default-proxies-alist
                (tramp-rpc--copy-proxies-alist))
               (tramp-show-ad-hoc-proxies t))
           (expand-file-name (tramp-make-tramp-file-name vec localname)))))
    (cons (tramp-rpc--file-cache-key vec expanded)
          (and lstat t))))

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

(defun tramp-rpc--decode-timestamp (secs nsec)
  "Convert SECS and optional NSEC to a 4-element Emacs Lisp timestamp.
Preserves nanosecond precision when NSEC is an integer."
  (if (and (integerp secs) (integerp nsec))
      (time-convert (cons (+ (* secs 1000000000) nsec) 1000000000) 'list)
    (seconds-to-time secs)))

(defun tramp-rpc--convert-file-attributes (stat id-format)
  "Convert STAT result to Emacs `file-attributes' format.
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
         (atime (tramp-rpc--decode-timestamp
                 (alist-get 'atime stat) (alist-get 'atime_nsec stat)))
         (mtime (tramp-rpc--decode-timestamp
                 (alist-get 'mtime stat) (alist-get 'mtime_nsec stat)))
         (ctime (tramp-rpc--decode-timestamp
                 (alist-get 'ctime stat) (alist-get 'ctime_nsec stat)))
         (size (alist-get 'size stat))
         (mode (tramp-file-mode-from-int (alist-get 'mode stat)))
         (inode (alist-get 'inode stat))
         (dev (alist-get 'dev stat)))
    ;; Quote a symlink target which looks remote.
    (when (and (stringp type) (tramp-tramp-file-p type))
      (setq type (file-name-quote type 'top)))
    ;; Return in file-attributes format
    (list type nlinks
          (if (eq id-format 'string) (or uname (number-to-string uid)) uid)
          (if (eq id-format 'string) (or gname (number-to-string gid)) gid)
          atime mtime ctime
          size mode nil inode dev)))

(defun tramp-rpc--cache-evict (cache)
  "Evict the oldest 25% of entries from CACHE."
  (let ((entries nil))
    ;; Collect all entries with timestamps
    (maphash (lambda (key value)
               (push (cons key (car value)) entries))
             cache)
    ;; Sort by timestamp (oldest first)
    (setq entries (sort entries :key #'cdr :lessp #'< :in-place t))
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
  (if (= (hash-table-count table) 0)
      0
    (let (entries)
      (maphash (lambda (key value)
                 (when (funcall predicate key value)
                   (push (cons key value) entries)))
               table)
      (dolist (entry entries)
        (remhash (car entry) table)
        (when callback
          (funcall callback (car entry) (cdr entry))))
      (length entries))))

(defun tramp-rpc--expand-cache-spellings (filename)
  "Return deduplicated expanded spellings of FILENAME without mutating proxies.
Includes both hop-preserving and hop-stripped forms when FILENAME has hops."
  (let ((tramp-default-proxies-alist
         (tramp-rpc--copy-proxies-alist)))
    (delete-dups
     (list (let ((tramp-show-ad-hoc-proxies t))
             (expand-file-name filename))
           (let ((tramp-show-ad-hoc-proxies nil))
             (expand-file-name filename))))))

(defun tramp-rpc--invalidate-cache-for-path (filename &optional no-tramp-flush)
  "Invalidate cache entries for FILENAME.
When NO-TRAMP-FLUSH (or `tramp-rpc--in-cache-invalidation') is non-nil, skip
calling `tramp-flush-*' (used when already reacting to a TRAMP cache flush)."
  ;; Derived caches are keyed by connection.  A mutation on one remote must
  ;; not evict state for every other remote.
  (let* ((tramp-default-proxies-alist
          (tramp-rpc--copy-proxies-alist))
         (tramp-show-ad-hoc-proxies t)
         (parsed-vec
          (when (tramp-tramp-file-p filename)
            (tramp-dissect-file-name
             (if (file-name-quoted-p filename)
                 (file-name-unquote filename)
               filename))))
         (route-tag (tramp-rpc--vec-route-tag parsed-vec)))
    (when parsed-vec
      (run-hook-with-args 'tramp-rpc-cache-invalidate-functions parsed-vec))
    (let ((skip-tramp-flush (or no-tramp-flush tramp-rpc--in-cache-invalidation))
          (tramp-rpc--in-cache-invalidation t))
      (cl-labels ((drop (candidate)
                    (let ((key (if route-tag (cons route-tag candidate) candidate)))
                      (remhash key tramp-rpc--file-exists-cache)
                      (remhash key tramp-rpc--file-truename-cache)
                      (remhash (cons key nil) tramp-rpc--file-stat-cache)
                      (remhash (cons key t) tramp-rpc--file-stat-cache)))
                  (flush-tramp-properties (candidate)
                    (when (and (not skip-tramp-flush)
                               (tramp-tramp-file-p candidate))
                      (with-parsed-tramp-file-name candidate nil
                        (tramp-flush-file-properties v localname))))
                  (flush-tramp-directory-properties (candidate)
                    (when (and (not skip-tramp-flush)
                               (tramp-tramp-file-p candidate))
                      (with-parsed-tramp-file-name candidate nil
                        (tramp-flush-directory-properties v localname))))
                  (spellings (path)
                    (delete-dups
                     (list path
                           (directory-file-name path)
                           (file-name-as-directory
                            (directory-file-name path))))))
        (dolist (expanded (tramp-rpc--expand-cache-spellings filename))
          (let* ((trimmed (directory-file-name expanded))
                 (parent (file-name-directory trimmed)))
            (dolist (candidate (spellings expanded))
              (drop candidate)
              (flush-tramp-properties candidate)
              (flush-tramp-directory-properties candidate))
            ;; Invalidate the parent directory's own metadata (e.g. mtime/nlinks)
            ;; without flushing TRAMP directory properties for all sibling entries.
            (when (and parent
                       (not (string= (directory-file-name parent) trimmed)))
              (dolist (candidate (spellings parent))
                (drop candidate)
                (flush-tramp-properties candidate)))))))))

(defun tramp-rpc--invalidate-cache-for-subtree (directory &optional no-tramp-flush)
  "Invalidate cache entries for DIRECTORY and all cached descendants.
When NO-TRAMP-FLUSH (or `tramp-rpc--in-cache-invalidation') is non-nil, skip
calling `tramp-flush-*'."
  (let* ((tramp-default-proxies-alist
          (tramp-rpc--copy-proxies-alist))
         (tramp-show-ad-hoc-proxies t)
         (parsed-vec
          (when (tramp-tramp-file-p directory)
            (tramp-dissect-file-name
             (if (file-name-quoted-p directory)
                 (file-name-unquote directory)
               directory))))
         (route-tag (tramp-rpc--vec-route-tag parsed-vec))
         (expanded-dirs
          (delete-dups
           (mapcar #'file-name-as-directory
                   (tramp-rpc--expand-cache-spellings directory))))
         (skip-tramp-flush (or no-tramp-flush tramp-rpc--in-cache-invalidation)))
    (let ((tramp-rpc--in-cache-invalidation skip-tramp-flush))
      (tramp-rpc--invalidate-cache-for-path
       (directory-file-name (car expanded-dirs))))
    (let ((tramp-rpc--in-cache-invalidation t))
      (cl-labels ((matches-key-p (key)
                    (let ((path (tramp-rpc--file-cache-key-path key)))
                      (and (equal (tramp-rpc--file-cache-key-route key) route-tag)
                           (stringp path)
                           (cl-some (lambda (dir) (string-prefix-p dir path))
                                    expanded-dirs))))
                  (flush-tramp-properties (candidate)
                    (when (and (not skip-tramp-flush)
                               (stringp candidate)
                               (tramp-tramp-file-p candidate))
                      (with-parsed-tramp-file-name candidate nil
                        (tramp-flush-file-properties v localname)
                        (tramp-flush-directory-properties v localname))))
                  (drop-key-prefix (cache)
                    (tramp-rpc--hash-remove-if
                     (lambda (key _value) (matches-key-p key))
                     cache
                     (lambda (key _value)
                       (flush-tramp-properties
                        (tramp-rpc--file-cache-key-path key)))))
                  (drop-stat-prefix ()
                    (tramp-rpc--hash-remove-if
                     (lambda (key _value)
                       (and (consp key) (matches-key-p (car key))))
                     tramp-rpc--file-stat-cache
                     (lambda (key _value)
                       (flush-tramp-properties
                        (tramp-rpc--file-cache-key-path (car key)))))))
        (drop-key-prefix tramp-rpc--file-exists-cache)
        (drop-key-prefix tramp-rpc--file-truename-cache)
        (drop-stat-prefix)))))

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
  "Clear cached file metadata for every connection."
  (clrhash tramp-rpc--file-exists-cache)
  (clrhash tramp-rpc--file-truename-cache)
  (clrhash tramp-rpc--file-stat-cache)
  (run-hook-with-args 'tramp-rpc-cache-invalidate-functions nil))

(defun tramp-rpc-clear-all-caches ()
  "Clear all project-owned caches and route-aware connection properties."
  (interactive)
  (let ((tramp-rpc--in-cache-invalidation t))
    (tramp-rpc--clear-file-metadata-caches)
    (tramp-rpc--clear-environment-caches)
    (maphash
     (lambda (_key connection)
       (when-let* ((vec (tramp-rpc-connection-vec connection)))
         (tramp-flush-directory-properties vec "/")
         (tramp-rpc--flush-owned-route-connection-properties vec)))
     tramp-rpc--connections)))

(defun tramp-rpc--connection-prefixes (vec)
  "Return deduplicated TRAMP root prefixes for VEC without mutating proxies."
  (let ((tramp-default-proxies-alist tramp-cache-undefined))
    (delete-dups
     (list (let ((tramp-show-ad-hoc-proxies t))
             (tramp-make-tramp-file-name vec "/"))
           (let ((tramp-show-ad-hoc-proxies nil))
             (tramp-make-tramp-file-name vec "/"))))))

(defvar tramp-rpc--cache-invalidation-all-routes nil
  "Non-nil while `tramp-rpc-cache-invalidate-functions' covers all routes.")

(defun tramp-rpc--vec-endpoint-identity (vec)
  "Return the hop-independent endpoint identity for VEC."
  (tramp-rpc--connection-key-route-hop vec))

(defun tramp-rpc--connection-key-endpoint-prefix (vec)
  "Return the route-independent prefix of `tramp-rpc--connection-key-string'.
VEC identifies the target endpoint.  Derived directly from endpoint fields so
all-route invalidation does not depend on resolving current proxy rules."
  (let ((print-length nil)
        (print-level nil)
        (print-circle nil))
    (format "(:method %S :host %S :user %S :port %S :route "
            (tramp-rpc--hop-component-string (tramp-file-name-method vec))
            (tramp-rpc--hop-component-string (tramp-file-name-host vec))
            (tramp-rpc--hop-component-string (tramp-file-name-user vec))
            (or (tramp-rpc--port-to-string (tramp-file-name-port vec)) "22"))))

(defun tramp-rpc--clear-file-caches-for-connection (vec &optional no-tramp-flush all-routes)
  "Clear cached file metadata for connection VEC.
Entries are keyed by route-aware expanded TRAMP filenames; this removes those
matching VEC and lets derived caches follow through
`tramp-rpc-cache-invalidate-functions'.
When NO-TRAMP-FLUSH (or `tramp-rpc--in-cache-invalidation') is non-nil, skip
calling `tramp-flush-directory-properties'.
When ALL-ROUTES is non-nil, match any cached proxy route (hidden or explicit)
for VEC's endpoint."
  (let ((tramp-rpc--cache-invalidation-all-routes all-routes))
    (run-hook-with-args 'tramp-rpc-cache-invalidate-functions vec))
  (unless (or no-tramp-flush tramp-rpc--in-cache-invalidation)
    (let ((tramp-rpc--in-cache-invalidation t))
      (tramp-flush-directory-properties vec "/")))
  (when (or (> (hash-table-count tramp-rpc--file-exists-cache) 0)
            (> (hash-table-count tramp-rpc--file-truename-cache) 0)
            (> (hash-table-count tramp-rpc--file-stat-cache) 0))
    (let ((prefixes (tramp-rpc--connection-prefixes vec))
          (route-tag (unless all-routes (tramp-rpc--vec-route-tag vec)))
          (endpoint (and all-routes (tramp-rpc--vec-endpoint-identity vec))))
      (cl-flet ((matches-key-p (key)
                  (let ((path (tramp-rpc--file-cache-key-path key)))
                    (and (stringp path)
                         (if all-routes
                             (or (cl-some (lambda (prefix) (string-prefix-p prefix path))
                                          prefixes)
                                 (and (tramp-tramp-file-p path)
                                      (let ((tramp-default-proxies-alist
                                             tramp-cache-undefined))
                                        (ignore-errors
                                          (equal (tramp-rpc--vec-endpoint-identity
                                                  (tramp-dissect-file-name path))
                                                 endpoint)))))
                           (and (equal (tramp-rpc--file-cache-key-route key) route-tag)
                                (cl-some (lambda (prefix) (string-prefix-p prefix path))
                                         prefixes)))))))
        (dolist (cache (list tramp-rpc--file-exists-cache
                             tramp-rpc--file-truename-cache))
          (tramp-rpc--hash-remove-if
           (lambda (key _value) (matches-key-p key)) cache))
        (tramp-rpc--hash-remove-if
         (lambda (key _value)
           (and (consp key) (matches-key-p (car key))))
         tramp-rpc--file-stat-cache)))))

(defun tramp-rpc--same-endpoint-vec-p (v1 v2)
  "Return non-nil when V1 and V2 share method, user, domain, host, and port."
  (and (tramp-file-name-p v1)
       (tramp-file-name-p v2)
       (equal (tramp-file-name-method v1) (tramp-file-name-method v2))
       (equal (tramp-file-name-user v1) (tramp-file-name-user v2))
       (equal (tramp-file-name-domain v1) (tramp-file-name-domain v2))
       (equal (tramp-file-name-host v1) (tramp-file-name-host v2))
       (equal (tramp-file-name-port v1) (tramp-file-name-port v2))))

(defun tramp-rpc--sync-tramp-flush-file (key file)
  "Synchronize TRAMP-RPC caches after `tramp-flush-file-properties'.
KEY is the flushed TRAMP file name vector and FILE its localname."
  (when (and (not tramp-rpc--in-cache-invalidation)
             (tramp-file-name-p key)
             (stringp file)
             (tramp-rpc--managed-file-name-p key))
    (let ((tramp-rpc--in-cache-invalidation t)
          (target (let ((tramp-default-proxies-alist tramp-cache-undefined)
                        (tramp-show-ad-hoc-proxies t))
                    (tramp-make-tramp-file-name key file))))
      (tramp-rpc--invalidate-cache-for-path target))))

(defun tramp-rpc--sync-tramp-flush-directory (key directory)
  "Synchronize TRAMP-RPC caches after `tramp-flush-directory-properties'.
KEY is the flushed TRAMP file name vector and DIRECTORY its localname."
  (when (and (not tramp-rpc--in-cache-invalidation)
             (tramp-file-name-p key)
             (stringp directory)
             (tramp-rpc--managed-file-name-p key))
    (let ((tramp-rpc--in-cache-invalidation t)
          (target (let ((tramp-default-proxies-alist tramp-cache-undefined)
                        (tramp-show-ad-hoc-proxies t))
                    (tramp-make-tramp-file-name key directory))))
      (tramp-rpc--invalidate-cache-for-subtree target))))

(defun tramp-rpc--sync-tramp-flush-connection (key)
  "Synchronize TRAMP-RPC caches after `tramp-flush-connection-properties'.
KEY is the flushed TRAMP connection key."
  (when (and (not tramp-rpc--in-cache-invalidation)
             (tramp-file-name-p key)
             (tramp-rpc--managed-file-name-p key))
    (let ((tramp-rpc--in-cache-invalidation t))
      ;; TRAMP's `tramp-flush-connection-properties' strips proxy hops when
      ;; wiping `tramp-cache-data', so reset cached connection metadata on
      ;; every live connection targeting this endpoint.  Preserve
      ;; `watcher-available' because it reflects a server capability rather
      ;; than cached remote data.
      (maphash
       (lambda (_conn-key connection)
         (when-let* ((vec (tramp-rpc-connection-vec connection))
                     ((tramp-rpc--same-endpoint-vec-p key vec)))
           (cl-incf (tramp-rpc-connection-metadata-epoch connection))
           (setf (tramp-rpc-connection-system-info connection) nil
                 (tramp-rpc-connection-environment connection) nil)))
       tramp-rpc--connections)
      (tramp-rpc--clear-file-caches-for-connection key t t))))

;; ============================================================================
;; Filesystem watching and unified watch leases
;; ============================================================================

(cl-defstruct (tramp-rpc-watch-lease
               (:constructor tramp-rpc--make-watch-lease)
               (:copier nil))
  "Server watch ownership state for one connection/path key.
Unifies explicit directory watches (`tramp-rpc-watch-directory') and
`file-notify' watches so ownership transitions, canonical path aliasing,
and server unwatch reference counting live in one place."
  directory
  canonical-directory
  notify-canonical-directory
  connection-process
  is-symlink
  explicit-p
  recursive-p
  (notify-count 0)
  notify-owned
  notify-synthetic
  notify-nofollow)

(defvar tramp-rpc--watch-leases (make-hash-table :test 'equal)
  "Hash table mapping \"conn-key:localname\" to `tramp-rpc-watch-lease'.")

(defvar tramp-rpc--expected-fs-events nil
  "Filesystem events caused by operations in progress.
Each element is (CONNECTION-KEY . REGEXP).  An event on that connection whose
paths all match REGEXP is still reported to `file-notify', but neither
invalidates caches nor runs `tramp-rpc-fs-events-functions': the operation
causing it keeps that state current itself.  Events for other paths or
connections that arrive while this is bound keep their full effect.")

(defun tramp-rpc--expected-fs-event-p (connection-key paths)
  "Return non-nil when all PATHS on CONNECTION-KEY are expected events."
  (and paths
       (cl-some (lambda (expected)
                  (and (equal (car expected) connection-key)
                       (cl-every (lambda (path)
                                   (string-match-p (cdr expected) path))
                                 paths)))
                tramp-rpc--expected-fs-events)))

(defun tramp-rpc--connection-key-string (vec)
  "Return a string key for connection VEC, suitable for hash table keys."
  (let ((key (tramp-rpc--connection-key vec))
        (print-length nil)
        (print-level nil)
        (print-circle nil))
    (format "%S" key)))

(defun tramp-rpc--watch-key (vec localname)
  "Return the watch-lease table key for VEC and LOCALNAME."
  (format "%s:%s" (tramp-rpc--connection-key-string vec) localname))

(defun tramp-rpc--canonical-directory-equal-p (a b)
  "Return non-nil if canonical directory names A and B are equal."
  (and (stringp a)
       (stringp b)
       (string= (directory-file-name a) (directory-file-name b))))

(defun tramp-rpc--watch-lease-canonical-dir (lease)
  "Return canonical TRAMP directory recorded in watch LEASE."
  (when lease
    (or (tramp-rpc-watch-lease-canonical-directory lease)
        (tramp-rpc-watch-lease-directory lease))))

(defun tramp-rpc--watch-lease-notify-directory (lease)
  "Return the canonical directory file-notify descriptors on LEASE match.
A synthetic symlink lease never falls back to an explicit watch's
followed target."
  (or (tramp-rpc-watch-lease-notify-canonical-directory lease)
      (unless (tramp-rpc-watch-lease-notify-synthetic lease)
        (tramp-rpc-watch-lease-canonical-directory lease))))

(defun tramp-rpc--canonical-watch-active-p (canonical-directory)
  "Return non-nil if CANONICAL-DIRECTORY has a target-following watch owner.
Synthetic and nofollow descriptors do not own that server registration."
  (let (active)
    (when (and (stringp canonical-directory)
               (hash-table-p tramp-rpc--watch-leases))
      (maphash
       (lambda (_key lease)
         (when (and (not active)
                    (or (tramp-rpc-watch-lease-explicit-p lease)
                        (and (> (tramp-rpc-watch-lease-notify-count lease) 0)
                             (not (tramp-rpc-watch-lease-notify-synthetic lease))
                             (not (tramp-rpc-watch-lease-notify-nofollow lease))))
                    (tramp-rpc--canonical-directory-equal-p
                     canonical-directory
                     (tramp-rpc--watch-lease-canonical-dir lease)))
           (setq active t)))
       tramp-rpc--watch-leases))
    active))

(defun tramp-rpc-watched-directory-p (directory &optional recursive)
  "Return non-nil if DIRECTORY has an active explicit watch.
When RECURSIVE is non-nil, require a recursive watch."
  (when (tramp-tramp-file-p directory)
    (with-parsed-tramp-file-name directory nil
      (when-let* ((lease (gethash (tramp-rpc--watch-key v localname)
                                  tramp-rpc--watch-leases)))
        (and (tramp-rpc-watch-lease-explicit-p lease)
             (or (not recursive)
                 (tramp-rpc-watch-lease-recursive-p lease)))))))

(defun tramp-rpc--handle-notification (process method params)
  "Handle the server notification METHOD with PARAMS received on PROCESS.
Only \"fs.events\" belongs to this module; other methods are left to the
remaining `tramp-rpc-notification-functions'."
  (when (string= method "fs.events")
    (tramp-rpc--handle-fs-events process params)))

(defun tramp-rpc--fs-event-path (vec event key)
  "Return EVENT's KEY path as a TRAMP file name on VEC, or nil."
  (when-let* ((path (tramp-rpc--decode-string (alist-get key event)))
              ((stringp path)))
    (if (tramp-tramp-file-p path)
        path
      (tramp-make-tramp-file-name vec path))))

(defun tramp-rpc--watch-canonical-directory (vec result)
  "Return the server's registered watch path from watch.add RESULT on VEC.
For target-following watches this is the canonical directory; for nofollow
symlink watches it is the symlink path with its parent canonicalized (matching
the paths emitted in server fs.events without conflating the symlink target)."
  (when-let* ((canonical-localname
               (and (listp result)
                    (tramp-rpc--decode-string (alist-get 'path result))))
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

(defun tramp-rpc--watch-alias-paths (path)
  "Return original watch spellings equivalent to canonical PATH."
  (let (aliases)
    (when (hash-table-p tramp-rpc--watch-leases)
      (maphash
       (lambda (_key lease)
         (let ((directory (tramp-rpc-watch-lease-directory lease)))
           (when directory
             (dolist (canonical-directory
                      (list (tramp-rpc-watch-lease-canonical-directory lease)
                            (tramp-rpc-watch-lease-notify-canonical-directory lease)))
               (when-let* ((canonical-directory)
                           (relative (tramp-rpc--path-under-directory-relative
                                      canonical-directory path)))
                 (let ((alias (if (string-empty-p relative)
                                  (directory-file-name directory)
                                (expand-file-name relative directory))))
                   (unless (string= alias path)
                     (cl-pushnew alias aliases :test #'string=))))))))
       tramp-rpc--watch-leases))
    aliases))

(defun tramp-rpc--invalidate-event-path (path &optional subtree)
  "Invalidate caches for PATH and equivalent original watch spellings.
When SUBTREE is non-nil, invalidate cached descendants as well."
  (dolist (candidate (cons path (tramp-rpc--watch-alias-paths path)))
    (if subtree
        (tramp-rpc--invalidate-cache-for-subtree candidate)
      (tramp-rpc--invalidate-cache-for-path candidate))))

(defun tramp-rpc--handle-fs-events (process params)
  "Handle an fs.events notification from PROCESS with PARAMS."
  (let ((events (alist-get 'events params)))
    (when events
      (tramp-rpc--debug "fs.events: %d events" (length events))
      (tramp-message process 6 "%s" events)
      (when-let* ((vec (process-get process :tramp-rpc-vec))
                  (connection (tramp-rpc--get-connection vec))
                  ((eq process (tramp-rpc-connection-process connection))))
        (let ((connection-key (tramp-rpc--connection-key-string vec))
              renamed-pairs)
          (cl-flet* ((event-paths (event)
                       (delq nil
                             (list (tramp-rpc--fs-event-path vec event 'path)
                                   (tramp-rpc--fs-event-path
                                    vec event 'path1))))
                     (expected-p (event)
                       (tramp-rpc--expected-fs-event-p
                        connection-key (event-paths event))))
            ;; The remote changed on this transport, not every remote
            ;; connection.  A rescan has no paths, so it is never expected,
            ;; and it reports unknown paths.
            (when-let* ((unexpected (cl-remove-if #'expected-p events)))
              (run-hook-with-args
               'tramp-rpc-fs-events-functions vec
               (unless (cl-some (lambda (event) (null (event-paths event)))
                                unexpected)
                 (mapcan #'event-paths unexpected))))
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
                        ;; unexpected changes no operation invalidates itself.
                        (tramp-rpc--clear-file-caches-for-connection vec)
                        ;; Public file-notify consumers still need a conservative
                        ;; event for the dropped paths.
                        (run-hook-with-args 'tramp-rpc-fs-rescan-functions process))
                    (when path
                      ;; Expected events skip only cache work; file-notify
                      ;; consumers still see every event.
                      (unless (expected-p event)
                        ;; Deleting or renaming a directory can orphan cached
                        ;; descendants, so drop subtrees for removal/rename paths.
                        (let ((subtree (member action
                                               '("deleted" "renamed" "renamed-from"))))
                          (tramp-rpc--invalidate-event-path path subtree))
                        (when path1
                          (tramp-rpc--invalidate-event-path
                           path1 (string= action "renamed"))))
                      (run-hook-with-args 'tramp-rpc-fs-event-dispatch-functions
                                          action path path1 cookie))))))))))))

(defun tramp-rpc-watch-directory (directory &optional recursive)
  "Start watching DIRECTORY for filesystem change events.
When RECURSIVE is non-nil, watch subdirectories too."
  (interactive "DDirectory to watch: ")
  (with-parsed-tramp-file-name directory nil
    (let* ((watch-key (tramp-rpc--watch-key v localname))
           (lease (gethash watch-key tramp-rpc--watch-leases))
           (upgrade-notify-owned
            (and recursive
                 lease
                 (not (tramp-rpc-watch-lease-recursive-p lease))
                 (tramp-rpc-watch-lease-notify-owned lease)
                 (not (tramp-rpc-watch-lease-notify-nofollow lease))))
           (result (tramp-rpc--call
                    v "watch.add"
                    `((path . ,(tramp-rpc--path-to-bin localname))
                      (recursive . ,(if (or recursive upgrade-notify-owned)
                                        t :msgpack-false)))))
           (canonical-directory (tramp-rpc--watch-canonical-directory v result))
           (is-symlink (and (listp result)
                            (eq t (alist-get 'is_symlink result))))
           (transport (tramp-rpc--connection-transport (tramp-rpc--get-connection v))))
      (if lease
          (progn
            (when upgrade-notify-owned
              (setf (tramp-rpc-watch-lease-notify-owned lease) nil))
            (setf (tramp-rpc-watch-lease-explicit-p lease) t
                  (tramp-rpc-watch-lease-recursive-p lease)
                  (or recursive (tramp-rpc-watch-lease-recursive-p lease))
                  (tramp-rpc-watch-lease-directory lease) directory
                  (tramp-rpc-watch-lease-canonical-directory lease)
                  (or canonical-directory
                      (tramp-rpc-watch-lease-canonical-directory lease))
                  (tramp-rpc-watch-lease-is-symlink lease)
                  (or is-symlink (tramp-rpc-watch-lease-is-symlink lease))
                  (tramp-rpc-watch-lease-connection-process lease) transport)
            ;; A follow watch shares canonical resolution with a follow-notify
            ;; lease, but never alters an independent nofollow or synthetic
            ;; symlink registration: those watch the link, not its target.
            (when (and canonical-directory
                       (not (tramp-rpc-watch-lease-notify-nofollow lease))
                       (not (tramp-rpc-watch-lease-notify-synthetic lease)))
              (setf (tramp-rpc-watch-lease-notify-canonical-directory lease)
                    canonical-directory)))
        (puthash watch-key
                 (tramp-rpc--make-watch-lease
                  :directory directory
                  :canonical-directory canonical-directory
                  :connection-process transport
                  :is-symlink is-symlink
                  :explicit-p t
                  :recursive-p (and recursive t))
                 tramp-rpc--watch-leases)))
    (tramp-rpc--debug "Watching: %s (recursive=%s)" localname recursive)))

(defun tramp-rpc-unwatch-directory (directory)
  "Stop watching DIRECTORY for filesystem change events."
  (interactive "DDirectory to unwatch: ")
  (with-parsed-tramp-file-name directory nil
    (let* ((watch-key (tramp-rpc--watch-key v localname))
           (lease (gethash watch-key tramp-rpc--watch-leases))
           (canonical-directory (tramp-rpc--watch-lease-canonical-dir lease)))
      (when lease
        (setf (tramp-rpc-watch-lease-explicit-p lease) nil
              (tramp-rpc-watch-lease-recursive-p lease) nil)
        (if (> (tramp-rpc-watch-lease-notify-count lease) 0)
            ;; If file-notify was relying on the explicit watch we just removed,
            ;; restore its direct non-recursive server watch so descriptors stay live.
            (when (and (not (tramp-rpc-watch-lease-notify-owned lease))
                       (not (tramp-rpc-watch-lease-notify-synthetic lease))
                       (not (tramp-rpc-watch-lease-notify-nofollow lease)))
              (let ((result (tramp-rpc--call
                             v "watch.add"
                             `((path . ,(tramp-rpc--path-to-bin localname))
                               (recursive . :msgpack-false)))))
                (when-let* ((restored (tramp-rpc--watch-canonical-directory v result)))
                  (setq canonical-directory restored)
                  (setf (tramp-rpc-watch-lease-canonical-directory lease)
                        restored
                        (tramp-rpc-watch-lease-notify-canonical-directory lease)
                        restored)))
              (setf (tramp-rpc-watch-lease-notify-owned lease) t))
          (remhash watch-key tramp-rpc--watch-leases)))
      (unless (tramp-rpc--canonical-watch-active-p canonical-directory)
        (tramp-rpc--call
         v "watch.remove"
         `((path . ,(tramp-rpc--path-to-bin
                     (if (stringp canonical-directory)
                         (tramp-file-local-name canonical-directory)
                       localname)))
           (nofollow . :msgpack-false)))))
    (tramp-rpc--debug "Unwatched: %s" localname)))

(defun tramp-rpc--watch-acquire-notify (vec directory localname)
  "Acquire or refcount a `file-notify' watch lease for DIRECTORY on VEC.
Uses `watch.add' with `nofollow_if_symlink' so both regular directories
and symlink watches complete in a single RPC round trip (0 RTTs when
piggybacking on an existing lease).  Returns the `tramp-rpc-watch-lease'."
  (let* ((watch-key (tramp-rpc--watch-key vec localname))
         (lease (gethash watch-key tramp-rpc--watch-leases)))
    (cond
     ;; Already tracked by file-notify: just increment refcount (0 RTT).
     ((and lease (> (tramp-rpc-watch-lease-notify-count lease) 0))
      (cl-incf (tramp-rpc-watch-lease-notify-count lease))
      lease)
     ;; Preexisting explicit watch on the same key: piggyback without
     ;; disturbing its recursive flag (0 RTT).
     (lease
      (setf (tramp-rpc-watch-lease-notify-count lease) 1
            (tramp-rpc-watch-lease-notify-owned lease) nil
            (tramp-rpc-watch-lease-notify-synthetic lease)
            (and (tramp-rpc-watch-lease-is-symlink lease) t)
            (tramp-rpc-watch-lease-notify-nofollow lease) nil
            ;; A synthetic symlink descriptor must not match the target that
            ;; the explicit follow watch resolved.
            (tramp-rpc-watch-lease-notify-canonical-directory lease)
            (unless (tramp-rpc-watch-lease-is-symlink lease)
              (tramp-rpc-watch-lease-canonical-directory lease)))
      lease)
     ;; Fresh watch: 1 RTT with server-side symlink detection & nofollow.
     (t
      (let ((synthetic nil)
            (result nil))
        (condition-case err
            (setq result
                  (tramp-rpc--call
                   vec "watch.add"
                   `((path . ,(tramp-rpc--path-to-bin localname))
                     (recursive . :msgpack-false)
                     (nofollow_if_symlink . t))))
          (error
           ;; On platforms without nofollow watch support (e.g. macOS),
           ;; fall back to a synthetic client-side descriptor for symlinks.
           (if (condition-case nil
                   (file-symlink-p directory)
                 (error nil))
               (progn
                 (setq synthetic t)
                 (tramp-rpc--debug
                  "nofollow file-notify watch unsupported for %s: %s"
                  directory (error-message-string err)))
             (signal (car err) (cdr err)))))
        (let* ((canonical-directory
                (tramp-rpc--watch-canonical-directory vec result))
               (is-symlink (or synthetic
                               (and (listp result)
                                    (eq t (alist-get 'is_symlink result)))))
               (nofollow (and (not synthetic)
                              (listp result)
                              (eq t (alist-get 'nofollow result))))
               (transport (tramp-rpc--connection-transport
                           (tramp-rpc--get-connection vec)))
               (new-lease
                (tramp-rpc--make-watch-lease
                 :directory directory
                 :canonical-directory (unless nofollow canonical-directory)
                 :notify-canonical-directory canonical-directory
                 :connection-process transport
                 :is-symlink is-symlink
                 :explicit-p nil
                 :recursive-p nil
                 :notify-count 1
                 :notify-owned (not synthetic)
                 :notify-synthetic synthetic
                 :notify-nofollow nofollow)))
          (puthash watch-key new-lease tramp-rpc--watch-leases)
          new-lease))))))

(defun tramp-rpc--watch-release-notify (watch-key)
  "Release one `file-notify' reference on WATCH-KEY."
  (when-let* ((lease (gethash watch-key tramp-rpc--watch-leases)))
    (let ((count (tramp-rpc-watch-lease-notify-count lease)))
      (cond
       ((> count 1)
        (setf (tramp-rpc-watch-lease-notify-count lease) (1- count)))
       ((> count 0)
        (let ((owned (tramp-rpc-watch-lease-notify-owned lease))
              (nofollow (tramp-rpc-watch-lease-notify-nofollow lease))
              (directory (tramp-rpc-watch-lease-directory lease))
              (canonical-directory
               (if (tramp-rpc-watch-lease-notify-nofollow lease)
                   (or (tramp-rpc-watch-lease-notify-canonical-directory lease)
                       (tramp-rpc-watch-lease-directory lease))
                 (or (tramp-rpc-watch-lease-notify-canonical-directory lease)
                     (tramp-rpc--watch-lease-canonical-dir lease)))))
          (setf (tramp-rpc-watch-lease-notify-count lease) 0
                (tramp-rpc-watch-lease-notify-owned lease) nil
                (tramp-rpc-watch-lease-notify-synthetic lease) nil
                (tramp-rpc-watch-lease-notify-nofollow lease) nil
                (tramp-rpc-watch-lease-notify-canonical-directory lease) nil)
          (unless (tramp-rpc-watch-lease-explicit-p lease)
            (remhash watch-key tramp-rpc--watch-leases))
          (when (and owned
                     ;; Nofollow registrations are refcounted on the server;
                     ;; every spelling that acquired one must release it.
                     ;; Follow watches are shared, so retain them for other owners.
                     (or nofollow
                         (not (tramp-rpc--canonical-watch-active-p
                               canonical-directory))))
            ;; Removing a file notification must not fail if the remote
            ;; connection has already gone away.
            (condition-case err
                (with-parsed-tramp-file-name directory nil
                  (tramp-rpc--call
                   v "watch.remove"
                   `((path . ,(tramp-rpc--path-to-bin
                               (if (stringp canonical-directory)
                                   (tramp-file-local-name canonical-directory)
                                 localname)))
                     (nofollow . ,(if nofollow t :msgpack-false)))))
              (error
               (tramp-rpc--debug "failed to remove file-notify watch %s: %s"
                                 directory (error-message-string err)))))))))))

(defun tramp-rpc--cleanup-watches-for-connection (vec &optional connection-process)
  "Remove watch leases for VEC's CONNECTION-PROCESS.
When VEC is nil, remove all matching leases."
  (let* ((prefix (and vec (concat (tramp-rpc--connection-key-string vec) ":")))
         (removed
          (tramp-rpc--hash-remove-if
           (lambda (key lease)
             (and (or (null prefix) (string-prefix-p prefix key))
                  (or (null connection-process)
                      (null (tramp-rpc-watch-lease-connection-process lease))
                      (eq connection-process
                          (tramp-rpc-watch-lease-connection-process lease)))))
           tramp-rpc--watch-leases)))
    (when (> removed 0)
      (tramp-rpc--debug "Cleaned up %d watch leases%s"
                        removed (if prefix (format " for %s" prefix) "")))))

(defun tramp-rpc-list-watches ()
  "List currently watched directories."
  (interactive)
  (let ((watches nil))
    (maphash (lambda (key lease)
               (when (tramp-rpc-watch-lease-explicit-p lease)
                 (push key watches)))
             tramp-rpc--watch-leases)
    (if watches
        (message "Watched directories:\n%s"
                 (mapconcat #'identity watches "\n"))
      (message "No directories being watched."))))

;; Keep the caches and watches consistent with transport generations, and
;; receive server notifications.  These internal callbacks are required when
;; this module is loaded standalone; they do not enable editor integrations.
(add-hook 'tramp-rpc-connection-invalidate-functions
          #'tramp-rpc--clear-file-caches-for-connection t)
(add-hook 'tramp-rpc-transport-cleanup-functions
          #'tramp-rpc--cleanup-watches-for-connection t)
(add-hook 'tramp-rpc-notification-functions
          #'tramp-rpc--handle-notification t)

(provide 'tramp-rpc-cache)
;;; tramp-rpc-cache.el ends here
