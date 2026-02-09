;;; tramp-rpc-magit.el --- Caching and watch support for TRAMP-RPC -*- lexical-binding: t; -*-

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

;; This file provides caching infrastructure and filesystem watching for
;; tramp-rpc.  It includes:
;; - TTL-based file-exists and file-truename caches
;; - Cache invalidation via server push notifications (fs.changed)
;; - Watch management (add/remove/list watched directories)
;; - Notification dispatch for filesystem change events

;;; Code:

(require 'cl-lib)
(require 'tramp)

;; Functions from tramp-rpc.el
(declare-function tramp-rpc--debug "tramp-rpc")
(declare-function tramp-rpc--call "tramp-rpc")
(declare-function tramp-rpc--connection-key "tramp-rpc")
(declare-function tramp-rpc-file-name-p "tramp-rpc")

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
  (let ((expanded (expand-file-name filename)))
    (remhash expanded tramp-rpc--file-exists-cache)
    (remhash expanded tramp-rpc--file-truename-cache)
    ;; Also invalidate parent directory
    (let ((dir (file-name-directory expanded)))
      (when dir
        (remhash dir tramp-rpc--file-exists-cache)
        (remhash dir tramp-rpc--file-truename-cache)))))

(defun tramp-rpc-clear-file-exists-cache ()
  "Clear the file-exists-p cache."
  (interactive)
  (clrhash tramp-rpc--file-exists-cache))

(defun tramp-rpc-clear-file-truename-cache ()
  "Clear the file-truename cache."
  (interactive)
  (clrhash tramp-rpc--file-truename-cache))

(defun tramp-rpc-clear-all-caches ()
  "Clear all tramp-rpc caches."
  (interactive)
  (tramp-rpc-clear-file-exists-cache)
  (tramp-rpc-clear-file-truename-cache))

;; ============================================================================
;; Filesystem watching
;; ============================================================================

(defvar tramp-rpc--watched-directories (make-hash-table :test 'equal)
  "Hash table of watched directories.
Keys are \"conn-key:path\" strings, values are t.")

(defvar tramp-rpc--suppress-fs-notifications nil
  "When non-nil, suppress handling of fs.changed notifications.
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

(defun tramp-rpc--handle-notification (process method params)
  "Handle a server-initiated notification.
PROCESS is the connection, METHOD is the notification method,
PARAMS is the notification parameters."
  (cond
   ((string= method "fs.changed")
    (unless tramp-rpc--suppress-fs-notifications
      (tramp-rpc--handle-fs-changed process params)))
   (t
    (tramp-rpc--debug "Unknown notification: %s" method))))

(defun tramp-rpc--handle-fs-changed (process params)
  "Handle an fs.changed notification from PROCESS with PARAMS.
Invalidates caches for the changed paths."
  (let ((paths (alist-get 'paths params)))
    (when paths
      (tramp-rpc--debug "fs.changed: %d paths changed" (length paths))
      ;; Invalidate caches for each changed path
      (dolist (path paths)
        (when (stringp path)
          ;; Build the full tramp path for cache invalidation
          ;; Get the vec from the process
          (when-let* ((vec (process-get process :tramp-rpc-vec)))
            (let ((full-path (tramp-make-tramp-file-name vec path)))
              (tramp-rpc--invalidate-cache-for-path full-path))))))))

(defun tramp-rpc-watch-directory (directory &optional recursive)
  "Start watching DIRECTORY for filesystem changes.
When RECURSIVE is non-nil, watch subdirectories too."
  (interactive "DDirectory to watch: ")
  (with-parsed-tramp-file-name directory nil
    (tramp-rpc--call v "watch.add"
                     `((path . ,localname)
                       (recursive . ,(if recursive t :json-false))))
    (let ((conn-key (tramp-rpc--connection-key-string v)))
      (puthash (format "%s:%s" conn-key localname) t
               tramp-rpc--watched-directories))
    (tramp-rpc--debug "Watching: %s (recursive=%s)" localname recursive)))

(defun tramp-rpc-unwatch-directory (directory)
  "Stop watching DIRECTORY for filesystem changes."
  (interactive "DDirectory to unwatch: ")
  (with-parsed-tramp-file-name directory nil
    (tramp-rpc--call v "watch.remove" `((path . ,localname)))
    (let ((conn-key (tramp-rpc--connection-key-string v)))
      (remhash (format "%s:%s" conn-key localname)
               tramp-rpc--watched-directories))
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

(provide 'tramp-rpc-magit)
;;; tramp-rpc-magit.el ends here
