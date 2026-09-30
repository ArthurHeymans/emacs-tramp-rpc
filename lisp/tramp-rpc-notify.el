;;; tramp-rpc-notify.el --- File notifications for TRAMP-RPC -*- lexical-binding: t; -*-

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

;; This file implements the `file-notify-add-watch', `file-notify-rm-watch'
;; and `file-notify-valid-p' handlers for TRAMP-RPC.  Descriptors are backed
;; by the server watches managed in tramp-rpc-cache.el, and the server's
;; fs.events notifications are dispatched to them from there.

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 'tramp-rpc-protocol)
(require 'tramp-rpc-connection)
(require 'tramp-rpc-transport)
(require 'tramp-rpc-cache)

(defvar tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq)
  "TRAMP-RPC file notification descriptors.")

(defvar tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal)
  "Reference counts for directories watched via file notifications.
Keys are the same connection/path keys as `tramp-rpc--watched-directories'.")

(defvar tramp-rpc-protocol--message-target)

(declare-function file-notify--rm-descriptor "filenotify")
(declare-function file-notify-rm-watch "filenotify")

(defun tramp-rpc--file-notify-monitor (vec)
  "Return the file notification monitor symbol for VEC."
  (let* ((info (condition-case err
                   (tramp-rpc--system-info vec)
                 (error
                  (tramp-rpc--debug "system.info probe failed: %s"
                                    (error-message-string err))
                  nil)))
         (watcher (and (listp info)
                       (tramp-rpc--decode-string (alist-get 'watcher info))))
         (os (and (listp info)
                  (tramp-rpc--decode-string (alist-get 'os info)))))
    (pcase watcher
      ("inotify" 'TrampRPCinotify)
      ("kqueue" 'TrampRPCkqueue)
      ("fsevent" 'TrampRPCfsevent)
      ("poll" 'TrampRPCpoll)
      ((pred null)
       (pcase os
         ("linux" 'TrampRPCinotify)
         ((or "freebsd" "openbsd" "netbsd" "dragonfly" "ios")
          'TrampRPCkqueue)
         ("macos" 'TrampRPCfsevent)
         (_ 'TrampRPC)))
      (_ 'TrampRPC))))

(defun tramp-rpc--file-notify-process-sentinel (descriptor event)
  "Clean up file notification DESCRIPTOR after EVENT closes it."
  (unless (process-live-p descriptor)
    (tramp-rpc--debug "file-notify descriptor closed: %S %s" descriptor event)
    ;; `file-notify-rm-watch' calls the file-name handler, which deletes the
    ;; descriptor process.  Avoid re-entering it for that intentional close.
    (unless (process-get descriptor 'tramp-rpc-file-notify-removing)
      (file-notify-rm-watch descriptor))))

(defun tramp-rpc--make-file-notify-descriptor (vec _directory localname)
  "Create a TRAMP-style process descriptor for a file notification watch.
VEC is the TRAMP connection vector.
LOCALNAME is the local file name."
  (let* (;; Emacs' remote file notification tests use the process name to
         ;; identify the remote library.  The concrete backend is exposed via
         ;; the "file-monitor" connection property below.
         (name "tramp-rpc")
         ;; This synthetic process is only a watch descriptor; it never
         ;; receives output.  Do not attach a buffer: `global-auto-revert-mode'
         ;; iterates over `buffer-list' while removing file notification
         ;; watches, and deleting descriptor buffers during that iteration can
         ;; make Emacs select a just-deleted buffer.
         (descriptor (make-pipe-process
                      :name name
                      :noquery t
                      :sentinel #'tramp-rpc--file-notify-process-sentinel)))
    ;; These two properties are the ones TRAMP's generic file-notify routing and
    ;; validity helpers expect on watch descriptors.
    (process-put descriptor 'tramp-vector vec)
    (process-put descriptor 'tramp-watch-name localname)
    ;; Match gio/smb-notify: tests can use the library name for broad behavior
    ;; and this property for backend-specific expectations.
    (tramp-set-connection-property
     descriptor "file-monitor" (tramp-rpc--file-notify-monitor vec))
    ;; RPC-private metadata lives in `tramp-rpc--file-notify-descriptors', but
    ;; mark the process as ours for debugging and defensive cleanup.
    (process-put descriptor 'tramp-rpc-file-notify t)
    descriptor))

(defun tramp-rpc--delete-file-notify-descriptor-process (descriptor)
  "Delete DESCRIPTOR's synthetic process."
  (when (processp descriptor)
    (process-put descriptor 'tramp-rpc-file-notify-removing t)
    (when (process-live-p descriptor)
      (delete-process descriptor))))

(defun tramp-rpc--canonical-directory-equal-p (a b)
  "Return non-nil if canonical directory names A and B are equal."
  (and (stringp a)
       (stringp b)
       (string= (directory-file-name a) (directory-file-name b))))

(defun tramp-rpc--watch-entry-canonical-directory (entry)
  "Return canonical directory recorded in watch ENTRY."
  (or (plist-get entry :canonical-directory)
      (plist-get entry :directory)))

(defun tramp-rpc--canonical-watch-active-p (canonical-directory)
  "Return non-nil if CANONICAL-DIRECTORY has a target-following owner.
Synthetic and nofollow descriptors do not own that server registration."
  (let (active)
    (when (and (stringp canonical-directory)
               (hash-table-p tramp-rpc--watched-directories))
      (maphash
       (lambda (_key entry)
         (when (tramp-rpc--canonical-directory-equal-p
                canonical-directory
                (tramp-rpc--watch-entry-canonical-directory entry))
           (setq active t)))
       tramp-rpc--watched-directories))
    (when (and (not active)
               (stringp canonical-directory)
               (hash-table-p tramp-rpc--file-notify-watch-counts))
      (maphash
       (lambda (_key entry)
         (when (and (plist-get entry :count)
                    (not (plist-get entry :synthetic))
                    (not (plist-get entry :nofollow))
                    (tramp-rpc--canonical-directory-equal-p
                     canonical-directory
                     (tramp-rpc--watch-entry-canonical-directory entry)))
           (setq active t)))
       tramp-rpc--file-notify-watch-counts))
    active))

(defun tramp-rpc--cleanup-file-notify-for-connection
    (&optional vec connection-process)
  "Remove file notification state for VEC's CONNECTION-PROCESS.
When VEC is nil, remove all state."
  (let* ((prefix (and vec (concat (tramp-rpc--connection-key-string vec) ":")))
         (descriptors-to-remove nil)
         (watch-keys-to-remove nil))
    (maphash
     (lambda (descriptor data)
       (let* ((watch-key (plist-get data :watch-key))
              (entry (and watch-key
                          (gethash watch-key tramp-rpc--file-notify-watch-counts)))
              (owner (or (plist-get data :connection-process)
                         (plist-get entry :connection-process))))
         (when (and (or (null prefix)
                        (and watch-key (string-prefix-p prefix watch-key)))
                    (or (null connection-process)
                        (null owner)
                        (eq connection-process owner)))
           (push descriptor descriptors-to-remove))))
     tramp-rpc--file-notify-descriptors)
    (maphash
     (lambda (watch-key data)
       (when (and (or (null prefix) (string-prefix-p prefix watch-key))
                  (or (null connection-process)
                      (null (plist-get data :connection-process))
                      (eq connection-process
                          (plist-get data :connection-process))))
         (push watch-key watch-keys-to-remove)))
     tramp-rpc--file-notify-watch-counts)
    (dolist (descriptor descriptors-to-remove)
      ;; Remove private state before sending the public `stopped' event, so
      ;; callbacks observing `file-notify-valid-p' during cleanup see the
      ;; descriptor as no longer valid.
      (remhash descriptor tramp-rpc--file-notify-descriptors)
      (tramp-rpc--delete-file-notify-descriptor-process descriptor)
      (when (and (boundp 'file-notify-descriptors)
                 (gethash descriptor file-notify-descriptors))
        (require 'filenotify)
        (file-notify--rm-descriptor descriptor)))
    (dolist (watch-key watch-keys-to-remove)
      (remhash watch-key tramp-rpc--file-notify-watch-counts))
    (when (or descriptors-to-remove watch-keys-to-remove)
      (tramp-rpc--debug
       "Cleaned up %d file-notify descriptors and %d file-notify watches%s"
       (length descriptors-to-remove)
       (length watch-keys-to-remove)
       (if vec (format " for %s" prefix) "")))))

(defun tramp-rpc--file-notify-relative-name (directory file)
  "Return FILE's relative name under DIRECTORY, or nil.
Only DIRECTORY itself and immediate children match.  DIRECTORY itself returns
the empty string."
  (let* ((dir (file-name-as-directory (directory-file-name directory)))
         (file (directory-file-name file)))
    (cond
     ((string= (directory-file-name dir) file) "")
     ((string-prefix-p dir file)
      (let ((rest (substring file (length dir))))
        (and (not (string-empty-p rest))
             (not (string-match-p "/" rest))
             rest))))))

(defun tramp-rpc--file-notify-direct-child-p (directory file)
  "Return non-nil if FILE is DIRECTORY or its immediate child."
  (and (tramp-rpc--file-notify-relative-name directory file) t))

(defun tramp-rpc--file-notify-action-enabled-p (action flags)
  "Return non-nil when ACTION is enabled by file notification FLAGS."
  (or (member action '("stopped"))
      (and (memq 'change flags)
           (member action '("created" "changed" "deleted"
                            "renamed" "renamed-from" "renamed-to")))
      (and (memq 'attribute-change flags)
           (string= action "attribute-changed"))))

(defun tramp-rpc--file-notify-callback-action (action)
  "Map protocol ACTION to an action accepted by `file-notify-callback'."
  (pcase action
    ("created" 'created)
    ("changed" 'changed)
    ("attribute-changed" 'attribute-changed)
    ("deleted" 'deleted)
    ("renamed" 'moved)
    ("renamed-from" 'moved-from)
    ("renamed-to" 'moved-to)
    ("stopped" 'unmounted)
    (_ nil)))

(defun tramp-rpc--file-notify-alias-paths (file-name)
  "Return original watch spellings equivalent to canonical FILE-NAME."
  (let (aliases)
    (when (hash-table-p tramp-rpc--file-notify-descriptors)
      (maphash
       (lambda (_descriptor data)
         (when-let* ((canonical-directory
                      (tramp-rpc--file-notify-canonical-directory data))
                     ((tramp-rpc--file-notify-relative-name
                       canonical-directory file-name))
                     (alias
                      (tramp-rpc--file-notify-original-spelling
                       data file-name))
                     ((not (string= alias file-name))))
           (cl-pushnew alias aliases :test #'string=)))
       tramp-rpc--file-notify-descriptors))
    aliases))

(defun tramp-rpc--file-notify-canonical-directory (data)
  "Return the canonical directory associated with descriptor DATA."
  (let* ((watch-key (plist-get data :watch-key))
         (watch-entry (and watch-key
                           (gethash watch-key
                                    tramp-rpc--file-notify-watch-counts))))
    ;; Prefer the shared watch entry, because explicit unwatch can restore a
    ;; file-notify-owned direct watch and learn a newer canonical path after
    ;; descriptors were created.
    (or (plist-get watch-entry :canonical-directory)
        (plist-get data :canonical-directory))))

(defun tramp-rpc--file-notify-original-spelling (data file-name)
  "Return FILE-NAME rewritten to descriptor DATA's original watch spelling."
  (let* ((canonical-directory
          (tramp-rpc--file-notify-canonical-directory data))
         (directory (plist-get data :directory))
         (relative (and canonical-directory directory
                        (tramp-rpc--file-notify-relative-name
                         canonical-directory file-name))))
    (if relative
        (if (string-empty-p relative)
            (directory-file-name directory)
          (expand-file-name relative directory))
      file-name)))

(defun tramp-rpc--file-notify-callback-name (data file-name)
  "Return FILE-NAME in the form expected by `file-notify-callback'.
`file-notify-callback' expands backend file names relative to the
watch directory stored in `file-notify-descriptors'.  Passing an
already expanded TRAMP name can therefore produce doubled remote
prefixes on some TRAMP versions.  Prefer the name relative to the
original watched directory, falling back to the original spelling when
we cannot derive one.
DATA is the payload to send."
  (let* ((display-file-name
          (tramp-rpc--file-notify-original-spelling data file-name))
         (directory (plist-get data :directory))
         (relative (and directory
                        (tramp-rpc--file-notify-relative-name
                         directory display-file-name))))
    (cond
     ((null relative) display-file-name)
     ((string-empty-p relative) ".")
     (t relative))))

(defun tramp-rpc--file-notify-path-matches-p (data file-name)
  "Return non-nil if descriptor DATA covers FILE-NAME."
  (let ((canonical-directory
         (tramp-rpc--file-notify-canonical-directory data)))
    (or (tramp-rpc--file-notify-direct-child-p
         (plist-get data :directory) file-name)
        ;; The server registers canonical watch paths and can report events
        ;; using that canonical spelling.  Keep the original directory for
        ;; Emacs' public descriptor table, but also match against the
        ;; canonical directory returned by `watch.add' when it differs (for
        ;; example, symlinked watched directories).
        (and canonical-directory
             (tramp-rpc--file-notify-direct-child-p
              canonical-directory file-name)))))

(defun tramp-rpc--file-notify-synthetic-watch-p (file-name)
  "Return non-nil if FILE-NAME is covered by a synthetic symlink watch."
  (let (matched)
    (when (hash-table-p tramp-rpc--file-notify-descriptors)
      (maphash
       (lambda (_descriptor data)
         (let* ((watch-key (plist-get data :watch-key))
                (entry (and watch-key
                            (gethash watch-key
                                     tramp-rpc--file-notify-watch-counts))))
           (when (and (plist-get entry :synthetic)
                      (tramp-rpc--file-notify-path-matches-p data file-name))
             (setq matched t))))
       tramp-rpc--file-notify-descriptors))
    matched))

(defun tramp-rpc--file-notify-dispatch-descriptor
    (descriptor data action file-name &optional file-name1 cookie)
  "Dispatch ACTION for one selected DESCRIPTOR using its watch DATA.
FILE-NAME1 is the destination for rename events.  COOKIE pairs tracked renames."
  (when-let* ((callback-action (tramp-rpc--file-notify-callback-action action)))
    ;; `file-notify-callback' and the special-event handler live in
    ;; filenotify.el.  It is normally loaded before watches are registered.
    (require 'filenotify)
    (let* ((display-file-name
            (tramp-rpc--file-notify-callback-name data file-name))
           (display-file-name1
            (and file-name1
                 (tramp-rpc--file-notify-callback-name data file-name1)))
           (event-data (append (list descriptor (list callback-action)
                                     display-file-name)
                               (cond
                                (display-file-name1 (list display-file-name1))
                                (cookie (list cookie)))))
           (event `(file-notify ,event-data file-notify-callback)))
      (if (fboundp 'insert-special-event)
          (insert-special-event event)
        (funcall (lookup-key special-event-map [file-notify]) event)))))

(defun tramp-rpc--file-notify-dispatch-rescan (connection-process)
  "Dispatch conservative events for live watches on CONNECTION-PROCESS."
  (let (dispatches)
    ;; Select concrete descriptors before dispatch.  Feeding their directories
    ;; through the path router would also reach dead or replacement-generation
    ;; descriptors that happen to watch the same spelling.
    (maphash
     (lambda (descriptor data)
       (when (and (process-live-p descriptor)
                  (eq connection-process
                      (plist-get data :connection-process)))
         (let ((directory (plist-get data :directory))
               (flags (plist-get data :flags)))
           (when (memq 'change flags)
             (push (list descriptor data "changed" directory) dispatches))
           (when (memq 'attribute-change flags)
             (push (list descriptor data "attribute-changed" directory)
                   dispatches)))))
     tramp-rpc--file-notify-descriptors)
    (dolist (dispatch dispatches)
      (apply #'tramp-rpc--file-notify-dispatch-descriptor dispatch))))

(defun tramp-rpc--file-notify-dispatch (action file-name &optional file-name1 cookie)
  "Dispatch a `file-notify' ACTION for TRAMP FILE-NAME.
FILE-NAME1 is the destination for `renamed' events.  COOKIE pairs
`renamed-from' and `renamed-to' events when the server provides one."
  (when (and (hash-table-p tramp-rpc--file-notify-descriptors)
             (> (hash-table-count tramp-rpc--file-notify-descriptors) 0)
             (tramp-rpc--file-notify-callback-action action))
    (let (descriptors)
      (maphash
       (lambda (descriptor data)
         (when (and (tramp-rpc--file-notify-action-enabled-p
                     action (plist-get data :flags))
                    (or (tramp-rpc--file-notify-path-matches-p data file-name)
                        (and file-name1
                             (tramp-rpc--file-notify-path-matches-p
                              data file-name1))))
           (push (cons descriptor data) descriptors)))
       tramp-rpc--file-notify-descriptors)
      (dolist (descriptor-data descriptors)
        (tramp-rpc--file-notify-dispatch-descriptor
         (car descriptor-data) (cdr descriptor-data)
         action file-name file-name1 cookie)))))

(defun tramp-rpc-handle-file-notify-add-watch (directory flags _callback)
  "Like `file-notify-add-watch' for TRAMP-RPC files.
DIRECTORY is the remote directory passed by `file-notify-add-watch'.
FLAGS controls the requested operation."
  ;; `file-notify-add-watch' validates FLAGS and CALLBACK before invoking file
  ;; name handlers, and stores the callback in `file-notify-descriptors' after
  ;; this handler returns.  We only need to create a distinct descriptor and
  ;; ensure the corresponding remote directory is watched.
  (require 'filenotify)
  (with-parsed-tramp-file-name directory nil
    (let* ((watch-key (format "%s:%s" (tramp-rpc--connection-key-string v)
                              localname))
           (entry (gethash watch-key tramp-rpc--file-notify-watch-counts))
           (preexisting (gethash watch-key tramp-rpc--watched-directories))
           ;; file-notify does not follow symlinks.  Ask the server for a
           ;; nofollow symlink watch when needed, falling back to a synthetic
           ;; client-side descriptor on platforms without nofollow support.
           (symlink-watch
            (condition-case err
                (file-symlink-p directory)
              (error
               (tramp-rpc--debug "symlink watch probe failed for %s: %s"
                                 directory (error-message-string err))
               nil)))
           (descriptor (tramp-rpc--make-file-notify-descriptor
                        v directory localname)))
      (if entry
          (plist-put entry :count (1+ (plist-get entry :count)))
        ;; Keep file-notify's non-recursive watches out of
        ;; `tramp-rpc--watched-directories'.  That table is also used by Magit
        ;; and cache invalidation, where a truthy entry means a recursive
        ;; worktree/cache watch may already exist.
        (let* ((synthetic nil)
               (result (cond
                        (symlink-watch
                         (if (or preexisting
                                 ;; Older servers remove both the symlink and
                                 ;; target registrations.  Do not acquire a
                                 ;; nofollow watch we cannot release safely.
                                 (not (eq t (alist-get 'watch_remove_nofollow
                                                      (tramp-rpc--system-info v)))))
                             (progn
                               (setq synthetic symlink-watch)
                               nil)
                           (condition-case err
                               (tramp-rpc--call
                                v "watch.add"
                                `((path . ,localname)
                                  (recursive . :msgpack-false)
                                  (nofollow . t)))
                             (error
                              (setq synthetic symlink-watch)
                              (tramp-rpc--debug
                               "nofollow file-notify watch unsupported for %s: %s"
                               directory (error-message-string err))
                              nil))))
                        (preexisting nil)
                        (t
                         (tramp-rpc--call v "watch.add"
                                          `((path . ,localname)
                                            (recursive . :msgpack-false))))))
               (canonical-localname (and (listp result)
                                         (alist-get 'path result)))
               (canonical-directory (cond
                                     ((and (stringp canonical-localname)
                                           (tramp-tramp-file-p canonical-localname))
                                      canonical-localname)
                                     ((stringp canonical-localname)
                                      (tramp-make-tramp-file-name
                                       v canonical-localname))
                                     ;; If the server watch preexisted, there
                                     ;; is no `watch.add' response to learn its
                                     ;; canonical spelling from.  Use TRAMP's
                                     ;; truename path as a best-effort match key
                                     ;; for symlinked watched directories.
                                     (preexisting
                                      (condition-case err
                                          (file-truename directory)
                                        (error
                                         (tramp-rpc--debug
                                          "watch truename probe failed for %s: %s"
                                          directory
                                          (error-message-string err))
                                         nil))))))
          (puthash watch-key
                   (list :count 1
                         :owned (and (not preexisting) (not synthetic))
                         :synthetic synthetic
                         :nofollow (and symlink-watch (not synthetic) t)
                         :directory directory
                         :canonical-directory canonical-directory
                         :connection-process (tramp-rpc--connection-transport (tramp-rpc--get-connection v)))
                   tramp-rpc--file-notify-watch-counts)))
      (let ((watch-entry (gethash watch-key tramp-rpc--file-notify-watch-counts)))
        (puthash descriptor
                 (list :directory directory
                       :canonical-directory (plist-get watch-entry
                                                       :canonical-directory)
                       :flags flags
                       :localname localname
                       :watch-key watch-key
                       :connection-process (tramp-rpc--connection-transport (tramp-rpc--get-connection v)))
                 tramp-rpc--file-notify-descriptors))
      descriptor)))

(defun tramp-rpc-handle-file-notify-rm-watch (descriptor)
  "Like `file-notify-rm-watch' for TRAMP-RPC watch DESCRIPTOR."
  (when-let* ((data (gethash descriptor tramp-rpc--file-notify-descriptors)))
    (let* ((watch-key (plist-get data :watch-key))
           (entry (gethash watch-key tramp-rpc--file-notify-watch-counts))
           (canonical-directory
            (tramp-rpc--watch-entry-canonical-directory entry))
           (count (and entry (plist-get entry :count))))
      (cond
       ((and count (> count 1))
        (plist-put entry :count (1- count)))
       (entry
        (remhash watch-key tramp-rpc--file-notify-watch-counts)
        (when (and (plist-get entry :owned)
                   ;; Nofollow registrations are refcounted on the server;
                   ;; every spelling that acquired one must release it.  Follow
                   ;; watches are shared, so retain them for other owners.
                   (or (plist-get entry :nofollow)
                       (not (tramp-rpc--canonical-watch-active-p
                             canonical-directory))))
          ;; Removing a file notification should not make
          ;; `file-notify-rm-watch' fail if the remote connection has already
          ;; gone away.
          (condition-case err
              (with-parsed-tramp-file-name (plist-get entry :directory) nil
                (tramp-rpc--call
                 v "watch.remove"
                 `((path . ,(if (stringp canonical-directory)
                                (tramp-file-local-name canonical-directory)
                              localname))
                   (nofollow . ,(if (plist-get entry :nofollow)
                                    t :msgpack-false)))))
            (error
             (tramp-rpc--debug "failed to remove file-notify watch %s: %s"
                               (plist-get entry :directory)
                               (error-message-string err))))))))
    (remhash descriptor tramp-rpc--file-notify-descriptors)
    (tramp-rpc--delete-file-notify-descriptor-process descriptor)))

(defun tramp-rpc-handle-file-notify-valid-p (descriptor)
  "Like `file-notify-valid-p' for TRAMP-RPC watch DESCRIPTOR."
  (and (processp descriptor)
       (process-live-p descriptor)
       (gethash descriptor tramp-rpc--file-notify-descriptors)
       t))

;; Internal transport teardown must always release file-notify watches.
;; This is backend lifecycle wiring, not an opt-in editor integration.
(add-hook 'tramp-rpc-transport-cleanup-functions
          #'tramp-rpc--cleanup-file-notify-for-connection t)

(provide 'tramp-rpc-notify)
;;; tramp-rpc-notify.el ends here
