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
;; by the unified watch leases managed in tramp-rpc-cache.el, and server
;; fs.events notifications are received via `tramp-rpc-fs-event-dispatch-functions'
;; and `tramp-rpc-fs-rescan-functions'.

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 'tramp-rpc-protocol)
(require 'tramp-rpc-connection)
(require 'tramp-rpc-transport)
(require 'tramp-rpc-cache)

(defvar tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq)
  "TRAMP-RPC file notification descriptors.")

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
                      :sentinel #'tramp-rpc--file-notify-process-sentinel))
         initialized)
    (unwind-protect
        (progn
          ;; These properties are used by TRAMP's generic file-notify routing
          ;; and validity helpers.
          (process-put descriptor 'tramp-vector vec)
          (process-put descriptor 'tramp-watch-name localname)
          ;; Match gio/smb-notify: expose the concrete remote backend.
          (tramp-set-connection-property
           descriptor "file-monitor" (tramp-rpc--file-notify-monitor vec))
          ;; RPC-private metadata lives in the descriptor table.
          (process-put descriptor 'tramp-rpc-file-notify t)
          (setq initialized t)
          descriptor)
      (unless initialized
        (tramp-rpc--delete-file-notify-descriptor-process descriptor)))))

(defun tramp-rpc--delete-file-notify-descriptor-process (descriptor)
  "Delete DESCRIPTOR's synthetic process."
  (when (processp descriptor)
    (process-put descriptor 'tramp-rpc-file-notify-removing t)
    (when (process-live-p descriptor)
      (delete-process descriptor))))

(defun tramp-rpc--cleanup-file-notify-for-connection
    (&optional vec connection-process)
  "Remove file notification descriptors for VEC's CONNECTION-PROCESS.
When VEC is nil, remove all descriptors."
  (let* ((prefix (and vec (concat (tramp-rpc--connection-key-string vec) ":")))
         (descriptors-to-remove nil))
    (maphash
     (lambda (descriptor data)
       (let* ((watch-key (plist-get data :watch-key))
              (lease (and watch-key
                          (gethash watch-key tramp-rpc--watch-leases)))
              (owner (or (plist-get data :connection-process)
                         (and lease
                              (tramp-rpc-watch-lease-connection-process lease)))))
         (when (and (or (null prefix)
                        (and watch-key (string-prefix-p prefix watch-key)))
                    (or (null connection-process)
                        (null owner)
                        (eq connection-process owner)))
           (push descriptor descriptors-to-remove))))
     tramp-rpc--file-notify-descriptors)
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
    ;; Clear file-notify state from any remaining watch leases for this
    ;; connection (e.g., if called before or without watch cleanup).
    (let (drop-keys)
      (maphash
       (lambda (key lease)
         (let ((owner (tramp-rpc-watch-lease-connection-process lease)))
           (when (and (> (tramp-rpc-watch-lease-notify-count lease) 0)
                      (or (null prefix) (string-prefix-p prefix key))
                      (or (null connection-process)
                          (null owner)
                          (eq connection-process owner)))
             (setf (tramp-rpc-watch-lease-notify-count lease) 0
                   (tramp-rpc-watch-lease-notify-owned lease) nil
                   (tramp-rpc-watch-lease-notify-synthetic lease) nil
                   (tramp-rpc-watch-lease-notify-nofollow lease) nil
                   (tramp-rpc-watch-lease-notify-canonical-directory lease) nil)
             (unless (tramp-rpc-watch-lease-explicit-p lease)
               (push key drop-keys)))))
       tramp-rpc--watch-leases)
      (dolist (key drop-keys)
        (remhash key tramp-rpc--watch-leases)))
    (when descriptors-to-remove
      (tramp-rpc--debug
       "Cleaned up %d file-notify descriptors%s"
       (length descriptors-to-remove)
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

(defun tramp-rpc--file-notify-canonical-directory (data)
  "Return the canonical directory associated with descriptor DATA."
  (let* ((watch-key (plist-get data :watch-key))
         (lease (and watch-key
                     (gethash watch-key tramp-rpc--watch-leases))))
    ;; Prefer the shared watch lease, because explicit unwatch can restore a
    ;; file-notify-owned direct watch and learn a newer canonical path after
    ;; descriptors were created.
    (or (and lease (tramp-rpc--watch-lease-notify-directory lease))
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
DATA is the descriptor metadata plist."
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
         (when (not matched)
           (let* ((watch-key (plist-get data :watch-key))
                  (lease (and watch-key
                              (gethash watch-key tramp-rpc--watch-leases))))
             (when (and lease
                        (tramp-rpc-watch-lease-notify-synthetic lease)
                        (tramp-rpc--file-notify-path-matches-p data file-name))
               (setq matched t)))))
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
    (let* ((watch-key (tramp-rpc--watch-key v localname))
           ;; Finish local setup before acquiring or incrementing a watch.
           (descriptor (tramp-rpc--make-file-notify-descriptor
                        v directory localname))
           registered)
      (unwind-protect
          (let ((lease (tramp-rpc--watch-acquire-notify v directory localname)))
            (puthash descriptor
                     (list :directory directory
                           :canonical-directory
                           (tramp-rpc--watch-lease-notify-directory lease)
                           :flags flags
                           :localname localname
                           :watch-key watch-key
                           :connection-process
                           (tramp-rpc-watch-lease-connection-process lease))
                     tramp-rpc--file-notify-descriptors)
            (setq registered t)
            descriptor)
        ;; Registration errors and quits must not leave an untracked process.
        (unless registered
          (tramp-rpc--delete-file-notify-descriptor-process descriptor))))))

(defun tramp-rpc-handle-file-notify-rm-watch (descriptor)
  "Like `file-notify-rm-watch' for TRAMP-RPC watch DESCRIPTOR."
  (when-let* ((data (gethash descriptor tramp-rpc--file-notify-descriptors)))
    (tramp-rpc--watch-release-notify (plist-get data :watch-key))
    (remhash descriptor tramp-rpc--file-notify-descriptors)
    (tramp-rpc--delete-file-notify-descriptor-process descriptor)))

(defun tramp-rpc-handle-file-notify-valid-p (descriptor)
  "Like `file-notify-valid-p' for TRAMP-RPC watch DESCRIPTOR."
  (and (processp descriptor)
       (process-live-p descriptor)
       (gethash descriptor tramp-rpc--file-notify-descriptors)
       t))

;; Internal transport teardown and fs.events hooks.
;; These are backend lifecycle wiring, not opt-in editor integrations.
(add-hook 'tramp-rpc-transport-cleanup-functions
          #'tramp-rpc--cleanup-file-notify-for-connection t)
(add-hook 'tramp-rpc-fs-event-dispatch-functions
          #'tramp-rpc--file-notify-dispatch t)
(add-hook 'tramp-rpc-fs-rescan-functions
          #'tramp-rpc--file-notify-dispatch-rescan t)

(provide 'tramp-rpc-notify)
;;; tramp-rpc-notify.el ends here
