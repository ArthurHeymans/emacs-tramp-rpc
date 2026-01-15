;;; tramp-rpc.el --- TRAMP backend using RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Arthur

;; Author: Arthur
;; Version: 0.1.0
;; Keywords: comm, processes, files
;; Package-Requires: ((emacs "30.1"))

;; This file is part of tramp-rpc.

;; tramp-rpc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This package provides a TRAMP backend that uses a custom RPC server
;; instead of parsing shell command output.  This significantly improves
;; performance for remote file operations.
;;
;; To use, add to your init.el:
;;   (require 'tramp-rpc)
;;
;; Then access files using the "rpc" method:
;;   /rpc:user@host:/path/to/file
;;
;; FEATURES:
;; - Fast file operations via binary RPC protocol
;; - Async process support (make-process, start-file-process)
;; - VC mode integration works (git, etc.)
;;
;; HOW ASYNC PROCESSES WORK:
;; Remote processes are started via RPC and polled periodically for output.
;; A local pipe process serves as a relay to provide Emacs process semantics.
;; Process filters, sentinels, and signals all work as expected.
;;
;; OPTIONAL CONFIGURATION:
;; If you experience issues with diff-hl in dired, you can disable it:
;;   (setq diff-hl-disable-on-remote t)

;;; Code:

(require 'tramp)
(require 'tramp-sh)
(require 'tramp-rpc-protocol)
(require 'tramp-rpc-deploy)

(defgroup tramp-rpc nil
  "TRAMP backend using RPC."
  :group 'tramp)

(defconst tramp-rpc-method "rpc"
  "TRAMP method for RPC-based remote access.")

;; ============================================================================
;; Connection management
;; ============================================================================

(defvar tramp-rpc--connections (make-hash-table :test 'equal)
  "Hash table mapping connection keys to RPC process info.
Key is (host user port), value is a plist with :process and :buffer.")

(defvar tramp-rpc--async-processes (make-hash-table :test 'eq)
  "Hash table mapping local relay processes to their remote process info.
Value is a plist with :vec, :pid, :timer, :stderr-buffer.")

(defun tramp-rpc--connection-key (vec)
  "Generate a connection key for VEC."
  (list (tramp-file-name-host vec)
        (tramp-file-name-user vec)
        (or (tramp-file-name-port vec) 22)))

(defun tramp-rpc--get-connection (vec)
  "Get the RPC connection for VEC, or nil if not connected."
  (gethash (tramp-rpc--connection-key vec) tramp-rpc--connections))

(defun tramp-rpc--set-connection (vec process buffer)
  "Store the RPC connection for VEC."
  (puthash (tramp-rpc--connection-key vec)
           (list :process process :buffer buffer)
           tramp-rpc--connections))

(defun tramp-rpc--remove-connection (vec)
  "Remove the RPC connection for VEC."
  (remhash (tramp-rpc--connection-key vec) tramp-rpc--connections))

(defun tramp-rpc--ensure-connection (vec)
  "Ensure we have an active RPC connection to VEC.
Returns the connection plist."
  (let ((conn (tramp-rpc--get-connection vec)))
    (if (and conn
             (process-live-p (plist-get conn :process)))
        conn
      ;; Need to establish connection
      (tramp-rpc--connect vec))))

(defun tramp-rpc--connect (vec)
  "Establish an RPC connection to VEC."
  ;; First, ensure the binary is deployed using shell-based tramp
  (let* ((binary-path (tramp-rpc-deploy-ensure-binary vec))
         (host (tramp-file-name-host vec))
         (user (tramp-file-name-user vec))
         (port (tramp-file-name-port vec))
         ;; Build SSH command to run the RPC server
         (ssh-args (append
                    (list "ssh")
                    (when user (list "-l" user))
                    (when port (list "-p" (number-to-string port)))
                    (list "-o" "BatchMode=yes")
                    (list "-o" "StrictHostKeyChecking=accept-new")
                    (list host binary-path)))
         (process-name (format "tramp-rpc<%s>" host))
         (buffer-name (format " *tramp-rpc %s*" host))
         (buffer (get-buffer-create buffer-name))
         process)

    ;; Clear buffer
    (with-current-buffer buffer
      (erase-buffer)
      (set-buffer-multibyte nil))

    ;; Start the process
    (setq process (apply #'start-process process-name buffer ssh-args))

    ;; Configure process
    (set-process-query-on-exit-flag process nil)
    (set-process-coding-system process 'utf-8 'utf-8)

    ;; Store connection
    (tramp-rpc--set-connection vec process buffer)

    ;; Wait for server to be ready by sending a ping
    (let ((response (tramp-rpc--call vec "system.info" nil)))
      (unless response
        (tramp-rpc--remove-connection vec)
        (error "Failed to connect to RPC server on %s" host)))

    (tramp-rpc--get-connection vec)))

(defun tramp-rpc--disconnect (vec)
  "Disconnect the RPC connection to VEC."
  ;; First, clean up any async processes for this connection
  (tramp-rpc--cleanup-async-processes vec)
  (let ((conn (tramp-rpc--get-connection vec)))
    (when conn
      (let ((process (plist-get conn :process)))
        (when (process-live-p process)
          (delete-process process)))
      (tramp-rpc--remove-connection vec))))

(defun tramp-rpc--cleanup-async-processes (&optional vec)
  "Clean up async processes, optionally only those for VEC."
  (maphash
   (lambda (local-process info)
     (when (or (null vec)
               (equal (tramp-rpc--connection-key (plist-get info :vec))
                      (tramp-rpc--connection-key vec)))
       ;; Cancel timer
       (when-let ((timer (plist-get info :timer)))
         (cancel-timer timer))
       ;; Kill local process
       (when (process-live-p local-process)
         (delete-process local-process))
       ;; Remove from tracking
       (remhash local-process tramp-rpc--async-processes)))
   tramp-rpc--async-processes))

;; ============================================================================
;; RPC communication
;; ============================================================================

(defun tramp-rpc--call (vec method params)
  "Call METHOD with PARAMS on the RPC server for VEC.
Returns the result or signals an error."
  (let* ((conn (tramp-rpc--ensure-connection vec))
         (process (plist-get conn :process))
         (buffer (plist-get conn :buffer))
         (request (tramp-rpc-protocol-encode-request method params)))

    ;; Send request
    (process-send-string process (concat request "\n"))

    ;; Wait for response
    (with-current-buffer buffer
      (let ((start (point-max))
            (timeout 30)
            response-line)
        ;; Wait for a complete line
        (while (and (not response-line)
                    (> timeout 0)
                    (process-live-p process))
          (with-local-quit
            (accept-process-output process 0.1))
          (goto-char start)
          (when (search-forward "\n" nil t)
            (setq response-line (buffer-substring start (1- (point))))
            ;; Clear processed data
            (delete-region (point-min) (point)))
          (cl-decf timeout 0.1))

        (unless response-line
          (error "Timeout waiting for RPC response from %s"
                 (tramp-file-name-host vec)))

        (let ((response (tramp-rpc-protocol-decode-response response-line)))
          (if (tramp-rpc-protocol-error-p response)
              (let ((code (tramp-rpc-protocol-error-code response))
                    (msg (tramp-rpc-protocol-error-message response)))
                (cond
                 ((= code tramp-rpc-protocol-error-file-not-found)
                  (signal 'file-missing (list "RPC" "No such file" msg)))
                 ((= code tramp-rpc-protocol-error-permission-denied)
                  (signal 'file-error (list "RPC" "Permission denied" msg)))
                 (t
                  (error "RPC error: %s" msg))))
            (plist-get response :result)))))))

;; ============================================================================
;; File name handler operations
;; ============================================================================

(defun tramp-rpc-handle-file-exists-p (filename)
  "Like `file-exists-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (tramp-rpc--call v "file.exists" `((path . ,localname)))))

(defun tramp-rpc-handle-file-readable-p (filename)
  "Like `file-readable-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (tramp-rpc--call v "file.readable" `((path . ,localname)))))

(defun tramp-rpc-handle-file-writable-p (filename)
  "Like `file-writable-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (if (tramp-rpc--call v "file.exists" `((path . ,localname)))
        (tramp-rpc--call v "file.writable" `((path . ,localname)))
      ;; File doesn't exist, check if parent directory is writable
      (let ((parent (file-name-directory (directory-file-name localname))))
        (tramp-rpc--call v "file.writable" `((path . ,parent)))))))

(defun tramp-rpc-handle-file-executable-p (filename)
  "Like `file-executable-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (tramp-rpc--call v "file.executable" `((path . ,localname)))))

(defun tramp-rpc-handle-file-directory-p (filename)
  "Like `file-directory-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (condition-case nil
        (let ((result (tramp-rpc--call v "file.stat" `((path . ,localname)))))
          (equal (alist-get 'type result) "directory"))
      (file-missing nil))))

(defun tramp-rpc-handle-file-regular-p (filename)
  "Like `file-regular-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (condition-case nil
        (let ((result (tramp-rpc--call v "file.stat" `((path . ,localname)))))
          (equal (alist-get 'type result) "file"))
      (file-missing nil))))

(defun tramp-rpc-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (condition-case nil
        (let ((result (tramp-rpc--call v "file.stat" `((path . ,localname) (lstat . t)))))
          (when (equal (alist-get 'type result) "symlink")
            (alist-get 'link_target result)))
      (file-missing nil))))

(defun tramp-rpc-handle-file-truename (filename)
  "Like `file-truename' for TRAMP-RPC files.
If the file doesn't exist, return the filename unchanged (like local file-truename)."
  (with-parsed-tramp-file-name filename nil
    (condition-case nil
        (let ((result (tramp-rpc--call v "file.truename" `((path . ,localname)))))
          (tramp-make-tramp-file-name v result))
      ;; If file doesn't exist, return the filename unchanged
      (file-missing filename))))

(defun tramp-rpc-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (condition-case nil
        (let ((result (tramp-rpc--call v "file.stat" `((path . ,localname) (lstat . t)))))
          (tramp-rpc--convert-file-attributes result id-format))
      (file-missing nil))))

(defun tramp-rpc--convert-file-attributes (stat id-format)
  "Convert STAT result to Emacs file-attributes format.
ID-FORMAT specifies whether to use numeric or string IDs."
  (let* ((type-str (alist-get 'type stat))
         (type (pcase type-str
                 ("file" nil)
                 ("directory" t)
                 ("symlink" (alist-get 'link_target stat))
                 (_ nil)))
         (nlinks (alist-get 'nlinks stat))
         (uid (alist-get 'uid stat))
         (gid (alist-get 'gid stat))
         (uname (alist-get 'uname stat))
         (gname (alist-get 'gname stat))
         (atime (seconds-to-time (alist-get 'atime stat)))
         (mtime (seconds-to-time (alist-get 'mtime stat)))
         (ctime (seconds-to-time (alist-get 'ctime stat)))
         (size (alist-get 'size stat))
         (mode (tramp-rpc--mode-to-string (alist-get 'mode stat) type-str))
         (inode (alist-get 'inode stat))
         (dev (alist-get 'dev stat)))
    ;; Return in file-attributes format
    (list type nlinks
          (if (eq id-format 'string) (or uname (number-to-string uid)) uid)
          (if (eq id-format 'string) (or gname (number-to-string gid)) gid)
          atime mtime ctime
          size mode nil inode dev)))

(defun tramp-rpc--mode-to-string (mode type)
  "Convert numeric MODE to a string like \"drwxr-xr-x\".
TYPE is the file type string."
  (let ((type-char (pcase type
                     ("directory" ?d)
                     ("symlink" ?l)
                     ("file" ?-)
                     ("chardevice" ?c)
                     ("blockdevice" ?b)
                     ("fifo" ?p)
                     ("socket" ?s)
                     (_ ?-))))
    (format "%c%c%c%c%c%c%c%c%c%c"
            type-char
            (if (> (logand mode #o400) 0) ?r ?-)
            (if (> (logand mode #o200) 0) ?w ?-)
            (if (> (logand mode #o4000) 0)
                (if (> (logand mode #o100) 0) ?s ?S)
              (if (> (logand mode #o100) 0) ?x ?-))
            (if (> (logand mode #o040) 0) ?r ?-)
            (if (> (logand mode #o020) 0) ?w ?-)
            (if (> (logand mode #o2000) 0)
                (if (> (logand mode #o010) 0) ?s ?S)
              (if (> (logand mode #o010) 0) ?x ?-))
            (if (> (logand mode #o004) 0) ?r ?-)
            (if (> (logand mode #o002) 0) ?w ?-)
            (if (> (logand mode #o1000) 0)
                (if (> (logand mode #o001) 0) ?t ?T)
              (if (> (logand mode #o001) 0) ?x ?-)))))

(defun tramp-rpc-handle-file-modes (filename &optional flag)
  "Like `file-modes' for TRAMP-RPC files."
  (let ((attrs (tramp-rpc-handle-file-attributes filename)))
    (when attrs
      (tramp-mode-string-to-int (nth 8 attrs)))))

(defun tramp-rpc-handle-set-file-modes (filename mode &optional flag)
  "Like `set-file-modes' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (tramp-rpc--call v "file.set_modes" `((path . ,localname) (mode . ,mode)))))

(defun tramp-rpc-handle-set-file-times (filename &optional timestamp flag)
  "Like `set-file-times' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (let ((mtime (floor (float-time (or timestamp (current-time))))))
      (tramp-rpc--call v "file.set_times" `((path . ,localname) (mtime . ,mtime))))))

(defun tramp-rpc-handle-file-newer-than-file-p (file1 file2)
  "Like `file-newer-than-file-p' for TRAMP-RPC files."
  (cond
   ((not (file-exists-p file1)) nil)
   ((not (file-exists-p file2)) t)
   (t
    ;; Both files exist, compare mtimes
    (let ((mtime1 (float-time (file-attribute-modification-time
                               (file-attributes file1))))
          (mtime2 (float-time (file-attribute-modification-time
                               (file-attributes file2)))))
      (> mtime1 mtime2)))))

;; ============================================================================
;; Directory operations
;; ============================================================================

(defun tramp-rpc-handle-directory-files (directory &optional full match nosort count)
  "Like `directory-files' for TRAMP-RPC files."
  (with-parsed-tramp-file-name (expand-file-name directory) nil
    (let* ((result (tramp-rpc--call v "dir.list"
                                    `((path . ,localname)
                                      (include_attrs . :json-false)
                                      (include_hidden . t))))
           (files (mapcar (lambda (entry) (alist-get 'name entry)) result)))
      ;; Filter by match pattern
      (when match
        (setq files (cl-remove-if-not
                     (lambda (f) (string-match-p match f))
                     files)))
      ;; Add full path if requested
      (when full
        (setq files (mapcar
                     (lambda (f)
                       (tramp-make-tramp-file-name
                        v (expand-file-name f localname)))
                     files)))
      ;; Sort unless nosort
      (unless nosort
        (setq files (sort files #'string<)))
      ;; Limit count
      (when count
        (setq files (seq-take files count)))
      files)))

(defun tramp-rpc-handle-directory-files-and-attributes
    (directory &optional full match nosort id-format count)
  "Like `directory-files-and-attributes' for TRAMP-RPC files."
  (with-parsed-tramp-file-name (expand-file-name directory) nil
    (let* ((result (tramp-rpc--call v "dir.list"
                                    `((path . ,localname)
                                      (include_attrs . t)
                                      (include_hidden . t))))
           (entries (mapcar
                     (lambda (entry)
                       (let* ((name (alist-get 'name entry))
                              (attrs (alist-get 'attrs entry))
                              (full-name (if full
                                             (tramp-make-tramp-file-name
                                              v (expand-file-name name localname))
                                           name)))
                         (cons full-name
                               (when attrs
                                 (tramp-rpc--convert-file-attributes attrs id-format)))))
                     result)))
      ;; Filter by match pattern
      (when match
        (setq entries (cl-remove-if-not
                       (lambda (e) (string-match-p match (car e)))
                       entries)))
      ;; Sort unless nosort
      (unless nosort
        (setq entries (sort entries (lambda (a b) (string< (car a) (car b))))))
      ;; Limit count
      (when count
        (setq entries (seq-take entries count)))
      entries)))

(defun tramp-rpc-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for TRAMP-RPC files."
  (with-parsed-tramp-file-name (expand-file-name directory) nil
    (tramp-rpc--call v "dir.completions"
                     `((directory . ,localname)
                       (prefix . ,filename)))))

(defun tramp-rpc-handle-make-directory (dir &optional parents)
  "Like `make-directory' for TRAMP-RPC files."
  (with-parsed-tramp-file-name dir nil
    (tramp-rpc--call v "dir.create"
                     `((path . ,localname)
                       (parents . ,(if parents t :json-false))))))

(defun tramp-rpc-handle-delete-directory (directory &optional recursive trash)
  "Like `delete-directory' for TRAMP-RPC files."
  (tramp-skeleton-delete-directory directory recursive trash
    (tramp-rpc--call v "dir.remove"
                     `((path . ,localname)
                       (recursive . ,(if recursive t :json-false))))))

(defun tramp-rpc-handle-insert-directory
    (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for TRAMP-RPC files.
Produces ls-like output for dired."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (let* ((result (tramp-rpc--call v "dir.list"
                                    `((path . ,localname)
                                      (include_attrs . t)
                                      (include_hidden . ,(if (string-match-p "a" (or switches "")) t :json-false)))))
           ;; Convert vector to list if needed
           (result-list (if (vectorp result) (append result nil) result))
           (entries (sort result-list (lambda (a b)
                                        (string< (alist-get 'name a)
                                                 (alist-get 'name b))))))
      ;; Insert header
      (insert (format "  %s:\n" filename))
      (insert "  total 0\n")  ; We don't calculate total blocks

      ;; Insert each entry
      (dolist (entry entries)
        (let* ((name (alist-get 'name entry))
               (attrs (alist-get 'attrs entry))
               (type (alist-get 'type attrs))
               (mode (alist-get 'mode attrs))
               (nlinks (or (alist-get 'nlinks attrs) 1))
               (uid (or (alist-get 'uid attrs) 0))
               (gid (or (alist-get 'gid attrs) 0))
               (uname (or (alist-get 'uname attrs) (number-to-string uid)))
               (gname (or (alist-get 'gname attrs) (number-to-string gid)))
               (size (or (alist-get 'size attrs) 0))
               (mtime (alist-get 'mtime attrs))
               (link-target (alist-get 'link_target attrs))
               (mode-str (tramp-rpc--mode-to-string (or mode 0) (or type "file")))
               (time-str (if mtime
                             (format-time-string "%b %e %H:%M" (seconds-to-time mtime))
                           "Jan  1 00:00")))
          ;; Skip . and .. unless -a is given
          (unless (and (member name '("." ".."))
                       (not (string-match-p "a" (or switches ""))))
            (insert (format "  %s %3d %-8s %-8s %8d %s %s"
                            mode-str nlinks uname gname size time-str name))
            (when (and link-target (equal type "symlink"))
              (insert (format " -> %s" link-target)))
            (insert "\n")))))))

;; ============================================================================
;; File I/O operations
;; ============================================================================

(defun tramp-rpc-handle-insert-file-contents
    (filename &optional visit beg end replace)
  "Like `insert-file-contents' for TRAMP-RPC files."
  (barf-if-buffer-read-only)
  (with-parsed-tramp-file-name filename nil
    (let* ((params `((path . ,localname)))
           (_ (when beg (push `(offset . ,beg) params)))
           (_ (when end (push `(length . ,(- end (or beg 0))) params)))
           (result (tramp-rpc--call v "file.read" params))
           (content (base64-decode-string (alist-get 'content result)))
           (size (length content)))

      (when replace
        (delete-region (point-min) (point-max)))

      (let ((point (point)))
        (insert content)
        (when visit
          (setq buffer-file-name filename)
          (set-visited-file-modtime)
          (set-buffer-modified-p nil)))

      (list filename size))))

(defun tramp-rpc-handle-write-region
    (start end filename &optional append visit lockname mustbenew)
  "Like `write-region' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (let* ((content (buffer-substring-no-properties start end))
           (encoded (base64-encode-string content t))
           (params `((path . ,localname)
                     (content . ,encoded)
                     (append . ,(if append t :json-false)))))

      ;; Check mustbenew
      (when mustbenew
        (when (file-exists-p filename)
          (if (eq mustbenew 'excl)
              (signal 'file-already-exists (list filename))
            (unless (yes-or-no-p
                     (format "File %s exists; overwrite? " filename))
              (signal 'file-already-exists (list filename))))))

      (tramp-rpc--call v "file.write" params)

      ;; Handle visit
      (when (or (eq visit t) (stringp visit))
        (setq buffer-file-name filename)
        (set-visited-file-modtime)
        (set-buffer-modified-p nil))
      (when (stringp visit)
        (setq buffer-file-name visit))

      nil)))

(defun tramp-rpc-handle-copy-file
    (filename newname &optional ok-if-already-exists keep-time
              preserve-uid-gid preserve-permissions)
  "Like `copy-file' for TRAMP-RPC files."
  (let ((source-remote (tramp-tramp-file-p filename))
        (dest-remote (tramp-tramp-file-p newname)))
    (cond
     ;; Both on same remote host using RPC
     ((and source-remote dest-remote
           (tramp-equal-remote filename newname))
      (with-parsed-tramp-file-name filename v1
        (with-parsed-tramp-file-name newname v2
          (unless ok-if-already-exists
            (when (file-exists-p newname)
              (signal 'file-already-exists (list newname))))
          (tramp-rpc--call v1 "file.copy"
                           `((src . ,v1-localname)
                             (dest . ,v2-localname)
                             (preserve . ,(if (or keep-time preserve-permissions) t :json-false)))))))
     ;; Different hosts or local, use default handler
     (t
      (tramp-run-real-handler
       #'copy-file
       (list filename newname ok-if-already-exists keep-time
             preserve-uid-gid preserve-permissions))))))

(defun tramp-rpc-handle-rename-file (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for TRAMP-RPC files."
  (let ((source-remote (tramp-tramp-file-p filename))
        (dest-remote (tramp-tramp-file-p newname)))
    (cond
     ;; Both on same remote host using RPC
     ((and source-remote dest-remote
           (tramp-equal-remote filename newname))
      (with-parsed-tramp-file-name filename v1
        (with-parsed-tramp-file-name newname v2
          (tramp-rpc--call v1 "file.rename"
                           `((src . ,v1-localname)
                             (dest . ,v2-localname)
                             (overwrite . ,(if ok-if-already-exists t :json-false)))))))
     ;; Different hosts, copy then delete
     (t
      (copy-file filename newname ok-if-already-exists t t t)
      (delete-file filename)))))

(defun tramp-rpc-handle-delete-file (filename &optional trash)
  "Like `delete-file' for TRAMP-RPC files."
  (tramp-skeleton-delete-file filename trash
    (tramp-rpc--call v "file.delete" `((path . ,localname)))))

(defun tramp-rpc-handle-make-symbolic-link (target linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for TRAMP-RPC files."
  (with-parsed-tramp-file-name linkname nil
    (unless ok-if-already-exists
      (when (file-exists-p linkname)
        (signal 'file-already-exists (list linkname))))
    (when (file-exists-p linkname)
      (delete-file linkname))
    (tramp-rpc--call v "file.make_symlink"
                     `((target . ,(if (tramp-tramp-file-p target)
                                      (tramp-file-local-name target)
                                    target))
                       (link_path . ,localname)))))

;; ============================================================================
;; Process operations
;; ============================================================================

(defun tramp-rpc-handle-process-file
    (program &optional infile destination display &rest args)
  "Like `process-file' for TRAMP-RPC files."
  (with-parsed-tramp-file-name default-directory nil
    (let* ((stdin-content (when (and infile (not (eq infile t)))
                            (with-temp-buffer
                              (insert-file-contents infile)
                              (base64-encode-string (buffer-string) t))))
           (result (tramp-rpc--call v "process.run"
                                    `((cmd . ,program)
                                      (args . ,(vconcat args))
                                      (cwd . ,localname)
                                      ,@(when stdin-content
                                          `((stdin . ,stdin-content))))))
           (exit-code (alist-get 'exit_code result))
           (stdout (base64-decode-string (alist-get 'stdout result)))
           (stderr (base64-decode-string (alist-get 'stderr result))))

      ;; Handle destination
      (cond
       ((null destination) nil)
       ((eq destination t)
        (insert stdout))
       ((stringp destination)
        (with-temp-file destination
          (insert stdout)))
       ((bufferp destination)
        (with-current-buffer destination
          (insert stdout)))
       ((consp destination)
        (let ((stdout-dest (car destination))
              (stderr-dest (cadr destination)))
          (when stdout-dest
            (cond
             ((eq stdout-dest t) (insert stdout))
             ((stringp stdout-dest)
              (with-temp-file stdout-dest (insert stdout)))
             ((bufferp stdout-dest)
              (with-current-buffer stdout-dest (insert stdout)))))
          (when stderr-dest
            (cond
             ((stringp stderr-dest)
              (with-temp-file stderr-dest (insert stderr)))
             ((bufferp stderr-dest)
              (with-current-buffer stderr-dest (insert stderr))))))))

      exit-code)))

(defun tramp-rpc-handle-shell-command (command &optional output-buffer error-buffer)
  "Like `shell-command' for TRAMP-RPC files."
  (with-parsed-tramp-file-name default-directory nil
    (let* ((result (tramp-rpc--call v "process.run"
                                    `((cmd . "/bin/sh")
                                      (args . ["-c" ,command])
                                      (cwd . ,localname))))
           (exit-code (alist-get 'exit_code result))
           (stdout (base64-decode-string (alist-get 'stdout result)))
           (stderr (base64-decode-string (alist-get 'stderr result))))

      (when output-buffer
        (with-current-buffer (get-buffer-create output-buffer)
          (erase-buffer)
          (insert stdout)))

      (when error-buffer
        (with-current-buffer (get-buffer-create error-buffer)
          (erase-buffer)
          (insert stderr)))

      exit-code)))

(defun tramp-rpc-handle-vc-registered (file)
  "Like `vc-registered' for TRAMP-RPC files.
Since tramp-rpc supports `process-file', VC backends can run their
commands (git, svn, hg) directly via RPC.

We set `default-directory' to the file's directory to ensure that
process-file calls from VC backends are routed through our tramp handler."
  (when vc-handled-backends
    (with-parsed-tramp-file-name file nil
      ;; Set default-directory to the file's remote directory so that
      ;; process-file calls from VC are handled by our tramp handler.
      (let ((default-directory (file-name-directory file))
            process-file-side-effects)
        (tramp-run-real-handler #'vc-registered (list file))))))

(defun tramp-rpc-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for TRAMP-RPC files."
  (let ((dir (or dir default-directory)))
    (cond
     ;; Absolute path
     ((file-name-absolute-p name)
      (if (tramp-tramp-file-p name)
          name
        (with-parsed-tramp-file-name dir nil
          (tramp-make-tramp-file-name v name))))
     ;; Relative path
     (t
      (tramp-make-tramp-file-name
       (tramp-dissect-file-name dir)
       (expand-file-name name (tramp-file-local-name dir)))))))

;; ============================================================================
;; Additional handlers to avoid shell dependency
;; ============================================================================

(defun tramp-rpc-handle-exec-path ()
  "Return remote exec-path using RPC."
  ;; Return a sensible default path
  '("/usr/local/bin" "/usr/bin" "/bin" "/usr/local/sbin" "/usr/sbin" "/sbin"))

(defun tramp-rpc-handle-file-local-copy (filename)
  "Create a local copy of remote FILENAME using RPC."
  (with-parsed-tramp-file-name filename nil
    (let* ((result (tramp-rpc--call v "file.read" `((path . ,localname))))
           (content (base64-decode-string (alist-get 'content result)))
           (tmpfile (tramp-make-temp-file nil)))
      (with-temp-file tmpfile
        (set-buffer-multibyte nil)
        (insert content))
      tmpfile)))

(defun tramp-rpc-handle-get-home-directory (vec &optional user)
  "Return home directory for USER on remote host VEC using RPC."
  (let ((result (tramp-rpc--call vec "system.info" nil)))
    (or (alist-get 'home result) "~")))

(defun tramp-rpc-handle-get-remote-uid (vec id-format)
  "Return remote UID using RPC."
  (let ((result (tramp-rpc--call vec "system.info" nil)))
    (let ((uid (alist-get 'uid result)))
      (if (eq id-format 'integer)
          uid
        (number-to-string uid)))))

(defun tramp-rpc-handle-get-remote-gid (vec id-format)
  "Return remote GID using RPC."
  (let ((result (tramp-rpc--call vec "system.info" nil)))
    (let ((gid (alist-get 'gid result)))
      (if (eq id-format 'integer)
          gid
        (number-to-string gid)))))

;; ============================================================================
;; Async Process Support
;; ============================================================================

(defvar tramp-rpc--process-poll-interval 0.1
  "Interval in seconds between polling remote process for output.")

(defvar tramp-rpc--poll-in-progress nil
  "Non-nil while a poll is in progress (prevents reentrancy).")

(defun tramp-rpc--start-remote-process (vec program args cwd)
  "Start PROGRAM with ARGS in CWD on remote host VEC.
Returns the remote process PID."
  (let ((result (tramp-rpc--call vec "process.start"
                                 `((cmd . ,program)
                                   (args . ,(vconcat args))
                                   (cwd . ,cwd)))))
    (alist-get 'pid result)))

(defun tramp-rpc--read-remote-process (vec pid)
  "Read output from remote process PID on VEC.
Returns plist with :stdout, :stderr, :exited, :exit-code."
  (let ((result (tramp-rpc--call vec "process.read" `((pid . ,pid)))))
    (list :stdout (when-let ((s (alist-get 'stdout result)))
                    (base64-decode-string s))
          :stderr (when-let ((s (alist-get 'stderr result)))
                    (base64-decode-string s))
          :exited (alist-get 'exited result)
          :exit-code (alist-get 'exit_code result))))

(defun tramp-rpc--write-remote-process (vec pid data)
  "Write DATA to stdin of remote process PID on VEC."
  (tramp-rpc--call vec "process.write"
                   `((pid . ,pid)
                     (data . ,(base64-encode-string data t)))))

(defun tramp-rpc--close-remote-stdin (vec pid)
  "Close stdin of remote process PID on VEC."
  (tramp-rpc--call vec "process.close_stdin" `((pid . ,pid))))

(defun tramp-rpc--kill-remote-process (vec pid &optional signal)
  "Send SIGNAL to remote process PID on VEC."
  (tramp-rpc--call vec "process.kill"
                   `((pid . ,pid)
                     (signal . ,(or signal 15))))) ; SIGTERM

(defun tramp-rpc--poll-process (local-process)
  "Poll for output from the remote process associated with LOCAL-PROCESS."
  ;; Guard against reentrancy (can happen during accept-process-output)
  (unless tramp-rpc--poll-in-progress
    (let ((tramp-rpc--poll-in-progress t))
      (when (process-live-p local-process)
        (let ((info (gethash local-process tramp-rpc--async-processes)))
          (when info
            (let* ((vec (plist-get info :vec))
                   (pid (plist-get info :pid))
                   (stderr-buffer (plist-get info :stderr-buffer))
                   (result (condition-case err
                               (tramp-rpc--read-remote-process vec pid)
                             (error
                              (message "tramp-rpc: Error polling process: %s" err)
                              nil))))
          (when result
            ;; Handle stdout - send to process filter/buffer
            (when-let ((stdout (plist-get result :stdout)))
              (when (> (length stdout) 0)
                (if-let ((filter (process-filter local-process)))
                    (funcall filter local-process stdout)
                  (when-let ((buf (process-buffer local-process)))
                    (when (buffer-live-p buf)
                      (with-current-buffer buf
                        (goto-char (point-max))
                        (insert stdout)))))))
            
            ;; Handle stderr - send to stderr buffer if specified
            (when-let ((stderr (plist-get result :stderr)))
              (when (> (length stderr) 0)
                (cond
                 ((bufferp stderr-buffer)
                  (when (buffer-live-p stderr-buffer)
                    (with-current-buffer stderr-buffer
                      (goto-char (point-max))
                      (insert stderr))))
                 ;; If no stderr buffer, mix with stdout
                 (t
                  (if-let ((filter (process-filter local-process)))
                      (funcall filter local-process stderr)
                    (when-let ((buf (process-buffer local-process)))
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (goto-char (point-max))
                          (insert stderr)))))))))
            
            ;; Check if process exited
            (when (plist-get result :exited)
              (tramp-rpc--handle-process-exit
               local-process (plist-get result :exit-code)))))))))))

(defun tramp-rpc--handle-process-exit (local-process exit-code)
  "Handle exit of remote process associated with LOCAL-PROCESS."
  (let ((info (gethash local-process tramp-rpc--async-processes)))
    (when info
      ;; Cancel the timer
      (when-let ((timer (plist-get info :timer)))
        (cancel-timer timer))
      ;; Remove from tracking
      (remhash local-process tramp-rpc--async-processes)
      ;; Store exit code and mark as exited
      (process-put local-process :tramp-rpc-exit-code exit-code)
      (process-put local-process :tramp-rpc-exited t)
      ;; Get sentinel before we modify anything
      (let ((sentinel (process-sentinel local-process))
            (event (if (= exit-code 0)
                       "finished\n"
                     (format "exited abnormally with code %d\n" exit-code))))
        ;; Remove sentinel temporarily to prevent double-call
        (set-process-sentinel local-process nil)
        ;; Delete the process (changes status)
        (when (process-live-p local-process)
          (delete-process local-process))
        ;; Call sentinel with our custom event
        (when sentinel
          (funcall sentinel local-process event))))))

(defun tramp-rpc-handle-make-process (&rest args)
  "Create an async process on the remote host.
ARGS are keyword arguments as per `make-process'."
  (let* ((name (plist-get args :name))
         (buffer (plist-get args :buffer))
         (command (plist-get args :command))
         (coding (plist-get args :coding))
         (noquery (plist-get args :noquery))
         (filter (plist-get args :filter))
         (sentinel (plist-get args :sentinel))
         (stderr (plist-get args :stderr))
         (file-handler (plist-get args :file-handler))
         (program (car command))
         (program-args (cdr command)))
    
    ;; Ensure we're in a remote directory
    (unless (tramp-tramp-file-p default-directory)
      (error "tramp-rpc-handle-make-process called without remote default-directory"))
    
    (with-parsed-tramp-file-name default-directory nil
      ;; Find the actual program path on remote
      (let* ((remote-program (if (file-name-absolute-p program)
                                 program
                               ;; Search for program in remote PATH
                               (or (tramp-rpc--find-executable v program)
                                   program)))
             ;; Start the remote process
             (remote-pid (tramp-rpc--start-remote-process
                          v remote-program program-args localname))
             ;; Create local relay process using pipe
             (local-process (make-pipe-process
                             :name (or name "tramp-rpc-async")
                             :buffer buffer
                             :coding (or coding 'utf-8)
                             :noquery noquery
                             :filter filter
                             :sentinel sentinel)))
        
        ;; Store remote info as process properties
        (process-put local-process :tramp-rpc-vec v)
        (process-put local-process :tramp-rpc-pid remote-pid)
        (process-put local-process :tramp-rpc-command command)
        
        ;; Set up stderr buffer
        (let ((stderr-buffer (cond
                              ((bufferp stderr) stderr)
                              ((stringp stderr) (get-buffer-create stderr))
                              (t nil))))
          
          ;; Start polling timer
          (let ((timer (run-with-timer
                        tramp-rpc--process-poll-interval
                        tramp-rpc--process-poll-interval
                        #'tramp-rpc--poll-process
                        local-process)))
            
            ;; Store tracking info
            (puthash local-process
                     (list :vec v
                           :pid remote-pid
                           :timer timer
                           :stderr-buffer stderr-buffer)
                     tramp-rpc--async-processes)))
        
        local-process))))

(defun tramp-rpc--find-executable (vec program)
  "Find PROGRAM in the remote PATH on VEC.
Returns the absolute path or nil."
  (let ((dirs '("/usr/local/bin" "/usr/bin" "/bin"
                "/usr/local/sbin" "/usr/sbin" "/sbin")))
    (cl-loop for dir in dirs
             for path = (concat dir "/" program)
             when (condition-case nil
                      (tramp-rpc--call vec "file.executable" `((path . ,path)))
                    (error nil))
             return path)))

(defun tramp-rpc-handle-start-file-process (name buffer program &rest args)
  "Start async process on remote host.
NAME is the process name, BUFFER is the output buffer,
PROGRAM is the command to run, ARGS are its arguments."
  (tramp-rpc-handle-make-process
   :name name
   :buffer buffer
   :command (cons program args)))

;; ============================================================================
;; Advice for process operations
;; ============================================================================

(defun tramp-rpc--process-send-string-advice (orig-fun process string)
  "Advice for `process-send-string' to handle TRAMP-RPC processes."
  ;; process-send-string can receive a buffer/buffer-name instead of process
  (let ((proc (cond
               ((processp process) process)
               ((or (bufferp process) (stringp process))
                (get-buffer-process (get-buffer process)))
               (t nil))))
    (if (and proc
             (process-get proc :tramp-rpc-pid)
             (process-get proc :tramp-rpc-vec))
        (condition-case err
            (tramp-rpc--write-remote-process
             (process-get proc :tramp-rpc-vec)
             (process-get proc :tramp-rpc-pid)
             string)
          (error
           (message "tramp-rpc: Error writing to process: %s" err)))
      (funcall orig-fun process string))))

(defun tramp-rpc--process-send-eof-advice (orig-fun &optional process)
  "Advice for `process-send-eof' to handle TRAMP-RPC processes."
  (let ((proc (or process (get-buffer-process (current-buffer)))))
    (if-let ((pid (process-get proc :tramp-rpc-pid))
             (vec (process-get proc :tramp-rpc-vec)))
        (condition-case err
            (tramp-rpc--close-remote-stdin vec pid)
          (error
           (message "tramp-rpc: Error closing stdin: %s" err)))
      (funcall orig-fun process))))

(defun tramp-rpc--signal-process-advice (orig-fun process sigcode &optional remote)
  "Advice for `signal-process' to handle TRAMP-RPC processes."
  (if-let ((pid (and (processp process)
                     (process-get process :tramp-rpc-pid)))
           (vec (process-get process :tramp-rpc-vec)))
      (condition-case err
          (progn
            (tramp-rpc--kill-remote-process vec pid sigcode)
            0) ; Return 0 for success
        (error
         (message "tramp-rpc: Error signaling process: %s" err)
         -1))
    (funcall orig-fun process sigcode remote)))

(defun tramp-rpc--process-status-advice (orig-fun process)
  "Advice for `process-status' to handle TRAMP-RPC processes."
  (if (process-get process :tramp-rpc-pid)
      (cond
       ((process-get process :tramp-rpc-exited) 'exit)
       ;; Use orig-fun to check live status, not process-live-p (which would recurse)
       ((memq (funcall orig-fun process) '(run open listen connect)) 'run)
       (t 'exit))
    (funcall orig-fun process)))

(defun tramp-rpc--process-exit-status-advice (orig-fun process)
  "Advice for `process-exit-status' to handle TRAMP-RPC processes."
  (if (process-get process :tramp-rpc-pid)
      (or (process-get process :tramp-rpc-exit-code) 0)
    (funcall orig-fun process)))

;; Install advice
(advice-add 'process-send-string :around #'tramp-rpc--process-send-string-advice)
(advice-add 'process-send-eof :around #'tramp-rpc--process-send-eof-advice)
(advice-add 'signal-process :around #'tramp-rpc--signal-process-advice)
(advice-add 'process-status :around #'tramp-rpc--process-status-advice)
(advice-add 'process-exit-status :around #'tramp-rpc--process-exit-status-advice)

;; ============================================================================
;; VC integration advice
;; ============================================================================

;; VC backends like vc-git-state use process-file internally, but they don't
;; set default-directory to the remote file's directory. This means process-file
;; runs locally instead of going through our tramp handler. We fix this by
;; advising vc-call-backend to set default-directory when the file is remote.

(defun tramp-rpc--vc-call-backend-advice (orig-fun backend op file &rest args)
  "Advice for `vc-call-backend' to handle TRAMP files correctly.
When OP is an operation that takes a FILE argument and FILE is a TRAMP path,
ensure `default-directory' is set to the file's directory so that process-file
calls are routed through the TRAMP handler."
  (if (and file
           (stringp file)
           (tramp-tramp-file-p file)
           ;; Operations that take a file and may call process-file
           (memq op '(state state-heuristic dir-status-files
                      working-revision previous-revision next-revision
                      responsible-p)))
      (let ((default-directory (file-name-directory file)))
        (apply orig-fun backend op file args))
    (apply orig-fun backend op file args)))

(advice-add 'vc-call-backend :around #'tramp-rpc--vc-call-backend-advice)

;; ============================================================================
;; File name handler registration
;; ============================================================================

(defconst tramp-rpc-file-name-handler-alist
  '(;; File attributes
    (file-exists-p . tramp-rpc-handle-file-exists-p)
    (file-readable-p . tramp-rpc-handle-file-readable-p)
    (file-writable-p . tramp-rpc-handle-file-writable-p)
    (file-executable-p . tramp-rpc-handle-file-executable-p)
    (file-directory-p . tramp-rpc-handle-file-directory-p)
    (file-regular-p . tramp-rpc-handle-file-regular-p)
    (file-symlink-p . tramp-rpc-handle-file-symlink-p)
    (file-truename . tramp-rpc-handle-file-truename)
    (file-attributes . tramp-rpc-handle-file-attributes)
    (file-modes . tramp-rpc-handle-file-modes)
    (set-file-modes . tramp-rpc-handle-set-file-modes)
    (set-file-times . tramp-rpc-handle-set-file-times)
    (file-newer-than-file-p . tramp-rpc-handle-file-newer-than-file-p)

    ;; Directory operations
    (directory-files . tramp-rpc-handle-directory-files)
    (directory-files-and-attributes . tramp-rpc-handle-directory-files-and-attributes)
    (file-name-all-completions . tramp-rpc-handle-file-name-all-completions)
    (make-directory . tramp-rpc-handle-make-directory)
    (delete-directory . tramp-rpc-handle-delete-directory)

    ;; File I/O
    (insert-file-contents . tramp-rpc-handle-insert-file-contents)
    (write-region . tramp-rpc-handle-write-region)
    (copy-file . tramp-rpc-handle-copy-file)
    (rename-file . tramp-rpc-handle-rename-file)
    (delete-file . tramp-rpc-handle-delete-file)
    (make-symbolic-link . tramp-rpc-handle-make-symbolic-link)

    ;; Process operations
    (process-file . tramp-rpc-handle-process-file)
    (shell-command . tramp-rpc-handle-shell-command)

    ;; Path operations
    (expand-file-name . tramp-rpc-handle-expand-file-name)

    ;; Fall back to tramp-sh for these
    (access-file . tramp-handle-access-file)
    (add-name-to-file . tramp-handle-add-name-to-file)
    (byte-compiler-base-file-name . ignore)
    (diff-latest-backup-file . ignore)
    (directory-file-name . tramp-handle-directory-file-name)
    (dired-uncache . tramp-handle-dired-uncache)
    (exec-path . tramp-rpc-handle-exec-path)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . ignore)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-rpc-handle-file-local-copy)
    (file-locked-p . tramp-handle-file-locked-p)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    (file-notify-add-watch . tramp-handle-file-notify-add-watch)
    (file-notify-rm-watch . tramp-handle-file-notify-rm-watch)
    (file-notify-valid-p . tramp-handle-file-notify-valid-p)
    (file-ownership-preserved-p . ignore)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . ignore)
    (file-system-info . ignore)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    (insert-directory . tramp-rpc-handle-insert-directory)
    (load . tramp-handle-load)
    (lock-file . tramp-handle-lock-file)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory-internal . ignore)
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . tramp-rpc-handle-make-process)
    (set-file-acl . ignore)
    (set-file-selinux-context . ignore)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (start-file-process . tramp-rpc-handle-start-file-process)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (tramp-get-home-directory . tramp-rpc-handle-get-home-directory)
    (tramp-get-remote-gid . tramp-rpc-handle-get-remote-gid)
    (tramp-get-remote-groups . ignore)
    (tramp-get-remote-uid . tramp-rpc-handle-get-remote-uid)
    (tramp-set-file-uid-gid . ignore)
    (unhandled-file-name-directory . ignore)
    (unlock-file . tramp-handle-unlock-file)
    (vc-registered . tramp-rpc-handle-vc-registered)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . tramp-rpc-handle-write-region))
  "Alist of handler functions for TRAMP-RPC method.")

(defun tramp-rpc-file-name-handler (operation &rest args)
  "Invoke TRAMP-RPC file name handler for OPERATION with ARGS."
  (if-let ((handler (assq operation tramp-rpc-file-name-handler-alist)))
      (save-match-data (apply (cdr handler) args))
    (tramp-run-real-handler operation args)))

;; ============================================================================
;; Method registration
;; ============================================================================

;;;###autoload
(tramp-register-foreign-file-name-handler
 #'tramp-rpc-file-name-p #'tramp-rpc-file-name-handler)

(defun tramp-rpc-file-name-p (vec-or-filename)
  "Check if VEC-OR-FILENAME is handled by TRAMP-RPC.
VEC-OR-FILENAME can be either a tramp-file-name struct or a filename string."
  (let ((method (cond
                 ((tramp-file-name-p vec-or-filename)
                  (tramp-file-name-method vec-or-filename))
                 ((stringp vec-or-filename)
                  (and (tramp-tramp-file-p vec-or-filename)
                       (tramp-file-name-method (tramp-dissect-file-name vec-or-filename))))
                 (t nil))))
    (string= method tramp-rpc-method)))

;;;###autoload
(add-to-list 'tramp-methods
             `(,tramp-rpc-method
               ;; Minimal method entry - no shell setup needed
               ;; The foreign file name handler handles everything
               ))

;; ============================================================================
;; Unload support
;; ============================================================================

(defun tramp-rpc-unload-function ()
  "Unload function for tramp-rpc.
Removes advice and cleans up async processes."
  ;; Remove advice
  (advice-remove 'process-send-string #'tramp-rpc--process-send-string-advice)
  (advice-remove 'process-send-eof #'tramp-rpc--process-send-eof-advice)
  (advice-remove 'signal-process #'tramp-rpc--signal-process-advice)
  (advice-remove 'process-status #'tramp-rpc--process-status-advice)
  (advice-remove 'process-exit-status #'tramp-rpc--process-exit-status-advice)
  (advice-remove 'vc-call-backend #'tramp-rpc--vc-call-backend-advice)
  ;; Clean up all async processes
  (tramp-rpc--cleanup-async-processes)
  ;; Return nil to allow normal unload to proceed
  nil)

(provide 'tramp-rpc)
;;; tramp-rpc.el ends here
