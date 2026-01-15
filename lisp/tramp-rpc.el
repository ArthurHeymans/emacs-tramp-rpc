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
;; LIMITATIONS:
;; - Async processes (make-process, start-file-process) are not supported.
;;   Use `process-file' for synchronous process execution instead.
;; - Version control integration (vc-mode) will not work because it requires
;;   async processes.
;;
;; RECOMMENDED CONFIGURATION:
;; To avoid errors from VC-related packages in dired, add to your init.el:
;;   (setq diff-hl-disable-on-remote t)  ; Disable diff-hl for remote files
;;   (setq vc-ignore-dir-regexp          ; Tell VC to ignore tramp files
;;         (format "%s\\|%s"
;;                 vc-ignore-dir-regexp
;;                 tramp-file-name-regexp))

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
  (let ((conn (tramp-rpc--get-connection vec)))
    (when conn
      (let ((process (plist-get conn :process)))
        (when (process-live-p process)
          (delete-process process)))
      (tramp-rpc--remove-connection vec))))

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
          (accept-process-output process 0.1)
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
         (atime (seconds-to-time (alist-get 'atime stat)))
         (mtime (seconds-to-time (alist-get 'mtime stat)))
         (ctime (seconds-to-time (alist-get 'ctime stat)))
         (size (alist-get 'size stat))
         (mode (tramp-rpc--mode-to-string (alist-get 'mode stat) type-str))
         (inode (alist-get 'inode stat))
         (dev (alist-get 'dev stat)))
    ;; Return in file-attributes format
    (list type nlinks
          (if (eq id-format 'string) (number-to-string uid) uid)
          (if (eq id-format 'string) (number-to-string gid) gid)
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
  (when trash
    (error "Trash not supported for TRAMP-RPC"))
  (with-parsed-tramp-file-name directory nil
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
            (insert (format "  %s %3d %5d %5d %8d %s %s"
                            mode-str nlinks uid gid size time-str name))
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
  (when trash
    (error "Trash not supported for TRAMP-RPC"))
  (with-parsed-tramp-file-name filename nil
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

(defun tramp-rpc-handle-make-process (&rest args)
  "Signal that async processes are not supported for TRAMP-RPC.
ARGS are ignored."
  (signal 'file-error
          (list "Async processes not supported"
                "TRAMP-RPC does not support async processes; use process-file instead")))

(defun tramp-rpc-handle-start-file-process (name buffer program &rest args)
  "Signal that async processes are not supported for TRAMP-RPC.
NAME, BUFFER, PROGRAM and ARGS are ignored."
  (signal 'file-error
          (list "Async processes not supported"
                "TRAMP-RPC does not support async processes; use process-file instead")))

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
    (vc-registered . ignore)
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

(provide 'tramp-rpc)
;;; tramp-rpc.el ends here
