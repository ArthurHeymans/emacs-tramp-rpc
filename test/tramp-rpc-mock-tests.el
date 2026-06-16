;;; tramp-rpc-mock-tests.el --- Mock tests for TRAMP RPC (CI-compatible)  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>

;; This file is part of tramp-rpc.

;;; Commentary:

;; This file provides mock tests that can run in CI without SSH access.
;; It tests the RPC server directly via a local pipe connection.
;;
;; These tests focus on:
;; - Protocol correctness (MessagePack encoding/decoding)
;; - Server response handling
;; - Error handling
;;
;; Run with:
;;   emacs -Q --batch -l test/tramp-rpc-mock-tests.el -f tramp-rpc-mock-test-all

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Compute project root at load time
(defvar tramp-rpc-mock-test--project-root
  (expand-file-name "../" (file-name-directory
                           (or load-file-name buffer-file-name
                               (expand-file-name "test/tramp-rpc-mock-tests.el"))))
  "Project root directory, computed at load time.")

;; Load tramp-rpc modules
(let ((lisp-dir (expand-file-name "lisp" tramp-rpc-mock-test--project-root)))
  (add-to-list 'load-path lisp-dir))

;; Install msgpack from MELPA if not available
(defvar tramp-rpc-mock-test--msgpack-available
  (or (require 'msgpack nil t)
      ;; Try to install from MELPA
      (condition-case err
          (progn
            (require 'package)
            (unless (assoc 'msgpack package-alist)
              ;; Add MELPA if not present
              (add-to-list 'package-archives
                           '("melpa" . "https://melpa.org/packages/") t)
              (package-initialize)
              (unless package-archive-contents
                (package-refresh-contents))
              (package-install 'msgpack))
            (require 'msgpack)
            t)
        (error
         (message "Could not install msgpack: %s" err)
         nil)))
  "Non-nil if msgpack.el is available.")

(when tramp-rpc-mock-test--msgpack-available
  (require 'tramp-rpc-protocol))

;;; ============================================================================
;;; Protocol Tests (No server required)
;;; ============================================================================

(ert-deftest tramp-rpc-mock-test-protocol-encode-request ()
  "Test MessagePack-RPC request encoding."
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (let* ((result (tramp-rpc-protocol-encode-request-with-id "file.stat" '((path . "/test"))))
         (id (car result))
         (bytes (cdr result)))
    ;; Should be a unibyte string with length prefix
    (should (stringp bytes))
    (should (not (multibyte-string-p bytes)))
    (should (>= (length bytes) 4))
    ;; Read length prefix
    (let* ((len (msgpack-bytes-to-unsigned (substring bytes 0 4)))
           (payload (substring bytes 4))
           ;; Decode the MessagePack payload
           (msgpack-map-type 'alist)
           (msgpack-key-type 'symbol)
           (parsed (msgpack-read-from-string payload)))
      ;; Length should match payload
      (should (= len (length payload)))
      ;; Check structure
      (should (assoc 'version parsed))
      (should (equal (cdr (assoc 'version parsed)) "2.0"))
      (should (assoc 'method parsed))
      (should (equal (cdr (assoc 'method parsed)) "file.stat"))
      (should (assoc 'params parsed))
      (should (equal (cdr (assoc 'path (cdr (assoc 'params parsed)))) "/test"))
      (should (assoc 'id parsed))
      ;; ID should match returned ID
      (should (equal (cdr (assoc 'id parsed)) id)))))

(ert-deftest tramp-rpc-mock-test-protocol-decode-success ()
  "Test MessagePack-RPC success response decoding."
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (let* ((response-data '((version . "2.0") (id . 1) (result . ((exists . t)))))
         (response-bytes (msgpack-encode response-data)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert response-bytes)
      (let ((response (tramp-rpc-protocol-decode-response (current-buffer) (point-min))))
        (should (plist-get response :id))
        (should (equal (plist-get response :id) 1))
        (should (plist-get response :result))
        (should-not (tramp-rpc-protocol-error-p response))))))

(ert-deftest tramp-rpc-mock-test-protocol-decode-error ()
  "Test MessagePack-RPC error response decoding."
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (let* ((response-data '((version . "2.0")
                          (id . 1)
                          (error . ((code . -32001) (message . "File not found")))))
         (response-bytes (msgpack-encode response-data)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert response-bytes)
      (let ((response (tramp-rpc-protocol-decode-response (current-buffer) (point-min))))
        (should (tramp-rpc-protocol-error-p response))
        (should (= (tramp-rpc-protocol-error-code response) -32001))
        (should (equal (tramp-rpc-protocol-error-message response) "File not found"))))))

(ert-deftest tramp-rpc-mock-test-protocol-batch-encode ()
  "Test MessagePack-RPC batch request encoding."
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (let* ((requests '(("file.stat" . ((path . "/a")))
                     ("file.stat" . ((path . "/b")))))
         (result (tramp-rpc-protocol-encode-batch-request-with-id requests))
         (id (car result))
         (bytes (cdr result)))
    ;; Skip length prefix and decode
    (let* ((payload (substring bytes 4))
           (msgpack-map-type 'alist)
           (msgpack-key-type 'symbol)
           (msgpack-array-type 'list)
           (parsed (msgpack-read-from-string payload)))
      ;; Should be a single request with batch method
      (should (assoc 'method parsed))
      (should (equal (cdr (assoc 'method parsed)) "batch"))
      (should (assoc 'params parsed))
      (let ((params (cdr (assoc 'params parsed))))
        (should (assoc 'requests params))
        (let ((reqs (cdr (assoc 'requests params))))
          (should (= (length reqs) 2)))))))

(ert-deftest tramp-rpc-mock-test-protocol-batch-decode ()
  "Test MessagePack-RPC batch response decoding."
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (let* ((response-plist '(:id 1
                           :result ((results . (((result . t))
                                                ((error (code . -32001)
                                                        (message . "Error"))))))))
         (decoded (tramp-rpc-protocol-decode-batch-response response-plist)))
    (should (listp decoded))
    (should (= (length decoded) 2))
    ;; First result is success
    (should (eq (car decoded) t))
    ;; Second is error
    (should (plist-get (cadr decoded) :error))))

(ert-deftest tramp-rpc-mock-test-protocol-length-framing ()
  "Test length-prefixed framing functions."
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (let* ((test-data '((foo . "bar")))
         (payload (msgpack-encode test-data))
         (framed (tramp-rpc-protocol--length-prefix payload)))
    ;; Length should be encoded in first 4 bytes
    (should (= (length framed) (+ 4 (length payload))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert framed)
      (set-marker (mark-marker) (point-min))
      (should (= (tramp-rpc-protocol-read-length (current-buffer)) (length payload))))
    ;; Try reading a complete message
    (let* ((response '((version . "2.0") (id . 42) (result . t)))
           (response-payload (msgpack-encode response))
           (response-framed (tramp-rpc-protocol--length-prefix response-payload)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert response-framed)
        (set-marker (mark-marker) (point-min))
        (let ((read-result (tramp-rpc-protocol-try-read-message (current-buffer))))
          (should read-result)
          (should (= (plist-get read-result :id) 42)))))))

(ert-deftest tramp-rpc-mock-test-protocol-incomplete-message ()
  "Test handling of incomplete messages."
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (let* ((response '((version . "2.0") (id . 1) (result . t)))
         (payload (msgpack-encode response))
         (framed (tramp-rpc-protocol--length-prefix payload)))
    ;; Truncate the message - too short for length header
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (substring framed 0 3))
      (set-marker (mark-marker) (point-min))
      (should-not (tramp-rpc-protocol-try-read-message (current-buffer))))
    ;; Truncate the message - has length header but incomplete payload
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (substring framed 0 5))
      (set-marker (mark-marker) (point-min))
      (should-not (tramp-rpc-protocol-try-read-message (current-buffer))))))

;;; ============================================================================
;;; MessagePack-RPC ID Generation Tests
;;; ============================================================================

(ert-deftest tramp-rpc-mock-test-protocol-id-uniqueness ()
  "Test that request IDs are unique."
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (let ((ids (make-hash-table :test 'equal)))
    (dotimes (_ 100)
      (let* ((result (tramp-rpc-protocol-encode-request-with-id "test" nil))
             (id (car result)))
        (should-not (gethash id ids))
        (puthash id t ids)))))

;;; ============================================================================
;;; Mode String Conversion Tests
;;; ============================================================================

(ert-deftest tramp-rpc-mock-test-mode-to-string ()
  "Test mode integer to string conversion using `tramp-file-mode-from-int'.
The server sends the full st_mode value including file type bits."
  ;; Regular file with 644 permissions (S_IFREG = #o100000)
  (let ((mode-str (tramp-file-mode-from-int (logior #o100000 #o644))))
    (should (equal mode-str "-rw-r--r--")))
  ;; Directory with 755 permissions (S_IFDIR = #o040000)
  (let ((mode-str (tramp-file-mode-from-int (logior #o040000 #o755))))
    (should (equal mode-str "drwxr-xr-x")))
  ;; Symlink (S_IFLNK = #o120000)
  (let ((mode-str (tramp-file-mode-from-int (logior #o120000 #o777))))
    (should (string-prefix-p "l" mode-str))))

;;; ============================================================================
;;; File Attributes Conversion Tests
;;; ============================================================================

(ert-deftest tramp-rpc-mock-test-convert-file-attributes ()
  "Test conversion of stat result to Emacs attributes."
  (when (fboundp 'tramp-rpc--convert-file-attributes)
    (let* ((stat-result `((type . "file")
                          (size . 1234)
                          (mode . ,(logior #o100000 #o644))  ; S_IFREG | 0644
                          (nlinks . 1)
                          (uid . 1000)
                          (gid . 1000)
                          (atime . 1700000000)
                          (mtime . 1700000001)
                          (ctime . 1700000002)
                          (inode . 12345)
                          (dev . 1)))
           (attrs (tramp-rpc--convert-file-attributes stat-result 'integer)))
      ;; Type should be nil for regular file
      (should (null (file-attribute-type attrs)))
      ;; Size
      (should (= (file-attribute-size attrs) 1234))
      ;; UIDs
      (should (= (file-attribute-user-id attrs) 1000))
      (should (= (file-attribute-group-id attrs) 1000))
      ;; Link count
      (should (= (file-attribute-link-number attrs) 1)))))

;;; ============================================================================
;;; Local Server Tests (runs actual server)
;;; ============================================================================

(defvar tramp-rpc-mock-test-server-process nil
  "Process for the local test server.")

(defvar tramp-rpc-mock-test-server-buffer nil
  "Buffer for server output.")

(defvar tramp-rpc-mock-test-temp-dir nil
  "Temporary directory for tests.")

(defun tramp-rpc-mock-test--find-server ()
  "Find the RPC server executable or Python script."
  (let* ((rust-binary (expand-file-name "target/release/tramp-rpc-server"
                                         tramp-rpc-mock-test--project-root))
         (rust-binary-server (expand-file-name "server/target/release/tramp-rpc-server"
                                                tramp-rpc-mock-test--project-root))
         ;; Cross-compiled binaries (CI uses target triple in path)
         (rust-binary-musl (expand-file-name "target/x86_64-unknown-linux-musl/release/tramp-rpc-server"
                                              tramp-rpc-mock-test--project-root))
         (rust-debug (expand-file-name "target/debug/tramp-rpc-server"
                                        tramp-rpc-mock-test--project-root))
         (rust-debug-server (expand-file-name "server/target/debug/tramp-rpc-server"
                                               tramp-rpc-mock-test--project-root)))
    (cond
     ((file-executable-p rust-binary) rust-binary)
     ((file-executable-p rust-binary-server) rust-binary-server)
     ((file-executable-p rust-binary-musl) rust-binary-musl)
     ((file-executable-p rust-debug) rust-debug)
     ((file-executable-p rust-debug-server) rust-debug-server)
     (t nil))))

(defun tramp-rpc-mock-test--start-server ()
  "Start a local RPC server for testing."
  (let ((server (tramp-rpc-mock-test--find-server)))
    (unless server
      (error "No RPC server found. Build with 'cargo build --release'"))
    (setq tramp-rpc-mock-test-temp-dir (make-temp-file "tramp-rpc-test" t))
    (setq tramp-rpc-mock-test-server-buffer (generate-new-buffer "*tramp-rpc-test-server*"))
    ;; Set buffer to unibyte for binary protocol and init mark for framing
    (with-current-buffer tramp-rpc-mock-test-server-buffer
      (set-buffer-multibyte nil)
      (set-marker (mark-marker) (point-min)))
    (setq tramp-rpc-mock-test-server-process
          (let ((process-connection-type nil))  ; Use pipes
            (start-process "test-server" tramp-rpc-mock-test-server-buffer server)))
    (set-process-query-on-exit-flag tramp-rpc-mock-test-server-process nil)
    ;; Use binary coding for MessagePack protocol
    (set-process-coding-system tramp-rpc-mock-test-server-process 'binary 'binary)
    ;; Use an explicit filter to append output with regular `insert'.
    ;; The default process filter uses `insert-before-markers' which
    ;; moves ALL markers (including mark-marker) past the inserted text,
    ;; breaking the mark-based framing used by the protocol functions.
    (set-process-filter
     tramp-rpc-mock-test-server-process
     (lambda (process output)
       (when (buffer-live-p (process-buffer process))
         (with-current-buffer (process-buffer process)
           (goto-char (point-max))
           (insert output)))))
    ;; Wait for server to be ready
    (sleep-for 0.1)
    tramp-rpc-mock-test-server-process))

(defun tramp-rpc-mock-test--stop-server ()
  "Stop the local RPC server."
  (when (and tramp-rpc-mock-test-server-process
             (process-live-p tramp-rpc-mock-test-server-process))
    (delete-process tramp-rpc-mock-test-server-process))
  (when (buffer-live-p tramp-rpc-mock-test-server-buffer)
    (kill-buffer tramp-rpc-mock-test-server-buffer))
  (when (and tramp-rpc-mock-test-temp-dir
             (file-directory-p tramp-rpc-mock-test-temp-dir))
    (delete-directory tramp-rpc-mock-test-temp-dir t))
  (setq tramp-rpc-mock-test-server-process nil
        tramp-rpc-mock-test-server-buffer nil
        tramp-rpc-mock-test-temp-dir nil))

(defun tramp-rpc-mock-test--rpc-call (method params)
  "Send an RPC call to the local test server.
Returns the result or signals an error."
  (unless (and tramp-rpc-mock-test-server-process
               (process-live-p tramp-rpc-mock-test-server-process))
    (error "Server not running"))
  (let* ((id-and-request (tramp-rpc-protocol-encode-request-with-id method params))
         (expected-id (car id-and-request))
         (request (cdr id-and-request)))
    ;; Send request (binary with length prefix, no newline)
    (process-send-string tramp-rpc-mock-test-server-process request)
    ;; Read response using length-prefixed framing
    (with-current-buffer tramp-rpc-mock-test-server-buffer
      (let ((timeout 5.0)
            response)
        (while (and (not response) (> timeout 0))
          (accept-process-output tramp-rpc-mock-test-server-process 0.1)
          ;; Try to read a complete message
          (let ((result (tramp-rpc-protocol-try-read-message (current-buffer))))
            (when result
              (setq response result)
              ;; Remove consumed data
              (delete-region (point-min) (mark-marker))
              (set-marker (mark-marker) (point-min))))
          (cl-decf timeout 0.1))
        (unless response
          (error "Timeout waiting for RPC response"))
        (if (tramp-rpc-protocol-error-p response)
            (list :error (tramp-rpc-protocol-error-message response))
          (plist-get response :result))))))

;;; Server tests (require server to be available)

(ert-deftest tramp-rpc-mock-test-server-system-info ()
  "Test system.info RPC call."
  :tags '(:server)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let ((result (tramp-rpc-mock-test--rpc-call "system.info" nil)))
          (should result)
          (should-not (plist-get result :error))
          ;; Check expected fields
          (should (assoc 'uid result))
          (should (assoc 'gid result))
          (should (assoc 'home result))
          ;; shell field should be present and be a string
          (should (assoc 'shell result))
          (let ((shell (alist-get 'shell result)))
            (when shell
              (should (stringp shell))
              (should (string-prefix-p "/" shell))))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-file-operations ()
  "Test basic file operations via RPC."
  :tags '(:server)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let ((test-file (expand-file-name "test.txt" tramp-rpc-mock-test-temp-dir)))
          ;; File shouldn't exist yet (stat returns nil)
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "file.stat" `((path . ,(encode-coding-string test-file 'utf-8))))))
            (should (not result)))

          ;; Write a file - content is now raw binary, not base64
          (tramp-rpc-mock-test--rpc-call
           "file.write" `((path . ,(encode-coding-string test-file 'utf-8))
                          (content . "hello world")
                          (append . :msgpack-false)))

          ;; File should exist now (stat returns attributes)
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "file.stat" `((path . ,(encode-coding-string test-file 'utf-8))))))
            (should result))

          ;; Read the file - content comes back as raw binary
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "file.read" `((path . ,(encode-coding-string test-file 'utf-8))))))
            (should result)
            (let ((content (alist-get 'content result)))
              (should (equal content "hello world"))))

          ;; Get file stats
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "file.stat" `((path . ,(encode-coding-string test-file 'utf-8))))))
            (should result)
            (should (equal (alist-get 'type result) "file"))
            (should (= (alist-get 'size result) 11)))  ; "hello world" = 11 bytes

          ;; Delete the file
          (tramp-rpc-mock-test--rpc-call "file.delete"
                                          `((path . ,(encode-coding-string test-file 'utf-8))))
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "file.stat" `((path . ,(encode-coding-string test-file 'utf-8))))))
            (should (not result)))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-directory-operations ()
  "Test directory operations via RPC."
  :tags '(:server)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let ((test-dir (expand-file-name "subdir" tramp-rpc-mock-test-temp-dir)))
          ;; Create directory
          (tramp-rpc-mock-test--rpc-call
           "dir.create" `((path . ,(encode-coding-string test-dir 'utf-8))
                          (parents . :msgpack-false)))

          ;; Check it exists
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "file.stat" `((path . ,(encode-coding-string test-dir 'utf-8))))))
            (should result)
            (should (equal (alist-get 'type result) "directory")))

          ;; Create files in directory - content is raw binary
          (tramp-rpc-mock-test--rpc-call
           "file.write" `((path . ,(encode-coding-string (expand-file-name "file1.txt" test-dir) 'utf-8))
                          (content . "a")
                          (append . :msgpack-false)))
          (tramp-rpc-mock-test--rpc-call
           "file.write" `((path . ,(encode-coding-string (expand-file-name "file2.txt" test-dir) 'utf-8))
                          (content . "b")
                          (append . :msgpack-false)))

          ;; List directory
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "dir.list" `((path . ,(encode-coding-string test-dir 'utf-8))
                                      (include_attrs . :msgpack-false)
                                      (include_hidden . t)))))
            (should result)
            (let ((names (mapcar (lambda (e) (alist-get 'name e)) result)))
              (should (member "file1.txt" names))
              (should (member "file2.txt" names))))

          ;; Remove directory recursively
          (tramp-rpc-mock-test--rpc-call
           "dir.remove" `((path . ,(encode-coding-string test-dir 'utf-8))
                          (recursive . t)))

          ;; Should be gone (stat returns nil)
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "file.stat" `((path . ,(encode-coding-string test-dir 'utf-8))))))
            (should (not result)))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-highlevel-locate-dominating-file ()
  "Test high-level locate-dominating-file RPC helper."
  :tags '(:server)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let* ((root (expand-file-name "highlevel-root" tramp-rpc-mock-test-temp-dir))
               (deep (expand-file-name "a/b/c/d" root))
               (file (expand-file-name "file.txt" deep)))
          (make-directory deep t)
          (make-directory (expand-file-name ".git" root) t)
          (with-temp-file file (insert "x"))
          (let* ((result (tramp-rpc-mock-test--rpc-call
                          "highlevel.locate_dominating_file_multi"
                          `((file . ,(encode-coding-string file 'utf-8))
                            (names . [".git" ".dir-locals.el"]))))
                 (first (car result)))
            (should (stringp first))
            (should (string-match-p "/highlevel-root/\\.git\\'" first)))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-highlevel-locate-dominating-file-preserves-symlink-path ()
  "Test locate-dominating-file keeps lexical symlink path."
  :tags '(:server)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (skip-unless (not (memq system-type '(windows-nt ms-dos))))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let* ((real-root (expand-file-name "highlevel-real-root" tramp-rpc-mock-test-temp-dir))
               (link-root (expand-file-name "highlevel-link-root" tramp-rpc-mock-test-temp-dir))
               (deep (expand-file-name "a/b/c/d" link-root))
               (file (expand-file-name "file.txt" deep)))
          (make-directory (expand-file-name "a/b/c/d" real-root) t)
          (make-directory (expand-file-name ".git" real-root) t)
          (ignore-errors (delete-file link-root))
          (make-symbolic-link real-root link-root)
          (with-temp-file file (insert "x"))
          (let* ((result (tramp-rpc-mock-test--rpc-call
                          "highlevel.locate_dominating_file_multi"
                          `((file . ,(encode-coding-string file 'utf-8))
                            (names . [".git"]))))
                 (first (car result)))
            (should (stringp first))
            (should (string-prefix-p link-root first))
            (should (string-match-p "/\\.git\\'" first)))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-highlevel-locate-dominating-file-depth-limit ()
  "Ensure dominating-file helper errors after 100 ancestor levels."
  :tags '(:server)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let* ((root (expand-file-name "highlevel-depth-limit" tramp-rpc-mock-test-temp-dir))
               (deep-rel (mapconcat (lambda (n) (format "d%03d" n))
                                    (number-sequence 1 110) "/"))
               (deep (expand-file-name deep-rel root))
               (file (expand-file-name "file.txt" deep)))
          (make-directory deep t)
          (make-directory (expand-file-name ".git" root) t)
          (with-temp-file file (insert "x"))
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "highlevel.locate_dominating_file_multi"
                         `((file . ,(encode-coding-string file 'utf-8))
                           (names . [".git"])))))
            (should (stringp (plist-get result :error)))
            (should (string-match-p
                     "Maximum ancestor traversal depth (100) exceeded"
                     (plist-get result :error))))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-highlevel-test-files-in-dir ()
  "Test high-level dir-locals file listing RPC helper."
  :tags '(:server)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let ((dir (expand-file-name "highlevel-locals" tramp-rpc-mock-test-temp-dir)))
          (make-directory dir t)
          (with-temp-file (expand-file-name ".dir-locals.el" dir) (insert "x"))
          (with-temp-file (expand-file-name ".dir-locals-2.el" dir) (insert "y"))
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "highlevel.test_files_in_dir"
                         `((directory . ,(encode-coding-string dir 'utf-8))
                           (names . [".dir-locals.el" ".dir-locals-2.el" "missing.el"])))))
            (should (= 2 (length result)))
            (should (seq-some (lambda (p) (string-match-p "\\.dir-locals\\.el\\'" p)) result))
            (should (seq-some (lambda (p) (string-match-p "\\.dir-locals-2\\.el\\'" p)) result)))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-highlevel-dir-locals-cache-update ()
  "Test high-level dir-locals cache update RPC helper."
  :tags '(:server)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let* ((root (expand-file-name "highlevel-cache" tramp-rpc-mock-test-temp-dir))
               (deep (expand-file-name "x/y/z" root))
               (file (expand-file-name "new-file.txt" deep)))
          (make-directory deep t)
          (with-temp-file (expand-file-name ".dir-locals.el" root) (insert "((nil . nil))"))
          (let* ((result (tramp-rpc-mock-test--rpc-call
                          "highlevel.dir_locals_find_file_cache_update"
                          `((file . ,(encode-coding-string file 'utf-8))
                            (names . [".dir-locals.el" ".dir-locals-2.el"])
                            (cache_dirs . [,(encode-coding-string root 'utf-8)]))))
                 (locals (alist-get 'locals result)))
            (should (alist-get 'file result))
            (should locals)
            (should (string-match-p "/highlevel-cache\\'" (alist-get 'dir locals)))
            (should (alist-get 'files locals)))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-highlevel-dir-locals-cache-update-preserves-symlink-path ()
  "Ensure dir-locals cache update keeps lexical symlink paths."
  :tags '(:server)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (skip-unless (not (memq system-type '(windows-nt ms-dos))))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let* ((real-root (expand-file-name "highlevel-cache-real" tramp-rpc-mock-test-temp-dir))
               (link-root (expand-file-name "highlevel-cache-link" tramp-rpc-mock-test-temp-dir))
               (deep (expand-file-name "a/b/c" link-root))
               (file (expand-file-name "new-file.txt" deep)))
          (make-directory (expand-file-name "a/b/c" real-root) t)
          (with-temp-file (expand-file-name ".dir-locals.el" real-root) (insert "((nil . nil))"))
          (ignore-errors (delete-file link-root))
          (make-symbolic-link real-root link-root)
          (let* ((result (tramp-rpc-mock-test--rpc-call
                          "highlevel.dir_locals_find_file_cache_update"
                          `((file . ,(encode-coding-string file 'utf-8))
                            (names . [".dir-locals.el"])
                            (cache_dirs . [,(encode-coding-string link-root 'utf-8)]))))
                 (locals (alist-get 'locals result))
                 (cache (alist-get 'cache result)))
            (should (alist-get 'file result))
            (should (string-match-p "/highlevel-cache-link/" (alist-get 'file result)))
            (should locals)
            (should (string-match-p "/highlevel-cache-link\\'" (alist-get 'dir locals)))
            (should cache)
            (should (string-match-p "/highlevel-cache-link\\'" (alist-get 'dir cache))))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-highlevel-dir-locals-cache-update-depth-limit ()
  "Ensure dir-locals cache helper errors after 100 ancestor levels."
  :tags '(:server)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let* ((root (expand-file-name "highlevel-cache-depth-limit" tramp-rpc-mock-test-temp-dir))
               (deep-rel (mapconcat (lambda (n) (format "d%03d" n))
                                    (number-sequence 1 110) "/"))
               (deep (expand-file-name deep-rel root))
               (file (expand-file-name "new-file.txt" deep)))
          (make-directory deep t)
          (with-temp-file (expand-file-name ".dir-locals.el" root) (insert "((nil . nil))"))
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "highlevel.dir_locals_find_file_cache_update"
                         `((file . ,(encode-coding-string file 'utf-8))
                           (names . [".dir-locals.el"])
                           (cache_dirs . [])))))
            (should (stringp (plist-get result :error)))
            (should (string-match-p
                     "Maximum ancestor traversal depth (100) exceeded"
                     (plist-get result :error))))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-process-run ()
  "Test process.run RPC call."
  :tags '(:server :process)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        ;; Run a simple command
        (let ((result (tramp-rpc-mock-test--rpc-call
                       "process.run" `((cmd . "echo")
                                       (args . ["hello" "world"])
                                       (cwd . "/tmp")))))
          (should result)
          (should (= (alist-get 'exit_code result) 0))
          ;; stdout is now raw binary
          (let ((stdout (alist-get 'stdout result)))
            (should (string-match-p "hello world" stdout)))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-process-signal-exit ()
  "Test process.run returns 128+signal for signal-killed processes.
This matches the behavior expected by `tramp-test28-process-file'."
  :tags '(:server :process)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        ;; Normal exit code
        (let ((result (tramp-rpc-mock-test--rpc-call
                       "process.run" `((cmd . "/bin/sh")
                                       (args . ["-c" "exit 42"])
                                       (cwd . "/tmp")))))
          (should result)
          (should (= (alist-get 'exit_code result) 42)))

        ;; SIGINT (signal 2) -> exit code 130
        (let ((result (tramp-rpc-mock-test--rpc-call
                       "process.run" `((cmd . "/bin/sh")
                                       (args . ["-c" "kill -2 $$"])
                                       (cwd . "/tmp")))))
          (should result)
          (should (= (alist-get 'exit_code result) (+ 128 2))))

        ;; SIGKILL (signal 9) -> exit code 137
        (let ((result (tramp-rpc-mock-test--rpc-call
                       "process.run" `((cmd . "/bin/sh")
                                       (args . ["-c" "kill -9 $$"])
                                       (cwd . "/tmp")))))
          (should result)
          (should (= (alist-get 'exit_code result) (+ 128 9))))

        ;; SIGTERM (signal 15) -> exit code 143
        (let ((result (tramp-rpc-mock-test--rpc-call
                       "process.run" `((cmd . "/bin/sh")
                                       (args . ["-c" "kill -15 $$"])
                                       (cwd . "/tmp")))))
          (should result)
          (should (= (alist-get 'exit_code result) (+ 128 15)))))
    (tramp-rpc-mock-test--stop-server)))

;;; ============================================================================
;;; Multi-Hop Tests (No server or SSH required)
;;; ============================================================================

;; Load tramp-rpc fully for multi-hop function testing
(defvar tramp-rpc-mock-test--tramp-rpc-loaded
  (condition-case err
      (progn
        (require 'tramp)
        (require 'tramp-rpc)
        t)
    (error
     (message "Could not load tramp-rpc: %s" err)
     nil))
  "Non-nil if tramp-rpc.el loaded successfully.")

(defun tramp-rpc-mock-test--sudo-helper-available-p ()
  "Return non-nil when the sudo path helpers needed by this test are available."
  (and (require 'tramp-cmds nil t)
       (fboundp 'tramp-file-name-with-sudo)))

(ert-deftest tramp-rpc-mock-test-file-notify-suppression-still-dispatches ()
  "fs.changed suppression skips cache work but still dispatches file notifications."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((vec (tramp-dissect-file-name "/rpc:mock:/tmp/"))
         (proc (make-process :name "tramp-rpc-fs-changed-test"
                             :buffer nil
                             :command '("cat")
                             :connection-type 'pipe
                             :noquery t))
         (status-clears 0)
         (invalidations nil)
         (dispatches nil)
         (tramp-rpc--suppress-fs-notifications t))
    (unwind-protect
        (progn
          (process-put proc :tramp-rpc-vec vec)
          (cl-letf (((symbol-function 'tramp-rpc-magit--clear-status-cache)
                     (lambda () (cl-incf status-clears)))
                    ((symbol-function 'tramp-rpc--invalidate-cache-for-path)
                     (lambda (path) (push path invalidations)))
                    ((symbol-function 'tramp-rpc--file-notify-dispatch)
                     (lambda (path) (push path dispatches))))
            (tramp-rpc--handle-notification
             proc "fs.changed" '((paths . ("/tmp/changed"))))
            (should (= status-clears 0))
            (should-not invalidations)
            (should (equal dispatches '("/rpc:mock:/tmp/changed")))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest tramp-rpc-mock-test-file-notify-cleanup-for-connection ()
  "Per-connection cleanup removes TRAMP-RPC and global file-notify descriptors."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (require 'filenotify)
  (let* ((tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (vec (tramp-dissect-file-name "/rpc:mock:/tmp/"))
         (other-vec (tramp-dissect-file-name "/rpc:other:/tmp/"))
         (watch-key (format "%s:%s" (tramp-rpc--connection-key-string vec) "/tmp/"))
         (other-key (format "%s:%s" (tramp-rpc--connection-key-string other-vec) "/tmp/"))
         (descriptor (cons 'tramp-rpc-file-notify 1001))
         (other-descriptor (cons 'tramp-rpc-file-notify 1002)))
    (unwind-protect
        (progn
          (puthash descriptor (list :watch-key watch-key :directory "/rpc:mock:/tmp/")
                   tramp-rpc--file-notify-descriptors)
          (puthash other-descriptor (list :watch-key other-key :directory "/rpc:other:/tmp/")
                   tramp-rpc--file-notify-descriptors)
          (puthash watch-key '(:count 1 :owned t) tramp-rpc--file-notify-watch-counts)
          (puthash other-key '(:count 1 :owned t) tramp-rpc--file-notify-watch-counts)
          (puthash descriptor 'dummy file-notify-descriptors)
          (puthash other-descriptor 'dummy file-notify-descriptors)
          (tramp-rpc--cleanup-file-notify-for-connection vec)
          (should-not (gethash descriptor tramp-rpc--file-notify-descriptors))
          (should-not (gethash watch-key tramp-rpc--file-notify-watch-counts))
          (should-not (gethash descriptor file-notify-descriptors))
          (should-not (tramp-rpc-handle-file-notify-valid-p descriptor))
          (should (gethash other-descriptor tramp-rpc--file-notify-descriptors))
          (should (gethash other-key tramp-rpc--file-notify-watch-counts))
          (should (gethash other-descriptor file-notify-descriptors)))
      (remhash descriptor file-notify-descriptors)
      (remhash other-descriptor file-notify-descriptors))))

(ert-deftest tramp-rpc-mock-test-file-notify-public-rm-and-valid-route ()
  "Public file-notify APIs route TRAMP-RPC descriptors to private state."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (require 'filenotify)
  (let* ((tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (tramp-rpc--watched-directories (make-hash-table :test 'equal))
         (directory "/rpc:mock:/tmp/repo/")
         (calls nil)
         descriptor)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method params)
                     (push (list method params) calls)
                     t))
                  ((symbol-function 'tramp-rpc-handle-file-directory-p)
                   (lambda (_filename) t)))
          (setq descriptor
                (file-notify-add-watch directory '(change) #'ignore))
          (should (gethash descriptor tramp-rpc--file-notify-descriptors))
          (should (gethash descriptor file-notify-descriptors))
          (should (file-notify-valid-p descriptor))
          (file-notify-rm-watch descriptor)
          (should-not (gethash descriptor tramp-rpc--file-notify-descriptors))
          (should-not (gethash descriptor file-notify-descriptors))
          (should (= (hash-table-count tramp-rpc--file-notify-watch-counts) 0))
          (should (equal (mapcar #'car (nreverse (copy-sequence calls)))
                         '("watch.add" "watch.remove"))))
      (when (and descriptor (boundp 'file-notify-descriptors))
        (remhash descriptor file-notify-descriptors)))))

(ert-deftest tramp-rpc-mock-test-file-notify-public-stale-descriptor-cleans-up ()
  "Public valid/rm clean TRAMP-RPC state when the global descriptor is gone."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (require 'filenotify)
  (let* ((tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (tramp-rpc--watched-directories (make-hash-table :test 'equal))
         (directory "/rpc:mock:/tmp/repo/")
         (calls nil)
         descriptor)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method params)
                     (push (list method params) calls)
                     t))
                  ((symbol-function 'tramp-rpc-handle-file-directory-p)
                   (lambda (_filename) t)))
          (setq descriptor
                (file-notify-add-watch directory '(change) #'ignore))
          (remhash descriptor file-notify-descriptors)
          (should-not (file-notify-valid-p descriptor))
          (should-not (gethash descriptor tramp-rpc--file-notify-descriptors))
          (should (= (hash-table-count tramp-rpc--file-notify-watch-counts) 0))
          (should (equal (mapcar #'car (nreverse (copy-sequence calls)))
                         '("watch.add" "watch.remove")))

          (setq calls nil
                descriptor (file-notify-add-watch directory '(change) #'ignore))
          (remhash descriptor file-notify-descriptors)
          (file-notify-rm-watch descriptor)
          (should-not (gethash descriptor tramp-rpc--file-notify-descriptors))
          (should (= (hash-table-count tramp-rpc--file-notify-watch-counts) 0))
          (should (equal (mapcar #'car (nreverse (copy-sequence calls)))
                         '("watch.add" "watch.remove"))))
      (when (and descriptor (boundp 'file-notify-descriptors))
        (remhash descriptor file-notify-descriptors)))))

(ert-deftest tramp-rpc-mock-test-file-notify-watch-upgrade-does-not-mask-recursive ()
  "A direct file-notify watch does not mask or remove a recursive watch."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (tramp-rpc--watched-directories (make-hash-table :test 'equal))
         (directory "/rpc:mock:/tmp/repo/")
         (vec (tramp-dissect-file-name directory))
         (watch-key (format "%s:%s" (tramp-rpc--connection-key-string vec) "/tmp/repo/"))
         (calls nil)
         descriptor)
    (cl-letf (((symbol-function 'tramp-rpc--call)
               (lambda (_vec method params)
                 (push (list method params) calls)
                 t)))
      (setq descriptor
            (tramp-rpc-handle-file-notify-add-watch directory '(change) #'ignore))
      (should (gethash descriptor tramp-rpc--file-notify-descriptors))
      (should-not (gethash watch-key tramp-rpc--watched-directories))
      (should (equal (mapcar #'car (nreverse (copy-sequence calls)))
                     '("watch.add")))
      (setq calls nil)
      (tramp-rpc-watch-directory directory t)
      (should (tramp-rpc--watch-entry-recursive-p
               (gethash watch-key tramp-rpc--watched-directories)))
      (should-not (plist-get (gethash watch-key tramp-rpc--file-notify-watch-counts)
                             :owned))
      (should (equal (mapcar #'car (nreverse (copy-sequence calls)))
                     '("watch.add")))
      (setq calls nil)
      (tramp-rpc-handle-file-notify-rm-watch descriptor)
      (should-not calls)
      (should (tramp-rpc--watch-entry-recursive-p
               (gethash watch-key tramp-rpc--watched-directories))))))

(ert-deftest tramp-rpc-mock-test-file-notify-unwatch-restores-direct-watch ()
  "Explicit unwatch restores a direct watch needed by file-notify."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (tramp-rpc--watched-directories (make-hash-table :test 'equal))
         (directory "/rpc:mock:/tmp/repo/")
         (vec (tramp-dissect-file-name directory))
         (watch-key (format "%s:%s" (tramp-rpc--connection-key-string vec) "/tmp/repo/"))
         (calls nil)
         descriptor)
    (cl-letf (((symbol-function 'tramp-rpc--call)
               (lambda (_vec method params)
                 (push (list method params) calls)
                 t))
              ((symbol-function 'file-truename)
               (lambda (filename) filename)))
      (tramp-rpc-watch-directory directory nil)
      (setq descriptor
            (tramp-rpc-handle-file-notify-add-watch directory '(change) #'ignore))
      (should (gethash descriptor tramp-rpc--file-notify-descriptors))
      (should-not (plist-get (gethash watch-key tramp-rpc--file-notify-watch-counts)
                             :owned))
      (tramp-rpc-unwatch-directory directory)
      (should-not (gethash watch-key tramp-rpc--watched-directories))
      (should (plist-get (gethash watch-key tramp-rpc--file-notify-watch-counts)
                         :owned))
      (should (equal (mapcar #'car (nreverse (copy-sequence calls)))
                     '("watch.add" "watch.remove" "watch.add"))))))

(ert-deftest tramp-rpc-mock-test-file-notify-watch-upgrade-failure-keeps-direct ()
  "A failed recursive upgrade does not remove a direct file-notify watch."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (tramp-rpc--watched-directories (make-hash-table :test 'equal))
         (directory "/rpc:mock:/tmp/repo/")
         (vec (tramp-dissect-file-name directory))
         (watch-key (format "%s:%s" (tramp-rpc--connection-key-string vec) "/tmp/repo/"))
         (calls nil)
         descriptor)
    (cl-letf (((symbol-function 'tramp-rpc--call)
               (lambda (_vec method params)
                 (push (list method params) calls)
                 (when (and (equal method "watch.add")
                            (eq (alist-get 'recursive params) t))
                   (error "recursive add failed"))
                 t)))
      (setq descriptor
            (tramp-rpc-handle-file-notify-add-watch directory '(change) #'ignore))
      (setq calls nil)
      (should-error (tramp-rpc-watch-directory directory t)
                    :type 'error)
      (should (gethash descriptor tramp-rpc--file-notify-descriptors))
      (should (plist-get (gethash watch-key tramp-rpc--file-notify-watch-counts)
                         :owned))
      (should-not (gethash watch-key tramp-rpc--watched-directories))
      (should (equal (mapcar #'car (nreverse (copy-sequence calls)))
                     '("watch.add"))))))

(ert-deftest tramp-rpc-mock-test-connection-stderr-drain-prevents-block ()
  "Test separated connection stderr is drained while waiting for stdout."
  :tags '(:connection)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (skip-unless (executable-find "python3"))
  (let* ((stdout-buffer (generate-new-buffer " *tramp-rpc-stderr-stdout*"))
         (stderr-buffer (generate-new-buffer " *tramp-rpc-stderr-stderr*"))
         (process
          (make-process
           :name "tramp-rpc-stderr-drain-test"
           :buffer stdout-buffer
           :command
           (list "python3" "-c"
                 (concat
                  "import sys; "
                  "sys.stderr.buffer.write(b'E' * 200000); "
                  "sys.stderr.flush(); "
                  "sys.stdout.buffer.write(b'DONE'); "
                  "sys.stdout.flush()"))
           :connection-type 'pipe
           :coding 'binary
           :noquery t
           :stderr stderr-buffer))
         (conn (list :process process
                     :buffer stdout-buffer
                     :stderr-buffer stderr-buffer)))
    (unwind-protect
        (let ((deadline (+ (float-time) 2.0)))
          (while (and (< (float-time) deadline)
                      (= (with-current-buffer stdout-buffer (buffer-size)) 0)
                      (process-live-p process))
            (tramp-rpc--drain-connection-stderr conn)
            (accept-process-output process 0.05 nil t)
            (tramp-rpc--drain-connection-stderr conn))
          (should (string-match-p
                   "DONE"
                   (with-current-buffer stdout-buffer (buffer-string))))
          (should (> (with-current-buffer stderr-buffer (buffer-size)) 100000)))
      (when (process-live-p process)
        (delete-process process))
      (ignore-errors
        (when-let* ((stderr-process (get-buffer-process stderr-buffer)))
          (delete-process stderr-process)))
      (kill-buffer stdout-buffer)
      (kill-buffer stderr-buffer))))

(ert-deftest tramp-rpc-mock-test-system-info-cache-shared ()
  "system.info is cached and shared by uid, gid, and home handlers."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name "/rpc:mockhost:/tmp"))
        (count 0))
    (tramp-flush-connection-properties vec)
    (cl-letf (((symbol-function 'tramp-rpc--call)
               (lambda (_vec method _params)
                 (should (equal method "system.info"))
                 (cl-incf count)
                 '((uid . 1234)
                   (gid . 5678)
                   (home . "/home/mock")
                   (user . "mock")
                   (os . "linux")
                   (shell . "/bin/zsh")))))
      (should (= (tramp-rpc-handle-get-remote-uid vec 'integer) 1234))
      (should (= (tramp-rpc-handle-get-remote-gid vec 'integer) 5678))
      (should (equal (tramp-rpc-handle-get-home-directory vec) "/home/mock"))
      (should (= count 1))
      (should (equal (tramp-get-connection-property vec "uname" nil) "Linux")))))

(ert-deftest tramp-rpc-mock-test-system-info-cache-seeding-reused ()
  "Seeded system.info properties are reused without another RPC."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name "/rpc:mock@mockhost:/tmp")))
    (tramp-flush-connection-properties vec)
    (tramp-rpc--cache-system-info
     vec '((uid . 1234)
           (gid . 5678)
           (home . "/home/mock")
           (user . "mock")
           (os . "linux")
           (shell . "/bin/zsh")))
    (cl-letf (((symbol-function 'tramp-rpc--call)
               (lambda (&rest _)
                 (ert-fail "system.info cache seed should avoid RPC calls"))))
      (should (= (tramp-rpc-handle-get-remote-uid vec 'integer) 1234))
      (should (= (tramp-rpc-handle-get-remote-gid vec 'integer) 5678))
      (should (equal (tramp-rpc-handle-get-home-directory vec) "/home/mock"))
      (should (equal (tramp-get-connection-property vec "uid-string" nil) "1234"))
      (should (equal (tramp-get-connection-property vec "gid-string" nil) "5678"))
      (should (equal (tramp-get-connection-property vec "~mock" nil) "/home/mock")))))

(ert-deftest tramp-rpc-mock-test-system-info-cold-connection-reuses-startup-cache ()
  "A cold system.info lookup reuses the cache seeded by connection setup."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name "/rpc:mock@mockhost:/tmp"))
        (ensure-count 0))
    (tramp-flush-connection-properties vec)
    (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
               (lambda (connection-vec)
                 (cl-incf ensure-count)
                 (tramp-rpc--cache-system-info
                  connection-vec '((uid . 1234)
                                   (gid . 5678)
                                   (home . "/home/mock")
                                   (user . "mock")
                                   (os . "linux")
                                   (shell . "/bin/zsh")))
                 '(:process mock)))
              ((symbol-function 'tramp-rpc--call)
               (lambda (&rest _)
                 (ert-fail "cold system.info lookup should reuse startup cache"))))
      (should (= (tramp-rpc-handle-get-remote-uid vec 'integer) 1234))
      (should (= ensure-count 1)))))

(ert-deftest tramp-rpc-mock-test-file-directory-p-caches-nil ()
  "`file-directory-p' caches negative RPC stat results."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((filename "/rpc:mockhost:/tmp/not-a-dir")
         (vec (tramp-dissect-file-name filename))
         (count 0))
    (tramp-flush-file-properties vec "/tmp/not-a-dir")
    (cl-letf (((symbol-function 'tramp-rpc--call-file-stat)
               (lambda (_vec _localname &optional _lstat)
                 (cl-incf count)
                 nil)))
      (should (equal (list (file-directory-p filename)
                           (file-directory-p filename))
                     '(nil nil)))
      (should (= count 1)))))

(ert-deftest tramp-rpc-mock-test-mkdir-parents-invalidates-prefix-caches ()
  "Parent mkdir invalidation clears stale caches for created prefixes."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((vec (tramp-dissect-file-name "/rpc:mockhost:/tmp/a/b/c"))
         (prefix-file "/rpc:mockhost:/tmp/a")
         (prefix-key (expand-file-name prefix-file))
         (missing (make-symbol "missing")))
    (tramp-set-file-property vec "/tmp/a" "file-directory-p" nil)
    (tramp-rpc--cache-put tramp-rpc--file-exists-cache prefix-key t)
    (tramp-rpc--cache-put tramp-rpc--file-truename-cache prefix-key "/tmp/a")
    (should-not (tramp-get-file-property vec "/tmp/a" "file-directory-p" missing))
    (should (gethash prefix-key tramp-rpc--file-exists-cache))
    (should (gethash prefix-key tramp-rpc--file-truename-cache))
    (tramp-rpc--invalidate-mkdir-caches
     vec "/rpc:mockhost:/tmp/a/b/c" "/tmp/a/b/c" t)
    (should (eq (tramp-get-file-property
                 vec "/tmp/a" "file-directory-p" missing)
                missing))
    (should-not (gethash prefix-key tramp-rpc--file-exists-cache))
    (should-not (gethash prefix-key tramp-rpc--file-truename-cache))))

(ert-deftest tramp-rpc-mock-test-set-file-modes-no-preflight ()
  "`set-file-modes' calls only file.set_modes on the success path."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((calls nil))
    (cl-letf (((symbol-function 'tramp-rpc--call)
               (lambda (_vec method _params)
                 (push method calls)
                 t)))
      (set-file-modes "/rpc:mockhost:/tmp/file" #o644)
      (should (equal (nreverse calls) '("file.set_modes"))))))

(ert-deftest tramp-rpc-mock-test-set-file-modes-no-preflight-missing ()
  "`set-file-modes' surfaces server-side missing-file errors directly."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((calls nil))
    (cl-letf (((symbol-function 'tramp-rpc--call)
               (lambda (_vec method _params)
                 (push method calls)
                 (signal 'file-missing '("RPC" "No such file" "/tmp/file")))))
      (should-error (set-file-modes "/rpc:mockhost:/tmp/file" #o644)
                    :type 'file-missing)
      (should (equal (nreverse calls) '("file.set_modes"))))))

(ert-deftest tramp-rpc-mock-test-set-file-times-no-preflight-missing ()
  "`set-file-times' surfaces server-side missing-file errors directly."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((calls nil))
    (cl-letf (((symbol-function 'tramp-rpc--call)
               (lambda (_vec method _params)
                 (push method calls)
                 (signal 'file-missing '("RPC" "No such file" "/tmp/file")))))
      (should-error (set-file-times "/rpc:mockhost:/tmp/file" (current-time))
                    :type 'file-missing)
      (should (equal (nreverse calls) '("file.set_times"))))))

(ert-deftest tramp-rpc-mock-test-multi-hop-advice ()
  "Test that tramp-multi-hop-p returns t for the rpc method."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (make-tramp-file-name :method "rpc" :host "target")))
    (should (tramp-multi-hop-p vec))))

(ert-deftest tramp-rpc-mock-test-multi-hop-advice-ssh-still-works ()
  "Test that tramp-multi-hop-p still returns t for ssh method."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (make-tramp-file-name :method "ssh" :host "target")))
    (should (tramp-multi-hop-p vec))))

(ert-deftest tramp-rpc-mock-test-multi-hop-dissect-single-hop ()
  "Test that TRAMP can dissect a single-hop rpc filename."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name "/rpc:gateway|rpc:user@target:/path")))
    (should (equal (tramp-file-name-method vec) "rpc"))
    (should (equal (tramp-file-name-user vec) "user"))
    (should (equal (tramp-file-name-host vec) "target"))
    (should (equal (tramp-file-name-localname vec) "/path"))
    ;; Hop should be set
    (should (tramp-file-name-hop vec))))

(ert-deftest tramp-rpc-mock-test-multi-hop-dissect-multi-hop ()
  "Test that TRAMP can dissect a multi-hop rpc filename."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name "/rpc:hop1|rpc:hop2|rpc:user@target:/path")))
    (should (equal (tramp-file-name-method vec) "rpc"))
    (should (equal (tramp-file-name-user vec) "user"))
    (should (equal (tramp-file-name-host vec) "target"))
    (should (equal (tramp-file-name-localname vec) "/path"))
    (should (tramp-file-name-hop vec))))

(ert-deftest tramp-rpc-mock-test-multi-hop-dissect-mixed-methods ()
  "Test that TRAMP can dissect a mixed ssh/rpc hop filename."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name "/ssh:gateway|rpc:user@target:/path")))
    (should (equal (tramp-file-name-method vec) "rpc"))
    (should (equal (tramp-file-name-user vec) "user"))
    (should (equal (tramp-file-name-host vec) "target"))
    (should (equal (tramp-file-name-localname vec) "/path"))
    (should (tramp-file-name-hop vec))))

(ert-deftest tramp-rpc-mock-test-hops-to-proxyjump-nil ()
  "Test that hops-to-proxyjump returns nil for no hops."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (make-tramp-file-name :method "rpc" :host "target"
                                   :localname "/path")))
    (should-not (tramp-rpc--hops-to-proxyjump vec))))

(ert-deftest tramp-rpc-mock-test-hops-to-proxyjump-single ()
  "Test ProxyJump conversion with a single hop."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name "/rpc:gateway|rpc:user@target:/path")))
    (should (equal (tramp-rpc--hops-to-proxyjump vec) "gateway"))))

(ert-deftest tramp-rpc-mock-test-hops-to-proxyjump-single-with-user ()
  "Test ProxyJump conversion with a single hop that has a user."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name "/rpc:admin@gateway|rpc:user@target:/path")))
    (should (equal (tramp-rpc--hops-to-proxyjump vec) "admin@gateway"))))

(ert-deftest tramp-rpc-mock-test-hops-to-proxyjump-multiple ()
  "Test ProxyJump conversion with multiple hops."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name "/rpc:hop1|rpc:hop2|rpc:user@target:/path")))
    (should (equal (tramp-rpc--hops-to-proxyjump vec) "hop1,hop2"))))

(ert-deftest tramp-rpc-mock-test-hops-to-proxyjump-with-port ()
  "Test ProxyJump conversion with a hop that has a port."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name "/rpc:admin@gateway#2222|rpc:user@target:/path")))
    (should (equal (tramp-rpc--hops-to-proxyjump vec) "admin@gateway:2222"))))

(ert-deftest tramp-rpc-mock-test-hops-to-proxyjump-mixed-methods ()
  "Test ProxyJump conversion with mixed ssh/rpc hops."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name "/ssh:gateway|rpc:user@target:/path")))
    (should (equal (tramp-rpc--hops-to-proxyjump vec) "gateway"))))

(ert-deftest tramp-rpc-mock-test-connection-key-no-hop ()
  "Test connection key without hops."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (make-tramp-file-name :method "rpc" :host "target"
                                   :user "user" :localname "/path")))
    (should (equal (tramp-rpc--connection-key vec)
                   '("target" "user" "22" nil)))))

(ert-deftest tramp-rpc-mock-test-connection-key-with-hop ()
  "Test connection key includes hop for differentiation."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((vec1 (tramp-dissect-file-name "/rpc:gateway|rpc:user@target:/path"))
         (vec2 (make-tramp-file-name :method "rpc" :host "target"
                                     :user "user" :localname "/path"))
         (key1 (tramp-rpc--connection-key vec1))
         (key2 (tramp-rpc--connection-key vec2)))
    ;; Keys should differ because one has a hop and the other doesn't
    (should-not (equal key1 key2))
    ;; Both should have the same host/user/port
    (should (equal (nth 0 key1) (nth 0 key2)))
    (should (equal (nth 1 key1) (nth 1 key2)))
    (should (equal (nth 2 key1) (nth 2 key2)))
    ;; Hop should differ
    (should (nth 3 key1))
    (should-not (nth 3 key2))))

(ert-deftest tramp-rpc-mock-test-connection-key-different-hops ()
  "Test that different hop routes produce different connection keys."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((vec1 (tramp-dissect-file-name "/rpc:gateway1|rpc:user@target:/path"))
         (vec2 (tramp-dissect-file-name "/rpc:gateway2|rpc:user@target:/path"))
         (key1 (tramp-rpc--connection-key vec1))
         (key2 (tramp-rpc--connection-key vec2)))
    (should-not (equal key1 key2))))

(ert-deftest tramp-rpc-mock-test-controlmaster-socket-different-hops ()
  "Test that different hop routes produce different ControlMaster socket paths."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((vec1 (tramp-dissect-file-name "/rpc:gateway1|rpc:user@target:/path"))
         (vec2 (tramp-dissect-file-name "/rpc:gateway2|rpc:user@target:/path"))
         (vec3 (make-tramp-file-name :method "rpc" :host "target"
                                     :user "user" :localname "/path"))
         (path1 (tramp-rpc--controlmaster-socket-path vec1))
         (path2 (tramp-rpc--controlmaster-socket-path vec2))
         (path3 (tramp-rpc--controlmaster-socket-path vec3)))
    ;; All three should be different
    (should-not (equal path1 path2))
    (should-not (equal path1 path3))
    (should-not (equal path2 path3))))

(ert-deftest tramp-rpc-mock-test-deploy-normalize-hops-nil ()
  "Test hop normalization with nil input."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (should-not (tramp-rpc-deploy--normalize-hops nil)))

(ert-deftest tramp-rpc-mock-test-deploy-normalize-hops-rpc ()
  "Test hop normalization converts rpc: to ssh:."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (should (equal (tramp-rpc-deploy--normalize-hops "rpc:gateway|")
                 "ssh:gateway|")))

(ert-deftest tramp-rpc-mock-test-deploy-normalize-hops-ssh ()
  "Test hop normalization leaves ssh: unchanged."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (should (equal (tramp-rpc-deploy--normalize-hops "ssh:gateway|")
                 "ssh:gateway|")))

(ert-deftest tramp-rpc-mock-test-delete-file-trash-follows-tramp ()
  "Test `delete-file' trash handling matches TRAMP's skeleton."
  :tags '(:delete)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((delete-by-moving-to-trash t)
        (remote-file-name-inhibit-delete-by-moving-to-trash nil)
        moved rpc-called)
    (cl-letf (((symbol-function 'move-file-to-trash)
               (lambda (filename) (setq moved filename)))
              ((symbol-function 'tramp-rpc--call)
               (lambda (&rest _args) (setq rpc-called t))))
      (tramp-rpc-handle-delete-file "/rpc:mock:/tmp/file" 'trash)
      (should (equal moved "/rpc:mock:/tmp/file"))
      (should-not rpc-called))))

(ert-deftest tramp-rpc-mock-test-move-file-to-trash-regular-file-local-trash ()
  "Test optimized `move-file-to-trash' copies a remote file to local trash."
  :tags '(:delete)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((trash-root (make-temp-file "tramp-rpc-trash" t))
         (trash-directory trash-root)
         (stat `((type . "file")
                 (mode . ,(logior #o100000 #o640))
                 (mtime . 1700000000)))
         calls)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call-file-stat)
                   (lambda (_vec localname &optional lstat)
                     (should (equal localname "/tmp/file"))
                     (should lstat)
                     stat))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method _params)
                     (push method calls)
                     (pcase method
                       ("file.read" '((content . "payload")))
                       ("file.delete" t)
                       (_ (error "Unexpected RPC method: %s" method)))))
                  ((symbol-function 'tramp-rpc--invalidate-cache-for-path) #'ignore)
                  ((symbol-function 'tramp-flush-file-properties) #'ignore)
                  ((symbol-function 'tramp-flush-directory-properties) #'ignore))
          (tramp-rpc-handle-move-file-to-trash "/rpc:mock:/tmp/file")
          (should (equal (sort calls #'string<) '("file.delete" "file.read")))
          (should (file-exists-p (expand-file-name "file" trash-root)))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally (expand-file-name "file" trash-root))
            (should (equal (buffer-string) "payload"))))
      (delete-directory trash-root t))))

(ert-deftest tramp-rpc-mock-test-move-file-to-trash-symlink-local-trash ()
  "Test optimized `move-file-to-trash' recreates symlinks without file.read."
  :tags '(:delete)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((trash-root (make-temp-file "tramp-rpc-trash" t))
         (trash-directory trash-root)
         (stat '((type . "symlink")
                 (link_target . "../target")
                 (mode . 41471)
                 (mtime . 1700000000)))
         calls)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call-file-stat)
                   (lambda (&rest _args) stat))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method _params)
                     (push method calls)
                     (pcase method
                       ("file.delete" t)
                       (_ (error "Unexpected RPC method: %s" method)))))
                  ((symbol-function 'tramp-rpc--invalidate-cache-for-path) #'ignore)
                  ((symbol-function 'tramp-flush-file-properties) #'ignore)
                  ((symbol-function 'tramp-flush-directory-properties) #'ignore))
          (tramp-rpc-handle-move-file-to-trash "/rpc:mock:/tmp/link")
          (should (equal calls '("file.delete")))
          (should (equal (file-symlink-p (expand-file-name "link" trash-root))
                         "../target")))
      (delete-directory trash-root t))))

(ert-deftest tramp-rpc-mock-test-move-file-to-trash-directory-local-trash ()
  "Test optimized `move-file-to-trash' recursively copies small directories."
  :tags '(:delete)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((trash-root (make-temp-file "tramp-rpc-trash" t))
         (trash-directory trash-root)
         (dir-stat `((type . "directory")
                     (mode . ,(logior #o040000 #o755))
                     (mtime . 1700000000)))
         (file-stat `((type . "file")
                      (mode . ,(logior #o100000 #o644))
                      (mtime . 1700000000)))
         (link-stat '((type . "symlink")
                      (link_target . "a.txt")
                      (mode . 41471)
                      (mtime . 1700000000)))
         calls)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call-file-stat)
                   (lambda (&rest _args) dir-stat))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method params)
                     (push method calls)
                     (pcase method
                       ("dir.list"
                        (should (eq (alist-get 'include_attrs params) t))
                        (should (eq (alist-get 'include_hidden params) t))
                        (pcase (alist-get 'path params)
                          ("/tmp/dir"
                           `(((name . ".") (type . "directory") (attrs . ,dir-stat))
                             ((name . "..") (type . "directory") (attrs . ,dir-stat))
                             ((name . "a.txt") (type . "file") (attrs . ,file-stat))
                             ((name . "link") (type . "symlink") (attrs . ,link-stat))
                             ((name . "sub") (type . "directory") (attrs . ,dir-stat))))
                          ("/tmp/dir/sub"
                           `(((name . ".") (type . "directory") (attrs . ,dir-stat))
                             ((name . "..") (type . "directory") (attrs . ,dir-stat))
                             ((name . "b.txt") (type . "file") (attrs . ,file-stat))))
                          (_ (error "Unexpected dir.list path: %S" params))))
                       ("dir.remove" t)
                       (_ (error "Unexpected RPC method: %s" method)))))
                  ((symbol-function 'tramp-rpc--call-batch)
                   (lambda (_vec requests)
                     (mapcar (lambda (request)
                               (pcase (alist-get 'path (cdr request))
                                 ("/tmp/dir/a.txt" '((content . "root")))
                                 ("/tmp/dir/sub/b.txt" '((content . "child")))
                                 (_ (error "Unexpected batch request: %S" request))))
                             requests)))
                  ((symbol-function 'tramp-rpc--invalidate-cache-for-path) #'ignore)
                  ((symbol-function 'tramp-flush-file-properties) #'ignore)
                  ((symbol-function 'tramp-flush-directory-properties) #'ignore))
          (tramp-rpc-handle-move-file-to-trash "/rpc:mock:/tmp/dir")
          (let ((dest (expand-file-name "dir" trash-root)))
            (should (file-directory-p dest))
            (should (equal (file-symlink-p (expand-file-name "link" dest)) "a.txt"))
            (with-temp-buffer
              (insert-file-contents-literally (expand-file-name "a.txt" dest))
              (should (equal (buffer-string) "root")))
            (with-temp-buffer
              (insert-file-contents-literally (expand-file-name "sub/b.txt" dest))
              (should (equal (buffer-string) "child"))))
          (should (member "dir.remove" calls)))
      (delete-directory trash-root t))))

(ert-deftest tramp-rpc-mock-test-move-file-to-trash-unsupported-cleans-up ()
  "Test unsupported optimized trash removes partial copy before fallback."
  :tags '(:delete)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((trash-root (make-temp-file "tramp-rpc-trash" t))
         (trash-directory trash-root)
         (dir-stat `((type . "directory")
                     (mode . ,(logior #o040000 #o755))
                     (mtime . 1700000000)))
         fallback-called)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call-file-stat)
                   (lambda (&rest _args) dir-stat))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method params)
                     (pcase method
                       ("dir.list"
                        (should (eq (alist-get 'include_attrs params) t))
                        (should (eq (alist-get 'include_hidden params) t))
                        '(((name . "socket")
                           (type . "socket")
                           (attrs . ((type . "socket"))))))
                       ((or "dir.remove" "file.delete")
                        (error "Remote source must not be deleted on fallback"))
                       (_ (error "Unexpected RPC method: %s" method)))))
                  ((symbol-function 'tramp-rpc--fallback-move-file-to-trash)
                   (lambda (filename)
                     (setq fallback-called filename)
                     (should-not (file-exists-p (expand-file-name "dir" trash-root)))
                     'fallback)))
          (should (eq (tramp-rpc-handle-move-file-to-trash "/rpc:mock:/tmp/dir")
                      'fallback))
          (should (equal fallback-called "/rpc:mock:/tmp/dir"))
          (should-not (file-exists-p (expand-file-name "dir" trash-root))))
      (delete-directory trash-root t))))

(ert-deftest tramp-rpc-mock-test-move-file-to-trash-copy-failure-cleans-up ()
  "Test ordinary optimized trash copy failures clean up and re-signal."
  :tags '(:delete)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((trash-root (make-temp-file "tramp-rpc-trash" t))
         (trash-directory trash-root)
         (stat `((type . "file")
                 (mode . ,(logior #o100000 #o640))
                 (mtime . 1700000000)))
         remote-delete-called fallback-called)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call-file-stat)
                   (lambda (&rest _args) stat))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method _params)
                     (pcase method
                       ("file.read" '((content . "payload")))
                       ("file.delete" (setq remote-delete-called t))
                       (_ (error "Unexpected RPC method: %s" method)))))
                  ((symbol-function 'tramp-rpc--write-local-trash-file)
                   (lambda (filename _content _stat)
                     (with-temp-file filename
                       (insert "partial"))
                     (signal 'file-error '("local write failed"))))
                  ((symbol-function 'tramp-rpc--fallback-move-file-to-trash)
                   (lambda (&rest _args) (setq fallback-called t))))
          (should-error (tramp-rpc-handle-move-file-to-trash "/rpc:mock:/tmp/file")
                        :type 'file-error)
          (should-not (file-exists-p (expand-file-name "file" trash-root)))
          (should-not remote-delete-called)
          (should-not fallback-called))
      (delete-directory trash-root t))))

(ert-deftest tramp-rpc-mock-test-move-file-to-trash-delete-failure-cleans-up ()
  "Test remote delete failures clean up the local trash copy and re-signal."
  :tags '(:delete)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((trash-root (make-temp-file "tramp-rpc-trash" t))
         (trash-directory trash-root)
         (stat `((type . "file")
                 (mode . ,(logior #o100000 #o640))
                 (mtime . 1700000000)))
         fallback-called)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call-file-stat)
                   (lambda (&rest _args) stat))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method _params)
                     (pcase method
                       ("file.read" '((content . "payload")))
                       ("file.delete" (signal 'file-error '("remote delete failed")))
                       (_ (error "Unexpected RPC method: %s" method)))))
                  ((symbol-function 'tramp-rpc--fallback-move-file-to-trash)
                   (lambda (&rest _args) (setq fallback-called t))))
          (should-error (tramp-rpc-handle-move-file-to-trash "/rpc:mock:/tmp/file")
                        :type 'file-error)
          (should-not (file-exists-p (expand-file-name "file" trash-root)))
          (should-not fallback-called))
      (delete-directory trash-root t))))

(ert-deftest tramp-rpc-mock-test-move-file-to-trash-directory-bounds-batches ()
  "Test optimized directory trash reads regular files in bounded batches."
  :tags '(:delete)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((trash-root (make-temp-file "tramp-rpc-trash" t))
         (trash-directory trash-root)
         (dir-stat `((type . "directory")
                     (mode . ,(logior #o040000 #o755))
                     (mtime . 1700000000)))
         (file-stat `((type . "file")
                      (mode . ,(logior #o100000 #o644))
                      (mtime . 1700000000)))
         (file-count (1+ tramp-rpc--trash-read-batch-size))
         (names (cl-loop for i below file-count collect (format "file-%02d" i)))
         batch-sizes)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call-file-stat)
                   (lambda (&rest _args) dir-stat))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method params)
                     (pcase method
                       ("dir.list"
                        (should (eq (alist-get 'include_attrs params) t))
                        (should (eq (alist-get 'include_hidden params) t))
                        (append
                         `(((name . ".") (type . "directory") (attrs . ,dir-stat))
                           ((name . "..") (type . "directory") (attrs . ,dir-stat)))
                         (mapcar (lambda (name)
                                   `((name . ,name) (type . "file") (attrs . ,file-stat)))
                                 names)))
                       ("dir.remove" t)
                       (_ (error "Unexpected RPC method: %s" method)))))
                  ((symbol-function 'tramp-rpc--call-batch)
                   (lambda (_vec requests)
                     (push (length requests) batch-sizes)
                     (mapcar (lambda (request)
                               `((content . ,(format "content:%s"
                                              (file-name-nondirectory
                                               (alist-get 'path (cdr request)))))))
                             requests)))
                  ((symbol-function 'tramp-rpc--invalidate-cache-for-path) #'ignore)
                  ((symbol-function 'tramp-flush-file-properties) #'ignore)
                  ((symbol-function 'tramp-flush-directory-properties) #'ignore))
          (tramp-rpc-handle-move-file-to-trash "/rpc:mock:/tmp/dir")
          (should (equal (nreverse batch-sizes)
                         (list tramp-rpc--trash-read-batch-size 1)))
          (dolist (name names)
            (should (file-exists-p (expand-file-name name
                                                     (expand-file-name "dir" trash-root))))))
      (delete-directory trash-root t))))

(ert-deftest tramp-rpc-mock-test-delete-file-trash-can-be-inhibited ()
  "Test `remote-file-name-inhibit-delete-by-moving-to-trash' forces unlink."
  :tags '(:delete)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((delete-by-moving-to-trash t)
        (remote-file-name-inhibit-delete-by-moving-to-trash t)
        moved method)
    (cl-letf (((symbol-function 'move-file-to-trash)
               (lambda (filename) (setq moved filename)))
              ((symbol-function 'tramp-rpc--call)
               (lambda (_vec m _params) (setq method m))))
      (tramp-rpc-handle-delete-file "/rpc:mock:/tmp/file" 'trash)
      (should-not moved)
      (should (equal method "file.delete")))))

(ert-deftest tramp-rpc-mock-test-delete-directory-trash-can-be-inhibited ()
  "Test inhibited trash makes `delete-directory' call remote remove."
  :tags '(:delete)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((delete-by-moving-to-trash t)
        (remote-file-name-inhibit-delete-by-moving-to-trash t)
        moved method)
    (cl-letf (((symbol-function 'move-file-to-trash)
               (lambda (filename) (setq moved filename)))
              ((symbol-function 'tramp-rpc--call)
               (lambda (_vec m _params) (setq method m))))
      (tramp-rpc-handle-delete-directory "/rpc:mock:/tmp/dir" 'recursive 'trash)
      (should-not moved)
      (should (equal method "dir.remove")))))

(ert-deftest tramp-rpc-mock-test-deploy-normalize-hops-mixed ()
  "Test hop normalization with mixed methods."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (should (equal (tramp-rpc-deploy--normalize-hops "rpc:hop1|ssh:hop2|")
                 "ssh:hop1|ssh:hop2|")))

(ert-deftest tramp-rpc-mock-test-deploy-normalize-hops-with-user ()
  "Test hop normalization preserves user@host."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (should (equal (tramp-rpc-deploy--normalize-hops "rpc:admin@gateway|")
                 "ssh:admin@gateway|")))

(ert-deftest tramp-rpc-mock-test-deploy-bootstrap-vec-preserves-hop ()
  "Test that bootstrap vec preserves and normalizes hops."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((vec (tramp-dissect-file-name "/rpc:gateway|rpc:user@target:/path"))
         (bootstrap (tramp-rpc-deploy--bootstrap-vec vec)))
    ;; Method should be the bootstrap method (default: scp)
    (should (equal (tramp-file-name-method bootstrap)
                   tramp-rpc-deploy-bootstrap-method))
    ;; Host and user should be preserved
    (should (equal (tramp-file-name-host bootstrap) "target"))
    (should (equal (tramp-file-name-user bootstrap) "user"))
    ;; Hop should be present and normalized (rpc -> ssh)
    (let ((hop (tramp-file-name-hop bootstrap)))
      (should hop)
      (should (string-match-p "ssh:" hop))
      (should-not (string-match-p "rpc:" hop)))))

(ert-deftest tramp-rpc-mock-test-deploy-binary-id-release-default ()
  "Test that non-git installs keep using the release version as binary id."
  :tags '(:deploy)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((tramp-rpc-deploy-source-directory nil)
        (tramp-rpc-deploy-git-build-policy 'auto))
    (should (equal (tramp-rpc-deploy--binary-id)
                   tramp-rpc-deploy-version))
    (should (string-suffix-p
             (format "tramp-rpc-server-%s" tramp-rpc-deploy-version)
             (tramp-rpc-deploy-expected-binary-localname)))))

(ert-deftest tramp-rpc-mock-test-deploy-source-directory-warning ()
  "Test source directory warnings explain release-id fallback."
  :tags '(:deploy)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((dir (make-temp-file "tramp-rpc-build" t)))
    (unwind-protect
        (let ((tramp-rpc-deploy-source-directory dir)
              (tramp-rpc-deploy-git-build-policy 'auto))
          (should (string-match-p
                   "does not contain Cargo.toml and server/"
                   (tramp-rpc-deploy--source-directory-warning))))
      (delete-directory dir t))))

(ert-deftest tramp-rpc-mock-test-deploy-default-source-follows-elc-source-symlink ()
  "Test default source directory follows straight-style .el symlinks."
  :tags '(:deploy)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((dir (make-temp-file "tramp-rpc-straight" t))
         (repo (expand-file-name "straight/repos/emacs-tramp-rpc" dir))
         (build (expand-file-name "straight/build/tramp-rpc" dir))
         (repo-lisp (expand-file-name "lisp" repo))
         (repo-file (expand-file-name "tramp-rpc-deploy.el" repo-lisp))
         (build-el (expand-file-name "tramp-rpc-deploy.el" build))
         (build-elc (expand-file-name "tramp-rpc-deploy.elc" build)))
    (unwind-protect
        (progn
          (make-directory repo-lisp t)
          (make-directory build t)
          (with-temp-file repo-file
            (insert ";; source\n"))
          (make-symbolic-link repo-file build-el)
          (with-temp-file build-elc
            (insert ";; compiled\n"))
          (let ((load-file-name build-elc))
            (should (equal (file-name-as-directory
                            (tramp-rpc-deploy--default-source-directory))
                           (file-name-as-directory repo)))))
      (delete-directory dir t))))

(ert-deftest tramp-rpc-mock-test-deploy-skips-stale-bundled-source-binary ()
  "Test source-id mode does not deploy stale bundled binaries."
  :tags '(:deploy)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((dir (make-temp-file "tramp-rpc-source" t))
         (bundled-dir (expand-file-name "lisp/binaries" dir))
         (bundled (expand-file-name "x86_64-linux/tramp-rpc-server" bundled-dir))
         (built (expand-file-name "built-server" dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" dir))
          (make-directory (expand-file-name "server/src" dir) t)
          (make-directory (file-name-directory bundled) t)
          (with-temp-file (expand-file-name "Cargo.toml" dir)
            (insert "[workspace]\nmembers = [\"server\"]\n"))
          (with-temp-file (expand-file-name "server/src/main.rs" dir)
            (insert "fn main() {}\n"))
          (with-temp-file bundled
            (insert "stale bundled binary\n"))
          (set-file-modes bundled #o755)
          (set-file-times bundled (seconds-to-time 0))
          (let ((tramp-rpc-deploy-source-directory dir)
                (tramp-rpc-deploy-git-build-policy 'auto)
                (tramp-rpc-deploy-bundled-binary-directory bundled-dir)
                (tramp-rpc-deploy-local-cache-directory
                 (expand-file-name "cache" dir)))
            (cl-letf (((symbol-function 'tramp-rpc-deploy--git-revision)
                       (lambda () "abcdef123456"))
                      ((symbol-function 'tramp-rpc-deploy--build-binary)
                       (lambda (_arch) built)))
              (should (equal (tramp-rpc-deploy--ensure-local-binary
                              "x86_64-linux")
                             built)))))
      (delete-directory dir t))))

(ert-deftest tramp-rpc-mock-test-deploy-binary-id-source-hash ()
  "Test that git checkouts key binary ids by server source content."
  :tags '(:deploy)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((dir (make-temp-file "tramp-rpc-source" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" dir))
          (make-directory (expand-file-name "server/src" dir) t)
          (with-temp-file (expand-file-name "Cargo.toml" dir)
            (insert "[package]\nname = \"tramp-rpc-server\"\n"))
          (with-temp-file (expand-file-name "server/src/main.rs" dir)
            (insert "fn main() {}\n"))
          (let ((tramp-rpc-deploy-source-directory dir)
                (tramp-rpc-deploy-git-build-policy 'auto))
            (cl-letf (((symbol-function 'tramp-rpc-deploy--git-revision)
                       (lambda () "abcdef123456")))
              (let ((id1 (tramp-rpc-deploy--binary-id)))
                (should (string-prefix-p "git-abcdef123456-" id1))
                (should (string-match-p
                         (concat "tramp-rpc-server-" (regexp-quote id1))
                         (tramp-rpc-deploy-expected-binary-localname)))
                (with-temp-file (expand-file-name "server/src/main.rs" dir)
                  (insert "fn main() { println!(\"changed\"); }\n"))
                (should-not (equal id1 (tramp-rpc-deploy--binary-id)))))))
      (delete-directory dir t))))

(ert-deftest tramp-rpc-mock-test-deploy-binary-id-release-policy ()
  "Test that release policy keeps version-keyed ids for git checkouts."
  :tags '(:deploy)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((dir (make-temp-file "tramp-rpc-source" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" dir))
          (make-directory (expand-file-name "server/src" dir) t)
          (with-temp-file (expand-file-name "Cargo.toml" dir)
            (insert "[workspace]\nmembers = [\"server\"]\n"))
          (with-temp-file (expand-file-name "server/src/main.rs" dir)
            (insert "fn main() {}\n"))
          (let ((tramp-rpc-deploy-source-directory dir)
                (tramp-rpc-deploy-git-build-policy 'release))
            (should (equal (tramp-rpc-deploy--binary-id)
                           tramp-rpc-deploy-version))))
      (delete-directory dir t))))

(ert-deftest tramp-rpc-mock-test-deploy-binary-id-ignores-lisp-files ()
  "Test that git binary ids only include files affecting the server build."
  :tags '(:deploy)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((dir (make-temp-file "tramp-rpc-source" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" dir))
          (make-directory (expand-file-name "server/src" dir) t)
          (make-directory (expand-file-name "lisp" dir) t)
          (with-temp-file (expand-file-name "Cargo.toml" dir)
            (insert "[workspace]\nmembers = [\"server\"]\n"))
          (with-temp-file (expand-file-name "server/src/main.rs" dir)
            (insert "fn main() {}\n"))
          (let ((tramp-rpc-deploy-source-directory dir)
                (tramp-rpc-deploy-git-build-policy 'auto))
            (cl-letf (((symbol-function 'tramp-rpc-deploy--git-revision)
                       (lambda () "abcdef123456")))
              (let ((id1 (tramp-rpc-deploy--binary-id)))
                (with-temp-file (expand-file-name "lisp/tramp-rpc.el" dir)
                  (insert ";; lisp-only change\n"))
                (should (equal id1 (tramp-rpc-deploy--binary-id)))))))
      (delete-directory dir t))))

;; With latest tramp, tramp-file-name-with-sudo natively produces
;; /rpc:user@host|sudo:root@host:/path for rpc paths since the rpc
;; method is now multi-hop capable (inherits ssh connection params).
(ert-deftest tramp-rpc-mock-test-zz-file-name-with-sudo-native ()
  "Test that tramp-file-name-with-sudo natively produces rpc+sudo path."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (skip-unless (tramp-rpc-mock-test--sudo-helper-available-p))
  (let* ((tramp-file-name-with-method "sudo")
         (filename (tramp-file-name-with-sudo "/rpc:user@target:/etc/hosts"))
         (vec (tramp-dissect-file-name filename)))
    (should (tramp-tramp-file-p filename))
    (should (equal (tramp-file-name-localname vec) "/etc/hosts"))
    (should (string= (tramp-file-name-method vec) "sudo"))
    (should (string= (tramp-file-name-host vec) "target"))
    ;; The hop should preserve the rpc method
    (when (tramp-file-name-hop vec)
      (should (string-match-p "rpc:" (tramp-file-name-hop vec)))))
  ;; Also verify non-rpc paths still work
  (let* ((tramp-file-name-with-method "sudo")
         (filename (tramp-file-name-with-sudo "/ssh:user@target:/etc/hosts"))
         (vec (tramp-dissect-file-name filename)))
    (should (tramp-tramp-file-p filename))
    (should (string= (tramp-file-name-method vec) "sudo"))
    (should (string= (tramp-file-name-host vec) "target"))))

(ert-deftest tramp-rpc-mock-test-rpc-method-advertises-host-arg ()
  "Test that the rpc method declares %%h in tramp-login-args.
This is required so `tramp-compute-multi-hops' allows rpc to appear
as a proxy hop alongside shell methods like sudo/su.  Without %%h,
the host-check in `tramp-compute-multi-hops' would reject the rpc
hop with \"Host name does not match\"."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((vec (make-tramp-file-name :method "rpc" :host "target"))
         (login-args (tramp-get-method-parameter vec 'tramp-login-args)))
    (should (member "%h" (flatten-tree login-args)))))

(ert-deftest tramp-rpc-mock-test-compute-multi-hops-rpc-sudo-chain ()
  "Test that `tramp-compute-multi-hops' accepts rpc as a proxy for sudo.
The rpc method inherits all ssh connection parameters, so
`tramp-maybe-open-connection' can process rpc hops using SSH."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  ;; Manually install the proxy entry that tramp-add-hops would create
  ;; when processing /rpc:server|sudo:root@server:/path.
  (let* ((tramp-default-proxies-alist
          (list (list "^server$" "^root$"
                      (propertize "/rpc:server:" 'tramp-ad-hoc t))))
         (sudo-vec (make-tramp-file-name :method "sudo" :user "root"
                                         :host "server"
                                         :localname "/var/log/kern.log"))
         result)
    (should (setq result (tramp-compute-multi-hops sudo-vec)))
    ;; The chain should contain 2 elements: rpc proxy hop + sudo destination.
    (should (= (length result) 2))
    ;; The rpc hop keeps its method name; it works because the rpc
    ;; method entry has ssh's tramp-login-program and tramp-login-args.
    (should (string= (tramp-file-name-method (car result)) "rpc"))
    (should (string= (tramp-file-name-host (car result)) "server"))))

(ert-deftest tramp-rpc-mock-test-rpc-method-has-ssh-login-program ()
  "Test that the rpc method inherits ssh's tramp-login-program.
This is needed for `tramp-maybe-open-connection' to process rpc
as a hop in multi-hop chains."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (make-tramp-file-name :method "rpc" :host "target")))
    (should (string= (tramp-get-method-parameter vec 'tramp-login-program) "ssh"))
    (should (tramp-get-method-parameter vec 'tramp-remote-shell))))

;;; ============================================================================
;;; Sudo-via-RPC tests (No server or SSH required)
;;; ============================================================================

(ert-deftest tramp-rpc-mock-test-detect-sudo-elevation-basic ()
  "Test sudo elevation detection for /rpc:user@host|sudo:root@host:/path."
  :tags '(:multi-hop :sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name
              "/rpc:alice@server|sudo:root@server:/etc/shadow")))
    (should (equal (tramp-rpc--detect-sudo-elevation vec) "alice"))))

(ert-deftest tramp-rpc-mock-test-detect-sudo-elevation-no-hop ()
  "Test that normal rpc paths return nil for sudo detection."
  :tags '(:multi-hop :sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (make-tramp-file-name :method "rpc" :user "root"
                                   :host "server" :localname "/root")))
    (should-not (tramp-rpc--detect-sudo-elevation vec))))

(ert-deftest tramp-rpc-mock-test-detect-sudo-elevation-different-host ()
  "Test that proxy hops to different hosts don't trigger sudo detection."
  :tags '(:multi-hop :sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name
              "/rpc:user@gateway|sudo:root@server:/root")))
    ;; gateway != server, so no sudo elevation
    (should-not (tramp-rpc--detect-sudo-elevation vec))))

(ert-deftest tramp-rpc-mock-test-sudo-file-name-predicate ()
  "Test the sudo+rpc handler predicate."
  :tags '(:multi-hop :sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  ;; rpc+sudo should match
  (should (tramp-rpc--sudo-file-name-p
           (tramp-dissect-file-name
            "/rpc:user@server|sudo:root@server:/root")))
  ;; plain rpc should not match
  (should-not (tramp-rpc--sudo-file-name-p
               (tramp-dissect-file-name "/rpc:user@server:/home")))
  ;; ssh+sudo should not match (no rpc in hop)
  (should-not (tramp-rpc--sudo-file-name-p
               (tramp-dissect-file-name
                "/ssh:user@server|sudo:root@server:/root"))))

(ert-deftest tramp-rpc-mock-test-proxy-hop-string-sudo ()
  "Test that same-host sudo hops are excluded from proxy hop string."
  :tags '(:multi-hop :sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  ;; Simple sudo: /rpc:user@host|sudo:root@host:/path -> no proxy hops
  (let ((vec (tramp-dissect-file-name
              "/rpc:user@server|sudo:root@server:/root")))
    (should-not (tramp-rpc--proxy-hop-string vec)))
  ;; With gateway: /rpc:gw|rpc:user@host|sudo:root@host:/path -> "rpc:gw|"
  (let ((vec (tramp-dissect-file-name
              "/rpc:gw|rpc:user@server|sudo:root@server:/root")))
    (should (string-match-p "rpc:gw" (tramp-rpc--proxy-hop-string vec)))
    ;; The same-host hop should be excluded
    (should-not (string-match-p "rpc:user@server"
                                (tramp-rpc--proxy-hop-string vec)))))

(ert-deftest tramp-rpc-mock-test-hops-to-proxyjump-skips-sudo ()
  "Test that hops-to-proxyjump skips same-host sudo hops."
  :tags '(:multi-hop :sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  ;; Simple sudo: no proxy jumps needed
  (let ((vec (tramp-dissect-file-name
              "/rpc:user@server|sudo:root@server:/root")))
    (should-not (tramp-rpc--hops-to-proxyjump vec)))
  ;; With gateway: only gateway in proxyjump
  (let ((vec (tramp-dissect-file-name
              "/rpc:gw|rpc:user@server|sudo:root@server:/root")))
    (let ((pj (tramp-rpc--hops-to-proxyjump vec)))
      (should pj)
      (should (string-match-p "gw" pj))
      (should-not (string-match-p "server" pj)))))

(ert-deftest tramp-rpc-mock-test-controlmaster-socket-shared ()
  "Test that sudo and normal connections share the ControlMaster socket."
  :tags '(:multi-hop :sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((normal-vec (make-tramp-file-name :method "rpc" :user "alice"
                                          :host "server" :localname "/home"))
        (sudo-vec (tramp-dissect-file-name
                   "/rpc:alice@server|sudo:root@server:/root")))
    ;; Both should produce the same ControlMaster socket path
    (should (equal (tramp-rpc--controlmaster-socket-path normal-vec)
                   (tramp-rpc--controlmaster-socket-path sudo-vec)))))

;;; ============================================================================
;;; VC handler tests (No server or SSH required)
;;; ============================================================================

(ert-deftest tramp-rpc-mock-test-vc-exec-after-logical-exit-runs-code ()
  "Test `vc-exec-after' handler treats exited TRAMP-RPC relays as done."
  :tags '(:vc-handler)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((buffer (generate-new-buffer " *tramp-rpc-vc-exec-after-test*"))
         (proc (make-process :name "tramp-rpc-vc-exec-after-test"
                             :buffer buffer
                             :command '("sh" "-c" "sleep 60")
                             :noquery t))
         (ran nil))
    (unwind-protect
        (with-current-buffer buffer
          (process-put proc :tramp-rpc-pid 123)
          (process-put proc :tramp-rpc-exited t)
          (cl-letf (((symbol-function 'tramp-run-real-handler)
                     (lambda (&rest _) (error "Unexpected process state"))))
            (tramp-rpc-handle-vc-exec-after
             (lambda () (setq ran t))))
          (should ran))
      (when (process-live-p proc)
        (delete-process proc))
      (kill-buffer buffer))))

(ert-deftest tramp-rpc-mock-test-vc-exec-after-raw-closed-state-runs-code ()
  "Test `vc-exec-after' handler handles raw non-run relay states."
  :tags '(:vc-handler)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((buffer (generate-new-buffer " *tramp-rpc-vc-exec-after-test*"))
         (proc (make-process :name "tramp-rpc-vc-exec-after-test"
                             :buffer buffer
                             :command '("sh" "-c" "sleep 60")
                             :noquery t))
         (ran nil))
    (unwind-protect
        (with-current-buffer buffer
          (process-put proc :tramp-rpc-pid 123)
          ;; Simulate native-compiled VC observing a raw relay state such as
          ;; `closed' rather than the logical state from TRAMP-RPC handler.
          (cl-letf (((symbol-function 'process-status)
                     (lambda (_process) 'closed))
                    ((symbol-function 'tramp-run-real-handler)
                     (lambda (&rest _) (error "Unexpected process state"))))
            (tramp-rpc-handle-vc-exec-after
             (lambda () (setq ran t))))
          (should ran))
      (when (process-live-p proc)
        (delete-process proc))
      (kill-buffer buffer))))

;;; ============================================================================
;;; Dir-locals advice tests (No server or SSH required)
;;; ============================================================================

(ert-deftest tramp-rpc-mock-test-dir-locals-enabled-for-rpc-buffer-file ()
  "Test that dir-locals advice enables remote dir-locals for RPC file buffers."
  :tags '(:dir-locals)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((enable-remote-dir-locals nil)
        (observed nil))
    ;; Simulate a buffer visiting an RPC remote file.
    (with-temp-buffer
      (setq buffer-file-name "/rpc:host:/home/user/project/foo.el")
      (tramp-rpc--hack-dir-local-variables-advice
       (lambda () (setq observed enable-remote-dir-locals)))
      (should (eq observed t)))))

(ert-deftest tramp-rpc-mock-test-dir-locals-enabled-for-rpc-default-directory ()
  "Test that dir-locals advice enables remote dir-locals via default-directory."
  :tags '(:dir-locals)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((enable-remote-dir-locals nil)
        (observed nil))
    ;; Simulate a buffer with an RPC remote default-directory (e.g. dired).
    (with-temp-buffer
      (setq default-directory "/rpc:host:/home/user/project/")
      (tramp-rpc--hack-dir-local-variables-advice
       (lambda () (setq observed enable-remote-dir-locals)))
      (should (eq observed t)))))

(ert-deftest tramp-rpc-mock-test-dir-locals-not-enabled-for-ssh ()
  "Test that dir-locals advice does NOT enable remote dir-locals for SSH files."
  :tags '(:dir-locals)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((enable-remote-dir-locals nil)
        (observed nil))
    (with-temp-buffer
      (setq buffer-file-name "/ssh:host:/home/user/project/foo.el")
      (tramp-rpc--hack-dir-local-variables-advice
       (lambda () (setq observed enable-remote-dir-locals)))
      (should-not observed))))

(ert-deftest tramp-rpc-mock-test-dir-locals-not-enabled-for-local ()
  "Test that dir-locals advice does NOT enable remote dir-locals for local files."
  :tags '(:dir-locals)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((enable-remote-dir-locals nil)
        (observed nil))
    (with-temp-buffer
      (setq buffer-file-name "/home/user/project/foo.el")
      (tramp-rpc--hack-dir-local-variables-advice
       (lambda () (setq observed enable-remote-dir-locals)))
      (should-not observed))))

(ert-deftest tramp-rpc-mock-test-dir-locals-preserves-existing-setting ()
  "Test that advice preserves `enable-remote-dir-locals' when already t."
  :tags '(:dir-locals)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((enable-remote-dir-locals t)
        (observed nil))
    ;; Even for a non-RPC file, the existing t value should be preserved.
    (with-temp-buffer
      (setq buffer-file-name "/ssh:host:/etc/hosts")
      (tramp-rpc--hack-dir-local-variables-advice
       (lambda () (setq observed enable-remote-dir-locals)))
      (should (eq observed t)))))

(ert-deftest tramp-rpc-mock-test-dir-locals-calls-orig-function ()
  "Test that the advice always calls the original function."
  :tags '(:dir-locals)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((orig-called nil))
    ;; RPC file
    (with-temp-buffer
      (setq buffer-file-name "/rpc:host:/foo")
      (tramp-rpc--hack-dir-local-variables-advice
       (lambda () (setq orig-called t)))
      (should orig-called))
    ;; Non-RPC file
    (setq orig-called nil)
    (with-temp-buffer
      (setq buffer-file-name "/home/user/foo")
      (tramp-rpc--hack-dir-local-variables-advice
       (lambda () (setq orig-called t)))
      (should orig-called))))

(ert-deftest tramp-rpc-mock-test-dir-locals-cache-covers-uses-containment ()
  "Ensure cache coverage check uses containment, not string length."
  :tags '(:dir-locals)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((locals "/tmp/a/b/")
        (cache "/tmp/a/b/c/d/")
        (sibling "/tmp/a/very-long-dirname/"))
    ;; These directories need not exist; this is a lexical containment check.
    (should (tramp-rpc--dir-locals-cache-covers-p locals locals))
    (should (tramp-rpc--dir-locals-cache-covers-p locals cache))
    ;; A sibling with a longer string must not be treated as contained.
    (should-not (tramp-rpc--dir-locals-cache-covers-p locals sibling))))

(ert-deftest tramp-rpc-mock-test-locate-dominating-file-unquotes-and-requotes-paths ()
  "Ensure locate-dominating handler unquotes RPC paths for transport.
Quoted RPC localnames (/: prefix) must be unquoted for server filesystem
operations and re-quoted on the way back."
  :tags '(:dir-locals)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let (captured-file)
    (cl-letf (((symbol-function 'tramp-rpc--call)
               (lambda (_vec method params)
                 (should (string= method "highlevel.locate_dominating_file_multi"))
                 (setq captured-file
                       (decode-coding-string (alist-get 'file params) 'utf-8 t))
                 (list (encode-coding-string "/tmp/tramp-rpc-root/.git" 'utf-8 t)))))
      (let* ((default-directory "/rpc:host:/:/tmp/tramp-rpc-root/subdir/")
             (result (tramp-rpc-handle-locate-dominating-file "foo" ".git")))
        (should (equal captured-file "/tmp/tramp-rpc-root/subdir/foo"))
        (should (equal result "/rpc:host:/:/tmp/tramp-rpc-root/"))))))

(ert-deftest tramp-rpc-mock-test-locate-dominating-file-respects-stop-regexp ()
  "Ensure locate-dominating handler filters results above stop regexp."
  :tags '(:dir-locals)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (cl-letf (((symbol-function 'tramp-rpc--call)
             (lambda (_vec method _params)
               (should (string= method "highlevel.locate_dominating_file_multi"))
               (list (encode-coding-string "/tmp/tramp-rpc-root/.git" 'utf-8 t)))))
    (let* ((default-directory "/rpc:host:/tmp/tramp-rpc-root/a/b/c/")
           (locate-dominating-stop-dir-regexp
            (regexp-quote "/tmp/tramp-rpc-root/a/b/")))
      (should-not (tramp-rpc-handle-locate-dominating-file "foo" ".git")))))

(ert-deftest tramp-rpc-mock-test-dir-locals-all-files-unquotes-and-requotes-paths ()
  "Ensure dir-locals-all-files handler preserves quoted RPC localnames."
  :tags '(:dir-locals)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let (captured-directory)
    (cl-letf (((symbol-function 'tramp-rpc--call)
               (lambda (_vec method params)
                 (should (string= method "highlevel.test_files_in_dir"))
                 (setq captured-directory
                       (decode-coding-string (alist-get 'directory params) 'utf-8 t))
                 (list (encode-coding-string "/tmp/tramp-rpc-root/.dir-locals.el"
                                             'utf-8 t)))))
      (let ((result (tramp-rpc-handle-dir-locals--all-files
                     "/rpc:host:/:/tmp/tramp-rpc-root/" nil)))
        (should (equal captured-directory "/tmp/tramp-rpc-root"))
        (should (equal result
                       '("/rpc:host:/:/tmp/tramp-rpc-root/.dir-locals.el")))))))

;;; ============================================================================
;;; Non-essential / recentf Tests (No server or SSH required)
;;; ============================================================================

(ert-deftest tramp-rpc-mock-test-ensure-connection-throws-non-essential ()
  "Test that `tramp-rpc--ensure-connection' throws when non-essential is t.
With no live connection and `non-essential' bound to t,
`tramp-connectable-p' returns nil and the function must throw
`non-essential' rather than attempting a 60s SSH handshake."
  :tags '(:non-essential)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((vec (make-tramp-file-name :method "rpc" :host "unreachable-test-host"
                                    :localname "/tmp"))
         (non-essential t)
         (result (catch 'non-essential
                   (tramp-rpc--ensure-connection vec)
                   'did-not-throw)))
    (should (eq result 'non-essential))))

(ert-deftest tramp-rpc-mock-test-ensure-connection-allows-when-essential ()
  "Test that `tramp-rpc--ensure-connection' does NOT throw when non-essential is nil.
It should attempt to connect (and fail), not silently bail."
  :tags '(:non-essential)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((vec (make-tramp-file-name :method "rpc" :host "unreachable-test-host"
                                    :localname "/tmp"))
         (non-essential nil))
    ;; With non-essential nil, it should try to connect and signal an error
    ;; (not throw 'non-essential)
    (should-error
     (tramp-rpc--ensure-connection vec)
     :type 'remote-file-error)))

(ert-deftest tramp-rpc-mock-test-handler-catches-non-essential ()
  "Test that `tramp-rpc-file-name-handler' falls back for non-essential ops.
When `non-essential' is t and no connection exists, file-exists-p on
a remote path should return nil (local fallback) instead of signaling."
  :tags '(:non-essential)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((non-essential t))
    ;; file-exists-p should return nil (local handler), not signal an error
    (should-not (file-exists-p "/rpc:unreachable-test-host:/no/such/path"))))

(ert-deftest tramp-rpc-mock-test-handler-file-remote-p-non-essential ()
  "Test that `file-remote-p' never triggers a connection attempt.
The handler should bind non-essential to t for file-remote-p,
so it works even when non-essential was nil."
  :tags '(:non-essential)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((non-essential nil))
    ;; file-remote-p should return the remote part without connecting
    (should (file-remote-p "/rpc:somehost:/path"))))

(ert-deftest tramp-rpc-mock-test-recentf-cleanup ()
  "Test that upstream `tramp-recentf-cleanup' removes matching entries.
tramp-rpc delegates recentf cleanup to the upstream function from
tramp-integration.el, registered on `tramp-cleanup-connection-hook'.
Uses an existing local path so `recentf-cleanup' does not also
discard it for being unreadable."
  :tags '(:non-essential :recentf)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (require 'recentf)
  (let* ((vec (make-tramp-file-name :method "rpc" :host "myhost"
                                    :localname "/dummy"))
         (local-file (expand-file-name "test/tramp-rpc-mock-tests.el"
                                       tramp-rpc-mock-test--project-root))
         (recentf-list (list "/rpc:myhost:/foo/bar"
                              "/rpc:myhost:/baz"
                              "/rpc:otherhost:/keep"
                              local-file)))
    (tramp-recentf-cleanup vec)
    ;; Only myhost entries should be removed; other remote and local kept.
    ;; recentf-cleanup abbreviates paths (~ for home), so compare
    ;; with abbreviate-file-name.
    (should (equal recentf-list
                   (list "/rpc:otherhost:/keep"
                         (abbreviate-file-name local-file))))))

(ert-deftest tramp-rpc-mock-test-recentf-cleanup-all ()
  "Test that upstream `tramp-recentf-cleanup-all' removes all remote entries.
tramp-rpc delegates recentf cleanup to the upstream function from
tramp-integration.el, registered on `tramp-cleanup-all-connections-hook'.
Uses an existing local path so `recentf-cleanup' does not also
discard it for being unreadable."
  :tags '(:non-essential :recentf)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (require 'recentf)
  ;; Use a path that actually exists so recentf-cleanup keeps it.
  ;; recentf-cleanup removes entries that match recentf-exclude OR
  ;; that fail the readability check.
  (let* ((local-file (expand-file-name "test/tramp-rpc-mock-tests.el"
                                       tramp-rpc-mock-test--project-root))
         (recentf-list (list "/rpc:host1:/foo"
                              "/ssh:host2:/bar"
                              local-file
                              "/rpc:host3:/baz")))
    (tramp-recentf-cleanup-all)
    ;; All remote entries should be removed, existing local file kept.
    ;; recentf-cleanup abbreviates paths (~ for home), so compare
    ;; with abbreviate-file-name.
    (should (equal recentf-list (list (abbreviate-file-name local-file))))))

;;; ============================================================================
;;; Tilde Quoting Tests
;;; ============================================================================

;; Verify that paths passed to shell commands use `tramp-shell-quote-argument'
;; (which preserves tilde expansion) rather than raw `shell-quote-argument'
;; (which escapes the tilde and breaks cd ~/... on the remote).

(ert-deftest tramp-rpc-mock-test-shell-quote-preserves-tilde ()
  "Test that `tramp-shell-quote-argument' preserves leading tilde."
  ;; tramp-shell-quote-argument is defined in tramp.el, always available.
  (should (string-prefix-p "~/" (tramp-shell-quote-argument "~/projects")))
  (should (equal "~" (tramp-shell-quote-argument "~")))
  (should (string-prefix-p "~user/" (tramp-shell-quote-argument "~user/dir"))))

(ert-deftest tramp-rpc-mock-test-shell-quote-tilde-vs-raw ()
  "Demonstrate the bug: raw `shell-quote-argument' escapes tilde."
  ;; raw shell-quote-argument escapes tilde (the bug this fix addresses)
  (should (string-prefix-p "\\~" (shell-quote-argument "~/projects")))
  ;; tramp-shell-quote-argument does not
  (should-not (string-prefix-p "\\~" (tramp-shell-quote-argument "~/projects"))))

(ert-deftest tramp-rpc-mock-test-shell-quote-absolute-path ()
  "Test that absolute paths are quoted normally by both functions."
  (should (equal (shell-quote-argument "/home/user/projects")
                 (tramp-shell-quote-argument "/home/user/projects"))))

(ert-deftest tramp-rpc-mock-test-shell-quote-path-with-spaces ()
  "Test that paths with spaces after tilde are properly quoted."
  (let ((quoted (tramp-shell-quote-argument "~/my projects")))
    ;; Tilde should be preserved
    (should (string-prefix-p "~/" quoted))
    ;; The space should be escaped (backslash-space on Unix)
    (should (string-match-p "\\\\ " quoted))))

;;; ============================================================================
;;; Remote Process Environment Tests (No server or SSH required)
;;; ============================================================================

(ert-deftest tramp-rpc-mock-test-remote-path-environment-uses-tramp-remote-path ()
  "Test that `tramp-remote-path' becomes a PATH environment entry."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((tramp-rpc-remote-path nil)
        (tramp-remote-path '(tramp-default-remote-path
                             "~/.cargo/bin"
                             tramp-own-remote-path
                             "/usr/bin"
                             unsupported-entry))
        (tramp-rpc--exec-path-cache (make-hash-table :test 'equal))
        (vec (make-tramp-file-name :method "rpc" :host "host" :user "user"
                                   :localname "/")))
    (cl-letf (((symbol-function 'tramp-rpc--fetch-default-remote-path)
               (lambda (_vec) '("/bin" "/usr/bin")))
              ((symbol-function 'tramp-rpc--fetch-remote-exec-path)
               (lambda (_vec) '("/home/user/.local/bin" "/usr/bin")))
              ((symbol-function 'tramp-get-home-directory)
               (lambda (_vec &optional _user) "/home/user")))
      (should (equal (tramp-rpc--remote-path-environment vec)
                     '(("PATH" . "/bin:/usr/bin:/home/user/.cargo/bin:/home/user/.local/bin")))))))

(ert-deftest tramp-rpc-mock-test-remote-path-environment-compat-override ()
  "Test deprecated `tramp-rpc-remote-path' still overrides `tramp-remote-path'."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((tramp-rpc-remote-path '(tramp-rpc-own-remote-path "/custom/bin"))
        (tramp-remote-path '("/ignored/bin"))
        (tramp-rpc--exec-path-cache (make-hash-table :test 'equal))
        (vec (make-tramp-file-name :method "rpc" :host "host" :user "user"
                                   :localname "/")))
    (cl-letf (((symbol-function 'tramp-rpc--fetch-remote-exec-path)
               (lambda (_vec) '("/login/bin"))))
      (should (equal (tramp-rpc--remote-path-environment vec)
                     '(("PATH" . "/login/bin:/custom/bin")))))))

(ert-deftest tramp-rpc-mock-test-fetch-remote-exec-path-ignores-banner ()
  "Test login shell PATH parsing ignores startup output before the marker."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (make-tramp-file-name :method "rpc" :host "host" :user "user"
                                   :localname "/")))
    (cl-letf (((symbol-function 'tramp-rpc--get-remote-login-shell)
               (lambda (_vec) "/bin/zsh"))
              ((symbol-function 'tramp-rpc--decode-output)
               (lambda (output _encoding) output))
              ((symbol-function 'tramp-rpc--call)
               (lambda (_vec method params)
                 (should (equal method "process.run"))
                 (let* ((args (alist-get 'args params))
                        (command (aref args 2)))
                   (should (equal (alist-get 'cmd params) "/bin/zsh"))
                   (should (equal (aref args 0) "-l"))
                   (should (equal (aref args 1) "-c"))
                   (should (string-match "echo \\([0-9a-f]+\\);" command))
                   `((exit_code . 0)
                     (stdout . ,(concat "banner text\n"
                                        (match-string 1 command)
                                        "\n/home/user/bin:/usr/bin\n")))))))
      (should (equal (tramp-rpc--fetch-remote-exec-path vec)
                     '("/home/user/bin" "/usr/bin"))))))

(ert-deftest tramp-rpc-mock-test-merge-environments-keeps-overrides ()
  "Test env merging preserves direnv/caller override semantics."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (should (equal (tramp-rpc--merge-environments
                  '(("PATH" . "/remote/bin") ("A" . "remote"))
                  '(("PATH" . "/direnv/bin") ("B" . "direnv"))
                  '(("GIT_INDEX_FILE" . "/tmp/index") ("A" . "caller")))
                 '(("PATH" . "/direnv/bin")
                   ("A" . "caller")
                   ("B" . "direnv")
                   ("GIT_INDEX_FILE" . "/tmp/index")))))

(ert-deftest tramp-rpc-mock-test-tramp-remote-process-environment-to-alist ()
  "Test conversion of dynamic `tramp-remote-process-environment' entries."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((tramp-remote-process-environment
         (append '("TERM=dumb" "PYTHONUNBUFFERED=1" "UNSET_ME" "EMPTY=")
                 (default-toplevel-value 'tramp-remote-process-environment))))
    (should (equal (tramp-rpc--tramp-remote-process-environment)
                   '(("TERM" . "dumb")
                     ("PYTHONUNBUFFERED" . "1")
                     ("EMPTY" . ""))))))

(ert-deftest tramp-rpc-mock-test-tramp-remote-process-environment-skips-baseline ()
  "Test TRAMP shell setup defaults are not passed as RPC child env vars."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((tramp-remote-process-environment
         (default-toplevel-value 'tramp-remote-process-environment)))
    (should-not (tramp-rpc--tramp-remote-process-environment))))

(ert-deftest tramp-rpc-mock-test-python-tramp-environment-handler ()
  "Test python.el TRAMP environment handler avoids shell refresh for RPC."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((body-env nil)
        (vec (make-tramp-file-name :method "rpc" :host "host" :user "user"
                                   :localname "/work/"))
        (tramp-remote-process-environment '("EXISTING=yes")))
    (tramp-rpc-handle-python-shell--tramp-with-environment
     vec
     '("TERM=dumb" "PYTHONUNBUFFERED=1")
     (lambda ()
       (setq body-env tramp-remote-process-environment)))
    (should (equal body-env
                   '("TERM=dumb" "PYTHONUNBUFFERED=1" "EXISTING=yes")))))

(ert-deftest tramp-rpc-mock-test-process-file-passes-path-and-unresolved-command ()
  "Test `process-file' searches commands with `tramp-remote-path' PATH."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((default-directory "/rpc:user@host:/work/")
        (captured-params nil)
        (tramp-rpc--exec-path-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'tramp-rpc--cached-remote-path)
               (lambda (_vec) '("/home/user/.cargo/bin" "/usr/bin" "/bin")))
              ((symbol-function 'tramp-rpc--get-direnv-environment)
               (lambda (&rest _) nil))
              ((symbol-function 'tramp-rpc--caller-environment)
               (lambda () nil))
              ((symbol-function 'tramp-rpc-magit--process-cache-lookup)
               (lambda (&rest _) nil))
              ((symbol-function 'tramp-rpc--decode-output)
               (lambda (output _encoding) output))
              ((symbol-function 'tramp-rpc--call)
               (lambda (_vec method params)
                 (should (equal method "process.run"))
                 (setq captured-params params)
                 '((exit_code . 0) (stdout . "") (stderr . "")))))
      (should (= (tramp-rpc-handle-process-file "rg" nil nil nil "pattern") 0))
      (should (equal (alist-get 'cmd captured-params) "rg"))
      (should (equal (alist-get 'args captured-params) ["pattern"]))
      (should (equal (assoc "PATH" (alist-get 'env captured-params))
                     '("PATH" . "/home/user/.cargo/bin:/usr/bin:/bin"))))))

(ert-deftest tramp-rpc-mock-test-process-file-passes-tramp-remote-environment ()
  "Test `process-file' forwards dynamic TRAMP remote process env vars."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((default-directory "/rpc:user@host:/work/")
        (captured-env nil)
        (tramp-remote-process-environment '("TERM=dumb" "PYTHONUNBUFFERED=1")))
    (cl-letf (((symbol-function 'tramp-rpc--cached-remote-path)
               (lambda (_vec) '("/usr/bin")))
              ((symbol-function 'tramp-rpc--get-direnv-environment)
               (lambda (&rest _) nil))
              ((symbol-function 'tramp-rpc--caller-environment)
               (lambda () nil))
              ((symbol-function 'tramp-rpc-magit--process-cache-lookup)
               (lambda (&rest _) nil))
              ((symbol-function 'tramp-rpc--decode-output)
               (lambda (output _encoding) output))
              ((symbol-function 'tramp-rpc--call)
               (lambda (_vec method params)
                 (should (equal method "process.run"))
                 (setq captured-env (alist-get 'env params))
                 '((exit_code . 0) (stdout . "") (stderr . "")))))
      (should (= (tramp-rpc-handle-process-file "python3" nil nil nil "-c" "pass") 0))
      (should (equal (assoc "TERM" captured-env) '("TERM" . "dumb")))
      (should (equal (assoc "PYTHONUNBUFFERED" captured-env)
                     '("PYTHONUNBUFFERED" . "1"))))))

;;; ============================================================================
;;; Direnv Cache Path Normalization Tests (No server or SSH required)
;;; ============================================================================

(ert-deftest tramp-rpc-mock-test-direnv-cache-key-deduplicates-tilde ()
  "Test that ~/project and /home/user/project produce the same cache key."
  :tags '(:direnv)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (make-tramp-file-name :method "rpc" :host "host" :user "user"
                                   :localname "/")))
    (cl-letf (((symbol-function 'tramp-get-home-directory)
               (lambda (_vec &optional _user) "/home/user")))
      (should (equal (tramp-rpc--direnv-cache-key vec "~/project")
                     (tramp-rpc--direnv-cache-key vec "/home/user/project"))))))

(ert-deftest tramp-rpc-mock-test-direnv-cache-no-duplicate-entries ()
  "Test that accessing ~/project and /home/user/project shares one cache entry."
  :tags '(:direnv)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((vec (make-tramp-file-name :method "rpc" :host "host" :user "user"
                                    :localname "/"))
         ;; Local direnv cache to avoid polluting global state
         (tramp-rpc--direnv-cache (make-hash-table :test 'equal))
         (fetch-count 0))
    (cl-letf (((symbol-function 'tramp-get-home-directory)
               (lambda (_vec &optional _user) "/home/user"))
              ((symbol-function 'tramp-rpc--fetch-direnv-environment)
               (lambda (_vec _dir)
                 (cl-incf fetch-count)
                 '(("PATH" . "/usr/bin")))))
      ;; First access via tilde path - should call fetch
      (tramp-rpc--get-direnv-environment vec "~/project")
      (should (= fetch-count 1))
      ;; Second access via absolute path - should hit the cache, not fetch again
      (tramp-rpc--get-direnv-environment vec "/home/user/project")
      (should (= fetch-count 1)))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

;;;###autoload
(defun tramp-rpc-mock-test-all ()
  "Run all mock tests."
  (interactive)
  (ert-run-tests-batch-and-exit "^tramp-rpc-mock-test"))

;;;###autoload
(defun tramp-rpc-mock-test-protocol ()
  "Run only protocol tests (no server needed)."
  (interactive)
  (ert-run-tests-interactively
   '(and (tag tramp-rpc-mock-test) (not (tag :server)))))

;;;###autoload
(defun tramp-rpc-mock-test-server ()
  "Run server tests."
  (interactive)
  (ert-run-tests-interactively '(tag :server)))

(provide 'tramp-rpc-mock-tests)
;;; tramp-rpc-mock-tests.el ends here
