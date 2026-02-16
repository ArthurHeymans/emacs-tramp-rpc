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
         (response-bytes (msgpack-encode response-data))
         (response (tramp-rpc-protocol-decode-response response-bytes)))
    (should (plist-get response :id))
    (should (equal (plist-get response :id) 1))
    (should (plist-get response :result))
    (should-not (tramp-rpc-protocol-error-p response))))

(ert-deftest tramp-rpc-mock-test-protocol-decode-error ()
  "Test MessagePack-RPC error response decoding."
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (let* ((response-data '((version . "2.0")
                          (id . 1)
                          (error . ((code . -32001) (message . "File not found")))))
         (response-bytes (msgpack-encode response-data))
         (response (tramp-rpc-protocol-decode-response response-bytes)))
    (should (tramp-rpc-protocol-error-p response))
    (should (= (tramp-rpc-protocol-error-code response) -32001))
    (should (equal (tramp-rpc-protocol-error-message response) "File not found"))))

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
    (should (= (tramp-rpc-protocol-read-length framed) (length payload)))
    ;; Try reading a complete message
    (let* ((response '((version . "2.0") (id . 42) (result . t)))
           (response-payload (msgpack-encode response))
           (response-framed (tramp-rpc-protocol--length-prefix response-payload))
           (read-result (tramp-rpc-protocol-try-read-message response-framed)))
      (should read-result)
      (should (consp read-result))
      (should (= (plist-get (car read-result) :id) 42))
      (should (equal (cdr read-result) "")))))

(ert-deftest tramp-rpc-mock-test-protocol-incomplete-message ()
  "Test handling of incomplete messages."
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (let* ((response '((version . "2.0") (id . 1) (result . t)))
         (payload (msgpack-encode response))
         (framed (tramp-rpc-protocol--length-prefix payload)))
    ;; Truncate the message
    (should-not (tramp-rpc-protocol-try-read-message (substring framed 0 3)))
    (should-not (tramp-rpc-protocol-try-read-message (substring framed 0 5)))))

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
  "Test mode integer to string conversion."
  ;; This tests the internal function if available
  (when (fboundp 'tramp-rpc--mode-to-string)
    ;; Regular file with 644 permissions
    (let ((mode-str (tramp-rpc--mode-to-string #o644 "file")))
      (should (equal mode-str "-rw-r--r--")))
    ;; Directory with 755 permissions
    (let ((mode-str (tramp-rpc--mode-to-string #o755 "directory")))
      (should (equal mode-str "drwxr-xr-x")))
    ;; Symlink
    (let ((mode-str (tramp-rpc--mode-to-string #o777 "symlink")))
      (should (string-prefix-p "l" mode-str)))))

;;; ============================================================================
;;; File Attributes Conversion Tests
;;; ============================================================================

(ert-deftest tramp-rpc-mock-test-convert-file-attributes ()
  "Test conversion of stat result to Emacs attributes."
  (when (fboundp 'tramp-rpc--convert-file-attributes)
    (let* ((stat-result '((type . "file")
                          (size . 1234)
                          (mode . 420)  ; #o644
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
    ;; Set buffer to unibyte for binary protocol
    (with-current-buffer tramp-rpc-mock-test-server-buffer
      (set-buffer-multibyte nil))
    (setq tramp-rpc-mock-test-server-process
          (let ((process-connection-type nil))  ; Use pipes
            (start-process "test-server" tramp-rpc-mock-test-server-buffer server)))
    (set-process-query-on-exit-flag tramp-rpc-mock-test-server-process nil)
    ;; Use binary coding for MessagePack protocol
    (set-process-coding-system tramp-rpc-mock-test-server-process 'binary 'binary)
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
          (let ((result (tramp-rpc-protocol-try-read-message
                         (buffer-substring (point-min) (point-max)))))
            (when result
              (setq response (car result))
              ;; Replace buffer contents with remaining data
              (erase-buffer)
              (insert (cdr result))))
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
          (should (assoc 'home result))))
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
