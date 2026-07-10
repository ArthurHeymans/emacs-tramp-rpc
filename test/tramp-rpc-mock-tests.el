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

;; Load tramp-rpc modules and the supported TRAMP checkout when available.
(let ((lisp-dir (expand-file-name "lisp" tramp-rpc-mock-test--project-root))
      (tramp-lisp-dir (expand-file-name
                       "lisp"
                       (or (getenv "TRAMP_SOURCE")
                           (expand-file-name "src/tramp" (getenv "HOME"))))))
  (add-to-list 'load-path lisp-dir)
  (when (file-directory-p tramp-lisp-dir)
    (add-to-list 'load-path tramp-lisp-dir)))

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

(unless tramp-rpc-mock-test--msgpack-available
  (error "tramp-rpc mock tests require msgpack.el"))

(require 'tramp-rpc-protocol)

(defun tramp-rpc-mock-test--bytes-string (data)
  "Return DATA as a plain byte string, unwrapping MessagePack bin."
  (if (and tramp-rpc-mock-test--msgpack-available (msgpack-bin-p data))
      (msgpack-bin-string data)
    data))

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
                                                ((error (code . -32003)
                                                        (message . "Error")
                                                        (data (os_errno . 20)))))))))
         (decoded (tramp-rpc-protocol-decode-batch-response response-plist)))
    (should (listp decoded))
    (should (= (length decoded) 2))
    ;; First result is success
    (should (eq (car decoded) t))
    ;; Second is error, including structured errno data.
    (should (plist-get (cadr decoded) :error))
    (should (= 20 (alist-get 'os_errno (plist-get (cadr decoded) :data))))))

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

(ert-deftest tramp-rpc-mock-test-runner-protocol-selector ()
  "The protocol runner selector covers the eight protocol regressions."
  (should (= (length (ert-select-tests "^tramp-rpc-mock-test-protocol" t)) 8)))

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
            (list :error (tramp-rpc-protocol-error-message response)
                  :code (tramp-rpc-protocol-error-code response)
                  :data (tramp-rpc-protocol-error-data response))
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
          (should (member (alist-get 'watcher result)
                          '("inotify" "fsevent" "kqueue" "poll"
                            "windows" "null" "unknown")))
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
              (should (msgpack-bin-p content))
              (should (equal (msgpack-bin-string content) "hello world"))))

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

(ert-deftest tramp-rpc-mock-test-server-write-offset-preserves-suffix ()
  "The file.write offset field replaces bytes without truncating the suffix."
  :tags '(:server)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let ((path (expand-file-name "offset" tramp-rpc-mock-test-temp-dir)))
          (tramp-rpc-mock-test--rpc-call
           "file.write" `((path . ,(encode-coding-string path 'utf-8))
                          (content . "abcdef")))
          (tramp-rpc-mock-test--rpc-call
           "file.write" `((path . ,(encode-coding-string path 'utf-8))
                          (content . "XY")
                          (offset . 2)))
          (let* ((result (tramp-rpc-mock-test--rpc-call
                          "file.read" `((path . ,(encode-coding-string path 'utf-8)))))
                 (content (alist-get 'content result)))
            (should (equal (msgpack-bin-string content) "abXYef")))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-write-offset-creates-zero-filled-file ()
  "An offset write creates a missing file with its leading hole zero-filled."
  :tags '(:server)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let ((path (expand-file-name "offset-missing" tramp-rpc-mock-test-temp-dir)))
          (tramp-rpc-mock-test--rpc-call
           "file.write" `((path . ,(encode-coding-string path 'utf-8))
                          (content . "XY")
                          (offset . 4)))
          (let* ((result (tramp-rpc-mock-test--rpc-call
                          "file.read" `((path . ,(encode-coding-string path 'utf-8)))))
                 (content (alist-get 'content result)))
            (should (equal (msgpack-bin-string content) "\0\0\0\0XY")))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-rename-dangling-symlink-no-overwrite ()
  "No-overwrite rename treats a dangling symlink as an existing destination."
  :tags '(:server)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let ((src (expand-file-name "rename-src" tramp-rpc-mock-test-temp-dir))
              (dest (expand-file-name "rename-dest" tramp-rpc-mock-test-temp-dir)))
          (with-temp-file src (insert "source"))
          (make-symbolic-link "missing-target" dest)
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "file.rename"
                         `((src . ,(encode-coding-string src 'utf-8))
                           (dest . ,(encode-coding-string dest 'utf-8))))))
            (should (= (plist-get result :code) -32003))
            (should (= (alist-get 'os_errno (plist-get result :data)) 17))
            (should (file-symlink-p dest))
            (should (file-exists-p src)))))
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
            (let ((names (mapcar (lambda (e)
                                    (tramp-rpc-mock-test--bytes-string
                                     (alist-get 'name e)))
                                  result)))
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
            (should (msgpack-bin-p stdout))
            (should (string-match-p "hello world" (msgpack-bin-string stdout))))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-process-spawn-enoent-is-classified ()
  "Local process.run distinguishes a missing executable from a missing cwd."
  :tags '(:server :process)
  (skip-unless tramp-rpc-mock-test--msgpack-available)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let ((missing-command
               (tramp-rpc-mock-test--rpc-call
                "process.run" `((cmd . "/definitely/not/tramp-rpc-command"))))
              (missing-cwd
               (tramp-rpc-mock-test--rpc-call
                "process.run" `((cmd . "/bin/true")
                                 (cwd . ,(expand-file-name "missing-cwd"
                                                            tramp-rpc-mock-test-temp-dir))))))
          (should (= (plist-get missing-command :code) -32004))
          (should (= (alist-get 'os_errno (plist-get missing-command :data)) 2))
          (should (alist-get 'spawn_not_found (plist-get missing-command :data)))
          (should (= (plist-get missing-cwd :code) -32004))
          (should (= (alist-get 'os_errno (plist-get missing-cwd :data)) 2))
          (should-not (alist-get 'spawn_not_found (plist-get missing-cwd :data)))))
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

;; Load the full backend.  Do not turn an unsupported TRAMP into skipped
;; tests: the runner must fail before it can claim a mock-test success.
(require 'tramp)
(unless (version<= "2.8.1.4" tramp-version)
  (error "tramp-rpc mock tests require Tramp >= 2.8.1.4, but %s is loaded; set TRAMP_SOURCE to a supported checkout"
         tramp-version))
(require 'tramp-rpc)
(defconst tramp-rpc-mock-test--tramp-rpc-loaded t
  "The full TRAMP-RPC backend was loaded successfully.")

(ert-deftest tramp-rpc-mock-test-file-executable-root ()
  "Root requires an execute bit, rather than bypassing all mode checks."
  (let ((attrs '(nil 1 1 1 0 0 0 0 "----------")))
    (should-not (tramp-rpc--mode-executable-p "----------" 0 0 attrs nil))
    (should (tramp-rpc--mode-executable-p "------x---" 0 0 attrs nil))))

(ert-deftest tramp-rpc-mock-test-directory-count-semantics ()
  "Both directory handlers share Emacs COUNT semantics."
  (let ((entries '("a" "b" "c")))
    (should (equal (tramp-rpc--apply-directory-count entries nil) entries))
    (should (equal (tramp-rpc--apply-directory-count entries 2) '("a" "b")))
    (should-not (tramp-rpc--apply-directory-count entries 0))
    (dolist (count '(-1 "invalid"))
      (should-error (tramp-rpc--apply-directory-count entries count)
                    :type 'wrong-type-argument))))

(ert-deftest tramp-rpc-mock-test-pipelined-timeout-signals-error ()
  "A live connection with no response reaches the pipeline timeout branch."
  (let* ((buffer (generate-new-buffer " *tramp-rpc-pipeline-test*"))
         (process (start-process "tramp-rpc-pipeline-test" buffer "sh" "-c" "sleep 2"))
         (vec (tramp-dissect-file-name "/rpc:mock:/tmp/"))
         (timeout 0.15)
         (started (float-time)))
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
                   (lambda (_vec) (list :process process :buffer buffer))))
          (should (process-live-p process))
          (condition-case err
              (progn
                (tramp-rpc--receive-responses vec '(1) timeout)
                (error "Expected pipelined response timeout"))
            (remote-file-error
             (should (string-match-p "Timeout" (error-message-string err)))))
          (should (>= (- (float-time) started) (* timeout 0.8)))
          (should (process-live-p process)))
      (when (process-live-p process) (delete-process process))
      (kill-buffer buffer))))

(declare-function tramp-rpc-magit--file-exists-in-ancestor-scan
                  "tramp-rpc-magit" (filename scan))
(declare-function tramp-rpc-magit--get-cache-key "tramp-rpc-magit" (vec directory))
(declare-function tramp-rpc-magit--process-cache-key "tramp-rpc-magit" (&rest args))
(declare-function tramp-rpc-magit--process-cache-lookup "tramp-rpc-magit" (program args))
(declare-function tramp-rpc-magit--process-cache-store "tramp-rpc-magit" (program args exit-code stdout))
(declare-function tramp-rpc-magit--cache-file-truename "tramp-rpc-magit" (vec localname result))
(declare-function tramp-rpc-handle-magit-status-setup-buffer "tramp-rpc-magit" (&optional directory))
(declare-function tramp-rpc-handle-dired-compress-file "tramp-rpc" (file))
(declare-function tramp-rpc-handle-file-regular-p "tramp-rpc" (filename))
(declare-function tramp-rpc--clear-file-caches-for-connection "tramp-rpc-magit" (vec))
(declare-function tramp-rpc--invalidate-cache-for-subtree "tramp-rpc-magit" (directory))
(defvar tramp-rpc-magit-disable-remote-diff-tab-width-detection)
(defvar tramp-rpc-magit--allow-process-cache)
(defvar tramp-rpc-magit--prefetch-directory)
(defvar tramp-rpc-magit--process-caches)
(defvar tramp-rpc-magit--ancestors-cache)
(defvar tramp-rpc-magit--ancestor-scan-caches)

(defconst tramp-rpc-mock-test--tramp-rpc-magit-loaded
  (progn (require 'tramp-rpc-magit) t)
  "The TRAMP-RPC Magit support was loaded successfully.")

(defun tramp-rpc-mock-test--clear-hash-tables (&rest symbols)
  "Clear the hash tables stored in SYMBOLS."
  (dolist (symbol symbols)
    (when-let* ((value (and (boundp symbol) (symbol-value symbol))))
      (when (hash-table-p value)
        (clrhash value)))))

(defun tramp-rpc-mock-test--reset-state ()
  "Remove state left by an earlier mock test."
  (tramp-rpc-mock-test--stop-server)
  ;; Resource cleanup must run while its descriptor and watch tables still
  ;; identify those resources; clearing them first leaks synthetic watches.
  (tramp-rpc-cleanup-all-connections)
  (tramp-cleanup-all-connections)
  (tramp-rpc-mock-test--clear-hash-tables
   'tramp-rpc--process-write-queues
   'tramp-rpc--direnv-cache
   'tramp-rpc--direnv-available-cache
   'tramp-rpc--exec-path-cache
   'tramp-rpc--login-shell-cache
   'tramp-rpc-magit--process-caches
   'tramp-rpc-magit--ancestor-scan-caches)
  (setq tramp-rpc-magit--ancestors-cache nil
        tramp-rpc-magit--prefetch-directory nil
        tramp-rpc-magit--allow-process-cache nil))

(defvar tramp-rpc-mock-test--isolate-tests nil
  "Non-nil while a TRAMP-RPC mock selector is running.")

(defun tramp-rpc-mock-test--run-isolated (run-test test)
  "Run a selected mock TEST with no state shared with other mock tests."
  (if (and tramp-rpc-mock-test--isolate-tests
           (string-prefix-p "tramp-rpc-mock-test"
                            (symbol-name (ert-test-name test))))
      (unwind-protect
          (progn
            (tramp-rpc-mock-test--reset-state)
            (funcall run-test test))
        (tramp-rpc-mock-test--reset-state))
    (funcall run-test test)))

(advice-add 'ert-run-test :around #'tramp-rpc-mock-test--run-isolated)

(ert-deftest tramp-rpc-mock-test-reset-state-cleans-file-notify-resources ()
  "State reset removes descriptors before clearing their tracking tables."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (require 'filenotify)
  (let* ((tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (tramp-rpc--watched-directories (make-hash-table :test 'equal))
         (file-notify-descriptors (make-hash-table :test 'eq))
         (vec (tramp-dissect-file-name "/rpc:mock:/tmp/"))
         (watch-key (format "%s:/tmp/" (tramp-rpc--connection-key-string vec)))
         (descriptor (tramp-rpc--make-file-notify-descriptor
                      vec "/rpc:mock:/tmp/" "/tmp/"))
         stopped)
    (unwind-protect
        (progn
          (puthash descriptor (list :watch-key watch-key :directory "/rpc:mock:/tmp/")
                   tramp-rpc--file-notify-descriptors)
          (puthash watch-key '(:count 1 :owned t)
                   tramp-rpc--file-notify-watch-counts)
          (puthash descriptor
                   (file-notify--watch-make
                    "/rpc:mock:/tmp/" nil
                    (lambda (event) (push event stopped)))
                   file-notify-descriptors)
          (tramp-rpc-mock-test--reset-state)
          (should-not (process-live-p descriptor))
          (should-not (gethash descriptor file-notify-descriptors))
          (should (equal stopped `((,descriptor stopped "/rpc:mock:/tmp/")))))
      (tramp-rpc--delete-file-notify-descriptor-process descriptor))))

(ert-deftest tramp-rpc-mock-test-runner-rejects-empty-and-skipped-selectors ()
  "The shell runner rejects unsupported Tramp, zero selections, and all skips."
  (let* ((temporary-directory (make-temp-file "tramp-rpc-runner-test-" t))
         (runner (expand-file-name "test/run-tests.sh"
                                   tramp-rpc-mock-test--project-root))
         (emacs (or (executable-find "emacs")
                    (error "Cannot find Emacs executable")))
         (wrapper (expand-file-name "emacs-wrapper" temporary-directory))
         (skipped (expand-file-name "skipped.el" temporary-directory))
         (empty (expand-file-name "empty.el" temporary-directory))
         (unsupported (expand-file-name "unsupported" temporary-directory)))
    (unwind-protect
        (progn
          (with-temp-file wrapper
            (insert "#!/bin/bash\nargs=()\nfor arg in \"$@\"; do\n"
                    "  if [[ $arg == */test/tramp-rpc-mock-tests.el ]]; then\n"
                    "    args+=(\"$RUNNER_TEST_FILE\")\n  else\n"
                    "    args+=(\"$arg\")\n  fi\ndone\n"
                    "exec \"$REAL_EMACS\" \"${args[@]}\"\n"))
          (set-file-modes wrapper #o755)
          (with-temp-file skipped
            (insert "(require 'ert)\n"
                    "(dotimes (n 8)\n"
                    "  (eval `(ert-deftest ,(intern (format \"tramp-rpc-mock-test-protocol-skip-%d\" n)) () (ert-skip \"simulated\"))))\n"))
          (with-temp-file empty (insert "(require 'ert)\n"))
          (make-directory (expand-file-name "lisp" unsupported) t)
          (with-temp-file (expand-file-name "lisp/tramp.el" unsupported)
            (insert "(defvar tramp-version \"0\")\n(provide 'tramp)\n"))
          (cl-labels
              ((run (test-file &optional tramp-source)
                 (with-temp-buffer
                   (let ((process-environment
                          (append (list (concat "EMACS=" wrapper)
                                        (concat "REAL_EMACS=" emacs)
                                        (concat "RUNNER_TEST_FILE=" test-file))
                                  (when tramp-source
                                    (list (concat "TRAMP_SOURCE=" tramp-source)))
                                  (cl-remove-if
                                   (lambda (entry)
                                     (or (string-prefix-p "EMACS=" entry)
                                         (string-prefix-p "REAL_EMACS=" entry)
                                         (string-prefix-p "RUNNER_TEST_FILE=" entry)
                                         (string-prefix-p "TRAMP_SOURCE=" entry)))
                                   process-environment))))
                     (list (call-process runner nil t nil "--protocol")
                           (buffer-string))))))
            (pcase-let ((`(,status ,output) (run skipped)))
              (should (/= status 0))
              (should (string-match-p
                       "ERT counts: selected=8 executed=0 skipped=8" output)))
            (pcase-let ((`(,status ,output) (run empty)))
              (should (/= status 0))
              (should (string-match-p "selected zero tests" output)))
            (pcase-let ((`(,status ,output) (run skipped unsupported)))
              (should (/= status 0))
              (should (string-match-p "require Tramp >= 2.8.1.4" output)))))
      (delete-directory temporary-directory t))))

(ert-deftest tramp-rpc-mock-test-dired-compress-file-registered ()
  "`dired-compress-file' is handled for TRAMP-RPC files."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (should (eq (alist-get 'dired-compress-file tramp-rpc-file-name-handler-alist)
              'tramp-rpc-handle-dired-compress-file)))

(ert-deftest tramp-rpc-mock-test-dired-compress-file-delegates-to-real-handler ()
  "TRAMP-RPC reuses Emacs' `dired-compress-file' implementation."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((calls nil))
    (cl-letf (((symbol-function 'tramp-run-real-handler)
               (lambda (operation args)
                 (push (list operation args) calls)
                 "/rpc:mock:/tmp/file.gz")))
      (should (equal (tramp-rpc-handle-dired-compress-file "/rpc:mock:/tmp/file")
                     "/rpc:mock:/tmp/file.gz"))
      (should (equal calls
                     '((dired-compress-file ("/rpc:mock:/tmp/file"))))))))

(ert-deftest tramp-rpc-mock-test-ancestor-scan-parent-falls-through ()
  "Closest-only ancestor scan must not cache false negatives above the hit."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-magit-loaded)
  (let ((scan '((".editorconfig" . "/repo/sub"))))
    (should (eq (tramp-rpc-magit--file-exists-in-ancestor-scan
                 "/ssh:mock:/repo/sub/.editorconfig" scan)
                t))
    ;; The closest hit proves that deeper descendants before /repo/sub do not
    ;; contain this marker.
    (should-not (tramp-rpc-magit--file-exists-in-ancestor-scan
                 "/ssh:mock:/repo/sub/nested/.editorconfig" scan))
    ;; It does not prove anything about ancestors above /repo/sub.  A parent
    ;; lookup must fall back to a real stat so a parent marker is not hidden by
    ;; the child marker cached from the closest-only scan.
    (should (eq (tramp-rpc-magit--file-exists-in-ancestor-scan
                 "/ssh:mock:/repo/.editorconfig" scan)
                'not-cached))))

(defmacro tramp-rpc-mock-test--with-git-process-cache (&rest body)
  "Run BODY with an isolated Magit process cache."
  (declare (indent 0) (debug t))
  `(let* ((default-directory "/ssh:mock:/repo/")
          (vec (tramp-dissect-file-name default-directory))
          (cache (make-hash-table :test 'equal))
          (tramp-rpc-magit--process-caches (make-hash-table :test 'equal))
          (tramp-rpc-magit--prefetch-directory default-directory)
          (process-environment (default-toplevel-value 'process-environment)))
     (cl-letf (((symbol-function 'tramp-rpc--connection-key)
                (lambda (_vec) '("rpc" nil "mock" nil))))
       (puthash (tramp-rpc-magit--get-cache-key vec default-directory)
                (list :time (float-time) :cache cache)
                tramp-rpc-magit--process-caches)
       ,@body)))

(ert-deftest tramp-rpc-mock-test-git-process-cache-requires-opt-in ()
  "Prefetched git output is ignored outside Magit's cache window."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-magit-loaded)
  (tramp-rpc-mock-test--with-git-process-cache
    (puthash (tramp-rpc-magit--process-cache-key "status" "--porcelain")
             '(0 . "cached") cache)
    (let ((tramp-rpc-magit--allow-process-cache nil))
      (should-not (tramp-rpc-magit--process-cache-lookup
                   "git" '("status" "--porcelain"))))))

(ert-deftest tramp-rpc-mock-test-git-process-cache-strips-safe-prefixes ()
  "Cache lookup ignores Magit's cache-neutral git prefixes."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-magit-loaded)
  (tramp-rpc-mock-test--with-git-process-cache
    (puthash (tramp-rpc-magit--process-cache-key "status" "--porcelain")
             '(0 . "cached") cache)
    (let ((tramp-rpc-magit--allow-process-cache t))
      (should (equal (tramp-rpc-magit--process-cache-lookup
                      "git" '("--no-pager" "--literal-pathspecs"
                              "-c" "core.preloadIndex=true"
                              "status" "--porcelain"))
                     '(0 . "cached"))))))

(ert-deftest tramp-rpc-mock-test-git-process-cache-rejects-semantic-prefixes ()
  "Cache lookup misses when git prefixes change repository semantics."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-magit-loaded)
  (tramp-rpc-mock-test--with-git-process-cache
    (puthash (tramp-rpc-magit--process-cache-key "status") '(0 . "cached") cache)
    (let ((tramp-rpc-magit--allow-process-cache t))
      (should-not (tramp-rpc-magit--process-cache-lookup
                   "git" '("-C" "/tmp" "status")))
      (should-not (tramp-rpc-magit--process-cache-lookup
                   "git" '("--glob-pathspecs" "status")))
      (should-not (tramp-rpc-magit--process-cache-lookup
                   "git" '("-c" "status.relativePaths=false" "status"))))))

(ert-deftest tramp-rpc-mock-test-git-process-cache-rejects-git-env ()
  "Cache lookup misses when dynamic GIT_* environment differs."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-magit-loaded)
  (tramp-rpc-mock-test--with-git-process-cache
    (puthash (tramp-rpc-magit--process-cache-key "status") '(0 . "cached") cache)
    (let ((tramp-rpc-magit--allow-process-cache t)
          (process-environment (cons "GIT_INDEX_FILE=/tmp/other-index"
                                     process-environment)))
      (should-not (tramp-rpc-magit--process-cache-lookup "git" '("status"))))))

(ert-deftest tramp-rpc-mock-test-git-process-cache-does-not-store-mutators ()
  "Mutating git commands are never stored in the process cache."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-magit-loaded)
  (tramp-rpc-mock-test--with-git-process-cache
    (let ((tramp-rpc-magit--allow-process-cache t))
      (tramp-rpc-magit--process-cache-store
       "git" '("update-index" "--refresh") 0 "")
      (should-not (gethash (tramp-rpc-magit--process-cache-key
                            "update-index" "--refresh")
                           cache)))))

(ert-deftest tramp-rpc-mock-test-git-process-cache-does-not-reuse-subdir ()
  "A repo-root process cache is not reused for a different cwd."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-magit-loaded)
  (tramp-rpc-mock-test--with-git-process-cache
    (puthash (tramp-rpc-magit--process-cache-key "status") '(0 . "cached") cache)
    (let ((default-directory "/ssh:mock:/repo/sub/")
          (tramp-rpc-magit--allow-process-cache t))
      (should-not (tramp-rpc-magit--process-cache-lookup "git" '("status"))))))

(ert-deftest tramp-rpc-mock-test-file-regular-p-uses-stat ()
  "`file-regular-p' uses one following file.stat."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let (calls)
    (cl-letf (((symbol-function 'tramp-rpc--call-file-stat)
               (lambda (_vec localname &optional lstat)
                 (push (list localname lstat) calls)
                 '((type . "file"))))
              ((symbol-function 'tramp-handle-file-regular-p)
               (lambda (_filename) (ert-fail "generic handler called"))))
      (should (tramp-rpc-handle-file-regular-p "/rpc:mock:/tmp/file"))
      (should (equal (nreverse calls) '(("/tmp/file" nil)))))))

(ert-deftest tramp-rpc-mock-test-connection-cache-clear-clears-ancestor-scans ()
  "Connection cache clearing also drops ancestor marker scans."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-magit-loaded)
  (let ((tramp-rpc-magit--ancestors-cache '((".git" . "/repo")))
        (tramp-rpc-magit--ancestor-scan-caches (make-hash-table :test 'equal))
        (vec (tramp-dissect-file-name "/ssh:mock:/repo/")))
    (puthash '("ssh:mock" . "/repo/") '((".git" . "/repo"))
             tramp-rpc-magit--ancestor-scan-caches)
    (cl-letf (((symbol-function 'tramp-flush-directory-properties) #'ignore))
      (tramp-rpc--clear-file-caches-for-connection vec))
    (should-not tramp-rpc-magit--ancestors-cache)
    (should (= 0 (hash-table-count tramp-rpc-magit--ancestor-scan-caches)))))

(ert-deftest tramp-rpc-mock-test-subtree-invalidation-flushes-tramp-properties ()
  "Subtree invalidation flushes descendant TRAMP file properties."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-magit-loaded)
  (let* ((child "/ssh:mock:/repo/child")
         (tramp-rpc--file-exists-cache (make-hash-table :test 'equal))
         (tramp-rpc--file-truename-cache (make-hash-table :test 'equal))
         (tramp-rpc--file-stat-cache (make-hash-table :test 'equal))
         flushed)
    (puthash child (cons (float-time) t) tramp-rpc--file-exists-cache)
    (puthash (cons child nil) (cons (float-time) '((type . "file")))
             tramp-rpc--file-stat-cache)
    (cl-letf (((symbol-function 'tramp-flush-file-properties)
               (lambda (_vec localname) (push (list 'file localname) flushed)))
              ((symbol-function 'tramp-flush-directory-properties)
               (lambda (_vec localname) (push (list 'directory localname) flushed))))
      (tramp-rpc--invalidate-cache-for-subtree "/ssh:mock:/repo/"))
    (should-not (gethash child tramp-rpc--file-exists-cache))
    (should-not (gethash (cons child nil) tramp-rpc--file-stat-cache))
    (should (member '(file "/repo/child") flushed))
    (should (member '(directory "/repo/child") flushed))))

(ert-deftest tramp-rpc-mock-test-magit-status-setup-clears-requested-directory ()
  "`magit-status-setup-buffer' clears metadata for its DIRECTORY argument."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-magit-loaded)
  (let ((default-directory "/ssh:other:/else/")
        (tramp-rpc-magit-disable-remote-diff-tab-width-detection nil)
        cleared)
    (cl-letf (((symbol-function 'tramp-rpc--clear-file-metadata-caches)
               (lambda () (push default-directory cleared)))
              ((symbol-function 'tramp-run-real-handler)
               (lambda (_operation _args) 'ok)))
      (tramp-rpc-handle-magit-status-setup-buffer "/ssh:mock:/repo"))
    (should (equal cleared '("/ssh:mock:/repo/")))))

(ert-deftest tramp-rpc-mock-test-magit-cache-file-truename-accepts-bin-result ()
  "Magit metadata prefetch accepts bin file.truename results."
  (skip-unless (and tramp-rpc-mock-test--tramp-rpc-magit-loaded
                   tramp-rpc-mock-test--msgpack-available))
  (let ((tramp-rpc--file-truename-cache (make-hash-table :test 'equal))
        (vec (tramp-dissect-file-name "/rpc:mock:/repo/")))
    (cl-letf (((symbol-function 'tramp-rpc--decode-string)
               #'tramp-rpc-mock-test--bytes-string))
      (tramp-rpc-magit--cache-file-truename
       vec "/repo" (msgpack-bin-make "/home/arthur/src/doom")))
    (should (equal (tramp-rpc--cache-get
                    tramp-rpc--file-truename-cache "/rpc:mock:/repo")
                   "/rpc:mock:/home/arthur/src/doom"))))

(defun tramp-rpc-mock-test--sudo-helper-available-p ()
  "Return non-nil when the sudo path helpers needed by this test are available."
  (and (require 'tramp-cmds nil t)
       (fboundp 'tramp-file-name-with-sudo)))

(ert-deftest tramp-rpc-mock-test-file-notify-descriptor-monitor-name ()
  "File notification descriptors expose library and monitor names for tests."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((vec (tramp-dissect-file-name "/rpc:mock:/tmp/"))
         descriptor)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--system-info)
                   (lambda (_vec) '((os . "linux") (watcher . "inotify")))))
          (setq descriptor
                (tramp-rpc--make-file-notify-descriptor
                 vec "/rpc:mock:/tmp/" "/tmp/"))
          (should (string-equal (process-name descriptor) "tramp-rpc"))
          (should (eq (tramp-get-connection-property
                       descriptor "file-monitor" nil)
                      'TrampRPCinotify)))
      (when descriptor
        (tramp-rpc--delete-file-notify-descriptor-process descriptor)))))

(ert-deftest tramp-rpc-mock-test-file-notify-symlink-requests-nofollow-watch ()
  "File notification watches on symlinks request nofollow server watches."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (require 'filenotify)
  (let* ((tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (tramp-rpc--watched-directories (make-hash-table :test 'equal))
         (directory "/rpc:mock:/tmp/link")
         (vec (tramp-dissect-file-name directory))
         (watch-key (format "%s:%s" (tramp-rpc--connection-key-string vec)
                            "/tmp/link"))
         calls
         descriptor)
    (unwind-protect
        (cl-letf (((symbol-function 'file-symlink-p)
                   (lambda (_filename) "/tmp/real"))
                  ((symbol-function 'tramp-rpc--system-info)
                   (lambda (_vec) '((os . "linux") (watcher . "inotify"))))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method params)
                     (push (list method params) calls)
                     '((path . "/tmp/link"))))
                  ((symbol-function 'tramp-rpc-unwatch-directory)
                   (lambda (directory)
                     (push (list "watch.remove" directory) calls))))
          (setq descriptor
                (tramp-rpc-handle-file-notify-add-watch
                 directory '(change attribute-change) #'ignore))
          (let ((entry (gethash watch-key tramp-rpc--file-notify-watch-counts)))
            (should entry)
            (should-not (plist-get entry :synthetic))
            (should (plist-get entry :owned))
            (should (string= (plist-get entry :canonical-directory)
                             "/rpc:mock:/tmp/link")))
          (let ((watch-add (car (last calls))))
            (should (equal (car watch-add) "watch.add"))
            (should (eq (alist-get 'nofollow (cadr watch-add)) t))
            (should (eq (alist-get 'recursive (cadr watch-add))
                        :msgpack-false)))
          (tramp-rpc-handle-file-notify-rm-watch descriptor)
          (setq descriptor nil)
          (should (equal (caar calls) "watch.remove")))
      (when descriptor
        (remhash descriptor tramp-rpc--file-notify-descriptors)
        (tramp-rpc--delete-file-notify-descriptor-process descriptor)))))

(ert-deftest tramp-rpc-mock-test-file-notify-symlink-falls-back-to-synthetic ()
  "Symlink file notification watches stay synthetic without server support."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (require 'filenotify)
  (let* ((tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (tramp-rpc--watched-directories (make-hash-table :test 'equal))
         (directory "/rpc:mock:/tmp/link")
         (vec (tramp-dissect-file-name directory))
         (watch-key (format "%s:%s" (tramp-rpc--connection-key-string vec)
                            "/tmp/link"))
         calls
         descriptor)
    (unwind-protect
        (cl-letf (((symbol-function 'file-symlink-p)
                   (lambda (_filename) "/tmp/real"))
                  ((symbol-function 'tramp-rpc--system-info)
                   (lambda (_vec) '((os . "linux") (watcher . "inotify"))))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method _params)
                     (push method calls)
                     (signal 'remote-file-error '("nofollow unsupported"))))
                  ((symbol-function 'tramp-rpc-unwatch-directory)
                   (lambda (_directory)
                     (push "watch.remove" calls))))
          (setq descriptor
                (tramp-rpc-handle-file-notify-add-watch
                 directory '(change attribute-change) #'ignore))
          (let ((entry (gethash watch-key tramp-rpc--file-notify-watch-counts)))
            (should entry)
            (should (plist-get entry :synthetic))
            (should-not (plist-get entry :owned))
            (should-not (plist-get entry :canonical-directory)))
          (should (equal calls '("watch.add")))
          (tramp-rpc-handle-file-notify-rm-watch descriptor)
          (setq descriptor nil)
          (should (equal calls '("watch.add"))))
      (when descriptor
        (remhash descriptor tramp-rpc--file-notify-descriptors)
        (tramp-rpc--delete-file-notify-descriptor-process descriptor)))))

(ert-deftest tramp-rpc-mock-test-file-notify-suppression-still-dispatches ()
  "fs.events suppression skips cache work but still dispatches file notifications."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((vec (tramp-dissect-file-name "/rpc:mock:/tmp/"))
         (proc (make-process :name "tramp-rpc-fs-events-test"
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
                     (lambda (action path &optional path1 cookie)
                       (push (list action path path1 cookie) dispatches))))
            (tramp-rpc--handle-notification
             proc "fs.events"
             '((events . (((action . "changed")
                            (path . "/tmp/changed"))))))
            (should (= status-clears 0))
            (should-not invalidations)
            (should (equal dispatches
                           '(("changed" "/rpc:mock:/tmp/changed" nil nil))))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest tramp-rpc-mock-test-file-notify-unsuppressed-events-invalidate-caches ()
  "Unsuppressed fs.events clear status/cache state and dispatch notifications."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((vec (tramp-dissect-file-name "/rpc:mock:/tmp/"))
         (proc (make-process :name "tramp-rpc-fs-events-unsuppressed-test"
                             :buffer nil
                             :command '("cat")
                             :connection-type 'pipe
                             :noquery t))
         (status-clears 0)
         (invalidations nil)
         (connection-clears nil)
         (dispatches nil)
         (tramp-rpc--suppress-fs-notifications nil))
    (unwind-protect
        (progn
          (process-put proc :tramp-rpc-vec vec)
          (cl-letf (((symbol-function 'tramp-rpc-magit--clear-status-cache)
                     (lambda () (cl-incf status-clears)))
                    ((symbol-function 'tramp-rpc--invalidate-cache-for-path)
                     (lambda (path) (push path invalidations)))
                    ((symbol-function 'tramp-rpc--clear-file-caches-for-connection)
                     (lambda (clear-vec) (push clear-vec connection-clears)))
                    ((symbol-function 'tramp-rpc--file-notify-dispatch)
                     (lambda (action path &optional path1 cookie)
                       (push (list action path path1 cookie) dispatches))))
            (tramp-rpc--handle-notification
             proc "fs.events"
             '((events . (((action . "changed")
                            (path . "/tmp/changed"))
                           ((action . "renamed")
                            (path . "/tmp/old")
                            (path1 . "/tmp/new"))
                           ((action . "rescan"))))))
            (should (= status-clears 1))
            (should (member "/rpc:mock:/tmp/changed" invalidations))
            (should (member "/rpc:mock:/tmp/old" invalidations))
            (should (member "/rpc:mock:/tmp/new" invalidations))
            (should (equal connection-clears (list vec)))
            (should (equal (nreverse dispatches)
                           '(("changed" "/rpc:mock:/tmp/changed" nil nil)
                             ("renamed" "/rpc:mock:/tmp/old" "/rpc:mock:/tmp/new" nil))))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest tramp-rpc-mock-test-file-notify-canonical-event-invalidates-original-watch-spelling ()
  "Canonical fs.events paths also invalidate equivalent original watch paths."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (vec (tramp-dissect-file-name "/rpc:mock:/tmp/"))
         (proc (make-process :name "tramp-rpc-fs-events-canonical-test"
                             :buffer nil
                             :command '("cat")
                             :connection-type 'pipe
                             :noquery t))
         (watch-key (format "%s:%s" (tramp-rpc--connection-key-string vec) "/tmp/link/"))
         (descriptor (tramp-rpc--make-file-notify-descriptor
                      vec "/rpc:mock:/tmp/link/" "/tmp/link/"))
         (invalidations nil))
    (unwind-protect
        (progn
          (process-put proc :tramp-rpc-vec vec)
          (puthash descriptor
                   (list :watch-key watch-key
                         :directory "/rpc:mock:/tmp/link/"
                         :canonical-directory "/rpc:mock:/tmp/real/"
                         :flags '(change))
                   tramp-rpc--file-notify-descriptors)
          (puthash watch-key
                   '(:count 1 :owned t
                     :directory "/rpc:mock:/tmp/link/"
                     :canonical-directory "/rpc:mock:/tmp/real/")
                   tramp-rpc--file-notify-watch-counts)
          (cl-letf (((symbol-function 'tramp-rpc-magit--clear-status-cache) #'ignore)
                    ((symbol-function 'tramp-rpc--invalidate-cache-for-path)
                     (lambda (path) (push path invalidations)))
                    ((symbol-function 'tramp-rpc--file-notify-dispatch) #'ignore))
            (tramp-rpc--handle-notification
             proc "fs.events"
             '((events . (((action . "changed")
                            (path . "/tmp/real/changed"))))))
            (should (member "/rpc:mock:/tmp/real/changed" invalidations))
            (should (member "/rpc:mock:/tmp/link/changed" invalidations))))
      (when (process-live-p proc)
        (delete-process proc))
      (tramp-rpc--delete-file-notify-descriptor-process descriptor))))

(ert-deftest tramp-rpc-mock-test-file-notify-watch-directory-canonical-event-invalidates-original-spelling ()
  "Explicit watch canonical fs.events paths invalidate original watched paths."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((tramp-rpc--watched-directories (make-hash-table :test 'equal))
         (vec (tramp-dissect-file-name "/rpc:mock:/tmp/"))
         (proc (make-process :name "tramp-rpc-watch-canonical-test"
                             :buffer nil
                             :command '("cat")
                             :connection-type 'pipe
                             :noquery t))
         (directory "/rpc:mock:/tmp/link/")
         (watch-key (format "%s:%s" (tramp-rpc--connection-key-string vec)
                            "/tmp/link/"))
         (invalidations nil))
    (unwind-protect
        (progn
          (process-put proc :tramp-rpc-vec vec)
          (cl-letf (((symbol-function 'tramp-rpc--call)
                     (lambda (_vec method _params)
                       (when (equal method "watch.add")
                         '((path . "/tmp/real/")))))
                    ((symbol-function 'tramp-rpc-magit--clear-status-cache) #'ignore)
                    ((symbol-function 'tramp-rpc--invalidate-cache-for-path)
                     (lambda (path) (push path invalidations)))
                    ((symbol-function 'tramp-rpc--file-notify-dispatch) #'ignore))
            (tramp-rpc-watch-directory directory t)
            (should (equal (plist-get (gethash watch-key tramp-rpc--watched-directories)
                                      :canonical-directory)
                           "/rpc:mock:/tmp/real/"))
            (tramp-rpc--handle-notification
             proc "fs.events"
             '((events . (((action . "changed")
                            (path . "/tmp/real/sub/file"))))))
            (should (member "/rpc:mock:/tmp/real/sub/file" invalidations))
            (should (member "/rpc:mock:/tmp/link/sub/file" invalidations))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest tramp-rpc-mock-test-watch-directory-canonical-aliases-share-server-watch ()
  "Explicit watches with the same canonical path do not remove each other."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((tramp-rpc--watched-directories (make-hash-table :test 'equal))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (link-directory "/rpc:mock:/tmp/link/")
         (real-directory "/rpc:mock:/tmp/real/")
         (calls nil))
    (cl-letf (((symbol-function 'tramp-rpc--call)
               (lambda (_vec method params)
                 (push (list method params) calls)
                 (when (equal method "watch.add")
                   '((path . "/tmp/real/"))))))
      (tramp-rpc-watch-directory link-directory t)
      (tramp-rpc-watch-directory real-directory t)
      (setq calls nil)
      (tramp-rpc-unwatch-directory link-directory)
      (should (equal (mapcar #'car (nreverse (copy-sequence calls))) nil))
      (should (tramp-rpc--directory-watched-p "/tmp/real/" (tramp-dissect-file-name real-directory)))
      (tramp-rpc-unwatch-directory real-directory)
      (should (equal (mapcar #'car (nreverse (copy-sequence calls)))
                     '("watch.remove"))))))

(ert-deftest tramp-rpc-mock-test-file-notify-canonical-aliases-share-server-watch ()
  "File notification watches with the same canonical path are refcounted."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (require 'filenotify)
  (let* ((tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (tramp-rpc--watched-directories (make-hash-table :test 'equal))
         (link-directory "/rpc:mock:/tmp/link/")
         (real-directory "/rpc:mock:/tmp/real/")
         (calls nil)
         link-descriptor
         real-descriptor)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method params)
                     (push (list method params) calls)
                     (when (equal method "watch.add")
                       '((path . "/tmp/real/")))))
                  ((symbol-function 'tramp-rpc-handle-file-directory-p)
                   (lambda (_filename) t)))
          (setq link-descriptor
                (file-notify-add-watch link-directory '(change) #'ignore))
          (setq real-descriptor
                (file-notify-add-watch real-directory '(change) #'ignore))
          (setq calls nil)
          (file-notify-rm-watch link-descriptor)
          (should (equal (mapcar #'car (nreverse (copy-sequence calls))) nil))
          (should (file-notify-valid-p real-descriptor))
          (file-notify-rm-watch real-descriptor)
          (should (equal (mapcar #'car (nreverse (copy-sequence calls)))
                         '("watch.remove"))))
      (when (and link-descriptor (boundp 'file-notify-descriptors))
        (remhash link-descriptor file-notify-descriptors))
      (when (and real-descriptor (boundp 'file-notify-descriptors))
        (remhash real-descriptor file-notify-descriptors)))))

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
         (descriptor (tramp-rpc--make-file-notify-descriptor
                      vec "/rpc:mock:/tmp/" "/tmp/"))
         (other-descriptor (tramp-rpc--make-file-notify-descriptor
                            other-vec "/rpc:other:/tmp/" "/tmp/"))
         (stopped-events nil)
         (other-stopped-events nil))
    (unwind-protect
        (progn
          (puthash descriptor (list :watch-key watch-key :directory "/rpc:mock:/tmp/")
                   tramp-rpc--file-notify-descriptors)
          (puthash other-descriptor (list :watch-key other-key :directory "/rpc:other:/tmp/")
                   tramp-rpc--file-notify-descriptors)
          (puthash watch-key '(:count 1 :owned t) tramp-rpc--file-notify-watch-counts)
          (puthash other-key '(:count 1 :owned t) tramp-rpc--file-notify-watch-counts)
          (puthash descriptor
                   (file-notify--watch-make
                    "/rpc:mock:/tmp/" nil
                    (lambda (event) (push event stopped-events)))
                   file-notify-descriptors)
          (puthash other-descriptor
                   (file-notify--watch-make
                    "/rpc:other:/tmp/" nil
                    (lambda (event) (push event other-stopped-events)))
                   file-notify-descriptors)
          (tramp-rpc--cleanup-file-notify-for-connection vec)
          (should-not (gethash descriptor tramp-rpc--file-notify-descriptors))
          (should-not (gethash watch-key tramp-rpc--file-notify-watch-counts))
          (should-not (gethash descriptor file-notify-descriptors))
          (should-not (tramp-rpc-handle-file-notify-valid-p descriptor))
          (should-not (process-live-p descriptor))
          (should (equal stopped-events
                         `((,descriptor stopped "/rpc:mock:/tmp/"))))
          (should-not other-stopped-events)
          (should (gethash other-descriptor tramp-rpc--file-notify-descriptors))
          (should (gethash other-key tramp-rpc--file-notify-watch-counts))
          (should (process-live-p other-descriptor))
          (should (gethash other-descriptor file-notify-descriptors)))
      (remhash descriptor file-notify-descriptors)
      (remhash other-descriptor file-notify-descriptors)
      (tramp-rpc--delete-file-notify-descriptor-process descriptor)
      (tramp-rpc--delete-file-notify-descriptor-process other-descriptor))))

(ert-deftest tramp-rpc-mock-test-file-notify-dispatch-matches-canonical-directory ()
  "Dispatch matches canonical watch paths returned by the server."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (require 'filenotify)
  (let* ((tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (tramp-rpc--watched-directories (make-hash-table :test 'equal))
         (directory "/rpc:mock:/tmp/link/")
         (events nil)
         descriptor)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method _params)
                     (when (equal method "watch.add")
                       '((path . "/tmp/real/")))))
                  ((symbol-function 'insert-special-event)
                   (lambda (event) (push event events))))
          (setq descriptor
                (tramp-rpc-handle-file-notify-add-watch directory '(change) #'ignore))
          (should (processp descriptor))
          (should (process-get descriptor 'tramp-vector))
          (should (equal (process-get descriptor 'tramp-watch-name) "/tmp/link/"))
          (should (equal (plist-get (gethash descriptor
                                             tramp-rpc--file-notify-descriptors)
                                    :canonical-directory)
                         "/rpc:mock:/tmp/real/"))
          (tramp-rpc--file-notify-dispatch "changed" "/rpc:mock:/tmp/real/changed")
          (should (equal events
                         `((file-notify
                            (,descriptor (changed) "changed")
                            file-notify-callback))))
          (setq events nil)
          (tramp-rpc--file-notify-dispatch
           "renamed" "/rpc:mock:/tmp/real/old" "/rpc:mock:/tmp/real/new")
          (should (equal events
                         `((file-notify
                            (,descriptor (moved) "old" "new")
                            file-notify-callback))))
          ;; Dispatch uses the shared watch entry's canonical directory if it is
          ;; refreshed after the descriptor was created.
          (setq events nil)
          (let* ((data (gethash descriptor tramp-rpc--file-notify-descriptors))
                 (entry (gethash (plist-get data :watch-key)
                                 tramp-rpc--file-notify-watch-counts)))
            (plist-put entry :canonical-directory "/rpc:mock:/tmp/new-real/"))
          (tramp-rpc--file-notify-dispatch "changed" "/rpc:mock:/tmp/new-real/changed")
          (should (equal events
                         `((file-notify
                            (,descriptor (changed) "changed")
                            file-notify-callback)))))
      (when descriptor
        (remhash descriptor tramp-rpc--file-notify-descriptors)
        (tramp-rpc--delete-file-notify-descriptor-process descriptor)))))

(ert-deftest tramp-rpc-mock-test-file-notify-callback-expands-relative-event-name ()
  "Dispatched relative backend names become absolute TRAMP callback names."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (require 'filenotify)
  (let* ((tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (tramp-rpc--watched-directories (make-hash-table :test 'equal))
         (directory "/rpc:mock:/tmp/link/")
         (events nil)
         descriptor)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method _params)
                     (when (equal method "watch.add")
                       '((path . "/tmp/real/")))))
                  ((symbol-function 'insert-special-event)
                   (lambda (event)
                     (funcall (lookup-key special-event-map [file-notify]) event))))
          (setq descriptor
                (tramp-rpc-handle-file-notify-add-watch directory '(change) #'ignore))
          (puthash descriptor
                   (file-notify--watch-make
                    (file-name-unquote (directory-file-name directory))
                    nil
                    (lambda (event) (push event events)))
                   file-notify-descriptors)
          (tramp-rpc--file-notify-dispatch "changed" "/rpc:mock:/tmp/real/changed")
          (should (equal events
                         `((,descriptor changed "/rpc:mock:/tmp/link/changed")))))
      (when descriptor
        (remhash descriptor file-notify-descriptors)
        (remhash descriptor tramp-rpc--file-notify-descriptors)
        (tramp-rpc--delete-file-notify-descriptor-process descriptor)))))

(ert-deftest tramp-rpc-mock-test-file-notify-dispatches-structured-actions ()
  "Structured server watch events dispatch the corresponding file-notify actions."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (require 'filenotify)
  (let* ((tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (tramp-rpc--watched-directories (make-hash-table :test 'equal))
         (directory "/rpc:mock:/tmp/repo/")
         (events nil)
         descriptor)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method _params)
                     (when (equal method "watch.add")
                       '((path . "/tmp/repo/")))))
                  ((symbol-function 'insert-special-event)
                   (lambda (event) (push event events))))
          (setq descriptor
                (tramp-rpc-handle-file-notify-add-watch
                 directory '(change attribute-change) #'ignore))
          (should (processp descriptor))
          (tramp-rpc--file-notify-dispatch "created" "/rpc:mock:/tmp/repo/new")
          (tramp-rpc--file-notify-dispatch "attribute-changed" "/rpc:mock:/tmp/repo/new")
          (tramp-rpc--file-notify-dispatch
           "renamed" "/rpc:mock:/tmp/repo/old" "/rpc:mock:/tmp/repo/new")
          (tramp-rpc--file-notify-dispatch "deleted" "/rpc:mock:/tmp/repo/new")
          (should (equal (nreverse events)
                         `((file-notify
                            (,descriptor (created) "new")
                            file-notify-callback)
                           (file-notify
                            (,descriptor (attribute-changed) "new")
                            file-notify-callback)
                           (file-notify
                            (,descriptor (moved) "old" "new")
                            file-notify-callback)
                           (file-notify
                            (,descriptor (deleted) "new")
                            file-notify-callback)))))
      (when descriptor
        (remhash descriptor tramp-rpc--file-notify-descriptors)
        (tramp-rpc--delete-file-notify-descriptor-process descriptor)))))

(ert-deftest tramp-rpc-mock-test-file-notify-dispatch-honors-flags ()
  "TRAMP-RPC file notifications honor change vs attribute-change flags."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (require 'filenotify)
  (let* ((tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq))
         (tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal))
         (tramp-rpc--watched-directories (make-hash-table :test 'equal))
         (directory "/rpc:mock:/tmp/repo/")
         (events nil)
         change-descriptor
         attribute-descriptor)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method _params)
                     (when (equal method "watch.add")
                       '((path . "/tmp/repo/")))))
                  ((symbol-function 'insert-special-event)
                   (lambda (event) (push event events))))
          (setq change-descriptor
                (tramp-rpc-handle-file-notify-add-watch directory '(change) #'ignore))
          (setq attribute-descriptor
                (tramp-rpc-handle-file-notify-add-watch directory '(attribute-change) #'ignore))
          (should (processp change-descriptor))
          (should (processp attribute-descriptor))
          (tramp-rpc--file-notify-dispatch "changed" "/rpc:mock:/tmp/repo/file")
          (tramp-rpc--file-notify-dispatch "attribute-changed" "/rpc:mock:/tmp/repo/file")
          (should (equal (nreverse events)
                         `((file-notify
                            (,change-descriptor (changed) "file")
                            file-notify-callback)
                           (file-notify
                            (,attribute-descriptor (attribute-changed) "file")
                            file-notify-callback)))))
      (dolist (descriptor (list change-descriptor attribute-descriptor))
        (when descriptor
          (remhash descriptor tramp-rpc--file-notify-descriptors)
          (tramp-rpc--delete-file-notify-descriptor-process descriptor))))))

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
                   (lambda (_filename) t))
                  ((symbol-function 'file-symlink-p)
                   (lambda (_filename) nil)))
          (setq descriptor
                (file-notify-add-watch directory '(change) #'ignore))
          (should (processp descriptor))
          (should (process-live-p descriptor))
          (should (equal (process-get descriptor 'tramp-watch-name) "/tmp/repo"))
          (should (gethash descriptor tramp-rpc--file-notify-descriptors))
          (should (gethash descriptor file-notify-descriptors))
          (should (file-notify-valid-p descriptor))
          (file-notify-rm-watch descriptor)
          (should-not (gethash descriptor tramp-rpc--file-notify-descriptors))
          (should-not (gethash descriptor file-notify-descriptors))
          (should-not (process-live-p descriptor))
          (should (= (hash-table-count tramp-rpc--file-notify-watch-counts) 0))
          (should (equal (mapcar #'car
                                (cl-remove-if-not
                                 (lambda (call) (string-prefix-p "watch." (car call)))
                                 (nreverse (copy-sequence calls))))
                         '("watch.add" "watch.remove"))))
      (when (and descriptor (boundp 'file-notify-descriptors))
        (remhash descriptor file-notify-descriptors))
      (when descriptor
        (tramp-rpc--delete-file-notify-descriptor-process descriptor)))))

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
                 t))
              ((symbol-function 'file-symlink-p)
               (lambda (_filename) nil)))
      (setq descriptor
            (tramp-rpc-handle-file-notify-add-watch directory '(change) #'ignore))
      (should (gethash descriptor tramp-rpc--file-notify-descriptors))
      (should-not (gethash watch-key tramp-rpc--watched-directories))
      (should (equal (mapcar #'car
                             (cl-remove-if-not
                              (lambda (call) (string-prefix-p "watch." (car call)))
                              (nreverse (copy-sequence calls))))
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
              ((symbol-function 'file-symlink-p)
               (lambda (_filename) nil))
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
      (should (equal (mapcar #'car
                             (cl-remove-if-not
                              (lambda (call) (string-prefix-p "watch." (car call)))
                              (nreverse (copy-sequence calls))))
                     '("watch.add" "watch.add"))))))

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
    (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
               (lambda (_vec) '(:process mock)))
              ((symbol-function 'tramp-rpc--call)
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

(ert-deftest tramp-rpc-mock-test-set-file-modes-invalidates-metadata-caches ()
  "`set-file-modes' clears cached metadata that depends on file mode bits."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((filename "/rpc:mockhost:/tmp/file")
         (vec (tramp-dissect-file-name filename))
         (localname "/tmp/file")
         (expanded (expand-file-name filename))
         (stat-key (tramp-rpc--file-stat-cache-key vec localname nil))
         (lstat-key (tramp-rpc--file-stat-cache-key vec localname t))
         (calls nil))
    (unwind-protect
        (progn
          (tramp-rpc--cache-put tramp-rpc--file-exists-cache expanded t)
          (tramp-rpc--cache-put tramp-rpc--file-truename-cache expanded expanded)
          (tramp-rpc--cache-put tramp-rpc--file-stat-cache stat-key '((mode . 420)))
          (tramp-rpc--cache-put tramp-rpc--file-stat-cache lstat-key '((mode . 420)))
          (cl-letf (((symbol-function 'tramp-rpc--call)
                     (lambda (_vec method _params)
                       (push method calls)
                       t)))
            (set-file-modes filename #o755))
          (should (equal (nreverse calls) '("file.set_modes")))
          (should-not (gethash expanded tramp-rpc--file-exists-cache))
          (should-not (gethash expanded tramp-rpc--file-truename-cache))
          (should-not (gethash stat-key tramp-rpc--file-stat-cache))
          (should-not (gethash lstat-key tramp-rpc--file-stat-cache)))
      (tramp-rpc--invalidate-cache-for-path filename))))

(ert-deftest tramp-rpc-mock-test-make-symlink-invalidates-negative-lstat-cache ()
  "`make-symbolic-link' clears stale negative lstat metadata for LINKNAME."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((filename "/rpc:mockhost:/tmp/link")
         (vec (tramp-dissect-file-name filename))
         (localname "/tmp/link")
         (expanded (expand-file-name filename))
         (lstat-key (tramp-rpc--file-stat-cache-key vec localname t))
         (linked nil)
         calls)
    (unwind-protect
        (progn
          ;; Simulate an earlier `file-symlink-p' or `file-attributes' miss.
          (tramp-rpc--cache-put tramp-rpc--file-exists-cache expanded nil)
          (tramp-rpc--cache-put tramp-rpc--file-stat-cache lstat-key nil)
          (cl-letf (((symbol-function 'tramp-connectable-p) (lambda (_filename) t))
                    ((symbol-function 'tramp-rpc--call)
                     (lambda (_vec method params)
                       (push method calls)
                       (pcase method
                         ("file.make_symlink"
                          (setq linked t)
                          t)
                         ("file.stat"
                          (if linked
                              (if (alist-get 'lstat params)
                                  `((type . "symlink")
                                    (link_target . ,(encode-coding-string
                                                     "target" 'utf-8-unix)))
                                '((type . "file")))
                            (signal 'file-missing
                                    (list "RPC" "No such file"
                                          (alist-get 'path params)))))
                         (_ (error "Unexpected RPC method: %s" method))))))
            (tramp-rpc-handle-make-symbolic-link "target" filename)
            (should linked)
            (should-not (gethash expanded tramp-rpc--file-exists-cache))
            (should-not (gethash lstat-key tramp-rpc--file-stat-cache))
            ;; A followed stat must not be cached as lstat, or
            ;; `file-symlink-p' will return nil.
            (should (tramp-rpc--call-file-stat vec localname))
            (should (equal (tramp-rpc-handle-file-symlink-p filename)
                           "target"))
            (should (member "file.make_symlink" calls))
            (should (member "file.stat" calls))))
      (tramp-rpc--invalidate-cache-for-path filename))))

(ert-deftest tramp-rpc-mock-test-follow-stat-does-not-seed-lstat-cache ()
  "A followed stat for a symlink must not pollute the lstat cache."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((filename "/rpc:mockhost:/tmp/link")
         (vec (tramp-dissect-file-name filename))
         (follow-key (tramp-rpc--file-stat-cache-key vec "/tmp/link" nil))
         (lstat-key (tramp-rpc--file-stat-cache-key vec "/tmp/link" t)))
    (unwind-protect
        (progn
          (tramp-rpc--cache-file-stat-result
           vec "/tmp/link" '((type . "file") (mode . 33188)) nil)
          (should (gethash follow-key tramp-rpc--file-stat-cache))
          (should-not (gethash lstat-key tramp-rpc--file-stat-cache))
          (tramp-rpc--cache-file-stat-result
           vec "/tmp/link" '((type . "file") (mode . 33188)) t)
          (should (gethash follow-key tramp-rpc--file-stat-cache))
          (should (gethash lstat-key tramp-rpc--file-stat-cache)))
      (tramp-rpc--invalidate-cache-for-path filename))))

(ert-deftest tramp-rpc-mock-test-file-stat-file-error-message-matched ()
  "`file.stat' treats ELOOP/ENOTDIR file-error messages as missing."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((filename "/rpc:mockhost:/tmp/file/.editorconfig")
         (vec (tramp-dissect-file-name filename))
         (key (tramp-rpc--file-stat-cache-key vec "/tmp/file/.editorconfig" nil)))
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call)
                   (lambda (_vec _method _params)
                     (signal 'file-error
                             '("RPC" "Not a directory" "/tmp/file/.editorconfig")))))
          (should-not (tramp-rpc--call-file-stat
                       vec "/tmp/file/.editorconfig"))
          (should (gethash key tramp-rpc--file-stat-cache)))
      (tramp-rpc--invalidate-cache-for-path filename))))

(ert-deftest tramp-rpc-mock-test-access-file-dangling-symlink-is-missing ()
  "`access-file' reports non-cyclic dangling symlinks as `file-missing'."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (cl-letf (((symbol-function 'tramp-handle-access-file)
             (lambda (_filename _string)
               (signal 'file-error '("Apparent cycle"))))
            ((symbol-function 'file-symlink-p)
             (lambda (filename)
               (and (string-suffix-p "/link" filename) "does-not-exist")))
            ((symbol-function 'file-exists-p) #'ignore))
    (should-error
     (tramp-rpc-handle-access-file "/rpc:mockhost:/tmp/link" "error")
     :type 'file-missing)))

(ert-deftest tramp-rpc-mock-test-access-file-cyclic-symlink-stays-file-error ()
  "`access-file' keeps self-referential symlinks as `file-error'."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (cl-letf (((symbol-function 'tramp-handle-access-file)
             (lambda (_filename _string)
               (signal 'file-error '("Apparent cycle"))))
            ((symbol-function 'file-symlink-p)
             (lambda (_filename) "link"))
            ((symbol-function 'file-exists-p) #'ignore))
    (condition-case err
        (progn
          (tramp-rpc-handle-access-file "/rpc:mockhost:/tmp/link" "error")
          (ert-fail "Expected file-error"))
      (error
       (should (eq (car err) 'file-error))))))

(ert-deftest tramp-rpc-mock-test-subtree-invalidation-clears-descendant-caches ()
  "Subtree invalidation clears stale metadata for cached descendants."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((root "/rpc:mockhost:/tmp/dest")
         (child "/rpc:mockhost:/tmp/dest/source")
         (nested "/rpc:mockhost:/tmp/dest/source/file")
         (sibling "/rpc:mockhost:/tmp/dest-sibling/source")
         (vec (tramp-dissect-file-name child))
         (root-key (expand-file-name root))
         (child-key (expand-file-name child))
         (nested-key (expand-file-name nested))
         (sibling-key (expand-file-name sibling))
         (child-stat-key (tramp-rpc--file-stat-cache-key vec "/tmp/dest/source" nil))
         (nested-stat-key (tramp-rpc--file-stat-cache-key
                           vec "/tmp/dest/source/file" nil))
         (sibling-stat-key (tramp-rpc--file-stat-cache-key
                            vec "/tmp/dest-sibling/source" nil)))
    (unwind-protect
        (progn
          (dolist (key (list root-key child-key nested-key sibling-key))
            (tramp-rpc--cache-put tramp-rpc--file-exists-cache key t)
            (tramp-rpc--cache-put tramp-rpc--file-truename-cache key key))
          (tramp-rpc--cache-put tramp-rpc--file-stat-cache
                                child-stat-key '((type . "directory")))
          (tramp-rpc--cache-put tramp-rpc--file-stat-cache
                                nested-stat-key '((type . "file")))
          (tramp-rpc--cache-put tramp-rpc--file-stat-cache
                                sibling-stat-key '((type . "directory")))
          (tramp-rpc--invalidate-cache-for-subtree root)
          (dolist (key (list root-key child-key nested-key))
            (should-not (gethash key tramp-rpc--file-exists-cache))
            (should-not (gethash key tramp-rpc--file-truename-cache)))
          (should-not (gethash child-stat-key tramp-rpc--file-stat-cache))
          (should-not (gethash nested-stat-key tramp-rpc--file-stat-cache))
          ;; Prefix matching must not evict similarly named siblings.
          (should (gethash sibling-key tramp-rpc--file-exists-cache))
          (should (gethash sibling-key tramp-rpc--file-truename-cache))
          (should (gethash sibling-stat-key tramp-rpc--file-stat-cache)))
      (dolist (filename (list root child nested sibling))
        (tramp-rpc--invalidate-cache-for-path filename)))))

(ert-deftest tramp-rpc-mock-test-hardlink-invalidates-source-and-dest ()
  "`add-name-to-file' clears source and destination metadata caches."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((source "/rpc:mockhost:/tmp/source")
         (dest "/rpc:mockhost:/tmp/dest")
         (vec (tramp-dissect-file-name source))
         (source-key (expand-file-name source))
         (dest-key (expand-file-name dest))
         (source-stat-key (tramp-rpc--file-stat-cache-key vec "/tmp/source" nil))
         (dest-stat-key (tramp-rpc--file-stat-cache-key vec "/tmp/dest" nil))
         calls)
    (unwind-protect
        (progn
          (dolist (key (list source-key dest-key))
            (tramp-rpc--cache-put tramp-rpc--file-exists-cache key t)
            (tramp-rpc--cache-put tramp-rpc--file-truename-cache key key))
          ;; Destination must look absent to the existence preflight.
          (tramp-rpc--cache-put tramp-rpc--file-exists-cache dest-key nil)
          (tramp-rpc--cache-put tramp-rpc--file-stat-cache
                                source-stat-key '((type . "file") (nlink . 1)))
          (tramp-rpc--cache-put tramp-rpc--file-stat-cache dest-stat-key nil)
          (cl-letf (((symbol-function 'file-exists-p) #'ignore)
                    ((symbol-function 'tramp-rpc--call)
                     (lambda (_vec method _params)
                       (push method calls)
                       (should (equal method "file.make_hardlink"))
                       t)))
            (tramp-rpc-handle-add-name-to-file source dest)
            (should (equal calls '("file.make_hardlink")))
            (dolist (key (list source-key dest-key))
              (should-not (gethash key tramp-rpc--file-exists-cache))
              (should-not (gethash key tramp-rpc--file-truename-cache)))
            (should-not (gethash source-stat-key tramp-rpc--file-stat-cache))
            (should-not (gethash dest-stat-key tramp-rpc--file-stat-cache))))
      (dolist (filename (list source dest))
        (tramp-rpc--invalidate-cache-for-path filename)))))

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
                   '(:method "rpc" :host "target" :user "user"
                     :port "22" :route nil)))))

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
    ;; Both should have the same target method/host/user/port.
    (should (equal (plist-get key1 :method) (plist-get key2 :method)))
    (should (equal (plist-get key1 :host) (plist-get key2 :host)))
    (should (equal (plist-get key1 :user) (plist-get key2 :user)))
    (should (equal (plist-get key1 :port) (plist-get key2 :port)))
    ;; Route should differ.
    (should (plist-get key1 :route))
    (should-not (plist-get key2 :route))))

(ert-deftest tramp-rpc-mock-test-connection-key-different-hops ()
  "Test that different hop routes produce different connection keys."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((vec1 (tramp-dissect-file-name "/rpc:gateway1|rpc:user@target:/path"))
         (vec2 (tramp-dissect-file-name "/rpc:gateway2|rpc:user@target:/path"))
         (key1 (tramp-rpc--connection-key vec1))
         (key2 (tramp-rpc--connection-key vec2)))
    (should-not (equal key1 key2))))

(ert-deftest tramp-rpc-mock-test-connection-key-hidden-sudo-route ()
  "Hidden native sudo routes should not collide with direct root rpc."
  :tags '(:multi-hop :sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (skip-unless (tramp-rpc-mock-test--sudo-helper-available-p))
  (let* ((tramp-default-proxies-alist nil)
         (tramp-file-name-with-method "sudo")
         (hidden (tramp-dissect-file-name
                  (tramp-file-name-with-sudo "/rpc:alice@server:/root")))
         (explicit (tramp-dissect-file-name
                    "/rpc:alice@server|sudo:root@server:/root"))
         (direct-root (tramp-dissect-file-name "/rpc:root@server:/root")))
    (should (equal (tramp-rpc--connection-key hidden)
                   (tramp-rpc--connection-key explicit)))
    (should-not (equal (tramp-rpc--connection-key hidden)
                       (tramp-rpc--connection-key direct-root)))))

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

(ert-deftest tramp-rpc-mock-test-delete-file-missing-is-noop ()
  "Missing files are ignored like current Emacs `delete-file'."
  :tags '(:delete)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let (invalidated)
    (cl-letf (((symbol-function 'tramp-rpc--call)
               (lambda (_vec method _params)
                 (should (equal method "file.delete"))
                 (signal 'file-missing '("RPC" "No such file" "/tmp/missing"))))
              ((symbol-function 'tramp-rpc--invalidate-cache-for-path)
               (lambda (filename) (setq invalidated filename))))
      (tramp-rpc-handle-delete-file "/rpc:mock:/tmp/missing" nil)
      (should (equal invalidated "/rpc:mock:/tmp/missing")))))

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
                        (pcase (tramp-rpc-mock-test--bytes-string
                                (alist-get 'path params))
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
                               (pcase (tramp-rpc-mock-test--bytes-string
                                       (alist-get 'path (cdr request)))
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
                                               (tramp-rpc-mock-test--bytes-string
                                                (alist-get 'path (cdr request))))))))
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
  :tags '(:multi-hop :sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (skip-unless (tramp-rpc-mock-test--sudo-helper-available-p))
  (let* ((tramp-default-proxies-alist nil)
         (tramp-file-name-with-method "sudo")
         (filename (tramp-file-name-with-sudo "/rpc:user@target:/etc/hosts"))
         (vec (tramp-dissect-file-name filename)))
    (should (tramp-tramp-file-p filename))
    (should (equal (tramp-file-name-localname vec) "/etc/hosts"))
    (should (string= (tramp-file-name-method vec) "sudo"))
    (should (string= (tramp-file-name-host vec) "target"))
    ;; Upstream TRAMP hides this ad-hoc hop in `tramp-default-proxies-alist'
    ;; when `tramp-show-ad-hoc-proxies' is nil; tramp-rpc must still claim it.
    (should-not (tramp-file-name-hop vec))
    (should (tramp-rpc--sudo-file-name-p filename))
    (should (equal (tramp-rpc--detect-sudo-elevation vec) "user")))
  ;; Also verify non-rpc paths still work and are not claimed by tramp-rpc.
  (let* ((tramp-default-proxies-alist nil)
         (tramp-file-name-with-method "sudo")
         (filename (tramp-file-name-with-sudo "/ssh:user@target:/etc/hosts"))
         (vec (tramp-dissect-file-name filename)))
    (should (tramp-tramp-file-p filename))
    (should (string= (tramp-file-name-method vec) "sudo"))
    (should (string= (tramp-file-name-host vec) "target"))
    (should-not (tramp-rpc--sudo-file-name-p filename))))

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
  ;; when processing /rpc:server|sudo:root@server:/path.  Disable the
  ;; tramp-rpc sudo foreign predicate for this low-level TRAMP check; otherwise
  ;; the hidden rpc+sudo path is intentionally claimed by tramp-rpc before
  ;; tramp-sh computes its multi-hop chain.
  (let* ((tramp-foreign-file-name-handler-alist
          (cl-remove-if
           (lambda (entry) (eq (car entry) 'tramp-rpc--sudo-file-name-p))
           tramp-foreign-file-name-handler-alist))
         (tramp-default-proxies-alist
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
                "/ssh:user@server|sudo:root@server:/root")))
  ;; rpc as a real proxy to a different host should not match either.
  (should-not (tramp-rpc--sudo-file-name-p
               (tramp-dissect-file-name
                "/rpc:user@gateway|sudo:root@server:/root"))))

(ert-deftest tramp-rpc-mock-test-doas-previous-hop-not-sudo-via-rpc ()
  "Non-sudo previous-hop methods must not be treated as sudo-via-RPC."
  :tags '(:multi-hop :sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (make-tramp-file-name :method "doas" :user "root"
                                   :host "server" :localname "/root"
                                   :hop "rpc:alice@server|")))
    (should (tramp-rpc--privilege-elevation-vec-p
             (make-tramp-file-name :method "sudo" :user "root"
                                   :host "server" :localname "/root"
                                   :hop "rpc:alice@server|")))
    (should (tramp-get-method-parameter vec 'tramp-password-previous-hop))
    (should-not (tramp-rpc--privilege-elevation-vec-p vec))
    (should-not (tramp-rpc--sudo-file-name-p vec))
    (should-not (tramp-rpc--detect-sudo-elevation vec))
    (should-not (tramp-rpc-multi-hop-p vec))))

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

(ert-deftest tramp-rpc-mock-test-sudo-rpc-hop-must-be-final-hop ()
  "Only the final hop before sudo is the sudo-via-RPC SSH detail hop."
  :tags '(:multi-hop :sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name
              "/rpc:alice@server|ssh:gateway|sudo:root@server:/root")))
    (should-not (tramp-rpc--detect-sudo-elevation vec))
    (should (equal (tramp-rpc--proxy-hop-string vec)
                   "rpc:alice@server|ssh:gateway|"))
    (should (equal (tramp-rpc--hops-to-proxyjump vec)
                   "alice@server,gateway"))))

(ert-deftest tramp-rpc-mock-test-hidden-sudo-proxies-from-native-tramp ()
  "TRAMP hidden ad-hoc proxies should still identify rpc+sudo paths."
  :tags '(:multi-hop :sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (skip-unless (tramp-rpc-mock-test--sudo-helper-available-p))
  (let* ((tramp-default-proxies-alist nil)
         (tramp-file-name-with-method "sudo")
         (filename (tramp-file-name-with-sudo
                    "/rpc:gw|rpc:alice@server:/root"))
         (vec (tramp-dissect-file-name filename)))
    (should-not (tramp-file-name-hop vec))
    (should (tramp-rpc--sudo-file-name-p filename))
    (should (equal (tramp-rpc--detect-sudo-elevation vec) "alice"))
    (should (equal (substring-no-properties (tramp-rpc--proxy-hop-string vec))
                   "rpc:gw|"))
    (should (equal (substring-no-properties (tramp-rpc--hops-to-proxyjump vec))
                   "gw"))))

(ert-deftest tramp-rpc-mock-test-hidden-sudo-handler-no-recursion ()
  "Hidden native rpc+sudo should be claimed without unregistering predicate."
  :tags '(:multi-hop :sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (skip-unless (tramp-rpc-mock-test--sudo-helper-available-p))
  (let* ((tramp-default-proxies-alist nil)
         (tramp-file-name-with-method "sudo")
         (filename (tramp-file-name-with-sudo "/rpc:alice@server:/root")))
    (should (eq (tramp-find-foreign-file-name-handler
                 (tramp-dissect-file-name filename))
                'tramp-rpc-file-name-handler))
    (should (assq 'tramp-rpc--sudo-file-name-p
                  tramp-foreign-file-name-handler-alist))))

(ert-deftest tramp-rpc-mock-test-hidden-different-host-sudo-not-claimed ()
  "Hidden rpc proxy to another host is not sudo-via-rpc for the target."
  :tags '(:multi-hop :sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((tramp-default-proxies-alist
          (list (list "^server$" "^root$"
                      (propertize "/rpc:alice@gateway:"
                                  'tramp-ad-hoc t))))
         (filename "/sudo:root@server:/root")
         (vec (tramp-dissect-file-name filename)))
    (should-not (tramp-rpc--sudo-file-name-p vec))
    (should-not (eq (tramp-find-foreign-file-name-handler vec)
                    'tramp-rpc-file-name-handler))))

(ert-deftest tramp-rpc-mock-test-different-host-sudo-probing-is-quiet ()
  "Non-matching rpc sudo probes should not emit TRAMP host-mismatch messages."
  :tags '(:multi-hop :sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (when format-string
                   (push (apply #'format-message format-string args)
                         messages)))))
      ;; Explicit rpc proxy to a different host.
      (should-not (tramp-rpc--sudo-file-name-p
                   (tramp-dissect-file-name
                    "/rpc:alice@gateway|sudo:root@server:/root")))
      ;; Hidden native ad-hoc proxy to a different host.
      (let* ((tramp-default-proxies-alist
              (list (list "^server$" "^root$"
                          (propertize "/rpc:alice@gateway:"
                                      'tramp-ad-hoc t))))
             (vec (tramp-dissect-file-name "/sudo:root@server:/root")))
        (should-not (tramp-rpc--sudo-file-name-p vec))))
    (should-not
     (cl-some (lambda (msg)
                (string-match-p "Host name .* does not match" msg))
              messages))))

(ert-deftest tramp-rpc-mock-test-eshell-sudo-uses-native-em-tramp ()
  "Eshell sudo should keep using em-tramp's TRAMP sudo rewrite."
  :tags '(:sudo :eshell)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (require 'em-tramp)
  (let* ((default-directory "/rpc:alice@server:/tmp/")
         (form (catch 'eshell-replace-command
                 (eshell/sudo "id")))
         (binding (caadr form)))
    (should (eq (car-safe form) 'let))
    (should (eq (car binding) 'default-directory))
    (should (string-match-p "/rpc:alice@server|sudo:root@server:/tmp/"
                            (cadr binding)))))

(ert-deftest tramp-rpc-mock-test-exec-path-sudo-uses-native-sudo-rpc-server ()
  "Eshell command lookup in /rpc|sudo should use the sudo RPC backend."
  :tags '(:sudo :eshell)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((default-directory "/rpc:alice@server|sudo:root@server:/tmp/")
        captured)
    (cl-letf (((symbol-function 'tramp-rpc--cached-remote-path)
               (lambda (vec)
                 (setq captured vec)
                 '("/bin"))))
      (should (equal (tramp-rpc-handle-exec-path) '("/bin" "/tmp/")))
      (should (string= (tramp-file-name-method captured) "sudo"))
      (should (string= (tramp-file-name-user captured) "root")))))

(ert-deftest tramp-rpc-mock-test-process-file-sudo-uses-native-sudo-rpc-server ()
  "process-file in /rpc|sudo should run inside the sudo RPC connection."
  :tags '(:sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((default-directory "/rpc:alice@server|sudo:root@server:/root/")
        captured)
    (cl-letf (((symbol-function 'tramp-rpc--remote-path-environment)
               (lambda (_vec) nil))
              ((symbol-function 'tramp-rpc--tramp-remote-process-environment)
               (lambda () nil))
              ((symbol-function 'tramp-rpc--get-direnv-environment)
               (lambda (&rest _) nil))
              ((symbol-function 'tramp-rpc--caller-environment)
               (lambda () nil))
              ((symbol-function 'tramp-rpc--directory-watched-p)
               (lambda (&rest _) t))
              ((symbol-function 'tramp-rpc--call)
               (lambda (vec _method params)
                 (setq captured (list vec params))
                 '((exit_code . 0) (stdout . "") (stderr . "")))))
      (should (= (tramp-rpc-handle-process-file "id" nil nil nil "-u") 0))
      (let ((vec (car captured))
            (params (cadr captured)))
        (should (string= (tramp-file-name-method vec) "sudo"))
        (should (string= (tramp-file-name-user vec) "root"))
        (should (equal (alist-get 'cmd params) "id"))
        (should (equal (alist-get 'cwd params) "/root/"))
        (should (equal (append (alist-get 'args params) nil) '("-u")))))))

(ert-deftest tramp-rpc-mock-test-make-process-sudo-uses-native-sudo-rpc-server ()
  "make-process in /rpc|sudo should use the elevated RPC connection."
  :tags '(:sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((default-directory "/rpc:alice@server|sudo:root@server:/root/")
        captured proc)
    (cl-letf (((symbol-function 'tramp-rpc--sudo-password-required-p)
               (lambda (_vec) nil))
              ((symbol-function 'tramp-rpc--remote-path-environment)
               (lambda (_vec) nil))
              ((symbol-function 'tramp-rpc--tramp-remote-process-environment)
               (lambda () nil))
              ((symbol-function 'tramp-rpc--get-direnv-environment)
               (lambda (&rest _) nil))
              ((symbol-function 'tramp-rpc--caller-environment)
               (lambda () nil))
              ((symbol-function 'tramp-rpc--start-remote-process)
               (lambda (vec program args cwd _env)
                 (setq captured (list vec program args cwd))
                 4242))
              ((symbol-function 'tramp-rpc--write-remote-process)
               (lambda (&rest _) nil))
              ((symbol-function 'tramp-rpc--start-async-read)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (setq proc (tramp-rpc-handle-make-process
                        :name "tramp-rpc-sudo-process-test"
                        :buffer nil
                        :command '("id" "-u")
                        :connection-type nil
                        :noquery t))
            (should (processp proc))
            (let ((vec (nth 0 captured)))
              (should (string= (tramp-file-name-method vec) "sudo"))
              (should (string= (tramp-file-name-user vec) "root")))
            (should (equal (nth 1 captured) "id"))
            (should (equal (nth 3 captured) "/root/"))
            (should (equal (nth 2 captured) '("-u"))))
        (when (processp proc)
          (delete-process proc))))))

(ert-deftest tramp-rpc-mock-test-cleanup-connection-hidden-sudo-via-rpc ()
  "Cleanup should remove RPC state for hidden native rpc+sudo paths."
  :tags '(:sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (skip-unless (tramp-rpc-mock-test--sudo-helper-available-p))
  (let* ((tramp-default-proxies-alist nil)
         (tramp-file-name-with-method "sudo")
         (vec (tramp-dissect-file-name
               (tramp-file-name-with-sudo "/rpc:alice@server:/root")))
         (buffer (generate-new-buffer " *tramp-rpc-cleanup-sudo-test*"))
         (proc (make-process :name "tramp-rpc-cleanup-sudo-test"
                             :buffer buffer
                             :command '("cat")
                             :connection-type 'pipe
                             :noquery t))
         (key (tramp-rpc--connection-key vec)))
    (unwind-protect
        (progn
          (puthash key (list :process proc :buffer buffer)
                   tramp-rpc--connections)
          (puthash buffer 'pending tramp-rpc--pending-responses)
          (cl-letf (((symbol-function 'tramp-rpc--cleanup-async-processes)
                     (lambda (&rest _) nil))
                    ((symbol-function 'tramp-rpc--cleanup-pty-processes)
                     (lambda (&rest _) nil))
                    ((symbol-function 'tramp-rpc--cleanup-watches-for-connection)
                     (lambda (&rest _) nil))
                    ((symbol-function 'tramp-rpc--cleanup-file-notify-for-connection)
                     (lambda (&rest _) nil))
                    ((symbol-function 'tramp-rpc--clear-direnv-cache)
                     (lambda (&rest _) nil))
                    ((symbol-function 'tramp-rpc--clear-file-caches-for-connection)
                     (lambda (&rest _) nil))
                    ((symbol-function 'tramp-rpc--cleanup-controlmaster)
                     (lambda (&rest _) nil))
                    ((symbol-function 'tramp-flush-directory-properties)
                     (lambda (&rest _) nil))
                    ((symbol-function 'tramp-flush-connection-properties)
                     (lambda (&rest _) nil)))
            (tramp-rpc-cleanup-connection vec))
          (should-not (gethash key tramp-rpc--connections))
          (should-not (gethash buffer tramp-rpc--pending-responses)))
      (remhash key tramp-rpc--connections)
      (remhash buffer tramp-rpc--pending-responses)
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest tramp-rpc-mock-test-start-server-sudo-password-uses-stdin ()
  "When sudo needs a password, start the elevated RPC server with sudo -S."
  :tags '(:sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name
              "/rpc:alice@server|sudo:root@server:/root/"))
        (orig-make-process (symbol-function 'make-process))
        command sent proc)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq command (plist-get plist :command))
                 (setq proc (funcall orig-make-process
                                      :name "tramp-rpc-mock-cat"
                                      :buffer nil
                                      :command '("cat")
                                      :connection-type 'pipe
                                      :noquery t))))
              ((symbol-function 'process-send-string)
               (lambda (_process string)
                 (setq sent string)))
              ((symbol-function 'tramp-rpc--call)
               (lambda (_vec method _params)
                 (should (equal method "system.info"))
                 '((uid . 0) (gid . 0) (home . "/root") (shell . "/bin/sh"))))
              ((symbol-function 'tramp-set-connection-local-variables)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (should (tramp-rpc--start-server-process
                     vec "/tmp/tramp-rpc-server" "secret"))
            (should (member "sudo" command))
            (should (member "-k" command))
            (should (member "-S" command))
            (should-not (member "-n" command))
            (should (member "-p" command))
            (should (member "Password:" command))
            (should (member "-H" command))
            (should-not (member "" command))
            (should (equal sent "secret\n")))
        (when (processp proc)
          (delete-process proc))
        (tramp-rpc--remove-connection vec)))))

(ert-deftest tramp-rpc-mock-test-start-server-sudo-noninteractive-uses-n-H ()
  "When sudo has a cached ticket, start the elevated RPC server with sudo -n -H."
  :tags '(:sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name
              "/rpc:alice@server|sudo:root@server:/root/"))
        (orig-make-process (symbol-function 'make-process))
        command sent proc)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq command (plist-get plist :command))
                 (setq proc (funcall orig-make-process
                                      :name "tramp-rpc-mock-cat"
                                      :buffer nil
                                      :command '("cat")
                                      :connection-type 'pipe
                                      :noquery t))))
              ((symbol-function 'process-send-string)
               (lambda (_process string)
                 (setq sent string)))
              ((symbol-function 'tramp-rpc--call)
               (lambda (_vec method _params)
                 (should (equal method "system.info"))
                 '((uid . 0) (gid . 0) (home . "/root") (shell . "/bin/sh"))))
              ((symbol-function 'tramp-set-connection-local-variables)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (should (tramp-rpc--start-server-process
                     vec "/tmp/tramp-rpc-server" nil))
            (should (member "sudo" command))
            (should (member "-n" command))
            (should (member "-H" command))
            (should-not (member "-S" command))
            (should-not (member "-p" command))
            (should-not (member "Password:" command))
            (should-not sent))
        (when (processp proc)
          (delete-process proc))
        (tramp-rpc--remove-connection vec)))))

(ert-deftest tramp-rpc-mock-test-password-string-unwraps-auth-source-entry ()
  "Normalize auth-source plist secrets before sending them to sudo -S."
  :tags '(:sudo)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (should (equal (tramp-rpc--password-string
                  '(:host "x220-nixos" :user "arthur" :port "sudo"
                    :secret (lambda () "secret")))
                 "secret")))

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

(ert-deftest tramp-rpc-mock-test-vc-exec-after-running-process-no-private-vc-sentinel ()
  "Test run-state handler doesn't call removed `vc--process-sentinel'."
  :tags '(:vc-handler)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (require 'vc-dispatcher)
  (let* ((buffer (generate-new-buffer " *tramp-rpc-vc-exec-after-test*"))
         (proc (make-process :name "tramp-rpc-vc-exec-after-test"
                             :buffer buffer
                             :command '("sh" "-c" "sleep 60")
                             :noquery t))
         (ran nil))
    (unwind-protect
        (with-current-buffer buffer
          (process-put proc :tramp-rpc-pid 123)
          (cl-letf (((symbol-function 'vc--process-sentinel)
                     (lambda (&rest _) (error "vc--process-sentinel called")))
                    ((symbol-function 'tramp-run-real-handler)
                     (lambda (&rest _) (error "Unexpected process state"))))
            (tramp-rpc-handle-vc-exec-after
             (lambda () (setq ran t)))
            (cl-letf (((symbol-function 'process-status) (lambda (_process) 'exit))
                      ((symbol-function 'process-exit-status) (lambda (_process) 0)))
              (funcall (process-sentinel proc) proc "finished")))
          (should ran))
      (when (process-live-p proc)
        (delete-process proc))
      (kill-buffer buffer))))

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
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (_filename) t))
              ((symbol-function 'tramp-rpc--fetch-default-remote-path)
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
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (_filename) t))
              ((symbol-function 'tramp-rpc--fetch-remote-exec-path)
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

(ert-deftest tramp-rpc-mock-test-process-file-not-found-returns-127 ()
  "Only a structured spawn ENOENT becomes process-file status 127."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((default-directory "/rpc:user@host:/work/"))
    (cl-letf (((symbol-function 'tramp-rpc--cached-remote-path)
               (lambda (_vec) '("/usr/bin")))
              ((symbol-function 'tramp-rpc--get-direnv-environment)
               (lambda (&rest _) nil))
              ((symbol-function 'tramp-rpc--caller-environment)
               (lambda () nil))
              ((symbol-function 'tramp-rpc-magit--process-cache-lookup)
               (lambda (&rest _) nil))
               ((symbol-function 'tramp-rpc--call)
                (lambda (&rest _)
                  (tramp-rpc--signal-rpc-error
                   "RPC" "missing executable" tramp-rpc-protocol-error-process 2
                   nil '((spawn_not_found . t))))))
       (should (= (tramp-rpc-handle-process-file "missing" nil nil nil) 127)))))

(ert-deftest tramp-rpc-mock-test-process-file-preserves-other-rpc-errors ()
  "A process cwd ENOENT remains a remote-file-error, not status 127."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((default-directory "/rpc:user@host:/work/"))
    (cl-letf (((symbol-function 'tramp-rpc--cached-remote-path)
               (lambda (_vec) '("/usr/bin")))
              ((symbol-function 'tramp-rpc--get-direnv-environment)
               (lambda (&rest _) nil))
              ((symbol-function 'tramp-rpc--caller-environment)
               (lambda () nil))
              ((symbol-function 'tramp-rpc-magit--process-cache-lookup)
               (lambda (&rest _) nil))
               ((symbol-function 'tramp-rpc--call)
                (lambda (&rest _)
                  (tramp-rpc--signal-rpc-error
                   "RPC" "missing cwd" tramp-rpc-protocol-error-process 2 nil
                   '((os_errno . 2) (spawn_not_found . :msgpack-false))))))
       (should-error (tramp-rpc-handle-process-file "broken" nil nil nil)
                     :type 'remote-file-error))))

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

(ert-deftest tramp-rpc-mock-test-make-process-connection-type-nil-is-pipe ()
  "An explicit `:connection-type nil' must not fall back to global PTY mode."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((default-directory "/rpc:user@host:/work/")
        (process-connection-type t)
        (started nil)
        (password-probed nil)
        (pty-called nil)
        proc)
    (cl-letf (((symbol-function 'tramp-rpc--sudo-password-required-p)
               (lambda (_vec)
                 (setq password-probed t)
                 nil))
              ((symbol-function 'tramp-rpc--sudo-read-password)
               (lambda (&rest _)
                 (error "Password should not be read when sudo -n succeeds")))
              ((symbol-function 'tramp-rpc--remote-path-environment)
               (lambda (_vec) nil))
              ((symbol-function 'tramp-rpc--tramp-remote-process-environment)
               (lambda () nil))
              ((symbol-function 'tramp-rpc--get-direnv-environment)
               (lambda (&rest _) nil))
              ((symbol-function 'tramp-rpc--caller-environment)
               (lambda () nil))
              ((symbol-function 'tramp-rpc--start-remote-process)
               (lambda (_vec program args _cwd _env)
                 (setq started (list program args))
                 4242))
              ((symbol-function 'tramp-rpc--write-remote-process)
               (lambda (&rest _) nil))
              ((symbol-function 'tramp-rpc--start-async-read)
               (lambda (&rest _) nil))
              ((symbol-function 'tramp-rpc--make-pty-process)
               (lambda (&rest _)
                 (setq pty-called t)
                 'pty-process)))
      (unwind-protect
          (progn
            (setq proc (tramp-rpc-handle-make-process
                        :name "tramp-rpc-connection-type-nil-test"
                        :buffer nil
                        :command '("sudo" "id")
                        :connection-type nil
                        :noquery t))
            (should (processp proc))
            (should-not pty-called)
            (should password-probed)
            (should (equal (car started) "sudo"))
            (should (equal (cadr started) '("-n" "id"))))
        (when (processp proc)
          (delete-process proc))))))

(ert-deftest tramp-rpc-mock-test-make-process-sudo-pipe-uses-stdin-password ()
  "Pipe-mode literal sudo should authenticate in the same stdin context."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((default-directory "/rpc:user@host:/work/")
        (started nil)
        (written nil)
        proc)
    (cl-letf (((symbol-function 'tramp-rpc--sudo-password-required-p)
               (lambda (_vec) t))
              ((symbol-function 'tramp-rpc--sudo-read-password)
               (lambda (_vec user)
                 (should (equal user "user"))
                 "secret"))
              ((symbol-function 'tramp-rpc--remote-path-environment)
               (lambda (_vec) nil))
              ((symbol-function 'tramp-rpc--tramp-remote-process-environment)
               (lambda () nil))
              ((symbol-function 'tramp-rpc--get-direnv-environment)
               (lambda (&rest _) nil))
              ((symbol-function 'tramp-rpc--caller-environment)
               (lambda () nil))
              ((symbol-function 'tramp-rpc--start-remote-process)
               (lambda (_vec program args _cwd _env)
                 (setq started (list program args))
                 4242))
              ((symbol-function 'tramp-rpc--write-remote-process)
               (lambda (_vec pid data)
                 (setq written (list pid data))))
              ((symbol-function 'tramp-rpc--start-async-read)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (setq proc (tramp-rpc-handle-make-process
                        :name "tramp-rpc-sudo-stdin-test"
                        :buffer nil
                        :command '("sudo" "id")
                        :connection-type nil
                        :noquery t))
            (should (processp proc))
            (should (equal (car started) "sudo"))
            (should (equal (cadr started)
                           '("-k" "-S" "-p" "" "id")))
            (should-not (member "-n" (cadr started)))
            (should (equal written '(4242 "secret\n"))))
        (when (processp proc)
          (delete-process proc))))))

(ert-deftest tramp-rpc-mock-test-process-cleanup-handles-already-exited-relay ()
  "Deferred cleanup must remove relays that exit before installation."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let* ((buffer (generate-new-buffer " *tramp-rpc-cleanup-exited-test*"))
         (proc (let ((process-connection-type nil))
                 (start-process "tramp-rpc-cleanup-exited-test" buffer "cat"))))
    (unwind-protect
        (progn
          (puthash proc '(:pid 4242) tramp-rpc--async-processes)
          (process-send-eof proc)
          (while (process-live-p proc)
            (accept-process-output proc 0.01 nil t))
          (should (gethash proc tramp-rpc--async-processes))
          (tramp-rpc--install-process-cleanup proc)
          (accept-process-output nil 0.05)
          (should-not (gethash proc tramp-rpc--async-processes))
          (should-not (get-buffer-process buffer)))
      (when (processp proc)
        (ignore-errors (delete-process proc)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest tramp-rpc-mock-test-sudo-via-rpc-pty-uses-rpc-backend ()
  "PTYs for sudo-via-RPC must not use direct SSH as the sudo target user."
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (let ((vec (tramp-dissect-file-name "/rpc:alice@server|sudo:root@server:/root/"))
        (tramp-rpc-use-direct-ssh-pty t)
        (rpc-called nil))
    (cl-letf (((symbol-function 'tramp-rpc--make-direct-ssh-pty-process)
               (lambda (&rest _)
                 (error "sudo-via-RPC PTY must not use direct SSH")))
              ((symbol-function 'tramp-rpc--make-rpc-pty-process)
               (lambda (got-vec name buffer command coding noquery
                         filter sentinel localname &optional direnv-env)
                 (setq rpc-called t)
                 (should (eq got-vec vec))
                 (should (equal name "sudo-pty"))
                 (should-not buffer)
                 (should (equal command '("id")))
                 (should-not coding)
                 (should noquery)
                 (should-not filter)
                 (should-not sentinel)
                 (should (equal localname "/root/"))
                 (should-not direnv-env)
                 'rpc-pty)))
      (should (eq (tramp-rpc--make-pty-process
                   vec "sudo-pty" nil '("id") nil t nil nil "/root/" nil)
                  'rpc-pty))
      (should rpc-called))))

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
