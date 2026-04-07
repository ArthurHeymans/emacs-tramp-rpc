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
                   '("target" "user" 22 nil)))))

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

(ert-deftest tramp-rpc-mock-test-zz-file-name-with-sudo-rpc-via-ssh ()
  "Test that RPC sudo elevation is rewritten through an SSH hop."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (skip-unless (tramp-rpc-mock-test--sudo-helper-available-p))
  (let* ((tramp-file-name-with-method "sudo")
         (filename (tramp-file-name-with-sudo "/rpc:user@target:/etc/hosts"))
         (vec (tramp-dissect-file-name filename))
         (hop (tramp-file-name-hop vec)))
    (should (tramp-tramp-file-p filename))
    (should (equal (tramp-file-name-localname vec) "/etc/hosts"))
    (should hop)
    (should (string-match-p (rx bos "ssh:") hop))
    (should-not (string-match-p "rpc:" hop))
    (should-not (string-match-p (rx bos "/rpc:") filename))))

(ert-deftest tramp-rpc-mock-test-zz-file-name-with-sudo-non-rpc-passthrough ()
  "Test that non-RPC sudo elevation still calls the original function."
  :tags '(:multi-hop)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  (skip-unless (tramp-rpc-mock-test--sudo-helper-available-p))
  (let ((called-with nil)
        (filename "/ssh:user@target:/etc/hosts"))
    (should
     (eq (tramp-rpc--file-name-with-sudo-advice
          (lambda (arg)
            (setq called-with arg)
            'passthrough)
          filename)
         'passthrough))
    (should (equal called-with (expand-file-name filename)))))

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
Regression test for GitHub issue #123: `sudo-edit' on an rpc-backed
file produced /rpc:server|sudo:root@server:/path, which then caused
  user-error: Host name `server' does not match `localhost-regexp'
because rpc had no tramp-login-args and the host-check in
`tramp-compute-multi-hops' rejected it."
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
    ;; Before the fix, tramp-compute-multi-hops would signal:
    ;;   user-error: Host name `server' does not match `localhost-regexp'
    ;; because the rpc item in target-alist had no %h in tramp-login-args.
    (should (setq result (tramp-compute-multi-hops sudo-vec)))
    ;; The chain should contain 2 elements: rpc proxy hop + sudo destination.
    (should (= (length result) 2))
    ;; The first element (gateway hop) should be the rpc method.
    (should (string= (tramp-file-name-method (car result)) "rpc"))))

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

(ert-deftest tramp-rpc-mock-test-dir-locals-advice-installed ()
  "Test that the dir-locals advice is installed and removed correctly."
  :tags '(:dir-locals)
  (skip-unless tramp-rpc-mock-test--tramp-rpc-loaded)
  ;; After loading tramp-rpc-advice, the advice should be active.
  (should (advice-member-p #'tramp-rpc--hack-dir-local-variables-advice
                           'hack-dir-local-variables))
  ;; After removing, it should be gone.
  (unwind-protect
      (progn
        (tramp-rpc-advice-remove)
        (should-not (advice-member-p #'tramp-rpc--hack-dir-local-variables-advice
                                     'hack-dir-local-variables)))
    ;; Restore advice for remaining tests.
    (tramp-rpc-advice-install)))

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
