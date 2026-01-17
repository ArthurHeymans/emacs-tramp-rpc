;;; tramp-rpc-mock-tests.el --- Mock tests for TRAMP RPC (CI-compatible)  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>

;; This file is part of tramp-rpc.

;;; Commentary:

;; This file provides mock tests that can run in CI without SSH access.
;; It tests the RPC server directly via a local pipe connection.
;;
;; These tests focus on:
;; - Protocol correctness
;; - Server response handling
;; - Error handling
;;
;; Run with:
;;   emacs -Q --batch -l test/tramp-rpc-mock-tests.el -f tramp-rpc-mock-test-all

;;; Code:

(require 'ert)
(require 'json)
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

(require 'tramp-rpc-protocol)

;;; ============================================================================
;;; Protocol Tests (No server required)
;;; ============================================================================

(ert-deftest tramp-rpc-mock-test-protocol-encode-request ()
  "Test JSON-RPC request encoding."
  (let* ((result (tramp-rpc-protocol-encode-request-with-id "file.exists" '((path . "/test"))))
         (id (car result))
         (json-str (cdr result))
         (parsed (json-read-from-string json-str)))
    ;; Check structure
    (should (assoc 'jsonrpc parsed))
    (should (equal (cdr (assoc 'jsonrpc parsed)) "2.0"))
    (should (assoc 'method parsed))
    (should (equal (cdr (assoc 'method parsed)) "file.exists"))
    (should (assoc 'params parsed))
    (should (equal (cdr (assoc 'path (cdr (assoc 'params parsed)))) "/test"))
    (should (assoc 'id parsed))
    ;; ID should match returned ID
    (should (equal (cdr (assoc 'id parsed)) id))))

(ert-deftest tramp-rpc-mock-test-protocol-decode-success ()
  "Test JSON-RPC success response decoding."
  (let* ((response-json "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"exists\":true}}")
         (response (tramp-rpc-protocol-decode-response response-json)))
    (should (plist-get response :id))
    (should (equal (plist-get response :id) 1))
    (should (plist-get response :result))
    (should-not (tramp-rpc-protocol-error-p response))))

(ert-deftest tramp-rpc-mock-test-protocol-decode-error ()
  "Test JSON-RPC error response decoding."
  (let* ((response-json "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32001,\"message\":\"File not found\"}}")
         (response (tramp-rpc-protocol-decode-response response-json)))
    (should (tramp-rpc-protocol-error-p response))
    (should (= (tramp-rpc-protocol-error-code response) -32001))
    (should (equal (tramp-rpc-protocol-error-message response) "File not found"))))

(ert-deftest tramp-rpc-mock-test-protocol-batch-encode ()
  "Test JSON-RPC batch request encoding."
  (let* ((requests '(("file.exists" . ((path . "/a")))
                     ("file.stat" . ((path . "/b")))))
         (result (tramp-rpc-protocol-encode-batch-request-with-id requests))
         (id (car result))
         (json-str (cdr result))
         (parsed (json-read-from-string json-str)))
    ;; Should be a single request with batch method
    (should (assoc 'method parsed))
    (should (equal (cdr (assoc 'method parsed)) "batch"))
    (should (assoc 'params parsed))
    (let ((params (cdr (assoc 'params parsed))))
      (should (assoc 'requests params))
      (let ((reqs (cdr (assoc 'requests params))))
        (should (= (length reqs) 2))))))

(ert-deftest tramp-rpc-mock-test-protocol-batch-decode ()
  "Test JSON-RPC batch response decoding."
  (let* ((response-plist '(:id 1
                           :result ((results . [((result . t))
                                                ((error (code . -32001)
                                                        (message . "Error")))]))))
         (decoded (tramp-rpc-protocol-decode-batch-response response-plist)))
    (should (listp decoded))
    (should (= (length decoded) 2))
    ;; First result is success
    (should (eq (car decoded) t))
    ;; Second is error
    (should (plist-get (cadr decoded) :error))))

;;; ============================================================================
;;; JSON-RPC ID Generation Tests
;;; ============================================================================

(ert-deftest tramp-rpc-mock-test-protocol-id-uniqueness ()
  "Test that request IDs are unique."
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
                                               tramp-rpc-mock-test--project-root))
         (python-script (expand-file-name "server/tramp-rpc-server.py"
                                          tramp-rpc-mock-test--project-root)))
    (cond
     ((file-executable-p rust-binary) rust-binary)
     ((file-executable-p rust-binary-server) rust-binary-server)
     ((file-executable-p rust-binary-musl) rust-binary-musl)
     ((file-executable-p rust-debug) rust-debug)
     ((file-executable-p rust-debug-server) rust-debug-server)
     ((file-exists-p python-script) python-script)
     (t nil))))

(defun tramp-rpc-mock-test--start-server ()
  "Start a local RPC server for testing."
  (let ((server (tramp-rpc-mock-test--find-server)))
    (unless server
      (error "No RPC server found. Build with 'cargo build --release' or use Python server"))
    (setq tramp-rpc-mock-test-temp-dir (make-temp-file "tramp-rpc-test" t))
    (setq tramp-rpc-mock-test-server-buffer (generate-new-buffer "*tramp-rpc-test-server*"))
    (setq tramp-rpc-mock-test-server-process
          (let ((process-connection-type nil))  ; Use pipes
            (if (string-suffix-p ".py" server)
                (start-process "test-server" tramp-rpc-mock-test-server-buffer
                               "python3" server)
              (start-process "test-server" tramp-rpc-mock-test-server-buffer
                             server))))
    (set-process-query-on-exit-flag tramp-rpc-mock-test-server-process nil)
    (set-process-coding-system tramp-rpc-mock-test-server-process 'utf-8 'utf-8)
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
  "Send an RPC call to the local test server."
  (unless (and tramp-rpc-mock-test-server-process
               (process-live-p tramp-rpc-mock-test-server-process))
    (error "Server not running"))
  (let* ((id-and-request (tramp-rpc-protocol-encode-request-with-id method params))
         (request (cdr id-and-request)))
    ;; Send request
    (process-send-string tramp-rpc-mock-test-server-process (concat request "\n"))
    ;; Read response
    (with-current-buffer tramp-rpc-mock-test-server-buffer
      (let ((timeout 5.0)
            response-line)
        (while (and (not response-line) (> timeout 0))
          (accept-process-output tramp-rpc-mock-test-server-process 0.1)
          (goto-char (point-min))
          (when (search-forward "\n" nil t)
            (setq response-line (buffer-substring (point-min) (1- (point))))
            (delete-region (point-min) (point)))
          (cl-decf timeout 0.1))
        (when response-line
          (let ((response (tramp-rpc-protocol-decode-response response-line)))
            (if (tramp-rpc-protocol-error-p response)
                (list :error (tramp-rpc-protocol-error-message response))
              (plist-get response :result))))))))

;;; Server tests (require server to be available)

(ert-deftest tramp-rpc-mock-test-server-system-info ()
  "Test system.info RPC call."
  :tags '(:server)
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
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let ((test-file (expand-file-name "test.txt" tramp-rpc-mock-test-temp-dir)))
          ;; File shouldn't exist yet (false becomes nil in Emacs JSON)
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "file.exists" `((path . ,test-file)))))
            (should (not result)))

          ;; Write a file
          (let ((content (base64-encode-string "hello world" t)))
            (tramp-rpc-mock-test--rpc-call
             "file.write" `((path . ,test-file)
                            (content . ,content)
                            (append . :json-false))))

          ;; File should exist now
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "file.exists" `((path . ,test-file)))))
            (should result))

          ;; Read the file
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "file.read" `((path . ,test-file)))))
            (should result)
            (let ((content (base64-decode-string (alist-get 'content result))))
              (should (equal content "hello world"))))

          ;; Get file stats
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "file.stat" `((path . ,test-file)))))
            (should result)
            (should (equal (alist-get 'type result) "file"))
            (should (= (alist-get 'size result) 11)))  ; "hello world" = 11 bytes

          ;; Delete the file
          (tramp-rpc-mock-test--rpc-call "file.delete" `((path . ,test-file)))
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "file.exists" `((path . ,test-file)))))
            (should (not result)))))
    (tramp-rpc-mock-test--stop-server)))

(ert-deftest tramp-rpc-mock-test-server-directory-operations ()
  "Test directory operations via RPC."
  :tags '(:server)
  (skip-unless (tramp-rpc-mock-test--find-server))
  (unwind-protect
      (progn
        (tramp-rpc-mock-test--start-server)
        (let ((test-dir (expand-file-name "subdir" tramp-rpc-mock-test-temp-dir)))
          ;; Create directory
          (tramp-rpc-mock-test--rpc-call
           "dir.create" `((path . ,test-dir) (parents . :json-false)))

          ;; Check it exists
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "file.stat" `((path . ,test-dir)))))
            (should result)
            (should (equal (alist-get 'type result) "directory")))

          ;; Create files in directory
          (tramp-rpc-mock-test--rpc-call
           "file.write" `((path . ,(expand-file-name "file1.txt" test-dir))
                          (content . ,(base64-encode-string "a" t))
                          (append . :json-false)))
          (tramp-rpc-mock-test--rpc-call
           "file.write" `((path . ,(expand-file-name "file2.txt" test-dir))
                          (content . ,(base64-encode-string "b" t))
                          (append . :json-false)))

          ;; List directory
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "dir.list" `((path . ,test-dir)
                                      (include_attrs . :json-false)
                                      (include_hidden . t)))))
            (should result)
            (let ((names (mapcar (lambda (e) (alist-get 'name e)) result)))
              (should (member "file1.txt" names))
              (should (member "file2.txt" names))))

          ;; Remove directory recursively
          (tramp-rpc-mock-test--rpc-call
           "dir.remove" `((path . ,test-dir) (recursive . t)))

          ;; Should be gone (false becomes nil in JSON parsing)
          (let ((result (tramp-rpc-mock-test--rpc-call
                         "file.exists" `((path . ,test-dir)))))
            (should (not result)))))
    (tramp-rpc-mock-test--stop-server)))

(defun tramp-rpc-mock-test--decode-output (data encoding)
  "Decode DATA according to ENCODING (text or base64)."
  (cond
   ((null data) "")
   ((equal encoding "text") data)
   (t (base64-decode-string data))))

(ert-deftest tramp-rpc-mock-test-server-process-run ()
  "Test process.run RPC call."
  :tags '(:server :process)
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
          (let ((stdout (tramp-rpc-mock-test--decode-output
                         (alist-get 'stdout result)
                         (alist-get 'stdout_encoding result))))
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
