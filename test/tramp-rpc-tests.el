;;; tramp-rpc-tests.el --- Tests for TRAMP RPC backend  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>

;; This file is part of tramp-rpc.

;; tramp-rpc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Comprehensive test suite for TRAMP RPC backend.
;;
;; These tests cover all major functionality of the RPC backend:
;; - File operations (read, write, copy, rename, delete)
;; - Directory operations (list, create, delete)
;; - File attributes and metadata
;; - Process execution
;; - Symbolic links
;;
;; Test Modes:
;; -----------
;; 1. Mock mode (CI): Uses a local mock server for testing without SSH.
;;    Set TRAMP_RPC_TEST_MOCK=1 environment variable.
;;
;; 2. Remote mode: Tests against a real remote host.
;;    Set TRAMP_RPC_TEST_HOST to the hostname (e.g., "x220-nixos").
;;
;; Running Tests:
;; --------------
;; From command line:
;;   emacs -Q --batch -l test/tramp-rpc-tests.el -f ert-run-tests-batch-and-exit
;;
;; Interactively:
;;   M-x ert-run-tests-interactively RET tramp-rpc RET
;;
;; Run all tests:
;;   M-x tramp-rpc-test-all
;;
;; Test Tags:
;; ----------
;; :expensive-test - Time-consuming tests (skipped in quick runs)
;; :unstable       - Tests that may fail intermittently
;; :process        - Tests requiring async process support

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'tramp)

;; Compute project root at load time (with fallback for eval after load)
(defvar tramp-rpc-test--project-root
  (expand-file-name "../" (file-name-directory
                           (or load-file-name buffer-file-name
                               (expand-file-name "test/tramp-rpc-tests.el"))))
  "Project root directory.")

;; Load tramp-rpc from the project
(let ((lisp-dir (expand-file-name "lisp" tramp-rpc-test--project-root)))
  (add-to-list 'load-path lisp-dir))

;; Ensure tests exercise updated source, not stale bytecode.
(setq load-prefer-newer t)

;; Install msgpack from MELPA if not available (required by tramp-rpc-protocol)
(unless (require 'msgpack nil t)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'msgpack)
  (require 'msgpack))

(require 'tramp-rpc)

(declare-function tramp-rpc--handle-async-read-response "tramp-rpc-process")

;;; ============================================================================
;;; Test Configuration
;;; ============================================================================

(defvar tramp-rpc-test-host (or (getenv "TRAMP_RPC_TEST_HOST") "localhost")
  "Remote host for testing.
Set via TRAMP_RPC_TEST_HOST environment variable.")

(defvar tramp-rpc-test-user (getenv "TRAMP_RPC_TEST_USER")
  "User for remote host. If nil, uses default SSH user.")

(defvar tramp-rpc-test-host-2 (getenv "TRAMP_RPC_TEST_HOST_2")
  "Second remote host for cross-remote tests.
Set via TRAMP_RPC_TEST_HOST_2 environment variable.
When set, cross-remote tests (copy/rename between different hosts) are run.")

(defvar tramp-rpc-test-mock-mode (getenv "TRAMP_RPC_TEST_MOCK")
  "When non-nil, use mock mode for testing without SSH.")

(defvar tramp-rpc-test-temp-dir "/tmp/tramp-rpc-test"
  "Remote directory for test files.")

(defconst tramp-rpc-test-name-prefix "tramp-rpc-test"
  "Prefix for temporary test files.")

;; Test configuration
(setq tramp-verbose 0  ; Reduce noise during tests
      tramp-cache-read-persistent-data nil
      tramp-persistency-file-name nil
      password-cache-expiry nil)

;;; ============================================================================
;;; Test Infrastructure
;;; ============================================================================

(defvar tramp-rpc-test-vec nil
  "Current test connection vector.")

(defvar tramp-rpc-test-enabled-checked nil
  "Cached result of `tramp-rpc-test-enabled'.
Value is a cons cell (CHECKED . RESULT).")

(defun tramp-rpc-test--clear-call-count-caches ()
  "Clear TRAMP/TRAMP-RPC caches before call-count measurement."
  (maphash
   (lambda (_key conn)
     (let* ((proc (plist-get conn :process))
            (vec (and proc (process-get proc :tramp-rpc-vec))))
       (when vec
         (tramp-flush-directory-properties vec "/")
         (tramp-flush-connection-properties vec)
         (tramp-rpc--clear-file-caches-for-connection vec))))
   tramp-rpc--connections))

(defun tramp-rpc-test--run-with-call-count (thunk)
  "Run THUNK and return (RESULT . RPC-CALL-COUNT)."
  (let ((count 0)
        result
        (orig-call-with-timeout (symbol-function 'tramp-rpc--call-with-timeout))
        (orig-call-batch (symbol-function 'tramp-rpc--call-batch))
        (orig-call-async (symbol-function 'tramp-rpc--call-async))
        (orig-send-requests (symbol-function 'tramp-rpc--send-requests)))
    (tramp-rpc-test--clear-call-count-caches)
    (cl-letf (((symbol-function 'tramp-rpc--call-with-timeout)
               (lambda (vec method params total-timeout poll-interval)
                 (cl-incf count)
                 (funcall orig-call-with-timeout
                          vec method params total-timeout poll-interval)))
              ((symbol-function 'tramp-rpc--call-batch)
               (lambda (vec requests)
                 (cl-incf count)
                 (funcall orig-call-batch vec requests)))
              ((symbol-function 'tramp-rpc--call-async)
               (lambda (vec method params callback)
                 (cl-incf count)
                 (funcall orig-call-async vec method params callback)))
              ((symbol-function 'tramp-rpc--send-requests)
               (lambda (vec requests)
                 (cl-incf count (length requests))
                 (funcall orig-send-requests vec requests))))
      (setq result (funcall thunk))
      (cons result count))))

(defun tramp-rpc-test--run-with-call-count-capturing-error (thunk)
  "Run THUNK and return a plist with :result, :error, and :count.
Unlike `tramp-rpc-test--run-with-call-count', this preserves the call count
when THUNK signals so tests can assert error-path roundtrips."
  (let ((count 0)
        result
        error
        (orig-call-with-timeout (symbol-function 'tramp-rpc--call-with-timeout))
        (orig-call-batch (symbol-function 'tramp-rpc--call-batch))
        (orig-call-async (symbol-function 'tramp-rpc--call-async))
        (orig-send-requests (symbol-function 'tramp-rpc--send-requests)))
    (tramp-rpc-test--clear-call-count-caches)
    (cl-letf (((symbol-function 'tramp-rpc--call-with-timeout)
               (lambda (vec method params total-timeout poll-interval)
                 (cl-incf count)
                 (funcall orig-call-with-timeout
                          vec method params total-timeout poll-interval)))
              ((symbol-function 'tramp-rpc--call-batch)
               (lambda (vec requests)
                 (cl-incf count)
                 (funcall orig-call-batch vec requests)))
              ((symbol-function 'tramp-rpc--call-async)
               (lambda (vec method params callback)
                 (cl-incf count)
                 (funcall orig-call-async vec method params callback)))
              ((symbol-function 'tramp-rpc--send-requests)
               (lambda (vec requests)
                 (cl-incf count (length requests))
                 (funcall orig-send-requests vec requests))))
      (condition-case err
          (setq result (funcall thunk))
        (error (setq error err)))
      (list :result result :error error :count count))))

(defmacro tramp-rpc-test--with-call-count (expected &rest body)
  "Run BODY and assert it makes EXPECTED RPC calls.
Returns BODY's result."
  (declare (indent 1) (debug (form body)))
  (let ((measurement (make-symbol "measurement")))
    `(let ((,measurement
            (tramp-rpc-test--run-with-call-count
             (lambda () ,@body))))
       (should (= ,expected (cdr ,measurement)))
       (car ,measurement))))

(defmacro tramp-rpc-test--with-call-count-error (expected error-type &rest body)
  "Run BODY, assert ERROR-TYPE is signaled, and count EXPECTED RPC calls."
  (declare (indent 2) (debug (form symbolp body)))
  (let ((measurement (make-symbol "measurement"))
        (err (make-symbol "err")))
    `(should-error
      (let* ((,measurement
              (tramp-rpc-test--run-with-call-count-capturing-error
               (lambda () ,@body)))
             (,err (plist-get ,measurement :error)))
        (should (= ,expected (plist-get ,measurement :count)))
        (if ,err
            (signal (car ,err) (cdr ,err))
          (ert-fail "Expected an error, but body returned normally")))
      :type ',error-type)))

(defun tramp-rpc-test--make-remote-path (filename)
  "Make a full TRAMP RPC path for FILENAME."
  (let ((user-part (if tramp-rpc-test-user
                       (concat tramp-rpc-test-user "@")
                     "")))
    (format "/rpc:%s%s:%s/%s"
            user-part
            tramp-rpc-test-host
            tramp-rpc-test-temp-dir
            filename)))

(defun tramp-rpc-test--make-temp-name (&optional local)
  "Return a temporary file name for test.
If LOCAL is non-nil, return a local temp name.
The file is not created."
  (let ((name (make-temp-name tramp-rpc-test-name-prefix)))
    (if local
        (expand-file-name name temporary-file-directory)
      (tramp-rpc-test--make-remote-path name))))

(defun tramp-rpc-test--remote-directory ()
  "Return the remote test directory path."
  (let ((user-part (if tramp-rpc-test-user
                       (concat tramp-rpc-test-user "@")
                     "")))
    (format "/rpc:%s%s:%s" user-part tramp-rpc-test-host tramp-rpc-test-temp-dir)))

(defun tramp-rpc-test-enabled ()
  "Check if remote testing is enabled and working.
Returns non-nil if tests can run."
  (unless (consp tramp-rpc-test-enabled-checked)
    (setq tramp-rpc-test-enabled-checked
          (cons t
                (condition-case nil
                    (let ((dir (tramp-rpc-test--remote-directory)))
                      (and (file-remote-p dir)
                           (progn
                             ;; Try to create test directory
                             (ignore-errors (make-directory dir t))
                             (file-directory-p dir)
                             (file-writable-p dir))))
                  (error nil)))))
  (cdr tramp-rpc-test-enabled-checked))

(defun tramp-rpc-test--make-remote-path-2 (filename)
  "Make a full TRAMP RPC path on the second host for FILENAME."
  (format "/rpc:%s:%s/%s"
          tramp-rpc-test-host-2
          tramp-rpc-test-temp-dir
          filename))

(defun tramp-rpc-test--make-temp-name-2 ()
  "Return a temporary file name on the second host.
The file is not created."
  (tramp-rpc-test--make-remote-path-2
   (make-temp-name tramp-rpc-test-name-prefix)))

(defun tramp-rpc-test--remote-directory-2 ()
  "Return the remote test directory path on the second host."
  (format "/rpc:%s:%s" tramp-rpc-test-host-2 tramp-rpc-test-temp-dir))

(defvar tramp-rpc-test-cross-remote-checked nil
  "Cached result of `tramp-rpc-test-cross-remote-enabled'.
Value is a cons cell (CHECKED . RESULT).")

(defun tramp-rpc-test-cross-remote-enabled ()
  "Check if cross-remote testing is enabled and working.
Returns non-nil if both test hosts are reachable."
  (unless (consp tramp-rpc-test-cross-remote-checked)
    (setq tramp-rpc-test-cross-remote-checked
          (cons t
                (and tramp-rpc-test-host-2
                     (tramp-rpc-test-enabled)
                     (condition-case nil
                         (let ((dir (tramp-rpc-test--remote-directory-2)))
                           (and (file-remote-p dir)
                                (progn
                                  (ignore-errors (make-directory dir t))
                                  (file-directory-p dir)
                                  (file-writable-p dir))))
                       (error nil))))))
  (cdr tramp-rpc-test-cross-remote-checked))

(defun tramp-rpc-test--setup ()
  "Set up test environment."
  (let ((dir (tramp-rpc-test--remote-directory)))
    ;; Clean up any leftover test files
    (ignore-errors (delete-directory dir t))
    ;; Create fresh test directory
    (make-directory dir t))
  ;; Also set up second host if available
  (when tramp-rpc-test-host-2
    (let ((dir2 (tramp-rpc-test--remote-directory-2)))
      (ignore-errors (delete-directory dir2 t))
      (ignore-errors (make-directory dir2 t)))))

(defun tramp-rpc-test--cleanup ()
  "Clean up test environment."
  (let ((dir (tramp-rpc-test--remote-directory)))
    (ignore-errors (delete-directory dir t)))
  (when tramp-rpc-test-host-2
    (let ((dir2 (tramp-rpc-test--remote-directory-2)))
      (ignore-errors (delete-directory dir2 t))))
  (ignore-errors (tramp-cleanup-all-connections)))

(defmacro tramp-rpc-test--with-temp-file (var content &rest body)
  "Create a temporary remote file with CONTENT, bind to VAR, execute BODY.
The file is deleted after BODY completes."
  (declare (indent 2) (debug (symbolp form body)))
  `(let ((,var (tramp-rpc-test--make-temp-name)))
     (unwind-protect
         (progn
           (write-region ,content nil ,var)
           ,@body)
       (ignore-errors (delete-file ,var)))))

(defmacro tramp-rpc-test--with-temp-dir (var &rest body)
  "Create a temporary remote directory, bind to VAR, execute BODY.
The directory is deleted after BODY completes."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (tramp-rpc-test--make-temp-name)))
     (unwind-protect
         (progn
           (make-directory ,var)
           ,@body)
       (ignore-errors (delete-directory ,var t)))))

;;; ============================================================================
;;; Test 00: Availability
;;; ============================================================================

(ert-deftest tramp-rpc-test00-system-info-shared-cache ()
  "UID, GID, and home directory share one system.info RPC."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((vec (tramp-dissect-file-name (tramp-rpc-test--remote-directory))))
    (let ((result (tramp-rpc-test--with-call-count 1
                    (list (tramp-get-remote-uid vec 'integer)
                          (tramp-get-remote-gid vec 'integer)
                          (tramp-get-home-directory vec)))))
      (should (integerp (nth 0 result)))
      (should (integerp (nth 1 result)))
      (should (stringp (nth 2 result))))))

(ert-deftest tramp-rpc-test00-availability ()
  "Test availability of TRAMP RPC functions."
  (skip-unless (tramp-rpc-test-enabled))
  (let ((dir (tramp-rpc-test--remote-directory)))
    (should (file-remote-p dir))
    (should (file-directory-p dir))
    (should (file-writable-p dir))))

(ert-deftest tramp-rpc-test00-remote-groups-dynamic-sizing ()
  "The server returns the remote user's complete supplementary group list."
  (skip-unless (tramp-rpc-test-enabled))
  (let ((vec (tramp-dissect-file-name (tramp-rpc-test--remote-directory))))
    (should (listp (tramp-get-remote-groups vec 'integer)))))

(ert-deftest tramp-rpc-test-pipelined-timeout-signals-error ()
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

;;; ============================================================================
;;; Test 01: File Name Syntax
;;; ============================================================================

(ert-deftest tramp-rpc-test01-file-name-syntax ()
  "Check TRAMP RPC file name syntax."
  ;; Valid RPC file names
  (should (tramp-tramp-file-p "/rpc:host:"))
  (should (tramp-tramp-file-p "/rpc:host:/path"))
  (should (tramp-tramp-file-p "/rpc:user@host:"))
  (should (tramp-tramp-file-p "/rpc:user@host:/path"))
  (should (tramp-tramp-file-p "/rpc:user@host#22:"))
  (should (tramp-tramp-file-p "/rpc:user@host#22:/path"))

  ;; Check method extraction
  (should (equal (tramp-file-name-method
                  (tramp-dissect-file-name "/rpc:host:/path"))
                 "rpc"))
  (should (equal (tramp-file-name-host
                  (tramp-dissect-file-name "/rpc:host:/path"))
                 "host"))
  (should (equal (tramp-file-name-user
                  (tramp-dissect-file-name "/rpc:user@host:/path"))
                 "user"))
  (should (equal (tramp-file-name-localname
                  (tramp-dissect-file-name "/rpc:host:/path/to/file"))
                 "/path/to/file")))

;;; ============================================================================
;;; Test 02: File Existence
;;; ============================================================================

(ert-deftest tramp-rpc-test02-file-exists-p ()
  "Test `file-exists-p' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  ;; Test directory exists
  ;; Measure on a fresh path to avoid cache hits.
  (let ((missing (tramp-rpc-test--make-temp-name)))
    (should-not (tramp-rpc-test--with-call-count 1
                  (file-exists-p missing))))

  ;; Test file creation and existence
  (tramp-rpc-test--with-temp-file tmp "test content"
    (should (file-exists-p tmp)))

  ;; Test non-existent file
  (should-not (file-exists-p (tramp-rpc-test--make-remote-path "nonexistent-file-xyz"))))

(ert-deftest tramp-rpc-test02-file-readable-p ()
  "Test `file-readable-p' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-file tmp "test content"
    (should (file-readable-p tmp))))

(ert-deftest tramp-rpc-test02-file-writable-p ()
  "Test `file-writable-p' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  ;; Existing file.  TRAMP's generic writable check probes access and metadata.
  (tramp-rpc-test--with-temp-file tmp "test content"
    (should (tramp-rpc-test--with-call-count 3
              (file-writable-p tmp))))

  ;; Non-existent file in writable directory
  (let ((new-file (tramp-rpc-test--make-temp-name)))
    (should (file-writable-p new-file))))

;;; ============================================================================
;;; Test 03: File Types
;;; ============================================================================

(ert-deftest tramp-rpc-test03-file-directory-p ()
  "Test `file-directory-p' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  ;; Test directory
  ;; Measure on a fresh path to avoid cache hits.
  (let ((missing (tramp-rpc-test--make-temp-name)))
    (should-not (tramp-rpc-test--with-call-count 1
                  (file-directory-p missing)))
    ;; Negative results are cached too.
    (should (equal (tramp-rpc-test--with-call-count 1
                     (list (file-directory-p missing)
                           (file-directory-p missing)))
                   '(nil nil))))

  ;; Test file
  (tramp-rpc-test--with-temp-file tmp "test content"
    (should-not (file-directory-p tmp)))

  ;; Test subdirectory
  (tramp-rpc-test--with-temp-dir subdir
    (should (file-directory-p subdir))
    (should (tramp-rpc-test--with-call-count 1
              (and (file-directory-p subdir)
                   (file-directory-p subdir))))))

(ert-deftest tramp-rpc-test03-file-directory-p-cache-invalidated-by-mkdir ()
  "A cached negative `file-directory-p' result is invalidated by mkdir."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((dir (tramp-rpc-test--make-temp-name)))
    (unwind-protect
        (progn
          (should-not (file-directory-p dir))
          (make-directory dir)
          (should (file-directory-p dir)))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest tramp-rpc-test03-file-directory-p-cache-invalidated-by-mkdir-parents ()
  "Parent mkdir invalidates stale negative `file-directory-p' prefix caches."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((top (tramp-rpc-test--make-temp-name))
         (deep (concat top "/b/c")))
    (unwind-protect
        (progn
          (should-not (file-directory-p top))
          (make-directory deep t)
          (should (file-directory-p top)))
      (ignore-errors (delete-directory top t)))))

(ert-deftest tramp-rpc-test03-file-regular-p ()
  "Test `file-regular-p' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  ;; Regular file
  (tramp-rpc-test--with-temp-file tmp "test content"
    (should (tramp-rpc-test--with-call-count 1
              (file-regular-p tmp))))

  ;; Directory is not regular
  (should-not (file-regular-p (tramp-rpc-test--remote-directory))))

(ert-deftest tramp-rpc-test03-file-symlink-p ()
  "Test `file-symlink-p' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-file target "target content"
    (let ((link (tramp-rpc-test--make-temp-name)))
      (unwind-protect
          (progn
            ;; Create symlink
            (condition-case nil
                (make-symbolic-link target link)
              (file-error (ert-skip "Symlinks not supported")))
            ;; Test symlink detection
            (should (tramp-rpc-test--with-call-count 1
                      (file-symlink-p link)))
            ;; Original file is not a symlink
            (should-not (file-symlink-p target)))
        (ignore-errors (delete-file link))))))

;;; ============================================================================
;;; Test 04: File Attributes
;;; ============================================================================

(ert-deftest tramp-rpc-test04-file-attributes ()
  "Test `file-attributes' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-file tmp "test content"
    (let ((attrs (tramp-rpc-test--with-call-count 1
                  (file-attributes tmp))))
      (should attrs)
      ;; Check it's a regular file
      (should (null (file-attribute-type attrs)))
      ;; Check size
      (should (= (file-attribute-size attrs) (length "test content")))
      ;; Check user ID exists
      (should (integerp (file-attribute-user-id attrs)))
      ;; Check group ID exists
      (should (integerp (file-attribute-group-id attrs)))
      ;; Check mode string
      (should (stringp (file-attribute-modes attrs)))
      ;; Check times
      (should (file-attribute-modification-time attrs))
      (should (file-attribute-access-time attrs)))))

(ert-deftest tramp-rpc-test04-file-attributes-directory ()
  "Test `file-attributes' for TRAMP RPC directories."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-dir subdir
    (let ((attrs (tramp-rpc-test--with-call-count 1
                  (file-attributes subdir))))
      (should attrs)
      ;; Check it's a directory
      (should (eq (file-attribute-type attrs) t)))
    ;; Directory attributes populate the `file-directory-p' cache.
    (should (tramp-rpc-test--with-call-count 1
              (and (file-attributes subdir)
                   (file-directory-p subdir))))))

(ert-deftest tramp-rpc-test04-file-modes ()
  "Test `file-modes' and `set-file-modes' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-file tmp "test content"
    (let ((orig-modes (tramp-rpc-test--with-call-count 1
                        (file-modes tmp))))
      (should (integerp orig-modes))
      ;; Set new modes.  The mutation itself should be one RPC; the server can
      ;; report ENOENT/permission errors without a preflight stat.
      (tramp-rpc-test--with-call-count 1
        (set-file-modes tmp #o644))
      (should (= (logand (file-modes tmp) #o777) #o644))
      ;; Set executable
      (tramp-rpc-test--with-call-count 1
        (set-file-modes tmp #o755))
      (should (= (logand (file-modes tmp) #o777) #o755)))))

(ert-deftest tramp-rpc-test04-set-file-modes-missing-file ()
  "`set-file-modes' reports a missing file without a preflight stat."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((missing (tramp-rpc-test--make-temp-name)))
    (tramp-rpc-test--with-call-count-error 1 file-missing
      (set-file-modes missing #o644))))

(ert-deftest tramp-rpc-test04-file-executable-root ()
  "Root execution still requires at least one execute bit."
  (let ((attrs '(nil 1 1 1 0 0 0 0 "----------")))
    (should-not (tramp-rpc--mode-executable-p "----------" 0 0 attrs nil))
    (should (tramp-rpc--mode-executable-p "------x---" 0 0 attrs nil))))

;;; ============================================================================
;;; Test 05: File Content Operations
;;; ============================================================================

(ert-deftest tramp-rpc-test05-write-region ()
  "Test `write-region' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  ;; Simple write
  (let ((file (tramp-rpc-test--make-temp-name))
        (content "Hello, World!"))
    (unwind-protect
        (progn
          (write-region content nil file)
          (should (file-exists-p file))
          (should (equal content (with-temp-buffer
                                   (insert-file-contents file)
                                   (buffer-string)))))
      (ignore-errors (delete-file file)))))

(ert-deftest tramp-rpc-test05-write-region-offset-preserves-suffix ()
  "Writing at an integer offset leaves the remaining remote bytes intact."
  (skip-unless (tramp-rpc-test-enabled))
  (tramp-rpc-test--with-temp-file file "abcdef"
    (write-region "XY" nil file 2)
    (should (equal (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))
                   "abXYef"))))

(ert-deftest tramp-rpc-test05-write-region-offset-creates-zero-filled-file ()
  "An integer offset write creates a missing remote file with a zero-filled hole."
  (skip-unless (tramp-rpc-test-enabled))
  (let ((file (tramp-rpc-test--make-temp-name)))
    (unwind-protect
        (progn
          (write-region "XY" nil file 4)
          (should (equal (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally file)
                           (buffer-string))
                         "\0\0\0\0XY")))
      (ignore-errors (delete-file file)))))

(ert-deftest tramp-rpc-test05-write-region-multiline ()
  "Test `write-region' with multiline content."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((file (tramp-rpc-test--make-temp-name))
        (content "Line 1\nLine 2\nLine 3\n"))
    (unwind-protect
        (progn
          (write-region content nil file)
          (should (equal content (with-temp-buffer
                                   (insert-file-contents file)
                                   (buffer-string)))))
      (ignore-errors (delete-file file)))))

(ert-deftest tramp-rpc-test05-write-region-binary ()
  "Test `write-region' with binary content."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((file (tramp-rpc-test--make-temp-name))
        (content (apply #'unibyte-string (number-sequence 0 255))))
    (unwind-protect
        (progn
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert content)
            (write-region (point-min) (point-max) file))
          (let ((read-content (with-temp-buffer
                                (set-buffer-multibyte nil)
                                (insert-file-contents file)
                                (buffer-string))))
            (should (equal (length content) (length read-content)))
            (should (equal content read-content))))
      (ignore-errors (delete-file file)))))

(ert-deftest tramp-rpc-test05-write-region-utf8 ()
  "Test `write-region' with UTF-8 content."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((file (tramp-rpc-test--make-temp-name))
        (content "Hello, World!\nBonjour le monde!\nHallo Welt!\nPrzyklad: zolw"))
    (unwind-protect
        (progn
          (write-region content nil file)
          (should (equal content (with-temp-buffer
                                   (insert-file-contents file)
                                   (buffer-string)))))
      (ignore-errors (delete-file file)))))

(ert-deftest tramp-rpc-test05-write-region-chinese ()
  "Test `write-region' with Chinese characters.
This tests Issue #13: Chinese characters decode incorrectly."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((file (tramp-rpc-test--make-temp-name))
        ;; Test various Chinese characters including common and less common ones
        (content "中文测试\n你好世界\n繁體中文\n日本語テスト"))
    (unwind-protect
        (progn
          (write-region content nil file)
          (should (file-exists-p file))
          ;; Verify content is read back correctly
          (let ((read-content (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string))))
            (should (equal content read-content))))
      (ignore-errors (delete-file file)))))

(ert-deftest tramp-rpc-test05-write-region-mixed-unicode ()
  "Test `write-region' with mixed Unicode content."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((file (tramp-rpc-test--make-temp-name))
        ;; Mix of ASCII, Chinese, Japanese, Korean, and emoji
        (content "Hello 你好 こんにちは 안녕하세요"))
    (unwind-protect
        (progn
          (write-region content nil file)
          (should (equal content (with-temp-buffer
                                   (insert-file-contents file)
                                   (buffer-string)))))
      (ignore-errors (delete-file file)))))

(ert-deftest tramp-rpc-test05-write-region-append ()
  "Test `write-region' with append."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((file (tramp-rpc-test--make-temp-name))
        (content1 "First part\n")
        (content2 "Second part\n"))
    (unwind-protect
        (progn
          (write-region content1 nil file)
          (write-region content2 nil file t)  ; append
          (should (equal (concat content1 content2)
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))))
      (ignore-errors (delete-file file)))))

(ert-deftest tramp-rpc-test05-write-region-large ()
  "Test `write-region' with large file."
  :tags '(:expensive-test)
  (skip-unless (tramp-rpc-test-enabled))

  (let ((file (tramp-rpc-test--make-temp-name))
        (content (make-string 102400 ?x)))  ; 100KB
    (unwind-protect
        (progn
          (write-region content nil file)
          (let ((attrs (file-attributes file)))
            (should (= (file-attribute-size attrs) 102400)))
          (should (equal content (with-temp-buffer
                                   (insert-file-contents file)
                                   (buffer-string)))))
      (ignore-errors (delete-file file)))))

(ert-deftest tramp-rpc-test05-insert-file-contents ()
  "Test `insert-file-contents' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-file tmp "test content"
    (with-temp-buffer
      (tramp-rpc-test--with-call-count 1
        (insert-file-contents tmp))
      (should (equal (buffer-string) "test content")))))

(ert-deftest tramp-rpc-test05-insert-file-contents-partial ()
  "Test partial `insert-file-contents' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-file tmp "0123456789"
    ;; Read bytes 2-5
    (with-temp-buffer
      (tramp-rpc-test--with-call-count 1
        (insert-file-contents tmp nil 2 6))
      (should (equal (buffer-string) "2345")))))

;;; ============================================================================
;;; Test 06: File Manipulation
;;; ============================================================================

(ert-deftest tramp-rpc-test06-copy-file ()
  "Test `copy-file' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-file src "source content"
    (let ((dest (tramp-rpc-test--make-temp-name)))
      (unwind-protect
          (progn
            (tramp-rpc-test--with-call-count 2
              (copy-file src dest))
            (should (file-exists-p dest))
            (should (equal (with-temp-buffer
                             (insert-file-contents src)
                             (buffer-string))
                           (with-temp-buffer
                             (insert-file-contents dest)
                             (buffer-string)))))
        (ignore-errors (delete-file dest))))))

(ert-deftest tramp-rpc-test06-copy-file-to-directory ()
  "Test `copy-file' to a directory destination (issue #45).
When NEWNAME is a directory without trailing slash, `copy-file' should
signal `file-already-exists'.  With trailing slash (via
`file-name-as-directory'), it should copy the file INTO the directory."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-file src "source content for dir copy"
    (let ((dest-dir (tramp-rpc-test--make-temp-name)))
      (unwind-protect
          (progn
            ;; Create destination directory
            (make-directory dest-dir)
            (should (file-directory-p dest-dir))
            ;; Without trailing /, should signal file-already-exists
            (should-error (copy-file src dest-dir)
                          :type 'file-already-exists)
            ;; With ok-if-already-exists but no trailing /, should
            ;; signal file-error ("File is a directory")
            (should-error (copy-file src dest-dir 'ok)
                          :type 'file-error)
            ;; With trailing / (file-name-as-directory), should copy INTO dir
            (tramp-rpc-test--with-call-count 2
              (copy-file src (file-name-as-directory dest-dir)))
            ;; File should now exist inside the directory with original name
            (let ((expected-dest (expand-file-name
                                  (file-name-nondirectory src) dest-dir)))
              (should (file-exists-p expected-dest))
              (should (equal (with-temp-buffer
                               (insert-file-contents src)
                               (buffer-string))
                             (with-temp-buffer
                               (insert-file-contents expected-dest)
                               (buffer-string))))))
        (ignore-errors (delete-directory dest-dir t))))))

(ert-deftest tramp-rpc-test06-copy-directory-roundtrips ()
  "Same-remote `copy-directory' uses a constant number of RPC round-trips."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((src (tramp-rpc-test--make-temp-name))
         (nested (expand-file-name "nested" src))
         (dest (tramp-rpc-test--make-temp-name)))
    (unwind-protect
        (progn
          (make-directory nested t)
          (write-region "top" nil (expand-file-name "top.txt" src))
          (write-region "child" nil (expand-file-name "child.txt" nested))
          ;; `copy-directory' itself calls `file-in-directory-p' before the
          ;; file-name handler dispatches, so the end-to-end count includes
          ;; that generic TRAMP safety check plus the backend's batch stat and
          ;; single recursive `file.copy' RPC.  The important property is that
          ;; this stays constant regardless of tree size.
          (tramp-rpc-test--with-call-count 7
            (copy-directory src dest))
          (should (file-exists-p (expand-file-name "top.txt" dest)))
          (should (file-exists-p (expand-file-name "nested/child.txt" dest)))
          (should (equal "child"
                         (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "nested/child.txt" dest))
                           (buffer-string)))))
      (ignore-errors (delete-directory src t))
      (ignore-errors (delete-directory dest t)))))

(ert-deftest tramp-rpc-test06-copy-directory-to-existing-dir-roundtrips ()
  "Same-remote `copy-directory' into an existing directory is constant time."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((src (tramp-rpc-test--make-temp-name))
         (dest-parent (tramp-rpc-test--make-temp-name))
         (expected-dest (expand-file-name (file-name-nondirectory src) dest-parent)))
    (unwind-protect
        (progn
          (make-directory src)
          (write-region "payload" nil (expand-file-name "payload.txt" src))
          (make-directory dest-parent)
          (tramp-rpc-test--with-call-count 7
            (copy-directory src (file-name-as-directory dest-parent)))
          (should (file-exists-p (expand-file-name "payload.txt" expected-dest)))
          (should (equal "payload"
                         (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "payload.txt" expected-dest))
                           (buffer-string)))))
      (ignore-errors (delete-directory src t))
      (ignore-errors (delete-directory dest-parent t)))))

(ert-deftest tramp-rpc-test06-copy-directory-copy-contents-roundtrips ()
  "Same-remote `copy-directory' with COPY-CONTENTS is constant time."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((src (tramp-rpc-test--make-temp-name))
         (nested (expand-file-name "nested" src))
         (dest (tramp-rpc-test--make-temp-name)))
    (unwind-protect
        (progn
          (make-directory nested t)
          (write-region "top" nil (expand-file-name "top.txt" src))
          (write-region "child" nil (expand-file-name "child.txt" nested))
          (make-directory dest)
          (tramp-rpc-test--with-call-count 7
            (copy-directory src (file-name-as-directory dest) nil nil t))
          (should (file-exists-p (expand-file-name "top.txt" dest)))
          (should (file-exists-p (expand-file-name "nested/child.txt" dest)))
          (should-not (file-exists-p
                       (expand-file-name (file-name-nondirectory src) dest))))
      (ignore-errors (delete-directory src t))
      (ignore-errors (delete-directory dest t)))))

(ert-deftest tramp-rpc-test06-copy-directory-keep-date-roundtrips ()
  "Same-remote `copy-directory' with KEEP-DATE is constant time."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((src (tramp-rpc-test--make-temp-name))
         (nested (expand-file-name "nested" src))
         (file (expand-file-name "payload.txt" nested))
         (dest (tramp-rpc-test--make-temp-name))
         (src-time (seconds-to-time 1600000101))
         (nested-time (seconds-to-time 1600000102))
         (file-time (seconds-to-time 1600000103)))
    (unwind-protect
        (progn
          (make-directory nested t)
          (write-region "payload" nil file)
          ;; Set parent directories after creating children so their mtimes are
          ;; stable before copying.
          (set-file-times file file-time)
          (set-file-times nested nested-time)
          (set-file-times src src-time)
          (copy-directory src dest t)
          (should (= (truncate (float-time src-time))
                     (truncate
                      (float-time
                       (file-attribute-modification-time
                        (file-attributes dest))))))
          (should (= (truncate (float-time nested-time))
                     (truncate
                      (float-time
                       (file-attribute-modification-time
                        (file-attributes (expand-file-name "nested" dest)))))))
          (should (= (truncate (float-time file-time))
                     (truncate
                      (float-time
                       (file-attribute-modification-time
                        (file-attributes
                         (expand-file-name "nested/payload.txt" dest))))))))
      (ignore-errors (delete-directory src t))
      (ignore-errors (delete-directory dest t)))))

(ert-deftest tramp-rpc-test06-copy-directory-merges-existing-target-dir ()
  "Same-remote `copy-directory' merges into an existing DIRECTORY basename."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((src (tramp-rpc-test--make-temp-name))
         (dest-parent (tramp-rpc-test--make-temp-name))
         (expected-dest (expand-file-name (file-name-nondirectory src) dest-parent)))
    (unwind-protect
        (progn
          (make-directory src)
          (write-region "new" nil (expand-file-name "payload.txt" src))
          (make-directory expected-dest t)
          (write-region "old" nil (expand-file-name "payload.txt" expected-dest))
          (tramp-rpc-test--with-call-count 7
            (copy-directory src (file-name-as-directory dest-parent)))
          (should-not (file-exists-p (expand-file-name
                                      (file-name-nondirectory src)
                                      expected-dest)))
          (should (equal "new"
                         (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "payload.txt" expected-dest))
                           (buffer-string)))))
      (ignore-errors (delete-directory src t))
      (ignore-errors (delete-directory dest-parent t)))))

(ert-deftest tramp-rpc-test06-copy-directory-rejects-existing-child-dir ()
  "With nil PARENTS, existing nested directories match Emacs semantics."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((src (tramp-rpc-test--make-temp-name))
         (dest-parent (tramp-rpc-test--make-temp-name))
         (expected-dest (expand-file-name (file-name-nondirectory src) dest-parent))
         (existing-child (expand-file-name "child" expected-dest)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "child" src) t)
          (write-region "new" nil (expand-file-name "child/new.txt" src))
          (make-directory existing-child t)
          (write-region "old" nil (expand-file-name "old.txt" existing-child))
          (should-error
           (copy-directory src (file-name-as-directory dest-parent))
           :type 'file-already-exists)
          (should (equal "old"
                         (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "old.txt" existing-child))
                           (buffer-string))))
          (should-not (file-exists-p (expand-file-name "new.txt" existing-child))))
      (ignore-errors (delete-directory src t))
      (ignore-errors (delete-directory dest-parent t)))))

(ert-deftest tramp-rpc-test06-copy-directory-parents-merges-existing-child-dir ()
  "With non-nil PARENTS, existing nested directories are accepted."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((src (tramp-rpc-test--make-temp-name))
         (dest-parent (tramp-rpc-test--make-temp-name))
         (expected-dest (expand-file-name (file-name-nondirectory src) dest-parent))
         (existing-child (expand-file-name "child" expected-dest)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "child" src) t)
          (write-region "new" nil (expand-file-name "child/new.txt" src))
          (make-directory existing-child t)
          (write-region "old" nil (expand-file-name "old.txt" existing-child))
          (copy-directory src (file-name-as-directory dest-parent) nil t)
          (should (file-exists-p (expand-file-name "old.txt" existing-child)))
          (should (equal "new"
                         (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "new.txt" existing-child))
                           (buffer-string)))))
      (ignore-errors (delete-directory src t))
      (ignore-errors (delete-directory dest-parent t)))))

(ert-deftest tramp-rpc-test06-copy-directory-parents-allows-existing-dir-newname ()
  "A non-directory-name NEWNAME may be an existing directory when PARENTS is t."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((src (tramp-rpc-test--make-temp-name))
         (dest (tramp-rpc-test--make-temp-name)))
    (unwind-protect
        (progn
          (make-directory src)
          (write-region "payload" nil (expand-file-name "payload.txt" src))
          (make-directory dest)
          (copy-directory src dest nil t)
          (should (equal "payload"
                         (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "payload.txt" dest))
                           (buffer-string)))))
      (ignore-errors (delete-directory src t))
      (ignore-errors (delete-directory dest t)))))

(ert-deftest tramp-rpc-test06-copy-directory-preserves-nonwritable-modes ()
  "Mode preservation happens after child entries are copied."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((src (tramp-rpc-test--make-temp-name))
         (child (expand-file-name "child" src))
         (dest (tramp-rpc-test--make-temp-name))
         (copied-child (expand-file-name "child" dest)))
    (unwind-protect
        (progn
          (make-directory child t)
          (write-region "payload" nil (expand-file-name "payload.txt" child))
          (set-file-modes child #o555)
          (set-file-modes src #o555)
          (copy-directory src dest)
          (should (equal "payload"
                         (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "payload.txt" copied-child))
                           (buffer-string))))
          (should (= (logand (file-modes dest) #o777) #o555))
          (should (= (logand (file-modes copied-child) #o777) #o555)))
      (ignore-errors (set-file-modes child #o755))
      (ignore-errors (set-file-modes src #o755))
      (ignore-errors (set-file-modes copied-child #o755))
      (ignore-errors (set-file-modes dest #o755))
      (ignore-errors (delete-directory src t))
      (ignore-errors (delete-directory dest t)))))

(ert-deftest tramp-rpc-test06-copy-directory-merges-through-symlinked-target-dir ()
  "Existing DIRECTORY basename may be a symlink to a directory."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((src (tramp-rpc-test--make-temp-name))
         (dest-parent (tramp-rpc-test--make-temp-name))
         (real-target (tramp-rpc-test--make-temp-name))
         (expected-link (expand-file-name (file-name-nondirectory src) dest-parent)))
    (unwind-protect
        (progn
          (make-directory src)
          (write-region "payload" nil (expand-file-name "payload.txt" src))
          (make-directory dest-parent)
          (make-directory real-target)
          (condition-case nil
              (make-symbolic-link real-target expected-link)
            (file-error (ert-skip "Symlinks not supported")))
          (tramp-rpc-test--with-call-count 7
            (copy-directory src (file-name-as-directory dest-parent)))
          (should (file-symlink-p expected-link))
          (should (equal "payload"
                         (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "payload.txt" real-target))
                           (buffer-string)))))
      (ignore-errors (delete-file expected-link))
      (ignore-errors (delete-directory src t))
      (ignore-errors (delete-directory dest-parent t))
      (ignore-errors (delete-directory real-target t)))))

(ert-deftest tramp-rpc-test06-copy-directory-follows-symlinked-newname-parent ()
  "A symlink to a directory may be the NEWNAME parent."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((src (tramp-rpc-test--make-temp-name))
         (link-parent (tramp-rpc-test--make-temp-name))
         (real-parent (tramp-rpc-test--make-temp-name))
         (expected-dest (expand-file-name (file-name-nondirectory src) real-parent)))
    (unwind-protect
        (progn
          (make-directory src)
          (write-region "payload" nil (expand-file-name "payload.txt" src))
          (make-directory real-parent)
          (condition-case nil
              (make-symbolic-link real-parent link-parent)
            (file-error (ert-skip "Symlinks not supported")))
          (copy-directory src (file-name-as-directory link-parent))
          (should (equal "payload"
                         (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "payload.txt" expected-dest))
                           (buffer-string)))))
      (ignore-errors (delete-file link-parent))
      (ignore-errors (delete-directory src t))
      (ignore-errors (delete-directory real-parent t)))))

(ert-deftest tramp-rpc-test06-copy-directory-follows-symlinked-newname-parent-file ()
  "A symlink to a directory may be the parent of a non-directory-name NEWNAME."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((src (tramp-rpc-test--make-temp-name))
         (link-parent (tramp-rpc-test--make-temp-name))
         (real-parent (tramp-rpc-test--make-temp-name))
         (expected-dest (expand-file-name "copied" real-parent)))
    (unwind-protect
        (progn
          (make-directory src)
          (write-region "payload" nil (expand-file-name "payload.txt" src))
          (make-directory real-parent)
          (condition-case nil
              (make-symbolic-link real-parent link-parent)
            (file-error (ert-skip "Symlinks not supported")))
          (copy-directory src (expand-file-name "copied" link-parent))
          (should (equal "payload"
                         (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "payload.txt" expected-dest))
                           (buffer-string)))))
      (ignore-errors (delete-file link-parent))
      (ignore-errors (delete-directory src t))
      (ignore-errors (delete-directory real-parent t)))))

(ert-deftest tramp-rpc-test06-copy-directory-create-symlink-overwrites-file ()
  "`copy-directory-create-symlink' follows Emacs overwrite behavior."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((target (tramp-rpc-test--make-temp-name))
         (src-link (tramp-rpc-test--make-temp-name))
         (dest (tramp-rpc-test--make-temp-name))
         (copy-directory-create-symlink t))
    (unwind-protect
        (progn
          (make-directory target)
          (condition-case nil
              (make-symbolic-link target src-link)
            (file-error (ert-skip "Symlinks not supported")))
          (write-region "old" nil dest)
          (copy-directory src-link dest)
          (should (equal (file-symlink-p dest) (file-local-name target))))
      (ignore-errors (delete-file src-link))
      (ignore-errors (delete-file dest))
      (ignore-errors (delete-directory target t)))))

(ert-deftest tramp-rpc-test06-copy-directory-create-symlink-dangling-source ()
  "`copy-directory-create-symlink' must not follow SOURCE's target."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((src-link (tramp-rpc-test--make-temp-name))
         (dest (tramp-rpc-test--make-temp-name))
         (target "missing-target")
         (copy-directory-create-symlink t))
    (unwind-protect
        (progn
          (condition-case nil
              (make-symbolic-link target src-link)
            (file-error (ert-skip "Symlinks not supported")))
          (copy-directory src-link dest)
          (should (equal (file-symlink-p dest) target)))
      (ignore-errors (delete-file src-link))
      (ignore-errors (delete-file dest)))))

(ert-deftest tramp-rpc-test06-copy-directory-create-symlink-ignores-copy-contents ()
  "Source symlink copies into DIRECTORY even when COPY-CONTENTS is non-nil."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((src-link (tramp-rpc-test--make-temp-name))
         (dest-dir (tramp-rpc-test--make-temp-name))
         (expected (expand-file-name (file-name-nondirectory src-link) dest-dir))
         (target "missing-target")
         (copy-directory-create-symlink t))
    (unwind-protect
        (progn
          (condition-case nil
              (make-symbolic-link target src-link)
            (file-error (ert-skip "Symlinks not supported")))
          (make-directory dest-dir)
          (copy-directory src-link (file-name-as-directory dest-dir) nil nil t)
          (should (equal (file-symlink-p expected) target)))
      (ignore-errors (delete-file src-link))
      (ignore-errors (delete-file expected))
      (ignore-errors (delete-directory dest-dir t)))))

(ert-deftest tramp-rpc-test06-rename-file ()
  "Test `rename-file' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((src (tramp-rpc-test--make-temp-name))
        (dest (tramp-rpc-test--make-temp-name))
        (content "rename test content"))
    (unwind-protect
        (progn
          (write-region content nil src)
          (tramp-rpc-test--with-call-count 2
            (rename-file src dest))
          (should-not (file-exists-p src))
          (should (file-exists-p dest))
          (should (equal content (with-temp-buffer
                                   (insert-file-contents dest)
                                   (buffer-string)))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file dest)))))

(ert-deftest tramp-rpc-test06-rename-dangling-symlink-no-overwrite ()
  "No-overwrite rename rejects a dangling symlink destination."
  (skip-unless (tramp-rpc-test-enabled))
  (let ((src (tramp-rpc-test--make-temp-name))
        (dest (tramp-rpc-test--make-temp-name)))
    (unwind-protect
        (progn
          (write-region "source" nil src)
          (make-symbolic-link "missing-target" dest)
          (should-error (rename-file src dest)
                        :type 'file-already-exists)
          (should (file-symlink-p dest))
          (should (file-exists-p src)))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file dest)))))

(ert-deftest tramp-rpc-test06-delete-file ()
  "Test `delete-file' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((file (tramp-rpc-test--make-temp-name)))
    (write-region "to be deleted" nil file)
    (should (file-exists-p file))
    (tramp-rpc-test--with-call-count 2
      (delete-file file))
    (should-not (file-exists-p file))))

(ert-deftest tramp-rpc-test06-copy-file-cross-remote ()
  "Test `copy-file' between two different RPC hosts.
This exercises the via-buffer code path that is used when source
and destination are on different remote hosts."
  :tags '(:expensive-test)
  (skip-unless (tramp-rpc-test-cross-remote-enabled))

  (tramp-rpc-test--with-temp-file src "cross-remote copy content"
    (let ((dest (tramp-rpc-test--make-temp-name-2)))
      (unwind-protect
          (progn
            ;; Verify source and dest are on different remotes
            (should-not (tramp-equal-remote src dest))
            (copy-file src dest)
            (should (file-exists-p dest))
            (should (equal (with-temp-buffer
                             (insert-file-contents src)
                             (buffer-string))
                           (with-temp-buffer
                             (insert-file-contents dest)
                             (buffer-string)))))
        (ignore-errors (delete-file dest))))))

(ert-deftest tramp-rpc-test06-copy-file-cross-remote-to-directory ()
  "Test `copy-file' between different RPC hosts into a directory."
  :tags '(:expensive-test)
  (skip-unless (tramp-rpc-test-cross-remote-enabled))

  (tramp-rpc-test--with-temp-file src "cross-remote dir copy content"
    (let ((dest-dir (tramp-rpc-test--make-temp-name-2)))
      (unwind-protect
          (progn
            (make-directory dest-dir)
            (should (file-directory-p dest-dir))
            ;; Copy into directory with trailing /
            (copy-file src (file-name-as-directory dest-dir))
            (let ((expected-dest (expand-file-name
                                  (file-name-nondirectory src) dest-dir)))
              (should (file-exists-p expected-dest))
              (should (equal (with-temp-buffer
                               (insert-file-contents src)
                               (buffer-string))
                             (with-temp-buffer
                               (insert-file-contents expected-dest)
                               (buffer-string))))))
        (ignore-errors (delete-directory dest-dir t))))))

(ert-deftest tramp-rpc-test06-rename-file-cross-remote ()
  "Test `rename-file' between two different RPC hosts.
This exercises copy-then-delete for cross-remote renames."
  :tags '(:expensive-test)
  (skip-unless (tramp-rpc-test-cross-remote-enabled))

  (let ((src (tramp-rpc-test--make-temp-name))
        (dest (tramp-rpc-test--make-temp-name-2))
        (content "cross-remote rename content"))
    (unwind-protect
        (progn
          (write-region content nil src)
          ;; Verify source and dest are on different remotes
          (should-not (tramp-equal-remote src dest))
          (rename-file src dest)
          (should-not (file-exists-p src))
          (should (file-exists-p dest))
          (should (equal content (with-temp-buffer
                                   (insert-file-contents dest)
                                   (buffer-string)))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file dest)))))

;;; ============================================================================
;;; Test 07: Directory Operations
;;; ============================================================================

(ert-deftest tramp-rpc-test07-make-directory ()
  "Test `make-directory' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((dir (tramp-rpc-test--make-temp-name)))
    (unwind-protect
        (progn
          (tramp-rpc-test--with-call-count 1
            (make-directory dir))
          (should (file-directory-p dir)))
      (ignore-errors (delete-directory dir)))))

(ert-deftest tramp-rpc-test07-make-directory-parents ()
  "Test `make-directory' with parents for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((dir (concat (tramp-rpc-test--make-temp-name) "/nested/path")))
    (unwind-protect
        (progn
          (tramp-rpc-test--with-call-count 1
            (make-directory dir t))
          (should (file-directory-p dir)))
      (ignore-errors (delete-directory
                      (file-name-directory (directory-file-name dir)) t)))))

(ert-deftest tramp-rpc-test07-delete-directory ()
  "Test `delete-directory' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((dir (tramp-rpc-test--make-temp-name)))
    (make-directory dir)
    (should (file-directory-p dir))
    (tramp-rpc-test--with-call-count 1
      (delete-directory dir))
    (should-not (file-exists-p dir))))

(ert-deftest tramp-rpc-test07-delete-directory-recursive ()
  "Test recursive `delete-directory' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((dir (tramp-rpc-test--make-temp-name)))
    (make-directory dir)
    (write-region "file1" nil (concat dir "/file1.txt"))
    (make-directory (concat dir "/subdir"))
    (write-region "file2" nil (concat dir "/subdir/file2.txt"))
    (should (file-directory-p dir))
    (tramp-rpc-test--with-call-count 1
      (delete-directory dir t))
    (should-not (file-exists-p dir))))

(ert-deftest tramp-rpc-test07-directory-files ()
  "Test `directory-files' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-dir dir
    (write-region "a" nil (concat dir "/file1.txt"))
    (write-region "b" nil (concat dir "/file2.txt"))
    (write-region "c" nil (concat dir "/other.log"))

    (let ((files (tramp-rpc-test--with-call-count 1
                  (directory-files dir))))
      ;; Should contain . and .. plus our files
      (should (member "." files))
      (should (member ".." files))
      (should (member "file1.txt" files))
      (should (member "file2.txt" files))
      (should (member "other.log" files)))

    ;; Test with pattern
    (let ((txt-files (directory-files dir nil "\\.txt$")))
      (should (= (length txt-files) 2))
      (should (member "file1.txt" txt-files))
      (should (member "file2.txt" txt-files)))))

(ert-deftest tramp-rpc-test07-directory-files-count ()
  "Directory COUNT accepts natural numbers and rejects other values."
  (skip-unless (tramp-rpc-test-enabled))
  (tramp-rpc-test--with-temp-dir dir
    (dolist (name '("a" "b" "c"))
      (write-region name nil (expand-file-name name dir)))
    (let ((all (directory-files dir)))
      (should (equal (directory-files dir nil nil nil 2) (seq-take all 2)))
      (should-not (directory-files dir nil nil nil 0))
      (dolist (count '(-1 "invalid"))
        (should-error (directory-files dir nil nil nil count)
                      :type 'wrong-type-argument)))))

(ert-deftest tramp-rpc-test07-directory-files-and-attributes-count ()
  "Attribute directory listings implement the same COUNT contract."
  (skip-unless (tramp-rpc-test-enabled))
  (tramp-rpc-test--with-temp-dir dir
    (dolist (name '("a" "b" "c"))
      (write-region name nil (expand-file-name name dir)))
    (let ((all (directory-files-and-attributes dir)))
      (should (equal (directory-files-and-attributes dir nil nil nil nil 2)
                     (seq-take all 2)))
      (should-not (directory-files-and-attributes dir nil nil nil nil 0))
      (dolist (count '(-1 "invalid"))
        (should-error (directory-files-and-attributes dir nil nil nil nil count)
                      :type 'wrong-type-argument)))))

(ert-deftest tramp-rpc-test07-directory-files-and-attributes ()
  "Test `directory-files-and-attributes' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-dir dir
    (write-region "content" nil (concat dir "/file.txt"))
    (make-directory (concat dir "/subdir"))

    (let ((entries (tramp-rpc-test--with-call-count 1
                    (directory-files-and-attributes dir))))
      ;; Find file entry
      (let ((file-entry (assoc "file.txt" entries)))
        (should file-entry)
        (should (null (file-attribute-type (cdr file-entry)))))  ; regular file

      ;; Find directory entry
      (let ((dir-entry (assoc "subdir" entries)))
        (should dir-entry)
        (should (eq (file-attribute-type (cdr dir-entry)) t))))))  ; directory

;;; ============================================================================
;;; Test 08: Symbolic Links
;;; ============================================================================

(ert-deftest tramp-rpc-test08-make-symbolic-link ()
  "Test `make-symbolic-link' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-file target "target content"
    (let ((link (tramp-rpc-test--make-temp-name)))
      (unwind-protect
          (progn
            (condition-case nil
                (make-symbolic-link target link)
              (file-error (ert-skip "Symlinks not supported")))
            (should (file-symlink-p link))
            (should (equal (with-temp-buffer
                             (insert-file-contents link)
                             (buffer-string))
                           "target content")))
        (ignore-errors (delete-file link))))))

(ert-deftest tramp-rpc-test08-file-truename ()
  "Test `file-truename' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-file target "content"
    (let ((link (tramp-rpc-test--make-temp-name)))
      (unwind-protect
          (progn
            (condition-case nil
                (make-symbolic-link target link)
              (file-error (ert-skip "Symlinks not supported")))
            ;; file-truename should resolve the symlink
            (let ((true-name (file-truename link)))
              (should (string-match-p (regexp-quote (file-local-name target))
                                      (file-local-name true-name)))))
        (ignore-errors (delete-file link))))))

(ert-deftest tramp-rpc-test08b-file-truename-nonexisting ()
  "Test `file-truename' for non-existing TRAMP RPC files.
This tests the fix for GitHub issue #37: file-truename should return
the filename unchanged for non-existing files, not signal an error."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((remote-dir (tramp-rpc-test--remote-directory))
         (nonexisting (expand-file-name "nonexisting-file-for-truename-test" remote-dir)))
    ;; Ensure the file doesn't exist
    (should-not (file-exists-p nonexisting))
    ;; file-truename should return the path unchanged (not error)
    (let ((truename (file-truename nonexisting)))
      (should (stringp truename))
      ;; The truename should contain the same localname
      (should (string-match-p (regexp-quote "nonexisting-file-for-truename-test")
                              truename))
      ;; It should still be a tramp path
      (should (tramp-tramp-file-p truename)))))

(ert-deftest tramp-rpc-test08c-find-file-nonexisting ()
  "Test `find-file' for non-existing TRAMP RPC files.
This tests creating new files via find-file, which was broken in issue #37."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((remote-dir (tramp-rpc-test--remote-directory))
         (newfile (expand-file-name
                   (format "new-file-%s" (format-time-string "%s%N"))
                   remote-dir)))
    (unwind-protect
        (progn
          ;; Ensure the file doesn't exist
          (should-not (file-exists-p newfile))
          ;; find-file-noselect should work without error
          (let ((buf (find-file-noselect newfile)))
            (should (bufferp buf))
            (unwind-protect
                (with-current-buffer buf
                  ;; Buffer should be set up correctly
                  (should (equal buffer-file-name newfile))
                  (should (stringp buffer-file-truename))
                  ;; Write some content and save
                  (insert "test content for new file")
                  (save-buffer)
                  ;; File should now exist
                  (should (file-exists-p newfile))
                  ;; Content should be correct
                  (should (equal (with-temp-buffer
                                   (insert-file-contents newfile)
                                   (buffer-string))
                                 "test content for new file")))
              (kill-buffer buf))))
      ;; Cleanup
      (ignore-errors (delete-file newfile)))))

(ert-deftest tramp-rpc-test08d-file-truename-nonexisting-in-symlink-dir ()
  "Test `file-truename' for non-existing file in a symlinked directory.
Note: Unlike SSH tramp (which uses `readlink --canonicalize-missing'),
tramp-rpc currently does not resolve symlinks in parent directories
for non-existing files.  This test verifies the function doesn't error
and returns a valid path."
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((remote-dir (tramp-rpc-test--remote-directory))
         (real-subdir (expand-file-name "real-subdir-for-truename" remote-dir))
         (link-subdir (expand-file-name "link-subdir-for-truename" remote-dir))
         (nonexisting-via-link (expand-file-name "nonexisting.txt" link-subdir)))
    (unwind-protect
        (progn
          ;; Create a real directory
          (make-directory real-subdir t)
          ;; Create a symlink to it
          (condition-case nil
              (make-symbolic-link real-subdir link-subdir)
            (file-error (ert-skip "Symlinks not supported")))
          ;; file-truename on non-existing file via symlink should not error
          (let ((truename (file-truename nonexisting-via-link)))
            (should (stringp truename))
            ;; It should be a valid tramp path containing the filename
            (should (tramp-tramp-file-p truename))
            (should (string-match-p "nonexisting\\.txt" truename))))
      ;; Cleanup
      (ignore-errors (delete-file link-subdir))
      (ignore-errors (delete-directory real-subdir t)))))

;;; ============================================================================
;;; Test 09: File Times
;;; ============================================================================

(ert-deftest tramp-rpc-test09-set-file-times ()
  "Test `set-file-times' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-file tmp "content"
    (let ((new-time (encode-time 0 0 12 1 1 2020)))
      (tramp-rpc-test--with-call-count 1
        (set-file-times tmp new-time))
      (let ((mtime (file-attribute-modification-time (file-attributes tmp))))
        ;; Check the time was set (allow some tolerance)
        (should (< (abs (- (float-time new-time) (float-time mtime))) 2))))))

(ert-deftest tramp-rpc-test09-set-file-times-missing-file ()
  "`set-file-times' reports a missing file without a preflight stat."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((missing (tramp-rpc-test--make-temp-name)))
    (tramp-rpc-test--with-call-count-error 1 file-missing
      (set-file-times missing (current-time)))))

;;; ============================================================================
;;; Test 10: Process Execution
;;; ============================================================================

(ert-deftest tramp-rpc-test10-process-file ()
  "Test `process-file' for TRAMP RPC files."
  :tags '(:process)
  (skip-unless (tramp-rpc-test-enabled))

  (let ((default-directory (tramp-rpc-test--remote-directory)))
    ;; Simple command
    (should (= 0 (process-file "true")))
    (should (= 1 (process-file "false")))

    ;; Command with output
    (with-temp-buffer
      (process-file "echo" nil t nil "hello")
      (should (string-match-p "hello" (buffer-string))))

    ;; Command with arguments
    (with-temp-buffer
      (process-file "ls" nil t nil "-la")
      (should (> (buffer-size) 0)))))

(ert-deftest tramp-rpc-test10-process-file-signal-exit ()
  "Test `process-file' returns 128+signal for signal-killed processes.
This matches the upstream `tramp-test28-process-file' test."
  :tags '(:process)
  (skip-unless (tramp-rpc-test-enabled))

  (let ((default-directory (tramp-rpc-test--remote-directory)))
    ;; Normal exit code
    (should (= 42 (process-file "/bin/sh" nil nil nil "-c" "exit 42")))

    ;; Return exit code in case the process is interrupted.
    ;; Signal 2 (SIGINT) -> exit code 130
    (let (process-file-return-signal-string)
      (should (= (+ 128 2)
                 (process-file "/bin/sh" nil nil nil "-c" "kill -2 $$"))))

    ;; Return string in case the process is interrupted and
    ;; process-file-return-signal-string is set.
    (when (boundp 'process-file-return-signal-string)
      (let ((process-file-return-signal-string t))
        (should (stringp
                 (process-file "/bin/sh" nil nil nil "-c" "kill -2 $$")))))))

(ert-deftest tramp-rpc-test10-process-file-with-stdin ()
  "Test `process-file' with stdin input."
  :tags '(:process)
  (skip-unless (tramp-rpc-test-enabled))

  (let ((default-directory (tramp-rpc-test--remote-directory)))
    ;; Create a file with input
    (tramp-rpc-test--with-temp-file input-file "hello world"
      (with-temp-buffer
        (let ((local-input (tramp-rpc-test--make-temp-name t)))
          (unwind-protect
              (progn
                ;; Copy remote file to local for stdin
                (copy-file input-file local-input t)
                (process-file "cat" local-input t nil)
                (should (string-match-p "hello world" (buffer-string))))
            (ignore-errors (delete-file local-input))))))))

(ert-deftest tramp-rpc-test10-shell-command ()
  "Test shell command execution."
  :tags '(:process)
  (skip-unless (tramp-rpc-test-enabled))

  (let ((default-directory (tramp-rpc-test--remote-directory)))
    (with-temp-buffer
      (shell-command "echo 'test output'" (current-buffer))
      (should (string-match-p "test output" (buffer-string))))))

;;; ============================================================================
;;; Test 11: Expand File Name
;;; ============================================================================

(ert-deftest tramp-rpc-test11-expand-file-name ()
  "Test `expand-file-name' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((dir (tramp-rpc-test--remote-directory)))
    ;; Absolute local path stays local - this is standard Emacs behavior.
    ;; The handler is not called for absolute paths.
    (should (equal (expand-file-name "/absolute/path" dir) "/absolute/path"))

    ;; Relative path is resolved against remote directory
    (let ((expanded (expand-file-name "relative" dir)))
      (should (string-match-p "relative" expanded))
      (should (tramp-tramp-file-p expanded)))

    ;; . and .. resolution
    (let ((expanded (expand-file-name "./file" dir)))
      (should (string-match-p "file" expanded))
      (should (tramp-tramp-file-p expanded)))

    (let ((expanded (expand-file-name "a/../b" dir)))
      (should (string-match-p "/b" expanded))
      (should-not (string-match-p "/a/" expanded))
      (should (tramp-tramp-file-p expanded)))

    ;; Empty localname should expand to home directory, not root.
    ;; This tests the fix for the inconsistency where "/rpc:host:" would
    ;; resolve to "/" instead of "~" (GitHub issue #55).
    (let* ((user-part (if tramp-rpc-test-user
                          (concat tramp-rpc-test-user "@")
                        ""))
           (bare-remote (format "/rpc:%s%s:" user-part tramp-rpc-test-host))
           (expanded (expand-file-name bare-remote)))
      (should (tramp-tramp-file-p expanded))
      ;; The expanded path should NOT be root "/"
      (should-not (equal (tramp-file-local-name expanded) "/"))
      ;; It should be the home directory (same as expanding "~")
      (let ((home-expanded (expand-file-name (concat bare-remote "~"))))
        (should (equal expanded home-expanded))))))

;;; ============================================================================
;;; Test 12: File Name Completion
;;; ============================================================================

(ert-deftest tramp-rpc-test12-file-name-completion ()
  "Test file name completion for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-dir dir
    (write-region "" nil (concat dir "/file-aaa.txt"))
    (write-region "" nil (concat dir "/file-aab.txt"))
    (write-region "" nil (concat dir "/other.txt"))

    (let ((completions (tramp-rpc-test--with-call-count 2
                         (file-name-all-completions "file-" dir))))
      (should (member "file-aaa.txt" completions))
      (should (member "file-aab.txt" completions))
      (should-not (member "other.txt" completions)))))

(ert-deftest tramp-rpc-test12b-file-name-completion-symlink-directory ()
  "Ensure completion marks symlinks to directories with trailing slash."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-dir dir
    (let ((real-dir (concat dir "/real-dir"))
          (link-dir (concat dir "/link-dir")))
      (make-directory real-dir t)
      (make-symbolic-link "real-dir" link-dir)
      (let ((completions (tramp-rpc-test--with-call-count 2
                           (file-name-all-completions "" dir))))
        (should (member "real-dir/" completions))
        (should (member "link-dir/" completions))))))

;;; ============================================================================
;;; Test 13: File System Info
;;; ============================================================================

(ert-deftest tramp-rpc-test13-file-system-info ()
  "Test `file-system-info' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((info (file-system-info (tramp-rpc-test--remote-directory))))
    (when info  ; May not be supported on all systems
      (should (= (length info) 3))
      (should (integerp (nth 0 info)))  ; total
      (should (integerp (nth 1 info)))  ; free
      (should (integerp (nth 2 info)))  ; available
      (should (>= (nth 0 info) (nth 1 info)))
      (should (>= (nth 1 info) 0)))))

;;; ============================================================================
;;; Test 13b: Coding argument handling (unit test, no remote host needed)
;;; ============================================================================

(ert-deftest tramp-rpc-test13b-coding-args ()
  "Test coding helpers handle symbol and cons pair coding."
  (let ((default-dec (car default-process-coding-system))
        (default-enc (cdr default-process-coding-system)))
    ;; Same-sided cons pair is valid `make-process' input, but can be
    ;; represented as a symbol internally.
    (should (eq 'utf-8-unix
                (tramp-rpc--normalize-coding
                 '(utf-8-unix . utf-8-unix))))
    ;; Different sides must remain a cons pair.
    (should (equal '(undecided-unix . utf-8-unix)
                   (tramp-rpc--normalize-coding
                    '(undecided-unix . utf-8-unix))))
    ;; Symbol case: same value used for both decoding and encoding
    (should (equal '(utf-8-unix utf-8-unix)
                   (tramp-rpc--coding-args 'utf-8-unix)))
    ;; Cons pair case: (DECODING . ENCODING)
    (should (equal '(undecided-unix utf-8-unix)
                   (tramp-rpc--coding-args '(undecided-unix . utf-8-unix))))
    ;; Cons pair with nil decoding: nil replaced with default
    (should (equal (list default-dec 'utf-8-unix)
                   (tramp-rpc--coding-args '(nil . utf-8-unix))))
    ;; Cons pair with nil encoding: nil replaced with default
    (should (equal (list 'utf-8-unix default-enc)
                   (tramp-rpc--coding-args '(utf-8-unix . nil))))))

(ert-deftest tramp-rpc-test13c-make-process-coding-pair ()
  "Test `make-process' handler accepts same-sided cons pair :coding values."
  (let ((default-directory "/rpc:mock:/tmp/")
        (default-process-coding-system '(utf-8-unix . utf-8-unix))
        proc)
    (cl-letf (((symbol-function 'tramp-rpc--start-remote-process)
               (lambda (&rest _args) 12345))
              ((symbol-function 'tramp-rpc--start-async-read)
               (lambda (&rest _args) nil))
              ((symbol-function 'tramp-rpc--kill-remote-process)
               (lambda (&rest _args) nil))
              ((symbol-function 'tramp-rpc--remote-path-environment)
               (lambda (&rest _args) nil))
              ((symbol-function 'tramp-rpc--tramp-remote-process-environment)
               (lambda (&rest _args) nil))
              ((symbol-function 'tramp-rpc--get-direnv-environment)
               (lambda (&rest _args) nil))
              ((symbol-function 'tramp-rpc--caller-environment)
               (lambda (&rest _args) nil)))
      (unwind-protect
          (progn
            (setq proc (tramp-rpc-handle-make-process
                        :name "tramp-rpc-coding-pair-test"
                        :buffer nil
                        :command '("jj" "workspace" "root")
                        :connection-type 'pipe
                        :coding default-process-coding-system
                        :noquery t
                        :sentinel #'ignore))
            (should (processp proc))
            (should (equal (process-coding-system proc)
                           '(utf-8-unix . utf-8-unix))))
        (when (processp proc)
          (ignore-errors (delete-process proc)))))))

(ert-deftest tramp-rpc-test13d-async-read-rpc-error-exits-process ()
  "Test async read RPC errors terminate the local process state."
  (let ((proc (make-pipe-process
               :name "tramp-rpc-read-error-test"
               :noquery t))
        exit-code)
    (unwind-protect
        (progn
          (puthash proc '(:vec mock :pid 12345)
                   tramp-rpc--async-processes)
          (cl-letf (((symbol-function 'tramp-rpc--handle-process-exit)
                     (lambda (_proc code)
                       (setq exit-code code))))
            (tramp-rpc--handle-async-read-response
             proc '(:error (:code -32004 :message "read failed"))))
          (should (= exit-code -1)))
      (remhash proc tramp-rpc--async-processes)
      (ignore-errors (delete-process proc)))))

;;; ============================================================================
;;; Test 14: Async Processes
;;; ============================================================================

(ert-deftest tramp-rpc-test14-start-file-process ()
  "Test `start-file-process' for TRAMP RPC files."
  :tags '(:process :expensive-test)
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((default-directory (tramp-rpc-test--remote-directory))
         (output "")
         (proc (start-file-process "test-proc" nil "echo" "async-test")))
    (unwind-protect
        (progn
          (set-process-filter
           proc (lambda (_proc str) (setq output (concat output str))))
          ;; Wait for process to complete
          (with-timeout (10 (error "Process timeout"))
            (while (process-live-p proc)
              (accept-process-output proc 0.1)))
          (should (string-match-p "async-test" output)))
      (ignore-errors (delete-process proc)))))

(ert-deftest tramp-rpc-test14-make-process ()
  "Test `make-process' for TRAMP RPC files."
  :tags '(:process :expensive-test)
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((default-directory (tramp-rpc-test--remote-directory))
         (output "")
         (proc (make-process
                :name "test-make-proc"
                :command '("cat")
                :connection-type 'pipe
                :file-handler t
                :filter (lambda (_proc str) (setq output (concat output str))))))
    (unwind-protect
        (progn
          ;; Send input
          (process-send-string proc "test-input\n")
          (process-send-eof proc)
          ;; Wait for output
          (with-timeout (10 (error "Process timeout"))
            (while (process-live-p proc)
              (accept-process-output proc 0.1)))
          (should (string-match-p "test-input" output)))
      (ignore-errors (delete-process proc)))))

(ert-deftest tramp-rpc-test14-make-process-drains-output-after-exit ()
  "Test `make-process' continues reading after remote child exit."
  :tags '(:process :expensive-test)
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((default-directory (tramp-rpc-test--remote-directory))
         (output "")
         (delay-first-read t)
         (orig-call-async (symbol-function 'tramp-rpc--call-async))
         proc)
    (unwind-protect
        (cl-letf (((symbol-function 'tramp-rpc--call-async)
                   (lambda (vec method params callback)
                     (if (equal method "process.read")
                         (let ((params (cons '(max_bytes . 1) params)))
                           (if delay-first-read
                               (progn
                                 (setq delay-first-read nil)
                                 ;; Let the short-lived child exit before the
                                 ;; first bounded read.  The old server returned
                                 ;; exited with the first byte and lost the rest.
                                 (run-at-time 0.2 nil orig-call-async
                                              vec method params callback))
                             (funcall orig-call-async
                                      vec method params callback)))
                       (funcall orig-call-async
                                vec method params callback)))))
          (setq proc
                (make-process
                 :name "tramp-rpc-eof-race"
                 :buffer nil
                 :command '("/bin/sh" "-c" "printf firstsecond")
                 :connection-type 'pipe
                 :coding 'binary
                 :noquery t
                 :file-handler t
                 :filter (lambda (_proc string)
                           (setq output (concat output string)))))
          (with-timeout (15 (error "Process output drain timeout"))
            (while (process-live-p proc)
              (accept-process-output proc 0.1)))
          (should (= 0 (process-exit-status proc)))
          (should (equal output "firstsecond")))
      (when (processp proc)
        (ignore-errors (delete-process proc))))))

(ert-deftest tramp-rpc-test14-python-shell-make-comint ()
  "Test `python-shell-make-comint' on an existing TRAMP RPC connection."
  :tags '(:process :expensive-test)
  (skip-unless (tramp-rpc-test-enabled))
  (require 'python)

  (let* ((default-directory (tramp-rpc-test--remote-directory))
         (python-shell-completion-native-enable nil)
         (python-shell-font-lock-enable nil)
         (buf nil)
         proc)
    (let* ((probe-buffer (generate-new-buffer "*tramp-rpc-python-probe*"))
           (process-connection-type t)
           (probe (start-file-process "tramp-rpc-python-probe" probe-buffer
                                      "python3" "--version")))
      (unwind-protect
          (progn
            (with-timeout (10 (error "Python probe timeout"))
              (while (process-live-p probe)
                (accept-process-output probe 0.1)))
            (skip-unless (= 0 (process-exit-status probe))))
        (ignore-errors (delete-process probe))
        (kill-buffer probe-buffer)))
    ;; Ensure the RPC connection exists before python.el tries to adjust its
    ;; remote environment.  This used to hang because python.el sent shell
    ;; snippets to the RPC server process via `tramp-send-command'.
    (should (file-directory-p default-directory))
    (unwind-protect
        (with-timeout (15 (error "Python shell timeout"))
          (setq buf (python-shell-make-comint "python3 -i" "Python-Test"))
          (setq proc (get-buffer-process buf))
          (while (and (process-live-p proc)
                      (not (with-current-buffer buf
                             (save-excursion
                               (goto-char (point-min))
                               (search-forward ">>>" nil t)))))
            (accept-process-output proc 0.1))
          (should (with-current-buffer buf
                    (save-excursion
                      (goto-char (point-min))
                      (search-forward ">>>" nil t)))))
      (when (processp proc)
        (ignore-errors (delete-process proc)))
      (when (and (bufferp buf) (buffer-live-p buf))
        (kill-buffer buf)))))

;;; ============================================================================
;;; Test 15: Copy/Rename Between Local and Remote
;;; ============================================================================

(ert-deftest tramp-rpc-test15-copy-local-to-remote ()
  "Test copying from local to remote."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((local-file (make-temp-file "local-test"))
        (remote-file (tramp-rpc-test--make-temp-name))
        (content "local to remote content"))
    (unwind-protect
        (progn
          (write-region content nil local-file)
          (copy-file local-file remote-file)
          (should (file-exists-p remote-file))
          (should (equal content (with-temp-buffer
                                   (insert-file-contents remote-file)
                                   (buffer-string)))))
      (ignore-errors (delete-file local-file))
      (ignore-errors (delete-file remote-file)))))

(ert-deftest tramp-rpc-test15-copy-remote-to-local ()
  "Test copying from remote to local."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((local-file (make-temp-file "local-test"))
        (content "remote to local content"))
    (unwind-protect
        (tramp-rpc-test--with-temp-file remote-file content
          (copy-file remote-file local-file t)
          (should (equal content (with-temp-buffer
                                   (insert-file-contents local-file)
                                   (buffer-string)))))
      (ignore-errors (delete-file local-file)))))

;;; ============================================================================
;;; Test 16: VC Integration
;;; ============================================================================

(ert-deftest tramp-rpc-test16-vc-registered ()
  "Test VC registration detection."
  :tags '(:expensive-test)
  (skip-unless (tramp-rpc-test-enabled))

  ;; Create a git repo in temp dir
  (tramp-rpc-test--with-temp-dir dir
    (let ((default-directory dir))
      ;; Initialize git repo
      (when (= 0 (process-file "git" nil nil nil "init"))
        (write-region "test" nil (concat dir "/test.txt"))
        (process-file "git" nil nil nil "add" "test.txt")
        ;; Check if VC detects it
        (should (vc-registered (concat dir "/test.txt")))))))

;;; ============================================================================
;;; Test 17: Overwrite Handling
;;; ============================================================================

(ert-deftest tramp-rpc-test17-write-region-overwrite ()
  "Test overwriting existing file."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((file (tramp-rpc-test--make-temp-name)))
    (unwind-protect
        (progn
          (write-region "original" nil file)
          (should (equal "original" (with-temp-buffer
                                      (insert-file-contents file)
                                      (buffer-string))))
          (write-region "updated" nil file)
          (should (equal "updated" (with-temp-buffer
                                     (insert-file-contents file)
                                     (buffer-string)))))
      (ignore-errors (delete-file file)))))

;;; ============================================================================
;;; Test 18: make-process :stderr contract (lsp-mode compatibility)
;;; ============================================================================

(ert-deftest tramp-rpc-test18-make-process-stderr-buffer-has-process ()
  "Test that make-process with :stderr creates a process on the stderr buffer.
Native `make-process' with `:stderr BUFFER' creates a pipe process
associated with that buffer, so `(get-buffer-process BUFFER)' returns
a process object.  lsp-mode relies on this contract:

  (set-process-query-on-exit-flag (get-buffer-process stderr-buf) nil)

Without a stderr relay process, `get-buffer-process' returns nil and
`set-process-query-on-exit-flag' signals `wrong-type-argument processp nil'.
This is the root cause of the lsp-mode crash reported with tramp-rpc."
  :tags '(:process :expensive-test)
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((default-directory (tramp-rpc-test--remote-directory))
         (stderr-buf (generate-new-buffer "*test-stderr*"))
         (proc (make-process
                :name "test-stderr-relay"
                :command '("sh" "-c" "echo stdout-msg; echo stderr-msg >&2")
                :connection-type 'pipe
                :buffer (generate-new-buffer "*test-stdout*")
                :stderr stderr-buf
                :noquery t
                :file-handler t)))
    (unwind-protect
        (progn
          ;; KEY ASSERTION: get-buffer-process must return non-nil,
          ;; exactly matching native make-process contract
          (should (get-buffer-process stderr-buf))
          ;; This is the exact call lsp-mode makes that crashed:
          (should (progn
                    (set-process-query-on-exit-flag
                     (get-buffer-process stderr-buf) nil)
                    t))
          ;; Wait for the command to finish
          (with-timeout (10 (error "Process timeout"))
            (while (process-live-p proc)
              (accept-process-output proc 0.1)))
          ;; Verify stderr output was delivered to the stderr buffer
          (with-timeout (2 nil)
            (while (= 0 (buffer-size stderr-buf))
              (accept-process-output nil 0.1)))
          (with-current-buffer stderr-buf
            (should (string-match-p "stderr-msg" (buffer-string)))))
      (ignore-errors (delete-process proc))
      (ignore-errors (kill-buffer (process-buffer proc)))
      (ignore-errors
        (when-let* ((stderr-proc (get-buffer-process stderr-buf)))
          (delete-process stderr-proc)))
      (ignore-errors (kill-buffer stderr-buf)))))

(ert-deftest tramp-rpc-test18-make-process-stderr-string ()
  "Test that make-process with :stderr as a string also works."
  :tags '(:process :expensive-test)
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((default-directory (tramp-rpc-test--remote-directory))
         (stderr-name "*test-stderr-string*")
         (proc (make-process
                :name "test-stderr-string"
                :command '("sh" "-c" "echo stderr-out >&2; exit 0")
                :connection-type 'pipe
                :stderr stderr-name
                :noquery t
                :file-handler t)))
    (unwind-protect
        (progn
          ;; Buffer should have been created
          (should (get-buffer stderr-name))
          ;; And should have a process
          (should (get-buffer-process (get-buffer stderr-name)))
          ;; Wait for completion
          (with-timeout (10 (error "Process timeout"))
            (while (process-live-p proc)
              (accept-process-output proc 0.1))))
      (ignore-errors (delete-process proc))
      (ignore-errors
        (when-let* ((buf (get-buffer stderr-name)))
          (when-let* ((p (get-buffer-process buf)))
            (delete-process p))
          (kill-buffer buf))))))

(ert-deftest tramp-rpc-test18-make-process-no-stderr ()
  "Test that make-process without :stderr still works (no regression)."
  :tags '(:process :expensive-test)
  (skip-unless (tramp-rpc-test-enabled))

  (let* ((default-directory (tramp-rpc-test--remote-directory))
         (output "")
         (proc (make-process
                :name "test-no-stderr"
                :command '("echo" "hello")
                :connection-type 'pipe
                :filter (lambda (_proc str) (setq output (concat output str)))
                :noquery t
                :file-handler t)))
    (unwind-protect
        (progn
          (with-timeout (10 (error "Process timeout"))
            (while (process-live-p proc)
              (accept-process-output proc 0.1)))
          (should (string-match-p "hello" output)))
      (ignore-errors (delete-process proc)))))

;;; ============================================================================
;;; Test 19: Empty File Handling
;;; ============================================================================

(ert-deftest tramp-rpc-test19-empty-file ()
  "Test handling of empty files."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((file (tramp-rpc-test--make-temp-name)))
    (unwind-protect
        (progn
          (write-region "" nil file)
          (should (file-exists-p file))
          (should (= 0 (file-attribute-size (file-attributes file))))
          (should (equal "" (with-temp-buffer
                              (insert-file-contents file)
                              (buffer-string)))))
      (ignore-errors (delete-file file)))))

;;; ============================================================================
;;; Test 19: High-level operations parity
;;; ============================================================================

(ert-deftest tramp-rpc-test19-locate-dominating-file ()
  "Ensure optimized `locate-dominating-file' matches baseline behavior."
  (skip-unless (tramp-rpc-test-enabled))
  (tramp-rpc-test--with-temp-dir root
    (let* ((deep (concat root "/a/b/c/d"))
           (target (concat deep "/target.txt")))
      (make-directory deep t)
      (make-directory (concat root "/.git") t)
      (write-region "x" nil target)
      (let ((expected (tramp-run-real-handler #'locate-dominating-file
                                              (list target ".git")))
            (actual (tramp-rpc-test--with-call-count 1
                      (locate-dominating-file target ".git"))))
        (should (equal expected actual))))))

(ert-deftest tramp-rpc-test19-locate-dominating-file-stop-regexp ()
  "Ensure stop-dir regexp behavior matches baseline locate-dominating-file."
  (skip-unless (tramp-rpc-test-enabled))
  (tramp-rpc-test--with-temp-dir root
    (let* ((deep (concat root "/a/b/c/d"))
           (target (concat deep "/target.txt"))
           (stop-dir (file-name-as-directory (concat root "/a/b")))
           (locate-dominating-stop-dir-regexp (regexp-quote stop-dir)))
      (make-directory deep t)
      (make-directory (concat root "/.git") t)
      (write-region "x" nil target)
      (let ((expected (tramp-run-real-handler #'locate-dominating-file
                                              (list target ".git")))
            (actual (locate-dominating-file target ".git")))
        (should (equal expected actual))))))

(ert-deftest tramp-rpc-test19-dir-locals-all-files ()
  "Ensure optimized `dir-locals--all-files' matches baseline behavior."
  (skip-unless (tramp-rpc-test-enabled))
  (tramp-rpc-test--with-temp-dir root
    (let ((deep (concat root "/p/q/r")))
      (make-directory deep t)
      (write-region "((nil . ((fill-column . 90))))\n" nil (concat root "/" dir-locals-file))
      (write-region "((nil . ((tab-width . 8))))\n" nil
                    (concat root "/" (string-replace ".el" "-2.el" dir-locals-file)))
      (let ((expected (tramp-run-real-handler #'dir-locals--all-files (list deep)))
            (actual (tramp-rpc-test--with-call-count 1
                      (dir-locals--all-files deep))))
        (should (equal expected actual))))))

(ert-deftest tramp-rpc-test19-dir-locals-find-file ()
  "Ensure optimized `dir-locals-find-file' matches baseline behavior."
  (skip-unless (tramp-rpc-test-enabled))
  (tramp-rpc-test--with-temp-dir root
    (let* ((deep (concat root "/alpha/beta/gamma"))
           (missing (concat deep "/new-file.txt")))
      (make-directory deep t)
      (write-region "((nil . ((fill-column . 100))))\n" nil (concat root "/" dir-locals-file))
      (let ((dir-locals-directory-cache nil)
            (dir-locals-class-alist nil))
        (let ((expected (tramp-run-real-handler #'dir-locals-find-file (list missing))))
          (setq dir-locals-directory-cache nil
                dir-locals-class-alist nil)
          (let ((actual (tramp-rpc-test--with-call-count 1
                          (dir-locals-find-file missing))))
            (should (equal expected actual))))))))

;;; ============================================================================
;;; Test 20: PTY login shell command normalization
;;; ============================================================================

(ert-deftest tramp-rpc-test20-pty-shell-login-args ()
  "Interactive PTY login shells should be started as login shells."
  (cl-letf (((symbol-function 'tramp-rpc--get-remote-login-shell)
             (lambda (_vec) "/opt/bin/my-shell"))
            ((symbol-function 'tramp-get-method-parameter)
             (lambda (_vec parameter &optional default)
               (if (eq parameter 'tramp-remote-shell-login) '("-l") default))))
    (should (equal (tramp-rpc--maybe-login-shell-command nil '("/opt/bin/my-shell"))
                   '("/opt/bin/my-shell" "-l")))
    (should (equal (tramp-rpc--maybe-login-shell-command nil '("/opt/bin/my-shell" "-i"))
                   '("/opt/bin/my-shell" "-i" "-l")))
    ;; Real `M-x shell' bash case: --noediting is a long option, so -l must be
    ;; appended -- bash rejects `-l --noediting' with "--: invalid option".
    (should (equal (tramp-rpc--maybe-login-shell-command nil '("/opt/bin/my-shell" "--noediting" "-i"))
                   '("/opt/bin/my-shell" "--noediting" "-i" "-l")))
    (should (equal (tramp-rpc--maybe-login-shell-command nil '("/opt/bin/my-shell" "-l" "-i"))
                   '("/opt/bin/my-shell" "-l" "-i")))
    (should (equal (tramp-rpc--maybe-login-shell-command nil '("/opt/bin/my-shell" "/tmp/script.sh"))
                   '("/opt/bin/my-shell" "/tmp/script.sh")))
    (should (equal (tramp-rpc--maybe-login-shell-command nil '("/opt/bin/my-shell" "-c" "echo ok"))
                   '("/opt/bin/my-shell" "-c" "echo ok")))
    (should (equal (tramp-rpc--maybe-login-shell-command nil '("/bin/bash" "-i"))
                   '("/bin/bash" "-i")))
    (should (equal (tramp-rpc--maybe-login-shell-command nil '("python3" "-i"))
                   '("python3" "-i")))))

(ert-deftest tramp-rpc-test20-make-process-pty-normalizes-login-shell ()
  "`tramp-rpc-handle-make-process' sends normalized login command to PTY path."
  (let ((default-directory "/rpc:test-host:/tmp/")
        captured-command)
    (cl-letf (((symbol-function 'tramp-rpc--get-remote-login-shell)
               (lambda (_vec) "/bin/bash"))
              ((symbol-function 'tramp-get-method-parameter)
               (lambda (_vec parameter &optional default)
                 (if (eq parameter 'tramp-remote-shell-login) '("-l") default)))
              ((symbol-function 'tramp-rpc--remote-path-environment)
               (lambda (_vec) nil))
              ((symbol-function 'tramp-rpc--tramp-remote-process-environment)
               (lambda () nil))
              ((symbol-function 'tramp-rpc--get-direnv-environment)
               (lambda (&rest _args) nil))
              ((symbol-function 'tramp-rpc--caller-environment)
               (lambda () nil))
              ((symbol-function 'tramp-rpc--ensure-inside-emacs-env)
               (lambda (env) env))
              ((symbol-function 'tramp-rpc--make-pty-process)
               (lambda (_vec _name _buffer command &rest _args)
                 (setq captured-command command)
                 'tramp-rpc-test-pty-process)))
      (should (eq (tramp-rpc-handle-make-process
                   :name "tramp-rpc-test"
                   :buffer nil
                   :command '("/bin/bash" "-i")
                   :connection-type 'pty)
                  'tramp-rpc-test-pty-process))
      (should (equal captured-command '("/bin/bash" "-i" "-l")))
      (let ((process-connection-type t))
        (setq captured-command nil)
        (should (eq (tramp-rpc-handle-make-process
                     :name "tramp-rpc-test-dynamic"
                     :buffer nil
                     :command '("/bin/bash" "-i"))
                    'tramp-rpc-test-pty-process))
        (should (equal captured-command '("/bin/bash" "-i" "-l")))))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

;;;###autoload
(defun tramp-rpc-test-all ()
  "Run all TRAMP RPC tests."
  (interactive)
  (tramp-rpc-test--setup)
  (unwind-protect
      (ert-run-tests-interactively "^tramp-rpc-test")
    (tramp-rpc-test--cleanup)))

;;;###autoload
(defun tramp-rpc-test-quick ()
  "Run quick TRAMP RPC tests (skip expensive ones)."
  (interactive)
  (tramp-rpc-test--setup)
  (unwind-protect
      (ert-run-tests-interactively
       '(and (tag tramp-rpc-test) (not (tag :expensive-test))))
    (tramp-rpc-test--cleanup)))

(provide 'tramp-rpc-tests)
;;; tramp-rpc-tests.el ends here
