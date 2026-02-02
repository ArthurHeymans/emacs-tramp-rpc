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

;;; ============================================================================
;;; Test Configuration
;;; ============================================================================

(defvar tramp-rpc-test-host (or (getenv "TRAMP_RPC_TEST_HOST") "localhost")
  "Remote host for testing.
Set via TRAMP_RPC_TEST_HOST environment variable.")

(defvar tramp-rpc-test-user (getenv "TRAMP_RPC_TEST_USER")
  "User for remote host. If nil, uses default SSH user.")

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

(defun tramp-rpc-test--setup ()
  "Set up test environment."
  (let ((dir (tramp-rpc-test--remote-directory)))
    ;; Clean up any leftover test files
    (ignore-errors (delete-directory dir t))
    ;; Create fresh test directory
    (make-directory dir t)))

(defun tramp-rpc-test--cleanup ()
  "Clean up test environment."
  (let ((dir (tramp-rpc-test--remote-directory)))
    (ignore-errors (delete-directory dir t))
    (ignore-errors (tramp-cleanup-all-connections))))

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

(ert-deftest tramp-rpc-test00-availability ()
  "Test availability of TRAMP RPC functions."
  (skip-unless (tramp-rpc-test-enabled))
  (let ((dir (tramp-rpc-test--remote-directory)))
    (should (file-remote-p dir))
    (should (file-directory-p dir))
    (should (file-writable-p dir))))

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
  (let ((dir (tramp-rpc-test--remote-directory)))
    (should (file-exists-p dir)))

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

  ;; Existing file
  (tramp-rpc-test--with-temp-file tmp "test content"
    (should (file-writable-p tmp)))

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
  (should (file-directory-p (tramp-rpc-test--remote-directory)))

  ;; Test file
  (tramp-rpc-test--with-temp-file tmp "test content"
    (should-not (file-directory-p tmp)))

  ;; Test subdirectory
  (tramp-rpc-test--with-temp-dir subdir
    (should (file-directory-p subdir))))

(ert-deftest tramp-rpc-test03-file-regular-p ()
  "Test `file-regular-p' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  ;; Regular file
  (tramp-rpc-test--with-temp-file tmp "test content"
    (should (file-regular-p tmp)))

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
            (should (file-symlink-p link))
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
    (let ((attrs (file-attributes tmp)))
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
    (let ((attrs (file-attributes subdir)))
      (should attrs)
      ;; Check it's a directory
      (should (eq (file-attribute-type attrs) t)))))

(ert-deftest tramp-rpc-test04-file-modes ()
  "Test `file-modes' and `set-file-modes' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-file tmp "test content"
    (let ((orig-modes (file-modes tmp)))
      (should (integerp orig-modes))
      ;; Set new modes
      (set-file-modes tmp #o644)
      (should (= (logand (file-modes tmp) #o777) #o644))
      ;; Set executable
      (set-file-modes tmp #o755)
      (should (= (logand (file-modes tmp) #o777) #o755)))))

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
      (insert-file-contents tmp)
      (should (equal (buffer-string) "test content")))))

(ert-deftest tramp-rpc-test05-insert-file-contents-partial ()
  "Test partial `insert-file-contents' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-file tmp "0123456789"
    ;; Read bytes 2-5
    (with-temp-buffer
      (insert-file-contents tmp nil 2 6)
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
            (copy-file src dest)
            (should (file-exists-p dest))
            (should (equal (with-temp-buffer
                             (insert-file-contents src)
                             (buffer-string))
                           (with-temp-buffer
                             (insert-file-contents dest)
                             (buffer-string)))))
        (ignore-errors (delete-file dest))))))

(ert-deftest tramp-rpc-test06-rename-file ()
  "Test `rename-file' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((src (tramp-rpc-test--make-temp-name))
        (dest (tramp-rpc-test--make-temp-name))
        (content "rename test content"))
    (unwind-protect
        (progn
          (write-region content nil src)
          (rename-file src dest)
          (should-not (file-exists-p src))
          (should (file-exists-p dest))
          (should (equal content (with-temp-buffer
                                   (insert-file-contents dest)
                                   (buffer-string)))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file dest)))))

(ert-deftest tramp-rpc-test06-delete-file ()
  "Test `delete-file' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((file (tramp-rpc-test--make-temp-name)))
    (write-region "to be deleted" nil file)
    (should (file-exists-p file))
    (delete-file file)
    (should-not (file-exists-p file))))

;;; ============================================================================
;;; Test 07: Directory Operations
;;; ============================================================================

(ert-deftest tramp-rpc-test07-make-directory ()
  "Test `make-directory' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((dir (tramp-rpc-test--make-temp-name)))
    (unwind-protect
        (progn
          (make-directory dir)
          (should (file-directory-p dir)))
      (ignore-errors (delete-directory dir)))))

(ert-deftest tramp-rpc-test07-make-directory-parents ()
  "Test `make-directory' with parents for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((dir (concat (tramp-rpc-test--make-temp-name) "/nested/path")))
    (unwind-protect
        (progn
          (make-directory dir t)
          (should (file-directory-p dir)))
      (ignore-errors (delete-directory
                      (file-name-directory (directory-file-name dir)) t)))))

(ert-deftest tramp-rpc-test07-delete-directory ()
  "Test `delete-directory' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (let ((dir (tramp-rpc-test--make-temp-name)))
    (make-directory dir)
    (should (file-directory-p dir))
    (delete-directory dir)
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
    (delete-directory dir t)
    (should-not (file-exists-p dir))))

(ert-deftest tramp-rpc-test07-directory-files ()
  "Test `directory-files' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-dir dir
    (write-region "a" nil (concat dir "/file1.txt"))
    (write-region "b" nil (concat dir "/file2.txt"))
    (write-region "c" nil (concat dir "/other.log"))

    (let ((files (directory-files dir)))
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

(ert-deftest tramp-rpc-test07-directory-files-and-attributes ()
  "Test `directory-files-and-attributes' for TRAMP RPC files."
  (skip-unless (tramp-rpc-test-enabled))

  (tramp-rpc-test--with-temp-dir dir
    (write-region "content" nil (concat dir "/file.txt"))
    (make-directory (concat dir "/subdir"))

    (let ((entries (directory-files-and-attributes dir)))
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
      (set-file-times tmp new-time)
      (let ((mtime (file-attribute-modification-time (file-attributes tmp))))
        ;; Check the time was set (allow some tolerance)
        (should (< (abs (- (float-time new-time) (float-time mtime))) 2))))))

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
      (should (tramp-tramp-file-p expanded)))))

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

    (let ((completions (file-name-all-completions "file-" dir)))
      (should (member "file-aaa.txt" completions))
      (should (member "file-aab.txt" completions))
      (should-not (member "other.txt" completions)))))

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
;;; Test 18: Empty File Handling
;;; ============================================================================

(ert-deftest tramp-rpc-test18-empty-file ()
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
