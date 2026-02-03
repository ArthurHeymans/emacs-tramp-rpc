;;; tramp-rpc-autoload-tests.el --- Tests for autoload mechanism  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>

;; This file is part of tramp-rpc.

;; tramp-rpc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Tests for the tramp-rpc autoload mechanism.
;;
;; These tests verify that:
;; 1. The autoloads file is correctly generated
;; 2. Loading autoloads defines tramp-rpc-method
;; 3. Function stubs are registered as autoloads
;; 4. Method registration happens when tramp loads
;; 5. Calling the predicate triggers full package load
;; 6. Handler is registered after full load
;;
;; Running Tests:
;; --------------
;; These tests must be run in a fresh Emacs without tramp-rpc loaded:
;;
;;   emacs -Q --batch -l test/tramp-rpc-autoload-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Or use the provided test runner:
;;
;;   ./test/run-autoload-tests.sh

;;; Code:

(require 'ert)

;; Get project root
(defvar tramp-rpc-autoload-test--project-root
  (expand-file-name "../" (file-name-directory
                           (or load-file-name buffer-file-name
                               (expand-file-name "test/tramp-rpc-autoload-tests.el"))))
  "Project root directory.")

(defvar tramp-rpc-autoload-test--lisp-dir
  (expand-file-name "lisp" tramp-rpc-autoload-test--project-root)
  "Lisp directory containing tramp-rpc.el.")

(defvar tramp-rpc-autoload-test--autoloads-file
  (expand-file-name "tramp-rpc-autoloads.el" tramp-rpc-autoload-test--lisp-dir)
  "Path to generated autoloads file.")

;;; ============================================================================
;;; Test Helpers
;;; ============================================================================

(defvar tramp-rpc-autoload-test--autoloads-generated nil
  "Non-nil if autoloads have been generated for this test session.")

(defun tramp-rpc-autoload-test--generate-autoloads ()
  "Generate autoloads file for testing.
Only generates once per test session to avoid file disappearing issues."
  ;; Only generate if not already done or file doesn't exist
  (when (or (not tramp-rpc-autoload-test--autoloads-generated)
            (not (file-exists-p tramp-rpc-autoload-test--autoloads-file)))
    (require 'autoload)
    ;; Delete old autoloads if present
    (when (file-exists-p tramp-rpc-autoload-test--autoloads-file)
      (delete-file tramp-rpc-autoload-test--autoloads-file))
    ;; Use loaddefs-generate if available (Emacs 28+), otherwise update-file-autoloads
    (if (fboundp 'loaddefs-generate)
        (loaddefs-generate
         tramp-rpc-autoload-test--lisp-dir
         tramp-rpc-autoload-test--autoloads-file
         nil nil nil t)
      ;; Fallback for older Emacs
      (let ((generated-autoload-file tramp-rpc-autoload-test--autoloads-file)
            (backup-inhibited t))
        (update-file-autoloads
         (expand-file-name "tramp-rpc.el" tramp-rpc-autoload-test--lisp-dir)
         t)))
    ;; Verify file was created
    (unless (file-exists-p tramp-rpc-autoload-test--autoloads-file)
      (error "Failed to generate autoloads file"))
    (setq tramp-rpc-autoload-test--autoloads-generated t)))

(defun tramp-rpc-autoload-test--clean-environment ()
  "Remove tramp-rpc from the environment for testing.
This allows testing autoload behavior in a clean state."
  ;; Unload tramp-rpc if loaded
  (when (featurep 'tramp-rpc)
    (unload-feature 'tramp-rpc t))
  ;; Remove from tramp-methods if present
  (when (boundp 'tramp-methods)
    (setq tramp-methods (assoc-delete-all "rpc" tramp-methods)))
  ;; Remove from handler alist if present
  (when (boundp 'tramp-foreign-file-name-handler-alist)
    (setq tramp-foreign-file-name-handler-alist
          (cl-remove-if (lambda (entry)
                          (eq (cdr entry) 'tramp-rpc-file-name-handler))
                        tramp-foreign-file-name-handler-alist)))
  ;; Unbind symbols
  (mapc (lambda (sym)
          (when (boundp sym) (makunbound sym))
          (when (fboundp sym) (fmakunbound sym)))
        '(tramp-rpc-method
          tramp-rpc-file-name-p
          tramp-rpc-file-name-handler)))

;;; ============================================================================
;;; Tests
;;; ============================================================================

(ert-deftest tramp-rpc-autoload-test-generate-autoloads ()
  "Test that autoloads file can be generated."
  (tramp-rpc-autoload-test--generate-autoloads)
  (should (file-exists-p tramp-rpc-autoload-test--autoloads-file)))

(ert-deftest tramp-rpc-autoload-test-autoloads-define-method ()
  "Test that loading autoloads defines tramp-rpc-method."
  (tramp-rpc-autoload-test--clean-environment)
  (tramp-rpc-autoload-test--generate-autoloads)
  ;; Load autoloads
  (load tramp-rpc-autoload-test--autoloads-file nil t)
  ;; Check method constant is defined
  (should (boundp 'tramp-rpc-method))
  (should (equal tramp-rpc-method "rpc")))

(ert-deftest tramp-rpc-autoload-test-function-stubs ()
  "Test that autoloads register function stubs."
  (tramp-rpc-autoload-test--clean-environment)
  (tramp-rpc-autoload-test--generate-autoloads)
  (load tramp-rpc-autoload-test--autoloads-file nil t)
  ;; Functions should be defined
  (should (fboundp 'tramp-rpc-file-name-p))
  (should (fboundp 'tramp-rpc-file-name-handler))
  ;; And they should be autoloads
  (should (autoloadp (symbol-function 'tramp-rpc-file-name-p)))
  (should (autoloadp (symbol-function 'tramp-rpc-file-name-handler))))

(ert-deftest tramp-rpc-autoload-test-method-registration ()
  "Test that method is registered when tramp loads."
  (tramp-rpc-autoload-test--clean-environment)
  (tramp-rpc-autoload-test--generate-autoloads)
  (load tramp-rpc-autoload-test--autoloads-file nil t)
  ;; Before loading tramp, method should not be in tramp-methods
  ;; (tramp-methods may not even exist yet)
  ;; Load tramp
  (require 'tramp)
  ;; Now method should be registered
  (should (assoc "rpc" tramp-methods)))

(ert-deftest tramp-rpc-autoload-test-predicate-triggers-load ()
  "Test that calling the predicate triggers full package load."
  ;; This test requires msgpack to be available
  (skip-unless (require 'msgpack nil t))
  (tramp-rpc-autoload-test--clean-environment)
  (tramp-rpc-autoload-test--generate-autoloads)
  ;; Add lisp dir to load-path for the autoload to find tramp-rpc.el
  (add-to-list 'load-path tramp-rpc-autoload-test--lisp-dir)
  (load tramp-rpc-autoload-test--autoloads-file nil t)
  (require 'tramp)
  ;; Before calling predicate, it's an autoload
  (should (autoloadp (symbol-function 'tramp-rpc-file-name-p)))
  ;; Call the predicate
  (tramp-rpc-file-name-p "/rpc:user@host:/path")
  ;; Now it should be a real function
  (should-not (autoloadp (symbol-function 'tramp-rpc-file-name-p)))
  ;; And the feature should be provided
  (should (featurep 'tramp-rpc)))

(ert-deftest tramp-rpc-autoload-test-handler-registered-via-autoload ()
  "Test that handler is registered via autoload when tramp loads."
  (tramp-rpc-autoload-test--clean-environment)
  (tramp-rpc-autoload-test--generate-autoloads)
  (add-to-list 'load-path tramp-rpc-autoload-test--lisp-dir)
  (load tramp-rpc-autoload-test--autoloads-file nil t)
  ;; Before loading tramp, handler should not be registered
  (should-not (rassq 'tramp-rpc-file-name-handler
                     tramp-foreign-file-name-handler-alist))
  ;; Load tramp - this triggers with-eval-after-load which registers handler
  (require 'tramp)
  ;; Now handler should be registered (as autoload stub)
  (should (rassq 'tramp-rpc-file-name-handler
                 tramp-foreign-file-name-handler-alist))
  ;; The predicate should still be an autoload at this point
  (should (autoloadp (symbol-function 'tramp-rpc-file-name-p))))

(ert-deftest tramp-rpc-autoload-test-predicate-result ()
  "Test that predicate returns correct results."
  ;; This test requires msgpack to be available
  (skip-unless (require 'msgpack nil t))
  (tramp-rpc-autoload-test--clean-environment)
  (tramp-rpc-autoload-test--generate-autoloads)
  (add-to-list 'load-path tramp-rpc-autoload-test--lisp-dir)
  (load tramp-rpc-autoload-test--autoloads-file nil t)
  (require 'tramp)
  ;; Test with rpc method - should return t
  (should (tramp-rpc-file-name-p "/rpc:user@host:/path"))
  ;; Test with other methods - should return nil
  (should-not (tramp-rpc-file-name-p "/ssh:user@host:/path"))
  (should-not (tramp-rpc-file-name-p "/sudo:root@localhost:/path"))
  (should-not (tramp-rpc-file-name-p "/path/to/local/file")))

(ert-deftest tramp-rpc-autoload-test-autoloads-content ()
  "Test that autoloads file contains expected content."
  (tramp-rpc-autoload-test--generate-autoloads)
  (with-temp-buffer
    (insert-file-contents tramp-rpc-autoload-test--autoloads-file)
    (let ((content (buffer-string)))
      ;; Should define tramp-rpc-method
      (should (string-match-p "defconst tramp-rpc-method" content))
      ;; Should have autoload for file-name-p
      (should (string-match-p "autoload.*tramp-rpc-file-name-p" content))
      ;; Should have autoload for handler
      (should (string-match-p "autoload.*tramp-rpc-file-name-handler" content))
      ;; Should have with-eval-after-load
      (should (string-match-p "with-eval-after-load 'tramp" content))
      ;; Should add to tramp-methods
      (should (string-match-p "add-to-list 'tramp-methods" content))
      ;; Should register in tramp-foreign-file-name-handler-alist
      (should (string-match-p "tramp-foreign-file-name-handler-alist" content)))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun tramp-rpc-autoload-test-run-all ()
  "Run all autoload tests."
  (interactive)
  (ert-run-tests-interactively "^tramp-rpc-autoload-test-"))

(provide 'tramp-rpc-autoload-tests)
;;; tramp-rpc-autoload-tests.el ends here
