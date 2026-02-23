;;; run-tramp-tests.el --- Run upstream tramp-tests.el with tramp-rpc  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;;; Commentary:

;; This file adapts the comprehensive test suite from ~/src/tramp/test/tramp-tests.el
;; to run against the tramp-rpc backend instead of tramp-sh (or mock).
;;
;; It works by:
;; 1. Loading tramp-rpc and its dependencies
;; 2. Setting `ert-remote-temporary-file-directory' to use the "rpc" method
;; 3. Overriding method-detection predicates so that tests which should
;;    run with rpc are not incorrectly skipped
;; 4. Loading the upstream tramp-tests.el
;;
;; Usage:
;;   emacs -Q --batch -L ~/src/tramp-rpc/lisp \
;;     -l ~/src/tramp-rpc/test/run-tramp-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; Or to run specific tests:
;;   emacs -Q --batch -L ~/src/tramp-rpc/lisp \
;;     -l ~/src/tramp-rpc/test/run-tramp-tests.el \
;;     --eval '(ert-run-tests-batch-and-exit "tramp-test0[0-9]")'
;;
;; Environment variables:
;;   TRAMP_RPC_TEST_HOST  - Remote host (default: "localhost")
;;   TRAMP_RPC_TEST_USER  - Remote user (default: current user)
;;   TRAMP_VERBOSE        - Tramp verbosity level (default: 0)
;;   TRAMP_TEST_SOURCE    - Path to tramp source tree containing test/tramp-tests.el
;;                          (default: "~/src/tramp")

;;; Code:

;; Install msgpack from MELPA if not available
(unless (require 'msgpack nil t)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (unless (package-installed-p 'msgpack)
    (package-install 'msgpack))
  (require 'msgpack))

;; Add tramp-rpc to load-path
(let ((lisp-dir (expand-file-name
                 "../lisp"
                 (file-name-directory (or load-file-name buffer-file-name
                                         (expand-file-name "test/run-tramp-tests.el"))))))
  (add-to-list 'load-path lisp-dir))

;; Add upstream tramp lisp to load-path so that the test file and
;; tramp library versions stay in sync.  Without this, loading a newer
;; tramp-tests.el against the system tramp.el can fail when tests
;; reference variables (e.g. `tramp-local-host-names') that only exist
;; in the upstream version.
(let* ((tramp-src (or (getenv "TRAMP_TEST_SOURCE")
                      (expand-file-name "~/src/tramp")))
       (upstream-lisp (expand-file-name "lisp" tramp-src)))
  (when (file-directory-p upstream-lisp)
    (add-to-list 'load-path upstream-lisp)))

;; Load tramp-rpc before setting up the test directory
(require 'tramp)
(require 'tramp-rpc)
;; userlock.el defines the `file-locked' error type needed by lock file tests.
;; Loading it directly fails (no feature provided), so load it without require.
(load "userlock" t t)

;; ============================================================================
;; Configuration
;; ============================================================================

(defvar tramp-rpc-test-host
  (or (getenv "TRAMP_RPC_TEST_HOST") "localhost")
  "Remote host for running tramp-tests.el.")

(defvar tramp-rpc-test-user
  (getenv "TRAMP_RPC_TEST_USER")
  "Remote user.  If nil, uses SSH default.")

;; Set ert-remote-temporary-file-directory BEFORE loading tramp-tests.el.
;; This is the key variable that tramp-tests.el uses to decide which method
;; to test.  It must be a directory name ending in /.
(defvar ert-remote-temporary-file-directory
  (let ((user-part (if tramp-rpc-test-user
                       (concat tramp-rpc-test-user "@")
                     "")))
    (format "/rpc:%s%s:/tmp/" user-part tramp-rpc-test-host)))

;; Set tramp-verbose from environment if provided
(when (getenv "TRAMP_VERBOSE")
  (setq tramp-verbose (string-to-number (getenv "TRAMP_VERBOSE"))))

;; ============================================================================
;; Load the upstream test suite
;; ============================================================================

(defvar tramp-rpc-test-source
  (or (getenv "TRAMP_TEST_SOURCE")
      (expand-file-name "~/src/tramp"))
  "Path to the tramp source tree containing test/tramp-tests.el.")

(message "=== Running tramp-tests.el with tramp-rpc method ===")
(message "Remote directory: %s" ert-remote-temporary-file-directory)
(message "Method: rpc, Host: %s" tramp-rpc-test-host)
(message "Tramp source: %s" tramp-rpc-test-source)

(let ((test-file (expand-file-name "test/tramp-tests.el" tramp-rpc-test-source)))
  (unless (file-exists-p test-file)
    (error "Upstream tramp-tests.el not found at %s.\nSet TRAMP_TEST_SOURCE to the tramp source tree" test-file))
  (load test-file))

;; ============================================================================
;; Predicate overrides
;; ============================================================================

;; tramp-rpc supports external processes via process.run and process.start
(setf (symbol-function #'tramp--test-supports-processes-p) #'always)
;; tramp-rpc supports set-file-modes via file.chmod RPC
(setf (symbol-function #'tramp--test-supports-set-file-modes-p) #'always)

(provide 'run-tramp-tests)
;;; run-tramp-tests.el ends here
