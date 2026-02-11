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
;;   TRAMP_RPC_TEST_HOST  - Remote host (default: "x220-nixos")
;;   TRAMP_RPC_TEST_USER  - Remote user (default: current user)
;;   TRAMP_VERBOSE        - Tramp verbosity level (default: 0)

;;; Code:

;; Install msgpack from MELPA if not available
(unless (require 'msgpack nil t)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'msgpack)
  (require 'msgpack))

;; Add tramp-rpc to load-path
(let ((lisp-dir (expand-file-name
                 "../lisp"
                 (file-name-directory (or load-file-name buffer-file-name
                                         (expand-file-name "test/run-tramp-tests.el"))))))
  (add-to-list 'load-path lisp-dir))

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
  (or (getenv "TRAMP_RPC_TEST_HOST") "x220-nixos")
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
;; Predicate overrides
;; ============================================================================

;; tramp-rpc is not a tramp-sh method, so tramp--test-sh-p returns nil.
;; However, tramp-rpc supports processes and many sh-gated features.
;; We define a tramp--test-rpc-p predicate and override the capability
;; predicates to include rpc.

(defun tramp--test-rpc-p ()
  "Check whether the rpc method is used."
  (string-equal
   "rpc" (file-remote-p ert-remote-temporary-file-directory 'method)))

;; tramp-rpc supports external processes via process.run and process.start
(defun tramp--test-supports-processes-p ()
  "Return whether the method under test supports external processes.
Overridden to include tramp-rpc."
  (unless (tramp--test-crypt-p)
    (or (tramp--test-adb-p)
        (tramp--test-sh-p)
        (tramp--test-sshfs-p)
        (tramp--test-rpc-p)
        (and (tramp--test-smb-p)
             (file-writable-p
              (file-name-concat
               (file-remote-p ert-remote-temporary-file-directory)
               "ADMIN$" "Boot"))))))

;; tramp-rpc supports set-file-modes via file.chmod RPC
(defun tramp--test-supports-set-file-modes-p ()
  "Return whether the method under test supports setting file modes.
Overridden to include tramp-rpc."
  (or (tramp--test-sh-p)
      (tramp--test-sshfs-p)
      (tramp--test-sudoedit-p)
      (tramp--test-rpc-p)
      (and
       (tramp--test-gvfs-p)
       (string-suffix-p
        "ftp" (file-remote-p ert-remote-temporary-file-directory 'method)))))

;; ============================================================================
;; Load the upstream test suite
;; ============================================================================

(message "=== Running tramp-tests.el with tramp-rpc method ===")
(message "Remote directory: %s" ert-remote-temporary-file-directory)
(message "Method: rpc, Host: %s" tramp-rpc-test-host)

(load (expand-file-name "~/src/tramp/test/tramp-tests.el"))

(provide 'run-tramp-tests)
;;; run-tramp-tests.el ends here
