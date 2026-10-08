;;; run-access-tests.el --- Run kernel access integration tests -*- lexical-binding: t -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Run with emacs -Q --batch -l test/run-access-tests.el.  TRAMP and msgpack
;; must be on load-path (or installed packages).  TRAMP_RPC_ACCESS_TEST_SERVER
;; must name an isolated executable copy of the server.  The Linux ACL runner
;; also sets TRAMP_RPC_ACCESS_TEST_ACL inside a disposable user namespace.
;; Missing prerequisites, skipped tests and an empty selection are failures.

;;; Code:

(require 'package)
(package-initialize)
(setq load-prefer-newer t)
(load (expand-file-name "tramp-rpc-mock-tests.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

(let ((server (getenv "TRAMP_RPC_ACCESS_TEST_SERVER"))
      (selector (if (getenv "TRAMP_RPC_ACCESS_TEST_ACL")
                    '(member tramp-rpc-mock-test-server-access-matches-native-emacs
                             tramp-rpc-mock-test-server-access-posix-acl)
                  '(member tramp-rpc-mock-test-server-access-matches-native-emacs)))
      (tramp-rpc-mock-test--isolate-tests t))
  (unless (and server (file-executable-p server))
    (error "TRAMP_RPC_ACCESS_TEST_SERVER must name an executable server copy"))
  (cl-letf (((symbol-function 'tramp-rpc-mock-test--find-server)
             (lambda () server)))
    (let* ((tests (ert-select-tests selector t))
           (stats (ert-run-tests-batch selector)))
      (unless (and (> (length tests) 0)
                   (= (ert-stats-completed stats) (length tests))
                   (zerop (ert-stats-skipped stats))
                   (zerop (ert-stats-completed-unexpected stats)))
        (error "Kernel access integration tests did not all pass")))))

;;; run-access-tests.el ends here
