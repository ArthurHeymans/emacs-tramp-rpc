;;; tramp-rpc-environment-tests.el --- Environment ownership regressions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; This file is part of tramp-rpc.

;;; Commentary:

;; Loaded by tramp-rpc-mock-tests.el.  Use its live local transport fixture
;; and mock only RPC responses; no SSH or remote server is needed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'tramp-rpc)

(ert-deftest tramp-rpc-mock-test-environment-cold-path-is-cached ()
  "A PATH probe which opens the connection is cached on that generation."
  (let ((vec (tramp-dissect-file-name "/rpc:environment-cold:/"))
        (tramp-rpc-remote-path '("/usr/bin"))
        (connects 0)
        (batches 0))
    (tramp-rpc-mock-test--with-environment-connection vec
      (let* ((old (tramp-rpc--get-connection vec))
             (process (tramp-rpc-connection-process old))
             (buffer (tramp-rpc-connection-buffer old)))
        (tramp-rpc--remove-connection vec process)
        (cl-letf (((symbol-function 'tramp-rpc--connect)
                   (lambda (vec)
                     (cl-incf connects)
                     (tramp-rpc--set-connection vec process buffer)))
                  ((symbol-function 'tramp-rpc--call-batch)
                   (lambda (&rest _)
                     (cl-incf batches)
                     '(((type . "directory"))))))
          (dotimes (_ 2)
            (should (equal (tramp-rpc--cached-remote-path vec) '("/usr/bin"))))
          (should (= connects 1))
          (should (= batches 1)))))))

(ert-deftest tramp-rpc-mock-test-environment-late-path-cannot-refill-reset-or-successor ()
  "Late PATH replies cannot populate reset state or a successor's cache."
  (dolist (transition '(replace reset))
    (let ((vec (tramp-dissect-file-name "/rpc:environment-path:/"))
          (tramp-rpc-remote-path '("/old"))
          (batches 0))
      (tramp-rpc-mock-test--with-environment-connection vec
        (cl-letf (((symbol-function 'tramp-rpc--call-batch)
                   (lambda (&rest _)
                     (when (= (cl-incf batches) 1)
                       (if (eq transition 'replace)
                           (let ((connection (tramp-rpc--get-connection vec)))
                             (tramp-rpc--set-connection
                              vec (tramp-rpc-connection-process connection)
                              (tramp-rpc-connection-buffer connection)))
                         (tramp-rpc--clear-environment-caches vec))
                       (setq tramp-rpc-remote-path '("/new")))
                     '(((type . "directory"))))))
          (should (equal (tramp-rpc--cached-remote-path vec) '("/old")))
          (dotimes (_ 2)
            (should (equal (tramp-rpc--cached-remote-path vec) '("/new"))))
          (should (= batches 2)))))))

(ert-deftest tramp-rpc-mock-test-environment-late-direnv-negative-is-owner-local ()
  "An old exit 127 cannot disable direnv after replacement or cache reset."
  (dolist (transition '(replace reset direnv-reset))
    (let ((vec (tramp-dissect-file-name "/rpc:environment-direnv:/"))
          (tramp-rpc-use-direnv t)
          (tramp-rpc-direnv-cache-timeout 300)
          (calls 0))
      (tramp-rpc-mock-test--with-environment-connection vec
        (cl-letf (((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method _params &optional connection)
                     (should (equal method "process.run"))
                     (should (eq connection (tramp-rpc--get-connection vec)))
                     (if (= (cl-incf calls) 1)
                         (progn
                           (pcase transition
                             ('replace
                              (tramp-rpc--set-connection
                               vec (tramp-rpc-connection-process connection)
                               (tramp-rpc-connection-buffer connection)))
                             ('reset (tramp-rpc--clear-environment-caches vec))
                             ('direnv-reset (tramp-rpc--clear-direnv-cache vec)))
                           '((exit_code . 127) (stdout . "")))
                       '((exit_code . 0) (stdout . "{\"PATH\":\"/new/bin\"}"))))))
          (should-not (tramp-rpc--get-direnv-environment vec "/tmp"))
          (dotimes (_ 2)
            (should (equal (tramp-rpc--get-direnv-environment vec "/tmp")
                           '(("PATH" . "/new/bin")))))
          (should (= calls 2)))))))

(ert-deftest tramp-rpc-mock-test-environment-direnv-clear-preserves-other-probes ()
  "Clearing direnv retains cached PATH and login-shell probes."
  (let ((vec (tramp-dissect-file-name "/rpc:environment-partial:/"))
        (tramp-rpc-remote-path '("/usr/bin"))
        (tramp-rpc-use-direnv t)
        (batches 0)
        (calls 0))
    (tramp-rpc-mock-test--with-environment-connection vec
      (tramp-rpc--cache-system-info vec '((shell . "/bin/zsh")))
      (cl-letf (((symbol-function 'tramp-rpc--call-batch)
                 (lambda (&rest _)
                   (cl-incf batches)
                   '(((type . "directory")))))
                ((symbol-function 'tramp-rpc--call)
                 (lambda (_vec method _params &optional _connection)
                   (should (equal method "process.run"))
                   (cl-incf calls)
                   '((exit_code . 0) (stdout . "{\"PATH\":\"/project/bin\"}")))))
        (dotimes (_ 2)
          (should (equal (tramp-rpc--cached-remote-path vec) '("/usr/bin")))
          (should (equal (tramp-rpc--get-remote-login-shell vec) "/bin/zsh"))
          (should (equal (tramp-rpc--get-direnv-environment vec "/tmp")
                         '(("PATH" . "/project/bin"))))
          (tramp-rpc--clear-direnv-cache vec))
        (should (= batches 1))
        (should (= calls 2))))))

(ert-deftest tramp-rpc-mock-test-environment-all-cache-clear-reseeds-tramp-metadata ()
  "A cache reset refreshes UID, home and the BSD uname property together."
  (let ((vec (tramp-dissect-file-name "/rpc:user@environment-bsd:/"))
        (calls 0))
    (tramp-rpc-mock-test--with-environment-connection vec
      (tramp-rpc--cache-system-info
       vec '((os . "macos") (uid . 1000) (gid . 100) (home . "/old")))
      (should (tramp-check-remote-uname vec tramp-bsd-unames))
      (should (= (tramp-get-remote-uid vec 'integer) 1000))
      (cl-letf (((symbol-function 'tramp-rpc--call)
                 (lambda (_vec method _params &optional _connection)
                   (should (equal method "system.info"))
                   (cl-incf calls)
                   '((os . "macos") (uid . 2000) (gid . 200) (home . "/new")))))
        (tramp-rpc-clear-all-caches)
        (should (= (tramp-get-remote-uid vec 'integer) 2000))
        (should (= (tramp-get-remote-gid vec 'integer) 200))
        (should (equal (tramp-get-home-directory vec) "/new"))
        (should (tramp-check-remote-uname vec tramp-bsd-unames))
        (should (= calls 1))))))

(ert-deftest tramp-rpc-mock-test-environment-late-system-info-keeps-successor-metadata ()
  "An old system.info reply cannot overwrite the successor's TRAMP mirror."
  (let ((vec (tramp-dissect-file-name "/rpc:user@environment-info:/")))
    (tramp-rpc-mock-test--with-environment-connection vec
      (cl-letf (((symbol-function 'tramp-rpc--call)
                 (lambda (_vec method _params &optional connection)
                   (should (equal method "system.info"))
                   (should (eq connection (tramp-rpc--get-connection vec)))
                   (tramp-rpc--set-connection
                    vec (tramp-rpc-connection-process connection)
                    (tramp-rpc-connection-buffer connection))
                   (tramp-rpc--cache-system-info
                    vec '((os . "macos") (uid . 2000) (home . "/new")))
                   '((os . "linux") (uid . 1000) (home . "/old")))))
        (should (= (tramp-get-remote-uid vec 'integer) 2000)))
      (should (= (tramp-get-remote-uid vec 'integer) 2000))
      (should (equal (tramp-get-home-directory vec) "/new"))
      (should (tramp-check-remote-uname vec tramp-bsd-unames)))))

(ert-deftest tramp-rpc-mock-test-environment-late-system-info-without-successor-snapshot ()
  "A stale UID reply fails rather than being cached by TRAMP on a successor."
  (let ((vec (tramp-dissect-file-name "/rpc:user@environment-info-empty:/"))
        (calls 0))
    (tramp-rpc-mock-test--with-environment-connection vec
      (cl-letf (((symbol-function 'tramp-rpc--call)
                 (lambda (_vec method _params &optional connection)
                   (should (equal method "system.info"))
                   (if (= (cl-incf calls) 1)
                       (progn
                         (tramp-rpc--set-connection
                          vec (tramp-rpc-connection-process connection)
                          (tramp-rpc-connection-buffer connection))
                         '((uid . 1000)))
                     '((uid . 2000))))))
        (should-error (tramp-get-remote-uid vec 'integer) :type 'remote-file-error)
        (dotimes (_ 2)
          (should (= (tramp-get-remote-uid vec 'integer) 2000)))
        (should (= calls 2))))))

(ert-deftest tramp-rpc-mock-test-environment-invalidator-errors-do-not-strand-cleanup ()
  "A failing or quitting invalidator cannot strand callbacks on a dead generation."
  (let ((vec (tramp-dissect-file-name "/rpc:environment-cleanup:/"))
        seen response)
    (tramp-rpc-mock-test--with-environment-connection vec
      (let* ((connection (tramp-rpc--get-connection vec))
             (process (tramp-rpc-connection-process connection))
             (tramp-rpc-connection-invalidate-functions
              (list (lambda (_vec) (push 'error seen) (error "Invalidation failed"))
                    (lambda (_vec) (push 'quit seen) (signal 'quit nil))
                    (lambda (_vec) (push 'last seen)))))
        (puthash 42 (lambda (result) (setq response result))
                 (tramp-rpc-connection-async-callbacks connection))
        (tramp-rpc--connection-transport-death process vec "closed\n")
        (should (equal (nreverse seen) '(error quit last)))
        (should (plist-get response :error))
        (should-not (tramp-rpc--get-connection vec))
        ;; Repeated death cannot rerun invalidation or callbacks.
        (setq seen nil response nil)
        (tramp-rpc--connection-transport-death process vec "closed again\n")
        (should-not seen)
        (should-not response)))))

(provide 'tramp-rpc-environment-tests)
;;; tramp-rpc-environment-tests.el ends here
