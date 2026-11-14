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

(ert-deftest tramp-rpc-mock-test-environment-login-path-retries-failed-shell-probes ()
  "Nonzero, markerless and truncated replies must not become cached PATHs."
  (dolist (accessor '(tramp-rpc--cached-login-path tramp-rpc--cached-remote-path))
    (dolist (failure '(nonzero missing-marker truncated))
      (let ((vec (tramp-dissect-file-name "/rpc:environment-login-retry:/"))
            (tramp-rpc-remote-path '(tramp-own-remote-path "/bin"))
            (calls 0))
        (tramp-rpc-mock-test--with-environment-connection vec
          (tramp-rpc--cache-system-info vec '((shell . "/bin/sh")))
          (cl-letf (((symbol-function 'tramp-rpc--call)
                     (lambda (_vec method params &optional _connection)
                       (should (equal method "process.run"))
                       (let* ((command (aref (alist-get 'args params) 2))
                              (marker (progn
                                        (should (string-match "echo \\([^;]+\\);" command))
                                        (match-string 1 command))))
                         (if (= (cl-incf calls) 1)
                             (pcase failure
                               ('nonzero `((exit_code . 1) (stdout . ,(concat marker "\n/old\n"))))
                               ('missing-marker '((exit_code . 0) (stdout . "banner\n")))
                               ('truncated `((exit_code . 0) (stdout . ,(concat marker "\n")))))
                           `((exit_code . 0) (stdout . ,(concat marker "\n/project/bin\n")))))))
                    ((symbol-function 'tramp-rpc--call-batch)
                     (lambda (_vec requests)
                       (make-list (length requests) '((type . "directory"))))))
            (should (equal (funcall accessor vec)
                           (when (eq accessor 'tramp-rpc--cached-remote-path) '("/bin"))))
            (dotimes (_ 2)
              (should (equal (funcall accessor vec)
                             (if (eq accessor 'tramp-rpc--cached-remote-path)
                                 '("/project/bin" "/bin")
                               '("/project/bin")))))
            (should (= calls 2))))))))

(ert-deftest tramp-rpc-mock-test-environment-empty-login-path-is-cached ()
  "A successfully empty login PATH is not confused with probe failure."
  (dolist (accessor '(tramp-rpc--cached-login-path tramp-rpc--cached-remote-path))
    (let ((vec (tramp-dissect-file-name "/rpc:environment-login-empty:/"))
          (tramp-rpc-remote-path '(tramp-own-remote-path))
          (calls 0))
      (tramp-rpc-mock-test--with-environment-connection vec
        (tramp-rpc--cache-system-info vec '((shell . "/bin/sh")))
        (cl-letf (((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method params &optional _connection)
                     (should (equal method "process.run"))
                     (cl-incf calls)
                     (let ((command (aref (alist-get 'args params) 2)))
                       (should (string-match "echo \\([^;]+\\);" command))
                       `((exit_code . 0)
                         (stdout . ,(concat (match-string 1 command) "\n\n")))))))
          (dotimes (_ 2)
            (should-not (funcall accessor vec)))
          (should (= calls 1)))))))

(ert-deftest tramp-rpc-mock-test-environment-reset-rejects-pending-system-info ()
  "TRAMP must not cache a pre-reset reply on the same live connection."
  (dolist (refreshed '(nil t))
    (let ((vec (tramp-dissect-file-name "/rpc:user@environment-metadata-reset:/"))
          (fresh '((uid . 2000) (gid . 200) (home . "/new") (os . "macos")))
          (calls 0))
      (tramp-rpc-mock-test--with-environment-connection vec
        (cl-letf (((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method _params &optional _connection)
                     (should (equal method "system.info"))
                     (if (= (cl-incf calls) 1)
                         (progn
                           (tramp-rpc-clear-all-caches)
                           (when refreshed
                             (tramp-rpc--cache-system-info vec fresh))
                           '((uid . 1000) (gid . 100) (home . "/old") (os . "linux")))
                       fresh))))
          (if refreshed
              (should (= (tramp-get-remote-uid vec 'integer) 2000))
            (should-error (tramp-get-remote-uid vec 'integer) :type 'remote-file-error))
          (dotimes (_ 2)
            (should (= (tramp-get-remote-uid vec 'integer) 2000))
            (should (= (tramp-get-remote-gid vec 'integer) 200))
            (should (equal (tramp-get-home-directory vec) "/new"))
            (should (tramp-check-remote-uname vec tramp-bsd-unames)))
          (should (= calls (if refreshed 1 2))))))))

(ert-deftest tramp-rpc-mock-test-environment-reset-rejects-startup-metadata ()
  "The readiness probe must not reseed metadata after a same-connection reset."
  (let* ((vec (tramp-dissect-file-name "/rpc:user@environment-startup-reset:/"))
         (buffer-name (tramp-buffer-name vec))
         (tramp-rpc--connections (make-hash-table :test 'equal))
         (calls 0)
         process)
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq process
                           (make-pipe-process :name "environment-startup-reset"
                                              :buffer (plist-get args :buffer)
                                              :noquery t))))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (_vec method _params &optional _connection)
                     (should (equal method "system.info"))
                     (if (= (cl-incf calls) 1)
                         (progn
                           (tramp-rpc-clear-all-caches)
                           `((protocol_revision . ,tramp-rpc-protocol-revision)
                             (uid . 1000) (home . "/old")))
                       '((uid . 2000) (home . "/new"))))))
          (tramp-rpc--start-server-process vec "/unused/server")
          (should (= (tramp-get-remote-uid vec 'integer) 2000))
          (should (equal (tramp-get-home-directory vec) "/new"))
          (should (= calls 2)))
      (tramp-rpc--disconnect vec)
      (when (and process (process-live-p process))
        (delete-process process))
      (dolist (name (list buffer-name (concat buffer-name " stderr")))
        (when-let* ((buffer (get-buffer name)))
          (kill-buffer buffer))))))

(ert-deftest tramp-rpc-mock-test-autoload-runner-discovers-installed-msgpack ()
  "The runner finds installed msgpack without activating an installed TRAMP."
  (let* ((home (make-temp-file "tramp-rpc-autoload-home-" t))
         (elpa (expand-file-name ".emacs.d/elpa" home))
         (msgpack-dir (expand-file-name "msgpack-0.1.1" elpa))
         (tramp-dir (expand-file-name "tramp-99.0" elpa))
         (msgpack (locate-library "msgpack"))
         (source (file-name-directory (locate-library "tramp")))
         (runner (expand-file-name "test/run-autoload-tests.sh"
                                   tramp-rpc-mock-test--project-root))
         (process-environment (copy-sequence process-environment))
         (default-directory temporary-file-directory))
    (unwind-protect
        (progn
          (make-directory msgpack-dir t)
          (copy-file msgpack (expand-file-name (file-name-nondirectory msgpack)
                                               msgpack-dir))
          (with-temp-file (expand-file-name "msgpack-pkg.el" msgpack-dir)
            (insert "(define-package \"msgpack\" \"0.1.1\" \"Test dependency\" nil)\n"))
          (make-directory tramp-dir t)
          (with-temp-file (expand-file-name "tramp-pkg.el" tramp-dir)
            (insert "(define-package \"tramp\" \"99.0\" \"Must not activate\" nil)\n"))
          (with-temp-file (expand-file-name "tramp-autoloads.el" tramp-dir)
            (insert "(error \"Installed TRAMP must not be activated\")\n"))
          (setenv "HOME" home)
          (setenv "XDG_CONFIG_HOME" (expand-file-name ".config" home))
          (setenv "EMACSLOADPATH" nil)
          (setenv "MSGPACK_SOURCE" nil)
          (setenv "TRAMP_SOURCE" source)
          (with-temp-buffer
            (let ((status (call-process "bash" nil t nil runner)))
              (ert-info ((buffer-string))
                (should (= status 0))))))
      (delete-directory home t))))

(provide 'tramp-rpc-environment-tests)
;;; tramp-rpc-environment-tests.el ends here
