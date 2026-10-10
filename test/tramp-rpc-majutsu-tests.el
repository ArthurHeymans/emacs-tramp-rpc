;;; tramp-rpc-majutsu-tests.el --- Majutsu batching tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Add Majutsu and its dependencies to load-path.  Live tests additionally
;; require TRAMP_RPC_TEST_SERVER (a disposable remote binary) and SSH access
;; to TRAMP_RPC_TEST_HOST (default localhost).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'tramp-rpc)
(require 'majutsu-log)
(require 'majutsu-workspace)

(defmacro tramp-rpc-majutsu-test--mock (&rest body)
  "Run BODY with a fake RPC workspace and transport."
  (declare (indent 0))
  `(let ((default-directory "/rpc:test:/repo/")
         (tramp-rpc-majutsu-optimize t)
         (majutsu-jj-global-arguments '("--no-pager" "--color=always"))
         (majutsu-log-sections-hook '(majutsu-log-insert-logs majutsu-log-insert-status))
         (tramp-rpc-majutsu--cache (make-hash-table :test 'equal))
         (tramp-rpc-majutsu--connection 'generation))
     (cl-letf (((symbol-function 'tramp-rpc--get-connection) (lambda (_) 'generation))
               ;; Model a Majutsu that captures stderr in a buffer.
               ((symbol-function 'majutsu--process-file-supported-p)
                (lambda (infile destination)
                  (and (null infile)
                       (or (atom destination)
                           (memq (cadr destination) '(nil t))
                           (stringp (cadr destination))
                           (bufferp (cadr destination))))))
               ((symbol-function 'tramp-rpc-majutsu--environment) (lambda (_vec _args) '(("X" . "1"))))
               ((symbol-function 'majutsu-log--get-value) (lambda (&rest _) '(nil nil)))
               ((symbol-function 'majutsu-log--build-args) (lambda () '("log" "-T" "description"))))
       ,@body)))

(ert-deftest tramp-rpc-majutsu-test-prefetch-pins-log-but-keeps-status-unpinned ()
  (tramp-rpc-majutsu-test--mock
    (let ((operation (make-string 128 ?a)) calls)
      (cl-letf (((symbol-function 'tramp-rpc--call)
                 (lambda (_vec method params &optional connection)
                   (should (equal method "commands.run_parallel"))
                   (let ((commands (alist-get 'commands params)))
                     (unless (equal (alist-get 'key (aref commands 0)) "operation")
                       (should (eq connection 'generation)))
                     (push commands calls)
                     (mapcar (lambda (command)
                               (cons (alist-get 'key command)
                                     `((exit_code . 0)
                                       (stdout . ,(if (equal (alist-get 'key command) "operation")
                                                      operation "OUT"))
                                       (stderr . "WARN"))))
                             (append commands nil))))))
	(tramp-rpc-majutsu--prefetch))
      (should (= 2 (length calls)))
      (should (member "op" (append (alist-get 'args (aref (cadr calls) 0)) nil)))
      (dolist (command (append (car calls) nil))
        (let ((args (append (alist-get 'args command) nil)))
          (if (member "status" args)
              (should (equal args '("--no-pager" "--color=always" "status")))
            (should (equal (seq-take args 2)
                           (list "--at-operation" operation))))))
      (with-temp-buffer
	(let ((stderr (current-buffer)))
          (with-temp-buffer
            (setq default-directory "/rpc:test:/repo/")
            (should (= 0 (tramp-rpc-majutsu--process-file
                          (lambda (&rest _) (ert-fail "Cache miss"))
                          "jj" nil (list t stderr) nil
                          "--no-pager" "--color=always" "status")))
            (should (equal (buffer-string) "OUT")))
          (should (equal (buffer-string) "WARN")))))))

(ert-deftest tramp-rpc-majutsu-test-prefetch-groups-command-environments ()
  "Batch only queries with identical effective remote environments."
  (tramp-rpc-majutsu-test--mock
    (let ((operation (make-string 128 ?a)) environments)
      (cl-letf (((symbol-function 'tramp-rpc-majutsu--environment)
                 (lambda (_vec args)
                   (list (cons "KIND" (if (member "status" args) "status" "other")))))
		((symbol-function 'tramp-rpc--call)
                 (lambda (_vec _method params &optional _connection)
                   (push (alist-get 'env params) environments)
                   (mapcar (lambda (command)
                             (cons (alist-get 'key command)
                                   `((exit_code . 0)
                                     (stdout . ,(if (equal (alist-get 'key command) "operation")
                                                    operation "OUT"))
                                     (stderr . ""))))
                           (append (alist-get 'commands params) nil)))))
	(tramp-rpc-majutsu--prefetch))
      (should (= 3 (length environments)))
      (should (member '(("KIND" . "status")) environments))
      (should (= 2 (hash-table-count tramp-rpc-majutsu--cache))))))

(ert-deftest tramp-rpc-majutsu-test-file-stderr-majutsu-skips-prefetch ()
  "Without buffer stderr capture no section can be served, so do not batch."
  (tramp-rpc-majutsu-test--mock
    (cl-letf (((symbol-function 'majutsu--process-file-supported-p)
               (lambda (_infile destination)
                 (not (and (consp destination) (bufferp (cadr destination))))))
              ((symbol-function 'tramp-rpc--call)
               (lambda (&rest _) (ert-fail "Must not prefetch"))))
      (tramp-rpc-majutsu--prefetch))))

(ert-deftest tramp-rpc-majutsu-test-custom-operation-bypasses-prefetch ()
  (tramp-rpc-majutsu-test--mock
    (cl-letf (((symbol-function 'tramp-rpc--call)
               (lambda (&rest _) (ert-fail "Must not snapshot a custom operation"))))
      (let ((majutsu-jj-global-arguments '("--at-operation=abc")))
	(tramp-rpc-majutsu--prefetch))
      (cl-letf (((symbol-function 'majutsu-log--build-args)
                 (lambda () '("log" "--ignore-working-copy"))))
	(tramp-rpc-majutsu--prefetch)))))

(ert-deftest tramp-rpc-majutsu-test-log-configuration-bypasses-prefetch ()
  "Log-only configuration must not be lost when selecting a snapshot."
  (tramp-rpc-majutsu-test--mock
    (let ((majutsu-log-sections-hook '(majutsu-log-insert-logs majutsu-insert-workspaces)))
      (dolist (args '(("--config=snapshot.max-new-file-size=0")
                      ("--config" "snapshot.auto-track=none()")
                      ("--config-file=/tmp/jj.toml")
                      ("--config-file" "/tmp/jj.toml")))
        (cl-letf (((symbol-function 'majutsu-log--get-value)
                   (lambda (&rest _) (list args nil)))
                  ((symbol-function 'tramp-rpc--call)
                   (lambda (&rest _) (ert-fail "Must preserve log configuration"))))
          (tramp-rpc-majutsu--prefetch))))))

(ert-deftest tramp-rpc-majutsu-test-cache-miss-invalidates-and-preserves-runner ()
  (tramp-rpc-majutsu-test--mock
    (puthash (tramp-rpc-majutsu--key "jj" '("status") '(("X" . "1")))
             '((exit_code . 0) (stdout . "cached") (stderr . ""))
             tramp-rpc-majutsu--cache)
    (let (call)
      (should (= 7 (tramp-rpc-majutsu--process-file
                    (lambda (&rest args) (setq call args) 7)
                    "jj" nil t nil "new")))
      (should (equal call '("jj" nil t nil "new")))
      (should (= 0 (hash-table-count tramp-rpc-majutsu--cache))))))

(ert-deftest tramp-rpc-majutsu-test-cache-key-respects-environment-and-generation ()
  (tramp-rpc-majutsu-test--mock
    (puthash (tramp-rpc-majutsu--key "jj" '("status") '(("X" . "1")))
             '((exit_code . 0) (stdout . "cached") (stderr . ""))
             tramp-rpc-majutsu--cache)
    (cl-letf (((symbol-function 'tramp-rpc-majutsu--environment)
               (lambda (_vec _args) '(("X" . "2")))))
      (should (= 9 (tramp-rpc-majutsu--process-file
                    (lambda (&rest _) 9) "jj" nil '(t nil) nil "status"))))
    (puthash (tramp-rpc-majutsu--key "jj" '("status") '(("X" . "1")))
             '((exit_code . 0) (stdout . "cached") (stderr . ""))
             tramp-rpc-majutsu--cache)
    (cl-letf (((symbol-function 'tramp-rpc--get-connection) (lambda (_) 'replacement)))
      (should (= 9 (tramp-rpc-majutsu--process-file
                    (lambda (&rest _) 9) "jj" nil '(t nil) nil "status"))))))

(ert-deftest tramp-rpc-majutsu-test-prefetch-failure-falls-back-but-render-errors-propagate ()
  (tramp-rpc-majutsu-test--mock
    (cl-letf (((symbol-function 'tramp-rpc-majutsu--prefetch)
               (lambda () (error "Transport failure"))))
      (should (eq 'rendered (tramp-rpc-majutsu--refresh (lambda () 'rendered))))
      (should-error (tramp-rpc-majutsu--refresh (lambda () (error "Render failure")))))
    (cl-letf (((symbol-function 'tramp-rpc-majutsu--prefetch)
               (lambda () (signal 'quit nil))))
      (should (eq 'quit (condition-case nil
                            (tramp-rpc-majutsu--refresh (lambda () (ert-fail "Must quit")))
                          (quit 'quit)))))))

(ert-deftest tramp-rpc-majutsu-test-setup-memoizes-only-for-one-opening ()
  (tramp-rpc-majutsu-test--mock
    (let ((calls 0))
      (cl-labels ((lookup (&optional _) (cl-incf calls) "/rpc:test:/repo/"))
	(dotimes (_ 2)
          (tramp-rpc-majutsu--setup
           (lambda ()
             (should (equal (tramp-rpc-majutsu--toplevel #'lookup) "/rpc:test:/repo/"))
             (tramp-rpc-majutsu--toplevel #'lookup))))
	(should (= 2 calls))))))

(ert-deftest tramp-rpc-majutsu-test-live-refresh ()
  "Render a real RPC workspace, compare outputs and check edits are fresh."
  (skip-unless (getenv "TRAMP_RPC_TEST_SERVER"))
  (skip-unless (tramp-rpc-majutsu--batching-p))
  (let* ((host (or (getenv "TRAMP_RPC_TEST_HOST") "localhost"))
         (tramp-rpc-deploy-never-deploy t)
         (tramp-rpc-deploy-remote-binary-path (getenv "TRAMP_RPC_TEST_SERVER"))
         (tramp-rpc-deploy-remote-directory
          (file-name-directory tramp-rpc-deploy-remote-binary-path))
         (prefix (format "/rpc:%s:" host))
         (root (make-temp-file (concat prefix "/tmp/tramp-rpc-majutsu-") t))
         (default-directory (file-name-as-directory root))
         (majutsu-jj-global-arguments '("--no-pager" "--color=always"
					"--config=ui.log-word-wrap=false"))
         ;; Relative ages can change between the two refreshes being compared.
         (majutsu-log-template-timestamp [:committer :timestamp])
         (majutsu-log--compiled-template-cache nil)
         (majutsu-log-sections-hook '(majutsu-log-insert-logs majutsu-log-insert-status
							      majutsu-insert-workspaces majutsu-log-insert-conflicts))
         (real-call (symbol-function 'tramp-rpc--call))
         calls misses baseline optimized buffer)
    (unwind-protect
        (progn
          (should (= 0 (majutsu-process-jj '(nil nil) "git" "init")))
          (write-region "first\n" nil (expand-file-name "file.txt") nil 'silent)
          (let ((tramp-rpc-majutsu-optimize nil))
            (setq buffer (majutsu-log-setup-buffer))
            (setq baseline (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max)))))
          (cl-letf (((symbol-function 'tramp-rpc--call)
                     (lambda (vec method &rest args)
                       (push method calls)
                       (apply real-call vec method args)))
                    ((symbol-function 'majutsu--process-file-responsive)
                     (lambda (&rest args)
                       (push args misses)
                       (ert-fail "Built-in refresh must not spawn responsive subprocesses"))))
            (with-current-buffer buffer
              (majutsu-refresh-buffer)
              (setq optimized (buffer-substring-no-properties (point-min) (point-max)))))
          (should (equal optimized baseline))
          (should-not misses)
          (should (= 2 (cl-count "commands.run_parallel" calls :test #'equal)))
          (should-not (cl-find-if (lambda (method) (string-prefix-p "process." method)) calls))
          (write-region "second\n" nil (expand-file-name "new-file.txt") nil 'silent)
          (with-current-buffer buffer
            (majutsu-refresh-buffer)
            (should (string-match-p "new-file.txt" (buffer-string))))
          ;; The optimized refresh must have really snapshotted the new file.
          (should (member "new-file.txt" (majutsu-jj-lines "--ignore-working-copy" "file" "list"))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t)
      (tramp-rpc-cleanup-all-connections))))

(defun tramp-rpc-majutsu-test--live-edge-case (config &optional log-args)
  "Compare real refreshes with global CONFIG and optional LOG-ARGS."
  (unless (getenv "TRAMP_RPC_TEST_SERVER")
    (ert-skip "Set TRAMP_RPC_TEST_SERVER to a disposable remote server"))
  (let* ((tramp-rpc-deploy-never-deploy t)
         (tramp-rpc-deploy-remote-binary-path (getenv "TRAMP_RPC_TEST_SERVER"))
         (tramp-rpc-deploy-remote-directory
          (file-name-directory tramp-rpc-deploy-remote-binary-path))
         (prefix (format "/rpc:%s:" (or (getenv "TRAMP_RPC_TEST_HOST") "localhost")))
         (root (make-temp-file (concat prefix "/tmp/tramp-rpc-majutsu-edge-") t))
         (default-directory (file-name-as-directory root))
         (majutsu-jj-global-arguments
          (list "--no-pager" "--color=always" (concat "--config=" config)))
         (majutsu-log-template-timestamp [:committer :timestamp])
         (majutsu-log--compiled-template-cache nil)
         (majutsu-log-sections-hook
          (if log-args
              '(majutsu-log-insert-logs majutsu-insert-workspaces)
            '(majutsu-log-insert-logs majutsu-log-insert-status majutsu-insert-workspaces)))
         (buffer (generate-new-buffer " *tramp-rpc-majutsu-edge*"))
         (real-call (symbol-function 'tramp-rpc--call))
         calls optimized baseline)
    (unwind-protect
        (progn
          (should (= 0 (majutsu-process-jj '(nil nil) "git" "init")))
          (write-region "larger than one byte\n" nil
                        (expand-file-name "untracked.txt") nil 'silent)
          (with-current-buffer buffer
            (setq default-directory (file-name-as-directory root))
            (majutsu-log-mode)
            (setq-local majutsu-buffer-log-args log-args)
            ;; Start optimized, before an ordinary refresh can track the file.
            (cl-letf (((symbol-function 'tramp-rpc--call)
                       (lambda (vec method &rest args)
                         (push method calls)
                         (apply real-call vec method args))))
              (majutsu-refresh-buffer)
              (setq optimized (buffer-substring-no-properties (point-min) (point-max))))
            (let ((tramp-rpc-majutsu-optimize nil))
              (majutsu-refresh-buffer)
              (setq baseline (buffer-substring-no-properties (point-min) (point-max)))))
          (should (equal optimized baseline))
          (if log-args
              (progn
                (should-not (member "commands.run_parallel" calls))
                (should (member "untracked.txt"
                                (majutsu-jj-lines "--ignore-working-copy" "file" "list"))))
            (should (string-match-p "^[?] untracked[.]txt$" optimized))
            (should (= (if (tramp-rpc-majutsu--batching-p) 2 0)
                       (cl-count "commands.run_parallel" calls :test #'equal)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t)
      (tramp-rpc-cleanup-all-connections))))

(ert-deftest tramp-rpc-majutsu-test-live-status-keeps-untracked-files ()
  "Untracked files remain visible when auto-tracking or size limits reject them."
  (dolist (config '("snapshot.auto-track=\"none()\"" "snapshot.max-new-file-size=1"))
    (tramp-rpc-majutsu-test--live-edge-case config)))

(ert-deftest tramp-rpc-majutsu-test-live-log-snapshot-config ()
  "A log-only snapshot limit override is respected without a status section."
  (tramp-rpc-majutsu-test--live-edge-case
   "snapshot.max-new-file-size=1" '("--config=snapshot.max-new-file-size=0")))

(provide 'tramp-rpc-majutsu-tests)
;;; tramp-rpc-majutsu-tests.el ends here
