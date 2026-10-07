;;; tramp-rpc-magit-tests.el --- Real Magit over SSH/RPC tests -*- lexical-binding: t; -*-

;; Run with test/run-magit-tests.sh.  All repositories and server binaries must
;; be disposable; the suite never uses an existing remote repository.

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'magit-stash)
(require 'tramp-rpc)

(defvar tramp-rpc-magit-test--root nil)
(defvar tramp-rpc-magit-test--trace nil)
(defvar tramp-rpc-magit-test--hits 0)
(defvar tramp-rpc-magit-test--measurements nil)

(defun tramp-rpc-magit-test--record-rpc (orig vec method params &rest rest)
  (push (cons method params) tramp-rpc-magit-test--trace)
  (apply orig vec method params rest))

(defun tramp-rpc-magit-test--record-hit (orig &rest args)
  (let ((result (apply orig args)))
    (when result (cl-incf tramp-rpc-magit-test--hits))
    result))

(defun tramp-rpc-magit-test--measure (label thunk)
  "Call THUNK, recording actual transport requests and elapsed time."
  (let ((tramp-rpc-magit-test--trace nil)
        (tramp-rpc-magit-test--hits 0)
        (start (float-time)))
    (advice-add 'tramp-rpc--call :around #'tramp-rpc-magit-test--record-rpc)
    (advice-add 'tramp-rpc-magit--process-cache-lookup
                :around #'tramp-rpc-magit-test--record-hit)
    (unwind-protect
        (let* ((result (save-current-buffer (funcall thunk)))
               (trace (nreverse tramp-rpc-magit-test--trace))
               (row (list :label label :seconds (- (float-time) start)
                          :rpcs (length trace) :hits tramp-rpc-magit-test--hits
                          :process-run (cl-count "process.run" trace
                                                 :key #'car :test #'equal)
                          :batches (cl-count "commands.run_parallel" trace
                                            :key #'car :test #'equal)
                          :trace trace :result result)))
          (push row tramp-rpc-magit-test--measurements)
          (message "MAGIT-MEASURE %s %.3fs rpc=%d process.run=%d batches=%d hits=%d"
                   label (plist-get row :seconds) (plist-get row :rpcs)
                   (plist-get row :process-run) (plist-get row :batches)
                   (plist-get row :hits))
          row)
      (advice-remove 'tramp-rpc--call #'tramp-rpc-magit-test--record-rpc)
      (advice-remove 'tramp-rpc-magit--process-cache-lookup
                     #'tramp-rpc-magit-test--record-hit))))

(defun tramp-rpc-magit-test--git (&rest args)
  "Run a fixture command, without consulting Magit's process cache."
  (let ((tramp-rpc-magit--allow-process-cache nil)
        (process-file-side-effects t))
    (with-temp-buffer
      (let ((exit (apply #'process-file "git" nil t nil args)))
        (unless (zerop exit)
          (error "Fixture git %S: %s" args (buffer-string)))
        (string-trim-right (buffer-string))))))

(defun tramp-rpc-magit-test--write (file text)
  (with-temp-file (expand-file-name file default-directory) (insert text)))

(defun tramp-rpc-magit-test--init ()
  (tramp-rpc-magit-test--git "init" "-b" "main")
  (tramp-rpc-magit-test--git "config" "user.name" "RPC Magit Test")
  (tramp-rpc-magit-test--git "config" "user.email" "rpc-test@example.invalid")
  (tramp-rpc-magit-test--git "config" "commit.gpgsign" "false")
  (tramp-rpc-magit-test--git "config" "tag.gpgsign" "false")
  (tramp-rpc-magit-test--git "config" "core.autocrlf" "false"))

(defun tramp-rpc-magit-test--commit (message)
  (tramp-rpc-magit-test--git "add" "--all")
  (tramp-rpc-magit-test--git "commit" "-m" message))

(defun tramp-rpc-magit-test--rich-fixture ()
  (tramp-rpc-magit-test--init)
  (dolist (file '("tracked.txt" "staged.txt" "deleted.txt" "rename.txt"
                  "space name.txt" "literal[1]|é.txt"))
    (tramp-rpc-magit-test--write file "base\n"))
  (tramp-rpc-magit-test--write ".gitignore" "ignored.txt\n")
  (tramp-rpc-magit-test--commit "initial")
  (tramp-rpc-magit-test--git "tag" "v1")
  (tramp-rpc-magit-test--git "branch" "other")
  (tramp-rpc-magit-test--write "tracked.txt" "stash\n")
  (tramp-rpc-magit-test--git "stash" "push" "-m" "saved change")
  (tramp-rpc-magit-test--write "staged.txt" "staged change\n")
  (tramp-rpc-magit-test--git "add" "staged.txt")
  (tramp-rpc-magit-test--git "mv" "rename.txt" "renamed space.txt")
  (delete-file (expand-file-name "deleted.txt" default-directory))
  (dolist (file '("tracked.txt" "space name.txt" "literal[1]|é.txt"))
    (tramp-rpc-magit-test--write file "unstaged change\n"))
  (tramp-rpc-magit-test--write "untracked.txt" "new\n")
  (tramp-rpc-magit-test--write "ignored.txt" "ignored\n"))

(defun tramp-rpc-magit-test--kill-buffers ()
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer
            (and (derived-mode-p 'magit-mode)
                 (string-prefix-p tramp-rpc-magit-test--root default-directory)))
      (kill-buffer buffer))))

(defmacro tramp-rpc-magit-test--with-repo (&rest body)
  "Create an isolated remote repository and always clean it up."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let* ((host (or (getenv "TRAMP_RPC_TEST_HOST") "localhost"))
            (tramp-rpc-magit-test--root
             (file-name-as-directory
              (make-temp-file (format "/rpc:%s:/tmp/tramp-rpc-magit-" host) t)))
            (default-directory tramp-rpc-magit-test--root)
            (magit-status-mode-hook
             (cons (lambda ()
                     (setq-local magit-section-initial-visibility-alist
                                 '((staged . hide) (unstaged . hide) (untracked . hide)))
                     (setq-local magit-section-set-visibility-hook nil))
                   magit-status-mode-hook))
            (magit-refresh-verbose nil)
            (magit-auto-revert-mode nil)
            (magit-process-popup-time -1)
            (magit-save-repository-buffers nil))
       (unwind-protect (progn ,@body)
         (tramp-rpc-magit-test--kill-buffers)
         (tramp-rpc-magit--clear-cache)
         (delete-directory tramp-rpc-magit-test--root t)
         (tramp-rpc-magit-enable)))))

(defun tramp-rpc-magit-test--sections (&optional root)
  (let ((root (or root magit-root-section)))
    (cons root (mapcan #'tramp-rpc-magit-test--sections (oref root children)))))

(defun tramp-rpc-magit-test--expand ()
  "Expand real lazy sections, including children created during expansion."
  (cl-labels ((show (section)
                (when (oref section hidden) (magit-section-show section))
                (mapc #'show (oref section children))))
    (show magit-root-section))
  (buffer-substring-no-properties (point-min) (point-max)))

(defun tramp-rpc-magit-test--status (optimized &optional expanded)
  (let ((directory default-directory)
        (tramp-rpc-magit-optimize optimized))
    (save-current-buffer
      (tramp-rpc-magit-test--kill-buffers)
      (tramp-rpc-magit--clear-caches-for-directory directory)
      (if optimized (tramp-rpc-magit-enable) (tramp-rpc-magit-disable))
      (let* ((default-directory directory)
             (buffer (magit-status-setup-buffer directory)))
        (should (bufferp buffer))
        (with-current-buffer buffer
          (should (file-remote-p default-directory))
          (should (string-prefix-p tramp-rpc-magit-test--root default-directory))
          (if expanded (tramp-rpc-magit-test--expand)
            (buffer-substring-no-properties (point-min) (point-max))))))))

(defun tramp-rpc-magit-test--current-status-text ()
  "Read the sentinel-refreshed status buffer without forcing another refresh."
  (with-current-buffer (magit-get-mode-buffer 'magit-status-mode)
    (tramp-rpc-magit-test--expand)))

(defun tramp-rpc-magit-test--parity (&optional label)
  "Compare complete real Magit output with and without optimization."
  (let ((plain (tramp-rpc-magit-test--measure
                (concat (or label "status") "/plain")
                (lambda () (tramp-rpc-magit-test--status nil t))))
        (fast (tramp-rpc-magit-test--measure
               (concat (or label "status") "/optimized")
               (lambda () (tramp-rpc-magit-test--status t t)))))
    (should (equal (plist-get plain :result) (plist-get fast :result)))
    (should (> (plist-get fast :hits) 0))
    (should (> (plist-get fast :batches) 0))
    (cons plain fast)))

(defun tramp-rpc-magit-test--wait (process)
  "Wait for the real Magit sentinel, with a hard deadline."
  (should (processp process))
  (let ((deadline (+ (float-time) 30)))
    (while (and (process-live-p process) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should-not (process-live-p process))
    (should (= (process-exit-status process) 0))))

(ert-deftest tramp-rpc-magit-test-status-and-refresh ()
  (tramp-rpc-magit-test--with-repo
    (tramp-rpc-magit-test--rich-fixture)
    (let* ((pair (tramp-rpc-magit-test--parity "rich-status"))
           (plain (car pair)) (fast (cdr pair))
           (trace (plist-get fast :trace))
           (updates (cl-remove-if-not
                     (lambda (entry)
                       (and (equal (car entry) "process.run")
                            (member "update-index"
                                    (append (alist-get 'args (cdr entry)) nil))))
                     trace)))
      (should (< (plist-get fast :process-run) (plist-get plain :process-run)))
      (should (< (plist-get fast :rpcs) (plist-get plain :rpcs)))
      (should (= (length updates) 1))
      (should (< (cl-position (car updates) trace)
                 (cl-position "commands.run_parallel" trace
                              :key #'car :test #'equal))))
    (let* ((buffer (magit-get-mode-buffer 'magit-status-mode))
           (before (with-current-buffer buffer (buffer-string)))
           (refresh (tramp-rpc-magit-test--measure
                     "expanded-refresh"
                     (lambda ()
                       (with-current-buffer buffer
                         (magit-refresh)
                         (tramp-rpc-magit-test--expand))))))
      (should (> (plist-get refresh :batches) 0))
      (with-current-buffer buffer
        (should (equal (substring-no-properties before)
                       (buffer-substring-no-properties (point-min) (point-max))))))))

(ert-deftest tramp-rpc-magit-test-lazy-and-expired-sections ()
  (tramp-rpc-magit-test--with-repo
    (tramp-rpc-magit-test--rich-fixture)
    (tramp-rpc-magit-test--status nil)
    (let ((expected
           (plist-get
            (tramp-rpc-magit-test--measure
             "plain-lazy-expand"
             (lambda ()
               (with-current-buffer (magit-get-mode-buffer 'magit-status-mode)
                 (tramp-rpc-magit-test--expand))))
            :result)))
      (tramp-rpc-magit-test--status t)
      (with-current-buffer (magit-get-mode-buffer 'magit-status-mode)
        (should (cl-some (lambda (section) (oref section hidden))
                         (tramp-rpc-magit-test--sections)))
        (let ((row (tramp-rpc-magit-test--measure
                    "lazy-expand" #'tramp-rpc-magit-test--expand)))
          (should (equal expected (plist-get row :result)))
          ;; This Magit can wash saved diff output without invoking Git again.
          (should (= (plist-get row :rpcs) 0)))
        (dolist (section (tramp-rpc-magit-test--sections))
          (when (eq (oref section type) 'file) (magit-section-hide section)))
        (maphash (lambda (_key entry)
                   (setf (plist-get entry :time) (- (float-time) 3600)))
                 tramp-rpc-magit--process-caches)
        (should-not (tramp-rpc-magit--get-process-cache))
        (let ((row (tramp-rpc-magit-test--measure
                    "expired-file-expand" #'tramp-rpc-magit-test--expand)))
          (should (equal expected (plist-get row :result)))
          (should (> (plist-get row :batches) 0)))
        (let ((row (tramp-rpc-magit-test--measure
                    "repeat-expand" #'tramp-rpc-magit-test--expand)))
          (should (= (plist-get row :rpcs) 0)))))))

(ert-deftest tramp-rpc-magit-test-stage-unstage-commit-checkout-stash ()
  (tramp-rpc-magit-test--with-repo
    (tramp-rpc-magit-test--init)
    (tramp-rpc-magit-test--write "tracked.txt" "base\n")
    (tramp-rpc-magit-test--commit "initial")
    (tramp-rpc-magit-test--git "branch" "other")
    (tramp-rpc-magit-test--write "tracked.txt" "changed\n")
    (tramp-rpc-magit-test--status t t)
    (with-current-buffer (magit-get-mode-buffer 'magit-status-mode)
      (magit-stage-files '("tracked.txt"))
      (should (string-match-p "^Staged changes"
                              (tramp-rpc-magit-test--current-status-text)))
      (should-not (string-match-p "Unstaged changes"
                                  (tramp-rpc-magit-test--current-status-text)))
      (should (equal (tramp-rpc-magit-test--git "diff" "--cached" "--name-only")
                     "tracked.txt"))
      (magit-unstage-files '("tracked.txt"))
      (should (string-match-p "Unstaged changes"
                              (tramp-rpc-magit-test--current-status-text)))
      (should-not (string-match-p "^Staged changes"
                                  (tramp-rpc-magit-test--current-status-text)))
      (should (string-empty-p (tramp-rpc-magit-test--git
                               "diff" "--cached" "--name-only")))
      (magit-stage-files '("tracked.txt"))
      (tramp-rpc-magit-test--wait
       (magit-run-git-async "commit" "-m" "Magit commit")))
    (should (string-match-p "Head:.*main.*Magit commit"
                            (tramp-rpc-magit-test--current-status-text)))
    (should (equal (tramp-rpc-magit-test--git "log" "-1" "--format=%s")
                   "Magit commit"))
    (tramp-rpc-magit-test--parity "after-commit")
    (with-current-buffer (magit-get-mode-buffer 'magit-status-mode)
      (tramp-rpc-magit-test--wait (magit-run-git-async "checkout" "other")))
    (should (string-match-p "Head:.*other.*initial"
                            (tramp-rpc-magit-test--current-status-text)))
    (should (equal (tramp-rpc-magit-test--git "branch" "--show-current") "other"))
    (tramp-rpc-magit-test--parity "after-checkout")
    (tramp-rpc-magit-test--write "tracked.txt" "stash me\n")
    (with-current-buffer (magit-get-mode-buffer 'magit-status-mode)
      (tramp-rpc-magit-test--wait
       (magit-run-git-async "stash" "push" "-m" "Magit stash")))
    (should (string-match-p "Magit stash"
                            (tramp-rpc-magit-test--current-status-text)))
    (should-not (string-match-p "Unstaged changes"
                                (tramp-rpc-magit-test--current-status-text)))
    (should (string-empty-p (tramp-rpc-magit-test--git "status" "--porcelain")))
    (tramp-rpc-magit-test--parity "after-stash")
    (with-current-buffer (magit-get-mode-buffer 'magit-status-mode)
      (magit-stash-pop "stash@{0}"))
    (should (string-match-p "Unstaged changes"
                            (tramp-rpc-magit-test--current-status-text)))
    (should (equal (tramp-rpc-magit-test--git "diff" "--name-only") "tracked.txt"))
    (tramp-rpc-magit-test--parity "after-stash-pop")))

(ert-deftest tramp-rpc-magit-test-unborn-and-detached ()
  (tramp-rpc-magit-test--with-repo
    (tramp-rpc-magit-test--init)
    (tramp-rpc-magit-test--write "new.txt" "new\n")
    (tramp-rpc-magit-test--parity "unborn-untracked")
    (tramp-rpc-magit-test--git "add" "new.txt")
    (tramp-rpc-magit-test--parity "unborn-staged")
    (tramp-rpc-magit-test--commit "initial")
    (tramp-rpc-magit-test--git "checkout" "--detach")
    (tramp-rpc-magit-test--parity "detached")))

(ert-deftest tramp-rpc-magit-test-linked-worktree-and-subdirectory ()
  (tramp-rpc-magit-test--with-repo
    (tramp-rpc-magit-test--init)
    (make-directory "sub")
    (tramp-rpc-magit-test--write "sub/file.txt" "base\n")
    (tramp-rpc-magit-test--commit "initial")
    (let ((default-directory (expand-file-name "sub/" default-directory)))
      (tramp-rpc-magit-test--parity "subdirectory"))
    (tramp-rpc-magit-test--git "worktree" "add" "-b" "linked" "linked")
    (let ((default-directory (expand-file-name "linked/" default-directory)))
      (tramp-rpc-magit-test--write "sub/file.txt" "linked change\n")
      (tramp-rpc-magit-test--parity "linked-worktree"))))

(ert-deftest tramp-rpc-magit-test-log-diff-show-and-blame ()
  (tramp-rpc-magit-test--with-repo
    (tramp-rpc-magit-test--rich-fixture)
    (dolist (operation
             (list (cons "log" (lambda () (magit-log-setup-buffer
                                           '("HEAD") '("-n10" "--decorate") nil)))
                   (cons "diff" (lambda () (magit-diff-setup-buffer
                                            nil "--cached" '("--no-ext-diff") nil)))
                   (cons "show" (lambda ()
                                  (magit-show-commit "HEAD")
                                  (magit-get-mode-buffer 'magit-revision-mode)))
                   (cons "blame" (lambda () (magit-git-string
                                             "blame" "--line-porcelain" "HEAD"
                                             "--" "tracked.txt")))))
      (let (outputs)
        (dolist (optimized '(nil t))
          (let ((tramp-rpc-magit-optimize optimized))
            (tramp-rpc-magit-test--status optimized t)
            (push (plist-get
                   (tramp-rpc-magit-test--measure
                    (format "%s/%s" (car operation) (if optimized "optimized" "plain"))
                    (lambda ()
                      (let ((result (funcall (cdr operation))))
                        (if (bufferp result)
                            (with-current-buffer result
                              (buffer-substring-no-properties (point-min) (point-max)))
                          result))))
                   :result) outputs)))
        (should (stringp (car outputs)))
        (should-not (string-empty-p (car outputs)))
        (should (equal (car outputs) (cadr outputs)))))))

(ert-deftest tramp-rpc-magit-test-prefetch-command-parity ()
  "Check every cached argv against real, uncached Git over the same transport."
  (tramp-rpc-magit-test--with-repo
    (tramp-rpc-magit-test--rich-fixture)
    (tramp-rpc-magit-test--status t t)
    (let ((cache (copy-hash-table (tramp-rpc-magit--get-process-cache)))
          (checked 0))
      (should (> (hash-table-count cache) 60))
      (maphash
       (lambda (key expected)
         (let* ((args (append tramp-rpc-magit--git-prefetch-prefix-args (read key)))
                (actual
                 (let ((tramp-rpc-magit--allow-process-cache nil)
                       (process-file-side-effects nil))
                   (with-temp-buffer
                     (let ((exit (apply #'process-file "git" nil '(t nil) nil args)))
                       (cons exit (buffer-string)))))))
           (ert-info ((format "Prefetched argv: %s" key))
             (should (equal expected actual)))
           (cl-incf checked)))
       cache)
      (message "MAGIT-PREFETCH checked %d actual Git commands" checked))))

(ert-deftest tramp-rpc-magit-test-hunk-stage-with-stdin ()
  "Use Magit's patch-via-stdin path, not just whole-file Git add/reset."
  (tramp-rpc-magit-test--with-repo
    (tramp-rpc-magit-test--init)
    (tramp-rpc-magit-test--write "file.txt" "base\n")
    (tramp-rpc-magit-test--commit "initial")
    (tramp-rpc-magit-test--write "file.txt" "changed\n")
    (tramp-rpc-magit-test--status t t)
    (with-current-buffer (magit-get-mode-buffer 'magit-status-mode)
      (let ((hunk (cl-find 'hunk (tramp-rpc-magit-test--sections)
                           :key (lambda (section) (oref section type)))))
        (should hunk)
        (goto-char (oref hunk start))
        (magit-stage))
      (should (equal (tramp-rpc-magit-test--git "diff" "--cached" "--name-only")
                     "file.txt"))
      (should (string-empty-p (tramp-rpc-magit-test--git "diff" "--name-only"))))
    (tramp-rpc-magit-test--parity "after-hunk-stage")))

(ert-deftest tramp-rpc-magit-test-merge-rebase-cherry-pick-conflicts ()
  (tramp-rpc-magit-test--with-repo
    (tramp-rpc-magit-test--init)
    (tramp-rpc-magit-test--write "conflict.txt" "base\n")
    (tramp-rpc-magit-test--commit "initial")
    (tramp-rpc-magit-test--git "checkout" "-b" "topic")
    (tramp-rpc-magit-test--write "conflict.txt" "topic\n")
    (tramp-rpc-magit-test--commit "topic change")
    (tramp-rpc-magit-test--git "checkout" "main")
    (tramp-rpc-magit-test--write "conflict.txt" "main\n")
    (tramp-rpc-magit-test--commit "main change")
    (dolist (scenario '(("merge" "topic") ("rebase" "main")
                        ("cherry-pick" "main")))
      (unless (equal (car scenario) "merge")
        (tramp-rpc-magit-test--git "checkout" "topic"))
      (with-temp-buffer
        (should (= (apply #'process-file "git" nil t nil scenario) 1)))
      (let ((pair (tramp-rpc-magit-test--parity (concat (car scenario) "-conflict"))))
        (should (string-match-p "conflict.txt" (plist-get (cdr pair) :result))))
      (tramp-rpc-magit-test--git (car scenario) "--abort"))))

(ert-deftest tramp-rpc-magit-test-fetch-push-and-upstream ()
  (tramp-rpc-magit-test--with-repo
    (tramp-rpc-magit-test--init)
    (tramp-rpc-magit-test--write "tracked.txt" "base\n")
    (tramp-rpc-magit-test--commit "initial")
    (let ((origin (expand-file-name "origin.git" default-directory)))
      (tramp-rpc-magit-test--git "init" "--bare" "-b" "main" "origin.git")
      (tramp-rpc-magit-test--git "remote" "add" "origin" (file-local-name origin))
      (tramp-rpc-magit-test--git "push" "-u" "origin" "main")
      (tramp-rpc-magit-test--git "remote" "set-head" "origin" "main")
      (tramp-rpc-magit-test--git "config" "remote.pushDefault" "origin")
      ;; Keep the embedded bare fixture out of Magit's untracked section.
      (tramp-rpc-magit-test--write ".git/info/exclude" "origin.git/\n")
      (tramp-rpc-magit-test--write "tracked.txt" "ahead\n")
      (tramp-rpc-magit-test--commit "ahead of upstream")
      (tramp-rpc-magit-test--parity "ahead-upstream")
      (with-current-buffer (magit-get-mode-buffer 'magit-status-mode)
        (tramp-rpc-magit-test--wait (magit-run-git-async "push" "origin" "main"))
        (tramp-rpc-magit-test--wait (magit-run-git-async "fetch" "origin")))
      (tramp-rpc-magit-test--parity "after-push-fetch")
      (should (equal (tramp-rpc-magit-test--git "rev-parse" "HEAD")
                     (tramp-rpc-magit-test--git "rev-parse" "origin/main"))))))

(ert-deftest tramp-rpc-magit-test-many-files-and-custom-diff ()
  "Cross the 200-command batch boundary and exercise non-default diff argv."
  (tramp-rpc-magit-test--with-repo
    (tramp-rpc-magit-test--init)
    (dotimes (i 55)
      (tramp-rpc-magit-test--write (format "file-%02d.txt" i) "base\n"))
    (tramp-rpc-magit-test--commit "many files")
    (dotimes (i 55)
      (tramp-rpc-magit-test--write (format "file-%02d.txt" i) "changed\n"))
    (let* ((pair (tramp-rpc-magit-test--parity "55-files"))
           (fast (cdr pair)))
      (should (>= (plist-get fast :batches) 3)))
    (let ((old (get 'magit-status-mode 'magit-diff-current-arguments)))
      (unwind-protect
          (progn
            (put 'magit-status-mode 'magit-diff-current-arguments
                 '("--no-ext-diff" "--ignore-space-change" "--histogram"))
            (tramp-rpc-magit-test--parity "custom-diff"))
        (if old (put 'magit-status-mode 'magit-diff-current-arguments old)
          (cl-remprop 'magit-status-mode 'magit-diff-current-arguments))))))

(ert-deftest tramp-rpc-magit-test-alternate-index-and-semantic-flags ()
  "A real alternate index and semantic prefixes must bypass the snapshot."
  (tramp-rpc-magit-test--with-repo
    (tramp-rpc-magit-test--rich-fixture)
    (tramp-rpc-magit-test--status t t)
    (let ((tramp-rpc-magit--allow-process-cache t)
          (process-file-side-effects nil))
      (let ((process-environment
             (cons (concat "GIT_INDEX_FILE="
                           (file-local-name (expand-file-name "alternate-index")))
                   process-environment)))
        (should (zerop (process-file "git" nil nil nil "read-tree" "--empty")))
        (let ((row (tramp-rpc-magit-test--measure
                    "alternate-index"
                    (lambda () (magit-git-string "ls-files" "-z" "--full-name")))))
          (should-not (plist-get row :result))
          (should (= (plist-get row :hits) 0))
          (should (= (plist-get row :process-run) 1))))
      (dolist (prefix '(("-c" "diff.noprefix=true") ("--glob-pathspecs")
                        ("-C" ".")))
        (let ((args (append prefix '("diff" "--stat" "--no-color"))))
          (let ((row (tramp-rpc-magit-test--measure
                      (format "semantic-prefix/%S" prefix)
                      (lambda ()
                        (with-temp-buffer
                          (should (zerop (apply #'process-file "git" nil t nil args)))
                          (buffer-string))))))
            (should (= (plist-get row :hits) 0))
            (should (= (plist-get row :process-run) 1))
            (let ((tramp-rpc-magit--allow-process-cache nil))
              (with-temp-buffer
                (should (zerop (apply #'process-file "git" nil t nil args)))
                (should (equal (plist-get row :result) (buffer-string)))))))))))

(ert-deftest tramp-rpc-magit-test-cached-error-preserves-stderr ()
  "A cache hit must preserve Git diagnostics when output is merged."
  (tramp-rpc-magit-test--with-repo
    (tramp-rpc-magit-test--init)
    (tramp-rpc-magit-test--write "tracked.txt" "base\n")
    (tramp-rpc-magit-test--commit "initial")
    (tramp-rpc-magit-test--status t t)
    (let ((process-file-side-effects nil)
          (args '("rev-parse" "--abbrev-ref" "@{upstream}"))
          outputs)
      (dolist (cached '(nil t))
        (let ((tramp-rpc-magit--allow-process-cache cached))
          (with-temp-buffer
            (should (= (apply #'process-file "git" nil t nil args) 128))
            (push (buffer-string) outputs))))
      (should (string-match-p "no upstream configured" (cadr outputs)))
      (should (equal (car outputs) (cadr outputs))))))

(ert-deftest tramp-rpc-magit-test-timing-samples ()
  "Interleave repeated plain/optimized runs; do not assert wall-clock speed."
  (tramp-rpc-magit-test--with-repo
    (tramp-rpc-magit-test--rich-fixture)
    (dotimes (i 5)
      (tramp-rpc-magit-test--parity (format "sample-%d" i)))))

(ert-deftest tramp-rpc-magit-test-external-change-invalidates-snapshot ()
  "An external SSH write invalidates the snapshot via real fs.events."
  (tramp-rpc-magit-test--with-repo
    (tramp-rpc-magit-test--init)
    (tramp-rpc-magit-test--write "tracked.txt" "base\n")
    (tramp-rpc-magit-test--commit "initial")
    (tramp-rpc-magit-test--status t t)
    (should (tramp-rpc-magit--get-process-cache))
    (let ((file (file-local-name (expand-file-name "tracked.txt")))
          (default-directory temporary-file-directory))
      (should (zerop
               (process-file "ssh" nil nil nil "-o" "BatchMode=yes"
                             (or (getenv "TRAMP_RPC_TEST_HOST") "localhost")
                             (concat "printf 'external change\\n' > "
                                     (shell-quote-argument file))))))
    (let ((deadline (+ (float-time) 10)))
      (while (and (tramp-rpc-magit--get-process-cache) (< (float-time) deadline))
        (accept-process-output nil 0.02))
      (should-not (tramp-rpc-magit--get-process-cache)))
    (with-current-buffer (magit-get-mode-buffer 'magit-status-mode)
      (magit-refresh)
      (should (string-match-p "external change" (tramp-rpc-magit-test--expand))))))

(provide 'tramp-rpc-magit-tests)
;;; tramp-rpc-magit-tests.el ends here
