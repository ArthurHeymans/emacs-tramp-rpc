;;; benchmark-magit.el --- Benchmark magit-like operations via TRAMP -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Arthur

;;; Commentary:

;; This file benchmarks magit-like git operations comparing TRAMP RPC vs SSH.
;; Tests operations that magit performs frequently.

;;; Code:

(require 'tramp)
(require 'vc)
(require 'vc-git)

;; Load tramp-rpc if available
(when load-file-name
  (let ((lisp-dir (expand-file-name "lisp" (file-name-directory load-file-name))))
    (add-to-list 'load-path lisp-dir)
    (require 'tramp-rpc nil t)))

;;; Configuration

(defvar tramp-rpc-magit-benchmark-host "x220-nixos"
  "Host to use for benchmarking.")

(defvar tramp-rpc-magit-benchmark-repo "/home/arthur/src/linux"
  "Remote git repository to use for tests.")

(defvar tramp-rpc-magit-benchmark-iterations 3
  "Number of iterations for each benchmark.")

(defvar tramp-rpc-magit-benchmark-results nil
  "Results storage.")

;;; Utility functions

(defun tramp-rpc-magit--make-path (method &optional file)
  "Make a TRAMP path for METHOD to repo, optionally with FILE."
  (let ((path (if file
                  (concat tramp-rpc-magit-benchmark-repo "/" file)
                tramp-rpc-magit-benchmark-repo)))
    (format "/%s:%s:%s" method tramp-rpc-magit-benchmark-host path)))

(defmacro tramp-rpc-magit--time (&rest body)
  "Execute BODY and return elapsed time in seconds."
  `(let ((start (current-time)))
     ,@body
     (float-time (time-subtract (current-time) start))))

(defun tramp-rpc-magit--run-n-times (n func)
  "Run FUNC N times and return list of execution times."
  (let (times)
    (dotimes (_ n)
      (push (funcall func) times))
    (nreverse times)))

(defun tramp-rpc-magit--stats (times)
  "Calculate statistics for TIMES list."
  (when times
    (let* ((n (length times))
           (sum (apply #'+ times))
           (mean (/ sum n))
           (sorted (sort (copy-sequence times) #'<))
           (median (nth (/ n 2) sorted))
           (min (car sorted))
           (max (car (last sorted))))
      (list :mean mean :median median :min min :max max :n n))))

(defun tramp-rpc-magit--record (name method times)
  "Record benchmark TIMES for NAME and METHOD."
  (let ((existing (assoc name tramp-rpc-magit-benchmark-results)))
    (if existing
        (setcdr existing (cons (cons method times) (cdr existing)))
      (push (cons name (list (cons method times))) tramp-rpc-magit-benchmark-results))))

(defun tramp-rpc-magit--flush-cache (path)
  "Flush TRAMP cache for PATH."
  (when (tramp-tramp-file-p path)
    (with-parsed-tramp-file-name path nil
      (tramp-flush-file-properties v localname)
      (tramp-flush-directory-properties v (file-name-directory localname)))))

;;; Benchmark implementations

(defun tramp-rpc-magit--git-status (method)
  "Benchmark git status for METHOD."
  (let ((default-directory (tramp-rpc-magit--make-path method)))
    (tramp-rpc-magit--time
     (with-temp-buffer
       (process-file "git" nil t nil "status" "--porcelain")))))

(defun tramp-rpc-magit--git-log-short (method)
  "Benchmark short git log for METHOD."
  (let ((default-directory (tramp-rpc-magit--make-path method)))
    (tramp-rpc-magit--time
     (with-temp-buffer
       (process-file "git" nil t nil "log" "--oneline" "-20")))))

(defun tramp-rpc-magit--git-log-full (method)
  "Benchmark full git log for METHOD."
  (let ((default-directory (tramp-rpc-magit--make-path method)))
    (tramp-rpc-magit--time
     (with-temp-buffer
       (process-file "git" nil t nil "log" "--format=fuller" "-10")))))

(defun tramp-rpc-magit--git-branch (method)
  "Benchmark git branch for METHOD."
  (let ((default-directory (tramp-rpc-magit--make-path method)))
    (tramp-rpc-magit--time
     (with-temp-buffer
       (process-file "git" nil t nil "branch" "-a")))))

(defun tramp-rpc-magit--git-diff-stat (method)
  "Benchmark git diff --stat for METHOD."
  (let ((default-directory (tramp-rpc-magit--make-path method)))
    (tramp-rpc-magit--time
     (with-temp-buffer
       (process-file "git" nil t nil "diff" "--stat" "HEAD~5..HEAD")))))

(defun tramp-rpc-magit--git-show (method)
  "Benchmark git show for METHOD."
  (let ((default-directory (tramp-rpc-magit--make-path method)))
    (tramp-rpc-magit--time
     (with-temp-buffer
       (process-file "git" nil t nil "show" "--stat" "HEAD")))))

(defun tramp-rpc-magit--git-rev-parse (method)
  "Benchmark git rev-parse for METHOD."
  (let ((default-directory (tramp-rpc-magit--make-path method)))
    (tramp-rpc-magit--time
     (with-temp-buffer
       (process-file "git" nil t nil "rev-parse" "HEAD")))))

(defun tramp-rpc-magit--git-ls-files (method)
  "Benchmark git ls-files for METHOD."
  (let ((default-directory (tramp-rpc-magit--make-path method)))
    (tramp-rpc-magit--time
     (with-temp-buffer
       (process-file "git" nil t nil "ls-files" "--" ".")))))

(defun tramp-rpc-magit--vc-state (method)
  "Benchmark vc-state for METHOD."
  (let* ((file (tramp-rpc-magit--make-path method "Makefile")))
    (tramp-rpc-magit--flush-cache file)
    (tramp-rpc-magit--time
     (vc-state file))))

(defun tramp-rpc-magit--vc-git-root (method)
  "Benchmark vc-git-root for METHOD."
  (let ((file (tramp-rpc-magit--make-path method "Makefile")))
    (tramp-rpc-magit--time
     (vc-git-root file))))

(defun tramp-rpc-magit--file-attributes-makefile (method)
  "Benchmark file-attributes on Makefile for METHOD."
  (let ((file (tramp-rpc-magit--make-path method "Makefile")))
    (tramp-rpc-magit--flush-cache file)
    (tramp-rpc-magit--time
     (file-attributes file))))

(defun tramp-rpc-magit--directory-files-root (method)
  "Benchmark directory-files on repo root for METHOD."
  (let ((dir (tramp-rpc-magit--make-path method)))
    (tramp-rpc-magit--flush-cache dir)
    (tramp-rpc-magit--time
     (directory-files dir))))

(defun tramp-rpc-magit--multiple-git-commands (method)
  "Benchmark multiple git commands (like magit-refresh) for METHOD."
  (let ((default-directory (tramp-rpc-magit--make-path method)))
    (tramp-rpc-magit--time
     (progn
       ;; Simulate what magit does on refresh
       (with-temp-buffer (process-file "git" nil t nil "rev-parse" "--show-toplevel"))
       (with-temp-buffer (process-file "git" nil t nil "rev-parse" "HEAD"))
       (with-temp-buffer (process-file "git" nil t nil "symbolic-ref" "--short" "HEAD"))
       (with-temp-buffer (process-file "git" nil t nil "status" "--porcelain" "-z"))
       (with-temp-buffer (process-file "git" nil t nil "stash" "list"))))))

(defun tramp-rpc-magit--connection-cold (method)
  "Benchmark cold connection (no existing connection) for METHOD."
  (tramp-cleanup-all-connections)
  (let ((default-directory (tramp-rpc-magit--make-path method)))
    (tramp-rpc-magit--time
     (with-temp-buffer
       (process-file "git" nil t nil "rev-parse" "HEAD")))))

;;; Main benchmark runner

(defconst tramp-rpc-magit--tests
  '(("cold-connection"    . tramp-rpc-magit--connection-cold)
    ("git-rev-parse"      . tramp-rpc-magit--git-rev-parse)
    ("git-status"         . tramp-rpc-magit--git-status)
    ("git-branch"         . tramp-rpc-magit--git-branch)
    ("git-log-short"      . tramp-rpc-magit--git-log-short)
    ("git-log-full"       . tramp-rpc-magit--git-log-full)
    ("git-diff-stat"      . tramp-rpc-magit--git-diff-stat)
    ("git-show"           . tramp-rpc-magit--git-show)
    ("git-ls-files"       . tramp-rpc-magit--git-ls-files)
    ("vc-state"           . tramp-rpc-magit--vc-state)
    ("vc-git-root"        . tramp-rpc-magit--vc-git-root)
    ("file-attrs"         . tramp-rpc-magit--file-attributes-makefile)
    ("dir-files"          . tramp-rpc-magit--directory-files-root)
    ("magit-refresh-sim"  . tramp-rpc-magit--multiple-git-commands))
  "Alist of magit benchmark tests.")

(defun tramp-rpc-magit--run-method (method)
  "Run all benchmarks for METHOD."
  (message "Testing %s method..." method)
  
  ;; Warm up connection
  (message "  Warming up connection...")
  (let ((default-directory (tramp-rpc-magit--make-path method)))
    (with-temp-buffer
      (process-file "git" nil t nil "rev-parse" "HEAD")))
  
  (dolist (test tramp-rpc-magit--tests)
    (let* ((name (car test))
           (func (cdr test))
           (is-cold-test (string= name "cold-connection")))
      (message "  Running %s..." name)
      (condition-case err
          (let ((times (tramp-rpc-magit--run-n-times
                        tramp-rpc-magit-benchmark-iterations
                        (lambda ()
                          (when is-cold-test
                            (tramp-cleanup-all-connections))
                          (funcall func method)))))
            (tramp-rpc-magit--record name method times)
            ;; Restore connection after cold test
            (when is-cold-test
              (let ((default-directory (tramp-rpc-magit--make-path method)))
                (with-temp-buffer
                  (process-file "git" nil t nil "rev-parse" "HEAD")))))
        (error
         (message "    ERROR: %s" err)))))
  
  (tramp-cleanup-all-connections))

(defun tramp-rpc-magit--format-time (secs)
  "Format SECS as a human-readable time."
  (cond
   ((null secs) "N/A")
   ((< secs 0.001) (format "%6.2f us" (* secs 1000000)))
   ((< secs 1.0)   (format "%6.2f ms" (* secs 1000)))
   (t              (format "%6.2f s " secs))))

(defun tramp-rpc-magit--report ()
  "Generate and display benchmark report."
  (with-current-buffer (get-buffer-create "*TRAMP Magit Benchmark*")
    (erase-buffer)
    (insert "TRAMP RPC vs SSH - Magit-like Operations Benchmark\n")
    (insert "===================================================\n\n")
    (insert (format "Host: %s\n" tramp-rpc-magit-benchmark-host))
    (insert (format "Repo: %s\n" tramp-rpc-magit-benchmark-repo))
    (insert (format "Iterations: %d\n\n" tramp-rpc-magit-benchmark-iterations))
    
    (insert (format "%-20s | %-14s | %-14s | %-10s\n"
                    "Test" "RPC (median)" "SSH (median)" "Speedup"))
    (insert (make-string 65 ?-) "\n")
    
    (dolist (test tramp-rpc-magit--tests)
      (let* ((name (car test))
             (results (cdr (assoc name tramp-rpc-magit-benchmark-results)))
             (rpc-times (cdr (assoc "rpc" results)))
             (ssh-times (or (cdr (assoc "sshx" results))
                            (cdr (assoc "ssh" results))))
             (rpc-stats (tramp-rpc-magit--stats rpc-times))
             (ssh-stats (tramp-rpc-magit--stats ssh-times)))
        (if (and rpc-stats ssh-stats)
            (let* ((rpc-median (plist-get rpc-stats :median))
                   (ssh-median (plist-get ssh-stats :median))
                   (speedup (if (and rpc-median ssh-median (> rpc-median 0))
                                (/ ssh-median rpc-median)
                              0)))
              (insert (format "%-20s | %14s | %14s | %7.2fx\n"
                              name
                              (tramp-rpc-magit--format-time rpc-median)
                              (tramp-rpc-magit--format-time ssh-median)
                              speedup)))
          (insert (format "%-20s | %14s | %14s | %10s\n"
                          name
                          (tramp-rpc-magit--format-time (plist-get rpc-stats :median))
                          (tramp-rpc-magit--format-time (plist-get ssh-stats :median))
                          "N/A")))))
    
    (insert "\n\nDetailed Statistics\n")
    (insert "-------------------\n")
    
    (dolist (test tramp-rpc-magit--tests)
      (let* ((name (car test))
             (results (cdr (assoc name tramp-rpc-magit-benchmark-results))))
        (insert (format "\n%s:\n" name))
        (dolist (method-result results)
          (let* ((method (car method-result))
                 (times (cdr method-result))
                 (stats (tramp-rpc-magit--stats times)))
            (when stats
              (insert (format "  %s: mean=%s median=%s min=%s max=%s\n"
                              method
                              (tramp-rpc-magit--format-time (plist-get stats :mean))
                              (tramp-rpc-magit--format-time (plist-get stats :median))
                              (tramp-rpc-magit--format-time (plist-get stats :min))
                              (tramp-rpc-magit--format-time (plist-get stats :max)))))))))
    
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;;###autoload
(defun tramp-rpc-magit-benchmark-run ()
  "Run magit-like benchmarks comparing TRAMP RPC vs SSH."
  (interactive)
  (setq tramp-rpc-magit-benchmark-results nil)
  
  (message "Starting magit-like benchmarks on %s:%s..."
           tramp-rpc-magit-benchmark-host
           tramp-rpc-magit-benchmark-repo)
  
  (dolist (method '("sshx" "rpc"))
    (message "\n=== Benchmarking %s ===" method)
    (condition-case err
        (tramp-rpc-magit--run-method method)
      (error
       (message "Error with %s: %s" method err))))
  
  (tramp-rpc-magit--report)
  (message "\nBenchmarks complete! See *TRAMP Magit Benchmark* buffer."))

;;;###autoload  
(defun tramp-rpc-magit-test-encoding ()
  "Test that text encoding optimization is working.
This shows the output size and encoding used for common git commands."
  (interactive)
  (let ((default-directory (tramp-rpc-magit--make-path "rpc")))
    (with-parsed-tramp-file-name default-directory nil
      (with-current-buffer (get-buffer-create "*TRAMP Encoding Test*")
        (erase-buffer)
        (insert "Testing output encoding optimization\n")
        (insert "=====================================\n\n")
        
        ;; Test a few commands and show encoding
        (dolist (cmd '(("git" "rev-parse" "HEAD")
                       ("git" "status" "--porcelain")
                       ("git" "log" "--oneline" "-5")
                       ("echo" "hello world")))
          (let* ((start (current-time))
                 (result (tramp-rpc--call v "process.run"
                           `((cmd . ,(car cmd))
                             (args . ,(vconcat (cdr cmd)))
                             (cwd . ,localname))))
                 (elapsed (float-time (time-subtract (current-time) start)))
                 (stdout (alist-get 'stdout result))
                 (encoding (alist-get 'stdout_encoding result))
                 (exit-code (alist-get 'exit_code result)))
            (insert (format "Command: %s\n" (string-join cmd " ")))
            (insert (format "  Encoding: %s\n" encoding))
            (insert (format "  Output size: %d bytes\n" (length stdout)))
            (insert (format "  Exit code: %d\n" exit-code))
            (insert (format "  Time: %.2f ms\n\n" (* elapsed 1000)))))
        
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

(provide 'benchmark-magit)
;;; benchmark-magit.el ends here
