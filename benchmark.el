;;; benchmark.el --- Benchmark TRAMP RPC vs TRAMP SSH -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Arthur

;;; Commentary:

;; This file provides benchmarks comparing TRAMP RPC against TRAMP SSH(x).
;; Run with: emacs -Q -l benchmark.el -f tramp-rpc-benchmark-run
;;
;; Or interactively:
;;   M-x tramp-rpc-benchmark-run
;;
;; Configuration:
;;   Set `tramp-rpc-benchmark-host' to your test host before running.

;;; Code:

(require 'tramp)

;; Load tramp-rpc if available
(let ((lisp-dir (expand-file-name "lisp" (file-name-directory load-file-name))))
  (add-to-list 'load-path lisp-dir)
  (require 'tramp-rpc nil t))

;;; Configuration

(defvar tramp-rpc-benchmark-host "x220-nixos"
  "Host to use for benchmarking.")

(defvar tramp-rpc-benchmark-user nil
  "User for remote host. If nil, uses default.")

(defvar tramp-rpc-benchmark-iterations 5
  "Number of iterations for each benchmark.")

(defvar tramp-rpc-benchmark-test-dir "/tmp/tramp-benchmark"
  "Remote directory to use for tests (will be created/cleaned).")

;;; Results storage

(defvar tramp-rpc-benchmark-results nil
  "Alist of benchmark results: ((name . ((method . times) ...)) ...)")

;;; Utility functions

(defun tramp-rpc-benchmark--make-path (method &optional file)
  "Make a TRAMP path for METHOD to test dir, optionally with FILE."
  (let ((user-part (if tramp-rpc-benchmark-user
                       (concat tramp-rpc-benchmark-user "@")
                     ""))
        (path (if file
                  (concat tramp-rpc-benchmark-test-dir "/" file)
                tramp-rpc-benchmark-test-dir)))
    (format "/%s:%s%s:%s" method user-part tramp-rpc-benchmark-host path)))

(defmacro tramp-rpc-benchmark--time (&rest body)
  "Execute BODY and return elapsed time in seconds."
  `(let ((start (current-time)))
     ,@body
     (float-time (time-subtract (current-time) start))))

(defun tramp-rpc-benchmark--run-n-times (n func)
  "Run FUNC N times and return list of execution times."
  (let (times)
    (dotimes (_ n)
      (push (funcall func) times))
    (nreverse times)))

(defun tramp-rpc-benchmark--stats (times)
  "Calculate statistics for TIMES list."
  (let* ((n (length times))
         (sum (apply #'+ times))
         (mean (/ sum n))
         (sorted (sort (copy-sequence times) #'<))
         (median (if (oddp n)
                     (nth (/ n 2) sorted)
                   (/ (+ (nth (/ n 2) sorted)
                         (nth (1- (/ n 2)) sorted))
                      2.0)))
         (min (car sorted))
         (max (car (last sorted))))
    (list :mean mean :median median :min min :max max :n n)))

(defun tramp-rpc-benchmark--record (name method times)
  "Record benchmark TIMES for NAME and METHOD."
  (let ((existing (assoc name tramp-rpc-benchmark-results)))
    (if existing
        (setcdr existing (cons (cons method times) (cdr existing)))
      (push (cons name (list (cons method times))) tramp-rpc-benchmark-results))))

;;; Setup and teardown

(defun tramp-rpc-benchmark--setup (method)
  "Set up test environment for METHOD."
  (let ((dir (tramp-rpc-benchmark--make-path method)))
    ;; Clear any existing tramp connections
    (tramp-cleanup-all-connections)
    ;; Create test directory
    (ignore-errors (delete-directory dir t))
    (make-directory dir t)
    ;; Create some test files (use write-region directly for RPC compatibility)
    (dotimes (i 10)
      (let ((file (tramp-rpc-benchmark--make-path method (format "file%d.txt" i))))
        (with-temp-buffer
          (insert (format "Test file %d\n" i))
          (insert (make-string 1000 ?x))
          (write-region (point-min) (point-max) file))))))

(defun tramp-rpc-benchmark--teardown (method)
  "Clean up test environment for METHOD."
  (let ((dir (tramp-rpc-benchmark--make-path method)))
    (ignore-errors (delete-directory dir t))
    (tramp-cleanup-all-connections)))

;;; Benchmark implementations

(defun tramp-rpc-benchmark--flush-cache (file)
  "Flush TRAMP cache for FILE."
  (when (tramp-tramp-file-p file)
    (with-parsed-tramp-file-name file nil
      (tramp-flush-file-properties v localname)
      (tramp-flush-directory-properties v (file-name-directory localname)))))

(defun tramp-rpc-benchmark--file-exists (method)
  "Benchmark file-exists-p for METHOD."
  (let ((file (tramp-rpc-benchmark--make-path method "file0.txt")))
    (tramp-rpc-benchmark--flush-cache file)
    (tramp-rpc-benchmark--time
     (file-exists-p file))))

(defun tramp-rpc-benchmark--file-attributes (method)
  "Benchmark file-attributes for METHOD."
  (let ((file (tramp-rpc-benchmark--make-path method "file0.txt")))
    (tramp-rpc-benchmark--flush-cache file)
    (tramp-rpc-benchmark--time
     (file-attributes file))))

(defun tramp-rpc-benchmark--file-read (method)
  "Benchmark reading a file for METHOD."
  (let ((file (tramp-rpc-benchmark--make-path method "file0.txt")))
    (tramp-rpc-benchmark--time
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-string)))))

(defun tramp-rpc-benchmark--file-write (method)
  "Benchmark writing a file for METHOD."
  (let ((file (tramp-rpc-benchmark--make-path method "test-write.txt"))
        ;; Use smaller size to avoid buffer issues with RPC
        (content (make-string 1000 ?y)))
    (tramp-rpc-benchmark--time
     (with-temp-buffer
       (insert content)
       (write-region (point-min) (point-max) file)))))

(defun tramp-rpc-benchmark--directory-files (method)
  "Benchmark directory-files for METHOD."
  (let ((dir (tramp-rpc-benchmark--make-path method)))
    (tramp-rpc-benchmark--flush-cache dir)
    (tramp-rpc-benchmark--time
     (directory-files dir))))

(defun tramp-rpc-benchmark--directory-files-and-attributes (method)
  "Benchmark directory-files-and-attributes for METHOD."
  (let ((dir (tramp-rpc-benchmark--make-path method)))
    (tramp-rpc-benchmark--flush-cache dir)
    (tramp-rpc-benchmark--time
     (directory-files-and-attributes dir))))

(defun tramp-rpc-benchmark--process-file (method)
  "Benchmark process-file (running a command) for METHOD."
  (let ((default-directory (tramp-rpc-benchmark--make-path method)))
    (tramp-rpc-benchmark--time
     (with-temp-buffer
       (process-file "ls" nil t nil "-la")))))

(defun tramp-rpc-benchmark--process-file-cat (method)
  "Benchmark process-file with cat for METHOD."
  (let ((default-directory (tramp-rpc-benchmark--make-path method)))
    (tramp-rpc-benchmark--time
     (with-temp-buffer
       (process-file "cat" nil t nil "file0.txt")))))

(defun tramp-rpc-benchmark--copy-file (method)
  "Benchmark copy-file for METHOD."
  (let ((src (tramp-rpc-benchmark--make-path method "file0.txt"))
        (dst (tramp-rpc-benchmark--make-path method "file0-copy.txt")))
    (tramp-rpc-benchmark--time
     (progn
       (ignore-errors (delete-file dst))
       (copy-file src dst)))))

(defun tramp-rpc-benchmark--multiple-stats (method)
  "Benchmark multiple file-attributes calls (simulating dired) for METHOD."
  ;; Flush cache for all files first
  (dotimes (i 10)
    (tramp-rpc-benchmark--flush-cache
     (tramp-rpc-benchmark--make-path method (format "file%d.txt" i))))
  (tramp-rpc-benchmark--time
   (dotimes (i 10)
     (file-attributes
      (tramp-rpc-benchmark--make-path method (format "file%d.txt" i))))))

(defun tramp-rpc-benchmark--connection-setup (method)
  "Benchmark initial connection setup for METHOD."
  (tramp-cleanup-all-connections)
  (let ((file (tramp-rpc-benchmark--make-path method "file0.txt")))
    (tramp-rpc-benchmark--time
     (file-exists-p file))))

;;; Main benchmark runner

(defconst tramp-rpc-benchmark--tests
  '(("connection-setup"   . tramp-rpc-benchmark--connection-setup)
    ("file-exists"        . tramp-rpc-benchmark--file-exists)
    ("file-attributes"    . tramp-rpc-benchmark--file-attributes)
    ("file-read"          . tramp-rpc-benchmark--file-read)
    ("file-write"         . tramp-rpc-benchmark--file-write)
    ("directory-files"    . tramp-rpc-benchmark--directory-files)
    ("dir-files-attrs"    . tramp-rpc-benchmark--directory-files-and-attributes)
    ("process-file-ls"    . tramp-rpc-benchmark--process-file)
    ("process-file-cat"   . tramp-rpc-benchmark--process-file-cat)
    ("copy-file"          . tramp-rpc-benchmark--copy-file)
    ("multi-stat"         . tramp-rpc-benchmark--multiple-stats))
  "Alist of benchmark tests (name . function).")

(defun tramp-rpc-benchmark--run-method (method)
  "Run all benchmarks for METHOD."
  (message "Setting up for %s..." method)
  (tramp-rpc-benchmark--setup method)
  
  ;; Warm up the connection
  (message "Warming up connection for %s..." method)
  (file-exists-p (tramp-rpc-benchmark--make-path method "file0.txt"))
  
  (dolist (test tramp-rpc-benchmark--tests)
    (let* ((name (car test))
           (func (cdr test))
           (is-connection-test (string= name "connection-setup")))
      (message "  Running %s for %s (%d iterations)..."
               name method tramp-rpc-benchmark-iterations)
      (condition-case err
          (let ((times (tramp-rpc-benchmark--run-n-times
                        tramp-rpc-benchmark-iterations
                        (lambda ()
                          (when is-connection-test
                            (tramp-cleanup-all-connections))
                          (funcall func method)))))
            (tramp-rpc-benchmark--record name method times))
        (error
         (message "    ERROR in %s/%s: %s" method name err)))))
  
  (message "Cleaning up for %s..." method)
  (tramp-rpc-benchmark--teardown method))

(defun tramp-rpc-benchmark--format-time (secs)
  "Format SECS as a human-readable time."
  (cond
   ((< secs 0.001) (format "%6.2f us" (* secs 1000000)))
   ((< secs 1.0)   (format "%6.2f ms" (* secs 1000)))
   (t              (format "%6.2f s " secs))))

(defun tramp-rpc-benchmark--report ()
  "Generate and display benchmark report."
  (with-current-buffer (get-buffer-create "*TRAMP Benchmark Results*")
    (erase-buffer)
    (insert "TRAMP RPC vs SSH Benchmark Results\n")
    (insert "===================================\n\n")
    (insert (format "Host: %s\n" tramp-rpc-benchmark-host))
    (insert (format "Iterations: %d\n\n" tramp-rpc-benchmark-iterations))
    
    (insert (format "%-20s | %-14s | %-14s | %-10s\n"
                    "Test" "RPC (median)" "SSH (median)" "Speedup"))
    (insert (make-string 65 ?-) "\n")
    
    (dolist (test tramp-rpc-benchmark--tests)
      (let* ((name (car test))
             (results (cdr (assoc name tramp-rpc-benchmark-results)))
             (rpc-times (cdr (assoc "rpc" results)))
             (ssh-times (or (cdr (assoc "sshx" results))
                            (cdr (assoc "ssh" results))))
             (rpc-stats (when rpc-times (tramp-rpc-benchmark--stats rpc-times)))
             (ssh-stats (when ssh-times (tramp-rpc-benchmark--stats ssh-times))))
        (when (and rpc-stats ssh-stats)
          (let* ((rpc-median (plist-get rpc-stats :median))
                 (ssh-median (plist-get ssh-stats :median))
                 (speedup (/ ssh-median rpc-median)))
            (insert (format "%-20s | %14s | %14s | %7.2fx\n"
                            name
                            (tramp-rpc-benchmark--format-time rpc-median)
                            (tramp-rpc-benchmark--format-time ssh-median)
                            speedup))))))
    
    (insert "\n\nDetailed Statistics\n")
    (insert "-------------------\n\n")
    
    (dolist (test tramp-rpc-benchmark--tests)
      (let* ((name (car test))
             (results (cdr (assoc name tramp-rpc-benchmark-results))))
        (insert (format "\n%s:\n" name))
        (dolist (method-result results)
          (let* ((method (car method-result))
                 (times (cdr method-result))
                 (stats (tramp-rpc-benchmark--stats times)))
            (insert (format "  %s: mean=%s median=%s min=%s max=%s\n"
                            method
                            (tramp-rpc-benchmark--format-time (plist-get stats :mean))
                            (tramp-rpc-benchmark--format-time (plist-get stats :median))
                            (tramp-rpc-benchmark--format-time (plist-get stats :min))
                            (tramp-rpc-benchmark--format-time (plist-get stats :max))))))))
    
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;;###autoload
(defun tramp-rpc-benchmark-run ()
  "Run TRAMP RPC vs SSH benchmarks."
  (interactive)
  (setq tramp-rpc-benchmark-results nil)
  
  (message "Starting TRAMP benchmarks on %s..." tramp-rpc-benchmark-host)
  (message "This may take a few minutes...\n")
  
  ;; Run benchmarks for each method
  (dolist (method '("sshx" "rpc"))
    (message "\n=== Benchmarking %s ===\n" method)
    (condition-case err
        (tramp-rpc-benchmark--run-method method)
      (error
       (message "Error running benchmarks for %s: %s" method err))))
  
  ;; Display results
  (tramp-rpc-benchmark--report)
  (message "\nBenchmarks complete! See *TRAMP Benchmark Results* buffer."))

;;;###autoload
(defun tramp-rpc-benchmark-quick ()
  "Run a quick benchmark (fewer iterations)."
  (interactive)
  (let ((tramp-rpc-benchmark-iterations 3))
    (tramp-rpc-benchmark-run)))

(provide 'benchmark)
;;; benchmark.el ends here
