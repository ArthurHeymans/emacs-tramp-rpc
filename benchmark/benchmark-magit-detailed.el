;;; benchmark-magit-detailed.el --- Detailed magit benchmarking for tramp-rpc -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;;; Commentary:

;; Detailed benchmarking to understand magit performance on remote servers
;; and investigate batching opportunities.

;;; Code:

(require 'tramp)

;; Load tramp-rpc if available
(when load-file-name
  (let ((lisp-dir (expand-file-name "../lisp" (file-name-directory load-file-name))))
    (add-to-list 'load-path lisp-dir)
    (require 'tramp-rpc nil t)
    (require 'tramp-rpc-protocol nil t)))

;; Silence byte-compiler warnings
(declare-function tramp-rpc--call "tramp-rpc")
(declare-function tramp-rpc--call-pipelined "tramp-rpc")
(declare-function tramp-rpc-run-git-commands "tramp-rpc")
(declare-function tramp-rpc-magit--prefetch-git-commands "tramp-rpc-magit")
(declare-function tramp-rpc--decode-output "tramp-rpc")
(declare-function tramp-rpc-magit-enable "tramp-rpc-magit")
(declare-function tramp-rpc-magit-disable "tramp-rpc-magit")
(declare-function magit-status-setup-buffer "magit-status")
(declare-function magit-get-mode-buffer "magit-mode")

;;; Configuration

(defvar tramp-rpc-magit-bench-remote "/rpc:x220-nixos:/home/arthur/"
  "Remote directory for benchmarking (must be a git repo).")

(defvar tramp-rpc-magit-bench-iterations 3
  "Number of iterations for each benchmark.")

;;; Utilities

(defmacro tramp-rpc-magit-bench--time (&rest body)
  "Execute BODY and return elapsed time in seconds."
  `(let ((start (current-time)))
     ,@body
     (float-time (time-subtract (current-time) start))))

(defvar tramp-rpc-magit-bench--process-file-log nil
  "Log of process-file calls during benchmarking.")

(defun tramp-rpc-magit-bench--log-process-file (orig-fun program &rest args)
  "Advice to log process-file calls."
  (let ((start (current-time))
        (result (apply orig-fun program args)))
    (push (list :program program
                :args (cdr args)  ; skip infile, buffer, display
                :time (float-time (time-subtract (current-time) start)))
          tramp-rpc-magit-bench--process-file-log)
    result))

(defun tramp-rpc-magit-bench--format-time (secs)
  "Format SECS as human-readable time."
  (cond
   ((null secs) "N/A")
   ((< secs 0.001) (format "%6.2f us" (* secs 1000000)))
   ((< secs 1.0)   (format "%6.2f ms" (* secs 1000)))
   (t              (format "%6.2f s " secs))))

;;; Benchmark: Current round-trip times

(defun tramp-rpc-magit-bench-roundtrip ()
  "Measure basic round-trip times to the remote server."
  (interactive)
  (let* ((default-directory tramp-rpc-magit-bench-remote)
         (times nil))
    (message "Measuring round-trip times to %s..." tramp-rpc-magit-bench-remote)

    ;; Warm up connection
    (with-temp-buffer
      (process-file "git" nil t nil "rev-parse" "HEAD"))

    ;; Measure multiple round trips
    (dotimes (_ 10)
      (push (tramp-rpc-magit-bench--time
             (with-temp-buffer
               (process-file "git" nil t nil "rev-parse" "HEAD")))
            times))

    (let* ((sorted (sort times #'<))
           (min (car sorted))
           (max (car (last sorted)))
           (mean (/ (apply #'+ times) (length times))))
      (message "Round-trip times (n=%d):" (length times))
      (message "  Min:  %s" (tramp-rpc-magit-bench--format-time min))
      (message "  Max:  %s" (tramp-rpc-magit-bench--format-time max))
      (message "  Mean: %s" (tramp-rpc-magit-bench--format-time mean))
      (list :min min :max max :mean mean))))

;;; Benchmark: Batched vs Sequential

(defun tramp-rpc-magit-bench-compare-batching ()
  "Compare sequential vs batched git command execution."
  (interactive)
  (let ((default-directory tramp-rpc-magit-bench-remote))
    (message "Comparing sequential vs batched execution...")

    ;; Define a set of git commands typical for magit-status
    (let* ((commands '(("rev-parse" "--show-toplevel")
                       ("rev-parse" "HEAD")
                       ("symbolic-ref" "--short" "HEAD")
                       ("log" "-1" "--format=%s" "HEAD")
                       ("rev-parse" "--abbrev-ref" "@{upstream}")
                       ("rev-parse" "--abbrev-ref" "@{push}")
                       ("status" "--porcelain" "-z" "--untracked-files=normal")
                       ("stash" "list" "--format=%gd: %gs")
                       ("config" "--list" "-z")))
           seq-time
           batch-time)

      ;; Time sequential execution (current magit behavior)
      (setq seq-time
            (tramp-rpc-magit-bench--time
             (dolist (cmd commands)
               (with-temp-buffer
                 (ignore-errors
                   (apply #'process-file "git" nil t nil cmd))))))

      ;; Time batched execution using new API
      (setq batch-time
            (tramp-rpc-magit-bench--time
             (tramp-rpc-run-git-commands default-directory commands)))

      (with-current-buffer (get-buffer-create "*Batching Comparison*")
        (erase-buffer)
        (insert "Batching Comparison Results\n")
        (insert "===========================\n\n")
        (insert (format "Remote: %s\n" tramp-rpc-magit-bench-remote))
        (insert (format "Commands: %d\n\n" (length commands)))

        (insert "Commands executed (simulating magit-status refresh):\n")
        (dolist (cmd commands)
          (insert (format "  git %s\n" (string-join cmd " "))))
        (insert "\n")

        (insert (format "Sequential time (current): %s\n"
                        (tramp-rpc-magit-bench--format-time seq-time)))
        (insert (format "Batched time (optimized): %s\n"
                        (tramp-rpc-magit-bench--format-time batch-time)))
        (insert (format "Speedup:                   %.2fx\n"
                        (/ seq-time batch-time)))
        (insert (format "Time saved:                %s\n"
                        (tramp-rpc-magit-bench--format-time (- seq-time batch-time))))

        (goto-char (point-min))
        (display-buffer (current-buffer))))))

(defun tramp-rpc-magit-bench-batched-status ()
  "Benchmark the commands.run_parallel RPC for magit prefetch."
  (interactive)
  (let ((default-directory tramp-rpc-magit-bench-remote))
    (message "Benchmarking parallel command prefetch RPC...")
    (with-parsed-tramp-file-name default-directory nil
      (let* ((commands (tramp-rpc-magit--prefetch-git-commands localname))
             (result nil)
             (rpc-time
              (tramp-rpc-magit-bench--time
               (setq result (tramp-rpc--call v "commands.run_parallel"
                                             `((commands . ,commands)))))))
        (with-current-buffer (get-buffer-create "*RPC Status Results*")
          (erase-buffer)
          (insert "Parallel Command Prefetch Results\n")
          (insert "=================================\n\n")
          (insert (format "Remote: %s\n" tramp-rpc-magit-bench-remote))
          (insert (format "Total time: %s\n"
                          (tramp-rpc-magit-bench--format-time rpc-time)))
          (insert (format "Commands:  %d\n\n" (length commands)))
          (insert "Results:\n")
          (insert (make-string 50 ?-) "\n")
          (dolist (entry result)
            (let* ((key (if (symbolp (car entry))
                            (symbol-name (car entry))
                          (car entry)))
                   (data (cdr entry))
                   (exit-code (alist-get 'exit_code data))
                   (stdout (tramp-rpc--decode-output
                            (alist-get 'stdout data) nil)))
              (insert (format "%-50s exit=%d stdout=%d bytes\n"
                              key exit-code (length (or stdout ""))))))
          (goto-char (point-min))
          (display-buffer (current-buffer)))))))

;;; Benchmark: Real magit-status with optimization comparison

(defvar tramp-rpc-magit-bench--rpc-call-log nil
  "Log of RPC calls during benchmarking.")

(defun tramp-rpc-magit-bench--reset-rpc-log ()
  "Reset the RPC call log."
  (setq tramp-rpc-magit-bench--rpc-call-log nil))

(defun tramp-rpc-magit-bench--log-rpc-call (orig-fun vec method params)
  "Advice to log RPC calls."
  (push (list :method method :time (current-time)) tramp-rpc-magit-bench--rpc-call-log)
  (funcall orig-fun vec method params))

(defun tramp-rpc-magit-bench--count-rpc-calls ()
  "Count RPC calls by method from the log."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (entry tramp-rpc-magit-bench--rpc-call-log)
      (let ((method (plist-get entry :method)))
        (puthash method (1+ (gethash method counts 0)) counts)))
    counts))

;;;###autoload
(defun tramp-rpc-magit-bench-optimizations ()
  "Benchmark magit-status with optimizations enabled vs disabled.
This tests the effectiveness of the caching and prefetching in tramp-rpc."
  (interactive)
  (require 'magit)
  (require 'tramp-rpc)

  (let ((default-directory tramp-rpc-magit-bench-remote)
        results-without results-with)

    ;; Ensure we have a fresh connection
    (message "Warming up connection...")
    (with-temp-buffer
      (process-file "git" nil t nil "rev-parse" "HEAD"))

    ;; Test WITHOUT optimizations
    (message "Testing WITHOUT magit optimizations...")
    (tramp-rpc-magit-disable)
    (tramp-rpc-magit-bench--reset-rpc-log)

    ;; Add RPC logging advice
    (when (fboundp 'tramp-rpc--call)
      (advice-add 'tramp-rpc--call :around #'tramp-rpc-magit-bench--log-rpc-call))

    (unwind-protect
        (progn
          (let ((time-without
                 (tramp-rpc-magit-bench--time
                  (magit-status-setup-buffer default-directory))))
            (setq results-without
                  (list :time time-without
                        :rpc-counts (copy-hash-table (tramp-rpc-magit-bench--count-rpc-calls))
                        :total-rpcs (length tramp-rpc-magit-bench--rpc-call-log))))

          ;; Kill the magit buffer
          (when-let* ((buf (magit-get-mode-buffer 'magit-status-mode)))
            (kill-buffer buf))

          ;; Short pause
          (sleep-for 0.5)

          ;; Test WITH optimizations
          (message "Testing WITH magit optimizations...")
          (tramp-rpc-magit-enable)
          (tramp-rpc-magit-bench--reset-rpc-log)

          (let ((time-with
                 (tramp-rpc-magit-bench--time
                  (magit-status-setup-buffer default-directory))))
            (setq results-with
                  (list :time time-with
                        :rpc-counts (copy-hash-table (tramp-rpc-magit-bench--count-rpc-calls))
                        :total-rpcs (length tramp-rpc-magit-bench--rpc-call-log)))))

      ;; Cleanup
      (when (fboundp 'tramp-rpc--call)
        (advice-remove 'tramp-rpc--call #'tramp-rpc-magit-bench--log-rpc-call))
      ;; Re-enable optimizations (they are on by default)
      (tramp-rpc-magit-enable))

    ;; Display results
    (with-current-buffer (get-buffer-create "*Optimization Benchmark*")
      (erase-buffer)
      (insert "Magit Optimization Benchmark Results\n")
      (insert "=====================================\n\n")
      (insert (format "Remote: %s\n\n" tramp-rpc-magit-bench-remote))

      (insert "Timing Results:\n")
      (insert (make-string 50 ?-) "\n")
      (insert (format "Without optimizations: %s\n"
                      (tramp-rpc-magit-bench--format-time (plist-get results-without :time))))
      (insert (format "With optimizations:    %s\n"
                      (tramp-rpc-magit-bench--format-time (plist-get results-with :time))))
      (insert (format "Speedup:               %.2fx\n\n"
                      (/ (plist-get results-without :time)
                         (max 0.001 (plist-get results-with :time)))))

      (insert "RPC Call Counts:\n")
      (insert (make-string 50 ?-) "\n")
      (insert (format "Without optimizations: %d total RPC calls\n"
                      (plist-get results-without :total-rpcs)))
      (insert (format "With optimizations:    %d total RPC calls\n"
                      (plist-get results-with :total-rpcs)))
      (insert (format "Reduction:             %d%% fewer calls\n\n"
                      (if (zerop (plist-get results-without :total-rpcs))
                          0
                        (round (* 100 (- 1.0 (/ (float (plist-get results-with :total-rpcs))
                                                (plist-get results-without :total-rpcs))))))))

      ;; Detailed breakdown
      (insert "Detailed RPC breakdown (without optimizations):\n")
      (maphash (lambda (method count)
                 (insert (format "  %s: %d\n" method count)))
               (plist-get results-without :rpc-counts))

      (insert "\nDetailed RPC breakdown (with optimizations):\n")
      (maphash (lambda (method count)
                 (insert (format "  %s: %d\n" method count)))
               (plist-get results-with :rpc-counts))

      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun tramp-rpc-magit-bench-all ()
  "Run all magit-related benchmarks."
  (interactive)
  (message "Running round-trip benchmark...")
  (tramp-rpc-magit-bench-roundtrip)
  (message "\nRunning batching comparison...")
  (tramp-rpc-magit-bench-compare-batching)
  (message "\nRunning server-side status RPC benchmark...")
  (tramp-rpc-magit-bench-batched-status))

;;;###autoload
(defun tramp-rpc-magit-bench-quick ()
  "Quick benchmark comparing sequential vs batched git commands."
  (interactive)
  (tramp-rpc-magit-bench-compare-batching))

(provide 'benchmark-magit-detailed)
;;; benchmark-magit-detailed.el ends here
