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
(declare-function tramp-rpc-magit-status "tramp-rpc")
(declare-function tramp-rpc-magit-enable "tramp-rpc")
(declare-function tramp-rpc-magit-disable "tramp-rpc")
(declare-function magit-status-setup-buffer "magit-status")
(declare-function magit-get-mode-buffer "magit-mode")

;;; Configuration

(defvar tramp-rpc-magit-bench-remote "/rpc:cptra-fpga:/home/ubuntu/caliptra-sw/"
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

;;; Benchmark: Magit-style refresh operations

(defun tramp-rpc-magit-bench-status-commands ()
  "Run the sequence of git commands that magit-status typically runs.
Returns timing information for each command."
  (let* ((default-directory tramp-rpc-magit-bench-remote)
         (results nil))
    
    ;; These are the typical commands magit runs during status refresh
    ;; (simplified version based on magit-status.el)
    (dolist (cmd '(;; Pre-refresh
                   ("git" "update-index" "--refresh")
                   ;; Headers
                   ("git" "rev-parse" "--show-toplevel")
                   ("git" "rev-parse" "HEAD")
                   ("git" "symbolic-ref" "--short" "HEAD")
                   ("git" "log" "-1" "--format=%h %s" "HEAD")
                   ;; Branch info
                   ("git" "config" "branch.main.remote")
                   ("git" "config" "branch.main.merge")
                   ("git" "config" "branch.main.rebase")
                   ;; Upstream/push info
                   ("git" "rev-parse" "--abbrev-ref" "@{upstream}")
                   ("git" "rev-parse" "--abbrev-ref" "@{push}")
                   ;; Tags
                   ("git" "describe" "--tags" "--abbrev=0")
                   ;; Status for untracked files
                   ("git" "status" "--porcelain" "-z" "--untracked-files=normal")
                   ;; Diffs
                   ("git" "diff-files" "-z" "--name-only" "--diff-filter=u")
                   ("git" "diff-index" "-z" "--name-only" "--cached" "HEAD")
                   ;; Stashes
                   ("git" "stash" "list")
                   ;; Config cache
                   ("git" "config" "--list" "-z")))
      (let* ((program (car cmd))
             (args (cdr cmd))
             (time (tramp-rpc-magit-bench--time
                    (with-temp-buffer
                      (ignore-errors
                        (apply #'process-file program nil t nil args))))))
        (push (list :cmd cmd :time time) results)))
    
    (nreverse results)))

(defun tramp-rpc-magit-bench-run-status ()
  "Run the status command benchmark and display results."
  (interactive)
  (let* ((default-directory tramp-rpc-magit-bench-remote)
         (results nil)
         (total-time 0))
    
    (message "Benchmarking magit-style status commands on %s..." 
             tramp-rpc-magit-bench-remote)
    
    ;; Warm up connection
    (with-temp-buffer
      (process-file "git" nil t nil "rev-parse" "HEAD"))
    
    ;; Run benchmark
    (dotimes (_ tramp-rpc-magit-bench-iterations)
      (push (tramp-rpc-magit-bench-status-commands) results))
    
    ;; Display results
    (with-current-buffer (get-buffer-create "*Magit Benchmark Results*")
      (erase-buffer)
      (insert "Magit Status Command Benchmark\n")
      (insert "==============================\n\n")
      (insert (format "Remote: %s\n" tramp-rpc-magit-bench-remote))
      (insert (format "Iterations: %d\n\n" tramp-rpc-magit-bench-iterations))
      
      ;; Calculate averages
      (let ((first-run (car results)))
        (insert (format "%-50s | %14s | %14s\n"
                        "Command" "Avg Time" "Total"))
        (insert (make-string 85 ?-) "\n")
        
        (dolist (cmd-result first-run)
          (let* ((cmd (plist-get cmd-result :cmd))
                 (cmd-str (string-join cmd " "))
                 (times (mapcar (lambda (run)
                                  (plist-get
                                   (seq-find (lambda (r)
                                               (equal (plist-get r :cmd) cmd))
                                             run)
                                   :time))
                                results))
                 (avg (/ (apply #'+ times) (length times))))
            (setq total-time (+ total-time avg))
            (insert (format "%-50s | %14s | %14s\n"
                            (truncate-string-to-width cmd-str 50)
                            (tramp-rpc-magit-bench--format-time avg)
                            (tramp-rpc-magit-bench--format-time 
                             (* avg tramp-rpc-magit-bench-iterations))))))
        
        (insert (make-string 85 ?-) "\n")
        (insert (format "%-50s | %14s |\n"
                        "TOTAL (sequential)"
                        (tramp-rpc-magit-bench--format-time total-time)))
        
        (let ((roundtrip (plist-get (tramp-rpc-magit-bench-roundtrip) :mean)))
          (insert (format "\nRound-trip overhead: %s\n"
                          (tramp-rpc-magit-bench--format-time roundtrip)))
          (insert (format "Commands: %d\n" (length first-run)))
          (insert (format "Theoretical batched time: %s + %s = %s\n"
                          (tramp-rpc-magit-bench--format-time roundtrip)
                          (tramp-rpc-magit-bench--format-time 
                           (- total-time (* (length first-run) roundtrip)))
                          (tramp-rpc-magit-bench--format-time
                           (+ roundtrip (- total-time (* (length first-run) roundtrip))))))))
      
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

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
  "Benchmark the tramp-rpc-magit-status RPC function."
  (interactive)
  (let ((default-directory tramp-rpc-magit-bench-remote))
    (message "Benchmarking server-side magit status RPC...")
    
    (let ((rpc-time
           (tramp-rpc-magit-bench--time
            (tramp-rpc-magit-status default-directory)))
          (result (tramp-rpc-magit-status default-directory)))
      
      (with-current-buffer (get-buffer-create "*RPC Status Results*")
        (erase-buffer)
        (insert "Server-side Magit Status Results\n")
        (insert "================================\n\n")
        (insert (format "Remote: %s\n" tramp-rpc-magit-bench-remote))
        (insert (format "Total time: %s\n\n"
                        (tramp-rpc-magit-bench--format-time rpc-time)))
        
        (insert "Results gathered in single RPC call:\n")
        (insert (make-string 50 ?-) "\n")
        (insert (format "Toplevel:     %s\n" (alist-get 'toplevel result)))
        (let ((head (alist-get 'head result)))
          (insert (format "HEAD hash:    %s\n" (alist-get 'hash head)))
          (insert (format "Branch:       %s\n" (alist-get 'branch head)))
          (insert (format "HEAD message: %s\n" (alist-get 'message head))))
        (let ((upstream (alist-get 'upstream result)))
          (insert (format "Upstream:     %s\n" (alist-get 'branch upstream))))
        (let ((push-info (alist-get 'push result)))
          (insert (format "Push remote:  %s\n" (alist-get 'branch push-info))))
        (insert (format "State:        %s\n" (alist-get 'state result)))
        (insert (format "Stashes:      %d\n"
                        (length (alist-get 'stashes result))))
        (insert (format "Untracked:    %d files\n"
                        (length (alist-get 'untracked result))))
        
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

;;; Benchmark: Real magit-status

(defvar tramp-rpc-magit-bench--original-process-file nil)

(defun tramp-rpc-magit-bench-magit-status ()
  "Benchmark actual magit-status with process-file logging."
  (interactive)
  (require 'magit)
  
  (let ((default-directory tramp-rpc-magit-bench-remote))
    (message "Benchmarking real magit-status on %s..." default-directory)
    
    ;; Clear log
    (setq tramp-rpc-magit-bench--process-file-log nil)
    
    ;; Add advice to log all process-file calls
    (advice-add 'process-file :around #'tramp-rpc-magit-bench--log-process-file)
    
    (unwind-protect
        (let ((total-time
               (tramp-rpc-magit-bench--time
                (magit-status-setup-buffer default-directory))))
          
          ;; Display results
          (with-current-buffer (get-buffer-create "*Magit Status Benchmark*")
            (erase-buffer)
            (insert "Magit Status Benchmark Results\n")
            (insert "==============================\n\n")
            (insert (format "Remote: %s\n" tramp-rpc-magit-bench-remote))
            (insert (format "Total time: %s\n" 
                            (tramp-rpc-magit-bench--format-time total-time)))
            (insert (format "Process-file calls: %d\n\n"
                            (length tramp-rpc-magit-bench--process-file-log)))
            
            (insert "Process calls (in order):\n")
            (insert (make-string 80 ?-) "\n")
            (insert (format "%-50s | %14s\n" "Command" "Time"))
            (insert (make-string 80 ?-) "\n")
            
            (let ((total-process-time 0))
              (dolist (entry (reverse tramp-rpc-magit-bench--process-file-log))
                (let* ((program (plist-get entry :program))
                       (args (plist-get entry :args))
                       (time (plist-get entry :time))
                       (cmd-str (format "%s %s"
                                        program
                                        (string-join 
                                         (seq-take (car (last args)) 5) " "))))
                  (setq total-process-time (+ total-process-time time))
                  (insert (format "%-50s | %14s\n"
                                  (truncate-string-to-width cmd-str 50)
                                  (tramp-rpc-magit-bench--format-time time)))))
              
              (insert (make-string 80 ?-) "\n")
              (insert (format "%-50s | %14s\n"
                              "Total process time"
                              (tramp-rpc-magit-bench--format-time total-process-time)))
              (insert (format "%-50s | %14s\n"
                              "Non-process overhead"
                              (tramp-rpc-magit-bench--format-time 
                               (- total-time total-process-time))))
              
              ;; Analyze potential savings
              (let ((roundtrip 0.05)  ; Assume 50ms roundtrip
                    (num-calls (length tramp-rpc-magit-bench--process-file-log)))
                (insert (format "\nBatching Analysis (assuming %s roundtrip):\n"
                                (tramp-rpc-magit-bench--format-time roundtrip)))
                (insert (format "  Current overhead: %d calls * %s = %s\n"
                                num-calls
                                (tramp-rpc-magit-bench--format-time roundtrip)
                                (tramp-rpc-magit-bench--format-time 
                                 (* num-calls roundtrip))))
                (insert (format "  With batching: 1 call * %s = %s\n"
                                (tramp-rpc-magit-bench--format-time roundtrip)
                                (tramp-rpc-magit-bench--format-time roundtrip)))
                (insert (format "  Potential time saved: %s\n"
                                (tramp-rpc-magit-bench--format-time 
                                 (* (1- num-calls) roundtrip))))))
            
            (goto-char (point-min))
            (display-buffer (current-buffer))))
      
      ;; Remove advice
      (advice-remove 'process-file #'tramp-rpc-magit-bench--log-process-file))))

;;; Main benchmark runner

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

;;; RPC call counting for optimization analysis

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

(provide 'benchmark-magit-detailed)
;;; benchmark-magit-detailed.el ends here
