;;; tramp-rpc-chaos-tests.el --- Randomized process lifecycle tests -*- lexical-binding: t -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>

;; This file is part of tramp-rpc.

;;; Commentary:

;; Seeded random scenarios that start, feed, signal, delete and disconnect
;; remote processes through the real client and server.  A seed fixes the
;; plan (scripts, inputs, client actions and their times); scheduling still
;; varies between runs, which is the point.
;;
;; Every plan first runs against local processes.  The same checks must
;; pass there, so a failure marked LOCAL is a bug in this oracle, not in
;; tramp-rpc.  Undisturbed processes must then match exactly: output,
;; status, exit code and the single sentinel event.  Disturbed processes
;; (client signals, deletion, connection loss) must still see exactly one
;; terminal sentinel call and a prefix of their output, and leave no
;; tracking state, local relays or remote processes behind.
;;
;; While remote plans run, connection filter input is split at random
;; offsets, so frames arrive torn and handlers run between the pieces.
;;
;; scripts/chaos-test.sh runs this file against a server behind a relay
;; that also delays and coalesces the byte stream.
;;
;; Environment:
;;   TRAMP_RPC_TEST_HOST     remote host (default localhost)
;;   TRAMP_RPC_CHAOS_SEED    first seed (default 1)
;;   TRAMP_RPC_CHAOS_SEEDS   number of seeds (default 20)
;;   TRAMP_RPC_CHAOS_SPLIT   percentage of filter calls split (default 50)

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'tramp-rpc-tests
         (expand-file-name "tramp-rpc-tests"
                           (file-name-directory (macroexp-file-name))))

(defvar tramp-rpc--async-processes)
(defvar tramp-rpc--pty-processes)
(defvar tramp-rpc--process-write-queues)
(defvar tramp-rpc--early-process-notifications)
(defvar tramp-rpc--process-starts)
(defvar tramp-rpc--connections)
(defvar tramp-rpc-use-direct-ssh-pty)

(defun tramp-rpc-chaos--env-number (name default)
  "Return environment variable NAME as a number, or DEFAULT."
  (let ((value (getenv name)))
    (if (and value (not (string-empty-p value)))
        (string-to-number value)
      default)))

(defvar tramp-rpc-chaos-first-seed (tramp-rpc-chaos--env-number "TRAMP_RPC_CHAOS_SEED" 1)
  "First seed to run.")

(defvar tramp-rpc-chaos-seeds (tramp-rpc-chaos--env-number "TRAMP_RPC_CHAOS_SEEDS" 20)
  "Number of consecutive seeds to run.")

(defvar tramp-rpc-chaos-split-percent (tramp-rpc-chaos--env-number "TRAMP_RPC_CHAOS_SPLIT" 50)
  "Percentage of connection filter calls whose input is split.")

(defvar tramp-rpc-chaos-timeout 60
  "Seconds one plan may take before it counts as hung.")

;;; ============================================================================
;;; Plans
;;; ============================================================================

(defun tramp-rpc-chaos--chance (percent)
  "Return non-nil with probability PERCENT."
  (< (random 100) percent))

(defun tramp-rpc-chaos--lines (prefix from to)
  "Return the output of seq -f PREFIX%.0f FROM TO."
  (mapconcat (lambda (n) (format "%s%d\n" prefix n))
             (number-sequence from to) ""))

(defun tramp-rpc-chaos--script (spec)
  "Fill SPEC with a random script and its expected results."
  (let ((stderr (plist-get spec :stderr))
        (cat (plist-get spec :input))
        (line 0) steps (out "") (err "") (catted nil))
    (dotimes (_ (+ 1 (random 4)))
      (pcase (random 4)
        ((or 0 1)
         (let* ((count (if (tramp-rpc-chaos--chance 10)
                           (+ 2000 (random 20000))
                         (+ 1 (random 200))))
                (to-err (and stderr (tramp-rpc-chaos--chance 40)))
                (prefix (if to-err "e" "o")))
           (push (format "seq -f '%s%%.0f' %d %d%s" prefix (1+ line) (+ line count)
                         (if to-err " >&2" ""))
                 steps)
           (if to-err
               (setq err (concat err (tramp-rpc-chaos--lines prefix (1+ line) (+ line count))))
             (setq out (concat out (tramp-rpc-chaos--lines prefix (1+ line) (+ line count)))))
           (cl-incf line count)))
        (2 (push (format "sleep 0.%02d" (random 25)) steps))
        (3 (when (and cat (not catted))
             (setq catted t)
             (push "cat" steps)
             (setq out (concat out (apply #'concat cat)))))))
    (when (and cat (not catted))
      (push "cat" steps)
      (setq out (concat out (apply #'concat cat))))
    (let ((end (pcase (random 10)
                 (0 (if (plist-get spec :no-self-signal) '(exit . 0) '(signal . 15)))
                 (1 (if (plist-get spec :no-self-signal) '(exit . 0) '(signal . 9)))
                 ((or 2 3) (cons 'exit (+ 1 (random 3))))
                 (_ '(exit . 0)))))
      (push (pcase end
              (`(signal . 15) "kill -TERM $$")
              (`(signal . 9) "kill -KILL $$")
              (`(exit . ,code) (format "exit %d" code)))
            steps)
      (append spec
              (list :script (concat ": " (plist-get spec :token) "; "
                                    (mapconcat #'identity (nreverse steps) "; "))
                    :stdout out
                    :stderr-text err
                    :end end)))))

(defun tramp-rpc-chaos--plan (seed)
  "Return the random plan for SEED."
  (random (format "tramp-rpc-chaos-%d" seed))
  (let* ((direct-pty (tramp-rpc-chaos--chance 30))
         (disconnect (and (tramp-rpc-chaos--chance 10) (+ 50 (random 500))))
         (specs
          (cl-loop
           for id below (+ 3 (random 6))
           collect
           (let* ((pty (tramp-rpc-chaos--chance 25))
                  (how (if (tramp-rpc-chaos--chance 20) 'start-file-process 'make-process))
                  (stderr (and (not pty) (eq how 'make-process)
                               (tramp-rpc-chaos--chance 30)))
                  (start (random 400))
                  (input (and (not pty) (tramp-rpc-chaos--chance 40)
                              (cl-loop for k below (+ 1 (random 3))
                                       collect (if (tramp-rpc-chaos--chance 10)
                                                   (tramp-rpc-chaos--lines
                                                    (format "big%d-%d-" id k) 1 5000)
                                                 (format "in%d-%d\n" id k)))))
                  (input-times (let ((at (+ start 20)))
                                 (mapcar (lambda (_) (cl-incf at (+ 5 (random 150))))
                                         input)))
                  (spec
                   (list :id id :pty pty :how how :stderr stderr :start start
                         :input input :input-times input-times
                         :eof (and input (+ (or (car (last input-times)) start)
                                            5 (random 100)))
                         ;; ssh -t reports a remote signal death as an exit.
                         :no-self-signal (and pty direct-pty)
                         :filter (tramp-rpc-chaos--chance 30)
                         :nested (pcase (random 7)
                                   (0 'process-file)
                                   (1 'accept))
                         :disrupt (and (tramp-rpc-chaos--chance 25)
                                       (cons (nth (random 4) '(kill-process
                                                               interrupt-process
                                                               delete-process
                                                               term))
                                             (+ start (random 400)))))))
             spec)))
         (syncs (cl-loop repeat (random 4)
                         collect (list :at (random 600)
                                       :lines (+ 1 (random 300))
                                       :code (random 3)))))
    (list :seed seed :direct-pty direct-pty :disconnect disconnect
          :specs specs :syncs syncs)))

(defun tramp-rpc-chaos--finalize-plan (plan remote)
  "Return PLAN with scripts for a REMOTE or local run."
  (let ((tag (format "chaos-%s-%d" (if remote "R" "L") (plist-get plan :seed))))
    ;; Scripts depend only on the plan seed, not on the run.
    (random (format "tramp-rpc-chaos-script-%d" (plist-get plan :seed)))
    (append
     (list :tag tag :remote remote
           :specs (mapcar (lambda (spec)
                            (tramp-rpc-chaos--script
                             (append spec (list :token (format "%s-%d" tag
                                                               (plist-get spec :id))))))
                          (plist-get plan :specs)))
     plan)))

;;; ============================================================================
;;; Filter chaos
;;; ============================================================================

(defvar tramp-rpc-chaos--filter-queues (make-hash-table :test 'eq :weakness 'key)
  "Undelivered filter input pieces per connection process.")

(defun tramp-rpc-chaos--split (output)
  "Return OUTPUT as a list of random pieces."
  (if (or (< (length output) 2)
          (not (tramp-rpc-chaos--chance tramp-rpc-chaos-split-percent)))
      (list output)
    (let* ((cuts (sort (delete-dups
                        (cl-loop repeat (+ 1 (random 5))
                                 collect (+ 1 (random (1- (length output))))))
                       #'<))
           (bounds (append '(0) cuts (list (length output)))))
      (cl-loop for (from to) on bounds while to
               collect (substring output from to)))))

(defun tramp-rpc-chaos--split-filter (orig process output)
  "Call ORIG for PROCESS with OUTPUT split into random pieces.
Pieces are queued per process, so a filter call nested in a piece's handling
delivers the remaining older pieces first and order is preserved."
  (puthash process
           (append (gethash process tramp-rpc-chaos--filter-queues)
                   (tramp-rpc-chaos--split output))
           tramp-rpc-chaos--filter-queues)
  (while (gethash process tramp-rpc-chaos--filter-queues)
    (let ((queue (gethash process tramp-rpc-chaos--filter-queues)))
      (puthash process (cdr queue) tramp-rpc-chaos--filter-queues)
      (funcall orig process (car queue)))))

;;; ============================================================================
;;; Execution
;;; ============================================================================

(cl-defstruct (tramp-rpc-chaos--rec (:constructor tramp-rpc-chaos--make-rec))
  spec process buffer stderr-buffer (filtered "") events
  output-at-exit status code start-error nested errors notes)

(defun tramp-rpc-chaos--output (rec)
  "Return the stdout REC has received so far, without PTY carriage returns."
  (let ((output
         (if (plist-get (tramp-rpc-chaos--rec-spec rec) :filter)
             (tramp-rpc-chaos--rec-filtered rec)
           (let ((buffer (tramp-rpc-chaos--rec-buffer rec)))
             (if (buffer-live-p buffer)
                 (with-current-buffer buffer (buffer-string))
               "")))))
    (if (plist-get (tramp-rpc-chaos--rec-spec rec) :pty)
        (string-replace "\r" "" output)
      output)))

(defmacro tramp-rpc-chaos--guard (rec &rest body)
  "Run BODY, recording any error in REC instead of signaling it."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error (push (format "%S" err) (tramp-rpc-chaos--rec-errors ,rec)))))

(defun tramp-rpc-chaos--nested-process-file ()
  "Run a short synchronous process; return a problem string or nil."
  (with-temp-buffer
    (let ((code (process-file "sh" nil t nil "-c" "printf nested; exit 7")))
      (unless (and (eql code 7) (equal (buffer-string) "nested"))
        (format "nested process-file returned %S with %S" code (buffer-string))))))

(defun tramp-rpc-chaos--sentinel (rec directory)
  "Return the user sentinel for REC, running nested work in DIRECTORY."
  (lambda (process event)
    (tramp-rpc-chaos--guard rec
      (push event (tramp-rpc-chaos--rec-events rec))
      (when (memq (process-status process) '(exit signal))
        (setf (tramp-rpc-chaos--rec-output-at-exit rec) (tramp-rpc-chaos--output rec)
              (tramp-rpc-chaos--rec-status rec) (process-status process)
              (tramp-rpc-chaos--rec-code rec) (process-exit-status process)))
      (pcase (plist-get (tramp-rpc-chaos--rec-spec rec) :nested)
        ('process-file
         (let ((default-directory directory))
           (setf (tramp-rpc-chaos--rec-nested rec)
                 (or (tramp-rpc-chaos--nested-process-file) 'ok))))
        ('accept (accept-process-output nil 0.02))))))

(defun tramp-rpc-chaos--start (rec directory)
  "Start the process of REC in DIRECTORY."
  (let* ((spec (tramp-rpc-chaos--rec-spec rec))
         (default-directory directory)
         (name (format "chaos-%d" (plist-get spec :id)))
         (buffer (generate-new-buffer name))
         (stderr (and (plist-get spec :stderr)
                      (generate-new-buffer (concat name "-err"))))
         (filter (and (plist-get spec :filter)
                      (lambda (_process output)
                        (setf (tramp-rpc-chaos--rec-filtered rec)
                              (concat (tramp-rpc-chaos--rec-filtered rec) output)))))
         (sentinel (tramp-rpc-chaos--sentinel rec directory))
         (command (list "sh" "-c" (plist-get spec :script))))
    (setf (tramp-rpc-chaos--rec-buffer rec) buffer
          (tramp-rpc-chaos--rec-stderr-buffer rec) stderr)
    (condition-case err
        (let ((process
               (if (eq (plist-get spec :how) 'start-file-process)
                   ;; Like vc and compile: configure after the process exists.
                   (let* ((process-connection-type (plist-get spec :pty))
                          (process (apply #'start-file-process name buffer command)))
                     (set-process-coding-system process 'no-conversion 'no-conversion)
                     (set-process-query-on-exit-flag process nil)
                     (when filter (set-process-filter process filter))
                     (set-process-sentinel process sentinel)
                     process)
                 (make-process :name name :buffer buffer :command command
                               :connection-type (if (plist-get spec :pty) 'pty 'pipe)
                               :coding 'no-conversion :noquery t :stderr stderr
                               :filter filter :sentinel sentinel :file-handler t))))
          (setf (tramp-rpc-chaos--rec-process rec) process))
      (error (setf (tramp-rpc-chaos--rec-start-error rec) (format "%S" err))))))

(defun tramp-rpc-chaos--act (rec action)
  "Apply client ACTION to the process of REC, if it was started."
  (if-let* ((process (tramp-rpc-chaos--rec-process rec)))
      (condition-case err
        (pcase action
          (`(send ,string) (process-send-string process string))
          ('eof (process-send-eof process))
          ('kill-process (kill-process process))
          ('interrupt-process (interrupt-process process))
          ('delete-process (delete-process process))
          ('term (signal-process process 'TERM)))
      (error
       ;; Disturbed processes may have exited before the action.
       (push (format "%S failed: %S" (if (eq (car-safe action) 'send) 'send action) err)
             (if (or (plist-get (tramp-rpc-chaos--rec-spec rec) :disrupt)
                     (plist-get (tramp-rpc-chaos--rec-spec rec) :disconnected))
                 (tramp-rpc-chaos--rec-notes rec)
               (tramp-rpc-chaos--rec-errors rec)))))
    ;; A slow start may still be running when its first actions are due.
    (push (format "%S skipped: not started yet" (if (eq (car-safe action) 'send) 'send action))
          (tramp-rpc-chaos--rec-notes rec))))

(defun tramp-rpc-chaos--disconnect ()
  "Kill every RPC transport like a dropped network connection."
  (maphash (lambda (_key connection)
             (let ((process (tramp-rpc-connection-process connection)))
               (when (process-live-p process)
                 (signal-process process 'KILL))))
           tramp-rpc--connections))

(defun tramp-rpc-chaos--sync (sync directory)
  "Run SYNC as process-file in DIRECTORY; return a problem string or nil."
  (let ((default-directory directory)
        (lines (plist-get sync :lines))
        (code (plist-get sync :code)))
    (with-temp-buffer
      (let ((result (process-file "sh" nil t nil "-c"
                                  (format "seq -f 's%%.0f' 1 %d; exit %d" lines code))))
        (unless (and (eql result code)
                     (equal (buffer-string) (tramp-rpc-chaos--lines "s" 1 lines)))
          (format "process-file returned %S with %d bytes" result (buffer-size)))))))

(defun tramp-rpc-chaos--finished-p (rec)
  "Return non-nil when REC needs no more events."
  (or (tramp-rpc-chaos--rec-start-error rec)
      (tramp-rpc-chaos--rec-status rec)))

(defun tramp-rpc-chaos--execute (plan directory)
  "Run PLAN in DIRECTORY; return (RECS SYNC-RESULTS HUNG)."
  (let* ((remote (plist-get plan :remote))
         (recs (mapcar (lambda (spec)
                         (tramp-rpc-chaos--make-rec
                          :spec (if (and remote (plist-get plan :disconnect))
                                    (append spec '(:disconnected t))
                                  spec)))
                       (plist-get plan :specs)))
         (sync-results (make-vector (length (plist-get plan :syncs)) 'pending))
         (timers nil)
         (deadline (+ (float-time) tramp-rpc-chaos-timeout)))
    (cl-flet ((at (ms function &rest args)
                (push (apply #'run-at-time (/ ms 1000.0) nil function args) timers)))
      (dolist (rec recs)
        (let ((spec (tramp-rpc-chaos--rec-spec rec)))
          (at (plist-get spec :start) #'tramp-rpc-chaos--start rec directory)
          (cl-loop for input in (plist-get spec :input)
                   for time in (plist-get spec :input-times)
                   do (at time #'tramp-rpc-chaos--act rec (list 'send input)))
          (when (plist-get spec :eof)
            (at (plist-get spec :eof) #'tramp-rpc-chaos--act rec 'eof))
          (when-let* ((disrupt (plist-get spec :disrupt)))
            (at (cdr disrupt) #'tramp-rpc-chaos--act rec (car disrupt)))))
      (cl-loop for sync in (plist-get plan :syncs)
               for index from 0
               do (at (plist-get sync :at)
                      (lambda (index sync)
                        (aset sync-results index
                              (condition-case err
                                  (or (tramp-rpc-chaos--sync sync directory) 'ok)
                                (error (format "process-file signaled %S" err)))))
                      index sync))
      (when (and remote (plist-get plan :disconnect))
        (at (plist-get plan :disconnect) #'tramp-rpc-chaos--disconnect)))
    (while (and (< (float-time) deadline)
                (not (and (cl-every #'tramp-rpc-chaos--finished-p recs)
                          (not (memq 'pending (append sync-results nil)))
                          (cl-notany (lambda (timer) (memq timer timer-list)) timers))))
      (accept-process-output nil 0.05))
    (let ((hung (< deadline (float-time))))
      (mapc #'cancel-timer timers)
      (list recs sync-results hung))))

;;; ============================================================================
;;; Checks
;;; ============================================================================

(defun tramp-rpc-chaos--settle (predicate seconds)
  "Run the event loop until PREDICATE holds or SECONDS pass; return its value."
  (let ((deadline (+ (float-time) seconds))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    value))

(defun tramp-rpc-chaos--event (end)
  "Return the sentinel event of a process that ended with END."
  (pcase end
    ('(exit . 0) "finished\n")
    (`(exit . ,code) (format "exited abnormally with code %d\n" code))
    ('(signal . 9) "killed\n")
    ('(signal . 15) "terminated\n")))

(defun tramp-rpc-chaos--check-rec (rec)
  "Return problem strings for REC."
  (let* ((spec (tramp-rpc-chaos--rec-spec rec))
         (pty (plist-get spec :pty))
         (disturbed (or (plist-get spec :disrupt) (plist-get spec :disconnected)))
         (events (tramp-rpc-chaos--rec-events rec))
         (output (tramp-rpc-chaos--output rec))
         (expected (plist-get spec :stdout))
         (stderr (and (buffer-live-p (tramp-rpc-chaos--rec-stderr-buffer rec))
                      (with-current-buffer (tramp-rpc-chaos--rec-stderr-buffer rec)
                        ;; Drop the status line of the stderr pipe's sentinel.
                        (replace-regexp-in-string
                         "\nProcess [^\n]+\n\\'" "" (buffer-string)))))
         (end (plist-get spec :end))
         problems)
    (cl-flet ((problem (format &rest args)
                (push (apply #'format format args) problems)))
      (dolist (err (tramp-rpc-chaos--rec-errors rec))
        (problem "error: %s" err))
      (cond
       ((tramp-rpc-chaos--rec-start-error rec)
        (unless (plist-get spec :disconnected)
          (problem "start failed: %s" (tramp-rpc-chaos--rec-start-error rec))))
       (t
        (unless (= 1 (length events))
          (problem "%d sentinel calls %S, expected 1" (length events) events))
        (unless (tramp-rpc-chaos--rec-status rec)
          (problem "no terminal sentinel call; status %S"
                   (process-status (tramp-rpc-chaos--rec-process rec))))
        (unless (equal output (tramp-rpc-chaos--rec-output-at-exit rec))
          (problem "%d stdout bytes at sentinel, %d after"
                   (length (tramp-rpc-chaos--rec-output-at-exit rec)) (length output)))
        (when (stringp (tramp-rpc-chaos--rec-nested rec))
          (problem "%s" (tramp-rpc-chaos--rec-nested rec)))
        (if disturbed
            (unless (or pty (string-prefix-p output expected))
              (problem "stdout (%d bytes) is not a prefix of the expected %d bytes"
                       (length output) (length expected)))
          ;; A PTY may add its own output, such as ssh's closing message.
          (unless (if pty (string-search expected output) (equal output expected))
            (let* ((diff (compare-strings output nil nil expected nil nil))
                   (at (and (integerp diff) (1- (abs diff)))))
              (problem "stdout %d bytes, expected %d; first difference at %S: got %S, expected %S"
                       (length output) (length expected) at
                       (and at (string-replace
                                "\n" "|" (substring output (max 0 (- at 30))
                                                    (min (length output) (+ at 60)))))
                       (and at (string-replace
                                "\n" "|" (substring expected (max 0 (- at 30))
                                                      (min (length expected) (+ at 60))))))))
          (unless (equal (cons (tramp-rpc-chaos--rec-status rec)
                               (tramp-rpc-chaos--rec-code rec))
                         end)
            (problem "ended %S, expected %S"
                     (cons (tramp-rpc-chaos--rec-status rec)
                           (tramp-rpc-chaos--rec-code rec))
                     end))
          (unless (equal events (list (tramp-rpc-chaos--event end)))
            (problem "sentinel event %S, expected %S"
                     events (tramp-rpc-chaos--event end))))
        (when stderr
          (unless (if disturbed
                      (string-prefix-p stderr (plist-get spec :stderr-text))
                    (equal stderr (plist-get spec :stderr-text)))
            (problem "stderr %d bytes, expected %d"
                     (length stderr) (length (plist-get spec :stderr-text))))))))
    (mapcar (lambda (problem)
              (format "process %d %S: %s" (plist-get spec :id)
                      (list :pty pty :how (plist-get spec :how)
                            :disrupt (plist-get spec :disrupt)
                            :filter (plist-get spec :filter)
                            :nested (plist-get spec :nested)
                            :end end)
                      problem))
            problems)))

(defun tramp-rpc-chaos--tracked (table)
  "Return a description of the processes tracked in TABLE."
  (let (processes)
    (maphash (lambda (process info)
               (push (list (process-name process) (process-status process)
                           :pid (plist-get info :pid)
                           :exited (process-get process :tramp-rpc-remote-exited))
                     processes))
             table)
    processes))

(defun tramp-rpc-chaos--client-state ()
  "Return a description of leftover tramp-rpc process state, or nil."
  (let ((state
         (delq nil
               (list
                (and (> (hash-table-count tramp-rpc--async-processes) 0)
                     (format "async-processes %S"
                             (tramp-rpc-chaos--tracked tramp-rpc--async-processes)))
                (and (> (hash-table-count tramp-rpc--pty-processes) 0)
                     (format "pty-processes %S"
                             (tramp-rpc-chaos--tracked tramp-rpc--pty-processes)))
                (and (> (hash-table-count tramp-rpc--process-write-queues) 0)
                     (format "write-queues %S"
                             (let (queues)
                               (maphash
                                (lambda (_key queue)
                                  (push (list :pid (plist-get queue :pid)
                                              :owner (plist-get queue :owner-process)
                                              :pending (length (plist-get queue :pending))
                                              :writing (plist-get queue :writing)
                                              :failure (plist-get queue :failure))
                                        queues))
                                tramp-rpc--process-write-queues)
                               queues)))
                (and tramp-rpc--early-process-notifications
                     (format "early-notifications %d"
                             (length tramp-rpc--early-process-notifications)))
                (and (/= tramp-rpc--process-starts 0)
                     (format "process-starts %d" tramp-rpc--process-starts))))))
    (and state (string-join state ", "))))

(defun tramp-rpc-chaos--live-local-processes ()
  "Return names of live local processes created by a plan."
  (cl-loop for process in (process-list)
           when (and (process-live-p process)
                     (string-prefix-p "chaos-" (process-name process)))
           collect (process-name process)))

(defun tramp-rpc-chaos--remote-leftovers (directory tag)
  "Return remote processes from TAG or server children, or nil.
Commands run in DIRECTORY."
  (let ((default-directory directory))
    (with-temp-buffer
      (process-file
       "sh" nil t nil "-c"
       ;; [c] keeps pgrep from matching this shell.  The parent of this shell
       ;; is the server, so its other children are leaked managed processes.
       (format "pgrep -af '[%s]%s-'; ps -o pid=,stat=,args= --ppid $PPID | awk -v me=$$ '$1 != me'"
               (substring tag 0 1) (substring tag 1)))
      (let ((output (string-trim (buffer-string))))
        (and (not (string-empty-p output)) output)))))

(defun tramp-rpc-chaos--check (plan directory result)
  "Return problem strings for PLAN run in DIRECTORY with RESULT."
  (pcase-let* ((`(,recs ,sync-results ,hung) result)
               (remote (plist-get plan :remote))
               (problems (mapcan #'tramp-rpc-chaos--check-rec recs)))
    (when hung
      (push (format "hung for %ds" tramp-rpc-chaos-timeout) problems))
    (cl-loop for sync-result across sync-results
             for sync in (plist-get plan :syncs)
             unless (or (eq sync-result 'ok)
                        (and remote (plist-get plan :disconnect)))
             do (push (format "sync %S: %s" sync sync-result) problems))
    (unless (tramp-rpc-chaos--settle
             (lambda () (null (tramp-rpc-chaos--live-local-processes))) 5)
      (push (format "live local processes %S" (tramp-rpc-chaos--live-local-processes))
            problems))
    (when remote
      (unless (tramp-rpc-chaos--settle
               (lambda () (null (tramp-rpc-chaos--client-state))) 5)
        (push (format "client state left: %s" (tramp-rpc-chaos--client-state)) problems)
        ;; Report each leak once instead of in every following seed.
        (clrhash tramp-rpc--async-processes)
        (clrhash tramp-rpc--pty-processes)
        (clrhash tramp-rpc--process-write-queues)
        (setq tramp-rpc--early-process-notifications nil))
      (let (leftovers)
        (tramp-rpc-chaos--settle
         (lambda ()
           (null (setq leftovers
                       (condition-case err
                           (tramp-rpc-chaos--remote-leftovers
                            directory (plist-get plan :tag))
                         (error (format "leftover check failed: %S" err))))))
         5)
        (when leftovers
          (push (format "remote processes left:\n%s" leftovers) problems))))
    (when problems
      (dolist (rec recs)
        (when (tramp-rpc-chaos--rec-notes rec)
          (push (format "note: process %d: %s"
                        (plist-get (tramp-rpc-chaos--rec-spec rec) :id)
                        (string-join (reverse (tramp-rpc-chaos--rec-notes rec)) "; "))
                problems))))
    (nreverse problems)))

(defun tramp-rpc-chaos--cleanup-buffers (result)
  "Kill the buffers of RESULT."
  (dolist (rec (car result))
    (dolist (buffer (list (tramp-rpc-chaos--rec-buffer rec)
                          (tramp-rpc-chaos--rec-stderr-buffer rec)))
      (when (buffer-live-p buffer)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))))

(defun tramp-rpc-chaos--run (plan directory)
  "Run PLAN in DIRECTORY and return its problems."
  (let ((result (tramp-rpc-chaos--execute plan directory)))
    (prog1 (tramp-rpc-chaos--check plan directory result)
      (tramp-rpc-chaos--cleanup-buffers result))))

(defun tramp-rpc-chaos--summary (plan)
  "Return a one-line summary of PLAN."
  (format "seed %d: %d processes, %d syncs%s%s"
          (plist-get plan :seed) (length (plist-get plan :specs))
          (length (plist-get plan :syncs))
          (if (plist-get plan :direct-pty) ", direct ssh pty" "")
          (if (plist-get plan :disconnect)
              (format ", disconnect at %dms" (plist-get plan :disconnect))
            "")))

(defun tramp-rpc-chaos--run-seed (seed)
  "Run SEED locally and remotely; return its problems."
  (let* ((plan (tramp-rpc-chaos--plan seed))
         (local-plan (tramp-rpc-chaos--finalize-plan plan nil))
         (remote-plan (tramp-rpc-chaos--finalize-plan plan t))
         (local (mapcar (lambda (problem) (concat "LOCAL (oracle bug): " problem))
                        (tramp-rpc-chaos--run local-plan temporary-file-directory)))
         (remote (let ((tramp-rpc-use-direct-ssh-pty (plist-get plan :direct-pty)))
                   (advice-add 'tramp-rpc--connection-filter :around
                               #'tramp-rpc-chaos--split-filter)
                   (unwind-protect
                       (tramp-rpc-chaos--run remote-plan
                                             (tramp-rpc-test--remote-directory))
                     (advice-remove 'tramp-rpc--connection-filter
                                    #'tramp-rpc-chaos--split-filter)))))
    (message "%s: %s" (tramp-rpc-chaos--summary plan)
             (if (or local remote) "FAILED" "ok"))
    (dolist (problem (append local remote))
      (message "  %s" problem))
    (when remote
      ;; Do not let one broken connection fail the following seeds.
      (tramp-cleanup-all-connections))
    (and (or local remote)
         (cons (tramp-rpc-chaos--summary plan) (append local remote)))))

(ert-deftest tramp-rpc-chaos-test-random-process-lifecycles ()
  "Random process lifecycles leave correct results and no leftovers."
  :tags '(:expensive-test :process)
  (skip-unless (tramp-rpc-test-enabled))
  (let ((failures
         (cl-loop for seed from tramp-rpc-chaos-first-seed
                  below (+ tramp-rpc-chaos-first-seed tramp-rpc-chaos-seeds)
                  when (tramp-rpc-chaos--run-seed seed) collect it)))
    (should-not failures)))

(provide 'tramp-rpc-chaos-tests)
;;; tramp-rpc-chaos-tests.el ends here
