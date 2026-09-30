;;; tramp-rpc-stress-remote-tests.el --- SSH stress tests for the subscriber model -*- lexical-binding: t -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>

;; This file is part of tramp-rpc.

;;; Commentary:

;; Stress tests for the process subscriber model exercised over a real SSH
;; connection via the TRAMP-RPC backend.  Unlike `tramp-rpc-stress-tests.el',
;; which talks to a locally-spawned server binary over a pipe, these tests go
;; through a genuine SSH tunnel to a remote host.
;;
;; Tests are designed to expose real failure modes of the subscriber model:
;;
;;   * Dropped output notifications under high concurrency.
;;   * Output arriving after process.exit notification (output-after-exit race).
;;   * Byte-count accuracy across many simultaneous streams.
;;   * Slow delivery caused by polling instead of push.
;;   * Notification state leaked by abrupt kill while output is flowing.
;;
;; Run:
;;   TRAMP_RPC_TEST_HOST=pd2 ./test/run-tests.sh --stress-remote
;;
;; Or directly:
;;   TRAMP_RPC_TEST_HOST=pd2 emacs -Q --batch \
;;     -L /path/to/tramp \
;;     -l test/tramp-rpc-stress-remote-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'tramp)

;; ---------------------------------------------------------------------------
;; Load paths (mirrors tramp-rpc-tests.el)
;; ---------------------------------------------------------------------------

(defvar tramp-rpc-stress-remote-test--project-root
  (expand-file-name "../" (file-name-directory
                           (or load-file-name buffer-file-name
                               (expand-file-name
                                "test/tramp-rpc-stress-remote-tests.el"))))
  "Project root directory.")

(let ((lisp-dir (expand-file-name "lisp" tramp-rpc-stress-remote-test--project-root))
      (source (getenv "TRAMP_SOURCE")))
  (add-to-list 'load-path lisp-dir)
  (when (and source (not (string-empty-p source))
             (file-directory-p (expand-file-name "lisp" source)))
    (add-to-list 'load-path (expand-file-name "lisp" source))))

(let ((msgpack-source
       (or (getenv "MSGPACK_SOURCE")
           (let ((elpaca-path (expand-file-name
                               "../../msgpack"
                               tramp-rpc-stress-remote-test--project-root)))
             (when (file-directory-p elpaca-path) elpaca-path)))))
  (when (and msgpack-source (not (string-empty-p msgpack-source)))
    (add-to-list 'load-path msgpack-source)))

(unless (or (require 'msgpack nil t)
            (progn (require 'package)
                   (package-initialize)
                   (require 'msgpack nil t)))
  (error "tramp-rpc stress remote tests require msgpack.el"))

(setq load-prefer-newer t)
(require 'tramp-rpc)

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(defvar tramp-rpc-stress-remote-test-host
  (or (getenv "TRAMP_RPC_TEST_HOST") "localhost")
  "Remote host for SSH stress testing.")

(defvar tramp-rpc-stress-remote-test-user
  (getenv "TRAMP_RPC_TEST_USER")
  "Remote user for SSH stress testing.")

(defvar tramp-rpc-stress-remote-test-temp-dir
  (format "/tmp/tramp-rpc-stress-remote-%s-%d"
          (user-login-name) (emacs-pid))
  "Unique temporary directory on the remote host for this test session.
Includes the local user name and Emacs PID to avoid collisions between
concurrent test runs on a shared host.")

(setq tramp-verbose 0
      tramp-cache-read-persistent-data nil
      tramp-persistency-file-name nil
      password-cache-expiry nil)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun tramp-rpc-stress-remote-test--remote-dir ()
  "Return the TRAMP path for the remote stress-test directory."
  (let ((user-part (if tramp-rpc-stress-remote-test-user
                       (concat tramp-rpc-stress-remote-test-user "@")
                     "")))
    (format "/rpc:%s%s:%s"
            user-part
            tramp-rpc-stress-remote-test-host
            tramp-rpc-stress-remote-test-temp-dir)))

(defvar tramp-rpc-stress-remote-test--enabled-cache nil)

(defun tramp-rpc-stress-remote-test--enabled ()
  "Return non-nil if the remote host is reachable via TRAMP-RPC."
  (unless (consp tramp-rpc-stress-remote-test--enabled-cache)
    (setq tramp-rpc-stress-remote-test--enabled-cache
          (cons t
                (condition-case nil
                    (let ((dir (tramp-rpc-stress-remote-test--remote-dir)))
                      (ignore-errors (make-directory dir t))
                      (and (file-directory-p dir)
                           (file-writable-p dir)))
                  (error nil)))))
  (cdr tramp-rpc-stress-remote-test--enabled-cache))

(defun tramp-rpc-stress-remote-test--wait-all (procs timeout-secs)
  "Wait up to TIMEOUT-SECS for all PROCS to exit.
Returns a plist :finished and :hung."
  (let ((deadline (+ (float-time) timeout-secs)))
    (while (and (cl-some #'process-live-p procs)
                (> deadline (float-time)))
      (accept-process-output nil 0.05)))
  (let (finished hung)
    (dolist (p procs)
      (if (process-live-p p) (push p hung) (push p finished)))
    (list :finished (nreverse finished) :hung (nreverse hung))))

(defmacro tramp-rpc-stress-remote-test--with-processes (procs-var &rest body)
  "Evaluate BODY, then unconditionally kill all processes in PROCS-VAR."
  (declare (indent 1))
  `(let ((,procs-var nil))
     (unwind-protect
         (progn ,@body)
       (dolist (p ,procs-var)
         (ignore-errors (delete-process p))))))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(ert-deftest tramp-rpc-stress-remote-test-output-verified-concurrent ()
  "100 concurrent processes: verify each filter receives exactly its unique token.
A broken subscriber model will drop output notifications or deliver them to
the wrong process.  Exit-status-only checks would miss both failure modes."
  :tags '(:stress :process)
  (skip-unless (tramp-rpc-stress-remote-test--enabled))
  (let* ((default-directory (tramp-rpc-stress-remote-test--remote-dir))
         (n 100)
         (output (make-hash-table :test 'eq))
         (tokens (make-hash-table :test 'eq)))
    (tramp-rpc-stress-remote-test--with-processes procs
      (dotimes (i n)
        (let* ((token (format "tok%06d" i))
               (proc (make-process
                       :name (format "stress-ov-%d" i)
                       :buffer nil
                       :command (list "/bin/sh" "-c" (format "printf '%%s' '%s'" token))
                       :connection-type 'pipe
                       :noquery t
                       :file-handler t
                       :filter (lambda (p s)
                                 (puthash p (concat (gethash p output "") s)
                                          output)))))
          (puthash proc token tokens)
          (push proc procs)))

      (let* ((result (tramp-rpc-stress-remote-test--wait-all procs 120))
             (hung (plist-get result :hung)))
        (when hung
          (ert-fail (format "%d/%d processes hung" (length hung) n))))

      ;; Drain until every process has received its token or the deadline passes.
      ;; A fixed sleep can mask true losses when output notifications are merely
      ;; delayed; looping to completion distinguishes "delayed" from "lost".
      (let ((drain-deadline (+ (float-time) 15.0)))
        (while (and (> drain-deadline (float-time))
                    (cl-some (lambda (p)
                               (not (equal (gethash p output "")
                                           (gethash p tokens))))
                             procs))
          (accept-process-output nil 0.05)))

      ;; Verify every process got exactly its own token — no drops, no mixing.
      (let (failures)
        (dolist (p procs)
          (let ((expected (gethash p tokens))
                (got (gethash p output "")))
            (unless (equal got expected)
              (push (format "proc %s: expected %S got %S"
                            (process-name p) expected got)
                    failures))))
        (when failures
          (ert-fail (format "%d/%d output mismatches:\n%s"
                            (length failures) n
                            (mapconcat #'identity (seq-take failures 5) "\n"))))))))

(ert-deftest tramp-rpc-stress-remote-test-output-after-exit-race ()
  "50 processes produce 2 KB then exit immediately: all bytes must arrive.
This is the primary race in the subscriber model: the server emits
process.output then process.exit in rapid succession.  A broken
implementation loses the output notification when the exit notification
is processed first, or drains output only up to the point of exit."
  :tags '(:stress :process)
  (skip-unless (tramp-rpc-stress-remote-test--enabled))
  (let* ((default-directory (tramp-rpc-stress-remote-test--remote-dir))
         (n 50)
         ;; 2048 bytes of 'x' per process, zero frills, exits immediately after.
         (payload-size 2048)
         (payload-cmd (format "dd if=/dev/zero bs=%d count=1 2>/dev/null | tr '\\0' x"
                              payload-size))
         (output (make-hash-table :test 'eq)))
    (tramp-rpc-stress-remote-test--with-processes procs
      (dotimes (i n)
        (let ((proc (make-process
                      :name (format "stress-race-%d" i)
                      :buffer nil
                      :command (list "/bin/sh" "-c" payload-cmd)
                      :connection-type 'pipe
                      :coding 'binary
                      :noquery t
                      :file-handler t
                      :filter (lambda (p s)
                                (puthash p (+ (gethash p output 0) (length s))
                                         output)))))
          (push proc procs)))

      (let* ((result (tramp-rpc-stress-remote-test--wait-all procs 120))
             (hung (plist-get result :hung)))
        (when hung
          (ert-fail (format "%d/%d processes hung waiting for exit" (length hung) n))))

      ;; Drain until all expected bytes arrive.  Output notifications for a
      ;; just-exited process may still be in-flight; adaptive draining
      ;; distinguishes "delayed" from "permanently lost".
      (let ((drain-deadline (+ (float-time) 15.0)))
        (while (and (> drain-deadline (float-time))
                    (cl-some (lambda (p)
                               (< (gethash p output 0) payload-size))
                             procs))
          (accept-process-output nil 0.1)))

      (let (short)
        (dolist (p procs)
          (let ((got (gethash p output 0)))
            (unless (= got payload-size)
              (push (format "%s: got %d/%d bytes" (process-name p) got payload-size)
                    short))))
        (when short
          (ert-fail (format "%d/%d processes lost output bytes after exit:\n%s"
                            (length short) n
                            (mapconcat #'identity (seq-take short 5) "\n"))))))))

(ert-deftest tramp-rpc-stress-remote-test-concurrent-large-output ()
  "20 concurrent processes × 256 KB each: verify exact byte count per stream.
Tests notification multiplexing when many large streams are active
simultaneously.  A polling implementation with short read windows will
truncate streams; a correct push implementation delivers every byte."
  :tags '(:stress :process)
  (skip-unless (tramp-rpc-stress-remote-test--enabled))
  (let* ((default-directory (tramp-rpc-stress-remote-test--remote-dir))
         (n 20)
         (per-process-bytes (* 256 1024))
         (cmd (format "dd if=/dev/zero bs=1024 count=256 2>/dev/null | tr '\\0' x"))
         (output (make-hash-table :test 'eq)))
    (tramp-rpc-stress-remote-test--with-processes procs
      (dotimes (i n)
        (let ((proc (make-process
                      :name (format "stress-large-%d" i)
                      :buffer nil
                      :command (list "/bin/sh" "-c" cmd)
                      :connection-type 'pipe
                      :coding 'binary
                      :noquery t
                      :file-handler t
                      :filter (lambda (p s)
                                (puthash p (+ (gethash p output 0) (length s))
                                         output)))))
          (push proc procs)))

      (let* ((result (tramp-rpc-stress-remote-test--wait-all procs 180))
             (hung (plist-get result :hung)))
        (when hung
          (ert-fail (format "%d/%d large-output processes hung" (length hung) n))))

      ;; Drain until all expected bytes arrive or a hard deadline is hit.
      ;; A fixed sleep can mask true losses if bytes are merely delayed;
      ;; looping to completion distinguishes "delayed" from "lost".
      (let ((drain-deadline (+ (float-time) 30.0)))
        (while (and (> drain-deadline (float-time))
                    (cl-some (lambda (p)
                               (< (gethash p output 0) per-process-bytes))
                             procs))
          (accept-process-output nil 0.1)))

      (let (wrong)
        (dolist (p procs)
          (let ((got (gethash p output 0)))
            (unless (= got per-process-bytes)
              (push (format "%s: %d/%d bytes" (process-name p) got per-process-bytes)
                    wrong))))
        (when wrong
          (ert-fail (format "%d/%d streams had wrong byte count:\n%s"
                            (length wrong) n
                            (mapconcat #'identity (seq-take wrong 5) "\n"))))))))

(ert-deftest tramp-rpc-stress-remote-test-rapid-short-lived ()
  "200 `true' processes: none must hang.
Verifies cleanup path under high churn — the subscriber task must stop
cleanly for every process even when exit happens before subscribe fires."
  :tags '(:stress :process)
  (skip-unless (tramp-rpc-stress-remote-test--enabled))
  (let* ((default-directory (tramp-rpc-stress-remote-test--remote-dir))
         (n 200))
    (tramp-rpc-stress-remote-test--with-processes procs
      (dotimes (i n)
        (push (make-process
               :name (format "stress-rapid-%d" i)
               :buffer nil
               :command '("true")
               :connection-type 'pipe
               :noquery t
               :file-handler t)
              procs))

      (let* ((result (tramp-rpc-stress-remote-test--wait-all procs 120))
             (hung (plist-get result :hung)))
        (when hung
          (ert-fail (format "%d/%d short-lived processes hung" (length hung) n))))

      (dolist (p procs)
        (should (= 0 (process-exit-status p)))))))

(ert-deftest tramp-rpc-stress-remote-test-stdin-output-verified ()
  "30 concurrent cat processes: verify output bytes equal input bytes sent.
The previous stdin test only checked exit status.  This test writes a
unique payload to each process and confirms the filter received every byte."
  :tags '(:stress :process)
  (skip-unless (tramp-rpc-stress-remote-test--enabled))
  (let* ((default-directory (tramp-rpc-stress-remote-test--remote-dir))
         (n 30)
         (output (make-hash-table :test 'eq))
         (payloads (make-hash-table :test 'eq)))
    (tramp-rpc-stress-remote-test--with-processes procs
      (dotimes (i n)
        (let* ((payload (format "payload-%06d-%s\n" i (make-string 64 ?x)))
               (proc (make-process
                       :name (format "stress-stdin-%d" i)
                       :buffer nil
                       :command '("cat")
                       :connection-type 'pipe
                       :noquery t
                       :file-handler t
                       :filter (lambda (p s)
                                 (puthash p (concat (gethash p output "") s)
                                          output)))))
          (puthash proc payload payloads)
          (push proc procs)
          (process-send-string proc payload)
          (process-send-eof proc)))

      (let* ((result (tramp-rpc-stress-remote-test--wait-all procs 120))
             (hung (plist-get result :hung)))
        (when hung
          (ert-fail (format "%d/%d cat processes hung" (length hung) n))))

      ;; Drain until all expected payloads arrive or a hard deadline is hit.
      (let ((drain-deadline (+ (float-time) 15.0)))
        (while (and (> drain-deadline (float-time))
                    (cl-some (lambda (p)
                               (not (equal (gethash p output "")
                                           (gethash p payloads))))
                             procs))
          (accept-process-output nil 0.05)))

      (let (failures)
        (dolist (p procs)
          (let ((expected (gethash p payloads))
                (got (gethash p output "")))
            (unless (equal got expected)
              (push (format "%s: expected %d bytes got %d bytes"
                            (process-name p) (length expected) (length got))
                    failures))))
        (when failures
          (ert-fail (format "%d/%d stdin processes had wrong output:\n%s"
                            (length failures) n
                            (mapconcat #'identity (seq-take failures 5) "\n"))))))))

(ert-deftest tramp-rpc-stress-remote-test-exit-codes-concurrent ()
  "40 concurrent processes with distinct exit codes: all must propagate correctly.
Stronger than the previous 5-process version — forces the server to
deliver 40 simultaneous exit notifications and checks every sentinel fires."
  :tags '(:stress :process)
  (skip-unless (tramp-rpc-stress-remote-test--enabled))
  (let* ((default-directory (tramp-rpc-stress-remote-test--remote-dir))
         (codes (number-sequence 0 39))
         (n (length codes))
         (recorded (make-hash-table :test 'eq))
         procs)
    (tramp-rpc-stress-remote-test--with-processes procs
      (dolist (code codes)
        (push (make-process
               :name (format "stress-ec-%d" code)
               :buffer nil
               :command (list "/bin/sh" "-c" (format "exit %d" code))
               :connection-type 'pipe
               :noquery t
               :file-handler t
               :sentinel (let ((c code))
                           (lambda (p _ev)
                             (unless (process-live-p p)
                               (puthash c (process-exit-status p) recorded)))))
              procs))

      (let* ((result (tramp-rpc-stress-remote-test--wait-all procs 120))
             (hung (plist-get result :hung)))
        (when hung
          (ert-fail (format "%d/%d exit-code processes hung" (length hung) n))))

      ;; Drain until all sentinels have fired or a hard deadline is hit.
      ;; Sentinels for just-exited processes may still be in-flight.
      (let ((drain-deadline (+ (float-time) 10.0)))
        (while (and (> drain-deadline (float-time))
                    (< (hash-table-count recorded) n))
          (accept-process-output nil 0.05)))

      (let (failures)
        (dolist (code codes)
          (let ((got (gethash code recorded :missing)))
            (unless (eql got code)
              (push (format "code %d: sentinel recorded %S" code got) failures))))
        (when failures
          (ert-fail (format "%d/%d exit codes wrong:\n%s"
                            (length failures) n
                            (mapconcat #'identity failures "\n"))))))))

(ert-deftest tramp-rpc-stress-remote-test-mixed-workload ()
  "Mixed concurrent load: fast-exit, large-output, stdin-cat, all simultaneous.
Tests the subscriber model under a heterogeneous notification stream.
A broken implementation typically fails one class while handling others."
  :tags '(:stress :process)
  (skip-unless (tramp-rpc-stress-remote-test--enabled))
  (let* ((default-directory (tramp-rpc-stress-remote-test--remote-dir))
         (output (make-hash-table :test 'eq))
         (expected (make-hash-table :test 'eq))
         all-procs)
    (tramp-rpc-stress-remote-test--with-processes all-procs
      ;; Group A: 40 fast-exit processes echoing unique tokens.
      (dotimes (i 40)
        (let* ((token (format "fast%06d" i))
               (proc (make-process
                       :name (format "stress-mix-fast-%d" i)
                       :buffer nil
                       :command (list "/bin/sh" "-c" (format "printf '%%s' '%s'" token))
                       :connection-type 'pipe
                       :noquery t
                       :file-handler t
                       :filter (lambda (p s)
                                 (puthash p (concat (gethash p output "") s) output)))))
          (puthash proc token expected)
          (push proc all-procs)))

      ;; Group B: 5 large-output processes × 128 KB.
      (let ((large-bytes (* 128 1024))
            (large-cmd "dd if=/dev/zero bs=1024 count=128 2>/dev/null | tr '\\0' y"))
        (dotimes (i 5)
          (let ((proc (make-process
                        :name (format "stress-mix-large-%d" i)
                        :buffer nil
                        :command (list "/bin/sh" "-c" large-cmd)
                        :connection-type 'pipe
                        :coding 'binary
                        :noquery t
                        :file-handler t
                        :filter (lambda (p s)
                                  (puthash p (+ (gethash p output 0) (length s)) output)))))
            (puthash proc large-bytes expected)
            (push proc all-procs))))

      ;; Group C: 15 stdin-cat processes with unique payloads.
      (dotimes (i 15)
        (let* ((payload (format "cat%06d-%s\n" i (make-string 32 ?z)))
               (proc (make-process
                       :name (format "stress-mix-cat-%d" i)
                       :buffer nil
                       :command '("cat")
                       :connection-type 'pipe
                       :noquery t
                       :file-handler t
                       :filter (lambda (p s)
                                 (puthash p (concat (gethash p output "") s) output)))))
          (puthash proc payload expected)
          (push proc all-procs)
          (process-send-string proc payload)
          (process-send-eof proc)))

      (let* ((result (tramp-rpc-stress-remote-test--wait-all all-procs 180))
             (hung (plist-get result :hung)))
        (when hung
          (ert-fail (format "%d/%d mixed-workload processes hung"
                            (length hung) (length all-procs)))))

      ;; Drain until all expected output arrives or a hard deadline is hit.
      (let ((drain-deadline (+ (float-time) 30.0)))
        (while (and (> drain-deadline (float-time))
                    (cl-some (lambda (proc)
                               (let ((exp (gethash proc expected))
                                     (got (gethash proc output)))
                                 (cond
                                  ((integerp exp) (not (eql got exp)))
                                  ((stringp exp) (not (equal got exp)))
                                  (t nil))))
                             all-procs))
          (accept-process-output nil 0.1)))

      ;; Verify every process got the right amount/content.
      (let (failures)
        (maphash
         (lambda (proc exp)
           (let ((got (gethash proc output)))
             (cond
              ((integerp exp)
               (unless (eql got exp)
                 (push (format "%s: expected %d bytes got %S"
                               (process-name proc) exp got)
                       failures)))
              ((stringp exp)
               (unless (equal got exp)
                 (push (format "%s: expected %d bytes got %d bytes"
                               (process-name proc) (length exp) (length (or got "")))
                       failures))))))
         expected)
        (when failures
          (ert-fail (format "%d output failures in mixed workload:\n%s"
                            (length failures)
                            (mapconcat #'identity (seq-take failures 10) "\n"))))))))

(ert-deftest tramp-rpc-stress-remote-test-kill-while-output-flows ()
  "Kill half of 30 active streaming processes mid-stream; survivors must complete.
Verifies that killing a subscribed process does not corrupt the notification
stream for other processes sharing the same connection."
  :tags '(:stress :process)
  (skip-unless (tramp-rpc-stress-remote-test--enabled))
  (let* ((default-directory (tramp-rpc-stress-remote-test--remote-dir))
         (n 30)
         ;; seq 1 N produces N lines; large enough that the process is still
         ;; running when we kill it.
         (stream-cmd "seq 1 50000")
         (output (make-hash-table :test 'eq)))
    (tramp-rpc-stress-remote-test--with-processes procs
      (dotimes (i n)
        (let ((proc (make-process
                      :name (format "stress-kill-flow-%d" i)
                      :buffer nil
                      :command (list "/bin/sh" "-c" stream-cmd)
                      :connection-type 'pipe
                      :coding 'binary
                      :noquery t
                      :file-handler t
                      :filter (lambda (p s)
                                (puthash p (+ (gethash p output 0) (length s)) output)))))
          (push proc procs)))
      (setq procs (nreverse procs))

      ;; Wait until at least one process has produced output, meaning the
      ;; streaming has started.  seq 1 50000 on a fast host can finish quickly,
      ;; so we do not rely on a fixed sleep — instead we poll until output
      ;; arrives or a generous deadline passes.
      (let ((stream-deadline (+ (float-time) 10.0)))
        (while (and (> stream-deadline (float-time))
                    (cl-every (lambda (p) (= (gethash p output 0) 0)) procs))
          (accept-process-output nil 0.05)))

      ;; Kill the even-indexed half while they are (or were) streaming.
      (cl-loop for p in procs for i from 0
               when (cl-evenp i) do (ignore-errors (delete-process p)))

      (let* ((survivors (cl-loop for p in procs for i from 0
                                 when (cl-oddp i) collect p))
             (result (tramp-rpc-stress-remote-test--wait-all survivors 120))
             (hung (plist-get result :hung)))
        (when hung
          (ert-fail (format "%d/%d surviving processes hung after sibling kill"
                            (length hung) (length survivors))))

        ;; Drain briefly so any in-flight filter calls from survivors run.
        (let ((drain-deadline (+ (float-time) 5.0)))
          (while (and (> drain-deadline (float-time))
                      (cl-some (lambda (p) (= (gethash p output 0) 0))
                               survivors))
            (accept-process-output nil 0.05)))

        ;; Survivors must have received a non-zero number of bytes —
        ;; killing siblings must not starve the survivors' notification delivery.
        (let (starved)
          (dolist (p survivors)
            (let ((got (gethash p output 0)))
              (when (= got 0)
                (push (process-name p) starved))))
          (when starved
            (ert-fail (format "%d/%d survivors received zero bytes after sibling kill: %s"
                              (length starved) (length survivors)
                              (mapconcat #'identity starved ", ")))))))))

;;; tramp-rpc-stress-remote-tests.el ends here
