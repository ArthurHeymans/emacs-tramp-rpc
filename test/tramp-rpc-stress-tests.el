;;; tramp-rpc-stress-tests.el --- Stress tests for the subscriber model -*- lexical-binding: t -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>

;; This file is part of tramp-rpc.

;;; Commentary:

;; End-to-end stress tests for the process subscriber model.  These tests run
;; the real server binary and verify that:
;;
;;   - process.output and process.exit notifications are delivered correctly.
;;   - No hangs or deadlocks occur under concurrent subscriptions.
;;   - All bytes arrive when a subscribed process produces large output.
;;   - Rapid subscribe/unsubscribe cycles do not leave orphaned state.
;;   - Many concurrently-subscribed processes all receive exit notifications.
;;
;; The tests communicate with the server through its stdin/stdout pipe using
;; the same length-prefixed MessagePack-RPC framing as production connections.
;;
;; Run:
;;   emacs -Q --batch -l test/tramp-rpc-stress-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; Or via the test runner:
;;   ./test/run-tests.sh --stress

;;; Code:

(require 'ert)
(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; Shared setup (mirrors tramp-rpc-mock-tests.el but with its own variables)
;; ---------------------------------------------------------------------------

(defvar tramp-rpc-stress-test--project-root
  (expand-file-name "../" (file-name-directory
                           (or load-file-name buffer-file-name
                               (expand-file-name "test/tramp-rpc-stress-tests.el"))))
  "Project root directory.")

(let ((lisp-dir (expand-file-name "lisp" tramp-rpc-stress-test--project-root))
      (source (getenv "TRAMP_SOURCE")))
  (add-to-list 'load-path lisp-dir)
  (when (and (not (string-empty-p (or source "")))
             (file-directory-p (expand-file-name "lisp" source)))
    (add-to-list 'load-path (expand-file-name "lisp" source))))

;; Prefer MSGPACK_SOURCE (env var) or the standard elpaca checkout over the
;; system ELPA package; the latter may be an old version that does not support
;; msgpack-read keyword arguments.
(let ((msgpack-source
       (or (getenv "MSGPACK_SOURCE")
           ;; Common elpaca location as a sensible default.
           (let ((elpaca-path (expand-file-name
                               "../../msgpack"
                               tramp-rpc-stress-test--project-root)))
             (when (file-directory-p elpaca-path) elpaca-path)))))
  (when (and msgpack-source (not (string-empty-p msgpack-source)))
    (add-to-list 'load-path msgpack-source)))

(unless (or (require 'msgpack nil t)
            (progn (require 'package)
                   (package-initialize)
                   (require 'msgpack nil t)))
  (error "tramp-rpc stress tests require msgpack.el"))

(require 'tramp-rpc-protocol)

;; ---------------------------------------------------------------------------
;; Server lifecycle helpers (independent of the mock-test variables)
;; ---------------------------------------------------------------------------

(defvar tramp-rpc-stress-test--server-process nil)
(defvar tramp-rpc-stress-test--server-buffer nil)
(defvar tramp-rpc-stress-test--temp-dir nil)
;; Notifications that arrived during a blocking --rpc-call and must be
;; replayed by --collect-notifications / --drain-messages.
(defvar tramp-rpc-stress-test--pending-notifications nil)

(defun tramp-rpc-stress-test--find-server ()
  "Return the path to the RPC server binary, or nil."
  (cl-find-if #'file-executable-p
              (mapcar (lambda (rel)
                        (expand-file-name rel tramp-rpc-stress-test--project-root))
                      '("server/target/release/tramp-rpc-server"
                        "target/release/tramp-rpc-server"
                        "target/x86_64-unknown-linux-musl/release/tramp-rpc-server"
                        "server/target/debug/tramp-rpc-server"
                        "target/debug/tramp-rpc-server"))))

(defun tramp-rpc-stress-test--start-server ()
  "Start a local RPC server process for stress testing."
  (let ((server (tramp-rpc-stress-test--find-server)))
    (unless server
      (error "No RPC server found; build with 'cargo build --release'"))
    (setq tramp-rpc-stress-test--temp-dir
          (make-temp-file "tramp-rpc-stress" t))
    (setq tramp-rpc-stress-test--server-buffer
          (generate-new-buffer "*tramp-rpc-stress-server*"))
    (with-current-buffer tramp-rpc-stress-test--server-buffer
      (set-buffer-multibyte nil)
      (set-marker (mark-marker) (point-min)))
    (setq tramp-rpc-stress-test--server-process
          (let ((process-connection-type nil))
            (start-process "stress-server"
                           tramp-rpc-stress-test--server-buffer
                           server)))
    (set-process-query-on-exit-flag tramp-rpc-stress-test--server-process nil)
    (set-process-coding-system tramp-rpc-stress-test--server-process
                                'binary 'binary)
    (set-process-filter
     tramp-rpc-stress-test--server-process
     (lambda (process output)
       (when (buffer-live-p (process-buffer process))
         (with-current-buffer (process-buffer process)
           (goto-char (point-max))
           (insert output)))))
    (sleep-for 0.1)
    tramp-rpc-stress-test--server-process))

(defun tramp-rpc-stress-test--stop-server ()
  "Terminate the stress-test server and clean up."
  (when (and tramp-rpc-stress-test--server-process
             (process-live-p tramp-rpc-stress-test--server-process))
    (delete-process tramp-rpc-stress-test--server-process))
  (when (buffer-live-p tramp-rpc-stress-test--server-buffer)
    (kill-buffer tramp-rpc-stress-test--server-buffer))
  (when (and tramp-rpc-stress-test--temp-dir
             (file-directory-p tramp-rpc-stress-test--temp-dir))
    (delete-directory tramp-rpc-stress-test--temp-dir t))
  (setq tramp-rpc-stress-test--server-process nil
        tramp-rpc-stress-test--server-buffer nil
        tramp-rpc-stress-test--temp-dir nil
        tramp-rpc-stress-test--pending-notifications nil))

;; ---------------------------------------------------------------------------
;; Low-level I/O helpers
;; ---------------------------------------------------------------------------

(defun tramp-rpc-stress-test--send (method params)
  "Send a request for METHOD with PARAMS; return the request ID."
  (unless (process-live-p tramp-rpc-stress-test--server-process)
    (error "Stress-test server not running"))
  (let* ((id-and-bytes (tramp-rpc-protocol-encode-request-with-id method params))
         (id (car id-and-bytes))
         (bytes (cdr id-and-bytes)))
    (process-send-string tramp-rpc-stress-test--server-process bytes)
    id))

(defun tramp-rpc-stress-test--try-read-one ()
  "Try to read one complete message from the server buffer.
Returns the decoded plist or nil if no complete message is available.
Advances the buffer's mark past any consumed frame."
  (with-current-buffer tramp-rpc-stress-test--server-buffer
    (let ((msg (tramp-rpc-protocol-try-read-message
                tramp-rpc-stress-test--server-buffer)))
      (when msg
        ;; Trim consumed bytes so the buffer doesn't grow unboundedly.
        (delete-region (point-min) (mark-marker))
        (set-marker (mark-marker) (point-min)))
      msg)))

(defun tramp-rpc-stress-test--drain-messages (timeout-secs)
  "Read all available messages within TIMEOUT-SECS seconds.
Drains any notifications stashed by `tramp-rpc-stress-test--rpc-call' first.
Returns a plist:
  :responses     – alist mapping request-id → response plist
  :notifications – list of notification plists (newest last)"
  (let ((responses (make-hash-table :test 'eql))
        (notifications nil)
        (deadline (+ (float-time) timeout-secs)))
    ;; Replay notifications captured during earlier blocking RPC calls.
    (dolist (msg (nreverse tramp-rpc-stress-test--pending-notifications))
      (push msg notifications))
    (setq tramp-rpc-stress-test--pending-notifications nil)
    (while (> deadline (float-time))
      (let ((msg (tramp-rpc-stress-test--try-read-one)))
        (if msg
            ;; Got a message – classify and continue immediately.
            (if (plist-get msg :notification)
                (push msg notifications)
              (when-let* ((id (plist-get msg :id)))
                (puthash id msg responses)))
          ;; No complete message yet – yield briefly.
          (accept-process-output tramp-rpc-stress-test--server-process 0.02))))
    (list :responses responses
          :notifications (nreverse notifications))))

(defun tramp-rpc-stress-test--rpc-call (method params &optional timeout)
  "Perform a synchronous RPC call; return result or signal error.
TIMEOUT defaults to 5 seconds."
  (let* ((id (tramp-rpc-stress-test--send method params))
         (timeout (or timeout 5.0))
         (deadline (+ (float-time) timeout))
         response)
    (while (and (not response) (> deadline (float-time)))
      (let ((msg (tramp-rpc-stress-test--try-read-one)))
        (cond
         ((null msg)
          (accept-process-output tramp-rpc-stress-test--server-process 0.05))
         ((plist-get msg :notification)
          ;; Stash notifications so they are not lost when they race ahead of
          ;; the response we are waiting for.  --collect-notifications and
          ;; --drain-messages drain this list before reading new messages.
          (push msg tramp-rpc-stress-test--pending-notifications))
         ((eql (plist-get msg :id) id)
          (setq response msg))
         (t
          ;; Response for a different in-flight request – ignore in stress tests.
          nil))))
    (unless response
      (error "Timeout waiting for response to %s (id=%s)" method id))
    (if (tramp-rpc-protocol-error-p response)
        (error "RPC error from %s: %s"
               method (tramp-rpc-protocol-error-message response))
      (plist-get response :result))))

;; ---------------------------------------------------------------------------
;; Notification-aware RPC helpers
;; ---------------------------------------------------------------------------

(defun tramp-rpc-stress-test--start-process (script)
  "Start a remote shell process running SCRIPT; return its server-side pid."
  (let ((result (tramp-rpc-stress-test--rpc-call
                 "process.start"
                 `((cmd . "/bin/sh")
                   (args . ["-c" ,script])))))
    (alist-get 'pid result)))

(defun tramp-rpc-stress-test--subscribe (pid)
  "Subscribe to push notifications for server PID."
  (tramp-rpc-stress-test--rpc-call "process.subscribe" `((pid . ,pid))))

(defun tramp-rpc-stress-test--unsubscribe (pid)
  "Stop push notifications for server PID."
  (tramp-rpc-stress-test--rpc-call "process.unsubscribe" `((pid . ,pid))))

(defun tramp-rpc-stress-test--handle-notification (msg stdout-map exit-map pending)
  "Process one notification MSG; update STDOUT-MAP and EXIT-MAP.
Returns the updated PENDING pid list."
  (let* ((method (plist-get msg :method))
         (params (plist-get msg :params))
         (pid (alist-get 'pid params)))
    (cond
     ((equal method "process.output")
      (when-let* ((raw (alist-get 'stdout params))
                  (bytes (if (msgpack-bin-p raw)
                             (msgpack-bin-string raw)
                           raw)))
        (puthash pid
                 (concat (gethash pid stdout-map "") bytes)
                 stdout-map)))
     ((equal method "process.exit")
      (puthash pid (alist-get 'exit_code params) exit-map)
      (setq pending (delq pid pending))))
    pending))

(defun tramp-rpc-stress-test--collect-notifications (pids timeout-secs)
  "Wait for process.exit notifications for all PIDS within TIMEOUT-SECS.
Drains any notifications stashed by `tramp-rpc-stress-test--rpc-call' first.
Returns a plist:
  :stdout  – alist pid → concatenated stdout bytes
  :exit    – alist pid → exit-code (integer)
  :missing – list of pids for which no exit notification arrived"
  (let ((stdout-map (make-hash-table :test 'eql))
        (exit-map (make-hash-table :test 'eql))
        (pending (copy-sequence pids))
        (deadline (+ (float-time) timeout-secs)))
    (dolist (pid pids)
      (puthash pid "" stdout-map))
    ;; Replay notifications stashed during earlier blocking RPC calls so that
    ;; exit events that raced ahead of the subscribe response are not lost.
    (dolist (msg (nreverse tramp-rpc-stress-test--pending-notifications))
      (when (plist-get msg :notification)
        (setq pending
              (tramp-rpc-stress-test--handle-notification
               msg stdout-map exit-map pending))))
    (setq tramp-rpc-stress-test--pending-notifications nil)
    (while (and pending (> deadline (float-time)))
      (let ((msg (tramp-rpc-stress-test--try-read-one)))
        (if (null msg)
            (accept-process-output tramp-rpc-stress-test--server-process 0.02)
          (when (plist-get msg :notification)
            (setq pending
                  (tramp-rpc-stress-test--handle-notification
                   msg stdout-map exit-map pending))))))
    (list :stdout stdout-map
          :exit exit-map
          :missing pending)))

;; ---------------------------------------------------------------------------
;; Convenience assertion helpers
;; ---------------------------------------------------------------------------

(defmacro tramp-rpc-stress-test--with-server (&rest body)
  "Wrap BODY in server start/stop with unwind-protect."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (tramp-rpc-stress-test--start-server)
         ,@body)
     (tramp-rpc-stress-test--stop-server)))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(ert-deftest tramp-rpc-stress-test-subscribe-single-exit-notification ()
  "Subscribe to one process; verify exactly one process.exit notification."
  (skip-unless (tramp-rpc-stress-test--find-server))
  (tramp-rpc-stress-test--with-server
    (let* ((pid (tramp-rpc-stress-test--start-process "exit 0"))
           (_ (tramp-rpc-stress-test--subscribe pid))
           ;; Drain with an explicit timeout so duplicate exits are visible.
           (exit-count 0)
           (result
            ;; We use --drain-messages directly so we can count every
            ;; process.exit notification, including duplicates that
            ;; --collect-notifications would discard after the first.
            (let* ((drained (tramp-rpc-stress-test--drain-messages 5.0))
                   (notifications (plist-get drained :notifications)))
              (dolist (msg notifications)
                (when (and (equal (plist-get msg :method) "process.exit")
                           (eql (alist-get 'pid (plist-get msg :params)) pid))
                  (cl-incf exit-count)))
              notifications)))
      (should (= exit-count 1))
      (let ((exit-notif (cl-find-if
                         (lambda (m)
                           (and (equal (plist-get m :method) "process.exit")
                                (eql (alist-get 'pid (plist-get m :params)) pid)))
                         result)))
        (should exit-notif)
        (should (= 0 (alist-get 'exit_code (plist-get exit-notif :params))))))))

(ert-deftest tramp-rpc-stress-test-subscribe-exit-code-propagated ()
  "Exit code from a subscribed process is delivered in process.exit."
  (skip-unless (tramp-rpc-stress-test--find-server))
  (tramp-rpc-stress-test--with-server
    (let* ((pid (tramp-rpc-stress-test--start-process "exit 42"))
           (_ (tramp-rpc-stress-test--subscribe pid))
           (result (tramp-rpc-stress-test--collect-notifications
                    (list pid) 5.0)))
      (should (null (plist-get result :missing)))
      (should (= 42 (gethash pid (plist-get result :exit)))))))

(ert-deftest tramp-rpc-stress-test-subscribe-stdout-delivered ()
  "Output from a subscribed process arrives as process.output notifications."
  (skip-unless (tramp-rpc-stress-test--find-server))
  (tramp-rpc-stress-test--with-server
    (let* ((pid (tramp-rpc-stress-test--start-process
                 "printf 'hello-stress'; exit 0"))
           (_ (tramp-rpc-stress-test--subscribe pid))
           (result (tramp-rpc-stress-test--collect-notifications
                    (list pid) 5.0)))
      (should (null (plist-get result :missing)))
      (should (string-match-p "hello-stress"
                              (gethash pid (plist-get result :stdout)))))))

(ert-deftest tramp-rpc-stress-test-subscribe-many-concurrent-processes ()
  "Subscribe to 15 concurrent processes; all must receive exit notifications."
  (skip-unless (tramp-rpc-stress-test--find-server))
  (tramp-rpc-stress-test--with-server
    (let* ((n 15)
           (pids (mapcar (lambda (i)
                           (tramp-rpc-stress-test--start-process
                            (format "printf 'proc%d'; exit 0" i)))
                         (number-sequence 0 (1- n)))))
      ;; Subscribe to every process.
      (dolist (pid pids)
        (tramp-rpc-stress-test--subscribe pid))
      ;; Collect notifications with a generous timeout.
      (let ((result (tramp-rpc-stress-test--collect-notifications pids 30.0)))
        (should (null (plist-get result :missing))
                )
        ;; All exit codes must be 0.
        (dolist (pid pids)
          (should (= 0 (gethash pid (plist-get result :exit)))))))))

(ert-deftest tramp-rpc-stress-test-subscribe-large-output-arrives-completely ()
  "A process producing ~256 KiB of output via subscribe delivers all bytes."
  (skip-unless (tramp-rpc-stress-test--find-server))
  (tramp-rpc-stress-test--with-server
    ;; 256 * 1024 = 262144 bytes of 'x'.
    (let* ((pid (tramp-rpc-stress-test--start-process
                 "dd if=/dev/zero bs=1024 count=256 2>/dev/null | tr '\\0' 'x'"))
           (_ (tramp-rpc-stress-test--subscribe pid))
           (result (tramp-rpc-stress-test--collect-notifications
                    (list pid) 30.0)))
      (should (null (plist-get result :missing)))
      (let ((out (gethash pid (plist-get result :stdout))))
        (should (= 262144 (length out)))
        (should (string-match-p "^x+$" out))))))

(ert-deftest tramp-rpc-stress-test-unsubscribe-then-resubscribe ()
  "Unsubscribe and re-subscribe; the second subscription delivers exit notification."
  (skip-unless (tramp-rpc-stress-test--find-server))
  (tramp-rpc-stress-test--with-server
    ;; Process stays alive for a bit so we can unsubscribe before it exits.
    (let* ((pid (tramp-rpc-stress-test--start-process
                 "sleep 2; exit 0")))
      (tramp-rpc-stress-test--subscribe pid)
      ;; Unsubscribe immediately – the task should stop.
      (tramp-rpc-stress-test--unsubscribe pid)
      ;; Re-subscribe.
      (tramp-rpc-stress-test--subscribe pid)
      ;; Now collect; we should still see the exit notification from the
      ;; second subscription.
      (let ((result (tramp-rpc-stress-test--collect-notifications
                     (list pid) 10.0)))
        (should (null (plist-get result :missing)))
        (should (= 0 (gethash pid (plist-get result :exit))))))))

(ert-deftest tramp-rpc-stress-test-rapid-subscribe-unsubscribe-no-hang ()
  "Rapidly subscribe and unsubscribe 8 times without hanging."
  (skip-unless (tramp-rpc-stress-test--find-server))
  (tramp-rpc-stress-test--with-server
    (let* ((pid (tramp-rpc-stress-test--start-process "sleep 30")))
      (dotimes (_ 8)
        (tramp-rpc-stress-test--subscribe pid)
        (tramp-rpc-stress-test--unsubscribe pid))
      ;; Kill the long-sleeping process and verify it is no longer listed,
      ;; proving the server is responsive and cleaned up state correctly.
      (tramp-rpc-stress-test--rpc-call
       "process.kill"
       `((pid . ,pid) (signal . 9)))
      ;; Wait for the killed process to actually disappear from process.list.
      (let ((deadline (+ (float-time) 5.0))
            still-listed)
        (while (and (> deadline (float-time))
                    (progn
                      (setq still-listed
                            (let ((list-result
                                   (tramp-rpc-stress-test--rpc-call
                                    "process.list" nil)))
                              (cl-some (lambda (entry)
                                         (eql (alist-get 'pid entry) pid))
                                       list-result)))
                      still-listed))
          (sleep-for 0.05))
        ;; The process must have exited and been removed from the server list.
        (should-not still-listed)))))

(ert-deftest tramp-rpc-stress-test-subscribe-write-then-exit ()
  "Write to stdin of a subscribed process; all stdout bytes arrive."
  (skip-unless (tramp-rpc-stress-test--find-server))
  (tramp-rpc-stress-test--with-server
    ;; cat echoes stdin to stdout.
    (let* ((pid (tramp-rpc-stress-test--start-process "cat"))
           (_ (tramp-rpc-stress-test--subscribe pid))
           (payload (make-string 4096 ?A)))
      (tramp-rpc-stress-test--rpc-call
       "process.write"
       `((pid . ,pid)
         (data . ,(msgpack-bin-make payload))))
      (tramp-rpc-stress-test--rpc-call
       "process.close_stdin"
       `((pid . ,pid)))
      (let ((result (tramp-rpc-stress-test--collect-notifications
                     (list pid) 10.0)))
        (should (null (plist-get result :missing)))
        (should (= 4096
                   (length (gethash pid (plist-get result :stdout)))))))))

(ert-deftest tramp-rpc-stress-test-subscribe-many-mixed-exit-codes ()
  "N concurrent processes with distinct exit codes all deliver correct codes."
  (skip-unless (tramp-rpc-stress-test--find-server))
  (tramp-rpc-stress-test--with-server
    (let* ((codes (number-sequence 0 9))
           (pid-code-alist
            (mapcar (lambda (code)
                      (cons (tramp-rpc-stress-test--start-process
                             (format "exit %d" code))
                            code))
                    codes)))
      (dolist (entry pid-code-alist)
        (tramp-rpc-stress-test--subscribe (car entry)))
      (let* ((pids (mapcar #'car pid-code-alist))
             (result (tramp-rpc-stress-test--collect-notifications pids 15.0)))
        (should (null (plist-get result :missing)))
        (dolist (entry pid-code-alist)
          (let ((pid (car entry))
                (expected (cdr entry)))
            (should (= expected
                       (gethash pid (plist-get result :exit))))))))))

;;; ---------------------------------------------------------------------------
;;; Test runner entry-point
;;; ---------------------------------------------------------------------------

(defun tramp-rpc-stress-test-run-all ()
  "Run all stress tests and exit with appropriate status code."
  (ert-run-tests-batch-and-exit "^tramp-rpc-stress-test-"))

(provide 'tramp-rpc-stress-tests)
;;; tramp-rpc-stress-tests.el ends here
