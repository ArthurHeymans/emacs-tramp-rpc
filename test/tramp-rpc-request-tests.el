;;; tramp-rpc-request-tests.el --- Request lifecycle tests -*- lexical-binding: t; -*-
;; Assisted-by: various LLMs

(require 'ert)
(require 'cl-lib)
(require 'tramp-rpc)

(declare-function tramp-rpc--invalidate-timed-out-connection "tramp-rpc-transport"
                  (process vec event))
(declare-function tramp-rpc-mock-test--wait-for "tramp-rpc-mock-tests"
                  (predicate description &optional process))

(defun tramp-rpc-mock-test-request--vec ()
  "Return a harmless vector for lifecycle diagnostics."
  (tramp-dissect-file-name "/rpc:request-test:/tmp/"))

(defun tramp-rpc-mock-test-request--connection ()
  "Return an attached connection generation for request lifecycle tests.
The generation's transport is a live pipe process on a fresh buffer."
  (let* ((buffer (generate-new-buffer " *tramp-rpc-mock-test-request*"))
         (process (make-pipe-process :name "tramp-rpc-mock-test-request"
                                     :buffer buffer :noquery t)))
    (tramp-rpc--attach-connection
     (tramp-rpc--make-connection
      :process process :buffer buffer
      :vec (tramp-rpc-mock-test-request--vec)))))

(defmacro tramp-rpc-mock-test-request--with-connection (spec &rest body)
  "Run BODY with a disposable generation bound to PROCESS, BUFFER and CONNECTION.
SPEC is (PROCESS BUFFER [CONNECTION]); CONNECTION defaults to `connection'."
  (declare (indent 1) (debug t))
  (let ((process (nth 0 spec))
        (buffer (nth 1 spec))
        (connection (or (nth 2 spec) 'connection)))
    `(let* ((,connection (tramp-rpc-mock-test-request--connection))
            (,process (tramp-rpc-connection-process ,connection))
            (,buffer (tramp-rpc-connection-buffer ,connection)))
       (unwind-protect
           (progn ,@body)
         (when (process-live-p ,process)
           (delete-process ,process))
         (when (buffer-live-p ,buffer)
           (kill-buffer ,buffer))))))

(defun tramp-rpc-mock-test-request--timeout-clock ()
  "Return a clock which makes the first wait check expire."
  (let ((calls 0))
    (lambda (&rest _)
      (setq calls (1+ calls))
      (if (<= calls 2) 0 100))))

(ert-deftest tramp-rpc-mock-test-request-sync-timeout-discards-late-response ()
  "A timed out synchronous ID is not buffered when its response arrives late."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((clock (tramp-rpc-mock-test-request--timeout-clock))
          (vec (tramp-rpc-mock-test-request--vec))
          (tramp-rpc--connections (make-hash-table :test 'equal))
          invalidated)
      (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
                 (lambda (_vec) connection))
                ((symbol-function 'tramp-rpc-protocol-encode-request-with-id)
                 (lambda (&rest _) '(101 . "request")))
                ((symbol-function 'process-send-string) (lambda (&rest _) nil))
                ((symbol-function 'tramp-rpc--invalidate-timed-out-connection)
                 (lambda (timed-out-process timed-out-vec _event)
                   (setq invalidated (list timed-out-process timed-out-vec))))
                ((symbol-function 'float-time) clock))
        (should-error (tramp-rpc--call-with-timeout vec "test" nil 0 0)
                      :type 'remote-file-error)
        (should (equal (list process vec) invalidated))
        (should-not (tramp-rpc-connection-pending-ids connection))
        (let ((messages (list '(:id 101 :result late))))
          (cl-letf (((symbol-function 'tramp-rpc-protocol-try-read-message)
                     (lambda (_buffer)
                       (set-marker (mark-marker) (point-max))
                       (pop messages))))
            (tramp-rpc--connection-filter process "late")))
        (should (zerop (hash-table-count
                        (tramp-rpc-connection-pending-responses connection))))))))

(ert-deftest tramp-rpc-mock-test-request-user-quit-retires-generation ()
  "User quit detaches and closes the synchronous request generation."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let* ((vec (tramp-rpc-mock-test-request--vec))
           (connection connection)
           (tramp-rpc--connections (make-hash-table :test 'equal)))
      (puthash (tramp-rpc--connection-key vec) connection tramp-rpc--connections)
      (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
                 (lambda (_vec) connection))
                ((symbol-function 'tramp-rpc-protocol-encode-request-with-id)
                 (lambda (&rest _) '(102 . "request")))
                ((symbol-function 'process-send-string) (lambda (&rest _) nil))
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _) (signal 'quit nil)))
                ((symbol-function 'tramp-rpc--cleanup-async-processes) #'ignore)
                ((symbol-function 'tramp-rpc--cleanup-pty-processes) #'ignore)
                ((symbol-function 'tramp-rpc--cleanup-watches-for-connection) #'ignore)
                ((symbol-function 'tramp-rpc--cleanup-file-notify-for-connection) #'ignore)
                ((symbol-function 'tramp-rpc--clear-direnv-cache) #'ignore)
                ((symbol-function 'tramp-rpc--clear-file-caches-for-connection) #'ignore)
                ((symbol-function 'tramp-rpc-magit--clear-status-cache-for-connection) #'ignore)
                ((symbol-function 'tramp-rpc--cleanup-controlmaster-unlocked) #'ignore))
        (should (eq (condition-case nil
                        (progn
                          (tramp-rpc--call-with-timeout vec "test" nil 1 0)
                          nil)
                      (quit 'quit))
                    'quit)))
      (should-not (process-live-p process))
      (should-not (tramp-rpc--get-connection vec))
      (should-not (tramp-rpc-connection-pending-ids connection)))))

(ert-deftest tramp-rpc-mock-test-request-partial-pipeline-send-quit-retires-generation ()
  "Quit after one pipelined send retires its ambiguously framed generation."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let* ((vec (tramp-rpc-mock-test-request--vec))
           (connection connection)
           (tramp-rpc--connections (make-hash-table :test 'equal))
           (next-id 200)
           (send-count 0))
      (puthash (tramp-rpc--connection-key vec) connection tramp-rpc--connections)
      (cl-letf (((symbol-function 'tramp-rpc-protocol-encode-request-with-id)
                 (lambda (&rest _)
                   (setq next-id (1+ next-id))
                   (cons next-id "request")))
                ((symbol-function 'process-send-string)
                 (lambda (&rest _)
                   (setq send-count (1+ send-count))
                   (when (= send-count 2) (signal 'quit nil))))
                ((symbol-function 'tramp-rpc--cleanup-async-processes) #'ignore)
                ((symbol-function 'tramp-rpc--cleanup-pty-processes) #'ignore)
                ((symbol-function 'tramp-rpc--cleanup-watches-for-connection) #'ignore)
                ((symbol-function 'tramp-rpc--cleanup-file-notify-for-connection) #'ignore)
                ((symbol-function 'tramp-rpc--clear-direnv-cache) #'ignore)
                ((symbol-function 'tramp-rpc--clear-file-caches-for-connection) #'ignore)
                ((symbol-function 'tramp-rpc-magit--clear-status-cache-for-connection) #'ignore)
                ((symbol-function 'tramp-rpc--cleanup-controlmaster-unlocked) #'ignore))
        (should
         (eq (condition-case nil
                 (progn
                   (tramp-rpc--send-requests
                    vec '(("one") ("two")) connection)
                   nil)
               (quit 'quit))
             'quit)))
      (should (= send-count 2))
      (should-not (process-live-p process))
      (should-not (tramp-rpc--get-connection vec))
      (should-not (tramp-rpc-connection-pending-ids connection)))))

(ert-deftest tramp-rpc-mock-test-request-pipeline-encode-error-preserves-generation ()
  "Failure before a pipelined transport write leaves the generation reusable."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let* ((vec (tramp-rpc-mock-test-request--vec))
           (connection connection)
           sent)
      (cl-letf (((symbol-function 'tramp-rpc-protocol-encode-request-with-id)
                 (lambda (&rest _) (error "encode failed")))
                ((symbol-function 'process-send-string)
                 (lambda (&rest _) (setq sent t))))
        (should-error
         (tramp-rpc--send-requests vec '(("one")) connection)))
      (should-not sent)
      (should (process-live-p process))
      (should-not (tramp-rpc-connection-pending-ids connection)))))

(ert-deftest tramp-rpc-mock-test-request-pipeline-collects-partial-responses ()
  "Pipelined waiting retains partial completion and original ID order."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let* ((vec (tramp-rpc-mock-test-request--vec))
           (connection connection)
           (pending (tramp-rpc-connection-pending-responses connection))
           (tramp-rpc-poll-interval 0.001)
           delivered)
      (setf (tramp-rpc-connection-pending-ids connection) '(301 302))
      (puthash 301 '(:id 301 :result first) pending)
      (cl-letf (((symbol-function 'accept-process-output)
                 (lambda (&rest _)
                   (unless delivered
                     (setq delivered t)
                     (puthash 302 '(:id 302 :result second) pending))
                   t)))
        (should
         (equal (tramp-rpc--receive-responses
                 vec '(301 302) 1 connection)
                '((301 :id 301 :result first)
                  (302 :id 302 :result second))))))))

(ert-deftest tramp-rpc-mock-test-request-pipeline-timeout-includes-stderr ()
  "Pipelined timeout diagnostics include missing IDs and SSH stderr."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let* ((stderr-buffer (generate-new-buffer " *tramp-rpc-request-stderr*"))
           (vec (tramp-rpc-mock-test-request--vec))
           (connection (progn (setf (tramp-rpc-connection-stderr-buffer connection)
                                    stderr-buffer)
                              connection))
           message)
      (unwind-protect
          (progn
            (with-current-buffer stderr-buffer (insert "permission denied"))
            (cl-letf (((symbol-function 'tramp-rpc--invalidate-timed-out-connection)
                       #'ignore))
              (condition-case err
                  (tramp-rpc--receive-responses vec '(401 402) 0 connection)
                (remote-file-error
                 (setq message (error-message-string err)))))
            (should (string-match-p "missing ids: (401 402)" message))
            (should (string-match-p "SSH stderr: permission denied" message)))
        (kill-buffer stderr-buffer)))))

(ert-deftest tramp-rpc-mock-test-request-timeout-invalidates-ssh-generation ()
  "Timeout invalidation removes the transport and its ControlMaster."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let* ((vec (tramp-rpc-mock-test-request--vec))
           (connection connection)
           (tramp-rpc--connections (make-hash-table :test 'equal))
           controlmaster-cleaned)
      (puthash (tramp-rpc--connection-key vec) connection tramp-rpc--connections)
      (cl-letf (((symbol-function 'tramp-rpc--cleanup-async-processes) #'ignore)
                ((symbol-function 'tramp-rpc--cleanup-pty-processes) #'ignore)
                ((symbol-function 'tramp-rpc--cleanup-watches-for-connection) #'ignore)
                ((symbol-function 'tramp-rpc--cleanup-file-notify-for-connection) #'ignore)
                ((symbol-function 'tramp-rpc--clear-direnv-cache) #'ignore)
                ((symbol-function 'tramp-rpc--clear-file-caches-for-connection) #'ignore)
                ((symbol-function 'tramp-rpc-magit--clear-status-cache-for-connection) #'ignore)
                ((symbol-function 'tramp-rpc--cleanup-controlmaster-unlocked)
                 (lambda (cleaned-vec)
                   (setq controlmaster-cleaned cleaned-vec))))
        (tramp-rpc--invalidate-timed-out-connection
         process vec "test timeout\n"))
      (should-not (process-live-p process))
      (should-not (tramp-rpc--get-connection vec))
      (should (equal vec controlmaster-cleaned)))))

(ert-deftest tramp-rpc-mock-test-request-timeout-preserves-replacement-controlmaster ()
  "Timeout cleanup does not tear down a replacement connection."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let* ((vec (tramp-rpc-mock-test-request--vec))
           (replacement-buffer (generate-new-buffer
                                " *tramp-rpc-mock-test-replacement*"))
           (replacement (make-pipe-process
                         :name "tramp-rpc-mock-test-replacement"
                         :buffer replacement-buffer :noquery t))
           (old-connection connection)
           (replacement-connection
            (tramp-rpc--attach-connection
             (tramp-rpc--make-connection :process replacement
                                         :buffer replacement-buffer :vec vec)))
           (tramp-rpc--connections (make-hash-table :test 'equal))
           cleanup-attempted)
      (unwind-protect
          (progn
            (puthash (tramp-rpc--connection-key vec) replacement-connection
                     tramp-rpc--connections)
            (cl-letf (((symbol-function 'tramp-rpc--cleanup-async-processes)
                       #'ignore)
                      ((symbol-function 'tramp-rpc--cleanup-pty-processes)
                       #'ignore)
                      ((symbol-function 'tramp-rpc--cleanup-watches-for-connection)
                       #'ignore)
                      ((symbol-function 'tramp-rpc--cleanup-file-notify-for-connection)
                       #'ignore)
                      ((symbol-function 'tramp-rpc--controlmaster-socket-path)
                       (lambda (_vec) (setq cleanup-attempted t))))
              (tramp-rpc--invalidate-timed-out-connection
               process vec "test timeout\n"))
            (should-not (process-live-p process))
            (should (process-live-p replacement))
            (should (eq replacement-connection
                        (tramp-rpc--get-connection vec)))
            (should-not cleanup-attempted))
        (when (process-live-p replacement)
          (delete-process replacement))
        (when (buffer-live-p replacement-buffer)
          (kill-buffer replacement-buffer))))))

(ert-deftest tramp-rpc-mock-test-request-timeout-preserves-shared-controlmaster ()
  "Timeout cleanup does not tear down a ControlMaster shared with a live connection."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let* ((vec (tramp-rpc-mock-test-request--vec))
           (other-buffer (generate-new-buffer " *tramp-rpc-mock-test-shared*"))
           (other (make-pipe-process :name "tramp-rpc-mock-test-shared"
                                     :buffer other-buffer :noquery t))
           (other-vec (tramp-dissect-file-name "/rpc:shared-test:/tmp/"))
           (old-connection connection)
           (other-connection
            (tramp-rpc--attach-connection
             (tramp-rpc--make-connection :process other :buffer other-buffer
                                         :vec other-vec)))
           (tramp-rpc--connections (make-hash-table :test 'equal))
           cleanup-attempted)
      (unwind-protect
          (progn
            (puthash (tramp-rpc--connection-key vec) old-connection
                     tramp-rpc--connections)
            (puthash (tramp-rpc--connection-key other-vec) other-connection
                     tramp-rpc--connections)
            (cl-letf (((symbol-function 'tramp-rpc--cleanup-async-processes)
                       #'ignore)
                      ((symbol-function 'tramp-rpc--cleanup-pty-processes)
                       #'ignore)
                      ((symbol-function 'tramp-rpc--cleanup-watches-for-connection)
                       #'ignore)
                      ((symbol-function 'tramp-rpc--cleanup-file-notify-for-connection)
                       #'ignore)
                      ((symbol-function 'tramp-rpc--clear-direnv-cache) #'ignore)
                      ((symbol-function 'tramp-rpc--clear-file-caches-for-connection)
                       #'ignore)
                      ((symbol-function 'tramp-rpc-magit--clear-status-cache-for-connection)
                       #'ignore)
                      ((symbol-function 'tramp-rpc--controlmaster-socket-path)
                       (lambda (_v) "/tmp/tramp-rpc-mock-shared-socket"))
                      ((symbol-function 'tramp-rpc--cleanup-controlmaster-unlocked)
                       (lambda (_v) (setq cleanup-attempted t))))
              (tramp-rpc--invalidate-timed-out-connection
               process vec "test timeout\n"))
            (should-not (process-live-p process))
            (should (process-live-p other))
            (should (eq other-connection
                        (tramp-rpc--get-connection other-vec)))
            (should-not cleanup-attempted))
        (when (process-live-p other)
          (delete-process other))
        (when (buffer-live-p other-buffer)
          (kill-buffer other-buffer))))))

(ert-deftest tramp-rpc-mock-test-request-callback-error-does-not-strand-next-frame ()
  "A failing async callback does not stop delivery of buffered responses."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((callbacks (tramp-rpc-connection-async-callbacks connection))
          (messages (list '(:id 201 :result first)
                          '(:id 202 :result second))))
      (puthash 201 (lambda (_response) (error "callback failed")) callbacks)
      (tramp-rpc--track-pending-request connection 202)
      (cl-letf (((symbol-function 'tramp-rpc-protocol-try-read-message)
                 (lambda (_buffer)
                   (set-marker (mark-marker) (point-max))
                   (pop messages))))
        (tramp-rpc--connection-filter process "two responses"))
      (should-not (gethash 201 callbacks))
      (should (equal '(:id 202 :result second)
                     (gethash 202 (tramp-rpc-connection-pending-responses connection)))))))

(defun tramp-rpc-mock-test-request--assert-poll-survives-wait
    (vec connection-process deliver wait)
  "Assert a relay poll scheduled inside a synchronous RPC wait stays live.
CONNECTION-PROCESS is the transport the waiter listens on.  DELIVER runs
inside the first `accept-process-output\=' and must buffer the response the
waiter expects.  WAIT performs the synchronous call; its value is returned."
  (let* ((relay (make-pipe-process
                 :name "tramp-rpc-mock-test-request-relay"
                 :noquery t))
         (tramp-rpc--async-processes (make-hash-table :test 'eq))
         (original-accept-process-output
          (symbol-function 'accept-process-output))
         (next-reads 0)
         response-delivered
         result)
    (unwind-protect
        (progn
          (puthash relay
                   (list :vec vec :pid 42
                         :connection-process connection-process
                         :delivery-timer nil :poll-timer nil)
                   tramp-rpc--async-processes)
          (cl-letf (((symbol-function 'tramp-rpc--call-async)
                     (lambda (&rest _) (cl-incf next-reads)))
                    ((symbol-function 'accept-process-output)
                     (lambda (&rest args)
                       (if response-delivered
                           (apply original-accept-process-output args)
                         (setq response-delivered t)
                         (tramp-rpc--handle-async-read-response
                          relay
                          '(:result ((stdout . nil)
                                     (stderr . nil)
                                     (exited . nil))))
                         (funcall deliver)
                         t))))
            (setq result (funcall wait))
            (tramp-rpc-mock-test--wait-for
             (lambda () (= next-reads 1))
             "next async process read" relay))
          (should (= next-reads 1))
          result)
      (when (process-live-p relay)
        (delete-process relay)))))

(ert-deftest tramp-rpc-mock-test-request-wait-preserves-async-process-poll ()
  "An async process response received during a sync wait schedules its next poll."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((vec (tramp-rpc-mock-test-request--vec))
          (tramp-rpc--connections (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
                 (lambda (_vec) connection))
                ((symbol-function 'tramp-rpc-protocol-encode-request-with-id)
                 (lambda (&rest _) '(301 . "request")))
                ((symbol-function 'process-send-string)
                 (lambda (&rest _) nil)))
        (should (eq 'done
                    (tramp-rpc-mock-test-request--assert-poll-survives-wait
                     vec process
                     (lambda ()
                       (puthash 301 '(:id 301 :result done)
                                (tramp-rpc-connection-pending-responses connection)))
                     (lambda ()
                       (tramp-rpc--call-with-timeout
                        vec "test" nil 1 0.01)))))))))

(ert-deftest tramp-rpc-mock-test-request-batch-wait-preserves-async-process-poll ()
  "An async process response received during a batch wait schedules its next poll."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((vec (tramp-rpc-mock-test-request--vec))
          (tramp-rpc--connections (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
                 (lambda (_vec) connection))
                ((symbol-function 'tramp-rpc-protocol-encode-batch-request-with-id)
                 (lambda (&rest _) '(302 . "batch")))
                ((symbol-function 'process-send-string)
                 (lambda (&rest _) nil))
                ((symbol-function 'tramp-rpc-protocol-decode-batch-response)
                 (lambda (_response) 'batch-result)))
        (should (eq 'batch-result
                    (tramp-rpc-mock-test-request--assert-poll-survives-wait
                     vec process
                     (lambda ()
                       (puthash 302 '(:id 302 :result batch)
                                (tramp-rpc-connection-pending-responses connection)))
                     (lambda ()
                       (tramp-rpc--call-batch vec '(("test" . nil)))))))))))

(ert-deftest tramp-rpc-mock-test-request-receive-preserves-async-process-poll ()
  "An async process response received during a pipelined wait schedules its next poll."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((vec (tramp-rpc-mock-test-request--vec))
          (tramp-rpc--connections (make-hash-table :test 'equal)))
      (tramp-rpc--track-pending-request connection 303)
      (should (equal '((303 . (:id 303 :result pipelined)))
                     (tramp-rpc-mock-test-request--assert-poll-survives-wait
                      vec process
                      (lambda ()
                        (puthash 303 '(:id 303 :result pipelined)
                                 (tramp-rpc-connection-pending-responses connection)))
                      (lambda ()
                        (tramp-rpc--receive-responses
                         vec '(303) 1 connection))))))))

(ert-deftest tramp-rpc-mock-test-request-batch-timeout-cleans-id ()
  "A batch timeout releases its request ID and response table."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((vec (tramp-rpc-mock-test-request--vec))
          (tramp-rpc--connections (make-hash-table :test 'equal))
          invalidated)
      (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
                 (lambda (_vec) connection))
                ((symbol-function 'tramp-rpc-protocol-encode-batch-request-with-id)
                 (lambda (&rest _) '(102 . "batch")))
                ((symbol-function 'process-send-string) (lambda (&rest _) nil))
                ((symbol-function 'tramp-rpc--invalidate-timed-out-connection)
                 (lambda (timed-out-process timed-out-vec _event)
                   (setq invalidated (list timed-out-process timed-out-vec))))
                ((symbol-function 'float-time) (tramp-rpc-mock-test-request--timeout-clock)))
        (should-error (tramp-rpc--call-batch vec '(("test" . nil)))
                      :type 'remote-file-error)
        (should (equal (list process vec) invalidated))
        (should-not (tramp-rpc-connection-pending-ids connection))
        (should (zerop (hash-table-count
                        (tramp-rpc-connection-pending-responses connection))))))))

(ert-deftest tramp-rpc-mock-test-request-batch-uses-configured-timeout ()
  "Batch RPC calls wait for the configured synchronous timeout."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((vec (tramp-rpc-mock-test-request--vec))
          (tramp-rpc-call-timeout 75)
          (tramp-rpc--connections (make-hash-table :test 'equal))
          (clock-calls 0))
      (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
                 (lambda (_vec) connection))
                ((symbol-function 'tramp-rpc-protocol-encode-batch-request-with-id)
                 (lambda (&rest _) '(103 . "batch")))
                ((symbol-function 'process-send-string) (lambda (&rest _) nil))
                ((symbol-function 'float-time)
                 (lambda (&rest _)
                   (setq clock-calls (1+ clock-calls))
                   (if (<= clock-calls 2) 0 50)))
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _)
                   (puthash 103 '(:id 103 :result batch)
                            (tramp-rpc-connection-pending-responses connection))
                   t))
                ((symbol-function 'tramp-rpc-protocol-decode-batch-response)
                 (lambda (_response) 'batch-result)))
        (should (eq 'batch-result
                    (tramp-rpc--call-batch vec '(("test" . nil)))))))))

(ert-deftest tramp-rpc-mock-test-request-pipeline-death-keeps-receive-on-old-generation ()
  "A pipeline receives its injected error from the generation that sent it."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let* ((replacement-buffer (generate-new-buffer " *tramp-rpc-replacement*"))
           (replacement (make-pipe-process :name "tramp-rpc-replacement"
                                           :buffer replacement-buffer :noquery t))
           (vec (tramp-rpc-mock-test-request--vec))
           (old-connection connection)
           (replacement-connection
            (tramp-rpc--attach-connection
             (tramp-rpc--make-connection :process replacement
                                         :buffer replacement-buffer :vec vec)))
           (tramp-rpc--connections (make-hash-table :test 'equal))
           (ensure-calls 0))
      (unwind-protect
          (progn
            (puthash (tramp-rpc--connection-key vec) old-connection
                     tramp-rpc--connections)
            (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
                       (lambda (_vec)
                         (setq ensure-calls (1+ ensure-calls))
                         (tramp-rpc--get-connection vec)))
                      ((symbol-function 'tramp-rpc-protocol-encode-request-with-id)
                       (lambda (&rest _) '(103 . "request")))
                      ((symbol-function 'process-send-string)
                       (lambda (&rest _)
                         (puthash (tramp-rpc--connection-key vec)
                                  replacement-connection tramp-rpc--connections)
                         (tramp-rpc--cleanup-connection-generation
                          process vec "closed\n" :transport-death))))
              (should (equal '((:error -32098 :message "RPC transport closed for request-test (closed)"))
                             (tramp-rpc--call-pipelined vec '(("test" . nil)))))
              (should (= ensure-calls 1))
              (should-not (tramp-rpc-connection-pending-ids connection))
              (should (zerop (hash-table-count
                        (tramp-rpc-connection-pending-responses connection))))
              (should (eq replacement
                          (tramp-rpc--connection-transport (tramp-rpc--get-connection vec)))))
        (when (process-live-p replacement)
          (delete-process replacement))
        (when (buffer-live-p replacement-buffer)
          (kill-buffer replacement-buffer)))))))

(ert-deftest tramp-rpc-mock-test-request-user-quit-cleans-id ()
  "User quit while synchronously waiting releases the request ID."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((vec (tramp-rpc-mock-test-request--vec))
          (tramp-rpc--connections (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
                 (lambda (_vec) connection))
                ((symbol-function 'tramp-rpc-protocol-encode-request-with-id)
                 (lambda (&rest _) '(105 . "quit")))
                ((symbol-function 'process-send-string) (lambda (&rest _) nil))
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _) (signal 'quit nil))))
        (condition-case nil
            (tramp-rpc--call-with-timeout vec "test" nil 30 0)
          (quit nil))
        (should-not (tramp-rpc-connection-pending-ids connection))
        (should (zerop (hash-table-count
                        (tramp-rpc-connection-pending-responses connection))))))))

(ert-deftest tramp-rpc-mock-test-request-send-error-cleans-id ()
  "A send failure before waiting releases the request ID."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((vec (tramp-rpc-mock-test-request--vec))
          (tramp-rpc--connections (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
                 (lambda (_vec) connection))
                ((symbol-function 'tramp-rpc-protocol-encode-request-with-id)
                 (lambda (&rest _) '(106 . "send-error")))
                ((symbol-function 'process-send-string)
                 (lambda (&rest _) (error "send failed"))))
        (should-error (tramp-rpc--call-with-timeout vec "test" nil 30 0))
        (should-not (tramp-rpc-connection-pending-ids connection))
        (should (zerop (hash-table-count
                        (tramp-rpc-connection-pending-responses connection))))))))

(ert-deftest tramp-rpc-mock-test-request-non-essential-bailout-cleans-id ()
  "A non-essential locked wait releases the request ID."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((vec (tramp-rpc-mock-test-request--vec))
          (non-essential t)
          (tramp-rpc--connections (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
                 (lambda (_vec) connection))
                ((symbol-function 'tramp-rpc-protocol-encode-request-with-id)
                 (lambda (&rest _) '(107 . "bail")))
                ((symbol-function 'process-send-string) (lambda (&rest _) nil))
                ((symbol-function 'tramp-rpc--process-accessible-p)
                 (lambda (_process) nil)))
        (should (eq 'non-essential
                    (catch 'non-essential
                      (tramp-rpc--call-with-timeout vec "test" nil 30 0)
                      nil)))
        (should-not (tramp-rpc-connection-pending-ids connection))
        (should (zerop (hash-table-count
                        (tramp-rpc-connection-pending-responses connection))))))))

(ert-deftest tramp-rpc-mock-test-request-transport-death-response-is-not-overwritten ()
  "Late normal output cannot overwrite a transport error awaiting its waiter."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((vec (tramp-rpc-mock-test-request--vec))
          (tramp-rpc--connections (make-hash-table :test 'equal)))
      (tramp-rpc--track-pending-request connection 108)
      (tramp-rpc--cleanup-connection-generation process vec "closed\n"
                                                 :transport-death)
      (let ((messages (list '(:id 108 :result late))))
        (cl-letf (((symbol-function 'tramp-rpc-protocol-try-read-message)
                   (lambda (_buffer)
                     (set-marker (mark-marker) (point-max))
                     (pop messages))))
          (tramp-rpc--connection-filter process "late")))
      (with-current-buffer buffer
        (let ((response (tramp-rpc--find-response-by-id connection 108)))
          (should (tramp-rpc-protocol-error-p response))
          (should (= -32098 (tramp-rpc-protocol-error-code response)))))
      (should-not (tramp-rpc-connection-pending-ids connection))
      (should (zerop (hash-table-count
                        (tramp-rpc-connection-pending-responses connection)))))))

(ert-deftest tramp-rpc-mock-test-request-abandon-one-preserves-live-request ()
  "Releasing one request does not discard another live request's response."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((pending (tramp-rpc-connection-pending-responses connection)))
      (tramp-rpc--track-pending-request connection 109)
      (tramp-rpc--track-pending-request connection 110)
      (puthash 110 '(:id 110 :result live) pending)
      (tramp-rpc--release-pending-requests connection '(109))
      (should (equal '(110) (tramp-rpc-connection-pending-ids connection)))
      (should (equal '(:id 110 :result live) (gethash 110 pending)))
      (should (= 1 (hash-table-count pending))))))

;;; tramp-rpc-request-tests.el ends here
