;;; tramp-rpc-request-tests.el --- Request lifecycle tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'tramp-rpc)

(defun tramp-rpc-mock-test-request--connection ()
  "Return a live process and its buffer for request lifecycle tests."
  (let* ((buffer (generate-new-buffer " *tramp-rpc-mock-test-request*"))
         (process (make-pipe-process :name "tramp-rpc-mock-test-request"
                                     :buffer buffer :noquery t)))
    (process-put process :tramp-rpc-buffer buffer)
    (list process buffer)))

(defmacro tramp-rpc-mock-test-request--with-connection (spec &rest body)
  "Run BODY with a disposable test PROCESS and BUFFER."
  (declare (indent 1) (debug t))
  (let ((process (nth 0 spec))
        (buffer (nth 1 spec)))
    `(let* ((pair (tramp-rpc-mock-test-request--connection))
            (,process (car pair))
            (,buffer (cadr pair)))
       (unwind-protect
           (progn ,@body)
         (when (process-live-p ,process)
           (delete-process ,process))
         (when (buffer-live-p ,buffer)
           (kill-buffer ,buffer))))))

(defun tramp-rpc-mock-test-request--vec ()
  "Return a harmless vector for lifecycle diagnostics."
  (tramp-dissect-file-name "/rpc:request-test:/tmp/"))

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
          (tramp-rpc--connections (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
                 (lambda (_vec) (list :process process :buffer buffer)))
                ((symbol-function 'tramp-rpc-protocol-encode-request-with-id)
                 (lambda (&rest _) '(101 . "request")))
                ((symbol-function 'process-send-string) (lambda (&rest _) nil))
                ((symbol-function 'float-time) clock))
        (should-error (tramp-rpc--call-with-timeout vec "test" nil 0 0)
                      :type 'remote-file-error)
        (should-not (process-get process :tramp-rpc-pending-ids))
        (let ((messages (list '(:id 101 :result late))))
          (cl-letf (((symbol-function 'tramp-rpc-protocol-try-read-message)
                     (lambda (_buffer)
                       (set-marker (mark-marker) (point-max))
                       (pop messages))))
            (tramp-rpc--connection-filter process "late")))
        (should-not (gethash buffer tramp-rpc--pending-responses))))))

(ert-deftest tramp-rpc-mock-test-request-batch-timeout-cleans-id ()
  "A batch timeout releases its request ID and response table."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((vec (tramp-rpc-mock-test-request--vec))
          (tramp-rpc--connections (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
                 (lambda (_vec) (list :process process :buffer buffer)))
                ((symbol-function 'tramp-rpc-protocol-encode-batch-request-with-id)
                 (lambda (&rest _) '(102 . "batch")))
                ((symbol-function 'process-send-string) (lambda (&rest _) nil))
                ((symbol-function 'float-time) (tramp-rpc-mock-test-request--timeout-clock)))
        (should-error (tramp-rpc--call-batch vec '(("test" . nil)))
                      :type 'remote-file-error)
        (should-not (process-get process :tramp-rpc-pending-ids))
        (should-not (gethash buffer tramp-rpc--pending-responses))))))

(ert-deftest tramp-rpc-mock-test-request-pipeline-death-keeps-receive-on-old-generation ()
  "A pipeline receives its injected error from the generation that sent it."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let* ((replacement-buffer (generate-new-buffer " *tramp-rpc-replacement*"))
           (replacement (make-pipe-process :name "tramp-rpc-replacement"
                                           :buffer replacement-buffer :noquery t))
           (vec (tramp-rpc-mock-test-request--vec))
           (old-connection (list :process process :buffer buffer))
           (replacement-connection (list :process replacement
                                         :buffer replacement-buffer))
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
              (should-not (process-get process :tramp-rpc-pending-ids))
              (should-not (gethash buffer tramp-rpc--pending-responses))
              (should (eq replacement
                          (plist-get (tramp-rpc--get-connection vec) :process))))
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
                 (lambda (_vec) (list :process process :buffer buffer)))
                ((symbol-function 'tramp-rpc-protocol-encode-request-with-id)
                 (lambda (&rest _) '(105 . "quit")))
                ((symbol-function 'process-send-string) (lambda (&rest _) nil))
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _) (signal 'quit nil))))
        (condition-case nil
            (tramp-rpc--call-with-timeout vec "test" nil 30 0)
          (quit nil))
        (should-not (process-get process :tramp-rpc-pending-ids))
        (should-not (gethash buffer tramp-rpc--pending-responses))))))

(ert-deftest tramp-rpc-mock-test-request-send-error-cleans-id ()
  "A send failure before waiting releases the request ID."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((vec (tramp-rpc-mock-test-request--vec))
          (tramp-rpc--connections (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
                 (lambda (_vec) (list :process process :buffer buffer)))
                ((symbol-function 'tramp-rpc-protocol-encode-request-with-id)
                 (lambda (&rest _) '(106 . "send-error")))
                ((symbol-function 'process-send-string)
                 (lambda (&rest _) (error "send failed"))))
        (should-error (tramp-rpc--call-with-timeout vec "test" nil 30 0))
        (should-not (process-get process :tramp-rpc-pending-ids))
        (should-not (gethash buffer tramp-rpc--pending-responses))))))

(ert-deftest tramp-rpc-mock-test-request-non-essential-bailout-cleans-id ()
  "A non-essential locked wait releases the request ID."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((vec (tramp-rpc-mock-test-request--vec))
          (non-essential t)
          (tramp-rpc--connections (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'tramp-rpc--ensure-connection)
                 (lambda (_vec) (list :process process :buffer buffer)))
                ((symbol-function 'tramp-rpc-protocol-encode-request-with-id)
                 (lambda (&rest _) '(107 . "bail")))
                ((symbol-function 'process-send-string) (lambda (&rest _) nil))
                ((symbol-function 'tramp-rpc--process-accessible-p)
                 (lambda (_process) nil)))
        (should (eq 'non-essential
                    (catch 'non-essential
                      (tramp-rpc--call-with-timeout vec "test" nil 30 0)
                      nil)))
        (should-not (process-get process :tramp-rpc-pending-ids))
        (should-not (gethash buffer tramp-rpc--pending-responses))))))

(ert-deftest tramp-rpc-mock-test-request-transport-death-response-is-not-overwritten ()
  "Late normal output cannot overwrite a transport error awaiting its waiter."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((vec (tramp-rpc-mock-test-request--vec))
          (tramp-rpc--connections (make-hash-table :test 'equal)))
      (tramp-rpc--track-pending-request process 108)
      (tramp-rpc--cleanup-connection-generation process vec "closed\n"
                                                 :transport-death)
      (let ((messages (list '(:id 108 :result late))))
        (cl-letf (((symbol-function 'tramp-rpc-protocol-try-read-message)
                   (lambda (_buffer)
                     (set-marker (mark-marker) (point-max))
                     (pop messages))))
          (tramp-rpc--connection-filter process "late")))
      (with-current-buffer buffer
        (let ((response (tramp-rpc--find-response-by-id 108 process)))
          (should (tramp-rpc-protocol-error-p response))
          (should (= -32098 (tramp-rpc-protocol-error-code response)))))
      (should-not (process-get process :tramp-rpc-pending-ids))
      (should-not (gethash buffer tramp-rpc--pending-responses)))))

(ert-deftest tramp-rpc-mock-test-request-abandon-one-preserves-live-request ()
  "Releasing one request does not discard another live request's response."
  (tramp-rpc-mock-test-request--with-connection (process buffer)
    (let ((pending (make-hash-table :test 'eql)))
      (puthash buffer pending tramp-rpc--pending-responses)
      (tramp-rpc--track-pending-request process 109)
      (tramp-rpc--track-pending-request process 110)
      (puthash 110 '(:id 110 :result live) pending)
      (tramp-rpc--release-pending-requests process buffer '(109))
      (should (equal '(110) (process-get process :tramp-rpc-pending-ids)))
      (should (equal '(:id 110 :result live) (gethash 110 pending)))
      (should (= 1 (hash-table-count pending))))))

;;; tramp-rpc-request-tests.el ends here
