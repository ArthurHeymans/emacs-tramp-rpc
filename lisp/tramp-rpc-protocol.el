;;; tramp-rpc-protocol.el --- MessagePack-RPC protocol for TRAMP-RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Keywords: comm, processes
;; Package-Requires: ((emacs "30.1") (msgpack "0.1.1"))

;; This file is part of tramp-rpc.

;; tramp-rpc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This file provides the MessagePack-RPC protocol implementation for
;; communicating with the tramp-rpc-server binary.
;;
;; Protocol framing: <4-byte big-endian length><msgpack payload>

;;; Code:

(require 'cl-lib)
(require 'msgpack)

(declare-function tramp-message "tramp-message")

(defvar tramp-rpc-protocol--request-id 0
  "Counter for generating unique request IDs.")

(defvar tramp-rpc-protocol--message-target nil
  "TRAMP vector or process used for level-6 protocol debug messages.")

(defvar tramp-rpc-protocol--deferred-poll-messages (make-hash-table :test 'eql)
  "Idle polling requests awaiting a response, keyed by request ID.
Each value is a cons cell containing the target connection and request.")

(defun tramp-rpc-protocol--clear-deferred-polls-for-target (target)
  "Discard deferred poll messages associated with TARGET."
  (let (ids)
    (maphash
     (lambda (id target-and-request)
       (when (eq target (car target-and-request))
         (push id ids)))
     tramp-rpc-protocol--deferred-poll-messages)
    (dolist (id ids)
      (remhash id tramp-rpc-protocol--deferred-poll-messages))))

(defun tramp-rpc-protocol--clear-deferred-polls ()
  "Discard all deferred poll messages."
  (clrhash tramp-rpc-protocol--deferred-poll-messages))

(defun tramp-rpc-protocol--polling-method-p (method)
  "Return non-nil when METHOD is a long-polling process read method."
  (member method '("process.read" "process.read_pty")))

(defun tramp-rpc-protocol--empty-poll-response-p (method response)
  "Return non-nil when RESPONSE is an uneventful poll for METHOD."
  (let ((result (plist-get response :result)))
    (and (not (plist-get response :error))
         (not (alist-get 'exited result))
         (pcase method
           ("process.read"
            (and (assq 'stdout result)
                 (assq 'stderr result)
                 (not (alist-get 'stdout result))
                 (not (alist-get 'stderr result))))
           ("process.read_pty"
            (and (assq 'output result)
                 (not (alist-get 'output result))))
           (_ nil)))))

(defun tramp-rpc-protocol--message (object)
  "Log OBJECT as a level-6 Tramp debug message when possible."
  (when (and tramp-rpc-protocol--message-target
             (fboundp 'tramp-message))
    (tramp-message tramp-rpc-protocol--message-target 6 "%s" object)))

(defun tramp-rpc-protocol--next-id ()
  "Generate the next request ID."
  (cl-incf tramp-rpc-protocol--request-id))

(defun tramp-rpc-protocol--length-prefix (payload)
  "Add 4-byte big-endian length prefix to PAYLOAD (unibyte string)."
  (let ((len (length payload)))
    (concat (msgpack-unsigned-to-bytes len 4) payload)))

(defun tramp-rpc-protocol-encode-request-with-id (method params)
  "Encode a MessagePack-RPC request for METHOD with PARAMS.
Returns a cons cell (ID . BYTES) for pipelining support."
  (let* ((id (tramp-rpc-protocol--next-id))
         (request `((version . "2.0")
                    (id . ,id)
                    (method . ,method)
                    (params . ,params)))
         (payload (msgpack-encode request)))
    ;; Idle process reads are continuous long polls.  Defer their request log
    ;; until the response is known so empty polls produce no debug noise while
    ;; output, exits, and errors still retain the complete request/response pair.
    (if (tramp-rpc-protocol--polling-method-p method)
        (puthash id (cons tramp-rpc-protocol--message-target request)
                 tramp-rpc-protocol--deferred-poll-messages)
      (tramp-rpc-protocol--message request))
    (cons id (tramp-rpc-protocol--length-prefix payload))))

(defun tramp-rpc-protocol-decode-response (buffer start)
  "Decode a MessagePack-RPC response or notification in BUFEER from START.
Returns a plist with :id, :result, and :error keys for responses.
For server-initiated notifications (no :id, has :method), returns a plist
with :notification t, :method, and :params keys."
  (let* ((response
	  (with-current-buffer buffer
	    (goto-char start)
	    (msgpack-read :map-type 'alist
                          :key-type 'symbol
                          :array-type 'list
                          :bin-type 'msgpack-bin)))
         (id (alist-get 'id response))
         (method (alist-get 'method response))
         (result
          ;; Notifications have method but no id (JSON-RPC 2.0 spec)
          (if (and method (not id))
              (list :notification t
                    :method method
                    :params (alist-get 'params response))
            ;; Normal response
            (let ((result (alist-get 'result response))
                  (error-obj (alist-get 'error response)))
              (list :id id
                    :result result
                    :error (when error-obj
                             (list :code (alist-get 'code error-obj)
                                   :message (alist-get 'message error-obj)
                                   :data (alist-get 'data error-obj))))))))
    (if-let* ((target-and-request
               (gethash id tramp-rpc-protocol--deferred-poll-messages))
              (request (cdr target-and-request)))
        (let ((tramp-rpc-protocol--message-target
               (car target-and-request)))
          (remhash id tramp-rpc-protocol--deferred-poll-messages)
          (unless (tramp-rpc-protocol--empty-poll-response-p
                   (alist-get 'method request) result)
            (tramp-rpc-protocol--message request)
            (tramp-rpc-protocol--message result)))
      (tramp-rpc-protocol--message result))
    result))

(defun tramp-rpc-protocol-error-p (response)
  "Return non-nil if RESPONSE contains an error."
  (plist-get response :error))

(defun tramp-rpc-protocol-error-message (response)
  "Extract the error message from RESPONSE."
  (plist-get (plist-get response :error) :message))

(defun tramp-rpc-protocol-error-code (response)
  "Extract the error code from RESPONSE."
  (plist-get (plist-get response :error) :code))

(defun tramp-rpc-protocol-error-data (response)
  "Extract the error data from RESPONSE.
Returns the data alist, or nil if not present."
  (plist-get (plist-get response :error) :data))

(defun tramp-rpc-protocol-error-errno (response)
  "Extract the OS errno from an IO error RESPONSE.
Returns the integer errno, or nil if not an IO error with errno."
  (let ((data (tramp-rpc-protocol-error-data response)))
    (when data
      (alist-get 'os_errno data))))

;; Error codes (only codes actually used by the client)
(defconst tramp-rpc-protocol-error-file-not-found -32001)
(defconst tramp-rpc-protocol-error-permission-denied -32002)
(defconst tramp-rpc-protocol-error-io -32003)

;; ============================================================================
;; Length-prefixed framing support
;; ============================================================================

(defun tramp-rpc-protocol-read-length (buffer)
  "Read the 4-byte big-endian length from BUFFER.
Returns the length as an integer, or nil if the BUFFER is too short."
  (with-current-buffer buffer
    (when (>= (point-max) (+ (mark-marker) 4))
      (msgpack-bytes-to-unsigned
       (buffer-substring (mark-marker) (+ (mark-marker) 4))))))

(defun tramp-rpc-protocol-try-read-message (buffer)
  "Try to read a complete message from BUFFER.
BUFFER should the process buffer containing received data.  Returns a
MESSAGE if a complete message is available, where MESSAGE is the decoded
response plist.  Returns nil if no complete message yet."
  (with-current-buffer buffer
    (when-let* ((start (+ (mark-marker) 4))
		(len (tramp-rpc-protocol-read-length buffer))
		((>= (point-max) (+ start len))))
      (set-marker (mark-marker) (+ start len))
      (tramp-rpc-protocol-decode-response buffer start))))

;; ============================================================================
;; Batch request support
;; ============================================================================

(defun tramp-rpc-protocol-encode-batch-request-with-id (requests)
  "Encode a batch request containing multiple REQUESTS.
REQUESTS is a list of (METHOD . PARAMS) cons cells.
Returns a cons cell (ID . BYTES) for ID tracking."
  (let ((batch-requests
         (mapcar (lambda (req)
                   `((method . ,(car req))
                     (params . ,(cdr req))))
                 requests)))
    (tramp-rpc-protocol-encode-request-with-id
     "batch"
     `((requests . ,(vconcat batch-requests))))))

(defun tramp-rpc-protocol-decode-batch-response (response)
  "Decode a batch response into a list of individual results.
RESPONSE is the decoded response plist from
`tramp-rpc-protocol-decode-response'.
Returns a list where each element is either:
  - The result value (if successful)
  - A plist (:error CODE :message MSG) if that sub-request failed."
  (let ((results-array (alist-get 'results (plist-get response :result))))
    (mapcar (lambda (result-obj)
              (if-let* ((error-obj (alist-get 'error result-obj)))
                  (list :error (alist-get 'code error-obj)
                        :message (alist-get 'message error-obj)
                        :data (alist-get 'data error-obj))
                (alist-get 'result result-obj)))
            results-array)))

;; ============================================================================
;; Unload support
;; ============================================================================

(add-hook 'tramp-rpc-unload-hook
	  (lambda ()
	    (when (featurep 'tramp-rpc-protocol)
	      (unload-feature 'tramp-rpc-protocol 'force))))

(provide 'tramp-rpc-protocol)
;;; tramp-rpc-protocol.el ends here
