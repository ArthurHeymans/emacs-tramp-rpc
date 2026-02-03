;;; tramp-rpc-protocol.el --- MessagePack-RPC protocol for TRAMP-RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Keywords: comm, processes
;; Package-Requires: ((emacs "30.1") (msgpack "0"))

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

(require 'msgpack)

(defvar tramp-rpc-protocol--request-id 0
  "Counter for generating unique request IDs.")

(defun tramp-rpc-protocol--next-id ()
  "Generate the next request ID."
  (cl-incf tramp-rpc-protocol--request-id))

(defun tramp-rpc-protocol--length-prefix (payload)
  "Add 4-byte big-endian length prefix to PAYLOAD (unibyte string)."
  (let ((len (length payload)))
    (concat (msgpack-unsigned-to-bytes len 4) payload)))

(defun tramp-rpc-protocol-encode-request (method params)
  "Encode a MessagePack-RPC request for METHOD with PARAMS.
Returns a length-prefixed unibyte string ready for transmission."
  (let* ((request `((version . "2.0")
                    (id . ,(tramp-rpc-protocol--next-id))
                    (method . ,method)
                    (params . ,params)))
         (payload (msgpack-encode request)))
    (tramp-rpc-protocol--length-prefix payload)))

(defun tramp-rpc-protocol-encode-request-with-id (method params)
  "Encode a MessagePack-RPC request for METHOD with PARAMS.
Returns a cons cell (ID . BYTES) for pipelining support."
  (let* ((id (tramp-rpc-protocol--next-id))
         (request `((version . "2.0")
                    (id . ,id)
                    (method . ,method)
                    (params . ,params)))
         (payload (msgpack-encode request)))
    (cons id (tramp-rpc-protocol--length-prefix payload))))

(defun tramp-rpc-protocol-decode-response (bytes)
  "Decode a MessagePack-RPC response from BYTES (unibyte string).
Returns a plist with :id, :result, and :error keys."
  (let* ((msgpack-map-type 'alist)
         (msgpack-key-type 'symbol)
         (msgpack-array-type 'list)
         (response (msgpack-read-from-string bytes))
         (id (alist-get 'id response))
         (result (alist-get 'result response))
         (error-obj (alist-get 'error response)))
    (list :id id
          :result result
          :error (when error-obj
                   (list :code (alist-get 'code error-obj)
                         :message (alist-get 'message error-obj)
                         :data (alist-get 'data error-obj))))))

(defun tramp-rpc-protocol-error-p (response)
  "Return non-nil if RESPONSE contains an error."
  (plist-get response :error))

(defun tramp-rpc-protocol-error-message (response)
  "Extract the error message from RESPONSE."
  (plist-get (plist-get response :error) :message))

(defun tramp-rpc-protocol-error-code (response)
  "Extract the error code from RESPONSE."
  (plist-get (plist-get response :error) :code))

;; Error codes
(defconst tramp-rpc-protocol-error-parse -32700)
(defconst tramp-rpc-protocol-error-invalid-request -32600)
(defconst tramp-rpc-protocol-error-method-not-found -32601)
(defconst tramp-rpc-protocol-error-invalid-params -32602)
(defconst tramp-rpc-protocol-error-internal -32603)
(defconst tramp-rpc-protocol-error-file-not-found -32001)
(defconst tramp-rpc-protocol-error-permission-denied -32002)
(defconst tramp-rpc-protocol-error-io -32003)
(defconst tramp-rpc-protocol-error-process -32004)

;; ============================================================================
;; Length-prefixed framing support
;; ============================================================================

(defun tramp-rpc-protocol-read-length (bytes)
  "Read the 4-byte big-endian length from BYTES.
Returns the length as an integer, or nil if BYTES is too short."
  (when (>= (length bytes) 4)
    (msgpack-bytes-to-unsigned (substring bytes 0 4))))

(defun tramp-rpc-protocol-try-read-message (buffer)
  "Try to read a complete message from BUFFER.
BUFFER should be a unibyte string containing received data.
Returns (MESSAGE . REMAINING) if a complete message is available,
where MESSAGE is the decoded response plist and REMAINING is
the leftover bytes.  Returns nil if no complete message yet."
  (when (>= (length buffer) 4)
    (let ((len (tramp-rpc-protocol-read-length buffer)))
      (when (and len (>= (length buffer) (+ 4 len)))
        (let* ((payload (substring buffer 4 (+ 4 len)))
               (remaining (substring buffer (+ 4 len)))
               (response (tramp-rpc-protocol-decode-response payload)))
          (cons response remaining))))))

;; ============================================================================
;; Batch request support
;; ============================================================================

(defun tramp-rpc-protocol-encode-batch-request (requests)
  "Encode a batch request containing multiple REQUESTS.
REQUESTS is a list of (METHOD . PARAMS) cons cells.
Returns length-prefixed bytes for a single RPC call to the batch method."
  (let ((batch-requests
         (mapcar (lambda (req)
                   `((method . ,(car req))
                     (params . ,(cdr req))))
                 requests)))
    (tramp-rpc-protocol-encode-request
     "batch"
     `((requests . ,(vconcat batch-requests))))))

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
                        :message (alist-get 'message error-obj))
                (alist-get 'result result-obj)))
            results-array)))

(provide 'tramp-rpc-protocol)
;;; tramp-rpc-protocol.el ends here
