;;; tramp-rpc-protocol.el --- JSON-RPC protocol for TRAMP-RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Keywords: comm, processes
;; Package-Requires: ((emacs "30.1"))

;; This file is part of tramp-rpc.

;; tramp-rpc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This file provides the JSON-RPC 2.0 protocol implementation for
;; communicating with the tramp-rpc-server binary.

;;; Code:

(require 'json)

(defvar tramp-rpc-protocol--request-id 0
  "Counter for generating unique request IDs.")

(defun tramp-rpc-protocol--next-id ()
  "Generate the next request ID."
  (cl-incf tramp-rpc-protocol--request-id))

(defun tramp-rpc-protocol-encode-request (method params)
  "Encode a JSON-RPC 2.0 request for METHOD with PARAMS.
Returns a JSON string."
  (let ((request `((jsonrpc . "2.0")
                   (id . ,(tramp-rpc-protocol--next-id))
                   (method . ,method)
                   (params . ,params))))
    (json-encode request)))

(defun tramp-rpc-protocol-encode-request-with-id (method params)
  "Encode a JSON-RPC 2.0 request for METHOD with PARAMS.
Returns a cons cell (ID . JSON-STRING) for pipelining support."
  (let* ((id (tramp-rpc-protocol--next-id))
         (request `((jsonrpc . "2.0")
                    (id . ,id)
                    (method . ,method)
                    (params . ,params))))
    (cons id (json-encode request))))

(defun tramp-rpc-protocol-decode-response (json-string)
  "Decode a JSON-RPC 2.0 response from JSON-STRING.
Returns a plist with :id, :result, and :error keys."
  (let* ((response (json-parse-string json-string
                                      :object-type 'alist
                                      :false-object nil
                                      :null-object nil))
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
;; Batch request support
;; ============================================================================

(defun tramp-rpc-protocol-encode-batch-request (requests)
  "Encode a batch request containing multiple REQUESTS.
REQUESTS is a list of (METHOD . PARAMS) cons cells.
Returns a JSON string for a single RPC call to the batch method."
  (let ((batch-requests
         (mapcar (lambda (req)
                   `((method . ,(car req))
                     (params . ,(cdr req))))
                 requests)))
    (tramp-rpc-protocol-encode-request
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
              (if-let ((error-obj (alist-get 'error result-obj)))
                  (list :error (alist-get 'code error-obj)
                        :message (alist-get 'message error-obj))
                (alist-get 'result result-obj)))
            results-array)))

(provide 'tramp-rpc-protocol)
;;; tramp-rpc-protocol.el ends here
