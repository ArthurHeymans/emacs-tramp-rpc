;;; tramp-rpc-protocol.el --- MessagePack-RPC protocol for TRAMP-RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Assisted-by: various LLMs
;; Keywords: comm, processes

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

(defun tramp-rpc--add-external-operation (&rest args)
  "Call `tramp-add-external-operation' with ARGS when available."
  (when (fboundp 'tramp-add-external-operation)
    (apply (symbol-function 'tramp-add-external-operation) args)))

(defun tramp-rpc--remove-external-operation (&rest args)
  "Call `tramp-remove-external-operation' with ARGS when available."
  (when (fboundp 'tramp-remove-external-operation)
    (apply (symbol-function 'tramp-remove-external-operation) args)))

(defgroup tramp-rpc nil
  "TRAMP backend using RPC."
  :group 'tramp)

(defcustom tramp-rpc-debug nil
  "When non-nil, log debug messages to *tramp-rpc-debug* buffer.
Set to t to enable debugging for hang diagnosis."
  :type 'boolean
  :group 'tramp-rpc)

(defun tramp-rpc--debug-log (format-string &rest args)
  "Append a formatted debug line to the *tramp-rpc-debug* buffer.
FORMAT-STRING and ARGS are passed to `format'.

When TRAMP_RPC_DEBUG_LOG or TRAMP_RPC_DEBUG_DIR is set in the environment,
also append each line directly to a local log file.  This preserves CI
telemetry even when later tests unload TRAMP and remove debug buffers.
Callers should use the `tramp-rpc--debug' macro, which skips argument
evaluation entirely when `tramp-rpc-debug' is nil."
  (let* ((line (concat (format-time-string "[%F %T.%3N] ")
                       (apply #'format format-string args)
                       "\n"))
         (log-file (or (getenv "TRAMP_RPC_DEBUG_LOG")
                       (when-let* ((dir (getenv "TRAMP_RPC_DEBUG_DIR")))
                         (expand-file-name "tramp-rpc-debug-live.log" dir)))))
    (with-current-buffer (get-buffer-create "*tramp-rpc-debug*")
      (goto-char (point-max))
      (insert line))
    (when log-file
      (condition-case nil
          (progn
            (make-directory (file-name-directory log-file) t)
            (write-region line nil log-file 'append 'silent))
        (error nil)))))

(defmacro tramp-rpc--debug (format-string &rest args)
  "Log FORMAT-STRING with ARGS to the debug buffer when `tramp-rpc-debug' is set.
ARGS are not evaluated unless debugging is enabled, so callers may pass
expensive expressions such as buffer sizes or `prin1' of large values."
  (declare (indent 0) (debug t))
  `(when tramp-rpc-debug
     (tramp-rpc--debug-log ,format-string ,@args)))

(defvar tramp-rpc-protocol--request-id 0
  "Counter for generating unique request IDs.")

(defvar tramp-rpc-protocol--message-target nil
  "TRAMP vector or process used for level-6 protocol debug messages.")

(defconst tramp-rpc-protocol-max-frame-size (* 100 1024 1024)
  "Largest MessagePack frame accepted from or sent to the RPC server.")

(define-error 'tramp-rpc-protocol-frame-too-large
  "TRAMP-RPC MessagePack frame exceeds the configured limit")

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
         (payload (msgpack-encode request))
         (payload-size (length payload)))
    (when (> payload-size tramp-rpc-protocol-max-frame-size)
      (signal 'tramp-rpc-protocol-frame-too-large
              (list (format "RPC request %s is %d bytes; maximum is %d"
                            method payload-size
                            tramp-rpc-protocol-max-frame-size))))
    ;; Idle process reads are continuous long polls.  Defer their request log
    ;; until the response is known so empty polls produce no debug noise while
    ;; output, exits, and errors still retain the complete request/response pair.
    (if (tramp-rpc-protocol--polling-method-p method)
        (puthash id (cons tramp-rpc-protocol--message-target request)
                 tramp-rpc-protocol--deferred-poll-messages)
      (tramp-rpc-protocol--message request))
    (cons id (tramp-rpc-protocol--length-prefix payload))))

(defun tramp-rpc-protocol-decode-response (buffer start &optional end)
  "Decode a MessagePack-RPC response or notification in BUFFER from START.
START is the buffer position of the encoded object.
When END is non-nil, require the MessagePack object to consume exactly the
bounded frame ending there.
Returns a plist with :id, :result, and :error keys for responses.
For server-initiated notifications (no :id, has :method), returns a plist
with :notification t, :method, and :params keys."
  (let* ((response
	  (with-current-buffer buffer
	    (save-restriction
	      (when end
		(narrow-to-region start end))
	      (goto-char start)
	      (prog1
		  (msgpack-read :map-type 'alist
                                :key-type 'symbol
                                :array-type 'list
                                :bin-type 'msgpack-bin)
		(when (and end (not (eobp)))
		  (error "Trailing data in MessagePack frame"))))))
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
  "Return non-nil for an error RESPONSE."
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
(defconst tramp-rpc-protocol-error-process -32004)

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
BUFFER should be the process buffer containing received data.  Returns a
MESSAGE if a complete message is available, where MESSAGE is the decoded
response plist.  Returns nil if no complete message yet."
  (with-current-buffer buffer
    (when-let* ((start (+ (mark-marker) 4))
		(len (tramp-rpc-protocol-read-length buffer)))
      (when (> len tramp-rpc-protocol-max-frame-size)
	(error "RPC frame length %d exceeds maximum %d"
	       len tramp-rpc-protocol-max-frame-size))
      (let ((end (+ start len)))
	(when (>= (point-max) end)
	  (prog1 (tramp-rpc-protocol-decode-response buffer start end)
	    ;; Commit the framing cursor only after successful exact decoding.
	    (set-marker (mark-marker) end)))))))

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
;; MessagePack value helpers
;; ============================================================================

(defun tramp-rpc--decode-string (data)
  "Decode binary DATA to a multibyte UTF-8 string.
MessagePack `bin' values carry bytes; MessagePack `str' values are already
text strings.  Returns nil if DATA is nil."
  (cond
   ((null data) nil)
   ((msgpack-bin-p data)
    (decode-coding-string (msgpack-bin-string data) 'utf-8-unix))
   ((stringp data) data)
   (t data)))

(defun tramp-rpc--binary-bytes (data)
  "Return raw bytes from DATA, unwrapping MessagePack bin values."
  (cond
   ((msgpack-bin-p data) (msgpack-bin-string data))
   ((and (stringp data) (multibyte-string-p data))
    (encode-coding-string data 'utf-8-unix))
   (t data)))

(defun tramp-rpc--decode-output (data)
  "Decode binary process DATA as UTF-8.
This helper is for synchronous command/file paths.  Async relays keep their
bytes raw and let the relay process decoder handle incremental output.
The server never reports a process output encoding, so UTF-8 is assumed,
matching `tramp-sh' behavior for command output."
  (if data
      (decode-coding-string (tramp-rpc--binary-bytes data) 'utf-8-unix)
    ""))

(defun tramp-rpc--decode-filename (entry)
  "Get filename from directory ENTRY.
With MessagePack, filenames come as raw bytes - decode to UTF-8."
  (tramp-rpc--decode-string (alist-get 'name entry)))

(defun tramp-rpc--path-to-bytes (path)
  "Convert PATH to a unibyte string for MessagePack transmission.
Handles both multibyte UTF-8 strings and unibyte byte strings.
Strips Emacs file-name quoting (the /: prefix) before sending to
the server, since the remote side does not understand it."
  (let ((unquoted (file-name-unquote path)))
    (if (multibyte-string-p unquoted)
        (encode-coding-string unquoted 'utf-8-unix)
      unquoted)))

(defun tramp-rpc--path-to-string (path)
  "Return unquoted PATH as a text string for RPC fields typed as strings."
  (let ((unquoted (file-name-unquote path)))
    (if (multibyte-string-p unquoted)
        unquoted
      (decode-coding-string unquoted 'utf-8-unix))))

(defun tramp-rpc--path-to-bin (path)
  "Return PATH as an explicit MessagePack bin value."
  (msgpack-bin-make (tramp-rpc--path-to-bytes path)))

(defun tramp-rpc--path-to-compatible-value (path)
  "Return PATH as text when UTF-8-compatible, otherwise MessagePack binary.
Using text for ordinary paths preserves compatibility with older servers whose
path parameters predate binary-path support."
  (let* ((bytes (tramp-rpc--path-to-bytes path))
         (decoded (decode-coding-string bytes 'utf-8-unix)))
    (if (and (cl-every (lambda (char)
                         (not (eq (char-charset char) 'eight-bit)))
                       decoded)
             (equal bytes (encode-coding-string decoded 'utf-8-unix)))
        decoded
      (msgpack-bin-make bytes))))

(defun tramp-rpc--encode-path (path)
  "Encode PATH for transmission to path-or-bytes server parameters.
Returns an alist with PATH as an explicit MessagePack bin value."
  `((path . ,(tramp-rpc--path-to-bin path))))


(provide 'tramp-rpc-protocol)
;;; tramp-rpc-protocol.el ends here
