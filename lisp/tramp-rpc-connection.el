;;; tramp-rpc-connection.el --- Connection generation object for TRAMP-RPC -*- lexical-binding: t; -*-

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

;; One `tramp-rpc-connection' object describes one transport generation:
;; an SSH process running the RPC server, its response buffer, and every
;; piece of request state that belongs to exactly that generation.
;;
;; Keeping this state on the generation object rather than on process
;; properties and global tables means a replacement connection for the
;; same host cannot observe or be confused by requests, callbacks, or
;; cleanup flags of the generation it replaced.  The transport process
;; carries a single back pointer to its generation (see
;; `tramp-rpc--process-connection') so process filters and sentinels can
;; find it.
;;
;; This module is required by every other tramp-rpc module and must not
;; depend on any of them.

;;; Code:

(require 'cl-lib)

(cl-defstruct (tramp-rpc-connection
               (:constructor tramp-rpc--make-connection)
               (:copier nil))
  "State of one RPC transport generation."
  ;; The SSH transport process running the RPC server.
  process
  ;; Buffer receiving the framed RPC response stream.
  buffer
  ;; Buffer receiving the transport's standard error, or nil.
  stderr-buffer
  ;; TRAMP vector this generation serves.
  vec
  ;; Synchronous request IDs awaiting a response, newest first.
  (pending-ids nil)
  ;; Request ID -> response plist, for synchronous waiters.
  (pending-responses (make-hash-table :test 'eql))
  ;; Request ID -> callback, for asynchronous calls.
  (async-callbacks (make-hash-table :test 'eql))
  ;; Lifecycle bookkeeping set by connection cleanup.
  cleanup-started
  cleanup-reason
  cleanup-event
  ;; Non-nil once generation-local state has been retired.
  transport-cleaned
  ;; Non-nil once no response can be relied on from PROCESS.
  transport-dead
  ;; Non-nil once the transport sentinel wrapper has been installed.
  sentinel-installed)

(defun tramp-rpc--connection-transport (connection)
  "Return the transport process of CONNECTION, or nil when CONNECTION is nil.
Use this where a connection lookup may legitimately return nil, for
example before a host has ever been connected."
  (and connection (tramp-rpc-connection-process connection)))

(defun tramp-rpc--process-connection (process)
  "Return the connection generation owning transport PROCESS, or nil."
  (and (processp process)
       (process-get process :tramp-rpc-connection)))

(defun tramp-rpc--transport-dead-p (process)
  "Return non-nil when transport PROCESS's generation is known to be dead."
  (when-let* ((conn (tramp-rpc--process-connection process)))
    (and (eq (tramp-rpc-connection-process conn) process)
         (tramp-rpc-connection-transport-dead conn))))

(defun tramp-rpc--attach-connection (connection)
  "Record CONNECTION as the generation owning its transport process."
  (let ((process (tramp-rpc-connection-process connection)))
    (when (processp process)
      (process-put process :tramp-rpc-connection connection)
      (process-put process :tramp-rpc-vec (tramp-rpc-connection-vec connection)))
    connection))

(provide 'tramp-rpc-connection)
;;; tramp-rpc-connection.el ends here
