;;; tramp-rpc-majutsu.el --- Batched Majutsu queries for TRAMP-RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Snapshot jj before prefetching built-in log sections at that operation.
;; Status runs unpinned to retain snapshot-derived untracked-file information.
;; Only exact prefetched invocations are served
;; from the refresh-local cache.  Editors, signing and side-effecting commands
;; retain Majutsu's responsive subprocess runner.

;;; Code:

(require 'cl-lib)
(require 'tramp-rpc-transport)

(declare-function tramp-rpc-file-name-p "tramp-rpc")
(declare-function majutsu-jj--executable "ext:majutsu-jj" ())
(declare-function majutsu-process-jj-arguments "ext:majutsu-jj" (args))
(declare-function majutsu-process-environment "ext:majutsu-process" (&optional args))
(declare-function majutsu--process-file-supported-p "ext:majutsu-process" (infile destination))
(declare-function majutsu-log--build-args "ext:majutsu-log" ())
(declare-function majutsu-log--get-value "ext:majutsu-log" (mode &optional use-buffer-args))
(declare-function majutsu-workspace--ensure-template-plan "ext:majutsu-workspace" ())

(defvar majutsu-jj-global-arguments)
(defvar majutsu-jj--conflicted-files-template)
(defvar majutsu-log-sections-hook)

(defcustom tramp-rpc-majutsu-optimize t
  "Whether to batch built-in Majutsu log sections on RPC workspaces.
Log, workspace and conflict queries share a concrete jj operation.  Status
runs normally to retain untracked-file information.  Results are kept only
while rendering that refresh, never across refreshes."
  :type 'boolean
  :group 'tramp-rpc)

(defvar tramp-rpc-majutsu--cache nil
  "Dynamically bound exact-invocation cache for one log refresh.")
(defvar tramp-rpc-majutsu--connection nil
  "Transport generation owning the dynamically bound refresh cache.")
(defvar tramp-rpc-majutsu--roots nil
  "Dynamically bound workspace-root memo for one log setup.")

(defun tramp-rpc-majutsu--rpc-p ()
  "Return non-nil when optimizations apply to `default-directory'."
  (and tramp-rpc-majutsu-optimize
       (tramp-rpc-file-name-p default-directory)))

(defun tramp-rpc-majutsu--batching-p ()
  "Return non-nil when Majutsu captures query stderr in a buffer.
Its built-in sections otherwise send stderr to a file, which a batched
result cannot fill, so prefetching would only add round trips."
  (with-temp-buffer
    (majutsu--process-file-supported-p nil (list t (current-buffer)))))

(defun tramp-rpc-majutsu--environment (vec args)
  "Return Majutsu's effective remote environment for ARGS on VEC."
  (let ((process-environment (majutsu-process-environment args)))
    (tramp-rpc--process-environment
     vec (file-remote-p default-directory 'localname))))

(defun tramp-rpc-majutsu--key (program args env)
  "Identify an exact PROGRAM invocation with ARGS and ENV."
  (list default-directory program args env))

(defun tramp-rpc-majutsu--entry (key program args)
  "Build a batch entry for KEY, PROGRAM and ARGS in the current workspace."
  `((key . ,key) (cmd . ,program) (args . ,(vconcat args))
    (cwd . ,(file-remote-p default-directory 'localname))))

(defun tramp-rpc-majutsu--result (results key)
  "Find KEY in decoded batch RESULTS."
  (cdr (cl-find key results :key (lambda (entry) (format "%s" (car entry)))
                :test #'equal)))

(defun tramp-rpc-majutsu--usable-result-p (result)
  "Return non-nil for an actual completed child RESULT, including jj errors."
  (let ((exit (alist-get 'exit_code result)))
    (and (integerp exit) (>= exit 0)
         (not (eq (alist-get 'timed_out result) t))
         (not (eq (alist-get 'not_admitted result) t)))))

(defun tramp-rpc-majutsu--queries ()
  "Build the exact queries used by enabled built-in log sections."
  (delq nil
        (list
         (when (memq 'majutsu-log-insert-logs majutsu-log-sections-hook)
           (majutsu-process-jj-arguments (majutsu-log--build-args)))
         (when (memq 'majutsu-log-insert-status majutsu-log-sections-hook)
           (majutsu-process-jj-arguments '("status")))
         (when (and (memq 'majutsu-insert-workspaces majutsu-log-sections-hook)
                    (fboundp 'majutsu-workspace--ensure-template-plan))
           (majutsu-process-jj-arguments
            (list "workspace" "list" "-T"
                  (plist-get (majutsu-workspace--ensure-template-plan) :template))))
         (when (memq 'majutsu-log-insert-conflicts majutsu-log-sections-hook)
           ;; majutsu-jj-items disables color for machine-readable output.
           (append '("--color=never")
                   (cl-remove-if (lambda (arg) (string-prefix-p "--color" arg))
                                 majutsu-jj-global-arguments)
                   (list "file" "list" "-r" "@" "-T"
                         majutsu-jj--conflicted-files-template))))))

(defun tramp-rpc-majutsu--prefetch ()
  "Prefetch the current log's sections into the dynamically bound cache.
Do not override custom repository/operation controls.  Each batch has a
single environment, so group queries by their effective environment."
  (let ((queries (tramp-rpc-majutsu--queries))
        (status-args (majutsu-process-jj-arguments '("status"))))
    (when (and (length> queries 1)
               (tramp-rpc-majutsu--batching-p)
               ;; User log configuration can affect snapshotting.  It is not
               ;; shared by the operation query, unlike global configuration.
               (not (and (memq 'majutsu-log-insert-logs majutsu-log-sections-hook)
                         (cl-some (lambda (arg) (string-prefix-p "--config" arg))
                                  (car (majutsu-log--get-value
                                        'majutsu-log-mode 'current)))))
               ;; Buffer-specific log arguments can also select a different
               ;; repository or opt out of snapshots.  Leave those alone.
               (not (cl-some
                     (lambda (arg)
                       (or (string-prefix-p "-R" arg)
                           (string-prefix-p "--repository" arg)
                           (string-prefix-p "--at-op" arg)
                           (equal arg "--ignore-working-copy")
                           (equal arg "--no-integrate-operation")))
                     (apply #'append queries))))
      (let* ((vec (tramp-dissect-file-name default-directory))
             (program (majutsu-jj--executable))
             ;; Unlike --ignore-working-copy queries, op log snapshots and
             ;; reconciles concurrent operations, then reports that state.
             (args (append '("--color=never")
                           (cl-remove-if (lambda (arg) (string-prefix-p "--color" arg))
                                         majutsu-jj-global-arguments)
                           '("op" "log" "--no-graph" "-n" "1" "-T" "id")))
             (result (tramp-rpc-majutsu--result
                      (tramp-rpc--call
                       vec "commands.run_parallel"
                       `((commands . ,(vector (tramp-rpc-majutsu--entry
                                               "operation" program args)))
                         (env . ,(tramp-rpc-majutsu--environment vec args))))
                      "operation"))
             (operation (and (tramp-rpc-majutsu--usable-result-p result)
                             (zerop (alist-get 'exit_code result))
                             (string-trim (tramp-rpc--decode-output
                                           (alist-get 'stdout result)))))
             (connection (tramp-rpc--get-connection vec)))
        (when (and operation
                   (string-match-p "\\`[[:xdigit:]]\\{128\\}\\'" operation))
          (let ((groups (make-hash-table :test 'equal)))
            (dolist (query queries)
              (let ((env (tramp-rpc-majutsu--environment vec query)))
                (puthash env (cons query (gethash env groups)) groups)))
            (maphash
             (lambda (env group)
               (let* ((commands
                       (vconcat
                        (mapcar (lambda (query)
                                  (tramp-rpc-majutsu--entry
                                   (prin1-to-string query) program
                                   ;; Pinned status has no snapshot statistics
                                   ;; and would hide files jj refused to track.
                                   (if (equal query status-args)
                                       query
                                     (append (list "--at-operation" operation) query))))
                                group)))
                      (results (tramp-rpc--call
                                vec "commands.run_parallel"
                                `((commands . ,commands) (env . ,env))
                                connection)))
                 (dolist (query group)
                   (let ((data (tramp-rpc-majutsu--result
                                results (prin1-to-string query))))
                     (when (tramp-rpc-majutsu--usable-result-p data)
                       (puthash (tramp-rpc-majutsu--key program query env)
                                data tramp-rpc-majutsu--cache))))))
             groups))
          (setq tramp-rpc-majutsu--connection connection))))))

(defun tramp-rpc-majutsu--refresh (orig &rest args)
  "Prefetch around ORIG log refresh with ARGS, only on RPC workspaces."
  (if (not (tramp-rpc-majutsu--rpc-p))
      (apply orig args)
    (let ((tramp-rpc-majutsu--cache (make-hash-table :test 'equal))
          (tramp-rpc-majutsu--connection nil))
      ;; Prefetch is opportunistic.  Never swallow rendering errors or quits,
      ;; and never cache partial results after a transport failure.
      (condition-case err
          (tramp-rpc-majutsu--prefetch)
        (error
         (clrhash tramp-rpc-majutsu--cache)
         (tramp-rpc--debug "Majutsu prefetch skipped: %s" (error-message-string err))))
      (apply orig args))))

(defun tramp-rpc-majutsu--insert-output (destination output)
  "Insert OUTPUT at point in buffer DESTINATION, or discard it if nil."
  (when destination
    (with-current-buffer (cond ((eq destination t) (current-buffer))
                               ((stringp destination) (get-buffer-create destination))
                               (t destination))
      (let ((inhibit-read-only t))
        (insert output)))))

(defun tramp-rpc-majutsu--process-file
    (orig program &optional infile destination display &rest args)
  "Serve an exact prefetched invocation or call ORIG with PROGRAM and ARGS.
INFILE, DESTINATION and DISPLAY retain Majutsu's `process-file' semantics."
  (let ((data
         (when (and tramp-rpc-majutsu--cache
                    (tramp-rpc-majutsu--rpc-p)
                    (null infile) (null display)
                    (majutsu--process-file-supported-p infile destination)
                    ;; Batch results cannot reconstruct stdout/stderr
                    ;; interleaving.  Serve only separate captures, and keep
                    ;; legacy stderr-file destinations on the normal path.
                    (consp destination)
                    (not (eq (cadr destination) t))
                    (not (stringp (cadr destination)))
                    (eq tramp-rpc-majutsu--connection
                        (tramp-rpc--get-connection
                         (tramp-dissect-file-name default-directory))))
           (gethash (tramp-rpc-majutsu--key
                     program args
                     (tramp-rpc-majutsu--environment
                      (tramp-dissect-file-name default-directory) args))
                    tramp-rpc-majutsu--cache))))
    (if (not data)
        (progn
          ;; An unexpected command may mutate jj.  Stop serving this snapshot
          ;; rather than attempting to classify arbitrary commands or aliases.
          (when tramp-rpc-majutsu--cache
            (clrhash tramp-rpc-majutsu--cache))
          (apply orig program infile destination display args))
      (let ((stdout (tramp-rpc--decode-output (alist-get 'stdout data)))
            (stderr (tramp-rpc--decode-output (alist-get 'stderr data))))
        (tramp-rpc-majutsu--insert-output (car destination) stdout)
        (when (bufferp (cadr destination))
          (with-current-buffer (cadr destination)
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert stderr))))
        (alist-get 'exit_code data)))))

(defun tramp-rpc-majutsu--setup (orig &rest args)
  "Memoize workspace roots while ORIG opens the log with ARGS."
  (let ((tramp-rpc-majutsu--roots (make-hash-table :test 'equal)))
    (apply orig args)))

(defun tramp-rpc-majutsu--toplevel (orig &optional directory)
  "Memoize ORIG workspace-root lookups for DIRECTORY during log setup."
  (let ((default-directory (or directory default-directory)))
    (if (not (and tramp-rpc-majutsu--roots (tramp-rpc-majutsu--rpc-p)))
        (funcall orig directory)
      (let* ((key (list default-directory (majutsu-jj--executable)
                        majutsu-jj-global-arguments
                        (majutsu-process-environment '("workspace" "root"))))
             (root (gethash key tramp-rpc-majutsu--roots)))
        (or root
            (when-let* ((result (funcall orig directory)))
              (puthash key result tramp-rpc-majutsu--roots)))))))

(defconst tramp-rpc-majutsu--advice
  '((majutsu-log-refresh-buffer . tramp-rpc-majutsu--refresh)
    (majutsu-process-file . tramp-rpc-majutsu--process-file)
    (majutsu-log . tramp-rpc-majutsu--setup)
    (majutsu-toplevel . tramp-rpc-majutsu--toplevel))
  "Majutsu integration advice pairs.")

(defun tramp-rpc-majutsu-install-optional-handlers ()
  "Install integration if Majutsu is already loaded, without loading it."
  (when (featurep 'majutsu-log)
    (dolist (pair tramp-rpc-majutsu--advice)
      (unless (advice-member-p (cdr pair) (car pair))
        (advice-add (car pair) :around (cdr pair))))))

(defun tramp-rpc-majutsu-unload-function ()
  "Remove Majutsu integration advice."
  (dolist (pair tramp-rpc-majutsu--advice)
    (advice-remove (car pair) (cdr pair)))
  nil)

(provide 'tramp-rpc-majutsu)
;;; tramp-rpc-majutsu.el ends here
