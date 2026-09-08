;;; tramp-rpc.el --- TRAMP backend using RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Assisted-by: various LLMs
;; Version: 0.13.1
;; URL: https://github.com/ArthurHeymans/emacs-tramp-rpc
;; Keywords: comm, processes, files
;; Package-Requires: ((emacs "30.1") (msgpack "0.1.1") (tramp "2.8.1.4"))

;; This file is part of tramp-rpc.

;; tramp-rpc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This package provides a TRAMP backend that uses a custom RPC server
;; instead of parsing shell command output.  This significantly improves
;; performance for remote file operations.
;;
;; Once installed, just access files using the "rpc" method:
;;   /rpc:user@host:/path/to/file
;;
;; The package autoloads automatically - no (require 'tramp-rpc) needed.
;;
;; FEATURES:
;; - Fast file operations via binary RPC protocol
;; - Async process support (make-process, start-file-process)
;; - VC mode integration works (git, etc.)
;;
;; HOW ASYNC PROCESSES WORK:
;; Remote processes are started via RPC and polled periodically for output.
;; A local pipe process serves as a relay to provide Emacs process semantics.
;; Process filters, sentinels, and signals all work as expected.
;;
;; OPTIONAL CONFIGURATION:
;; If you experience issues with diff-hl in Dired, you can disable it:
;;   (setq diff-hl-disable-on-remote t)
;;
;; AUTHENTICATION:
;; When ControlMaster is enabled (default), tramp-rpc establishes the SSH
;; ControlMaster connection first, which supports both key-based and password
;; authentication.  If your SSH key isn't available, you'll be prompted for
;; a password.  Subsequent operations reuse this connection without prompting.

;;; Code:

(eval-and-compile
  ;; When loading this file by absolute path during development, make sure the
  ;; sibling modules required below are loaded from the same checkout rather
  ;; than from an installed package earlier in `load-path'.
  (when-let* ((dir (file-name-directory (or load-file-name
                                            (and (boundp 'byte-compile-current-file)
                                                 byte-compile-current-file)
                                            buffer-file-name))))
    (add-to-list 'load-path dir)))

;; Autoload support - these forms are extracted to tramp-rpc-autoloads.el
;; and run at package-initialize time, before the full file is loaded.

;;;###autoload
(defconst tramp-rpc-method "rpc"
  "TRAMP method for RPC-based remote access.")

(eval-when-compile
  ;; Compile the deferred registration against TRAMP's real declarations
  ;; without loading TRAMP when the generated autoload file is evaluated.
  (require 'tramp))

;;;###autoload
(progn
  (defun tramp-rpc--autoload-register ()
    "Register TRAMP-RPC after TRAMP has initialized."
  ;; Check, that `tramp-rpc-method' is still bound.  It isn't after
  ;; unloading `tramp-rpc', but this body still exists as compiled
  ;; function in `after-load-alist'.
  (when (boundp 'tramp-rpc-method)
  ;; Register the method
  (add-to-list 'tramp-methods
               `(,tramp-rpc-method
                 ;; Placeholder; replaced with ssh's parameters below.
                 (tramp-login-args (("%h")))))

  ;; Give the rpc method all ssh connection parameters so it can serve
  ;; as a hop in tramp-sh multi-hop chains.  This matters in two ways:
  ;; - /rpc:host|sudo:root@host:/path is claimed by the tramp-rpc
  ;;   foreign handler (see `tramp-rpc--sudo-file-name-p'), which never
  ;;   uses these parameters for login.
  ;; - Chains ending in shell-based methods such as su or docker, e.g.
  ;;   /rpc:host|su::/path or /rpc:host|docker:container:/path, are
  ;;   handled by tramp-sh, which logs in through the rpc hop as if it
  ;;   were ssh (issue #99).  That happens without ever loading
  ;;   tramp-rpc.el, so the full parameter set must be installed here when
  ;;   TRAMP loads, not when the main file is loaded.  RPC-to-RPC
  ;;   chains remain RPC-backed.
  ;; This is future-proof: if ssh's parameters change in future TRAMP
  ;; versions, rpc automatically inherits the updates.
  ;; Suggested by Michael Albinus.
  (when-let* ((ssh-params (alist-get "ssh" tramp-methods nil nil #'equal))
              (rpc-entry (assoc tramp-rpc-method tramp-methods)))
    (setcdr rpc-entry ssh-params))

  ;; Enable direct-async-process for the rpc method.
  ;; This tells upstream tramp that our async processes are "direct"
  ;; (i.e., they use a direct SSH PTY connection rather than piping
  ;; through the control channel).  As a consequence, stderr cannot
  ;; be separated from stdout in async processes.
  (connection-local-set-profile-variables
   'tramp-rpc-connection-local-default-profile
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   `(:application tramp :protocol ,tramp-rpc-method)
   'tramp-rpc-connection-local-default-profile)

  ;; Define the predicate in the autoload file so it is available without
  ;; loading tramp-rpc.el.  This avoids recursive autoloading: TRAMP calls
  ;; the predicate to decide which handler to use, and if it were an
  ;; autoload stub it would load tramp-rpc.el which `(require 'tramp)'.
  (defvar tramp-rpc--sudo-file-name-p-in-progress nil
    "Non-nil while checking hidden rpc+sudo proxy expansion.")

  (defun tramp-rpc-file-name-p (vec-or-filename)
    "Check if VEC-OR-FILENAME is handled by TRAMP-RPC."
    (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename)))
      (string= (tramp-file-name-method vec) tramp-rpc-method)))

  ;; Detect privilege elevation paths with rpc hops, e.g.
  ;; /rpc:user@host|sudo:root@host:/path.  These are handled by the
  ;; tramp-rpc handler which starts the RPC server via sudo.
  (defun tramp-rpc--sudo-file-name-p (vec-or-filename)
    "Check if VEC-OR-FILENAME is a privilege elevation with an rpc hop."
    (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename))
                (target-host (tramp-file-name-host vec)))
      (and
       (not tramp-rpc--sudo-file-name-p-in-progress)
       (string= (tramp-file-name-method vec) "sudo")
       (tramp-get-method-parameter vec 'tramp-password-previous-hop)
       (or
        ;; Explicit ad-hoc hop syntax, as produced by `em-tramp' for Eshell:
        ;; /rpc:user@host|sudo:root@host:/path.
        (when-let* ((hop (tramp-file-name-hop vec))
                    (last-hop (car (last (split-string
                                          hop tramp-postfix-hop-regexp
                                          'omit))))
                    (hop-vec (tramp-dissect-hop-name
                              (concat last-hop tramp-postfix-hop-format)
                              'nodefault)))
          (and (string= (tramp-file-name-method hop-vec) tramp-rpc-method)
               (string= (tramp-file-name-host hop-vec) target-host)))
        ;; Native TRAMP helpers such as `tramp-file-name-with-sudo' often hide
        ;; ad-hoc hops in `tramp-default-proxies-alist'.  Ask TRAMP to expand
        ;; them and inspect the immediate hop before the sudo target.  Hide this
        ;; predicate while expanding; `tramp-compute-multi-hops' itself asks
        ;; `tramp-find-foreign-file-name-handler', which would otherwise recurse.
        ;; Bind `tramp-verbose' to zero: probing a non-matching hidden proxy is
        ;; not user-visible connection work and should not emit host-mismatch
        ;; messages.
        (unless (tramp-file-name-hop vec)
          (condition-case nil
              (let ((tramp-rpc--sudo-file-name-p-in-progress t)
                    (tramp-verbose 0)
                    (tramp-foreign-file-name-handler-alist
                     (delq nil
                           (mapcar
                            (lambda (entry)
                              (unless (eq (car entry)
                                          'tramp-rpc--sudo-file-name-p)
                                entry))
                            tramp-foreign-file-name-handler-alist))))
                (when-let* ((chain (tramp-compute-multi-hops vec))
                            (previous-hop (car (last (butlast chain)))))
                  (and (string= (tramp-file-name-method previous-hop)
                                tramp-rpc-method)
                       (string= (tramp-file-name-host previous-hop)
                                target-host))))
            (error nil)))))))

  ;; Register the foreign handler directly in the alist.  We cannot use
  ;; `tramp-register-foreign-file-name-handler' here because it tries to
  ;; read `tramp-rpc-file-name-handler-alist' (defined in the full file),
  ;; which isn't loaded yet.  The handler function itself is an autoload
  ;; stub that triggers loading of tramp-rpc.el on first use.
  (add-to-list 'tramp-foreign-file-name-handler-alist
               '(tramp-rpc-file-name-p . tramp-rpc-file-name-handler))
  ;; sudo+rpc handler must be checked, maps to the same handler.
  (add-to-list 'tramp-foreign-file-name-handler-alist
               '(tramp-rpc--sudo-file-name-p . tramp-rpc-file-name-handler))

  ;; Configure user and host name completion.
  (tramp-set-completion-function "rpc" tramp-completion-function-alist-ssh)

  ;; Allow the "rpc" method in multi-hop filename syntax.
  ;; TRAMP's `tramp-multi-hop-p' only returns t for tramp-sh methods,
  ;; which would cause `tramp-dissect-file-name' to reject filenames like
  ;; /rpc:hop|rpc:target:/path.  We extend it via `tramp-multi-hop-p-hook'.
  (defun tramp-rpc-multi-hop-p (vec)
    "Allow the rpc method and rpc+sudo paths in multi-hop chains.
This is called from `tramp-multi-hop-p-hook'."
    (or (string= (tramp-file-name-method vec) tramp-rpc-method)
        ;; Also allow previous-hop privilege methods when their final hop is rpc.
        ;; Keep this check explicit-only: `tramp-compute-multi-hops' calls this
        ;; hook, so consulting hidden proxy expansion here would recurse.
        (when-let* ((hop (tramp-file-name-hop vec))
                    (last-hop (car (last (split-string
                                          hop tramp-postfix-hop-regexp
                                          'omit))))
                    (target-host (tramp-file-name-host vec))
                    (hop-vec (tramp-dissect-hop-name
                              (concat last-hop tramp-postfix-hop-format)
                              'nodefault)))
          (and (string= (tramp-file-name-method vec) "sudo")
               (tramp-get-method-parameter vec 'tramp-password-previous-hop)
               (string= (tramp-file-name-method hop-vec) tramp-rpc-method)
               (string= (tramp-file-name-host hop-vec) target-host)))))
  (add-hook 'tramp-multi-hop-p-hook #'tramp-rpc-multi-hop-p)))

  ;; TRAMP runs `tramp--startup-hook' after its methods and handlers are
  ;; initialized.  If TRAMP was loaded first, register immediately instead.
  (if (featurep 'tramp)
      (tramp-rpc--autoload-register)
    (add-hook 'tramp--startup-hook #'tramp-rpc--autoload-register)))

;; Now the actual implementation
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'tramp)
(require 'tramp-sh)
(require 'tramp-rpc-protocol)
(require 'tramp-rpc-connection)
(require 'tramp-rpc-transport)

;; Package-Requires enforces this for package installations.  Keep loading
;; harmless for tooling that checks each file against Emacs's older bundled
;; TRAMP, and report the actionable error when the backend is actually used.
(defun tramp-rpc--check-tramp-version ()
  "Signal a clear error unless the loaded TRAMP version is supported."
  (when (version< tramp-version "2.8.1.4")
    (error "Tramp RPC requires Tramp >= 2.8.1.4, but %s is loaded"
           tramp-version)))

;; These predicates are emitted inside the single autoload form above.  The
;; compiler does not discover nested definitions, so declare only those two
;; autoload-owned functions used by the full implementation below.
(declare-function tramp-rpc--sudo-file-name-p "tramp-rpc")
(declare-function tramp-rpc-multi-hop-p "tramp-rpc")
(declare-function tramp-sh-handle-copy-file "tramp-sh"
                  (filename newname &optional ok-if-already-exists keep-date
                            preserve-uid-gid preserve-extended-attributes))


;; Helper modules only define functions while the main package is loading.
;; Runtime integration is installed explicitly after `tramp-rpc' is provided.
(require 'tramp-rpc-deploy)
(require 'tramp-rpc-process)
(require 'tramp-rpc-advice)
(require 'tramp-rpc-cache)
(require 'tramp-rpc-magit)


(defcustom tramp-rpc-compress-file-read (fboundp 'zlib-decompress-region)
  "When non-nil, use compression for file reads to enable faster transfers."
  :type 'boolean
  :group 'tramp-rpc)

(defun tramp-rpc--extract-file-read-content (rpc-result)
  "Extract and optionally decompress content from FILE.READ RPC-RESULT.
Signals `remote-file-error' on compressed payload decode failures."
  (let ((content (tramp-rpc--binary-bytes
                  (if (or (stringp rpc-result) (msgpack-bin-p rpc-result))
                      rpc-result
                    (alist-get 'content rpc-result)))))
    (if (and (not (stringp rpc-result))
             (alist-get 'compressed rpc-result))
        (let ((compression (or (alist-get 'compression rpc-result) "zlib")))
          (cond
           ((and (string= compression "zlib")
                 (fboundp 'zlib-decompress-region))
            (condition-case err
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert content)
                  (zlib-decompress-region (point-min) (point-max))
                  (buffer-string))
              (error
               (signal 'remote-file-error
                       (list "RPC"
                             (format "zlib decompression failed: %s" err))))))
           (t
            (signal 'remote-file-error
                    (list "RPC"
                          (format "Unsupported file.read compression: %s" compression))))))
      content)))

(defconst tramp-rpc--file-read-chunk-size (* 16 1024 1024)
  "Default maximum bytes requested by one `file.read' RPC.")

(defun tramp-rpc--read-chunk-size (vec)
  "Return the maximum `file.read' request size advertised for VEC.
Never exceed `tramp-rpc--file-read-chunk-size', which remains the fallback and
can be let-bound by tests and callers that need smaller requests.  Only the
system.info cached at connection setup is consulted; this never issues an RPC,
so a flushed cache simply falls back to the default."
  (let ((advertised
         (alist-get 'max_read_chunk_bytes
                    (tramp-rpc--cached-system-info vec))))
    (if (and (integerp advertised) (> advertised 0))
        (min advertised tramp-rpc--file-read-chunk-size)
      tramp-rpc--file-read-chunk-size)))

(defun tramp-rpc--file-read-params (localname &optional force-uncompressed)
  "Build params for `file.read' on LOCALNAME.
When `tramp-rpc-compress-file-read' is non-nil, request compression unless
FORCE-UNCOMPRESSED is non-nil."
  (let ((params (tramp-rpc--encode-path localname)))
    (when (and tramp-rpc-compress-file-read
               (not force-uncompressed))
      (push '(compress . t) params))
    params))

(defun tramp-rpc--read-file-bytes
    (vec localname &optional beg end force-uncompressed)
  "Read bytes from LOCALNAME on VEC in bounded, pipelined RPC chunks.
BEG and END are byte offsets.  The total length is fixed by END, or by the
`file_size' snapshot the server reports with the first chunk when END is nil
\(one `file.stat' fallback for servers without that field).  Concurrent
appends are excluded; concurrent truncation can yield a short result.  If no
size can be determined, only the first chunk is returned.
FORCE-UNCOMPRESSED is passed to `tramp-rpc--file-read-params'."
  (let* ((offset (or beg 0))
         (chunk-size (tramp-rpc--read-chunk-size vec))
         (requested (if end (min (- end offset) chunk-size) chunk-size)))
    (when (< requested 0)
      (signal 'args-out-of-range (list beg end)))
    (if (= requested 0)
        ""
      (let* ((params (tramp-rpc--file-read-params localname force-uncompressed))
             (_ (push `(offset . ,offset) params))
             (_ (push `(length . ,requested) params))
             (first-result (tramp-rpc--call vec "file.read" params))
             (first (tramp-rpc--extract-file-read-content first-result))
             (received (length first)))
        ;; Keep the common one-chunk path to one RPC.  Only discover the total
        ;; length after a completely full first chunk indicates more may exist.
        (if (< received requested)
            first
          (let* ((total (if end
                            (- end offset)
                          ;; Fix the END-nil read length to one size snapshot:
                          ;; prefer the fstat `file_size' the server sent with
                          ;; the first chunk (no extra RPC); fall back to one
                          ;; `file.stat' for older servers.
                          (max 0 (- (or (alist-get 'file_size first-result)
                                        (alist-get 'size
                                                   (tramp-rpc--call-file-stat
                                                    vec localname))
                                         received)
                                    offset))))
                 (next-offset (+ offset received))
                 (remaining (max 0 (- total received)))
                 (pieces (list first))
                 done)
            (while (and (> remaining 0) (not done))
              (let (requests sizes)
                (dotimes (_ 3)
                  (when (> remaining 0)
                    (let ((size (min remaining chunk-size))
                          (chunk-params
                           (tramp-rpc--file-read-params
                            localname force-uncompressed)))
                      (push `(offset . ,next-offset) chunk-params)
                      (push `(length . ,size) chunk-params)
                      (push (cons "file.read" chunk-params) requests)
                      (push size sizes)
                      (setq next-offset (+ next-offset size)
                            remaining (- remaining size)))))
                (cl-mapc
                 (lambda (result size)
                   ;; A short chunk means the file shrank, so later offsets
                   ;; are stale.  Discard trailing responses already received
                   ;; in this batch; `done' also prevents later batches.
                   (unless done
                     (when (tramp-rpc--batch-error-p result)
                       (tramp-rpc--signal-batch-failure
                        "file.read" localname result))
                     (let ((content (tramp-rpc--extract-file-read-content result)))
                       (push content pieces)
                       (when (< (length content) size)
                         (setq done t)))))
                 (tramp-rpc--call-batch vec (nreverse requests))
                 (nreverse sizes))))
            (apply #'concat (nreverse pieces))))))))

;; ============================================================================
;; File name handler operations
;; ============================================================================

(defun tramp-rpc--mode-executable-p (mode-string remote-uid remote-gid attrs groups)
  "Return non-nil when MODE-STRING permits the remote user to execute ATTRS.
REMOTE-UID is the remote user ID.
REMOTE-GID is the remote group ID."
  (if (equal remote-uid tramp-root-id-integer)
      ;; Root may execute only when some execute bit is set.
      (or (memq (aref mode-string 3) '(?x ?s))
          (memq (aref mode-string 6) '(?x ?s))
          (memq (aref mode-string 9) '(?x ?t)))
    (or
     ;; World executable.
     (memq (aref mode-string 9) '(?x ?t))
     ;; Owner executable.
     (and (memq (aref mode-string 3) '(?x ?s))
          (equal remote-uid (file-attribute-user-id attrs)))
     ;; Group executable and we are in that group.
     (and (memq (aref mode-string 6) '(?x ?s))
          (or (equal remote-gid (file-attribute-group-id attrs))
              (member (file-attribute-group-id attrs) groups))))))

(defun tramp-rpc-handle-file-executable-p (filename)
  "Like `file-executable-p' for TRAMP-RPC files.
Checks execute permission from `file-attributes' mode string and
the remote uid/gid.  No dedicated RPC call needed.
For symlinks, follows through to the target (like
`tramp-handle-file-readable-p' does).
FILENAME is the file name being handled."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-executable-p"
      (when-let* ((attrs (file-attributes filename 'integer)))
        (if (stringp (file-attribute-type attrs))
            ;; Symlink: follow it and check the target.
            (file-executable-p (file-truename filename))
          ;; Regular file or directory: check mode bits.
          (when-let* ((mode-string (file-attribute-modes attrs))
                      (remote-uid (tramp-get-remote-uid v 'integer))
                      (remote-gid (tramp-get-remote-gid v 'integer)))
            (tramp-rpc--mode-executable-p
             mode-string remote-uid remote-gid attrs
             (tramp-get-remote-groups v 'integer))))))))

(defun tramp-rpc--call-file-stat (vec localname &optional lstat)
  "Call file.stat for LOCALNAME on VEC, returning nil if file doesn't exist.
If LSTAT is non-nil, don't follow symlinks.
Uses `tramp-rpc--call' internally but converts file-missing and
ELOOP errors to nil (the file effectively doesn't exist for stat)."
  (let* ((use-cache (not (eq remote-file-name-inhibit-cache t)))
         (cache-key (tramp-rpc--file-stat-cache-key vec localname lstat))
         (cached (if use-cache
                     (tramp-rpc--cache-lookup tramp-rpc--file-stat-cache cache-key)
                   'not-cached)))
    (if (not (eq cached 'not-cached))
        cached
      (let ((params (append (tramp-rpc--encode-path localname)
                            (when lstat '((lstat . t))))))
        (condition-case err
            (let ((stat (tramp-rpc--call vec "file.stat" params)))
              (when use-cache
                (tramp-rpc--cache-file-stat-result vec localname stat lstat))
              stat)
          (file-missing
           (when use-cache
             (tramp-rpc--cache-file-stat-result vec localname nil lstat))
           nil)
          (file-error
           ;; Return nil for ELOOP (symlink loop) and ENOTDIR (path component
           ;; is not a directory, e.g. "file.py/.editorconfig") - the file
           ;; can't be resolved, so it effectively doesn't exist for stat purposes.
           (let ((message (error-message-string err)))
             (if (or (string-match-p "Too many levels of symbolic links" message)
                     (string-match-p "Not a directory" message))
                 (progn
                   (when use-cache
                     (tramp-rpc--cache-file-stat-result vec localname nil lstat))
                   nil)
               (signal (car err) (cdr err))))))))))

(defun tramp-rpc--file-exists-cache-lookup (filename)
  "Return cached `file-exists-p' value for FILENAME, or `not-cached'.
FILENAME is expanded before lookup.  Full cache inhibition bypasses the
stored entry without purging it."
  (if (eq remote-file-name-inhibit-cache t)
      'not-cached
    (tramp-rpc--cache-lookup
     tramp-rpc--file-exists-cache (expand-file-name filename))))

(defun tramp-rpc-handle-file-exists-p (filename)
  "Like `file-exists-p' for TRAMP-RPC files.
Uses TRAMP-RPC caches and Magit ancestor scan data before falling back to a
single `file.stat' RPC.  This avoids latency-amplified marker scans from
Projectile, project.el, and editorconfig during Magit section expansion.
FILENAME is the file name being handled."
  (or
   ;; Preserve TRAMP's completion/root and non-essential fast paths, but avoid
   ;; `tramp-skeleton-file-exists-p' here: its generic symlink preflight adds a
   ;; second stat for ordinary missing files.  Server `file.stat' follows
   ;; symlinks and maps dangling/cyclic links to nil, so one RPC is enough.
   (tramp-string-empty-or-nil-p (tramp-file-local-name filename))
   (string-equal (tramp-file-local-name filename) "/")
   (when (tramp-connectable-p filename)
     (with-parsed-tramp-file-name (expand-file-name filename) nil
       (with-tramp-file-property v localname "file-exists-p"
         (cond
          ((and-let* (((tramp-file-property-p v localname "file-attributes"))
                      (fa (tramp-get-file-property v localname "file-attributes"))
                      ((not (stringp (car fa)))))))
          (t
           (pcase (if (eq remote-file-name-inhibit-cache t)
                      'not-cached
                    (tramp-rpc-magit--file-exists-p filename))
             ('not-cached
              (pcase (tramp-rpc--file-exists-cache-lookup filename)
                ('not-cached
                 (let* ((stat (tramp-rpc--call-file-stat v localname))
                        (exists (if stat t nil)))
                   (unless (eq remote-file-name-inhibit-cache t)
                     (tramp-rpc--cache-put tramp-rpc--file-exists-cache
                                           (expand-file-name filename)
                                           exists))
                   exists))
                (cached cached)))
             (cached cached)))))))))

(defun tramp-rpc-handle-file-readable-p (filename)
  "Like `file-readable-p' for TRAMP-RPC files.
For cached-missing marker files, avoid delegating to TRAMP's generic handler,
which would otherwise perform another remote stat.
FILENAME is the file name being handled."
  (pcase (tramp-rpc-magit--file-exists-p filename)
    ('nil nil)
    (_ (tramp-handle-file-readable-p filename))))

(defun tramp-rpc-handle-file-regular-p (filename)
  "Like `file-regular-p' for TRAMP-RPC files.
FILENAME is the file name being handled."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-regular-p"
      (when-let* ((stat (tramp-rpc--call-file-stat v localname)))
        (equal (alist-get 'type stat) "file")))))

(defun tramp-rpc-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for TRAMP-RPC files.
FILENAME is the file name being handled."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (when-let* ((stat (tramp-rpc--call-file-stat v localname t))
                ((equal (alist-get 'type stat) "symlink"))
                (result (tramp-rpc--decode-string
                         (alist-get 'link_target stat))))
      ;; Quote a symlink target which looks remote.
      (if (tramp-tramp-file-p result)
          (file-name-quote result 'top)
        result))))

(defun tramp-rpc-handle-access-file (filename string)
  "Like `access-file' for TRAMP-RPC files.
FILENAME is the file name being checked.
STRING is prepended to any resulting error message."
  (condition-case err
      (tramp-handle-access-file filename string)
    (file-error
     (let* ((target (file-symlink-p filename))
            (target-file
             (and target
                  (if (file-name-absolute-p target)
                      (with-parsed-tramp-file-name filename nil
                        (tramp-make-tramp-file-name v target))
                    (expand-file-name target (file-name-directory filename))))))
       (if (and target
                (not (file-exists-p filename))
                ;; Preserve symlink-cycle errors; only dangling links are missing.
                (not (file-symlink-p target-file)))
           (signal 'file-missing (cdr err))
         (signal (car err) (cdr err)))))))


(defun tramp-rpc-handle-file-truename (filename)
  "Like `file-truename' for TRAMP-RPC files.
Resolves symlinks in the path.  For non-existing files, returns the
path unchanged (after resolving any symlinks in parent directories).
FILENAME is the file name being handled."
  (let* ((expanded (expand-file-name filename))
         (cached (and (not (eq remote-file-name-inhibit-cache t))
                      (tramp-rpc--cache-get tramp-rpc--file-truename-cache
                                            expanded))))
    (or cached
        (let ((truename
               ;; Use tramp-skeleton-file-truename which handles:
               ;; - Caching via with-tramp-file-property
               ;; - Proper filename expansion and unquoting
               ;; - Preserving trailing "/" and requoting
               ;; The BODY must return a localname, which the skeleton wraps with
               ;; tramp-make-tramp-file-name.
               (tramp-skeleton-file-truename filename
                 ;; Try RPC first for existing files (fast path)
                 (condition-case nil
                     (let* ((result (tramp-rpc--call v "file.truename"
                                                     (tramp-rpc--encode-path localname)))
                            (path (tramp-rpc--decode-string
                                   (if (and (listp result) (not (msgpack-bin-p result)))
                                       (alist-get 'path result)
                                     result))))
                       (or path localname))
                   ;; If file doesn't exist or has a symlink loop, fall back to
                   ;; symlink-chasing approach (same as tramp-handle-file-truename).
                   ;; ELOOP (symlink loop) maps to file-error, not file-missing.
                   (file-error
                    (let ((result (directory-file-name localname))
                          (numchase 0)
                          (numchase-limit 20)
                          symlink-target)
                      (while (and (setq symlink-target
                                        (file-symlink-p (tramp-make-tramp-file-name v result)))
                                  (< numchase numchase-limit))
                        (setq numchase (1+ numchase)
                              result
                              (if (tramp-tramp-file-p symlink-target)
                                  (file-name-quote symlink-target 'top)
                                (tramp-drop-volume-letter
                                 (expand-file-name
                                  symlink-target (file-name-directory result)))))
                        (when (>= numchase numchase-limit)
                          (tramp-error
                           v 'file-error
                           "Maximum number (%d) of symlinks exceeded" numchase-limit)))
                      (directory-file-name result)))))))
          (unless (eq remote-file-name-inhibit-cache t)
            (tramp-rpc--cache-put tramp-rpc--file-truename-cache expanded truename))
          truename))))


(defun tramp-rpc-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for TRAMP-RPC files.
FILENAME is the file name being handled.
ID-FORMAT controls how user and group IDs are represented."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property
        v localname (format "file-attributes-%s" id-format)
      (let ((result (tramp-rpc--call-file-stat v localname t)))  ; lstat=t
        ;; Populate file-exists cache as side effect when lstat is definitive
        ;; for follow semantics.  For symlinks, lstat success does not tell us
        ;; whether the target exists (dangling symlink), so leave file-exists-p
        ;; uncached.
        (unless (or (eq remote-file-name-inhibit-cache t)
                    (and result (equal (alist-get 'type result) "symlink")))
          (let ((expanded (expand-file-name filename)))
            (tramp-rpc--cache-put tramp-rpc--file-exists-cache
                                  expanded (if result t nil))))
        ;; `file-attributes' uses lstat, while `file-directory-p' follows
        ;; symlinks.  Only populate the directory predicate cache when the
        ;; lstat answer is definitive for the follow case too.
        (pcase (alist-get 'type result)
          ("directory"
           (tramp-set-file-property v localname "file-directory-p" t))
          ((or "file" (pred null))
           (tramp-set-file-property v localname "file-directory-p" nil)))
        (when result
          (tramp-rpc--convert-file-attributes result id-format))))))

(defun tramp-rpc-handle-file-directory-p (filename)
  "Like `file-directory-p' for TRAMP-RPC files.
Uses a single `file.stat' call instead of the generic TRAMP path
which resolves truename and then stats.
FILENAME is the file name being handled."
  (or
   ;; Preserve TRAMP's completion-time fast path semantics.
   (tramp-string-empty-or-nil-p (tramp-file-local-name filename))
   (string-equal (tramp-file-local-name filename) "/")
   (with-parsed-tramp-file-name (expand-file-name filename) nil
     (with-tramp-file-property v localname "file-directory-p"
       (let ((stat (tramp-rpc--call-file-stat v localname)))
         (and stat (equal (alist-get 'type stat) "directory")))))))

(defmacro tramp-rpc--with-set-file-attributes-rpc (filename &rest body)
  "Run BODY for a file attribute mutation on FILENAME without preflight stat.
This mirrors `tramp-skeleton-set-file-modes-times-uid-gid' cache flushing and
error handling, but lets the RPC server report missing-file and permission
errors instead of probing first."
  (declare (indent 1) (debug (form body)))
  `(with-parsed-tramp-file-name (expand-file-name ,filename) nil
     (with-tramp-saved-file-properties
         v localname
         ;; Keep the same properties TRAMP's skeleton preserves.  These are not
         ;; changed by chmod/touch/chown, while attributes and predicate caches
         ;; that depend on modes/timestamps must be recomputed.
         '("file-directory-p" "file-exists-p" "file-symlink-p" "file-truename")
       (tramp-flush-file-properties v localname))
    (condition-case err
        (let ((result (progn ,@body)))
          ;; Attribute mutations change mode/time/owner data used by
          ;; `file-attributes' and predicates such as `file-executable-p'.  TRAMP
          ;; properties were flushed above; also drop TRAMP-RPC's parallel
          ;; metadata caches so a prior `file.stat' result cannot survive chmod
          ;; or touch until TTL expiry.
          (tramp-rpc--invalidate-cache-for-path
           (tramp-make-tramp-file-name v localname))
          result)
      (error (if tramp-inhibit-errors-if-setting-file-attributes-fail
                 (display-warning 'tramp (error-message-string err))
               (signal (car err) (cdr err)))))))

(defun tramp-rpc-handle-set-file-modes (filename mode &optional _flag)
  "Like `set-file-modes' for TRAMP-RPC files.
FILENAME is the file name being handled.
MODE is the requested file mode."
  (tramp-rpc--with-set-file-attributes-rpc filename
    (tramp-rpc--call v "file.set_modes"
                     (append (tramp-rpc--encode-path localname)
                             `((mode . ,mode))))))

(defun tramp-rpc-handle-set-file-times (filename &optional timestamp flag)
  "Like `set-file-times' for TRAMP-RPC files.
FILENAME is the file name being handled.
TIMESTAMP is the requested access and modification time, or nil for now.
FLAG equal to `nofollow' prevents following a symbolic link."
  (tramp-rpc--with-set-file-attributes-rpc filename
    (let* ((mtime (floor (float-time (or timestamp (current-time)))))
           (tramp-name (tramp-make-tramp-file-name v localname))
           (result
            (tramp-rpc--call v "file.set_times"
                             (append (tramp-rpc--encode-path localname)
                                     `((mtime . ,mtime)
                                       (nofollow . ,(if flag t :msgpack-false)))))))
      ;; Synthetic symlink watches intentionally do not install a server watch,
      ;; because the notify backend follows symlink watch paths.  Attribute
      ;; changes made through this handler are the one symlink event we can
      ;; report precisely without watching the target.
      (when (and flag (tramp-rpc--file-notify-synthetic-watch-p tramp-name))
        (tramp-rpc--file-notify-dispatch "attribute-changed" tramp-name))
      result)))


;; ============================================================================
;; High-level operations
;; ============================================================================

(defun tramp-rpc--dir-locals-candidate-files (&optional base-el-only)
  "Return dir-locals candidate file names.
When BASE-EL-ONLY is non-nil, return only `dir-locals-file'."
  (let ((file-1 dir-locals-file)
        (file-2 (and (string-match "\\.el\\'" dir-locals-file)
                     (replace-match "-2.el" t nil dir-locals-file))))
    (if base-el-only
        (list file-1)
      (delq nil (list file-1 file-2)))))

(defun tramp-rpc--quote-localname (original-localname new-localname)
  "Return NEW-LOCALNAME with ORIGINAL-LOCALNAME quoting style.
If ORIGINAL-LOCALNAME is file-name-quoted, quote NEW-LOCALNAME too."
  (if (file-name-quoted-p original-localname)
      (file-name-quote new-localname)
    new-localname))

(defun tramp-rpc--parent-directory (directory)
  "Return parent directory for DIRECTORY, or nil at filesystem root."
  (let* ((current (directory-file-name directory))
         (parent (file-name-directory current)))
    (when parent
      (let ((parent (directory-file-name parent)))
        (unless (equal parent current)
          parent)))))

(defun tramp-rpc--locate-search-directory (path)
  "Return lexical search directory for locate-dominating PATH."
  (if (string-suffix-p "/" path)
      (directory-file-name path)
    (let ((normalized (directory-file-name path)))
      (or (and (file-name-directory normalized)
               (directory-file-name (file-name-directory normalized)))
          normalized))))

(defun tramp-rpc--locate-dominating-before-stop-p (search-path dominating-dir)
  "Return non-nil when DOMINATING-DIR is reachable without crossing stop regexp.
SEARCH-PATH and DOMINATING-DIR must use the same pathname form (remote/local)
that `locate-dominating-stop-dir-regexp' is expected to match."
  (let ((stop locate-dominating-stop-dir-regexp))
    (if (or (null stop) (equal stop ""))
        t
      (let ((current (tramp-rpc--locate-search-directory search-path))
            (target (directory-file-name dominating-dir))
            (blocked nil))
        (while (and current (not blocked) (not (equal current target)))
          (when (string-match-p stop (file-name-as-directory current))
            (setq blocked t))
          (setq current (tramp-rpc--parent-directory current)))
        (and (not blocked)
             (equal current target))))))

(defun tramp-rpc-handle-dir-locals--all-files (directory &optional base-el-only)
  "Like `dir-locals--all-files' for TRAMP-RPC files.
Return readable dir-locals files in DIRECTORY in increasing priority order.
BASE-EL-ONLY non-nil excludes the secondary `-2.el' file."
  (with-parsed-tramp-file-name (expand-file-name directory) nil
    ;; Unquote file names (e.g. /: prefix) before sending to server.
    (let* ((quoted-localname localname)
           (localdir (directory-file-name (file-name-unquote localname)))
           (names (tramp-rpc--dir-locals-candidate-files base-el-only))
           (result (tramp-rpc--call
                    v "highlevel.test_files_in_dir"
                    `((directory . ,(tramp-rpc--path-to-string localdir))
                      (names . ,(vconcat names))))))
      (mapcar (lambda (path)
                (tramp-make-tramp-file-name
                 v
                 (tramp-rpc--quote-localname
                  quoted-localname
                  (tramp-rpc--decode-string path))))
              result))))

(defun tramp-rpc-handle-locate-dominating-file (file name)
  "Like `locate-dominating-file' for TRAMP-RPC files.
For string/list NAME, uses a high-level RPC call.  Predicate NAME falls back
to the built-in implementation.
FILE is the file name being handled."
  (if (functionp name)
      (tramp-run-real-handler #'locate-dominating-file (list file name))
    (with-parsed-tramp-file-name (expand-file-name file) nil
      ;; Unquote file names (e.g. /: prefix) before sending to server.
      (let* ((quoted-localname localname)
             (localname (file-name-unquote localname))
             (names (ensure-list name))
             (result (tramp-rpc--call
                      v "highlevel.locate_dominating_file_multi"
                      `((file . ,(tramp-rpc--path-to-string localname))
                        (names . ,(vconcat names))))))
        (when-let* ((marker (car result))
                    (marker-path (tramp-rpc--decode-string marker)))
          (let* ((dominating-dir (file-name-directory marker-path))
                 (search-remote
                  (tramp-make-tramp-file-name
                   v
                   (tramp-rpc--quote-localname quoted-localname localname)))
                 (dominating-remote
                  (tramp-make-tramp-file-name
                   v
                   (tramp-rpc--quote-localname quoted-localname dominating-dir))))
            (when (tramp-rpc--locate-dominating-before-stop-p
                   search-remote dominating-remote)
              dominating-remote)))))))

(defun tramp-rpc--dir-locals-cache-update (file cache)
  "Call RPC helper for `dir-locals-find-file' update using FILE and CACHE."
  (with-parsed-tramp-file-name (expand-file-name file) nil
    ;; Unquote file names (e.g. /: prefix) before sending to server.
    (let* ((localname (file-name-unquote localname))
           (file-connection (file-remote-p file))
           (names (tramp-rpc--dir-locals-candidate-files nil))
           (cache-dirs
            (seq-uniq
             (cl-loop
              for cache-entry in cache
              for cache-dir = (car cache-entry)
              when (string= file-connection (file-remote-p cache-dir))
              collect (file-name-unquote (file-local-name cache-dir))))))
      (tramp-rpc--call
       v "highlevel.dir_locals_find_file_cache_update"
       `((file . ,(tramp-rpc--path-to-string localname))
         (names . ,(vconcat names))
         (cache_dirs . ,(vconcat cache-dirs)))))))

(defun tramp-rpc--dir-locals-latest-mtime (files)
  "Return latest mtime from FILES alist data as a Lisp time value."
  (let ((latest 0))
    (dolist (f files latest)
      (let ((f-time (seconds-to-time (alist-get 'mtime f))))
        (when (time-less-p latest f-time)
          (setq latest f-time))))))

(defun tramp-rpc--dir-locals-cache-covers-p (locals-dir cache-dir)
  "Return non-nil when CACHE-DIR is at or below LOCALS-DIR.
This is a lexical path check: the directories can be remote or not yet exist."
  (let ((locals (file-name-as-directory (directory-file-name locals-dir)))
        (cache (file-name-as-directory (directory-file-name cache-dir))))
    (or (equal locals cache)
        (string-prefix-p locals cache))))

(defun tramp-rpc-handle-dir-locals-find-file (file)
  "Like `dir-locals-find-file' for TRAMP-RPC files.
FILE is the file name being handled."
  (let* ((file (expand-file-name file))
         (file-connection (file-remote-p file))
         (cache-update (tramp-rpc--dir-locals-cache-update file dir-locals-directory-cache))
         (locals-dir-update (alist-get 'locals cache-update))
         (locals-dir (when locals-dir-update
                       (file-name-as-directory
                        (concat file-connection
                                (tramp-rpc--decode-string
                                 (alist-get 'dir locals-dir-update))))))
         (cache-dir-update (alist-get 'cache cache-update))
         (cache-dir (when cache-dir-update
                      (file-name-as-directory
                       (concat file-connection
                               (tramp-rpc--decode-string
                                (alist-get 'dir cache-dir-update))))))
         (dir-elt (when cache-dir
                    (seq-find (lambda (elt) (string= (car elt) cache-dir))
                              dir-locals-directory-cache))))
    (if (and dir-elt
             (or (null locals-dir)
                 (tramp-rpc--dir-locals-cache-covers-p locals-dir (car dir-elt))))
        ;; Potential cache hit, verify mtimes.
        (if (or (null (nth 2 dir-elt))
                (let ((cached-files (alist-get 'files cache-dir-update)))
                  (and cached-files
                       (time-equal-p
                        (nth 2 dir-elt)
                        (tramp-rpc--dir-locals-latest-mtime cached-files)))))
            dir-elt
          (progn
            ;; Cache entry invalid, clear and return discovered locals dir.
            (setq dir-locals-directory-cache
                  (delq dir-elt dir-locals-directory-cache))
            locals-dir))
      ;; No cache entry.
      locals-dir)))

;; ============================================================================
;; Directory operations
;; ============================================================================

(defun tramp-rpc--apply-directory-count (entries count)
  "Apply Emacs `directory-files' COUNT semantics to ENTRIES."
  (when count
    (unless (natnump count)
      (signal 'wrong-type-argument (list 'wholenump count)))
    (setq entries (seq-take entries count)))
  entries)

(defun tramp-rpc-handle-directory-files (directory &optional full match nosort count)
  "Like `directory-files' for TRAMP-RPC files.

Use the server's `dir.list' result directly instead of the generic
TRAMP skeleton.  The skeleton first checks `file-exists-p' and
`file-directory-p', which costs extra network round-trips on high-latency
links.  `dir.list' already reports missing or non-directory paths as errors,
so a single RPC can both validate and list the directory.
DIRECTORY is the directory being handled.
FULL non-nil returns names prefixed with DIRECTORY.
MATCH, when non-nil, is a regexp matched against each relative file name.
NOSORT non-nil preserves the server's directory order.
COUNT limits the number of returned entries."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         result)
    (with-parsed-tramp-file-name directory nil
      (setq result
            (with-tramp-file-property v localname "directory-files"
              (mapcar #'tramp-rpc--decode-filename
                      (tramp-rpc--call v "dir.list"
                                       (append (tramp-rpc--encode-path localname)
                                               '((include_attrs . :msgpack-false)
                                                 (include_hidden . t))))))))
    (when match
      (setq result (cl-remove-if-not
                    (lambda (name) (string-match-p match name))
                    result)))
    (unless nosort
      (setq result (sort (copy-sequence result) #'string<)))
    (setq result (tramp-rpc--apply-directory-count result count))
    (if full
        (mapcar (lambda (name) (concat directory name)) result)
      result)))

(defun tramp-rpc-handle-directory-files-and-attributes
    (directory &optional full match nosort id-format count)
  "Like `directory-files-and-attributes' for TRAMP-RPC files.
DIRECTORY is the directory being handled.
FULL non-nil returns names prefixed with DIRECTORY.
MATCH, when non-nil, is a regexp matched against each relative file name.
NOSORT non-nil preserves the server's directory order.
ID-FORMAT controls how user and group IDs are represented.
COUNT limits the number of returned entries."
  (with-parsed-tramp-file-name (expand-file-name directory) nil
    (let* ((result (tramp-rpc--call v "dir.list"
                                    (append (tramp-rpc--encode-path localname)
                                            '((include_attrs . t)
                                              (include_hidden . t)))))
           (entries (mapcar
                     (lambda (entry)
                       (let* ((name (tramp-rpc--decode-filename entry))
                              (attrs (alist-get 'attrs entry))
                              (full-name (if full
                                             (tramp-make-tramp-file-name
                                              v (expand-file-name name localname))
                                           name)))
                         (cons full-name
                               (when attrs
                                 (tramp-rpc--convert-file-attributes attrs id-format)))))
                     result)))
      ;; Filter by match pattern
      (when match
        (setq entries (cl-remove-if-not
                       (lambda (e) (string-match-p match (car e)))
                       entries)))
      ;; Sort unless nosort
      (unless nosort
        (setq entries (sort entries (lambda (a b) (string< (car a) (car b))))))
      (tramp-rpc--apply-directory-count entries count))))

;; Declared in ls-lisp.el; dynamically rebound for RPC Dired formatting.
(defvar ls-lisp-format-time-list)
(defvar ls-lisp-use-localized-time-format)
;; Declared in Tramp 2.8.1.3+; forward-declare so byte compiler treats it as dynamic.
(defvar tramp-fnac-add-trailing-slash)

(defun tramp-rpc-handle-insert-directory
    (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for TRAMP-RPC files.
Use `ls-lisp' via TRAMP, but force GNU ls-like date strings so RPC Dired
matches SSH Dired output style.
FILENAME is the file name being handled.
SWITCHES contains the Dired listing switches.
WILDCARD non-nil means to treat FILENAME as a shell wildcard.
FULL-DIRECTORY-P non-nil requests a full listing of FILENAME as a directory."
  ;; `ls-lisp-format-time-list' is honored only for the C/POSIX locale unless
  ;; `ls-lisp-use-localized-time-format' is non-nil; force it so the GNU-style
  ;; format applies regardless of the local locale.
  (let ((ls-lisp-format-time-list '("%b %e %H:%M" "%b %e  %Y"))
        (ls-lisp-use-localized-time-format t))
    (tramp-handle-insert-directory
     filename switches wildcard full-directory-p)))

(defun tramp-rpc-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for TRAMP-RPC files.
FILENAME is the file name being handled.
DIRECTORY is the directory being handled."
  ;; Suppress check for trailing slash in `tramp-skeleton-file-name-all-completions'.
  (let (tramp-fnac-add-trailing-slash)
    (tramp-skeleton-file-name-all-completions filename directory
      (with-parsed-tramp-file-name (expand-file-name directory) nil
	;; Get all entries in the directory. Convert vector to list if needed.
	(let ((entries
	       (append (tramp-rpc--call v "dir.list"
				       (append (tramp-rpc--encode-path localname)
					       '((include_attrs . :msgpack-false)
                                                 (include_hidden . t))))
		       nil)))
          ;; Build list of names with trailing / for directories
          (mapcar (lambda (entry)
                    (let ((name (tramp-rpc--decode-filename entry))
                          (file-type (alist-get 'type entry)))
                      (if (equal file-type "directory")
                          (concat name "/")
			name)))
                  entries))))))

(defun tramp-rpc--localname-prefixes (localname)
  "Return non-root path prefixes for absolute LOCALNAME.
For example, /tmp/a/b returns /tmp, /tmp/a, and /tmp/a/b."
  (let ((prefixes nil)
        (current nil))
    (dolist (component (split-string (directory-file-name localname) "/" t))
      (setq current (if current
                        (concat current "/" component)
                      (concat "/" component)))
      (push current prefixes))
    (nreverse prefixes)))

(defun tramp-rpc--invalidate-mkdir-caches (vec filename localname parents)
  "Invalidate caches after mkdir of FILENAME / LOCALNAME on VEC.
When PARENTS is non-nil, the server may create any missing path prefix, so
flush every prefix to avoid stale negative predicate results."
  (if parents
      (dolist (prefix (tramp-rpc--localname-prefixes localname))
        (tramp-flush-file-properties vec prefix)
        (when-let* ((parent (file-name-directory prefix)))
          (tramp-flush-directory-properties vec parent))
        (tramp-rpc--invalidate-cache-for-path
         (tramp-make-tramp-file-name vec prefix)))
    ;; Preserve the non-PARENTS invalidation shape: only the created directory,
    ;; its parent directory properties, and the custom caches for FILENAME.
    (tramp-flush-directory-properties vec (file-name-directory localname))
    (tramp-flush-file-properties vec localname)
    (when-let* ((parent (file-name-directory filename)))
      (tramp-rpc--invalidate-cache-for-path parent))
    (tramp-rpc--invalidate-cache-for-path filename)))

(defun tramp-rpc-handle-make-directory (dir &optional parents)
  "Like `make-directory' for TRAMP-RPC files.

Delegate parent creation to the server instead of using
`tramp-skeleton-make-directory'.  The generic skeleton probes each path
component with separate `file-exists-p' / `file-directory-p' calls before the
actual mkdir.  Server-side `create_dir_all' performs the same validation in one
network round-trip.
DIR is the directory being handled.
PARENTS non-nil creates missing parent directories."
  (let ((dir (directory-file-name (expand-file-name dir))))
    (with-parsed-tramp-file-name dir nil
      (let ((created
             (tramp-rpc--call v "dir.create"
                              (append (tramp-rpc--encode-path localname)
                                      `((parents . ,(if parents t :msgpack-false))
                                        (mode . ,(default-file-modes)))))))
        (tramp-rpc--invalidate-mkdir-caches v dir localname parents)
        ;; Match `make-directory' return convention: nil when a directory was
        ;; created, t when PARENTS was non-nil and the directory already existed.
        (and parents (not created))))))

(defun tramp-rpc-handle-delete-directory (directory &optional recursive trash)
  "Like `delete-directory' for TRAMP-RPC files.
DIRECTORY is the directory being handled.
RECURSIVE non-nil removes directory contents recursively.
TRASH non-nil requests moving the directory to the trash."
  ;; Follow TRAMP's skeleton semantics for TRASH.  Callers that want
  ;; direct deletion can bind
  ;; `remote-file-name-inhibit-delete-by-moving-to-trash'.
  (tramp-skeleton-delete-directory directory recursive trash
    (tramp-rpc--call v "dir.remove"
                     (append (tramp-rpc--encode-path localname)
                             `((recursive . ,(if recursive t :msgpack-false))))))
  (if recursive
      (tramp-rpc--invalidate-cache-for-subtree directory)
    (tramp-rpc--invalidate-cache-for-path directory)))

;; ============================================================================
;; File I/O operations
;; ============================================================================

(defun tramp-rpc-handle-write-region
    (start end filename &optional append visit lockname mustbenew)
  "Like `write-region' for TRAMP-RPC files.
START and END normally delimit the buffer region to write.  A nil START
means the whole buffer, while a string START is written directly; END is
ignored in both cases.
FILENAME is the destination file name.
APPEND non-nil appends; an integer APPEND writes at that file offset.
VISIT t marks the buffer as visiting FILENAME, while a string marks it as
visiting that file; other non-nil values suppress the \"Wrote file\" message.
LOCKNAME overrides the file name used for locking.
MUSTBENEW requests an overwrite check; `excl' rejects an existing file."
  (tramp-skeleton-write-region
      start end filename append visit lockname mustbenew
    ;; If START is a string, write it directly; otherwise extract from buffer.
    ;; When APPEND is an integer, it is a file offset for writing.
    (let* ((content (if (stringp start)
                        start
                      (buffer-substring-no-properties
                       (or start (point-min))
                       (or end (point-max)))))
           ;; Match `write-region': a dynamic output override takes precedence
           ;; over the visited buffer's coding system.
           (coding (or coding-system-for-write
                       (and (not (stringp start))
                            buffer-file-coding-system)
                       'utf-8-unix))
           (content-bytes (encode-coding-string content coding))
           ;; Integer APPEND is the offset understood by `file.write'; do not
           ;; rewrite the remote prefix, which would discard its suffix.
           (real-append (and append (not (integerp append))))
           (params (append (tramp-rpc--encode-path localname)
                           `((content . ,(msgpack-bin-make content-bytes))
                             (append . ,(if real-append t :msgpack-false))
                             ,@(when (integerp append)
                                 `((offset . ,append)))))))

      (let ((tramp-rpc--suppress-fs-notifications t))
        (tramp-rpc--call v "file.write" params))

      ;; Invalidate caches for the written file
      (tramp-rpc--invalidate-cache-for-path filename)

      ;; Tell the skeleton which coding system we used.
      ;; `encode-coding-string' sets `last-coding-system-used', but
      ;; the skeleton shadows it with a local `let', so use the value
      ;; from our `coding' variable instead.
      (setq coding-system-used coding))))

(defun tramp-rpc--stat-type (stat)
  "Return file type string from STAT, or nil."
  (and stat (alist-get 'type stat)))

(cl-defun tramp-rpc--copy-file-same-remote
    (filename newname ok-if-already-exists keep-time preserve-uid-gid
              preserve-permissions)
  "Copy FILENAME to NEWNAME on one TRAMP-RPC remote with fewer round-trips.
OK-IF-ALREADY-EXISTS controls existing-destination handling.
KEEP-TIME non-nil preserves timestamps.
PRESERVE-UID-GID non-nil preserves ownership for regular files.
PRESERVE-PERMISSIONS non-nil preserves file permissions."
  (with-parsed-tramp-file-name filename v1
    (with-parsed-tramp-file-name newname v2
      (let* ((stats (tramp-rpc--call-batch
                     v1
                     `(("file.stat" . ,(append (tramp-rpc--encode-path v1-localname)
                                                '((lstat . t))))
                       ("file.stat" . ,(append (tramp-rpc--encode-path v2-localname)
                                                '((lstat . t)))))))
             (source-stat (tramp-rpc--batch-result-or-signal
                           "file.stat" filename (nth 0 stats)))
             (dest-stat (tramp-rpc--batch-result-or-signal
                         "file.stat" newname (nth 1 stats)))
             (source-type (tramp-rpc--stat-type source-stat))
             (dest-type (tramp-rpc--stat-type dest-stat)))
        (unless source-stat
          (signal 'file-missing (list "Opening input file" "No such file" filename)))
        (when (and (directory-name-p newname)
                   (equal dest-type "directory"))
          (cl-return-from tramp-rpc--copy-file-same-remote
            (tramp-rpc--copy-file-same-remote
             filename
             (expand-file-name (file-name-nondirectory filename) newname)
             ok-if-already-exists keep-time preserve-uid-gid
             preserve-permissions)))
        (unless ok-if-already-exists
          (when dest-stat
            (signal 'file-already-exists (list newname))))
        (when (and (equal dest-type "directory")
                   (not (directory-name-p newname)))
          (signal 'file-error (list "File is a directory" newname)))
        (cond
         ((equal source-type "directory")
          (copy-directory filename newname keep-time t))
         ((equal source-type "symlink")
          (make-symbolic-link
           (tramp-rpc--decode-string (alist-get 'link_target source-stat))
           newname ok-if-already-exists))
         (t
          (tramp-rpc--call v1 "file.copy"
                           `((src . ,(tramp-rpc--path-to-bin
                                      (file-name-unquote v1-localname)))
                             (dest . ,(tramp-rpc--path-to-bin
                                       (file-name-unquote v2-localname)))
                             (preserve . ,(if (or keep-time preserve-uid-gid
                                                   preserve-permissions)
                                              t :msgpack-false))
                             (overwrite . ,(if ok-if-already-exists
                                               t :msgpack-false))))
          (when preserve-uid-gid
            (tramp-rpc--call
             v2 "file.chown"
             `((path . ,(tramp-rpc--path-to-bin
                         (file-name-unquote v2-localname)))
               (uid . ,(alist-get 'uid source-stat))
               (gid . ,(alist-get 'gid source-stat)))))))
        (tramp-flush-file-properties v1 v1-localname)
        (tramp-flush-file-properties v2 v2-localname)
        (tramp-flush-directory-properties v2 v2-localname)
        (tramp-rpc--invalidate-cache-for-path newname)))))

(defun tramp-rpc--copy-directory-fallback
    (dirname newname keep-date parents copy-contents)
  "Copy DIRNAME to NEWNAME with the generic TRAMP handler.
KEEP-DATE, PARENTS, and COPY-CONTENTS are passed through unchanged.  Invalidate
only the source path and destination subtree caches."
  (prog1
      (tramp-handle-copy-directory
       dirname newname keep-date parents copy-contents)
    (tramp-rpc--invalidate-cache-for-path dirname)
    (tramp-rpc--invalidate-cache-for-subtree newname)))

(cl-defun tramp-rpc-handle-copy-directory
    (dirname newname &optional keep-date parents copy-contents)
  "Like `copy-directory' for TRAMP-RPC files.

For same-remote directory copies, use the server-side recursive `file.copy'
operation.  That keeps the number of RPC round-trips constant instead of
walking the tree from Emacs and issuing one RPC per entry.  Lisp computes the
Emacs/TRAMP destination and policy details; the server only receives primitive
copy options.  Fall back to the generic TRAMP handler for cross-remote copies.
DIRNAME is the directory name being handled.
NEWNAME is the destination file name.
KEEP-DATE non-nil preserves timestamps.
PARENTS non-nil creates missing parent directories.
COPY-CONTENTS non-nil copies directory contents."
  (setq dirname (expand-file-name dirname)
        newname (expand-file-name newname))
  (if (and (not keep-date)
           (not (and parents copy-contents))
           (tramp-tramp-file-p dirname)
           (tramp-tramp-file-p newname)
           (tramp-equal-remote dirname newname))
      (with-parsed-tramp-file-name dirname v1
        (with-parsed-tramp-file-name newname v2
          (let* ((src-localname (file-name-unquote v1-localname))
                 (dest-localname (file-name-unquote v2-localname))
                 ;; Match `copy-directory': a directory-name NEWNAME means
                 ;; copy into NEWNAME under DIRECTORY's basename, unless
                 ;; COPY-CONTENTS requests copying DIRECTORY's entries directly
                 ;; into NEWNAME.  Otherwise NEWNAME is the exact destination
                 ;; directory.
                 (actual-dest-localname
                  (if (and (directory-name-p newname)
                           (not copy-contents))
                      (expand-file-name
                       (file-name-nondirectory (directory-file-name src-localname))
                       dest-localname)
                    dest-localname))
                 ;; The source-symlink special case in `copy-directory' ignores
                 ;; COPY-CONTENTS: a directory-name NEWNAME still receives a
                 ;; symlink named after DIRECTORY.
                 (symlink-dest-localname
                  (if (directory-name-p newname)
                      (expand-file-name
                       (file-name-nondirectory (directory-file-name src-localname))
                       dest-localname)
                    dest-localname))
                 (source-symlink-target
                  (and copy-directory-create-symlink (file-symlink-p dirname)))
                 (actual-dest
                  (tramp-make-tramp-file-name
                   v2 (file-name-quote actual-dest-localname)))
                 (parent-localname
                  (file-name-directory (directory-file-name actual-dest-localname)))
                 (parent
                  (tramp-make-tramp-file-name v2 (file-name-quote parent-localname)))
                 (stats (tramp-rpc--call-batch
                         v1
                         `(("file.stat" . ,(append (tramp-rpc--encode-path src-localname)
                                                   '((lstat . t))))
                           ("file.stat" . ,(tramp-rpc--encode-path src-localname))
                           ("file.stat" . ,(append (tramp-rpc--encode-path actual-dest-localname)
                                                   '((lstat . t))))
                           ("file.stat" . ,(tramp-rpc--encode-path actual-dest-localname))
                           ("file.stat" . ,(tramp-rpc--encode-path parent-localname)))))
                 (source-lstat (tramp-rpc--batch-result-or-signal
                                "file.stat" dirname (nth 0 stats)))
                 ;; Keep following-stat results raw until after the
                 ;; `copy-directory-create-symlink' branch.  Emacs copies a
                 ;; source symlink as a symlink without following it, so a
                 ;; dangling or looping symlink target must not make the fast
                 ;; path fail before `make-symbolic-link' runs.
                 (source-stat-result (nth 1 stats))
                 (actual-dest-lstat-result (nth 2 stats))
                 (actual-dest-stat-result (nth 3 stats))
                 (parent-stat-result (nth 4 stats))
                 (source-lstat-type (tramp-rpc--stat-type source-lstat)))
            (if (or source-symlink-target
                    (and copy-directory-create-symlink
                         (equal source-lstat-type "symlink")))
                (tramp-rpc--call
                 v2 "file.make_symlink"
                 `((target . ,(tramp-rpc--path-to-bin
                               (or source-symlink-target
                                   (tramp-rpc--decode-string
                                    (alist-get 'link_target source-lstat)))))
                   (link_path . ,(tramp-rpc--path-to-bin symlink-dest-localname))))
              (let* ((source-stat (tramp-rpc--batch-result-or-signal
                                   "file.stat" dirname source-stat-result))
                     (actual-dest-lstat
                      (tramp-rpc--batch-result-or-signal
                       "file.stat" actual-dest actual-dest-lstat-result))
                     (actual-dest-stat
                      (tramp-rpc--batch-result-or-signal
                       "file.stat" actual-dest actual-dest-stat-result))
                     (parent-stat (tramp-rpc--batch-result-or-signal
                                   "file.stat" parent parent-stat-result))
                     (source-type (tramp-rpc--stat-type source-stat))
                     (actual-dest-type (tramp-rpc--stat-type actual-dest-stat))
                     (parent-type (tramp-rpc--stat-type parent-stat)))
                (unless source-stat
                  (signal 'file-missing (list "Opening input file" "No such file" dirname)))
                (unless (equal source-type "directory")
                  (signal 'file-error (list "Not a directory" dirname)))
                ;; `copy-directory' allows an already existing destination
                ;; directory when NEWNAME is a directory name, and also when
                ;; PARENTS is non-nil (because `make-directory' with PARENTS
                ;; accepts existing directories).  In all other cases, existing
                ;; destination entries are errors.  Directory checks follow
                ;; symlinks, so a symlink to a directory is accepted like TRAMP's
                ;; generic handler accepts it.
                (when (and actual-dest-lstat
                           (not (equal actual-dest-type "directory")))
                  (signal 'file-already-exists
                          (list actual-dest)))
                (when (and actual-dest-stat
                           (not (directory-name-p newname))
                           (not parents))
                  (signal 'file-already-exists (list actual-dest)))
                (when (and (not actual-dest-stat)
                           (not parents))
                  (unless parent-stat
                    (signal 'file-missing
                            (list "Creating directory" "No such file or directory"
                                  actual-dest)))
                  (unless (equal parent-type "directory")
                    (signal 'file-error (list "Not a directory" parent))))
                (when (zerop (logand (or (alist-get 'mode source-stat) 0) #o200))
                  (cl-return-from tramp-rpc-handle-copy-directory
                    (tramp-rpc--copy-directory-fallback
                     dirname newname keep-date parents copy-contents)))
                (tramp-rpc--call v1 "file.copy"
                                 `((src . ,(tramp-rpc--path-to-bytes src-localname))
                                   (dest . ,(tramp-rpc--path-to-bytes actual-dest-localname))
                                   (preserve . ,(if keep-date t :msgpack-false))
                                   (preserve_permissions . t)
                                   (preserve_times . ,(if keep-date t :msgpack-false))
                                   (overwrite . t)
                                   (exact_dest . t)
                                   (merge_existing_directories . ,(if parents
                                                                      t :msgpack-false))))))
            (tramp-flush-file-properties v1 v1-localname)
            (let* ((copied-localname
                    (if (and copy-directory-create-symlink
                             (equal source-lstat-type "symlink"))
                        symlink-dest-localname
                      actual-dest-localname))
                   (copied-parent-localname
                    (file-name-directory (directory-file-name copied-localname))))
              (tramp-flush-file-properties v2 copied-localname)
              ;; Flush both the copied directory and its parent.  For
              ;; COPY-CONTENTS, COPIED-LOCALNAME is the destination whose
              ;; listing has changed; for whole-directory copies the parent
              ;; listing has changed, and flushing the copied directory is
              ;; harmless and clears any stale attributes created during
              ;; preflight.
              (tramp-flush-directory-properties v2 copied-localname)
              (tramp-flush-directory-properties v2 copied-parent-localname)
              (tramp-flush-connection-properties v2)
              (tramp-rpc--invalidate-cache-for-path dirname)
              ;; Preserve NEWNAME's quoted or unquoted spelling so custom cache
              ;; keys and TRAMP properties for that spelling are both cleared.
              (tramp-rpc--invalidate-cache-for-subtree newname)))))
    (tramp-rpc--copy-directory-fallback
     dirname newname keep-date parents copy-contents)))

(cl-defun tramp-rpc-handle-copy-file
    (filename newname &optional ok-if-already-exists keep-time
              preserve-uid-gid preserve-permissions)
  "Like `copy-file' for TRAMP-RPC files.
FILENAME is the file name being handled.
NEWNAME is the destination file name.
OK-IF-ALREADY-EXISTS controls existing-destination handling.
KEEP-TIME non-nil preserves timestamps.
PRESERVE-UID-GID requests ownership preservation on generic fallback paths.
PRESERVE-PERMISSIONS non-nil preserves file permissions."
  (setq filename (expand-file-name filename)
        newname (expand-file-name newname))
  ;; Fast path for same-remote copies: batch source/destination stats, then do
  ;; the server-side copy.  This avoids the generic preflight predicates each
  ;; costing their own network round-trip.  Keep ownership preservation on the
  ;; RPC connection: `tramp-sh-handle-copy-file' cannot drive an rpc method's
  ;; non-shell transport and can wait forever when a file watch is active.
  (when (and (tramp-tramp-file-p filename)
             (tramp-tramp-file-p newname)
             (tramp-equal-remote filename newname))
    (cl-return-from tramp-rpc-handle-copy-file
      (tramp-rpc--copy-file-same-remote
       filename newname ok-if-already-exists keep-time preserve-uid-gid
       preserve-permissions)))
  ;; For copies crossing the RPC boundary, retain TRAMP's ownership-preserving
  ;; fallback rather than silently ignoring PRESERVE-UID-GID.
  (when (and preserve-uid-gid
             (or (tramp-tramp-file-p filename)
                 (tramp-tramp-file-p newname)))
    (cl-return-from tramp-rpc-handle-copy-file
      (tramp-sh-handle-copy-file
       filename newname ok-if-already-exists keep-time
       preserve-uid-gid preserve-permissions)))
  ;; When NEWNAME is a directory name (trailing /), copy INTO it.
  (when (and (directory-name-p newname)
             (file-directory-p newname))
    (setq newname (expand-file-name
                   (file-name-nondirectory filename) newname)))
  ;; Common checks before dispatching by host combination.
  (unless ok-if-already-exists
    (when (file-exists-p newname)
      (signal 'file-already-exists (list newname))))
  (when (and (file-directory-p newname)
             (not (directory-name-p newname)))
    (signal 'file-error (list "File is a directory" newname)))
  (let ((source-remote (tramp-tramp-file-p filename))
        (dest-remote (tramp-tramp-file-p newname)))
    (cond
     ;; Directory source: delegate to copy-directory.
     ((file-directory-p filename)
      (copy-directory filename newname keep-time t))

     ;; Symlink source: recreate the symlink at the destination rather
     ;; than copying the target file contents (matches upstream tramp).
     ((file-symlink-p filename)
      (make-symbolic-link
       (file-symlink-p filename) newname ok-if-already-exists))

     ;; Remote source, local dest - read via RPC, write locally
     ((and source-remote (not dest-remote))
      ;; Use file-local-copy to get a temp local copy, then rename
      (let ((tmpfile (file-local-copy filename)))
        (unwind-protect
            (progn
              (rename-file tmpfile newname ok-if-already-exists)
              (when keep-time
                (set-file-times newname (file-attribute-modification-time
                                         (file-attributes filename))))
              (when preserve-permissions
                (set-file-extended-attributes newname (file-extended-attributes
						       filename))))
          (when (file-exists-p tmpfile)
            (delete-file tmpfile)))))
     ;; Local source, remote dest - read locally, write via RPC
     ((and (not source-remote) dest-remote)
      ;; Read local file and write to remote
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally filename)
        (write-region (point-min) (point-max) newname nil 'nomessage))
      (when keep-time
        (set-file-times newname (file-attribute-modification-time
                                 (file-attributes filename))))
      (when preserve-permissions
        (set-file-extended-attributes newname (file-extended-attributes
					       filename))))
     ;; Both remote, different hosts - copy via local Emacs buffer.
     ;; This is the universal fallback matching upstream tramp's
     ;; `tramp-do-copy-or-rename-file-via-buffer': read source via its
     ;; handler, write destination via its handler.
     ((and source-remote dest-remote)
      (abort-if-file-too-large
       (file-attribute-size (file-attributes (file-truename filename)))
       "copy" filename)
      (let ((coding-system-for-read 'binary)
            (coding-system-for-write 'binary)
            (jka-compr-inhibit t))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally filename)
          (write-region (point-min) (point-max) newname nil 'nomessage)))
      (when keep-time
        (set-file-times newname (file-attribute-modification-time
                                 (file-attributes filename))))
      (when preserve-permissions
        (set-file-extended-attributes newname (file-extended-attributes
					       filename))))
     ;; Neither remote - should not reach this handler, but be safe.
     (t
      (tramp-run-real-handler
       #'copy-file
       (list filename newname ok-if-already-exists keep-time
             preserve-uid-gid preserve-permissions))))
    ;; Flush tramp file property cache for source and destination
    (when source-remote
      (with-parsed-tramp-file-name filename v1
        (tramp-flush-file-properties v1 v1-localname)))
    (when dest-remote
      (with-parsed-tramp-file-name newname v2
        (tramp-flush-file-properties v2 v2-localname)
        (tramp-flush-directory-properties v2 v2-localname))
      (tramp-rpc--invalidate-cache-for-path newname))))

(cl-defun tramp-rpc--rename-file-same-remote
    (filename newname ok-if-already-exists)
  "Rename FILENAME to NEWNAME on one TRAMP-RPC remote with fewer round-trips.
OK-IF-ALREADY-EXISTS controls existing-destination handling."
  (with-parsed-tramp-file-name filename v1
    (with-parsed-tramp-file-name newname v2
      (let* ((stats (tramp-rpc--call-batch
                     v1
                     `(("file.stat" . ,(append (tramp-rpc--encode-path v1-localname)
                                                '((lstat . t))))
                       ("file.stat" . ,(append (tramp-rpc--encode-path v2-localname)
                                                '((lstat . t)))))))
             (source-stat (tramp-rpc--batch-result-or-signal
                           "file.stat" filename (nth 0 stats)))
             (dest-stat (tramp-rpc--batch-result-or-signal
                         "file.stat" newname (nth 1 stats)))
             (source-type (tramp-rpc--stat-type source-stat))
             (dest-type (tramp-rpc--stat-type dest-stat)))
        (when dest-stat
          (unless ok-if-already-exists
            (signal 'file-already-exists (list newname)))
          (when (and (equal dest-type "directory")
                     (not (directory-name-p newname))
                     (not (equal source-type "directory")))
            (signal 'file-error (list "File is a directory" newname))))
        (when (and (equal dest-type "directory")
                   (directory-name-p newname))
          (cl-return-from tramp-rpc--rename-file-same-remote
            (tramp-rpc--rename-file-same-remote
             filename
             (expand-file-name (file-name-nondirectory filename) newname)
             ok-if-already-exists)))
        (tramp-rpc--call v1 "file.rename"
                         `((src . ,(tramp-rpc--path-to-bin
                                    (file-name-unquote v1-localname)))
                           (dest . ,(tramp-rpc--path-to-bin
                                     (file-name-unquote v2-localname)))
                           (overwrite . ,(if ok-if-already-exists
                                             t :msgpack-false))))
        (tramp-flush-file-properties v1 v1-localname)
        (if (equal source-type "directory")
            (tramp-rpc--invalidate-cache-for-subtree filename)
          (tramp-rpc--invalidate-cache-for-path filename))
        (tramp-flush-file-properties v2 v2-localname)
        (tramp-flush-directory-properties v2 v2-localname)
        (if (equal source-type "directory")
            (tramp-rpc--invalidate-cache-for-subtree newname)
          (tramp-rpc--invalidate-cache-for-path newname))))))

(cl-defun tramp-rpc-handle-rename-file (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for TRAMP-RPC files.
FILENAME is the file name being handled.
NEWNAME is the destination file name.
OK-IF-ALREADY-EXISTS controls existing-destination handling."
  (setq filename (expand-file-name filename)
        newname (expand-file-name newname))
  ;; Fast path for same-remote renames: one batched preflight plus the rename.
  (when (and (tramp-tramp-file-p filename)
             (tramp-tramp-file-p newname)
             (tramp-equal-remote filename newname))
    (cl-return-from tramp-rpc-handle-rename-file
      (tramp-rpc--rename-file-same-remote
       filename newname ok-if-already-exists)))
  (let ((source-directory-p (file-directory-p filename)))
    ;; Check ok-if-already-exists BEFORE any directory rewriting.
    (when (file-exists-p newname)
      (unless ok-if-already-exists
        (signal 'file-already-exists (list newname)))
      ;; Even with ok-if-already-exists, can't rename a file onto a directory.
      (when (and (file-directory-p newname)
                 (not (directory-name-p newname))
                 (not (file-directory-p filename)))
        (signal 'file-error (list "File is a directory" newname))))
    ;; If newname is a directory (with trailing slash), rename INTO it.
    (when (and (file-directory-p newname)
               (directory-name-p newname))
      (setq newname (expand-file-name (file-name-nondirectory filename) newname)))
    (let ((source-remote (tramp-tramp-file-p filename))
          (dest-remote (tramp-tramp-file-p newname)))
      (cond
       ;; Both on same remote host using RPC.
       ((and source-remote dest-remote
             (tramp-equal-remote filename newname))
        (with-parsed-tramp-file-name filename v1
          (with-parsed-tramp-file-name newname v2
            (tramp-rpc--call v1 "file.rename"
                             `((src . ,(tramp-rpc--path-to-bin
                                        (file-name-unquote v1-localname)))
                               (dest . ,(tramp-rpc--path-to-bin
                                         (file-name-unquote v2-localname)))
                               (overwrite . ,(if ok-if-already-exists
                                                 t :msgpack-false)))))))
       ;; Different hosts, copy then delete.
       (t
        (copy-file filename newname ok-if-already-exists t t t)
        (if (file-directory-p filename)
            (delete-directory filename 'recursive)
          (delete-file filename))))
      ;; Flush tramp file property cache for source and destination.
      (when source-remote
        (with-parsed-tramp-file-name filename v1
          (tramp-flush-file-properties v1 v1-localname))
        (if source-directory-p
            (tramp-rpc--invalidate-cache-for-subtree filename)
          (tramp-rpc--invalidate-cache-for-path filename)))
      (when dest-remote
        (with-parsed-tramp-file-name newname v2
          (tramp-flush-file-properties v2 v2-localname)
          (tramp-flush-directory-properties v2 v2-localname))
        (if source-directory-p
            (tramp-rpc--invalidate-cache-for-subtree newname)
          (tramp-rpc--invalidate-cache-for-path newname))))))


(defun tramp-rpc-handle-delete-file (filename &optional trash)
  "Like `delete-file' for TRAMP-RPC files.
Calls `file.delete' directly.  Current Emacs `delete-file' treats missing files
as a no-op, so ignore the server's ENOENT mapping as well.
FILENAME is the file name being handled.
TRASH non-nil requests moving the file to the trash."
  (tramp-skeleton-delete-file filename trash
    (condition-case err
        (tramp-rpc--call v "file.delete" (tramp-rpc--encode-path localname))
      (file-missing
       (tramp-rpc--debug "delete-file ignored missing %s: %s"
                         filename (error-message-string err)))))
  (tramp-rpc--invalidate-cache-for-path filename))

(defconst tramp-rpc--trash-read-batch-size 16
  "Maximum regular files to read in one optimized trash batch.")

(defun tramp-rpc--apply-local-trash-attributes (filename stat)
  "Apply mode and mtime from remote STAT to local FILENAME when possible."
  (when-let* ((mode (alist-get 'mode stat)))
    ;; The server sends full st_mode, including file type bits.  Local
    ;; `set-file-modes' wants only the permission/special bits.
    (set-file-modes filename (logand mode #o7777)))
  (when-let* ((mtime (alist-get 'mtime stat)))
    (set-file-times filename (seconds-to-time mtime))))

(defun tramp-rpc--write-local-trash-file (filename content stat)
  "Write CONTENT as binary data to local trash FILENAME and apply STAT."
  (let ((coding-system-for-write 'binary))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert content)
      (write-region (point-min) (point-max) filename nil 'quiet)))
  (tramp-rpc--apply-local-trash-attributes filename stat))

(defun tramp-rpc--copy-remote-trash-entry-to-local (vec localname dest stat)
  "Copy remote LOCALNAME on VEC to local trash DEST using STAT metadata."
  (pcase (tramp-rpc--stat-type stat)
    ("file"
     (tramp-rpc--write-local-trash-file
      dest (tramp-rpc--read-file-bytes vec localname nil nil t) stat))
    ("symlink"
     (make-symbolic-link
      (or (tramp-rpc--decode-string (alist-get 'link_target stat)) "") dest)
     ;; Do not call `tramp-rpc--apply-local-trash-attributes' for symlinks:
     ;; Emacs' local setters follow links on most systems.
     )
    ("directory"
     (tramp-rpc--copy-remote-trash-directory-to-local vec localname dest stat))
    (_
     ;; FIFOs/devices/sockets are intentionally not optimized yet.  Falling
     ;; back before deleting the source is safer than creating the wrong local
     ;; object in the trash.
     (throw 'tramp-rpc-trash-unsupported localname))))

(defun tramp-rpc--copy-remote-trash-directory-to-local (vec localname dest stat)
  "Recursively copy remote directory LOCALNAME on VEC to local trash DEST."
  (make-directory dest)
  (let* ((entries (tramp-rpc--call
                   vec "dir.list"
                   (append (tramp-rpc--encode-path localname)
                           '((include_attrs . t)
                             (include_hidden . t)))))
         regulars directories)
    (dolist (entry entries)
      (let* ((name (tramp-rpc--decode-filename entry))
             (entry-stat (alist-get 'attrs entry))
             (type (or (tramp-rpc--stat-type entry-stat)
                       (alist-get 'type entry))))
        (unless (member name '("." ".."))
          (unless entry-stat
            (setq entry-stat
                  (tramp-rpc--call-file-stat
                   vec (file-name-concat localname name) t)
                  type (or type (tramp-rpc--stat-type entry-stat))))
          (pcase type
            ("file"
             (push (list name entry-stat) regulars))
            ("symlink"
             (make-symbolic-link
              (or (tramp-rpc--decode-string (alist-get 'link_target entry-stat)) "")
              (expand-file-name name (file-name-as-directory dest))))
            ("directory"
             (push (list name entry-stat) directories))
            (_
             (throw 'tramp-rpc-trash-unsupported
                    (file-name-concat localname name)))))))
    ;; Batch regular file contents per directory.  Directories are then handled
    ;; recursively so the implementation stays small while avoiding the generic
    ;; TRAMP copy-file/stat/truename path for the common tiny test trees.
    (when regulars
      (let (large-files small-files)
        (dolist (item (nreverse regulars))
          (if (> (or (alist-get 'size (cadr item)) 0)
                 (tramp-rpc--read-chunk-size vec))
              (push item large-files)
            (push item small-files)))
        ;; Preserve the low-latency batch path for files that fit in one RPC.
        (let ((remaining (nreverse small-files)))
          (while remaining
            (let ((chunk nil)
                  (count 0))
              (while (and remaining (< count tramp-rpc--trash-read-batch-size))
                (push (pop remaining) chunk)
                (setq count (1+ count)))
              (setq chunk (nreverse chunk))
              (let ((reads (tramp-rpc--call-batch
                            vec
                            (mapcar
                             (lambda (item)
                               (cons "file.read"
                                     (tramp-rpc--file-read-params
                                      (file-name-concat localname (car item)) t)))
                             chunk))))
                (cl-mapc
                 (lambda (item result)
                   (let* ((path (file-name-concat localname (car item)))
                          (content
                           (if (and (tramp-rpc--batch-error-p result)
                                    (= (plist-get result :error)
                                       tramp-rpc--invalid-params-error-code))
                               ;; The file may have grown beyond the advertised
                               ;; limit since dir.list supplied its size.
                               (tramp-rpc--read-file-bytes vec path nil nil t)
                             (when (tramp-rpc--batch-error-p result)
                               (tramp-rpc--signal-batch-failure
                                "file.read" path result))
                             (tramp-rpc--extract-file-read-content result))))
                     (tramp-rpc--write-local-trash-file
                      (expand-file-name (car item) (file-name-as-directory dest))
                      content
                      (cadr item))))
                 chunk reads)))))
        ;; Large files are read in bounded chunks instead of failing the
        ;; optimized trash path at the server's per-request limit.
        (dolist (item (nreverse large-files))
          (let ((name (car item)))
            (tramp-rpc--write-local-trash-file
             (expand-file-name name (file-name-as-directory dest))
             (tramp-rpc--read-file-bytes
              vec (file-name-concat localname name) nil nil t)
             (cadr item))))))
    (dolist (item (nreverse directories))
      (tramp-rpc--copy-remote-trash-directory-to-local
       vec
       (file-name-concat localname (car item))
       (expand-file-name (car item) (file-name-as-directory dest))
       (cadr item))))
  ;; Apply directory attributes after creating children, because recursive file
  ;; creation updates the directory mtime.
  (tramp-rpc--apply-local-trash-attributes dest stat))

(defun tramp-rpc--local-trash-destination (filename)
  "Return local `trash-directory' destination for FILENAME, or nil.
This mirrors the `trash-directory' branch of `move-file-to-trash'.  It only
returns a destination when that branch resolves to a local directory."
  (unless (fboundp 'system-move-file-to-trash)
    (when-let* ((trash-directory (connection-local-value trash-directory))
                (trash-dir (expand-file-name trash-directory))
                ((not (file-remote-p trash-dir))))
      (let* ((fn (directory-file-name (expand-file-name filename)))
             (new-fn (concat (file-name-as-directory trash-dir)
                             (file-name-nondirectory fn))))
        ;; Match `move-file-to-trash' for this branch.
        (when (string-prefix-p fn trash-dir)
          (error "Trash directory `%s' is a subdirectory of `%s'"
                 trash-dir filename))
        (unless (file-directory-p trash-dir)
          (make-directory trash-dir t))
        (when (file-attributes new-fn)
          (let ((version-control t)
                (backup-directory-alist nil))
            (setq new-fn (car (find-backup-file-name new-fn)))))
        new-fn))))

(defvar tramp-rpc--move-file-to-trash-function
  (symbol-function 'move-file-to-trash)
  "Original `move-file-to-trash' function, captured before TRAMP advice.")

(defun tramp-rpc--fallback-move-file-to-trash (filename)
  "Run the original `move-file-to-trash' implementation for FILENAME.
Bypass only TRAMP's external-operation advice.  File operations performed by
the original implementation must still dispatch through the TRAMP-RPC file
name handler."
  (funcall tramp-rpc--move-file-to-trash-function filename))

(defun tramp-rpc--delete-local-trash-copy (filename)
  "Best-effort removal of a partial local trash copy at FILENAME."
  (condition-case nil
      (cond
       ((file-symlink-p filename) (delete-file filename))
       ((file-directory-p filename) (delete-directory filename t))
       ((file-exists-p filename) (delete-file filename)))
    (error nil)))

(defun tramp-rpc-handle-move-file-to-trash (filename)
  "Like `move-file-to-trash' for TRAMP-RPC files.
Optimize the common `trash-directory' case where a remote file is moved to a
local trash directory.  Unsupported trash modes fall back to Emacs' real
implementation, which will use the normal TRAMP-RPC file handlers underneath.
FILENAME is the file name being handled."
  (if-let* ((dest (tramp-rpc--local-trash-destination filename)))
      (with-parsed-tramp-file-name (directory-file-name (expand-file-name filename)) nil
        (let ((stat (tramp-rpc--call-file-stat v localname t)))
          (unless stat
            (signal 'file-missing (list "Opening input file" "No such file" filename)))
          (if (eq (condition-case err
                      (catch 'tramp-rpc-trash-unsupported
                        (tramp-rpc--copy-remote-trash-entry-to-local v localname dest stat)
                        'copied)
                    (error
                     (tramp-rpc--delete-local-trash-copy dest)
                     (signal (car err) (cdr err))))
                  'copied)
              (progn
                (condition-case err
                    (if (equal (tramp-rpc--stat-type stat) "directory")
                        (tramp-rpc--call v "dir.remove"
                                         (append (tramp-rpc--encode-path localname)
                                                 '((recursive . t))))
                      (tramp-rpc--call v "file.delete" (tramp-rpc--encode-path localname)))
                  (error
                   (tramp-rpc--delete-local-trash-copy dest)
                   (signal (car err) (cdr err))))
                (tramp-flush-file-properties v localname)
                (tramp-flush-directory-properties v (file-name-directory localname))
                (tramp-rpc--invalidate-cache-for-path filename)
                nil)
            (tramp-rpc--delete-local-trash-copy dest)
            (tramp-rpc--fallback-move-file-to-trash filename))))
    (tramp-rpc--fallback-move-file-to-trash filename)))

(defun tramp-rpc-handle-make-symbolic-link
    (target linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for TRAMP-RPC files.
TARGET is the link target.
LINKNAME is the name of the link to create.
OK-IF-ALREADY-EXISTS non-nil permits an existing destination."
  (prog1
      (tramp-skeleton-make-symbolic-link target linkname ok-if-already-exists
        (let ((target-path (file-name-unquote target)))
          (tramp-rpc--call
           v "file.make_symlink"
           `((target . ,(tramp-rpc--path-to-bin target-path))
             (link_path . ,(tramp-rpc--path-to-bin localname))))))

    (tramp-rpc--invalidate-cache-for-path linkname)))

(defun tramp-rpc-handle-add-name-to-file (filename newname &optional ok-if-already-exists)
  "Like `add-name-to-file' for TRAMP-RPC files.
Creates a hard link from NEWNAME to FILENAME."
  ;; When newname is a directory-name (trailing /), create the link inside it.
  (when (and (directory-name-p newname)
             (file-directory-p newname))
    (setq newname (expand-file-name (file-name-nondirectory filename) newname)))
  (unless (tramp-equal-remote filename newname)
    (with-parsed-tramp-file-name
        (if (tramp-tramp-file-p filename) filename newname) nil
      (tramp-error
       v 'remote-file-error
       "add-name-to-file: %s"
       "only implemented for same method, same user, same host")))
  (with-parsed-tramp-file-name (expand-file-name filename) v1
    (with-parsed-tramp-file-name (expand-file-name newname) v2
      ;; Handle the 'confirm if exists' thing
      (when (file-exists-p newname)
        (if (or (null ok-if-already-exists)
                (and (numberp ok-if-already-exists)
                     (not (yes-or-no-p
                           (format "File %s already exists; make it a link anyway?"
                                   v2-localname)))))
            (tramp-error v2 'file-already-exists newname)
          (delete-file newname)))
      (tramp-flush-file-properties v1 v1-localname)
      (tramp-flush-file-properties v2 v2-localname)
      (tramp-rpc--call v1 "file.make_hardlink"
                       `((src . ,(tramp-rpc--path-to-bin
                                  (file-name-unquote v1-localname)))
                         (dest . ,(tramp-rpc--path-to-bin
                                  (file-name-unquote v2-localname)))))
      (tramp-rpc--invalidate-cache-for-path filename)
      (tramp-rpc--invalidate-cache-for-path newname))))


(defun tramp-rpc-handle-set-file-uid-gid (filename &optional uid gid)
  "Like `tramp-set-file-uid-gid' for TRAMP-RPC files.
Set the ownership of FILENAME to UID and GID.
Either UID or GID can be nil or -1 to leave that unchanged."
  (tramp-skeleton-set-file-modes-times-uid-gid filename
    (let ((uid (or (and (natnump uid) uid)
                   (tramp-rpc-handle-get-remote-uid v 'integer)))
          (gid (or (and (natnump gid) gid)
                   (tramp-rpc-handle-get-remote-gid v 'integer))))
      (tramp-rpc--call v "file.chown"
                       (append (tramp-rpc--encode-path localname)
                               `((uid . ,uid)
                                 (gid . ,gid)))))))

(defun tramp-rpc-handle-file-system-info (filename)
  "Like `file-system-info' for TRAMP-RPC files.
Returns a list of (TOTAL FREE AVAILABLE) bytes for the filesystem
containing FILENAME."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (condition-case nil
        (let ((result (tramp-rpc--call v "system.statvfs" (tramp-rpc--encode-path localname))))
          (list (alist-get 'total result)
                (alist-get 'free result)
                (alist-get 'available result)))
      (error nil))))

(defun tramp-rpc-handle-get-remote-groups (vec id-format)
  "Return remote groups using RPC.
ID-FORMAT specifies whether to return integer GIDs or string names.
VEC is the TRAMP connection vector."
  (condition-case nil
      (let ((result (tramp-rpc--call vec "system.groups" nil)))
        (mapcar (lambda (g)
                  (if (eq id-format 'integer)
                      (alist-get 'gid g)
                    (or (tramp-rpc--decode-string (alist-get 'name g))
                        (number-to-string (alist-get 'gid g)))))
                result))
    (error nil)))

;; ============================================================================
;; ACL Support
;; ============================================================================

(defun tramp-rpc--cached-capability-probe (vec property command args)
  "Return cached capability PROPERTY for VEC, probing COMMAND with ARGS.
PROPERTY must start with a space so TRAMP keeps it ephemeral.  Successful
probes cache both enabled and disabled results.  Transport and RPC errors
return nil without caching so a later operation can retry."
  (let* ((missing (make-symbol "missing"))
         (cached (tramp-rpc--get-route-connection-property
                  vec property missing)))
    (if (not (eq cached missing))
        cached
      (condition-case nil
          (let* ((result (tramp-rpc--call vec "process.run"
                                          `((cmd . ,command)
                                            (args . ,args)
                                            (cwd . "/"))))
                 (enabled (zerop (alist-get 'exit_code result))))
            (tramp-rpc--set-route-connection-property vec property enabled)
            enabled)
        (error nil)))))

(defun tramp-rpc--acl-enabled-p (vec)
  "Check if ACL is available on the remote host VEC.
Cache successful probe results for the connection lifetime."
  (tramp-rpc--cached-capability-probe
   vec " rpc-acl-enabled" "getfacl" ["--version"]))

(defun tramp-rpc-handle-file-acl (filename)
  "Like `file-acl' for TRAMP-RPC files.
Returns the ACL string for FILENAME, or nil if ACLs are not supported."
  (with-parsed-tramp-file-name (expand-file-name (file-name-unquote filename)) nil
    (when (tramp-rpc--acl-enabled-p v)
      (let ((result (tramp-rpc--call v "process.run"
                                     `((cmd . "getfacl")
                                       (args . ["-ac" ,localname])
                                       (cwd . "/")))))
        (when (zerop (alist-get 'exit_code result))
          (let ((output (tramp-rpc--decode-output
                         (alist-get 'stdout result))))
            ;; Return nil if output is empty or only whitespace
            (when (string-match-p "[^ \t\n]" output)
	      ;; By convention, the result string has a trailing
	      ;; newline.  Don't let tests fail.
	      (concat (string-trim output) "\n"))))))))

(defun tramp-rpc-handle-set-file-acl (filename acl-string)
  "Like `set-file-acl' for TRAMP-RPC files.
Set the ACL of FILENAME to ACL-STRING.
Returns t on success, nil on failure."
  (with-parsed-tramp-file-name (expand-file-name (file-name-unquote filename)) nil
    (when (and (stringp acl-string)
               (tramp-rpc--acl-enabled-p v))
      ;; Use setfacl with --set-file=- to read ACL from stdin
      ;; stdin must be binary for MessagePack
      (let* ((acl-bytes (encode-coding-string acl-string 'utf-8-unix))
             (result (tramp-rpc--call v "process.run"
                                      `((cmd . "setfacl")
                                        (args . ["--set-file=-" ,localname])
                                        (cwd . "/")
                                        (stdin . ,(msgpack-bin-make acl-bytes))))))
        (zerop (alist-get 'exit_code result))))))

;; ============================================================================
;; SELinux Support
;; ============================================================================

(defun tramp-rpc--selinux-enabled-p (vec)
  "Check if SELinux is enabled on the remote host VEC.
Cache successful probe results for the connection lifetime."
  (tramp-rpc--cached-capability-probe
   vec " rpc-selinux-enabled" "selinuxenabled" []))

(defun tramp-rpc-handle-file-selinux-context (filename)
  "Like `file-selinux-context' for TRAMP-RPC files.
Returns a list of (USER ROLE TYPE RANGE), or (nil nil nil nil) if not available.
FILENAME is the file name being handled."
  (with-parsed-tramp-file-name (expand-file-name (file-name-unquote filename)) nil
    (let ((context '(nil nil nil nil)))
      (when (tramp-rpc--selinux-enabled-p v)
        (let ((result (tramp-rpc--call v "process.run"
                                       `((cmd . "ls")
                                         (args . ["-d" "-Z" ,localname])
                                         (cwd . "/")))))
          (when (zerop (alist-get 'exit_code result))
            (let ((output (tramp-rpc--decode-output
                           (alist-get 'stdout result))))
              ;; Parse SELinux context from ls -Z output
              ;; Format: user:role:type:range filename
              (when (string-match
                     "\\([^:]+\\):\\([^:]+\\):\\([^:]+\\):\\([^ \t\n]+\\)"
                     output)
                (setq context (list (match-string 1 output)
                                    (match-string 2 output)
                                    (match-string 3 output)
                                    (match-string 4 output))))))))
      context)))

(defun tramp-rpc-handle-set-file-selinux-context (filename context)
  "Like `set-file-selinux-context' for TRAMP-RPC files.
Set the SELinux context of FILENAME to CONTEXT.
CONTEXT is a list of (USER ROLE TYPE RANGE).
Returns t on success, nil on failure."
  (with-parsed-tramp-file-name (expand-file-name (file-name-unquote filename)) nil
    (when (and (consp context)
               (tramp-rpc--selinux-enabled-p v))
      (let* ((user (and (stringp (nth 0 context)) (nth 0 context)))
             (role (and (stringp (nth 1 context)) (nth 1 context)))
             (type (and (stringp (nth 2 context)) (nth 2 context)))
             (range (and (stringp (nth 3 context)) (nth 3 context)))
             (args (append
                    (when user (list (format "--user=%s" user)))
                    (when role (list (format "--role=%s" role)))
                    (when type (list (format "--type=%s" type)))
                    (when range (list (format "--range=%s" range)))
                    (list localname)))
             (result (tramp-rpc--call v "process.run"
                                      `((cmd . "chcon")
                                        (args . ,(vconcat args))
                                        (cwd . "/")))))
        (zerop (alist-get 'exit_code result))))))

;; ============================================================================
;; Process operations
;; ============================================================================

(defun tramp-rpc--route-process-file-stream (destination output &optional file-p)
  "Route OUTPUT to DESTINATION.
When FILE-P is non-nil, a string DESTINATION names a file; otherwise it names
an Emacs buffer, matching `call-process' and `process-file'."
  (cond
   ((null destination) nil)
   ((eq destination t) (insert output))
   ((bufferp destination)
    (with-current-buffer destination (insert output)))
   ((and (stringp destination) file-p)
    (with-temp-file destination (insert output)))
   ((stringp destination)
    (with-current-buffer (get-buffer-create destination) (insert output)))))

(defun tramp-rpc--process-file-merge-output-p (destination)
  "Return non-nil when DESTINATION requires ordered combined output."
  (or (not (consp destination))
      (eq (car destination) :file)
      (eq (cadr destination) t)))

(defun tramp-rpc--route-process-file-output (destination stdout &optional stderr)
  "Route `process-file' STDOUT and STDERR according to DESTINATION.
DESTINATION follows the `process-file' convention: nil discards output; t,
a buffer, or a buffer name receives combined output; (:file FILE) writes
combined output to FILE; and (STDOUT-DEST STDERR-DEST) separates the streams.
A t STDERR-DEST mixes stderr into STDOUT-DEST."
  (let ((combined (concat stdout (or stderr ""))))
    (cond
     ((and (consp destination) (eq (car destination) :file))
      (tramp-rpc--route-process-file-stream (cadr destination) combined t))
     ((consp destination)
      (let ((stdout-destination (car destination))
            (stderr-destination (cadr destination)))
        (if (eq stderr-destination t)
            (tramp-rpc--route-process-file-stream stdout-destination combined)
          (tramp-rpc--route-process-file-stream stdout-destination stdout)
          (when stderr
            (tramp-rpc--route-process-file-stream stderr-destination stderr t)))))
     (t
      (tramp-rpc--route-process-file-stream destination combined)))))

(defun tramp-rpc--get-signal-strings (vec)
  "Strings to return by `process-file' in case of signals on VEC.
Runs `kill -l' on the remote host to get signal names, then maps
signal numbers to human-readable strings like \"Interrupt\" or
\"Signal 2\".  The result is cached per connection."
  (tramp-rpc--with-route-connection-property vec "rpc-signal-strings"
    (let* ((result (tramp-rpc--call vec "process.run"
                                    `((cmd . "/bin/sh")
                                      (args . ["-c" "kill -l"])
                                      (cwd . "/"))))
           (exit-code (alist-get 'exit_code result))
           (stdout (tramp-rpc--decode-output
                    (alist-get 'stdout result)))
           (raw-signals (when (and (eq exit-code 0) (> (length stdout) 0))
                          (split-string (string-trim stdout) nil 'omit)))
           ;; Prepend a placeholder 0 for signal 0 so that (nth 1 signals)
           ;; corresponds to signal 1 (HUP), (nth 2 signals) to signal 2 (INT), etc.
           (signals (cons 0 raw-signals))
           (vec-strings (make-vector 128 nil)))
      ;; Sanity: remove duplicate leading "0" entry if kill -l included one
      (when (and (stringp (cadr signals)) (string-equal (cadr signals) "0"))
        (setcdr signals (cddr signals)))
      ;; Map signal names to human-readable strings
      (dotimes (i 128)
        (let ((sig (nth i signals)))
          (aset vec-strings i
                (cond
                 ((zerop i) 0)
                 ((null sig) (format "Signal %d" i))
                 ((string-equal sig "HUP") "Hangup")
                 ((string-equal sig "INT") "Interrupt")
                 ((string-equal sig "QUIT") "Quit")
                 ((string-equal sig "STOP") "Stopped (signal)")
                 ((string-equal sig "TSTP") "Stopped")
                 ((string-equal sig "TTIN") "Stopped (tty input)")
                 ((string-equal sig "TTOU") "Stopped (tty output)")
                 (t (format "Signal %d" i))))))
      vec-strings)))

(defun tramp-rpc-handle-process-file
    (program &optional infile destination _display &rest args)
  "Like `process-file' for TRAMP-RPC files.
Resolves PROGRAM path and loads direnv environment from working directory.
When `tramp-rpc-magit--process-caches' is populated (during magit
refresh), git commands are served from the prefetch cache when possible.
INFILE is the input file name.
DESTINATION controls where standard output and error are sent, as for
`process-file'.
ARGS contains the original function arguments."
  (with-parsed-tramp-file-name default-directory nil
    ;; Unquote localname in case of file-name-quoted paths (e.g. /: prefix).
    (setq localname (file-name-unquote localname))
    ;; Try serving from magit prefetch cache first (no RPC needed)
    (let ((cached (when (null infile)  ; no stdin redirection
                    (tramp-rpc-magit--process-cache-lookup program args))))
      (if cached
          ;; Cache hit - serve from prefetch
          (let ((exit-code (car cached))
                (stdout (cdr cached)))
            (tramp-rpc--route-process-file-output destination stdout)
            exit-code)
        ;; Cache miss - make actual RPC call.  Leave relative PROGRAM names
        ;; unresolved so the server's process launcher searches the PATH we pass
        ;; below, matching `tramp-remote-path' order.
        (let* (;; Like TRAMP's process handlers, pass only the remote-relevant
               ;; environment.  The PATH entry comes from `tramp-remote-path'
               ;; (or deprecated `tramp-rpc-remote-path').  Shell commands also
               ;; retain login-shell entries, matching tramp-sh; direnv and
               ;; dynamic caller variables keep their previous roles and
               ;; override it.
               (env (tramp-rpc--process-environment
                     v localname (equal program shell-file-name)))
               (stdin-content (when (and infile (not (eq infile t)))
                                (with-temp-buffer
                                  (set-buffer-multibyte nil)
                                  (insert-file-contents-literally infile)
                                  (buffer-string)))))
          ;; Clear after every RPC exit when Emacs says the command may have side
          ;; effects.  This includes decoding, prefetch, and output-routing
          ;; failures, and prevents metadata cached while the command ran from
          ;; surviving it.
          (unwind-protect
              (let ((result
                     (condition-case err
                         (tramp-rpc--call
                          v "process.run"
                          `((cmd . ,program)
                            (args . ,(vconcat args))
                            (cwd . ,localname)
                            (env . ,env)
                            ,@(when (tramp-rpc--process-file-merge-output-p
                                     destination)
                                '((merge_stderr . t)))
                            ,@(when stdin-content
                                `((stdin . ,stdin-content)))))
                       ;; The server marks confirmed spawn ENOENT as
                       ;; `file-missing'.  Shell-based TRAMP returns status
                       ;; 127 and leaves the diagnostic on stderr, so preserve
                       ;; both parts of that contract for split destinations.
                       (file-missing
                        (tramp-rpc--route-process-file-output
                         destination "" (concat (error-message-string err) "\n"))
                        127))))
                (if (eq result 127)
                    127
                  (if result
                      (let ((exit-code (alist-get 'exit_code result))
                            (stdout (tramp-rpc--decode-output
                                     (alist-get 'stdout result)))
                            (stderr (tramp-rpc--decode-output
                                     (alist-get 'stderr result))))

                        ;; Memoize uncached Magit git calls made during lazy
                        ;; remote status expansion.
                        (when (null infile)
                          (tramp-rpc-magit--process-cache-store
                           program args exit-code stdout))

                        ;; Let the real `update-index --refresh' run, then
                        ;; build the read snapshot used by later Magit calls.
                        (when (and
                               (null infile)
                               (bound-and-true-p
                                tramp-rpc-magit--allow-process-cache)
                               (or (string-suffix-p "/git" program)
                                   (string= "git" program))
                               (= exit-code 0)
                               (tramp-rpc-magit--git-cache-safe-environment-p))
                          (let ((core-args
                                 (tramp-rpc-magit--strip-git-prefix-args args)))
                            (when (and
                                   (equal (car core-args) "update-index")
                                   (member "--refresh" core-args))
                              (tramp-rpc-magit--clear-status-cache-for-connection v)
                              (tramp-rpc-magit--prefetch default-directory))))

                        (tramp-rpc--route-process-file-output
                         destination stdout stderr)

                        ;; Handle signal strings when requested by Emacs.
                        (if (and
                             (bound-and-true-p
                              process-file-return-signal-string)
                             (natnump exit-code) (>= exit-code 128))
                            (let ((strings (tramp-rpc--get-signal-strings v)))
                              (aref strings (- exit-code 128)))
                          exit-code))
                    ;; A successful RPC result is always non-nil.
                    (signal 'remote-file-error
                            (list "Empty process.run response")))))
            ;; Any external command may mutate the filesystem, and watcher
            ;; delivery is asynchronous.  Honor Emacs' conservative default;
            ;; callers with a proven read-only scope can bind
            ;; `process-file-side-effects' to nil.
            (when process-file-side-effects
              (tramp-rpc--clear-file-caches-for-connection v))))))))

(defun tramp-rpc-handle-vc-registered (file)
  "Like `vc-registered' for TRAMP-RPC files.
Since tramp-rpc supports `process-file', VC backends can run their
commands (git, svn, hg) directly via RPC.

We set `default-directory' to the file's directory to ensure that
`process-file' calls from VC backends are routed through our tramp handler.
FILE is the file name being handled."
  (when vc-handled-backends
    (with-parsed-tramp-file-name file nil
      ;; Set default-directory to the file's remote directory so that
      ;; process-file calls from VC are handled by our tramp handler.
      (let ((default-directory (file-name-directory file))
            process-file-side-effects)
        (tramp-run-real-handler #'vc-registered (list file))))))

;; ============================================================================
;; Additional handlers to avoid shell dependency
;; ============================================================================

(defun tramp-rpc-handle-exec-path ()
  "Return remote variable `exec-path' using RPC.
Uses `tramp-remote-path' by default, including its standard placeholders
`tramp-default-remote-path' and `tramp-own-remote-path'.  A non-nil
`tramp-rpc-remote-path' overrides it for backward compatibility.
Appends the remote working directory as the last element (the equivalent
of `exec-directory'), matching `tramp-sh-handle-exec-path' behavior.
Caches the PATH portion per connection."
  (with-parsed-tramp-file-name default-directory nil
    ;; Append localname of default-directory as last element,
    ;; the equivalent to `exec-directory'.
    (append (tramp-rpc--cached-remote-path v)
            (list (tramp-file-local-name
                   (expand-file-name default-directory))))))

(defun tramp-rpc-handle-insert-file-contents
    (filename &optional visit beg end replace)
  "Like `insert-file-contents' for TRAMP-RPC files.
Reads directly through `file.read' instead of going through
`file-local-copy', avoiding the generic TRAMP temp-file path and its extra
round-trips for the common non-VISIT case.
FILENAME is the file name being handled.
VISIT controls whether Emacs visits the destination.
BEG and END are source-file byte offsets delimiting the data to insert.
REPLACE non-nil replaces the accessible buffer contents."
  (barf-if-buffer-read-only)
  (setq filename (expand-file-name filename))
  (if (or visit replace)
      ;; Visiting a file and REPLACE have extra buffer-state and return-value
      ;; semantics.  Keep the battle-tested generic TRAMP path there; the
      ;; latency-sensitive optimization is for ordinary reads (the common
      ;; programmatic case).
      (tramp-handle-insert-file-contents filename visit beg end replace)
    (let ((start (point))
          result)
      (with-parsed-tramp-file-name filename nil
        (let* ((content (tramp-rpc--read-file-bytes
                         v localname beg end))
               (decoded-content
                (if enable-multibyte-characters
                    (decode-coding-string
                     content (or coding-system-for-read 'undecided))
                  content)))
          (insert decoded-content)
          (setq result (list filename (- (point) start)))
          (goto-char start)))
      result)))

(defun tramp-rpc-handle-file-local-copy (filename)
  "Create a local copy of remote FILENAME using RPC."
  (tramp-skeleton-file-local-copy filename
    (let ((content (tramp-rpc--read-file-bytes v localname)))
      (with-temp-file tmpfile
        (set-buffer-multibyte nil)
        (insert content)))))

(defun tramp-rpc-handle-get-home-directory (vec &optional user)
  "Return home directory for USER on remote host VEC using RPC.
If USER is nil or matches the connection user, returns the current user's
home directory from system.info.  For other users, looks up via getent.
Signals an error rather than returning nil, so that
`tramp-get-home-directory' does not cache a nil result."
  (let* ((conn-user (tramp-file-name-user vec))
         (target-user (or user conn-user)))
    (if (or (null target-user)
            (string-empty-p target-user)
            (equal target-user conn-user))
        ;; Current user - use system.info (errors propagate, not cached)
        (or (tramp-rpc--decode-string
             (alist-get 'home (tramp-rpc--system-info vec)))
            (tramp-error vec 'file-error
                         "Remote home directory not available"))
      ;; Different user - look up via getent passwd
      (let* ((result (tramp-rpc--call vec "process.run"
                                       `((cmd . "getent")
                                         (args . ["passwd" ,target-user])
                                         (cwd . "/"))))
             (exit-code (alist-get 'exit_code result))
             (stdout (tramp-rpc--decode-output
                      (alist-get 'stdout result))))
        (when (and (eq exit-code 0) (> (length stdout) 0))
          ;; getent passwd format: name:x:uid:gid:gecos:home:shell
          (let ((fields (split-string (string-trim stdout) ":")))
            (when (>= (length fields) 6)
              (nth 5 fields))))))))

(defun tramp-rpc-handle-get-remote-uid (vec id-format)
  "Return remote UID using RPC.
VEC is the TRAMP connection vector.
ID-FORMAT controls whether the UID is returned as an integer or string."
  (let* ((result (tramp-rpc--system-info vec))
         (uid (alist-get 'uid result)))
    (if (eq id-format 'integer)
        uid
      (number-to-string uid))))

(defun tramp-rpc-handle-get-remote-gid (vec id-format)
  "Return remote GID using RPC.
VEC is the TRAMP connection vector.
ID-FORMAT controls whether the GID is returned as an integer or string."
  (let* ((result (tramp-rpc--system-info vec))
         (gid (alist-get 'gid result)))
    (if (eq id-format 'integer)
        gid
      (number-to-string gid))))

(defun tramp-rpc-handle-file-ownership-preserved-p (filename &optional group)
  "Like `file-ownership-preserved-p' for TRAMP-RPC files.
Check if file ownership would be preserved when creating FILENAME.
If GROUP is non-nil, also check that group would be preserved.
Uses cached `file-attributes' and connection-cached remote uid/gid,
so this typically requires no RPC calls."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property
        v localname
        (format "file-ownership-preserved-p%s" (if group "-group" ""))
      (let ((attributes (file-attributes filename 'integer)))
        ;; Return t if the file doesn't exist, since it's true that no
        ;; information would be lost by an (attempted) delete and create.
        (or (null attributes)
            (and
             (= (file-attribute-user-id attributes)
                (tramp-get-remote-uid v 'integer))
             (or (not group)
                 ;; On BSD-derived systems files always inherit the
                 ;; parent directory's group, so skip the group-gid test.
                 (tramp-check-remote-uname v tramp-bsd-unames)
                 (= (file-attribute-group-id attributes)
                    (tramp-get-remote-gid v 'integer)))))))))

(defun tramp-rpc-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for TRAMP-RPC files.
Delegates to `tramp-handle-expand-file-name'.  If tilde expansion
fails because the connection is not available (e.g. during
`tramp-cleanup-all-connections'), retries with `tramp-tolerate-tilde'
so the path is returned with the tilde unexpanded rather than
signalling an error.
`tramp-verbose' is suppressed during the first attempt because
`tramp-error' logs a level-1 message before signalling, which
would otherwise flood the echo area with \"Cannot expand tilde\".
NAME identifies the connection.
DIR is the directory being handled."
  ;; The generic `tramp-handle-expand-file-name' defaults non-absolute
  ;; localnames to "/" (root), but the ssh handler
  ;; (`tramp-sh-handle-expand-file-name') defaults to "~/" instead.
  ;; Match that behavior: empty localnames get "~", and non-absolute
  ;; localnames (e.g. ".config/") get "~/" prepended so they resolve
  ;; relative to the home directory rather than the filesystem root.
  ;; Guard with `tramp-connectable-p' so that the tilde substitution is
  ;; skipped during completion when no connection exists, avoiding a
  ;; blocking connection attempt when `non-essential' is t.  When not
  ;; connectable the generic handler falls through to "/" (root) rather
  ;; than the home directory — acceptable for the completion case.
  ;; Use `tramp-dissect-file-name' and `tramp-make-tramp-file-name'
  ;; instead of `file-remote-p' to avoid re-entering expand-file-name.
  (when (tramp-tramp-file-p name)
    (let ((v (tramp-dissect-file-name name)))
      (when (tramp-connectable-p v)
        (let ((localname (tramp-file-name-localname v)))
          (cond
           ;; Empty localname (e.g. "/rpc:host:") -> expand to home.
           ((tramp-string-empty-or-nil-p localname)
            (setq name (tramp-make-tramp-file-name v "~")))
           ;; Non-absolute localname (e.g. ".config/") -> make relative
           ;; to home, matching tramp-sh-handle-expand-file-name behavior.
           ;; Without this, the generic handler prepends "/" (root).
           ((not (tramp-run-real-handler
                  #'file-name-absolute-p (list localname)))
            (setq name (tramp-make-tramp-file-name
                        v (concat "~/" localname)))))))))
  (condition-case nil
      (let ((tramp-verbose 0))
        (tramp-handle-expand-file-name name dir))
    (file-error
     (let ((tramp-tolerate-tilde t))
       (tramp-handle-expand-file-name name dir)))))

(defvar tramp-rpc--file-notify-descriptors (make-hash-table :test 'eq)
  "TRAMP-RPC file notification descriptors.")

(defvar tramp-rpc--file-notify-watch-counts (make-hash-table :test 'equal)
  "Reference counts for directories watched via file notifications.
Keys are the same connection/path keys as `tramp-rpc--watched-directories'.")

(defvar tramp-rpc-protocol--message-target)

(declare-function file-notify--rm-descriptor "filenotify")
(declare-function file-notify-rm-watch "filenotify")

(defun tramp-rpc--file-notify-monitor (vec)
  "Return the file notification monitor symbol for VEC."
  (let* ((info (condition-case err
                   (tramp-rpc--system-info vec)
                 (error
                  (tramp-rpc--debug "system.info probe failed: %s"
                                    (error-message-string err))
                  nil)))
         (watcher (and (listp info)
                       (tramp-rpc--decode-string (alist-get 'watcher info))))
         (os (and (listp info)
                  (tramp-rpc--decode-string (alist-get 'os info)))))
    (pcase watcher
      ("inotify" 'TrampRPCinotify)
      ("kqueue" 'TrampRPCkqueue)
      ("fsevent" 'TrampRPCfsevent)
      ("poll" 'TrampRPCpoll)
      ((pred null)
       (pcase os
         ("linux" 'TrampRPCinotify)
         ((or "freebsd" "openbsd" "netbsd" "dragonfly" "ios")
          'TrampRPCkqueue)
         ("macos" 'TrampRPCfsevent)
         (_ 'TrampRPC)))
      (_ 'TrampRPC))))

(defun tramp-rpc--file-notify-process-sentinel (descriptor event)
  "Clean up file notification DESCRIPTOR after EVENT closes it."
  (unless (process-live-p descriptor)
    (tramp-rpc--debug "file-notify descriptor closed: %S %s" descriptor event)
    ;; `file-notify-rm-watch' calls the file-name handler, which deletes the
    ;; descriptor process.  Avoid re-entering it for that intentional close.
    (unless (process-get descriptor 'tramp-rpc-file-notify-removing)
      (file-notify-rm-watch descriptor))))

(defun tramp-rpc--make-file-notify-descriptor (vec _directory localname)
  "Create a TRAMP-style process descriptor for a file notification watch.
VEC is the TRAMP connection vector.
LOCALNAME is the local file name."
  (let* (;; Emacs' remote file notification tests use the process name to
         ;; identify the remote library.  The concrete backend is exposed via
         ;; the "file-monitor" connection property below.
         (name "tramp-rpc")
         ;; This synthetic process is only a watch descriptor; it never
         ;; receives output.  Do not attach a buffer: `global-auto-revert-mode'
         ;; iterates over `buffer-list' while removing file notification
         ;; watches, and deleting descriptor buffers during that iteration can
         ;; make Emacs select a just-deleted buffer.
         (descriptor (make-pipe-process
                      :name name
                      :noquery t
                      :sentinel #'tramp-rpc--file-notify-process-sentinel)))
    ;; These two properties are the ones TRAMP's generic file-notify routing and
    ;; validity helpers expect on watch descriptors.
    (process-put descriptor 'tramp-vector vec)
    (process-put descriptor 'tramp-watch-name localname)
    ;; Match gio/smb-notify: tests can use the library name for broad behavior
    ;; and this property for backend-specific expectations.
    (tramp-set-connection-property
     descriptor "file-monitor" (tramp-rpc--file-notify-monitor vec))
    ;; RPC-private metadata lives in `tramp-rpc--file-notify-descriptors', but
    ;; mark the process as ours for debugging and defensive cleanup.
    (process-put descriptor 'tramp-rpc-file-notify t)
    descriptor))

(defun tramp-rpc--delete-file-notify-descriptor-process (descriptor)
  "Delete DESCRIPTOR's synthetic process."
  (when (processp descriptor)
    (process-put descriptor 'tramp-rpc-file-notify-removing t)
    (when (process-live-p descriptor)
      (delete-process descriptor))))

(defun tramp-rpc--canonical-directory-equal-p (a b)
  "Return non-nil if canonical directory names A and B are equal."
  (and (stringp a)
       (stringp b)
       (string= (directory-file-name a) (directory-file-name b))))

(defun tramp-rpc--watch-entry-canonical-directory (entry)
  "Return canonical directory recorded in watch ENTRY."
  (or (plist-get entry :canonical-directory)
      (plist-get entry :directory)))

(defun tramp-rpc--canonical-watch-active-p (canonical-directory)
  "Return non-nil if CANONICAL-DIRECTORY still has a client-side owner.
Both explicit watch entries and file-notify watch entries count as owners."
  (let (active)
    (when (and (stringp canonical-directory)
               (hash-table-p tramp-rpc--watched-directories))
      (maphash
       (lambda (_key entry)
         (when (tramp-rpc--canonical-directory-equal-p
                canonical-directory
                (tramp-rpc--watch-entry-canonical-directory entry))
           (setq active t)))
       tramp-rpc--watched-directories))
    (when (and (not active)
               (stringp canonical-directory)
               (hash-table-p tramp-rpc--file-notify-watch-counts))
      (maphash
       (lambda (_key entry)
         (when (and (plist-get entry :count)
                    (tramp-rpc--canonical-directory-equal-p
                     canonical-directory
                     (tramp-rpc--watch-entry-canonical-directory entry)))
           (setq active t)))
       tramp-rpc--file-notify-watch-counts))
    active))

(defun tramp-rpc--cleanup-file-notify-for-connection
    (&optional vec connection-process)
  "Remove file notification state for VEC's CONNECTION-PROCESS.
When VEC is nil, remove all state."
  (let* ((prefix (and vec (concat (tramp-rpc--connection-key-string vec) ":")))
         (descriptors-to-remove nil)
         (watch-keys-to-remove nil))
    (maphash
     (lambda (descriptor data)
       (let* ((watch-key (plist-get data :watch-key))
              (entry (and watch-key
                          (gethash watch-key tramp-rpc--file-notify-watch-counts)))
              (owner (or (plist-get data :connection-process)
                         (plist-get entry :connection-process))))
         (when (and (or (null prefix)
                        (and watch-key (string-prefix-p prefix watch-key)))
                    (or (null connection-process)
                        (null owner)
                        (eq connection-process owner)))
           (push descriptor descriptors-to-remove))))
     tramp-rpc--file-notify-descriptors)
    (maphash
     (lambda (watch-key data)
       (when (and (or (null prefix) (string-prefix-p prefix watch-key))
                  (or (null connection-process)
                      (null (plist-get data :connection-process))
                      (eq connection-process
                          (plist-get data :connection-process))))
         (push watch-key watch-keys-to-remove)))
     tramp-rpc--file-notify-watch-counts)
    (dolist (descriptor descriptors-to-remove)
      ;; Remove private state before sending the public `stopped' event, so
      ;; callbacks observing `file-notify-valid-p' during cleanup see the
      ;; descriptor as no longer valid.
      (remhash descriptor tramp-rpc--file-notify-descriptors)
      (tramp-rpc--delete-file-notify-descriptor-process descriptor)
      (when (and (boundp 'file-notify-descriptors)
                 (gethash descriptor file-notify-descriptors))
        (require 'filenotify)
        (file-notify--rm-descriptor descriptor)))
    (dolist (watch-key watch-keys-to-remove)
      (remhash watch-key tramp-rpc--file-notify-watch-counts))
    (when (or descriptors-to-remove watch-keys-to-remove)
      (tramp-rpc--debug
       "Cleaned up %d file-notify descriptors and %d file-notify watches%s"
       (length descriptors-to-remove)
       (length watch-keys-to-remove)
       (if vec (format " for %s" prefix) "")))))

(defun tramp-rpc--file-notify-relative-name (directory file)
  "Return FILE's relative name under DIRECTORY, or nil.
Only DIRECTORY itself and immediate children match.  DIRECTORY itself returns
the empty string."
  (let* ((dir (file-name-as-directory (directory-file-name directory)))
         (file (directory-file-name file)))
    (cond
     ((string= (directory-file-name dir) file) "")
     ((string-prefix-p dir file)
      (let ((rest (substring file (length dir))))
        (and (not (string-empty-p rest))
             (not (string-match-p "/" rest))
             rest))))))

(defun tramp-rpc--file-notify-direct-child-p (directory file)
  "Return non-nil if FILE is DIRECTORY or its immediate child."
  (and (tramp-rpc--file-notify-relative-name directory file) t))

(defun tramp-rpc--file-notify-action-enabled-p (action flags)
  "Return non-nil when ACTION is enabled by file notification FLAGS."
  (or (member action '("stopped"))
      (and (memq 'change flags)
           (member action '("created" "changed" "deleted"
                            "renamed" "renamed-from" "renamed-to")))
      (and (memq 'attribute-change flags)
           (string= action "attribute-changed"))))

(defun tramp-rpc--file-notify-callback-action (action)
  "Map protocol ACTION to an action accepted by `file-notify-callback'."
  (pcase action
    ("created" 'created)
    ("changed" 'changed)
    ("attribute-changed" 'attribute-changed)
    ("deleted" 'deleted)
    ("renamed" 'moved)
    ("renamed-from" 'moved-from)
    ("renamed-to" 'moved-to)
    ("stopped" 'unmounted)
    (_ nil)))

(defun tramp-rpc--file-notify-alias-paths (file-name)
  "Return original watch spellings equivalent to canonical FILE-NAME."
  (let (aliases)
    (when (hash-table-p tramp-rpc--file-notify-descriptors)
      (maphash
       (lambda (_descriptor data)
         (when-let* ((canonical-directory
                      (tramp-rpc--file-notify-canonical-directory data))
                     ((tramp-rpc--file-notify-relative-name
                       canonical-directory file-name))
                     (alias
                      (tramp-rpc--file-notify-original-spelling
                       data file-name))
                     ((not (string= alias file-name))))
           (cl-pushnew alias aliases :test #'string=)))
       tramp-rpc--file-notify-descriptors))
    aliases))

(defun tramp-rpc--file-notify-canonical-directory (data)
  "Return the canonical directory associated with descriptor DATA."
  (let* ((watch-key (plist-get data :watch-key))
         (watch-entry (and watch-key
                           (gethash watch-key
                                    tramp-rpc--file-notify-watch-counts))))
    ;; Prefer the shared watch entry, because explicit unwatch can restore a
    ;; file-notify-owned direct watch and learn a newer canonical path after
    ;; descriptors were created.
    (or (plist-get watch-entry :canonical-directory)
        (plist-get data :canonical-directory))))

(defun tramp-rpc--file-notify-original-spelling (data file-name)
  "Return FILE-NAME rewritten to descriptor DATA's original watch spelling."
  (let* ((canonical-directory
          (tramp-rpc--file-notify-canonical-directory data))
         (directory (plist-get data :directory))
         (relative (and canonical-directory directory
                        (tramp-rpc--file-notify-relative-name
                         canonical-directory file-name))))
    (if relative
        (if (string-empty-p relative)
            (directory-file-name directory)
          (expand-file-name relative directory))
      file-name)))

(defun tramp-rpc--file-notify-callback-name (data file-name)
  "Return FILE-NAME in the form expected by `file-notify-callback'.
`file-notify-callback' expands backend file names relative to the
watch directory stored in `file-notify-descriptors'.  Passing an
already expanded TRAMP name can therefore produce doubled remote
prefixes on some TRAMP versions.  Prefer the name relative to the
original watched directory, falling back to the original spelling when
we cannot derive one.
DATA is the payload to send."
  (let* ((display-file-name
          (tramp-rpc--file-notify-original-spelling data file-name))
         (directory (plist-get data :directory))
         (relative (and directory
                        (tramp-rpc--file-notify-relative-name
                         directory display-file-name))))
    (cond
     ((null relative) display-file-name)
     ((string-empty-p relative) ".")
     (t relative))))

(defun tramp-rpc--file-notify-path-matches-p (data file-name)
  "Return non-nil if descriptor DATA covers FILE-NAME."
  (let ((canonical-directory
         (tramp-rpc--file-notify-canonical-directory data)))
    (or (tramp-rpc--file-notify-direct-child-p
         (plist-get data :directory) file-name)
        ;; The server registers canonical watch paths and can report events
        ;; using that canonical spelling.  Keep the original directory for
        ;; Emacs' public descriptor table, but also match against the
        ;; canonical directory returned by `watch.add' when it differs (for
        ;; example, symlinked watched directories).
        (and canonical-directory
             (tramp-rpc--file-notify-direct-child-p
              canonical-directory file-name)))))

(defun tramp-rpc--file-notify-synthetic-watch-p (file-name)
  "Return non-nil if FILE-NAME is covered by a synthetic symlink watch."
  (let (matched)
    (when (hash-table-p tramp-rpc--file-notify-descriptors)
      (maphash
       (lambda (_descriptor data)
         (let* ((watch-key (plist-get data :watch-key))
                (entry (and watch-key
                            (gethash watch-key
                                     tramp-rpc--file-notify-watch-counts))))
           (when (and (plist-get entry :synthetic)
                      (tramp-rpc--file-notify-path-matches-p data file-name))
             (setq matched t))))
       tramp-rpc--file-notify-descriptors))
    matched))

(defun tramp-rpc--file-notify-dispatch-descriptor
    (descriptor data action file-name &optional file-name1 cookie)
  "Dispatch ACTION for one selected DESCRIPTOR using its watch DATA.
FILE-NAME1 is the destination for rename events.  COOKIE pairs tracked renames."
  (when-let* ((callback-action (tramp-rpc--file-notify-callback-action action)))
    ;; `file-notify-callback' and the special-event handler live in
    ;; filenotify.el.  It is normally loaded before watches are registered.
    (require 'filenotify)
    (let* ((display-file-name
            (tramp-rpc--file-notify-callback-name data file-name))
           (display-file-name1
            (and file-name1
                 (tramp-rpc--file-notify-callback-name data file-name1)))
           (event-data (append (list descriptor (list callback-action)
                                     display-file-name)
                               (cond
                                (display-file-name1 (list display-file-name1))
                                (cookie (list cookie)))))
           (event `(file-notify ,event-data file-notify-callback)))
      (if (fboundp 'insert-special-event)
          (insert-special-event event)
        (funcall (lookup-key special-event-map [file-notify]) event)))))

(defun tramp-rpc--file-notify-dispatch-rescan (connection-process)
  "Dispatch conservative events for live watches on CONNECTION-PROCESS."
  (let (dispatches)
    ;; Select concrete descriptors before dispatch.  Feeding their directories
    ;; through the path router would also reach dead or replacement-generation
    ;; descriptors that happen to watch the same spelling.
    (maphash
     (lambda (descriptor data)
       (when (and (process-live-p descriptor)
                  (eq connection-process
                      (plist-get data :connection-process)))
         (let ((directory (plist-get data :directory))
               (flags (plist-get data :flags)))
           (when (memq 'change flags)
             (push (list descriptor data "changed" directory) dispatches))
           (when (memq 'attribute-change flags)
             (push (list descriptor data "attribute-changed" directory)
                   dispatches)))))
     tramp-rpc--file-notify-descriptors)
    (dolist (dispatch dispatches)
      (apply #'tramp-rpc--file-notify-dispatch-descriptor dispatch))))

(defun tramp-rpc--file-notify-dispatch (action file-name &optional file-name1 cookie)
  "Dispatch a `file-notify' ACTION for TRAMP FILE-NAME.
FILE-NAME1 is the destination for `renamed' events.  COOKIE pairs
`renamed-from' and `renamed-to' events when the server provides one."
  (when (and (hash-table-p tramp-rpc--file-notify-descriptors)
             (> (hash-table-count tramp-rpc--file-notify-descriptors) 0)
             (tramp-rpc--file-notify-callback-action action))
    (let (descriptors)
      (maphash
       (lambda (descriptor data)
         (when (and (tramp-rpc--file-notify-action-enabled-p
                     action (plist-get data :flags))
                    (or (tramp-rpc--file-notify-path-matches-p data file-name)
                        (and file-name1
                             (tramp-rpc--file-notify-path-matches-p
                              data file-name1))))
           (push (cons descriptor data) descriptors)))
       tramp-rpc--file-notify-descriptors)
      (dolist (descriptor-data descriptors)
        (tramp-rpc--file-notify-dispatch-descriptor
         (car descriptor-data) (cdr descriptor-data)
         action file-name file-name1 cookie)))))

(defun tramp-rpc-handle-file-notify-add-watch (directory flags _callback)
  "Like `file-notify-add-watch' for TRAMP-RPC files.
DIRECTORY is the remote directory passed by `file-notify-add-watch'.
FLAGS controls the requested operation."
  ;; `file-notify-add-watch' validates FLAGS and CALLBACK before invoking file
  ;; name handlers, and stores the callback in `file-notify-descriptors' after
  ;; this handler returns.  We only need to create a distinct descriptor and
  ;; ensure the corresponding remote directory is watched.
  (require 'filenotify)
  (with-parsed-tramp-file-name directory nil
    (let* ((watch-key (format "%s:%s" (tramp-rpc--connection-key-string v)
                              localname))
           (entry (gethash watch-key tramp-rpc--file-notify-watch-counts))
           (preexisting (gethash watch-key tramp-rpc--watched-directories))
           ;; file-notify does not follow symlinks.  Ask the server for a
           ;; nofollow symlink watch when needed, falling back to a synthetic
           ;; client-side descriptor on platforms without nofollow support.
           (symlink-watch
            (condition-case err
                (file-symlink-p directory)
              (error
               (tramp-rpc--debug "symlink watch probe failed for %s: %s"
                                 directory (error-message-string err))
               nil)))
           (descriptor (tramp-rpc--make-file-notify-descriptor
                        v directory localname)))
      (if entry
          (plist-put entry :count (1+ (plist-get entry :count)))
        ;; Keep file-notify's non-recursive watches out of
        ;; `tramp-rpc--watched-directories'.  That table is also used by Magit
        ;; and cache invalidation, where a truthy entry means a recursive
        ;; worktree/cache watch may already exist.
        (let* ((synthetic nil)
               (result (cond
                        (symlink-watch
                         (if preexisting
                             (progn
                               (setq synthetic symlink-watch)
                               nil)
                           (condition-case err
                               (tramp-rpc--call
                                v "watch.add"
                                `((path . ,localname)
                                  (recursive . :msgpack-false)
                                  (nofollow . t)))
                             (error
                              (setq synthetic symlink-watch)
                              (tramp-rpc--debug
                               "nofollow file-notify watch unsupported for %s: %s"
                               directory (error-message-string err))
                              nil))))
                        (preexisting nil)
                        (t
                         (tramp-rpc--call v "watch.add"
                                          `((path . ,localname)
                                            (recursive . :msgpack-false))))))
               (canonical-localname (and (listp result)
                                         (alist-get 'path result)))
               (canonical-directory (cond
                                     ((and (stringp canonical-localname)
                                           (tramp-tramp-file-p canonical-localname))
                                      canonical-localname)
                                     ((stringp canonical-localname)
                                      (tramp-make-tramp-file-name
                                       v canonical-localname))
                                     ;; If the server watch preexisted, there
                                     ;; is no `watch.add' response to learn its
                                     ;; canonical spelling from.  Use TRAMP's
                                     ;; truename path as a best-effort match key
                                     ;; for symlinked watched directories.
                                     (preexisting
                                      (condition-case err
                                          (file-truename directory)
                                        (error
                                         (tramp-rpc--debug
                                          "watch truename probe failed for %s: %s"
                                          directory
                                          (error-message-string err))
                                         nil))))))
          (puthash watch-key
                   (list :count 1
                         :owned (and (not preexisting) (not synthetic))
                         :synthetic synthetic
                         :directory directory
                         :canonical-directory canonical-directory
                         :connection-process (tramp-rpc--connection-transport (tramp-rpc--get-connection v)))
                   tramp-rpc--file-notify-watch-counts)))
      (let ((watch-entry (gethash watch-key tramp-rpc--file-notify-watch-counts)))
        (puthash descriptor
                 (list :directory directory
                       :canonical-directory (plist-get watch-entry
                                                       :canonical-directory)
                       :flags flags
                       :localname localname
                       :watch-key watch-key
                       :connection-process (tramp-rpc--connection-transport (tramp-rpc--get-connection v)))
                 tramp-rpc--file-notify-descriptors))
      descriptor)))

(defun tramp-rpc-handle-file-notify-rm-watch (descriptor)
  "Like `file-notify-rm-watch' for TRAMP-RPC watch DESCRIPTOR."
  (when-let* ((data (gethash descriptor tramp-rpc--file-notify-descriptors)))
    (let* ((watch-key (plist-get data :watch-key))
           (entry (gethash watch-key tramp-rpc--file-notify-watch-counts))
           (canonical-directory
            (tramp-rpc--watch-entry-canonical-directory entry))
           (count (and entry (plist-get entry :count))))
      (cond
       ((and count (> count 1))
        (plist-put entry :count (1- count)))
       (entry
        (remhash watch-key tramp-rpc--file-notify-watch-counts)
        (when (and (plist-get entry :owned)
                   ;; If a Magit/cache watch has been installed for the same
                   ;; key or canonical path while this file notification was
                   ;; live, do not remove the server watch from underneath it.
                   (not (tramp-rpc--canonical-watch-active-p
                         canonical-directory)))
          ;; Removing a file notification should not make
          ;; `file-notify-rm-watch' fail if the remote connection has already
          ;; gone away.
          (condition-case err
              (tramp-rpc-unwatch-directory (plist-get entry :directory))
            (error
             (tramp-rpc--debug "failed to remove file-notify watch %s: %s"
                               (plist-get entry :directory)
                               (error-message-string err))))))))
    (remhash descriptor tramp-rpc--file-notify-descriptors)
    (tramp-rpc--delete-file-notify-descriptor-process descriptor)))

(defun tramp-rpc-handle-file-notify-valid-p (descriptor)
  "Like `file-notify-valid-p' for TRAMP-RPC watch DESCRIPTOR."
  (and (processp descriptor)
       (process-live-p descriptor)
       (gethash descriptor tramp-rpc--file-notify-descriptors)
       t))

;; ============================================================================
;; File name handler registration
;; ============================================================================

(defconst tramp-rpc-file-name-handler-alist
  '(;; =========================================================================
    ;; RPC-based file attribute operations
    ;; =========================================================================
    (file-exists-p . tramp-rpc-handle-file-exists-p)
    (file-readable-p . tramp-rpc-handle-file-readable-p)
    (file-writable-p . tramp-handle-file-writable-p)
    (file-executable-p . tramp-rpc-handle-file-executable-p)
    (file-directory-p . tramp-rpc-handle-file-directory-p)
    (file-regular-p . tramp-rpc-handle-file-regular-p)
    (file-symlink-p . tramp-rpc-handle-file-symlink-p)
    (file-truename . tramp-rpc-handle-file-truename)
    (file-attributes . tramp-rpc-handle-file-attributes)
    (file-modes . tramp-handle-file-modes)
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
    (file-ownership-preserved-p . tramp-rpc-handle-file-ownership-preserved-p)
    (file-system-info . tramp-rpc-handle-file-system-info)

    ;; =========================================================================
    ;; RPC-based file modification operations
    ;; =========================================================================
    (set-file-modes . tramp-rpc-handle-set-file-modes)
    (set-file-times . tramp-rpc-handle-set-file-times)
    (tramp-set-file-uid-gid . tramp-rpc-handle-set-file-uid-gid)

    ;; =========================================================================
    ;; RPC-based directory operations
    ;; =========================================================================
    (directory-files . tramp-rpc-handle-directory-files)
    (directory-files-and-attributes . tramp-rpc-handle-directory-files-and-attributes)
    (file-name-all-completions . tramp-rpc-handle-file-name-all-completions)
    (make-directory . tramp-rpc-handle-make-directory)
    (delete-directory . tramp-rpc-handle-delete-directory)

    (insert-directory . tramp-rpc-handle-insert-directory)
    (copy-directory . tramp-rpc-handle-copy-directory)

    ;; =========================================================================
    ;; RPC-based file I/O operations
    ;; =========================================================================
    (insert-file-contents . tramp-rpc-handle-insert-file-contents)
    (write-region . tramp-rpc-handle-write-region)
    (copy-file . tramp-rpc-handle-copy-file)
    (rename-file . tramp-rpc-handle-rename-file)
    (delete-file . tramp-rpc-handle-delete-file)
    (make-symbolic-link . tramp-rpc-handle-make-symbolic-link)
    (add-name-to-file . tramp-rpc-handle-add-name-to-file)
    (file-local-copy . tramp-rpc-handle-file-local-copy)

    ;; =========================================================================
    ;; RPC-based process operations
    ;; =========================================================================
    (process-file . tramp-rpc-handle-process-file)
    (shell-command . tramp-handle-shell-command)
    (make-process . tramp-rpc-handle-make-process)
    (start-file-process . tramp-rpc-handle-start-file-process)

    ;; =========================================================================
    ;; RPC-based system information
    ;; =========================================================================
    (tramp-get-home-directory . tramp-rpc-handle-get-home-directory)
    (tramp-get-remote-uid . tramp-rpc-handle-get-remote-uid)
    (tramp-get-remote-gid . tramp-rpc-handle-get-remote-gid)
    (tramp-get-remote-groups . tramp-rpc-handle-get-remote-groups)
    (exec-path . tramp-rpc-handle-exec-path)
    (list-system-processes . tramp-handle-list-system-processes)
    (process-attributes . tramp-handle-process-attributes)

    ;; =========================================================================
    ;; RPC-based extended attributes (ACL/SELinux via process.run)
    ;; =========================================================================
    (file-acl . tramp-rpc-handle-file-acl)
    (set-file-acl . tramp-rpc-handle-set-file-acl)
    (file-selinux-context . tramp-rpc-handle-file-selinux-context)
    (set-file-selinux-context . tramp-rpc-handle-set-file-selinux-context)

    ;; =========================================================================
    ;; RPC-based path and VC operations
    ;; =========================================================================
    (expand-file-name . tramp-rpc-handle-expand-file-name)
    (vc-registered . tramp-rpc-handle-vc-registered)

    ;; =========================================================================
    ;; Generic TRAMP handlers (work with any backend, no remote I/O needed)
    ;; These use tramp-handle-* functions that operate on cached data or
    ;; delegate to our RPC handlers internally.
    ;; =========================================================================
    (abbreviate-file-name . tramp-handle-abbreviate-file-name)
    (file-group-gid . tramp-handle-file-group-gid)
    (file-user-uid . tramp-handle-file-user-uid)
    (memory-info . tramp-handle-memory-info)
    (access-file . tramp-rpc-handle-access-file)
    (directory-file-name . tramp-handle-directory-file-name)
    (dired-uncache . tramp-handle-dired-uncache)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    (file-remote-p . tramp-handle-file-remote-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    (load . tramp-handle-load)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)

    ;; =========================================================================
    ;; Generic TRAMP handlers for local Emacs state (locking, modtime, temp files)
    ;; =========================================================================
    (file-locked-p . tramp-handle-file-locked-p)
    (lock-file . tramp-handle-lock-file)
    (unlock-file . tramp-handle-unlock-file)
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (temporary-file-directory . tramp-handle-temporary-file-directory)

    ;; =========================================================================
    ;; RPC-backed file notifications
    ;; =========================================================================
    (file-notify-add-watch . tramp-rpc-handle-file-notify-add-watch)
    (file-notify-rm-watch . tramp-rpc-handle-file-notify-rm-watch)
    (file-notify-valid-p . tramp-rpc-handle-file-notify-valid-p)

    ;; =========================================================================
    ;; Intentionally ignored (not applicable or handled elsewhere)
    ;; =========================================================================
    (byte-compiler-base-file-name . ignore)  ; Not needed for remote files
    (diff-latest-backup-file . ignore)       ; Backup handling is local
    (make-directory-internal . ignore)       ; We implement make-directory
    (unhandled-file-name-directory . ignore) ; Should return nil for TRAMP
    )
  "Alist of handler functions for TRAMP-RPC method.")

(defun tramp-rpc--install-core-external-operations ()
  "Install external operations implemented by the core module."
  (tramp-rpc--add-external-operation 'locate-dominating-file 'tramp-rpc-handle-locate-dominating-file 'tramp-rpc)
  (tramp-rpc--add-external-operation 'dir-locals--all-files 'tramp-rpc-handle-dir-locals--all-files 'tramp-rpc)
  (tramp-rpc--add-external-operation 'dir-locals-find-file 'tramp-rpc-handle-dir-locals-find-file 'tramp-rpc)
  (tramp-rpc--add-external-operation 'move-file-to-trash 'tramp-rpc-handle-move-file-to-trash 'tramp-rpc 'file))

;;;###autoload
(defun tramp-rpc-file-name-handler (operation &rest args)
  "Invoke TRAMP-RPC file name handler for OPERATION with ARGS.
Falls back to the local handler when `non-essential' is non-nil and
a backend function throws `non-essential' (e.g. because no connection
exists and opening one would block).  This mirrors the catch/throw
pattern in `tramp-file-name-handler'."
  (tramp-rpc--check-tramp-version)
  ;; `file-remote-p' is called for everything, even for symbolic
  ;; links which look remote.  We don't want to get an error.
  (let ((non-essential (or non-essential (eq operation 'file-remote-p))))
    (if-let* ((handler (assq operation tramp-rpc-file-name-handler-alist)))
        (let ((result (catch 'non-essential
                        (save-match-data (apply (cdr handler) args)))))
          (if (eq result 'non-essential)
              (tramp-run-real-handler operation args)
            result))
      (tramp-run-real-handler operation args))))

;; ============================================================================
;; Method predicate and handler registration
;; ============================================================================

;; `tramp-rpc-file-name-p' is defined in the autoload block above.  Re-define
;; it here so the installed implementation is associated with this source file for
;; the full-load case so it gets proper byte-compilation.
(defun tramp-rpc-file-name-p (vec-or-filename)
  "Check if VEC-OR-FILENAME is handled by TRAMP-RPC.
VEC-OR-FILENAME can be either a tramp-file-name struct or a filename string."
  (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename)))
    (string= (tramp-file-name-method vec) tramp-rpc-method)))

;; Re-register with the full defun now that the file is loaded.
;; (Already registered by the autoload code, but this ensures the
;; byte-compiled defun version is used.)
(tramp-register-foreign-file-name-handler
 #'tramp-rpc-file-name-p #'tramp-rpc-file-name-handler)

;; ============================================================================
;; Connection cleanup support
;; ============================================================================

(add-hook 'tramp-rpc-transport-cleanup-functions
          #'tramp-rpc--cleanup-file-notify-for-connection t)

(defun tramp-rpc-cleanup-connection (vec)
  "Clean up TRAMP-RPC resources for connection VEC.
This is called from `tramp-cleanup-connection-hook' after TRAMP's
generic cleanup has already run (passwords cleared, timers cancelled,
connection buffer killed, TRAMP caches flushed).

Handles RPC-specific state: the connection hash table, async/PTY
processes, file watches, ControlMaster process/socket, pending RPC
responses, and RPC-specific caches (direnv, executable, file-exists,
`file-truename')."
  (when (tramp-rpc--managed-file-name-p vec)
    ;; Delegate to disconnect for the common cleanup: async/PTY
    ;; processes, watches, connection hash, executable cache.
    ;; The redundant tramp-flush-* calls in disconnect are harmless.
    (tramp-rpc--disconnect vec)
    ;; Clear RPC-specific caches for this connection.
    (tramp-rpc--clear-direnv-cache vec)
    (tramp-rpc--clear-file-caches-for-connection vec)
    ;; Clean up ControlMaster SSH process and socket.
    (tramp-rpc--cleanup-controlmaster vec)
    ;; Note: recentf cleanup is handled by `tramp-recentf-cleanup' from
    ;; tramp-integration.el, which is registered on the same
    ;; `tramp-cleanup-connection-hook'.
    ))

(defun tramp-rpc-cleanup-all-connections ()
  "Clean up all TRAMP-RPC connections.
Called from `tramp-cleanup-all-connections-hook' after TRAMP's generic
cleanup of all connections has run."
  ;; Snapshot actual generations, then run the same explicit cleanup core for
  ;; each live transport.  In particular, do not pass a nil process to the
  ;; per-process cleanup helpers: that would allow one generation to clean
  ;; another and would skip remote termination ordering.
  (let (generations)
    (maphash (lambda (_key conn)
               (when-let* ((process (tramp-rpc-connection-process conn))
                           (vec (tramp-rpc-connection-vec conn)))
                 (push (cons vec process) generations)))
             tramp-rpc--connections)
    (dolist (generation generations)
      (let ((vec (car generation))
            (process (cdr generation)))
        (when (processp process)
          (tramp-rpc--cleanup-connection-generation
           process vec "explicit global disconnect\n" :explicit-disconnect t)
          (tramp-rpc--cleanup-controlmaster vec))))
    (clrhash tramp-rpc--watched-directories)
    (tramp-rpc--cleanup-file-notify-for-connection)
    ;; Also kill orphaned auth buffers from failed connection attempts.
    (dolist (buf (buffer-list))
      (when (string-match-p "\\` \\*tramp-rpc-auth " (buffer-name buf))
        (when-let* ((proc (get-buffer-process buf)))
          (when (process-live-p proc)
            (delete-process proc)))
        (kill-buffer buf)))
    (clrhash tramp-rpc--connections)
    (clrhash tramp-rpc--connection-failures))
  ;; Clear all RPC-specific caches.
  (tramp-rpc-protocol--clear-deferred-polls)
  (tramp-rpc--clear-direnv-cache)
  (tramp-rpc--clear-file-metadata-caches)
  ;; Note: recentf cleanup is handled by `tramp-recentf-cleanup-all'
  ;; from tramp-integration.el, registered on the same
  ;; `tramp-cleanup-all-connections-hook'.
  )

;; ============================================================================
;; Unload support
;; ============================================================================

(defun tramp-rpc--after-load-integrations (_file)
  "Install integrations whose optional packages have just loaded."
  (tramp-rpc-process-install-optional-handlers)
  (tramp-rpc-advice-install-optional-handlers)
  (tramp-rpc-magit-install-optional-handlers))

(defun tramp-rpc--unload-from-tramp ()
  "Unload tramp-rpc when TRAMP itself is unloaded."
  (when (featurep 'tramp-rpc)
    (unload-feature 'tramp-rpc 'force)))

(defun tramp-rpc--install ()
  "Install TRAMP-RPC operations, advice, and lifecycle hooks."
  (tramp-rpc--install-core-external-operations)
  (dolist (function '(tramp-get-connection-property
                      tramp-set-connection-property
                      tramp-flush-connection-property
                      tramp-connection-property-p))
    (unless (advice-member-p
             #'tramp-rpc--route-generic-connection-property-advice function)
      (advice-add function :around
                  #'tramp-rpc--route-generic-connection-property-advice)))
  (tramp-rpc-handler-install)
  (tramp-rpc--after-load-integrations nil)
  (add-hook 'after-load-functions #'tramp-rpc--after-load-integrations)
  (add-hook 'tramp-cleanup-connection-hook #'tramp-rpc-cleanup-connection)
  (add-hook 'tramp-cleanup-all-connections-hook
            #'tramp-rpc-cleanup-all-connections)
  (add-hook 'tramp-unload-hook #'tramp-rpc--unload-from-tramp))

(defun tramp-rpc--sanitize-native-comp-load-history ()
  "Remove malformed native-comp entries that prevent unloading.

Native compilation can record anonymous compiled functions as
`(defun . --anonymous-lambda)' in `load-history'.  Emacs bug#80446
causes `unload-feature' to reject those entries.  Remove them from all
TRAMP-RPC modules before any of the modules are unloaded."
  (dolist (entry load-history)
    (when (and (stringp (car entry))
               (string-match-p
                (rx bos "tramp-rpc" (opt "-" (+ nonl))
                    (| ".el" ".elc" ".eln") eos)
                (file-name-nondirectory (car entry))))
      (setcdr entry
              (delete '(defun . --anonymous-lambda) (cdr entry))))))

(defun tramp-rpc-unload-function ()
  "Unload function for tramp-rpc.
Removes advice and cleans up async processes."
  ;; Work around Emacs bug#80446 before explicitly unloading helper modules.
  (tramp-rpc--sanitize-native-comp-load-history)
  ;; Remove high-level external operations from tramp-rpc core.
  (tramp-rpc--remove-external-operation 'locate-dominating-file 'tramp-rpc)
  (tramp-rpc--remove-external-operation 'dir-locals--all-files 'tramp-rpc)
  (tramp-rpc--remove-external-operation 'dir-locals-find-file 'tramp-rpc)
  (tramp-rpc--remove-external-operation 'move-file-to-trash 'tramp-rpc)
  ;; Unload helper modules explicitly.  Their standard feature unload
  ;; functions perform module-specific cleanup.
  (dolist (feature '(tramp-rpc-advice tramp-rpc-magit tramp-rpc-cache
                     tramp-rpc-process tramp-rpc-transport tramp-rpc-deploy
                     tramp-rpc-hops tramp-rpc-connection tramp-rpc-protocol))
    (when (featurep feature)
      (unload-feature feature 'force)))
  ;; Remove multi-hop hook, property advice, and cleanup hooks.
  (dolist (function '(tramp-get-connection-property
                      tramp-set-connection-property
                      tramp-flush-connection-property
                      tramp-connection-property-p))
    (advice-remove function #'tramp-rpc--route-generic-connection-property-advice))
  (remove-hook 'after-load-functions #'tramp-rpc--after-load-integrations)
  (remove-hook 'tramp-multi-hop-p-hook #'tramp-rpc-multi-hop-p)
  (remove-hook 'tramp-cleanup-connection-hook #'tramp-rpc-cleanup-connection)
  (remove-hook 'tramp-cleanup-all-connections-hook #'tramp-rpc-cleanup-all-connections)
  (remove-hook 'tramp-unload-hook #'tramp-rpc--unload-from-tramp)
  ;; Remove method registrations.
  (setq tramp-methods (delete (assoc tramp-rpc-method tramp-methods) tramp-methods))
  (setq tramp-foreign-file-name-handler-alist
	(delete (assoc 'tramp-rpc--sudo-file-name-p
			tramp-foreign-file-name-handler-alist)
		tramp-foreign-file-name-handler-alist))
  (setq tramp-foreign-file-name-handler-alist
	(delete (assoc 'tramp-rpc-file-name-p
			tramp-foreign-file-name-handler-alist)
		tramp-foreign-file-name-handler-alist))
  ;; Return nil to allow normal unload to proceed
  nil)

(provide 'tramp-rpc)
(condition-case err
    (tramp-rpc--install)
  (error
   ;; `tramp-add-external-operation' requires the backend feature, so the
   ;; feature must be visible during installation.  Do not leave a partially
   ;; initialized package marked as loaded when installation fails.
   (setq features (delq 'tramp-rpc features))
   (signal (car err) (cdr err))))
;;; tramp-rpc.el ends here
