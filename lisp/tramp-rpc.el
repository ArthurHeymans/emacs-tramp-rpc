;;; tramp-rpc.el --- TRAMP backend using RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Version: 0.12.0
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
;; If you experience issues with diff-hl in dired, you can disable it:
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

;;;###autoload
(with-eval-after-load 'tramp
  ;; Check, that `tramp-rpc-method' is still bound.  It isn't after
  ;; unloading `tramp-rpc', but this body still exists as compiled
  ;; function in `after-load-alist'.
  (when (boundp 'tramp-rpc-method)
  ;; Register the method
  (add-to-list 'tramp-methods
               `(,tramp-rpc-method
                 ;; Declare that the rpc method uses the host name.
                 ;; tramp-compute-multi-hops validates that methods without
                 ;; "%h" in tramp-login-args use a host matching the previous
                 ;; hop.  Since rpc IS host-directed (it SSH-connects to the
                 ;; specified host), advertising "%h" here lets rpc appear
                 ;; as a proxy hop in chains like /rpc:server|sudo:root@server:/path
                 ;; without triggering the "host does not match" error.
                 ;; The actual tramp-login-args value is never used for login
                 ;; because rpc is a foreign (non-tramp-sh) file handler.
                 (tramp-login-args (("%h")))
                 ;; Match TRAMP's remote-shell convention so PTY login shells
                 ;; can honor method/connection-local overrides.
                 (tramp-remote-shell-login ("-l"))
                 ;; Direct async process support: tramp-rpc uses direct SSH
                 ;; PTY connections for async processes, which means stderr
                 ;; is mixed with stdout (normal PTY behavior).  Setting
                 ;; tramp-direct-async lets upstream tests know to skip
                 ;; stderr-separation assertions for async shell-command.
                 (tramp-direct-async t)))

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

  ;; Define the predicate inline (as defsubst) so it's available without
  ;; loading tramp-rpc.el.  This avoids recursive autoloading: TRAMP calls
  ;; the predicate to decide which handler to use, and if it were an
  ;; autoload stub it would load tramp-rpc.el which `(require 'tramp)'.
  ;; Reference TRAMP uses the same pattern (defsubst in tramp-loaddefs.el).
  (defvar tramp-rpc--sudo-file-name-p-in-progress nil
    "Non-nil while checking hidden rpc+sudo proxy expansion.")

  (defsubst tramp-rpc-file-name-p (vec-or-filename)
    "Check if VEC-OR-FILENAME is handled by TRAMP-RPC."
    (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename)))
      (string= (tramp-file-name-method vec) tramp-rpc-method)))

  ;; Detect privilege elevation paths with rpc hops, e.g.
  ;; /rpc:user@host|sudo:root@host:/path.  These are handled by the
  ;; tramp-rpc handler which starts the RPC server via sudo.
  (defsubst tramp-rpc--sudo-file-name-p (vec-or-filename)
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

;; Now the actual implementation
(require 'cl-lib)
(require 'tramp)
(require 'tramp-sh)
(require 'tramp-rpc-protocol)

;; Check for minimum Tramp version.  The Package-Requires header declares
;; (tramp "2.8.1.4") but that is only enforced by package.el at install
;; time.  Guard at load time so that manual installations fail clearly.
(when (version< tramp-version "2.8.1.4")
  (error "tramp-rpc requires Tramp >= 2.8.1.4, but %s is loaded"
         tramp-version))

;; Give the rpc method all ssh connection parameters so it can serve
;; as a hop in tramp-sh multi-hop chains (e.g.
;; /rpc:host|sudo:root@host:/path).  For single-hop rpc, the foreign
;; handler takes over and these parameters are never used.  This is
;; future-proof: if ssh's parameters change in future TRAMP versions,
;; rpc automatically inherits the updates.
;; Suggested by Michael Albinus.
(when-let* ((ssh-params (alist-get "ssh" tramp-methods nil nil #'equal))
            (rpc-entry (assoc tramp-rpc-method tramp-methods)))
  (setcdr rpc-entry ssh-params))

;; Silence byte-compiler warnings for functions defined in with-eval-after-load
(declare-function tramp-add-external-operation "tramp")
(declare-function tramp-remove-external-operation "tramp")
(declare-function dired-compress-file "dired-aux")
(declare-function tramp-rpc--sudo-file-name-p "tramp-rpc")
(declare-function tramp-rpc-multi-hop-p "tramp-rpc")

(defvar tramp-rpc--sudo-file-name-p-in-progress nil
  "Non-nil while checking hidden rpc+sudo proxy expansion.")

;; ============================================================================
;; Sudo-via-RPC: detect privilege elevation from hop chains
;; ============================================================================

(defun tramp-rpc--privilege-elevation-vec-p (vec)
  "Return non-nil when VEC is TRAMP's sudo previous-hop method.
Other TRAMP previous-hop privilege methods, such as doas, are not
supported here because tramp-rpc starts elevated backends with sudo."
  (and (string= (tramp-file-name-method vec) "sudo")
       (tramp-get-method-parameter vec 'tramp-password-previous-hop)))

(defun tramp-rpc--hop-string-without-postfix (hop-string)
  "Return HOP-STRING without its trailing hop delimiter."
  (if (string-suffix-p tramp-postfix-hop-format hop-string)
      (substring hop-string 0 (- (length tramp-postfix-hop-format)))
    hop-string))

(defun tramp-rpc--hop-vec-to-string (hop-vec)
  "Return HOP-VEC as an ad-hoc hop string without trailing delimiter."
  (tramp-rpc--hop-string-without-postfix
   (tramp-make-tramp-hop-name
    (make-tramp-file-name
     :method (tramp-file-name-method hop-vec)
     :user (tramp-file-name-user hop-vec)
     :domain (tramp-file-name-domain hop-vec)
     :host (tramp-file-name-host hop-vec)
     :port (tramp-file-name-port hop-vec)))))

(defun tramp-rpc--explicit-hop-pairs (vec)
  "Return explicit ad-hoc hops of VEC as (HOP-STRING . HOP-VEC) pairs."
  (when-let* ((hop (tramp-file-name-hop vec)))
    (mapcar
     (lambda (hop-str)
       (cons hop-str
             (tramp-dissect-hop-name
              (concat hop-str tramp-postfix-hop-format) 'nodefault)))
     (split-string hop tramp-postfix-hop-regexp 'omit))))

(defun tramp-rpc--hidden-sudo-proxy-handler-alist ()
  "Return foreign handler alist without the tramp-rpc sudo predicate.
`tramp-compute-multi-hops' asks TRAMP which handler owns the target; if the
sudo predicate remains registered while it is expanding hidden ad-hoc proxies,
it can re-enter `tramp-rpc--sudo-file-name-p'."
  (cl-remove-if
   (lambda (entry) (eq (car entry) 'tramp-rpc--sudo-file-name-p))
   tramp-foreign-file-name-handler-alist))

(defun tramp-rpc--computed-hop-pairs (vec)
  "Return hidden TRAMP proxy hops of VEC as (HOP-STRING . HOP-VEC) pairs.
Native TRAMP helpers, including `tramp-file-name-with-sudo', can store
ad-hoc hops in `tramp-default-proxies-alist' instead of VEC's hop slot."
  (when (and (tramp-rpc--privilege-elevation-vec-p vec)
             (not tramp-rpc--sudo-file-name-p-in-progress))
    (condition-case nil
        (let ((tramp-rpc--sudo-file-name-p-in-progress t)
              (tramp-verbose 0)
              (tramp-foreign-file-name-handler-alist
               (tramp-rpc--hidden-sudo-proxy-handler-alist)))
          (mapcar
           (lambda (hop-vec)
             (cons (tramp-rpc--hop-vec-to-string hop-vec) hop-vec))
           (butlast (tramp-compute-multi-hops vec))))
      (error nil))))

(defun tramp-rpc--hop-pairs (vec)
  "Return VEC's explicit or hidden proxy hops as pairs.
Explicit hops are preferred because they preserve the exact user spelling
from the filename.  Hidden proxy expansion is used for native TRAMP sudo
helpers which record ad-hoc hops in `tramp-default-proxies-alist'."
  (or (tramp-rpc--explicit-hop-pairs vec)
      (tramp-rpc--computed-hop-pairs vec)))

(defun tramp-rpc--hop-component-string (value)
  "Return tramp hop component VALUE as a comparison string."
  (cond ((null value) "")
        ((stringp value) (substring-no-properties value))
        (t (format "%s" value))))

(defun tramp-rpc--same-hop-p (a b)
  "Return non-nil when tramp vecs A and B denote the same hop."
  (and (string= (tramp-rpc--hop-component-string
                 (tramp-file-name-method a))
                (tramp-rpc--hop-component-string
                 (tramp-file-name-method b)))
       (string= (tramp-rpc--hop-component-string
                 (tramp-file-name-user a))
                (tramp-rpc--hop-component-string
                 (tramp-file-name-user b)))
       (string= (tramp-rpc--hop-component-string
                 (tramp-file-name-domain a))
                (tramp-rpc--hop-component-string
                 (tramp-file-name-domain b)))
       (string= (tramp-rpc--hop-component-string
                 (tramp-file-name-host a))
                (tramp-rpc--hop-component-string
                 (tramp-file-name-host b)))
       (string= (tramp-rpc--hop-component-string
                 (tramp-file-name-port a))
                (tramp-rpc--hop-component-string
                 (tramp-file-name-port b)))))

(defun tramp-rpc--same-host-rpc-hop (vec &optional return-string)
  "Return VEC's matching same-host rpc hop for privilege elevation.
When RETURN-STRING is non-nil, return (HOP-STRING . HOP-VEC).  Return
nil for non-privilege targets, so same-host rpc|rpc chains are not
misclassified as sudo-via-RPC."
  (when-let* ((target-host (tramp-file-name-host vec)))
    (when (tramp-rpc--privilege-elevation-vec-p vec)
      (when-let* ((hop (car (last (tramp-rpc--hop-pairs vec))))
                  (hop-vec (cdr hop)))
        ;; Only the final hop before the privilege-elevation method carries the
        ;; SSH connection details for that sudo target.  Earlier same-host rpc
        ;; hops are still real proxy hops and must not be stripped.
        (when (and (string= (tramp-file-name-method hop-vec) "rpc")
                   (string= (tramp-file-name-host hop-vec) target-host))
          (if return-string hop hop-vec))))))

(defun tramp-rpc--sudo-rpc-hop-vec (vec)
  "Return VEC's same-host rpc hop vec for sudo-via-RPC, or nil."
  (tramp-rpc--same-host-rpc-hop vec))

(defun tramp-rpc--detect-sudo-elevation (vec)
  "Return the SSH user if VEC needs sudo elevation via RPC, or nil.
Detects privilege-elevation targets such as
/rpc:user@host|sudo:root@host:/path.  Same-host rpc|rpc chains are not
sudo elevation and return nil."
  (when-let* ((hop-vec (tramp-rpc--sudo-rpc-hop-vec vec)))
    (or (tramp-file-name-user hop-vec) (user-login-name))))

(defun tramp-rpc--ssh-detail-vec (vec)
  "Return the vec carrying SSH connection details for VEC.
For sudo-via-RPC this is the matching same-host rpc hop.  Otherwise it
is VEC itself."
  (or (tramp-rpc--sudo-rpc-hop-vec vec) vec))

(defun tramp-rpc--ssh-detail-user (vec &optional default)
  "Return the SSH user for VEC, using sudo-via-RPC hop details."
  (or (tramp-file-name-user (tramp-rpc--ssh-detail-vec vec)) default))

(defun tramp-rpc--ssh-detail-port (vec)
  "Return the SSH port for VEC, using sudo-via-RPC hop details."
  (tramp-file-name-port (tramp-rpc--ssh-detail-vec vec)))

(declare-function tramp-read-passwd "tramp")

(defun tramp-rpc--sudo-auth-vec (vec)
  "Return the unprivileged rpc vector used to validate sudo for VEC."
  (when-let* ((sudo-hop (tramp-rpc--sudo-rpc-hop-vec vec)))
    (make-tramp-file-name
     :method tramp-rpc-method
     :user (or (tramp-file-name-user sudo-hop) (user-login-name))
     :domain (tramp-file-name-domain sudo-hop)
     :host (tramp-file-name-host sudo-hop)
     :port (tramp-file-name-port sudo-hop)
     :localname (tramp-file-name-localname vec)
     :hop (tramp-rpc--proxy-hop-string vec))))

(defun tramp-rpc--sudo-password-required-p (vec)
  "Return non-nil when sudo on VEC needs a password for non-PTY use."
  (let* ((auth-vec (or (tramp-rpc--sudo-auth-vec vec) vec))
         (result (tramp-rpc--call auth-vec "process.run"
                                  '((cmd . "sudo")
                                    (args . ["-n" "-v"])
                                    (cwd . "/"))))
         (exit-code (alist-get 'exit_code result)))
    (not (eq exit-code 0))))

(defun tramp-rpc--password-string (password)
  "Return PASSWORD as a string, unwrapping auth-source cache entries.
Some Emacs/auth-source combinations can hand `password-read' callers a
cached auth-source plist whose `:secret' is a function.  Normalize that
shape before passing the value to `sudo -S'."
  (while (and (listp password) (plist-member password :secret))
    (setq password (plist-get password :secret)))
  (while (functionp password)
    (setq password (funcall password))
    (while (and (listp password) (plist-member password :secret))
      (setq password (plist-get password :secret))))
  (unless (stringp password)
    (error "sudo password is not a string: %S" password))
  password)

(defun tramp-rpc--sudo-read-password (vec ssh-user)
  "Read sudo password for SSH-USER on VEC using TRAMP auth machinery."
  (let* ((host (tramp-file-name-host vec))
         (raw-port (tramp-rpc--ssh-detail-port vec))
         (port (cond ((stringp raw-port) raw-port)
                     ((numberp raw-port) (number-to-string raw-port))))
         (buffer (get-buffer-create " *tramp-rpc-sudo-password*"))
         (process (make-pipe-process
                   :name "tramp-rpc-sudo-password"
                   :buffer buffer
                   :noquery t)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (insert "Password:"))
          (process-put process 'tramp-vector vec)
          (tramp-set-connection-property process "hop-vector" vec)
          (tramp-set-connection-property
           process "pw-vector"
           (make-tramp-file-name
            :method "sudo" :user ssh-user :host host :port port))
          (tramp-rpc--password-string
           (tramp-read-passwd
            process
            (format "Password for /sudo:%s%s: "
                    (if ssh-user (concat ssh-user "@") "") host))))
      (when (process-live-p process)
        (delete-process process)))))

(defun tramp-rpc--proxy-hop-string (vec)
  "Return VEC's hop string with its sudo rpc hop removed.
For /rpc:gw|rpc:user@host|sudo:root@host:/path, returns \"rpc:gw|\".
For non-sudo paths, returns the original hop string.  Returns nil if no
proxy hops remain."
  (when-let* ((hop-pairs (tramp-rpc--hop-pairs vec)))
    (let ((sudo-hop (tramp-rpc--same-host-rpc-hop vec 'return-string))
          (proxy-hops nil))
      (dolist (hop hop-pairs)
        ;; Drop only the rpc hop that represents privilege elevation.
        ;; Same-host rpc|rpc chains are real hops and must be preserved.
        (unless (and sudo-hop (tramp-rpc--same-hop-p (cdr hop) (cdr sudo-hop)))
          (push (car hop) proxy-hops)))
      (when proxy-hops
        (concat (mapconcat #'identity (nreverse proxy-hops)
                           tramp-postfix-hop-format)
                tramp-postfix-hop-format)))))

(require 'tramp-rpc-deploy)

;; Silence byte-compiler warnings for functions defined elsewhere
;; (vterm variables are declared in tramp-rpc-process.el)

;; Forward declarations for cache/watch functions (tramp-rpc-magit.el)
(defvar tramp-rpc--cache-ttl)
(defvar tramp-rpc--file-exists-cache)
(defvar tramp-rpc--file-truename-cache)
(defvar tramp-rpc--file-stat-cache)
(defvar tramp-rpc--suppress-fs-notifications)
(defvar tramp-rpc--watched-directories)
(defvar tramp-rpc-magit--allow-process-cache)
(declare-function tramp-rpc--cache-get "tramp-rpc-magit")
(declare-function tramp-rpc--cache-put "tramp-rpc-magit")
(declare-function tramp-rpc--cache-lookup "tramp-rpc-magit")
(declare-function tramp-rpc--file-stat-cache-key "tramp-rpc-magit")
(declare-function tramp-rpc--cache-file-stat-result "tramp-rpc-magit")
(declare-function tramp-rpc--invalidate-cache-for-path "tramp-rpc-magit")
(declare-function tramp-rpc--invalidate-cache-for-subtree "tramp-rpc-magit")
(declare-function tramp-rpc--connection-key-string "tramp-rpc-magit")
(declare-function tramp-rpc--directory-watched-p "tramp-rpc-magit")
(declare-function tramp-rpc--handle-notification "tramp-rpc-magit")
(declare-function tramp-rpc-watch-directory "tramp-rpc-magit")
(declare-function tramp-rpc-unwatch-directory "tramp-rpc-magit")
(declare-function tramp-rpc-clear-file-exists-cache "tramp-rpc-magit")
(declare-function tramp-rpc-clear-file-truename-cache "tramp-rpc-magit")
(declare-function tramp-rpc-clear-file-stat-cache "tramp-rpc-magit")
(declare-function tramp-rpc--clear-file-metadata-caches "tramp-rpc-magit")
(declare-function tramp-rpc--cleanup-watches-for-connection "tramp-rpc-magit")
(declare-function tramp-rpc--cleanup-async-processes "tramp-rpc-process")
(declare-function tramp-rpc--cleanup-pty-processes "tramp-rpc-process")
(declare-function tramp-rpc--cleanup-process-write-queues "tramp-rpc-process")
(declare-function tramp-rpc--clear-file-caches-for-connection "tramp-rpc-magit")
(declare-function tramp-rpc-magit--clear-cache "tramp-rpc-magit")
(declare-function tramp-rpc-magit--process-cache-lookup "tramp-rpc-magit")
(declare-function tramp-rpc-magit--process-cache-store "tramp-rpc-magit")
(declare-function tramp-rpc-magit--file-exists-p "tramp-rpc-magit")
(declare-function tramp-rpc-magit--clear-status-cache "tramp-rpc-magit")
(declare-function tramp-rpc-magit--prefetch "tramp-rpc-magit")
(declare-function tramp-rpc-magit--strip-git-prefix-args "tramp-rpc-magit")
(declare-function tramp-rpc-magit--git-cache-safe-environment-p "tramp-rpc-magit")
(declare-function tramp-rpc-magit--clear-cache "tramp-rpc-magit")
(defvar tramp-rpc-magit--debug)
(defvar tramp-rpc-magit--process-caches)

(defvar tramp-rpc--readonly-programs
  '("git" "ls" "cat" "find" "grep" "rg" "test" "stat" "head" "tail"
    "wc" "sort" "uniq" "diff" "comm" "file" "readlink" "realpath"
    "which" "whereis" "id" "whoami" "hostname" "uname" "env" "printenv"
    "date" "du" "df" "free" "uptime" "ps" "top" "awk" "sed" "tr"
    "cut" "paste" "join" "tee" "xargs" "basename" "dirname" "sha256sum"
    "md5sum" "true" "false" "echo" "printf")
  "Programs known to not modify the filesystem.
Used to skip cache invalidation in `tramp-rpc-handle-process-file'.")

(defgroup tramp-rpc nil
  "TRAMP backend using RPC."
  :group 'tramp)

(defcustom tramp-rpc-use-controlmaster t
  "Whether to use SSH ControlMaster for connection sharing.
When enabled, multiple connections to the same host share a single
SSH connection, significantly reducing connection overhead.

The control socket is stored in `tramp-rpc-controlmaster-path'."
  :type 'boolean
  :group 'tramp-rpc)

(defcustom tramp-rpc-controlmaster-path "~/.ssh/tramp-rpc/%C"
  "Path template for SSH ControlMaster socket.
Use SSH escape sequences: %r=remote user, %h=host, %p=port, %C=connection hash.
The %C token (available in OpenSSH 6.7+) creates a unique hash from
%l%h%p%r (local host, remote host, port, user), avoiding path length issues.
For older OpenSSH versions, use: ~/.ssh/tramp-rpc-%r@%h:%p
The directory must exist and be writable."
  :type 'string
  :group 'tramp-rpc)

(defcustom tramp-rpc-controlmaster-persist 600
  "How long (in seconds) to keep ControlMaster connections alive.
Set to 0 to close immediately when last connection exits.
Set to \"yes\" to keep alive indefinitely."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Indefinitely" "yes"))
  :group 'tramp-rpc)

(defcustom tramp-rpc-ssh-options nil
  "Additional SSH options to pass when connecting.
This is a list of strings, each of which is passed as an SSH -o option.
For example, to disable strict host key checking:
  (setq tramp-rpc-ssh-options \\='(\"StrictHostKeyChecking=no\"
                                 \"UserKnownHostsFile=/dev/null\"))

Note: The following options are always passed by default:
  - BatchMode=yes (for RPC connection; ControlMaster handles auth first)
  - StrictHostKeyChecking=accept-new (accept new keys, reject changed)
  - ControlMaster/ControlPath/ControlPersist (if `tramp-rpc-use-controlmaster')

Set this variable to override or supplement these defaults."
  :type '(repeat string)
  :group 'tramp-rpc)

(defcustom tramp-rpc-ssh-args nil
  "Raw SSH arguments to pass when connecting.
This is a list of strings that are passed directly to SSH.
For example: \\='(\"-v\" \"-F\" \"/path/to/config\")

Unlike `tramp-rpc-ssh-options' which adds -o options, this allows
passing any SSH command-line arguments."
  :type '(repeat string)
  :group 'tramp-rpc)

(defcustom tramp-rpc-use-direct-ssh-pty t
  "Whether to use direct SSH connections for PTY processes.
When non-nil, interactive terminal processes (vterm, shell-mode, term-mode)
use a direct SSH connection with `-t` for the PTY, providing much lower
latency than the RPC-based PTY.  The SSH connection reuses the existing
ControlMaster socket, so authentication is already handled.

Note: `signal-process' on direct SSH PTY sends signal to the local SSH
process, which may not propagate to the remote process in all cases."
  :type 'boolean
  :group 'tramp-rpc)

(defcustom tramp-rpc-debug nil
  "When non-nil, log debug messages to *tramp-rpc-debug* buffer.
Set to t to enable debugging for hang diagnosis."
  :type 'boolean
  :group 'tramp-rpc)

(defcustom tramp-rpc-compress-file-read (fboundp 'zlib-decompress-region)
  "When non-nil, use compression for file reads to enable faster transfers."
  :type 'boolean
  :group 'tramp-rpc)

(defconst tramp-rpc-own-remote-path 'tramp-rpc-own-remote-path
  "Deprecated placeholder in `tramp-rpc-remote-path'.
Use TRAMP's `tramp-own-remote-path' in `tramp-remote-path' instead.
This symbol is still accepted for backward compatibility and is treated
like `tramp-own-remote-path'.")

(defcustom tramp-rpc-remote-path nil
  "Deprecated tramp-rpc-specific remote executable search path.
When nil, tramp-rpc uses TRAMP's standard `tramp-remote-path'.  When
non-nil, this value overrides `tramp-remote-path' for compatibility with
older tramp-rpc configurations.

Prefer customizing `tramp-remote-path'.  This compatibility variable
accepts directory strings plus the standard TRAMP placeholders
`tramp-default-remote-path' and `tramp-own-remote-path'.  The old
tramp-rpc placeholder `tramp-rpc-own-remote-path' is also accepted and is
treated like `tramp-own-remote-path'."
  :type '(choice
          (const :tag "Use `tramp-remote-path'" nil)
          (repeat :tag "Compatibility override"
                  (choice (string :tag "Directory")
                          (const :tag "Default Directories" tramp-default-remote-path)
                          (const :tag "Private Directories" tramp-own-remote-path)
                          (const :tag "Deprecated tramp-rpc private directories"
                                 tramp-rpc-own-remote-path))))
  :group 'tramp-rpc)

(defun tramp-rpc--debug (format-string &rest args)
  "Log a debug message to *tramp-rpc-debug* buffer if debugging is enabled.
FORMAT-STRING and ARGS are passed to `format'.

When TRAMP_RPC_DEBUG_LOG or TRAMP_RPC_DEBUG_DIR is set in the environment,
also append each line directly to a local log file.  This preserves CI
telemetry even when later tests unload TRAMP and remove debug buffers."
  (when tramp-rpc-debug
    (let* ((line (concat (format-time-string "[%Y-%m-%d %H:%M:%S.%3N] ")
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
          (error nil))))))

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

(defun tramp-rpc--file-read-params (localname &optional force-uncompressed)
  "Build params for `file.read' on LOCALNAME.
When `tramp-rpc-compress-file-read' is non-nil, request compression unless
FORCE-UNCOMPRESSED is non-nil."
  (let ((params (tramp-rpc--encode-path localname)))
    (when (and tramp-rpc-compress-file-read
               (not force-uncompressed))
      (push '(compress . t) params))
    params))

;; ============================================================================
;; Connection management
;; ============================================================================

(defvar tramp-rpc--connections (make-hash-table :test 'equal)
  "Hash table mapping normalized connection keys to RPC process info.
Keys include target method/user/host/port plus the effective route (explicit
or hidden TRAMP ad-hoc proxy hops).  Values are plists with :process, :buffer,
and optional :stderr-buffer.")

;; tramp-rpc--async-processes and tramp-rpc--pty-processes are defined in
;; tramp-rpc-process.el (loaded via require below)

(defvar tramp-rpc--async-callbacks (make-hash-table :test 'eql)
  "Hash table mapping request IDs to callback functions for async RPC calls.")

(defvar tramp-rpc--async-callback-processes (make-hash-table :test 'eql)
  "Hash table mapping async request IDs to their transport process.")

(defvar tramp-rpc--pending-responses (make-hash-table :test 'eq)
  "Hash table mapping buffers to their pending response hash tables.
Each buffer has its own hash table mapping request IDs to response plists.")

(defun tramp-rpc--get-pending-responses (buffer)
  "Get the pending responses hash table for BUFFER, creating if needed."
  (or (gethash buffer tramp-rpc--pending-responses)
      (puthash buffer (make-hash-table :test 'eql) tramp-rpc--pending-responses)))

;; tramp-rpc--process-write-queues is defined in tramp-rpc-process.el

;; ============================================================================
;; Direnv environment caching for process execution
;; ============================================================================

(defvar tramp-rpc--direnv-cache (make-hash-table :test 'equal)
  "Cache of direnv environments keyed by (connection-key . directory).
Value is a plist with :env (alist) and :timestamp.")

(defvar tramp-rpc--direnv-available-cache (make-hash-table :test 'equal)
  "Cache tracking whether direnv is available on each connection.
Value is :available, :unavailable, or nil (unknown).")

(defcustom tramp-rpc-use-direnv t
  "Whether to load direnv environment for remote processes.
When enabled, runs `direnv export json` to get project-specific
environment variables. Set to nil to disable for better performance."
  :type 'boolean
  :group 'tramp-rpc)

(defcustom tramp-rpc-direnv-cache-timeout 300
  "Seconds to cache direnv environment before re-fetching.
Set to 0 to disable caching (not recommended)."
  :type 'integer
  :group 'tramp-rpc)

(defun tramp-rpc--direnv-cache-key (vec directory)
  "Generate cache key for direnv environment on VEC in DIRECTORY.
Normalizes DIRECTORY via `expand-file-name' so that ~ and the expanded
home path map to the same cache key."
  (cons (tramp-rpc--connection-key vec)
        (tramp-file-local-name
         (expand-file-name
          (tramp-make-tramp-file-name vec directory)))))

(defun tramp-rpc--get-direnv-environment (vec directory)
  "Get direnv environment for DIRECTORY on VEC.
Returns alist of (VAR . VALUE) pairs, or nil if direnv unavailable/disabled.
Results are cached for `tramp-rpc-direnv-cache-timeout' seconds."
  (when tramp-rpc-use-direnv
    (let* ((conn-key (tramp-rpc--connection-key vec))
           (direnv-status (gethash conn-key tramp-rpc--direnv-available-cache)))
      ;; Skip if we already know direnv is unavailable on this host
      (unless (eq direnv-status :unavailable)
        (let* ((cache-key (tramp-rpc--direnv-cache-key vec directory))
               (cached (gethash cache-key tramp-rpc--direnv-cache))
               (now (float-time)))
          ;; Check if cache is valid
          (if (and cached
                   (< (- now (plist-get cached :timestamp))
                      tramp-rpc-direnv-cache-timeout))
              (plist-get cached :env)
            ;; Need to fetch fresh
            (let ((env (tramp-rpc--fetch-direnv-environment vec directory)))
              ;; Cache the result (even if nil, to avoid repeated failures)
              (puthash cache-key
                       (list :env env :timestamp now)
                       tramp-rpc--direnv-cache)
              env)))))))

(defcustom tramp-rpc-direnv-essential-vars
  '("PATH" "LD_LIBRARY_PATH" "LIBRARY_PATH"
    "CARGO_HOME" "RUSTUP_HOME" "RUST_SRC_PATH"
    "CC" "CXX" "PKG_CONFIG_PATH"
    "NIX_CC" "NIX_CFLAGS_COMPILE" "NIX_LDFLAGS"
    "GOPATH" "GOROOT"
    "PYTHONPATH" "VIRTUAL_ENV"
    "NODE_PATH" "NPM_CONFIG_PREFIX")
  "Environment variables to extract from direnv.
Only these variables are passed to remote processes to avoid
performance issues with large environments."
  :type '(repeat string)
  :group 'tramp-rpc)

(defun tramp-rpc--fetch-direnv-environment (vec directory)
  "Fetch direnv environment for DIRECTORY on VEC.
Returns alist of (VAR . VALUE) pairs for essential variables only.
See `tramp-rpc-direnv-essential-vars' for the list of variables."
  (condition-case err
      (let* ((result (tramp-rpc--call vec "process.run"
                                       `((cmd . "/bin/sh")
                                         (args . ["-l" "-c"
                                                  ,(concat "cd " (tramp-shell-quote-argument directory)
                                                           " && direnv export json 2>/dev/null")])
                                         (cwd . "/"))))
             (exit-code (alist-get 'exit_code result))
             (stdout (tramp-rpc--decode-output
                      (alist-get 'stdout result)
                      (alist-get 'stdout_encoding result))))
        (if (and (eq exit-code 0)
                 (> (length stdout) 0))
            ;; Parse JSON output into alist, filter to essential vars
            (condition-case err
                (let* ((json-object-type 'alist)
                       (json-key-type 'string)
                       (full-env (json-read-from-string stdout)))
                  ;; Filter to only essential variables
                  (cl-loop for var in tramp-rpc-direnv-essential-vars
                           for pair = (assoc var full-env)
                           when pair collect pair))
              (error
               (tramp-rpc--debug "direnv JSON parse failed: %S" err)
               nil))
          ;; If exit code is 127 (command not found), mark direnv as unavailable
          (when (eq exit-code 127)
            (puthash (tramp-rpc--connection-key vec)
                     :unavailable
                     tramp-rpc--direnv-available-cache))
          nil))
    (error
     (tramp-rpc--debug "direnv fetch failed: %S" err)
     nil)))

(defun tramp-rpc--clear-direnv-cache (&optional vec)
  "Clear the direnv caches.
If VEC is provided, only clear entries for that connection.
Otherwise clear all entries."
  (if vec
      (let ((conn-key (tramp-rpc--connection-key vec)))
        ;; Clear environment cache entries for this connection
        (let ((keys-to-remove nil))
          (maphash (lambda (key _value)
                     (when (equal (car key) conn-key)
                       (push key keys-to-remove)))
                   tramp-rpc--direnv-cache)
          (dolist (key keys-to-remove)
            (remhash key tramp-rpc--direnv-cache)))
        ;; Clear availability cache for this connection
        (remhash conn-key tramp-rpc--direnv-available-cache))
    (clrhash tramp-rpc--direnv-cache)
    (clrhash tramp-rpc--direnv-available-cache)))

;; Forward-declare caches used by tramp-rpc--remove-connection (defined
;; later in the exec-path section).  The byte-compiler needs to see
;; these defvars before their first reference.
(defvar tramp-rpc--exec-path-cache (make-hash-table :test 'equal)
  "Cache of remote exec-path keyed by connection-key.")

(defvar tramp-rpc--login-shell-cache (make-hash-table :test 'equal)
  "Cache of remote login shell keyed by connection-key.")

(defun tramp-rpc--environment-with (env key value)
  "Return ENV with KEY set to VALUE.
ENV is an alist of (KEY . VALUE) string pairs.  If KEY already exists,
its value is replaced in-place in the returned list; otherwise a new
entry is appended."
  (if-let* ((cell (assoc key env)))
      (progn
        (setcdr cell value)
        env)
    (append env (list (cons key value)))))

(defun tramp-rpc--ensure-inside-emacs-env (env)
  "Ensure INSIDE_EMACS is set in environment alist ENV.
ENV is an alist of (KEY . VALUE) string pairs, or nil.
If INSIDE_EMACS is not already present, it is added with the value
from `tramp-inside-emacs'.  Returns the (possibly augmented) alist."
  (if (assoc "INSIDE_EMACS" env)
      env
    (tramp-rpc--environment-with env "INSIDE_EMACS" (tramp-inside-emacs))))

(defun tramp-rpc--merge-environments (&rest environments)
  "Merge ENVIRONMENTS alists with later entries overriding earlier ones.
Duplicate variable names are removed before the alist is sent over RPC.
This avoids relying on duplicate MessagePack map key ordering on the Rust
server side."
  (let (merged)
    (dolist (env environments)
      (dolist (pair env)
        (when (and (consp pair)
                   (stringp (car pair))
                   (stringp (cdr pair)))
          (setq merged
                (tramp-rpc--environment-with merged (car pair) (cdr pair))))))
    merged))

(defun tramp-rpc--cached-remote-path (vec)
  "Return cached remote PATH directories for VEC, computing them if needed."
  (let* ((key (tramp-rpc--connection-key vec))
         (cached (gethash key tramp-rpc--exec-path-cache)))
    (or cached
        (let ((path (tramp-rpc--compute-remote-path vec)))
          (puthash key path tramp-rpc--exec-path-cache)
          path))))

(defun tramp-rpc--remote-path-environment (vec)
  "Return a PATH environment entry for VEC.
Uses `tramp-remote-path' by default.  A non-nil deprecated
`tramp-rpc-remote-path' overrides it for compatibility."
  (let ((remote-path (tramp-rpc--cached-remote-path vec)))
    (when remote-path
      `(("PATH" . ,(mapconcat #'identity remote-path ":"))))))

(defvar tramp-remote-process-environment)

(defun tramp-rpc--environment-list-to-alist (environment)
  "Convert ENVIRONMENT strings of the form NAME=VALUE to an alist.
Entries without an equals sign are ignored because they request unsetting a
variable in Emacs process APIs, while tramp-rpc sends an explicit environment
map to the remote server."
  (let (alist)
    (dolist (elt environment)
      (when (and (stringp elt)
                 (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" elt))
        (push (cons (match-string 1 elt) (match-string 2 elt)) alist)))
    (nreverse alist)))

(defun tramp-rpc--tramp-remote-process-environment ()
  "Return dynamic `tramp-remote-process-environment' entries as an alist.
This includes dynamic bindings made by packages such as python.el for remote
processes.  The baseline `tramp-remote-process-environment' entries are shell
setup snippets for tramp-sh (for example, LC_CTYPE set to two single quotes),
not a direct exec environment.  Do not pass those defaults to RPC child
processes, because they can leak shell-only quoting and produce warnings on
stdout/stderr."
  (when (boundp 'tramp-remote-process-environment)
    (let ((baseline (default-toplevel-value 'tramp-remote-process-environment))
          dynamic)
      (dolist (elt tramp-remote-process-environment)
        (unless (member elt baseline)
          (push elt dynamic)))
      (tramp-rpc--environment-list-to-alist (nreverse dynamic)))))

(defun tramp-rpc--caller-environment ()
  "Extract environment variable overrides from `process-environment'.
Emacs packages dynamically bind env vars via `with-environment-variables'
or `setenv' (e.g. magit sets GIT_INDEX_FILE for temp-index operations).
These additions/changes land in `process-environment' but are not forwarded
by `tramp-rpc-handle-process-file' unless we explicitly extract them.

Compares the current `process-environment' against the toplevel default.
Entries that are only present in the current dynamic scope (e.g. added
by `with-environment-variables') are returned as an alist of
\(NAME . VALUE) pairs."
  (let ((toplevel (default-toplevel-value 'process-environment))
        (env nil))
    (dolist (elt process-environment)
      (when (and (stringp elt)
                 (not (member elt toplevel))
                 (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" elt))
        (push (cons (match-string 1 elt) (match-string 2 elt)) env)))
    (nreverse env)))

(defsubst tramp-rpc--port-to-string (port)
  "Normalize PORT to a string, or return nil.
PORT may be a number (from defaults), a string (from filename
parsing via `tramp-dissect-file-name'), or nil (when unset).
Upstream TRAMP always stores port as a string in the
`tramp-file-name' struct, but defensive handling of numbers
avoids breakage if callers supply numeric defaults."
  (cond ((stringp port) port)
        ((numberp port) (number-to-string port))
        (t nil)))

(defun tramp-rpc--connection-key-route-hop (hop-vec)
  "Return normalized route identity for HOP-VEC."
  (list (tramp-rpc--hop-component-string (tramp-file-name-method hop-vec))
        (tramp-rpc--hop-component-string (tramp-file-name-user hop-vec))
        (tramp-rpc--hop-component-string (tramp-file-name-domain hop-vec))
        (tramp-rpc--hop-component-string (tramp-file-name-host hop-vec))
        (or (tramp-rpc--port-to-string (tramp-file-name-port hop-vec)) "22")))

(defun tramp-rpc--connection-key (vec)
  "Generate a connection key for VEC.
Includes the effective hop chain, including hidden TRAMP ad-hoc proxy hops, so
that different multi-hop routes to the same host produce distinct connections.
Hidden and explicit rpc+sudo spellings for the same route share a key, while a
direct root rpc connection does not collide with sudo over an unprivileged rpc
hop."
  (list :method (tramp-rpc--hop-component-string (tramp-file-name-method vec))
        :host (tramp-rpc--hop-component-string (tramp-file-name-host vec))
        :user (tramp-rpc--hop-component-string (tramp-file-name-user vec))
        :port (or (tramp-rpc--port-to-string (tramp-file-name-port vec)) "22")
        :route (mapcar (lambda (hop)
                         (tramp-rpc--connection-key-route-hop (cdr hop)))
                       (tramp-rpc--hop-pairs vec))))

(defun tramp-rpc--get-connection (vec)
  "Get the RPC connection for VEC, or nil if not connected."
  (gethash (tramp-rpc--connection-key vec) tramp-rpc--connections))

(defun tramp-rpc--set-connection (vec process buffer &optional stderr-buffer)
  "Store the RPC connection for VEC.
Caches keyed only by the TRAMP connection spelling are invalidated before a
new generation is made visible."
  (tramp-rpc--clear-direnv-cache vec)
  (tramp-rpc--clear-file-caches-for-connection vec)
  (tramp-rpc-magit--clear-cache)
  (puthash (tramp-rpc--connection-key vec)
           (list :process process :buffer buffer :stderr-buffer stderr-buffer
                 :generation process)
           tramp-rpc--connections))

(defun tramp-rpc--remove-connection (vec &optional process)
  "Remove VEC's connection, optionally only when PROCESS is current.
Also clears the exec-path and login shell caches."
  (let* ((key (tramp-rpc--connection-key vec))
         (current (gethash key tramp-rpc--connections)))
    (when (and current
               (or (null process) (eq process (plist-get current :process))))
      (remhash key tramp-rpc--connections)
      (remhash key tramp-rpc--exec-path-cache)
      (remhash key tramp-rpc--login-shell-cache))))

(defun tramp-rpc--connection-error-response (vec event)
  "Return an RPC error response for a transport failure on VEC."
  (list :error
        (list :code -32098
              :type 'remote-file-error
              :message (format "RPC transport closed for %s%s"
                               (tramp-file-name-host vec)
                               (if event (format " (%s)" (string-trim event)) "")))))

(defun tramp-rpc--track-pending-request (process id)
  "Record synchronous request ID ID as pending on PROCESS."
  (process-put process :tramp-rpc-pending-ids
               (cons id (delete id (process-get process :tramp-rpc-pending-ids)))))

(defun tramp-rpc--untrack-pending-request (process id)
  "Forget synchronous request ID ID on PROCESS."
  (when (processp process)
    (process-put process :tramp-rpc-pending-ids
                 (delete id (process-get process :tramp-rpc-pending-ids)))))

(defun tramp-rpc--maybe-clear-pending-response-state (process buffer)
  "Discard BUFFER's pending-response table when PROCESS has no requests."
  (when (and (processp process)
             (null (process-get process :tramp-rpc-pending-ids)))
    (remhash buffer tramp-rpc--pending-responses)))

(defun tramp-rpc--release-pending-requests (process buffer ids)
  "Untrack IDS on PROCESS and remove their buffered responses from BUFFER.
This is idempotent so it can run from every synchronous wait exit path."
  (dolist (id ids)
    (tramp-rpc--untrack-pending-request process id)
    (when-let* ((pending (gethash buffer tramp-rpc--pending-responses)))
      (remhash id pending)))
  (tramp-rpc--maybe-clear-pending-response-state process buffer))

(defmacro tramp-rpc--with-pending-requests (spec &rest body)
  "Run BODY, releasing unresolved request IDS on every exit."
  (declare (indent 1) (debug t))
  (let ((process (nth 0 spec))
        (buffer (nth 1 spec))
        (ids (nth 2 spec)))
    `(unwind-protect
         (progn ,@body)
       (tramp-rpc--release-pending-requests ,process ,buffer ,ids))))

(defun tramp-rpc--cleanup-connection-generation
    (process vec event reason &optional remote-cleanup)
  "Clean up one PROCESS generation for VEC.
REASON and EVENT are retained on PROCESS and used for both explicit disconnect
and unexpected transport death.  REMOTE-CLEANUP requests termination before
local relay deletion when PROCESS is still live."
  (when (and (processp process)
             (not (process-get process :tramp-rpc-transport-cleaned)))
    (process-put process :tramp-rpc-transport-cleaned t)
    (process-put process :tramp-rpc-transport-dead t)
    (process-put process :tramp-rpc-cleanup-reason reason)
    (process-put process :tramp-rpc-cleanup-event event)
    (let* ((conn (tramp-rpc--get-connection vec))
           (current-p (or (null conn)
                          (eq process (plist-get conn :process))))
           (generation-buffer (or (process-get process :tramp-rpc-buffer)
                                  (and current-p conn
                                       (plist-get conn :buffer))))
           (error-response (tramp-rpc--connection-error-response vec event))
           callbacks)
      ;; Wake synchronous callers before removing any generation state.
      (dolist (id (process-get process :tramp-rpc-pending-ids))
        (when (buffer-live-p generation-buffer)
          (puthash id error-response
                   (tramp-rpc--get-pending-responses generation-buffer))))
      ;; Detach callbacks before invoking them, so callback code cannot observe
      ;; or recreate a callback belonging to this dead generation.
      (maphash
       (lambda (id callback-process)
         (when (eq callback-process process)
           (when-let* ((callback (gethash id tramp-rpc--async-callbacks)))
             (push callback callbacks))
           (remhash id tramp-rpc--async-callbacks)
           (remhash id tramp-rpc--async-callback-processes)))
       tramp-rpc--async-callback-processes)
      ;; These caches are keyed only by the connection spelling.  Clear the old
      ;; generation while it is current, before relay sentinels or callbacks can
      ;; reconnect and populate replacement-generation entries.
      (when current-p
        (tramp-rpc--clear-direnv-cache vec)
        (tramp-rpc--clear-file-caches-for-connection vec)
        (tramp-rpc-magit--clear-cache))
      ;; These functions keep local relays tracked through delete-process so
      ;; their wrapped sentinels can preserve the user's sentinel.
      (tramp-rpc--cleanup-async-processes vec process remote-cleanup)
      (tramp-rpc--cleanup-pty-processes vec process remote-cleanup)
      (tramp-rpc--cleanup-watches-for-connection vec process)
      (tramp-rpc--cleanup-file-notify-for-connection vec process)
      (when current-p
        (tramp-rpc--remove-connection vec process))
      (dolist (callback callbacks)
        (condition-case callback-error
            (funcall callback error-response)
          (error
           (tramp-rpc--debug "transport cleanup callback failed: %S"
                             callback-error))))
      ;; Keep injected errors and the request IDs until active waiters consume
      ;; them.  Their unwind-protect cleanup removes the generation state.
      (tramp-rpc--maybe-clear-pending-response-state process generation-buffer)
      ;; Explicit disconnect owns transport deletion; unexpected death is
      ;; already dispatched by Emacs and this is harmlessly idempotent.
      (when (and remote-cleanup (process-live-p process))
        (delete-process process)))))

(defun tramp-rpc--connection-transport-death (process vec event)
  "Clean up unexpected death of PROCESS generation for VEC."
  (tramp-rpc--cleanup-connection-generation
   process vec event :transport-death nil))

(defun tramp-rpc--install-connection-sentinel (process vec)
  "Install the transport sentinel on PROCESS for VEC's generation."
  (unless (process-get process :tramp-rpc-connection-sentinel-installed)
    (process-put process :tramp-rpc-connection-sentinel-installed t)
    (let ((sentinel (process-sentinel process)))
      (set-process-sentinel
       process
       (lambda (proc event)
         (when sentinel
           (funcall sentinel proc event))
         (when (memq (process-status proc) '(exit signal))
           (tramp-rpc--connection-transport-death proc vec event)))))))

(defun tramp-rpc--ensure-connection (vec)
  "Ensure we have an active RPC connection to VEC.
Returns the connection plist.
When `non-essential' is non-nil and no live connection exists,
throws `non-essential' instead of opening a new connection.
This prevents background operations (timers, fontification,
completion) from blocking on unreachable hosts."
  (let ((conn (tramp-rpc--get-connection vec)))
    (if (and conn
             (process-live-p (plist-get conn :process))
             (buffer-live-p (plist-get conn :buffer)))
        conn
      ;; Stale connection - remove it before reconnecting
      (when conn
        (tramp-rpc--remove-connection vec))
      ;; During non-essential operations, don't open new connections.
      ;; This mirrors the (unless (tramp-connectable-p vec)
      ;; (throw 'non-essential 'non-essential)) pattern used by every
      ;; standard TRAMP backend in their maybe-open-connection functions.
      (unless (tramp-connectable-p vec)
        (throw 'non-essential 'non-essential))
      ;; Need to establish connection
      (tramp-rpc--connect vec))))

(defun tramp-rpc--ensure-controlmaster-directory ()
  "Ensure the ControlMaster socket directory exists.
Creates the directory from `tramp-rpc-controlmaster-path' if needed."
  (when tramp-rpc-use-controlmaster
    (let* ((path (expand-file-name tramp-rpc-controlmaster-path))
           (dir (file-name-directory path)))
      (when (and dir (not (file-directory-p dir)))
        (make-directory dir t)
        ;; Set restrictive permissions for security
        (set-file-modes dir #o700)))))

;;; ============================================================================
;;; Authentication via tramp-process-actions
;;; ============================================================================

;; Reuse upstream TRAMP's `tramp-process-actions' state machine for all
;; interactive authentication (SSH passwords, sudo, host-key prompts,
;; OTP, security keys).  This gives us auth-source integration, password
;; caching, wrong-password detection, and locale-aware prompt matching
;; for free, instead of reimplementing with a custom regexp + loop.

(defvar tramp-rpc--controlmaster-socket-path nil
  "Dynamically bound socket path during ControlMaster establishment.
Used by `tramp-rpc--action-controlmaster-established'.")

(defun tramp-rpc--action-controlmaster-established (proc _vec)
  "Succeed when the ControlMaster socket file appears, fail on process death.
The target socket path is read from the dynamic variable
`tramp-rpc--controlmaster-socket-path'."
  (cond
   ((file-exists-p tramp-rpc--controlmaster-socket-path)
    (throw 'tramp-action 'ok))
   ((not (process-live-p proc))
    (while (tramp-accept-process-output proc))
    (throw 'tramp-action 'process-died))))

(defconst tramp-rpc--controlmaster-actions
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-yesno-prompt-regexp tramp-action-yesno)
    (tramp-yn-prompt-regexp tramp-action-yn)
    (tramp-process-alive-regexp tramp-rpc--action-controlmaster-established))
  "Actions for SSH ControlMaster establishment.
Handles password prompts, host-key verification, and detects the
ControlMaster socket file appearing as the success condition.")

;;; ============================================================================
;;; Multi-hop support
;;; ============================================================================

(defun tramp-rpc--hops-to-proxyjump (vec)
  "Convert VEC's hop chain to an SSH ProxyJump (-J) string.
Parses the TRAMP hop field (for example, `rpc:user@gateway|') and converts
each hop to the SSH ProxyJump format (for example, `user@gateway').
Returns nil if there are no hops.

For sudo-via-RPC paths, the same-host rpc hop carrying the SSH details is
not a ProxyJump; other same-host rpc hops are preserved.  Supports mixed
methods: both `rpc:' and `ssh:' hops are accepted since ProxyJump only
needs host connectivity."
  (when-let* ((hop-pairs (tramp-rpc--hop-pairs vec)))
    (let ((sudo-hop (tramp-rpc--same-host-rpc-hop vec 'return-string))
          proxy-parts)
      (dolist (hop hop-pairs)
        (let* ((hop-vec (cdr hop)))
          ;; Skip only the rpc hop that represents sudo-via-RPC.
          (unless (and sudo-hop (tramp-rpc--same-hop-p (cdr hop) (cdr sudo-hop)))
            (push (concat
                   (when (tramp-file-name-user hop-vec)
                     (concat (tramp-file-name-user hop-vec) "@"))
                   (tramp-file-name-host hop-vec)
                   (when-let* ((port (tramp-rpc--port-to-string
                                      (tramp-file-name-port hop-vec))))
                     (concat ":" port)))
                  proxy-parts))))
      (when proxy-parts
        (mapconcat #'identity (nreverse proxy-parts) ",")))))

(defun tramp-rpc--controlmaster-socket-path (vec)
  "Return the ControlMaster socket path for VEC.
Expands SSH escape sequences in `tramp-rpc-controlmaster-path'.
For sudo-via-RPC paths, uses the SSH user and excludes the sudo
hop so the socket is shared with the normal rpc connection."
  (let* ((sudo-hop (tramp-rpc--sudo-rpc-hop-vec vec))
         (host (tramp-file-name-host vec))
         (user (or (and sudo-hop
                         (or (tramp-file-name-user sudo-hop)
                             (user-login-name)))
                   (tramp-file-name-user vec)
                   (user-login-name)))
         (port (or (tramp-rpc--port-to-string
                    (if sudo-hop
                        (tramp-file-name-port sudo-hop)
                      (tramp-file-name-port vec)))
                   "22"))
         ;; For sudo, use only proxy hops (exclude the same-host sudo hop)
         (hop (if sudo-hop
                  (tramp-rpc--proxy-hop-string vec)
                (tramp-file-name-hop vec)))
         (path tramp-rpc-controlmaster-path))
    ;; Expand common SSH escape sequences
    ;; %h = host, %r = remote user, %p = port
    ;; %C = hash of %l%h%p%r (we approximate this)
    (setq path (replace-regexp-in-string "%h" host path t t))
    (setq path (replace-regexp-in-string "%r" user path t t))
    (setq path (replace-regexp-in-string "%p" port path t t))
    ;; For %C, use a simple hash approximation
    ;; Include the hop chain so different multi-hop routes get different sockets
    (setq path (replace-regexp-in-string
                "%C"
                (md5 (format "%s%s%s%s%s" (system-name) host port user
                             (or hop "")))
                path t t))
    (expand-file-name path)))

(defun tramp-rpc--controlmaster-active-p (vec)
  "Return non-nil if a ControlMaster connection is active for VEC."
  (let* ((socket-path (tramp-rpc--controlmaster-socket-path vec))
         (host (tramp-file-name-host vec))
         (user (tramp-rpc--ssh-detail-user vec))
         (port (tramp-rpc--port-to-string
                (tramp-rpc--ssh-detail-port vec)))
         (proxyjump (tramp-rpc--hops-to-proxyjump vec)))
    (and (file-exists-p socket-path)
         ;; Check if the socket is actually usable via ssh -O check
         (zerop (apply #'call-process "ssh" nil nil nil
                       (append
                        (when user (list "-l" user))
                        (when port (list "-p" port))
                        (when proxyjump (list "-J" proxyjump))
                        (list "-o" (format "ControlPath=%s" socket-path)
                              "-O" "check"
                              host)))))))

(cl-defun tramp-rpc--establish-controlmaster (vec)
  "Establish a ControlMaster connection for VEC.
This creates an interactive SSH connection (without BatchMode) that can
prompt for passwords if needed, then keeps it running as a ControlMaster.
Subsequent BatchMode connections reuse this socket.
Returns non-nil on success."
  ;; Check if already connected
  (when (tramp-rpc--controlmaster-active-p vec)
    (tramp-rpc--debug "ControlMaster already active for %s" (tramp-file-name-host vec))
    (cl-return-from tramp-rpc--establish-controlmaster t))
  (tramp-rpc--ensure-controlmaster-directory)
  (let* ((host (tramp-file-name-host vec))
         (user (tramp-rpc--ssh-detail-user vec))
         (port (tramp-rpc--port-to-string
                (tramp-rpc--ssh-detail-port vec)))
         (proxyjump (tramp-rpc--hops-to-proxyjump vec))
         (socket-path (tramp-rpc--controlmaster-socket-path vec))
         (process-name (format "*tramp-rpc-auth %s*" host))
         (buffer (get-buffer-create (format " *tramp-rpc-auth %s*" host)))
         (ssh-args (append
                    (list "ssh")
                    tramp-rpc-ssh-args
                    (when user (list "-l" user))
                    (when port (list "-p" port))
                    ;; Multi-hop via ProxyJump
                    (when proxyjump (list "-J" proxyjump))
                    ;; NO BatchMode - allow password prompts
                    (list "-o" "StrictHostKeyChecking=accept-new")
                    ;; ControlMaster options
                    (list "-o" "ControlMaster=yes"
                          "-o" (format "ControlPath=%s" socket-path)
                          "-o" (format "ControlPersist=%s"
                                       tramp-rpc-controlmaster-persist))
                    ;; Connect and immediately exit, leaving ControlMaster running
                    (list "-N" host)))
         process)
    ;; If the socket file exists but `tramp-rpc--controlmaster-active-p' did
    ;; not accept it, it is stale.  OpenSSH exits immediately when asked to
    ;; create a ControlMaster on top of a stale ControlPath, which later shows
    ;; up as a generic "Tramp failed to connect" during unrelated file ops.
    (when (file-exists-p socket-path)
      (ignore-errors (delete-file socket-path)))
    (with-current-buffer buffer
      (erase-buffer))
    ;; Start SSH with PTY for interactive password prompt
    (let ((process-connection-type t))  ; Use PTY for password prompts
      (setq process (apply #'start-process process-name buffer ssh-args)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process #'ignore)
    ;; Set up process properties for tramp-process-actions / tramp-read-passwd.
    ;; pw-vector tells auth-source where to look up credentials.
    (process-put process 'tramp-vector vec)
    (tramp-set-connection-property process "hop-vector" vec)
    (tramp-set-connection-property
     process "pw-vector"
     (make-tramp-file-name :method "ssh" :user user :host host :port port))
    ;; Use upstream tramp-process-actions for password/host-key handling.
    ;; The custom action checks for the ControlMaster socket appearing.
    (let ((tramp-rpc--controlmaster-socket-path socket-path))
      (tramp-process-actions process vec nil
                             tramp-rpc--controlmaster-actions 60))
    ;; tramp-process-actions throws on failure; reaching here means success.
    (sleep-for 0.1)
    t))

(defun tramp-rpc--start-server-process (vec binary-path &optional sudo-password)
  "Start the RPC server on VEC at BINARY-PATH and verify it responds.
BINARY-PATH is the remote localname of the server binary (may contain ~).
For sudo-via-RPC paths, the server is started via sudo.  If SUDO-PASSWORD
is non-nil, feed it to sudo -S before the RPC server starts reading stdin.
Returns the connection plist.  Signals `remote-file-error' on failure."
  (let* ((sudo-ssh-user (tramp-rpc--detect-sudo-elevation vec))
         (host (tramp-file-name-host vec))
         (user (or sudo-ssh-user (tramp-file-name-user vec)))
         (port (tramp-rpc--port-to-string
                (tramp-rpc--ssh-detail-port vec)))
         (proxyjump (tramp-rpc--hops-to-proxyjump vec))
         ;; Build SSH command to run the RPC server
         (ssh-args (append
                    (list "ssh")
                    ;; Raw SSH arguments (e.g., -v, -F config)
                    tramp-rpc-ssh-args
                    (when user (list "-l" user))
                    (when port (list "-p" port))
                    ;; Multi-hop via ProxyJump
                    (when proxyjump (list "-J" proxyjump))
                    ;; Only use BatchMode=yes when ControlMaster handles auth;
                    ;; without it, BatchMode=yes prevents password prompts.
                    (when tramp-rpc-use-controlmaster
                      (list "-o" "BatchMode=yes"))
                    (list "-o" "StrictHostKeyChecking=accept-new")
                    ;; User-specified SSH options
                    (mapcan (lambda (opt) (list "-o" opt))
                            tramp-rpc-ssh-options)
                    ;; ControlMaster options for connection sharing
                    ;; Use the expanded socket path to match what establish-controlmaster created
                    (when tramp-rpc-use-controlmaster
                      (list "-o" "ControlMaster=auto"
                            "-o" (format "ControlPath=%s"
                                         (tramp-rpc--controlmaster-socket-path vec))
                            "-o" (format "ControlPersist=%s"
                                         tramp-rpc-controlmaster-persist)))
                    ;; For sudo elevation, wrap the binary in sudo.  This is
                    ;; the RPC analogue of TRAMP's native sudo method: open an
                    ;; elevated long-lived backend connection, then run process
                    ;; operations inside that connection.
                    (if sudo-ssh-user
                        (append
                         (list host "sudo")
                         ;; Do not use an empty sudo prompt here.  OpenSSH
                         ;; builds the remote command from argv and drops the
                         ;; empty argument, making sudo parse "-u" as the
                         ;; prompt string instead of the user option.
                         (if sudo-password
                             '("-k" "-S" "-p" "Password:")
                           '("-n"))
                         ;; Match TRAMP's sudo method: run with the target
                         ;; user's home environment rather than preserving the
                         ;; SSH user's HOME.
                         (list "-u" (tramp-file-name-user vec) "-H"
                               binary-path))
                      (list host binary-path))))
         ;; Use TRAMP's standard naming so tramp-get-connection-process works
         (process-name (tramp-get-connection-name vec))
         (buffer-name (tramp-buffer-name vec))
         (buffer (get-buffer-create buffer-name))
         (stderr-buffer (get-buffer-create (concat buffer-name " stderr")))
         process)

    ;; Clear buffers.  The main buffer must be unibyte for binary MessagePack
    ;; framing.  Keep SSH stderr in a separate buffer: OpenSSH diagnostics (for
    ;; example ControlMaster mux messages) are not part of the RPC protocol, and
    ;; if they are mixed into the stdout buffer the length-prefixed reader loses
    ;; framing and every call waits for the full RPC timeout.
    (with-current-buffer buffer
      (erase-buffer)
      (set-buffer-multibyte nil)
      (set-marker (mark-marker) (point-min)))
    (with-current-buffer stderr-buffer
      (erase-buffer))

    ;; Start the process with pipe connection (not PTY).  PTYs have line
    ;; buffering and ~4KB line length limits that break large MessagePack
    ;; requests.  Use `make-process' rather than `start-process' so local SSH
    ;; stderr can be separated from the binary stdout protocol stream.
    (setq process
          (make-process
           :name process-name
           :buffer buffer
           :command ssh-args
           :connection-type 'pipe
           :coding 'binary
           :noquery t
           :stderr stderr-buffer
           :filter #'tramp-rpc--connection-filter))

    ;; If sudo is reading the password from stdin, send it before the RPC
    ;; server starts.  sudo consumes this line; after exec, the same pipe
    ;; carries MessagePack-RPC frames to the server.
    (when sudo-password
      (process-send-string process (concat sudo-password "\n")))

    ;; Store connection
    (tramp-rpc--set-connection vec process buffer stderr-buffer)

    ;; Store vec on the process so notifications can identify the connection
    ;; and install the generation sentinel before the first RPC can be sent.
    (process-put process :tramp-rpc-vec vec)
    (process-put process :tramp-rpc-buffer buffer)
    (process-put process 'tramp-vector vec)
    (tramp-rpc--install-connection-sentinel process vec)

    ;; Wait for server to be ready by sending a ping, and seed the
    ;; connection-local system.info cache for later uid/gid/home/shell lookups.
    (let ((response (tramp-rpc--cache-system-info
                     vec (tramp-rpc--call vec "system.info" nil))))
      (unless response
        (tramp-rpc--remove-connection vec)
        (signal 'remote-file-error (list "Failed to connect to RPC server on" host))))

    ;; Set connection-local variables in the connection buffer.
    ;; Every TRAMP backend must call this after establishing the connection
    ;; so that connection-local variable profiles (registered via
    ;; `connection-local-set-profiles') are applied.  This enables variables
    ;; like `tramp-direct-async-process', `shell-file-name', `path-separator'
    ;; etc. to take effect in the connection buffer.
    (tramp-set-connection-local-variables vec)

    ;; Mark as connected for TRAMP's connectivity checks (used by projectile, etc.)
    (tramp-set-connection-property process "connected" t)

    ;; Mark as connected on the vec so `tramp-list-connections' finds
    ;; this connection and `tramp-cleanup-connection' can offer it
    ;; interactively.  The value is the connection buffer, matching the
    ;; convention in `tramp-get-buffer'.
    ;; Emacs 30.x uses "process-buffer"; newer TRAMP (31+) uses " connected".
    ;; Set both for compatibility.
    (tramp-set-connection-property vec "process-buffer" buffer)
    (tramp-set-connection-property vec " connected" buffer)

    (tramp-rpc--get-connection vec)))

(defun tramp-rpc--cleanup-failed-connection (vec)
  "Clean up a failed connection attempt for VEC.
Kills the process if still alive and removes the connection entry."
  (let ((conn (tramp-rpc--get-connection vec)))
    (when conn
      (let ((proc (plist-get conn :process)))
        (when (process-live-p proc)
          (delete-process proc)))
      (tramp-rpc--remove-connection vec))))

(defun tramp-rpc--cleanup-bootstrap-connection (vec)
  "Close the scpx/scp bootstrap connection for VEC if it exists.
The bootstrap connection is only needed during deploy and should be
closed afterward to prevent other packages (vc, diff-hl) from
accidentally routing file operations through tramp-sh."
  (let* ((bootstrap-vec (tramp-rpc-deploy--bootstrap-vec vec))
         (proc (tramp-get-connection-process bootstrap-vec)))
    (when (and proc (process-live-p proc))
      (delete-process proc))))

(defun tramp-rpc--connect (vec)
  "Establish an RPC connection to VEC."
  ;; Ensure ControlMaster directory exists
  (tramp-rpc--ensure-controlmaster-directory)
  ;; When ControlMaster is enabled, establish it first.
  ;; This handles both key-based and password authentication:
  ;; - Key-based: connects silently
  ;; - Password: prompts user, then subsequent connections reuse it
  (when tramp-rpc-use-controlmaster
    (condition-case err
        (tramp-rpc--establish-controlmaster vec)
      (remote-file-error
       ;; A stale ControlMaster socket can make OpenSSH exit immediately while
       ;; TRAMP reports only a generic connection failure.  Remove the socket
       ;; and retry once before surfacing the error.
       (let ((socket-path (tramp-rpc--controlmaster-socket-path vec)))
         (when (file-exists-p socket-path)
           (ignore-errors (delete-file socket-path)))
         (sleep-for 0.1)
         (condition-case nil
             (tramp-rpc--establish-controlmaster vec)
           (remote-file-error
            (signal (car err) (cdr err))))))))
  (let* ((sudo-ssh-user (tramp-rpc--detect-sudo-elevation vec))
         ;; TRAMP's sudo method opens an elevated backend connection.  For the
         ;; RPC backend that means starting the server via sudo.  Prefer sudo
         ;; -n when a ticket is already valid; otherwise read the password with
         ;; TRAMP's auth machinery and feed it to sudo -S during server start.
         (sudo-password (when (and sudo-ssh-user
                                   (tramp-rpc--sudo-password-required-p vec))
                          (tramp-rpc--sudo-read-password vec sudo-ssh-user))))
    (if tramp-rpc-deploy-never-deploy
      ;; Never-deploy mode: use the configured path directly, no fallback.
      (let ((binary-path (tramp-rpc-deploy-ensure-binary vec)))
        (condition-case err
            (tramp-rpc--start-server-process vec binary-path sudo-password)
          (remote-file-error
           (tramp-rpc--cleanup-failed-connection vec)
           (signal 'remote-file-error
                   (list (format
			  "tramp-rpc-server not found at \"%s\" on %s (never-deploy is set, no deployment attempted). Set `tramp-rpc-deploy-remote-binary-path' to the correct path. Original error: %s"
                          binary-path (tramp-file-name-host vec)
                          (error-message-string err)))))))
    ;; Normal mode: try expected path first, deploy on failure.
    ;; This avoids opening a bootstrap (scpx) connection just to run
    ;; `test -x binary', which takes ~6s for tramp-sh to establish the
    ;; shell.  If the binary exists (the common case after first deploy),
    ;; this connects directly.  If it doesn't exist (first time or after
    ;; version bump), SSH exits immediately, we catch the error, deploy
    ;; via scpx, and retry.
    (condition-case nil
        (tramp-rpc--start-server-process
         vec (tramp-rpc-deploy-expected-binary-localname) sudo-password)
      (remote-file-error
       ;; Connection failed - binary likely missing.  Clean up and deploy.
       (tramp-rpc--cleanup-failed-connection vec)
       (let ((binary-path (tramp-rpc-deploy-ensure-binary vec)))
         ;; Close the bootstrap connection - it's no longer needed and
         ;; leaving it alive can cause vc/diff-hl sentinels to route
         ;; file operations through tramp-sh instead of tramp-rpc.
         (tramp-rpc--cleanup-bootstrap-connection vec)
         (tramp-rpc--start-server-process vec binary-path sudo-password)))))))

(defun tramp-rpc--disconnect (vec)
  "Disconnect the RPC connection to VEC explicitly."
  (when-let* ((conn (tramp-rpc--get-connection vec))
              (connection-process (plist-get conn :process)))
    ;; Marking this state suppresses recursive sentinel work, but the common
    ;; cleanup core still wakes waiters and fails callbacks before removal.
    (process-put connection-process :tramp-rpc-disconnecting t)
    (tramp-rpc--cleanup-connection-generation
     connection-process vec "explicit disconnect\n" :explicit-disconnect t))
  ;; Flush TRAMP caches so a reconnect gets fresh data (home dir, uid, etc.).
  (tramp-flush-directory-properties vec "/")
  (tramp-flush-connection-properties vec))

(defun tramp-rpc--cleanup-controlmaster (vec)
  "Clean up the ControlMaster process and socket for VEC.
Sends an SSH -O exit command to gracefully close the ControlMaster
socket, then kills the auth process and buffer."
  (when tramp-rpc-use-controlmaster
    (let* ((host (tramp-file-name-host vec))
           (user (tramp-rpc--ssh-detail-user vec))
           (port (tramp-rpc--port-to-string
                  (tramp-rpc--ssh-detail-port vec)))
           (proxyjump (tramp-rpc--hops-to-proxyjump vec))
           (socket-path (tramp-rpc--controlmaster-socket-path vec))
           (auth-process-name (format "*tramp-rpc-auth %s*" host))
           (auth-buffer-name (format " *tramp-rpc-auth %s*" host))
           (auth-process (get-process auth-process-name))
           (auth-buffer (get-buffer auth-buffer-name)))
      ;; Close the ControlMaster socket gracefully via ssh -O exit.
      ;; This is a local control message (no network round-trip), so fast.
      (when (file-exists-p socket-path)
        (ignore-errors
          (apply #'call-process "ssh" nil nil nil
                 (append
                  (when user (list "-l" user))
                  (when port (list "-p" port))
                  (when proxyjump (list "-J" proxyjump))
                  (list "-o" (format "ControlPath=%s" socket-path)
                        "-O" "exit" host)))))
      ;; Kill the auth process.
      (when (and auth-process (process-live-p auth-process))
        (delete-process auth-process))
      ;; Kill the auth buffer.
      (when (buffer-live-p auth-buffer)
        (kill-buffer auth-buffer)))))

;; ============================================================================
;; RPC communication
;; ============================================================================

(defun tramp-rpc--connection-filter (process output)
  "Filter for RPC connection PROCESS receiving OUTPUT.
Handles async responses by dispatching to registered callbacks.
Uses length-prefixed binary framing: <4-byte BE length><msgpack payload>."
  (let ((buffer (process-buffer process))
	response)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Append output to buffer
        (goto-char (point-max))
        (insert output)
        (tramp-rpc--debug "FILTER received %d bytes, buffer-size=%d"
                          (length output) (buffer-size))
        ;; Process complete messages
        (goto-char (point-min))
        (let ((tramp-rpc-protocol--message-target process))
          (while (setq response (tramp-rpc-protocol-try-read-message buffer))
            ;; Replace buffer contents with remaining data
            (delete-region (point-min) (mark-marker))
            (goto-char (point-min))
            ;; Check for server-initiated notification (no id, has method)
            (if (plist-get response :notification)
                (tramp-rpc--handle-notification
                 process
                 (plist-get response :method)
                 (plist-get response :params))
              ;; A cleaned generation may still receive buffered output.  Its
              ;; injected transport-death errors belong to live waiters and
              ;; must not be overwritten by those late normal responses.
              (unless (or (process-get process :tramp-rpc-transport-cleaned)
                          (process-get process :tramp-rpc-transport-dead))
                (let* ((id (plist-get response :id))
                       (callback (gethash id tramp-rpc--async-callbacks)))
                  (if callback
                      (progn
                        (tramp-rpc--debug "FILTER dispatching async id=%s" id)
                        (remhash id tramp-rpc--async-callbacks)
                        (remhash id tramp-rpc--async-callback-processes)
                        (funcall callback response))
                    ;; Store only responses for this transport's live waiters.
                    ;; Late responses from an abandoned generation are discarded.
                    (when (member id (process-get process :tramp-rpc-pending-ids))
                      (tramp-rpc--debug "FILTER storing sync response id=%s" id)
                      (puthash id response
                               (tramp-rpc--get-pending-responses
                                (process-buffer process))))))))))))))

(defun tramp-rpc--call-async (vec method params callback &optional connection)
  "Call METHOD with PARAMS asynchronously on the RPC server for VEC.
CALLBACK is called with the response plist when it arrives.  CONNECTION, when
non-nil, is the captured connection generation to use.
Returns the request ID."
  (let* ((conn (or connection (tramp-rpc--ensure-connection vec)))
         (process (plist-get conn :process))
         (id-and-request (let ((tramp-rpc-protocol--message-target process))
                           (tramp-rpc-protocol-encode-request-with-id
                            method params)))
         (id (car id-and-request))
         (request (cdr id-and-request)))
    (tramp-rpc--debug "SEND-ASYNC id=%s method=%s" id method)
    ;; Register callback with its exact transport generation.
    (puthash id callback tramp-rpc--async-callbacks)
    (puthash id process tramp-rpc--async-callback-processes)
    ;; Send request (binary data with length prefix, no newline)
    (process-send-string process request)
    id))

(defun tramp-rpc--call (vec method params &optional connection)
  "Call METHOD with PARAMS on the RPC server for VEC.
CONNECTION, when non-nil, is the captured connection generation to use.
Returns the result or signals an error."
  (tramp-rpc--call-with-timeout vec method params 30 0.1 connection))

(defun tramp-rpc--call-fast (vec method params)
  "Call METHOD with PARAMS with shorter timeout for low-latency ops.
Returns the result or signals an error.
Uses 5s total timeout with 10ms polling."
  (tramp-rpc--call-with-timeout vec method params 5 0.01))

(defun tramp-rpc--find-response-by-id (expected-id &optional process)
  "Check pending responses for EXPECTED-ID.
Returns the response plist if found and removes it from pending, nil otherwise.
PROCESS identifies the transport generation owning the request."
  (let* ((buffer (current-buffer))
         (owner-process (or process (get-buffer-process buffer)))
         (pending (gethash buffer tramp-rpc--pending-responses))
         (response (and pending (gethash expected-id pending))))
    (when response
      (tramp-rpc--release-pending-requests
       owner-process buffer (list expected-id))
      response)))

(defun tramp-rpc--process-accessible-p (process)
  "Return t if PROCESS can be accessed from the current thread.
Returns nil if the process is locked to a different thread."
  (let ((locked-thread (process-thread process)))
    (or (null locked-thread)
        (eq locked-thread (current-thread)))))

(defun tramp-rpc--drain-connection-stderr (conn)
  "Drain pending stderr output for CONN's SSH process.
`make-process' with `:stderr' creates a separate stderr process.  The RPC
wait loops accept output with JUST-THIS-ONE for the stdout protocol process,
so explicitly service stderr as well to prevent the stderr pipe from filling
and blocking SSH or the remote server."
  (when-let* ((stderr-buffer (plist-get conn :stderr-buffer))
              ((buffer-live-p stderr-buffer))
              (stderr-process (get-buffer-process stderr-buffer))
              ((tramp-rpc--process-accessible-p stderr-process)))
    (while (accept-process-output stderr-process 0 nil t))))

(defun tramp-rpc--connection-stderr-tail (conn &optional max-bytes)
  "Return a diagnostic tail from CONN's stderr buffer, or nil."
  (when-let* ((stderr-buffer (plist-get conn :stderr-buffer))
              ((buffer-live-p stderr-buffer)))
    (with-current-buffer stderr-buffer
      (when (> (buffer-size) 0)
        (buffer-substring-no-properties
         (max (point-min) (- (point-max) (or max-bytes 1024)))
         (point-max))))))

(defun tramp-rpc--call-with-timeout (vec method params total-timeout poll-interval
                                          &optional connection)
  "Call METHOD with PARAMS on the RPC server for VEC.
TOTAL-TIMEOUT is maximum seconds to wait.
POLL-INTERVAL is seconds between accept-process-output checks.
CONNECTION, when non-nil, is the captured connection generation to use.
Returns the result or signals an error."
  (let* ((conn (or connection (tramp-rpc--ensure-connection vec)))
         (process (plist-get conn :process))
         (buffer (plist-get conn :buffer))
         (id-and-request (let ((tramp-rpc-protocol--message-target process))
                           (tramp-rpc-protocol-encode-request-with-id
                            method params)))
         (expected-id (car id-and-request))
         (request (cdr id-and-request)))

    (tramp-rpc--debug "SEND id=%s method=%s" expected-id method)
    (tramp-rpc--track-pending-request process expected-id)

    (tramp-rpc--with-pending-requests (process buffer (list expected-id))
      ;; Send request (binary data with length prefix, no newline)
      (process-send-string process request)

      ;; Wait for response with matching ID using wall-clock deadline.
    ;; NOTE: We use (float-time) instead of decrementing a counter because
    ;; accept-process-output can return early (e.g. async process output
    ;; arrives), and decrementing by poll-interval each iteration would
    ;; cause premature timeouts when there is concurrent I/O traffic.
    (with-current-buffer buffer
      (let ((start-time (float-time))
            (deadline (+ (float-time) total-timeout))
            response)
        ;; A transport sentinel may have injected an error before the loop.
        (setq response (tramp-rpc--find-response-by-id expected-id process))
        ;; Wait for a response with the correct ID
        (while (and (not response)
                    (< (float-time) deadline)
                    (process-live-p process))
          ;; Check if process is locked to another thread before trying to accept
          (if (not (tramp-rpc--process-accessible-p process))
              (progn
                ;; Process locked - if non-essential, bail out; otherwise sleep and retry
                (if non-essential
                    (progn
                      (tramp-rpc--debug "LOCKED id=%s method=%s (non-essential, bailing)"
                                       expected-id method)
                      (throw 'non-essential 'non-essential))
                  ;; Sleep briefly - other thread may receive our response
                  (sleep-for poll-interval)
                  ;; Check if other thread already got our response
                  (setq response (tramp-rpc--find-response-by-id expected-id process))))
            ;; Process is accessible - proceed with accept-process-output
            ;; Use same pattern as tramp-accept-process-output:
            ;; - poll-interval timeout to avoid spinning
            ;; - JUST-THIS-ONE=t to only accept from this process (Bug#12145)
            ;; - with-local-quit to allow C-g, returns t on success
            ;; - Propagate quit if user pressed C-g
            ;; - with-tramp-suspended-timers to prevent deferred process
            ;;   sentinels (scheduled via run-at-time 0) from firing
            ;;   inside accept-process-output and blocking this call.
            ;;   The sentinels will run when control returns to the
            ;;   command loop.  (Mirrors tramp-accept-process-output.)
            (if (with-tramp-suspended-timers
                  (with-local-quit
                    (tramp-rpc--drain-connection-stderr conn)
                    (accept-process-output process poll-interval nil t)
                    (tramp-rpc--drain-connection-stderr conn)
                    t))
                ;; Check if our response arrived in pending responses
                (setq response (tramp-rpc--find-response-by-id expected-id process))
              ;; User quit - propagate it
              (tramp-rpc--debug "QUIT id=%s (user interrupted)" expected-id)
              (keyboard-quit))))

        (unless response
          (if (or (process-get process :tramp-rpc-transport-dead)
                  (not (process-live-p process)))
              (signal 'remote-file-error
                      (list (format "RPC transport disconnected from %s"
                                    (tramp-file-name-host vec))))
            (let ((elapsed (- (float-time) start-time))
                  (stderr-tail (tramp-rpc--connection-stderr-tail conn)))
              (tramp-rpc--debug
               "TIMEOUT id=%s method=%s elapsed=%.1fs buffer-size=%d process-live=%s stderr-tail=%S"
               expected-id method elapsed (buffer-size) (process-live-p process)
               stderr-tail)
              (signal
               'remote-file-error
               (list (concat
                      (format
                       "Timeout waiting for RPC response from %s (id=%s, method=%s, waited %.1fs)"
                       (tramp-file-name-host vec) expected-id method elapsed)
                      (when stderr-tail
                        (format "; SSH stderr: %s" stderr-tail))))))))

        (tramp-rpc--debug "RECV id=%s (found)" expected-id)
        (if (tramp-rpc-protocol-error-p response)
            (let ((code (tramp-rpc-protocol-error-code response))
                  (msg (tramp-rpc-protocol-error-message response))
                  (data (tramp-rpc-protocol-error-data response))
                  (os-errno (tramp-rpc-protocol-error-errno response)))
              (tramp-rpc--debug "ERROR id=%s code=%s msg=%s errno=%s"
                               expected-id code msg os-errno)
              (tramp-rpc--signal-rpc-error "RPC" msg code os-errno nil data))
          (plist-get response :result)))))))

(defun tramp-rpc--call-batch (vec requests)
  "Execute multiple RPC REQUESTS in a single round-trip for VEC.
REQUESTS is a list of (METHOD . PARAMS) cons cells.
Returns a list of results (or error plists) in the same order.

Example:
  (tramp-rpc--call-batch vec
    \\='((\"file.exists\" . ((path . \"/foo\")))
      (\"file.stat\" . ((path . \"/bar\")))
      (\"process.run\" . ((cmd . \"git\") (args . [\"status\"])))))

Returns:
  (t                          ; file.exists result
   ((type . \"file\") ...)    ; file.stat result
   (:error -32001 :message \"...\"))  ; or error plist"
  (let* ((conn (tramp-rpc--ensure-connection vec))
         (process (plist-get conn :process))
         (buffer (plist-get conn :buffer))
         (id-and-request (let ((tramp-rpc-protocol--message-target process))
                           (tramp-rpc-protocol-encode-batch-request-with-id
                            requests)))
         (expected-id (car id-and-request))
         (request (cdr id-and-request)))

    (tramp-rpc--debug "SEND-BATCH id=%s count=%d" expected-id (length requests))
    (tramp-rpc--track-pending-request process expected-id)

    (tramp-rpc--with-pending-requests (process buffer (list expected-id))
      ;; Send batch request (binary data with length prefix, no newline)
      (process-send-string process request)

      ;; Wait for response with matching ID using wall-clock deadline
    (with-current-buffer buffer
      (let ((start-time (float-time))
            (deadline (+ (float-time) 30))
            response)
        ;; A transport sentinel may have injected an error before the loop.
        (setq response (tramp-rpc--find-response-by-id expected-id process))
        (while (and (not response)
                    (< (float-time) deadline)
                    (process-live-p process))
          ;; Check if process is locked to another thread before trying to accept
          (if (not (tramp-rpc--process-accessible-p process))
              ;; Process locked - if non-essential, bail out; otherwise sleep and retry
              (if non-essential
                  (progn
                    (tramp-rpc--debug "LOCKED-BATCH id=%s (non-essential, bailing)" expected-id)
                    (throw 'non-essential 'non-essential))
                ;; Sleep briefly - other thread may receive our response
                (sleep-for 0.1)
                ;; Check if other thread already got our response
                (setq response (tramp-rpc--find-response-by-id expected-id process)))
            ;; Process is accessible
            (if (with-tramp-suspended-timers
                  (with-local-quit
                    (tramp-rpc--drain-connection-stderr conn)
                    (accept-process-output process 0.1 nil t)
                    (tramp-rpc--drain-connection-stderr conn)
                    t))
                ;; Check if our response arrived in pending responses
                (setq response (tramp-rpc--find-response-by-id expected-id process))
              (tramp-rpc--debug "QUIT-BATCH id=%s (user interrupted)" expected-id)
              (keyboard-quit))))

        (unless response
          (if (or (process-get process :tramp-rpc-transport-dead)
                  (not (process-live-p process)))
              (signal 'remote-file-error
                      (list (format "RPC transport disconnected from %s"
                                    (tramp-file-name-host vec))))
            (let ((elapsed (- (float-time) start-time))
                  (stderr-tail (tramp-rpc--connection-stderr-tail conn)))
              (tramp-rpc--debug
               "TIMEOUT-BATCH id=%s elapsed=%.1fs buffer-size=%d process-live=%s stderr-tail=%S"
               expected-id elapsed (buffer-size) (process-live-p process)
               stderr-tail)
              (signal
               'remote-file-error
               (list (concat
                      (format
                       "Timeout waiting for batch RPC response from %s (id=%s, waited %.1fs)"
                       (tramp-file-name-host vec) expected-id elapsed)
                      (when stderr-tail
                        (format "; SSH stderr: %s" stderr-tail))))))))

        (tramp-rpc--debug "RECV-BATCH id=%s (found)" expected-id)
        (if (tramp-rpc-protocol-error-p response)
            (progn
              (tramp-rpc--debug "ERROR-BATCH id=%s msg=%s"
                               expected-id (tramp-rpc-protocol-error-message response))
              (signal
	       'remote-file-error
	       (list "Batch RPC error"
                     (tramp-rpc-protocol-error-message response))))
          (tramp-rpc-protocol-decode-batch-response response)))))))

;; ============================================================================
;; Request pipelining support
;; ============================================================================

(defun tramp-rpc--send-requests (vec requests &optional connection)
  "Send multiple REQUESTS to the RPC server for VEC without waiting.
REQUESTS is a list of (METHOD . PARAMS) cons cells.
CONNECTION, when non-nil, is the captured connection generation to use.
Returns a list of request IDs in the same order."
  (let* ((conn (or connection (tramp-rpc--ensure-connection vec)))
         (process (plist-get conn :process))
         (buffer (plist-get conn :buffer))
         ids completed)
    (unwind-protect
        (progn
          (dolist (req requests)
            (let* ((id-and-bytes (let ((tramp-rpc-protocol--message-target process))
                                   (tramp-rpc-protocol-encode-request-with-id
                                    (car req) (cdr req))))
                   (id (car id-and-bytes))
                   (bytes (cdr id-and-bytes)))
              (tramp-rpc--debug "SEND-PIPE id=%s method=%s" id (car req))
              (push id ids)
              (tramp-rpc--track-pending-request process id)
              ;; Send binary data with length prefix, no newline
              (process-send-string process bytes)))
          (setq completed t)
          (nreverse ids))
      (unless completed
        (tramp-rpc--release-pending-requests process buffer ids)))))

(defun tramp-rpc--receive-responses (vec ids &optional timeout connection)
  "Receive responses for request IDS from the RPC server for VEC.
Returns an alist mapping each ID to its response plist.
TIMEOUT is the maximum time to wait in seconds (default 30).
CONNECTION, when non-nil, is the captured connection generation to use."
  (let* ((conn (or connection (tramp-rpc--ensure-connection vec)))
         (process (plist-get conn :process))
         (buffer (plist-get conn :buffer))
         (deadline (+ (float-time) (or timeout 30)))
         (remaining-ids (copy-sequence ids))
         (responses (make-hash-table :test 'eql)))
    (tramp-rpc--debug "RECV-PIPE waiting for %d responses: %S" (length ids) ids)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            ;; Consume transport-death errors before checking process status.
            (dolist (id remaining-ids)
              (let ((response (tramp-rpc--find-response-by-id id process)))
                (when response
                  (puthash id response responses)
                  (setq remaining-ids (delete id remaining-ids)))))
            (while (and remaining-ids
                  (< (float-time) deadline)
                  (process-live-p process))
        ;; Check if process is locked to another thread before trying to accept
        (if (not (tramp-rpc--process-accessible-p process))
            ;; Process locked - if non-essential, bail out; otherwise sleep and retry
            (if non-essential
                (progn
                  (tramp-rpc--debug "LOCKED-PIPE (non-essential, bailing)")
                  (throw 'non-essential 'non-essential))
              ;; Sleep briefly - other thread may receive our responses
              (sleep-for 0.1)
              ;; Check if other thread already got any of our responses
              (dolist (id remaining-ids)
                (let ((response (tramp-rpc--find-response-by-id id process)))
                  (when response
                    (tramp-rpc--debug "RECV-PIPE found id=%s (after sleep)" id)
                    (puthash id response responses)
                    (setq remaining-ids (delete id remaining-ids))))))
          ;; Process is accessible
          (if (with-tramp-suspended-timers
                (with-local-quit
                  (tramp-rpc--drain-connection-stderr conn)
                  (accept-process-output process 0.1 nil t)
                  (tramp-rpc--drain-connection-stderr conn)
                  t))
              ;; Check for each remaining ID in pending responses
              (dolist (id remaining-ids)
                (let ((response (tramp-rpc--find-response-by-id id process)))
                  (when response
                    (tramp-rpc--debug "RECV-PIPE found id=%s" id)
                    ;; Store response by ID
                    (puthash id response responses)
                    ;; Remove from remaining
                    (setq remaining-ids (delete id remaining-ids)))))
            (tramp-rpc--debug "RECV-PIPE quit (user interrupted)")
              (keyboard-quit)))))
          (when remaining-ids
            (tramp-rpc--debug "RECV-PIPE missing ids: %S" remaining-ids)
            (signal
             'remote-file-error
             (list (if (process-live-p process)
                       (format "Timeout waiting for pipelined RPC responses from %s (missing ids: %S)"
                               (tramp-file-name-host vec) remaining-ids)
                     (format "RPC process died waiting for pipelined responses from %s (missing ids: %S)"
                             (tramp-file-name-host vec) remaining-ids)))))
          ;; Convert hash table to alist in original order
          (mapcar (lambda (id)
                    (cons id (gethash id responses)))
                  ids))
      (tramp-rpc--release-pending-requests process buffer ids))))

(defun tramp-rpc--call-pipelined (vec requests)
  "Execute multiple REQUESTS in a pipelined fashion for VEC.
REQUESTS is a list of (METHOD . PARAMS) cons cells.
Returns a list of results in the same order as REQUESTS.
Each result is either the actual result or an error plist.

Unlike `tramp-rpc--call-batch', this sends each request as a separate
RPC call, allowing the server to process them concurrently.
This is more efficient when the server has async support."
  (let* ((connection (tramp-rpc--ensure-connection vec))
         (ids (tramp-rpc--send-requests vec requests connection))
         (responses (tramp-rpc--receive-responses vec ids nil connection)))
    ;; Process responses in order and extract results
    (mapcar (lambda (id-response)
              (let ((response (cdr id-response)))
                (if (tramp-rpc-protocol-error-p response)
                    (let ((code (tramp-rpc-protocol-error-code response))
                          (msg (tramp-rpc-protocol-error-message response)))
                      (list :error code :message msg))
                  (plist-get response :result))))
            responses)))

;; ============================================================================
;; Output decoding helper
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

(defun tramp-rpc--decode-output (data encoding)
  "Decode binary process DATA using ENCODING.
This helper is for synchronous command/file paths.  Async relays keep their
bytes raw and let the relay process decoder handle incremental output."
  (if data
      (let ((coding (cond
                     ((null encoding) 'utf-8-unix)
                     ((coding-system-p encoding) encoding)
                     ((stringp encoding)
                      (or (coding-system-from-name encoding)
                          'utf-8-unix))
                     (t 'utf-8-unix))))
        (decode-coding-string (tramp-rpc--binary-bytes data) coding))
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

(defun tramp-rpc--encode-path (path)
  "Encode PATH for transmission to path-or-bytes server parameters.
Returns an alist with PATH as an explicit MessagePack bin value."
  `((path . ,(tramp-rpc--path-to-bin path))))

;; ============================================================================
;; File name handler operations
;; ============================================================================

(defun tramp-rpc--mode-executable-p (mode-string remote-uid remote-gid attrs groups)
  "Return non-nil when MODE-STRING permits the remote user to execute ATTRS."
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
`tramp-handle-file-readable-p' does)."
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
  (let* ((cache-key (tramp-rpc--file-stat-cache-key vec localname lstat))
         (cached (tramp-rpc--cache-lookup tramp-rpc--file-stat-cache cache-key)))
    (if (not (eq cached 'not-cached))
        cached
      (let ((params (append (tramp-rpc--encode-path localname)
                            (when lstat '((lstat . t))))))
        (condition-case err
            (let ((stat (tramp-rpc--call vec "file.stat" params)))
              (tramp-rpc--cache-file-stat-result vec localname stat lstat)
              stat)
          (file-missing
           (tramp-rpc--cache-file-stat-result vec localname nil lstat)
           nil)
          (file-error
           ;; Return nil for ELOOP (symlink loop) and ENOTDIR (path component
           ;; is not a directory, e.g. "file.py/.editorconfig") - the file
           ;; can't be resolved, so it effectively doesn't exist for stat purposes.
           (let ((message (error-message-string err)))
             (if (or (string-match-p "Too many levels of symbolic links" message)
                     (string-match-p "Not a directory" message))
                 (progn
                   (tramp-rpc--cache-file-stat-result vec localname nil lstat)
                   nil)
               (signal (car err) (cdr err))))))))))

(defun tramp-rpc--file-exists-cache-lookup (filename)
  "Return cached `file-exists-p' value for FILENAME, or `not-cached'.
Unlike `tramp-rpc--cache-get', this preserves cached nil values."
  (let* ((expanded (expand-file-name filename))
         (entry (gethash expanded tramp-rpc--file-exists-cache)))
    (if (not entry)
        'not-cached
      (let ((timestamp (car entry))
            (value (cdr entry)))
        (if (< (- (float-time) timestamp) tramp-rpc--cache-ttl)
            value
          (remhash expanded tramp-rpc--file-exists-cache)
          'not-cached)))))

(defun tramp-rpc-handle-file-exists-p (filename)
  "Like `file-exists-p' for TRAMP-RPC files.
Uses TRAMP-RPC caches and Magit ancestor scan data before falling back to a
single `file.stat' RPC.  This avoids latency-amplified marker scans from
Projectile, project.el, and editorconfig during Magit section expansion."
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
           (pcase (tramp-rpc-magit--file-exists-p filename)
             ('not-cached
              (pcase (tramp-rpc--file-exists-cache-lookup filename)
                ('not-cached
                 (let* ((stat (tramp-rpc--call-file-stat v localname))
                        (exists (if stat t nil)))
                   (tramp-rpc--cache-put tramp-rpc--file-exists-cache
                                         (expand-file-name filename)
                                         exists)
                   exists))
                (cached cached)))
             (cached cached)))))))))

(defun tramp-rpc-handle-file-readable-p (filename)
  "Like `file-readable-p' for TRAMP-RPC files.
For cached-missing marker files, avoid delegating to TRAMP's generic handler,
which would otherwise perform another remote stat."
  (pcase (tramp-rpc-magit--file-exists-p filename)
    ('nil nil)
    (_ (tramp-handle-file-readable-p filename))))

(defun tramp-rpc-handle-file-regular-p (filename)
  "Like `file-regular-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-regular-p"
      (when-let* ((stat (tramp-rpc--call-file-stat v localname)))
        (equal (alist-get 'type stat) "file")))))

(defun tramp-rpc-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for TRAMP-RPC files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (when-let* ((stat (tramp-rpc--call-file-stat v localname t))
                ((equal (alist-get 'type stat) "symlink")))
      (tramp-rpc--decode-string (alist-get 'link_target stat)))))

(defun tramp-rpc-handle-access-file (filename string)
  "Like `access-file' for TRAMP-RPC files."
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
path unchanged (after resolving any symlinks in parent directories)."
  (let* ((expanded (expand-file-name filename))
         (cached (tramp-rpc--cache-get tramp-rpc--file-truename-cache
                                      expanded)))
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
          (tramp-rpc--cache-put tramp-rpc--file-truename-cache expanded truename)
          truename))))


(defun tramp-rpc-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for TRAMP-RPC files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property
        v localname (format "file-attributes-%s" id-format)
      (let ((result (tramp-rpc--call-file-stat v localname t)))  ; lstat=t
        ;; Populate file-exists cache as side effect when lstat is definitive
        ;; for follow semantics.  For symlinks, lstat success does not tell us
        ;; whether the target exists (dangling symlink), so leave file-exists-p
        ;; uncached.
        (unless (and result (equal (alist-get 'type result) "symlink"))
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
which resolves truename and then stats."
  (or
   ;; Preserve TRAMP's completion-time fast path semantics.
   (tramp-string-empty-or-nil-p (tramp-file-local-name filename))
   (string-equal (tramp-file-local-name filename) "/")
   (with-parsed-tramp-file-name (expand-file-name filename) nil
     (with-tramp-file-property v localname "file-directory-p"
       (let ((stat (tramp-rpc--call-file-stat v localname)))
         (and stat (equal (alist-get 'type stat) "directory")))))))

(defun tramp-rpc--convert-file-attributes (stat id-format)
  "Convert STAT result to Emacs file-attributes format.
ID-FORMAT specifies whether to use numeric or string IDs."
  (let* ((type-str (alist-get 'type stat))
         (type (pcase type-str
                 ("file" nil)
                 ("directory" t)
                 ("symlink" (tramp-rpc--decode-string (alist-get 'link_target stat)))
                 (_ nil)))
         (nlinks (alist-get 'nlinks stat))
         (uid (alist-get 'uid stat))
         (gid (alist-get 'gid stat))
         (uname (tramp-rpc--decode-string (alist-get 'uname stat)))
         (gname (tramp-rpc--decode-string (alist-get 'gname stat)))
         (atime (seconds-to-time (alist-get 'atime stat)))
         (mtime (seconds-to-time (alist-get 'mtime stat)))
         (ctime (seconds-to-time (alist-get 'ctime stat)))
         (size (alist-get 'size stat))
         (mode (tramp-file-mode-from-int (alist-get 'mode stat)))
         (inode (alist-get 'inode stat))
         (dev (alist-get 'dev stat)))
    ;; Return in file-attributes format
    (list type nlinks
          (if (eq id-format 'string) (or uname (number-to-string uid)) uid)
          (if (eq id-format 'string) (or gname (number-to-string gid)) gid)
          atime mtime ctime
          size mode nil inode dev)))




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
  "Like `set-file-modes' for TRAMP-RPC files."
  (tramp-rpc--with-set-file-attributes-rpc filename
    (tramp-rpc--call v "file.set_modes"
                     (append (tramp-rpc--encode-path localname)
                             `((mode . ,mode))))))

(defun tramp-rpc-handle-set-file-times (filename &optional timestamp flag)
  "Like `set-file-times' for TRAMP-RPC files."
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
Return readable dir-locals files in DIRECTORY in increasing priority order."
  (with-parsed-tramp-file-name
      (if (file-name-absolute-p directory)
          directory
        (file-name-concat default-directory directory))
      nil
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
to the built-in implementation."
  (if (functionp name)
      (tramp-run-real-handler #'locate-dominating-file (list file name))
    (with-parsed-tramp-file-name
        (if (file-name-absolute-p file)
            file
          (file-name-concat default-directory file))
        nil
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
  (with-parsed-tramp-file-name
      (if (file-name-absolute-p file)
          file
        (file-name-concat default-directory file))
      nil
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
  "Like `dir-locals-find-file' for TRAMP-RPC files."
  (let* ((file (if (file-name-absolute-p file)
                   file
                 (file-name-concat default-directory file)))
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
so a single RPC can both validate and list the directory."
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
  "Like `directory-files-and-attributes' for TRAMP-RPC files."
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

;; Declared in Tramp 2.8.1.3+; forward-declare so byte compiler treats it as dynamic.
(defvar tramp-fnac-add-trailing-slash)

(defun tramp-rpc-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for TRAMP-RPC files."
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
network round-trip."
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
  "Like `delete-directory' for TRAMP-RPC files."
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
  "Like `write-region' for TRAMP-RPC files."
  (tramp-skeleton-write-region
      start end filename append visit lockname mustbenew
    ;; If START is a string, write it directly; otherwise extract from buffer.
    ;; When APPEND is an integer, it is a file offset for writing.
    (let* ((content (if (stringp start)
                        start
                      (buffer-substring-no-properties
                       (or start (point-min))
                       (or end (point-max)))))
           ;; Encode using buffer's coding system or default to utf-8
           (coding (or (and (not (stringp start))
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

(defun tramp-rpc--batch-error-p (value)
  "Return non-nil when VALUE is an error object from `tramp-rpc--call-batch'."
  (and (consp value) (plist-get value :error)))

(defun tramp-rpc--batch-error-errno (error)
  "Return OS errno from batched RPC ERROR, or nil."
  (when-let* ((data (plist-get error :data)))
    (alist-get 'os_errno data)))

(defun tramp-rpc--error-args (operation detail message filename &optional data)
  "Build signal args for OPERATION, DETAIL, MESSAGE, FILENAME, and DATA."
  (append
   (cond
    ((and filename detail) (list operation detail filename message))
    (filename (list operation filename message))
    (detail (list operation detail message))
    (t (list operation message)))
   (when data (list data))))

(defun tramp-rpc--signal-rpc-error
    (operation message code os-errno &optional filename data)
  "Signal RPC error CODE/OS-ERRNO and optional structured DATA for OPERATION."
  (cond
   ((= code tramp-rpc-protocol-error-file-not-found)
    (signal 'file-missing
            (tramp-rpc--error-args operation "No such file" message filename data)))
   ((= code tramp-rpc-protocol-error-permission-denied)
    (signal 'permission-denied
            (tramp-rpc--error-args operation "Permission denied" message filename data)))
   ;; process.run can report ENOENT for its executable or its cwd.  Only the
   ;; server's explicit marker represents command-not-found status 127.
   ((and (= code tramp-rpc-protocol-error-process)
         (eq (alist-get 'spawn_not_found data) t))
    (signal 'file-missing
            (tramp-rpc--error-args operation "No such file" message filename data)))
   ((and (not (= code tramp-rpc-protocol-error-process))
         (eql os-errno 2)) ; ENOENT
    (signal 'file-missing
            (tramp-rpc--error-args operation "No such file" message filename data)))
   ((eql os-errno 17) ; EEXIST
    (signal 'file-already-exists
            (tramp-rpc--error-args operation nil message filename data)))
   ((eql os-errno 39) ; ENOTEMPTY
    (signal 'file-error
            (tramp-rpc--error-args operation "Directory not empty" message filename data)))
   ((eql os-errno 20) ; ENOTDIR
    (signal 'file-error
            (tramp-rpc--error-args operation "Not a directory" message filename data)))
   ((eql os-errno 21) ; EISDIR
    (signal 'file-error
            (tramp-rpc--error-args operation "Is a directory" message filename data)))
   ((eql os-errno 40) ; ELOOP
    (signal 'file-error
            (tramp-rpc--error-args
             operation "Too many levels of symbolic links" message filename data)))
   ((= code tramp-rpc-protocol-error-io)
    (signal 'remote-file-error
            (tramp-rpc--error-args operation nil message filename data)))
   (t
    (signal 'remote-file-error
            (tramp-rpc--error-args operation nil message filename data)))))

(defun tramp-rpc--signal-batch-error (operation filename error)
  "Signal ERROR returned by a batched RPC for OPERATION on FILENAME."
  (tramp-rpc--signal-rpc-error
   operation
   (or (plist-get error :message) "RPC batch subrequest failed")
   (plist-get error :error)
   (tramp-rpc--batch-error-errno error)
   filename
   (plist-get error :data)))

(defun tramp-rpc--batch-result-or-signal (operation filename result)
  "Return batched RESULT, or signal its embedded error for FILENAME."
  (if (tramp-rpc--batch-error-p result)
      (tramp-rpc--signal-batch-error operation filename result)
    result))

(cl-defun tramp-rpc--copy-file-same-remote
    (filename newname ok-if-already-exists keep-time preserve-permissions)
  "Copy FILENAME to NEWNAME on one TRAMP-RPC remote with fewer round-trips."
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
             ok-if-already-exists keep-time preserve-permissions)))
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
                             (preserve . ,(if (or keep-time preserve-permissions)
                                              t :msgpack-false))
                             (overwrite . ,(if ok-if-already-exists
                                               t :msgpack-false))))))
        (tramp-flush-file-properties v1 v1-localname)
        (tramp-flush-file-properties v2 v2-localname)
        (tramp-flush-directory-properties v2 v2-localname)
        (tramp-rpc--invalidate-cache-for-path newname)))))

(cl-defun tramp-rpc-handle-copy-directory
    (dirname newname &optional keep-date parents copy-contents)
  "Like `copy-directory' for TRAMP-RPC files.

For same-remote directory copies, use the server-side recursive `file.copy'
operation.  That keeps the number of RPC round-trips constant instead of
walking the tree from Emacs and issuing one RPC per entry.  Lisp computes the
Emacs/TRAMP destination and policy details; the server only receives primitive
copy options.  Fall back to the generic TRAMP handler for cross-remote copies."
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
                (prog1
                    (tramp-rpc--call
                     v2 "file.make_symlink"
                     (append
                      `((target . ,(tramp-rpc--path-to-bin
                                    (or source-symlink-target
                                        (tramp-rpc--decode-string
                                         (alist-get 'link_target source-lstat))))))
                      (mapcar (lambda (param)
                                (pcase (car param)
                                  ('path (cons 'link_path (cdr param)))
                                  ('path_encoding (cons 'link_path_encoding (cdr param)))
                                  (_ param)))
                              (tramp-rpc--encode-path symlink-dest-localname))))
                  (tramp-rpc-clear-all-caches))
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
                    (tramp-handle-copy-directory
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
              (tramp-rpc--invalidate-cache-for-subtree
               (tramp-make-tramp-file-name v2 (file-name-quote copied-localname)))))))
    (tramp-handle-copy-directory dirname newname keep-date parents copy-contents)))

(cl-defun tramp-rpc-handle-copy-file
    (filename newname &optional ok-if-already-exists keep-time
              preserve-uid-gid preserve-permissions)
  "Like `copy-file' for TRAMP-RPC files."
  (setq filename (expand-file-name filename)
        newname (expand-file-name newname))
  ;; Fast path for same-remote copies: batch source/destination stats, then do
  ;; the server-side copy.  This avoids the generic preflight predicates each
  ;; costing their own network round-trip.
  (when (and (tramp-tramp-file-p filename)
             (tramp-tramp-file-p newname)
             (tramp-equal-remote filename newname))
    (cl-return-from tramp-rpc-handle-copy-file
      (tramp-rpc--copy-file-same-remote
       filename newname ok-if-already-exists keep-time preserve-permissions)))
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
  "Rename FILENAME to NEWNAME on one TRAMP-RPC remote with fewer round-trips."
  (with-parsed-tramp-file-name filename v1
    (with-parsed-tramp-file-name newname v2
      (let* ((stats (tramp-rpc--call-batch
                     v1
                     `(("file.stat" . ,(append (tramp-rpc--encode-path v1-localname)
                                                '((lstat . t))))
                       ("file.stat" . ,(append (tramp-rpc--encode-path v2-localname)
                                                '((lstat . t)))))))
             (source-stat (nth 0 stats))
             (dest-stat (nth 1 stats))
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
  "Like `rename-file' for TRAMP-RPC files."
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
as a no-op, so ignore the server's ENOENT mapping as well."
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
     (let ((result (tramp-rpc--call vec "file.read"
                                    (tramp-rpc--file-read-params localname t))))
       (tramp-rpc--write-local-trash-file
        dest (tramp-rpc--extract-file-read-content result) stat)))
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
      (let ((remaining (nreverse regulars)))
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
                 (when (tramp-rpc--batch-error-p result)
                   (tramp-rpc--signal-batch-error
                    "file.read" (file-name-concat localname (car item)) result))
                 (tramp-rpc--write-local-trash-file
                  (expand-file-name (car item) (file-name-as-directory dest))
                  (tramp-rpc--extract-file-read-content result)
                  (cadr item)))
               chunk reads))))))
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

(defun tramp-rpc--fallback-move-file-to-trash (filename)
  "Run the real `move-file-to-trash' for FILENAME without this external op."
  (let ((inhibit-file-name-handlers
         (cons #'tramp-rpc-file-name-handler inhibit-file-name-handlers))
        (inhibit-file-name-operation 'move-file-to-trash))
    (move-file-to-trash filename)))

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
implementation, which will use the normal TRAMP-RPC file handlers underneath."
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
  "Like `make-symbolic-link' for TRAMP-RPC files."
  (prog1
      (tramp-skeleton-make-symbolic-link target linkname ok-if-already-exists
        (let* ((target-path (file-name-unquote target))
               (link-path-params (tramp-rpc--encode-path localname))
               ;; Rename 'path' to 'link_path' in the encoded params.
               (params (mapcar (lambda (p)
                                  (cond
                                   ((eq (car p) 'path)
                                    (cons 'link_path (cdr p)))
                                   ((eq (car p) 'path_encoding)
                                    (cons 'link_path_encoding (cdr p)))
                                   (t p)))
                                link-path-params)))
          (tramp-rpc--call
           v "file.make_symlink"
           (append `((target . ,(tramp-rpc--path-to-bin target-path)))
                   params))))

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
ID-FORMAT specifies whether to return integer GIDs or string names."
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

(defun tramp-rpc--acl-enabled-p (vec)
  "Check if ACL is available on the remote host VEC.
Caches the result for efficiency."
  ;; Check if getfacl exists and works
  (condition-case nil
      (let ((result (tramp-rpc--call vec "process.run"
                                     `((cmd . "getfacl")
                                       (args . ["--version"])
                                       (cwd . "/")))))
        (zerop (alist-get 'exit_code result)))
    (error nil)))

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
                         (alist-get 'stdout result)
                         (alist-get 'stdout_encoding result))))
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
  "Check if SELinux is enabled on the remote host VEC."
  (condition-case nil
      (let ((result (tramp-rpc--call vec "process.run"
                                     `((cmd . "selinuxenabled")
                                       (args . [])
                                       (cwd . "/")))))
        (zerop (alist-get 'exit_code result)))
    (error nil)))

(defun tramp-rpc-handle-file-selinux-context (filename)
  "Like `file-selinux-context' for TRAMP-RPC files.
Returns a list of (USER ROLE TYPE RANGE), or (nil nil nil nil) if not available."
  (with-parsed-tramp-file-name (expand-file-name (file-name-unquote filename)) nil
    (let ((context '(nil nil nil nil)))
      (when (tramp-rpc--selinux-enabled-p v)
        (let ((result (tramp-rpc--call v "process.run"
                                       `((cmd . "ls")
                                         (args . ["-d" "-Z" ,localname])
                                         (cwd . "/")))))
          (when (zerop (alist-get 'exit_code result))
            (let ((output (tramp-rpc--decode-output
                           (alist-get 'stdout result)
                           (alist-get 'stdout_encoding result))))
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
;; Dired operations
;; ============================================================================

(defun tramp-rpc-handle-dired-compress-file (file)
  "Like `dired-compress-file' for TRAMP-RPC files."
  (tramp-run-real-handler #'dired-compress-file (list file)))

;; ============================================================================
;; Process operations
;; ============================================================================

(defun tramp-rpc-run-git-commands (directory commands)
  "Run multiple git COMMANDS in DIRECTORY using pipelined RPC.
COMMANDS is a list of lists, where each sublist is arguments to git.
For example: ((\"rev-parse\" \"HEAD\") (\"status\" \"--porcelain\"))

Returns a list of plists, each containing:
  :exit-code - the exit code of the command
  :stdout    - standard output as a string
  :stderr    - standard error as a string

This is much faster than running each command sequentially over TRAMP
because all commands are sent in a single network round-trip."
  (with-parsed-tramp-file-name directory nil
    (setq localname (file-name-unquote localname))
    (let* ((requests
            (mapcar (lambda (args)
                      (cons "process.run"
                            `((cmd . "git")
                              (args . ,(vconcat args))
                              (cwd . ,localname))))
                    commands))
           (results (tramp-rpc--call-pipelined v requests)))
      ;; Convert results to a more convenient format
      (mapcar (lambda (result)
                (if (plist-get result :error)
                    (list :exit-code -1
                          :stdout ""
                          :stderr (or (plist-get result :message) "RPC error"))
                  (list :exit-code (alist-get 'exit_code result)
                        :stdout (tramp-rpc--decode-output
                                 (alist-get 'stdout result)
                                 (alist-get 'stdout_encoding result))
                        :stderr (tramp-rpc--decode-output
                                 (alist-get 'stderr result)
                                 (alist-get 'stderr_encoding result)))))
              results))))

(defun tramp-rpc--route-process-file-output (destination stdout &optional stderr)
  "Route process-file STDOUT and STDERR according to DESTINATION.
DESTINATION follows the `process-file' convention:
  nil       - discard
  t         - insert into current buffer
  string    - write to file
  buffer    - insert into buffer
  (stdout-dest . stderr-dest) - cons for separate handling"
  (cond
   ((null destination) nil)
   ((eq destination t)
    (insert stdout))
   ((stringp destination)
    (with-temp-file destination
      (insert stdout)))
   ((bufferp destination)
    (with-current-buffer destination
      (insert stdout)))
   ((consp destination)
    (let ((stdout-dest (car destination))
          (stderr-dest (cadr destination)))
      (when stdout-dest
        (cond
         ((eq stdout-dest t) (insert stdout))
         ((stringp stdout-dest)
          (with-temp-file stdout-dest (insert stdout)))
         ((bufferp stdout-dest)
          (with-current-buffer stdout-dest (insert stdout)))))
      (when (and stderr-dest stderr)
        (cond
         ((stringp stderr-dest)
          (with-temp-file stderr-dest (insert stderr)))
         ((bufferp stderr-dest)
          (with-current-buffer stderr-dest (insert stderr)))))))))

(defun tramp-rpc--get-signal-strings (vec)
  "Strings to return by `process-file' in case of signals on VEC.
Runs `kill -l' on the remote host to get signal names, then maps
signal numbers to human-readable strings like \"Interrupt\" or
\"Signal 2\".  The result is cached per connection."
  (with-tramp-connection-property vec "rpc-signal-strings"
    (let* ((result (tramp-rpc--call vec "process.run"
                                    `((cmd . "/bin/sh")
                                      (args . ["-c" "kill -l"])
                                      (cwd . "/"))))
           (exit-code (alist-get 'exit_code result))
           (stdout (tramp-rpc--decode-output
                    (alist-get 'stdout result)
                    (alist-get 'stdout_encoding result)))
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
refresh), git commands are served from the prefetch cache when possible."
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
               ;; (or deprecated `tramp-rpc-remote-path'); direnv and dynamic
               ;; caller variables keep their previous roles and override it.
               (env (tramp-rpc--ensure-inside-emacs-env
                     (tramp-rpc--merge-environments
                      (tramp-rpc--remote-path-environment v)
                      (tramp-rpc--tramp-remote-process-environment)
                      (tramp-rpc--get-direnv-environment v localname)
                      (tramp-rpc--caller-environment))))
               (stdin-content (when (and infile (not (eq infile t)))
                                 (with-temp-buffer
                                   (set-buffer-multibyte nil)
                                   (insert-file-contents-literally infile)
                                   (buffer-string))))
               (result (condition-case nil
                           (tramp-rpc--call v "process.run"
                                            `((cmd . ,program)
                                              (args . ,(vconcat args))
                                              (cwd . ,localname)
                                              (env . ,env)
                                              ,@(when stdin-content
                                                  `((stdin . ,stdin-content)))))
                         ;; The server marks confirmed spawn ENOENT as
                         ;; `file-missing'; other RPC failures must propagate.
                         (file-missing 127))))
          (if (eq result 127)
              127
            (if result
              (let ((exit-code (alist-get 'exit_code result))
                    (stdout (tramp-rpc--decode-output
                             (alist-get 'stdout result)
                             (alist-get 'stdout_encoding result)))
                    (stderr (tramp-rpc--decode-output
                             (alist-get 'stderr result)
                             (alist-get 'stderr_encoding result))))

                ;; Memoize uncached Magit git calls made during lazy remote
                ;; status expansion, so repeated section washing queries don't
                ;; pay another round-trip.
                (when (null infile)
                  (tramp-rpc-magit--process-cache-store
                   program args exit-code stdout))

                ;; Preserve Magit's ordering for status refresh: let the real
                ;; `update-index --refresh' run, then build the read snapshot
                ;; that subsequent status/diff/log commands may reuse.
                (when (and (null infile)
                           (bound-and-true-p tramp-rpc-magit--allow-process-cache)
                           (or (string-suffix-p "/git" program)
                               (string= "git" program))
                           (= exit-code 0)
                           (tramp-rpc-magit--git-cache-safe-environment-p))
                  (let ((core-args (tramp-rpc-magit--strip-git-prefix-args args)))
                    (when (and (equal (car core-args) "update-index")
                               (member "--refresh" core-args))
                      (tramp-rpc-magit--clear-status-cache)
                      (tramp-rpc-magit--prefetch default-directory))))

                ;; Handle destination
                (tramp-rpc--route-process-file-output destination stdout stderr)

                ;; Invalidate caches if the program might modify the filesystem
                ;; and the directory isn't being watched (watched dirs get
                ;; server-pushed invalidation)
                (let ((program-name (file-name-nondirectory program)))
                  (unless (or (member program-name tramp-rpc--readonly-programs)
                              (tramp-rpc--directory-watched-p localname v))
                    (tramp-rpc--invalidate-cache-for-path default-directory)))

                ;; Handle signal strings: when
                ;; `process-file-return-signal-string' is non-nil and exit
                ;; code >= 128, return the signal name string instead.
                (if (and (bound-and-true-p process-file-return-signal-string)
                         (natnump exit-code) (>= exit-code 128))
                    (let ((strings (tramp-rpc--get-signal-strings v)))
                      (aref strings (- exit-code 128)))
                  exit-code))
            ;; A successful RPC result is always non-nil.
            (signal 'remote-file-error (list "Empty process.run response")))))))))

(defun tramp-rpc-handle-vc-registered (file)
  "Like `vc-registered' for TRAMP-RPC files.
Since tramp-rpc supports `process-file', VC backends can run their
commands (git, svn, hg) directly via RPC.

We set `default-directory' to the file's directory to ensure that
process-file calls from VC backends are routed through our tramp handler."
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
  "Return remote exec-path using RPC.
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

(defun tramp-rpc--effective-remote-path-spec (vec)
  "Return the remote PATH specification used by tramp-rpc on VEC.
Connection-local values are honored, matching `tramp-get-remote-path'."
  (condition-case nil
      (with-current-buffer (tramp-get-connection-buffer vec)
        (tramp-set-connection-local-variables vec)
        (copy-tree (or tramp-rpc-remote-path tramp-remote-path)))
    (error
     (copy-tree (or tramp-rpc-remote-path tramp-remote-path)))))

(defun tramp-rpc--append-path-entries (entries result)
  "Append string ENTRIES to RESULT, preserving order and removing duplicates."
  (dolist (dir entries result)
    (when (and (stringp dir)
               (not (string-empty-p dir))
               (not (member dir result)))
      (setq result (append result (list dir))))))

(defun tramp-rpc--expand-remote-path-entry (vec entry)
  "Expand one remote PATH ENTRY for VEC when necessary."
  (if (and (stringp entry)
           (string-match "\\`~\\([^/]*\\)\\(/.*\\)?\\'" entry))
      (let* ((user (match-string 1 entry))
             (suffix (or (match-string 2 entry) ""))
             (home (tramp-get-home-directory
                    vec (unless (string-empty-p user) user))))
        (concat (directory-file-name home) suffix))
    entry))

(defun tramp-rpc--fetch-default-remote-path (vec)
  "Fetch the POSIX default PATH for VEC, falling back to /bin:/usr/bin."
  (condition-case nil
      (let* ((result (tramp-rpc--call vec "process.run"
                                      `((cmd . "/bin/sh")
                                        (args . ["-c" "getconf PATH 2>/dev/null"])
                                        (cwd . "/"))))
             (exit-code (alist-get 'exit_code result))
             (stdout (tramp-rpc--decode-output
                      (alist-get 'stdout result)
                      (alist-get 'stdout_encoding result))))
        (if (and (eq exit-code 0) (> (length stdout) 0))
            (split-string (string-trim stdout) ":" t)
          '("/bin" "/usr/bin")))
    (error '("/bin" "/usr/bin"))))

(defun tramp-rpc--compute-remote-path (vec)
  "Compute remote exec-path for VEC from `tramp-remote-path'.
A non-nil deprecated `tramp-rpc-remote-path' overrides
`tramp-remote-path'.  Supports the standard TRAMP placeholders
`tramp-default-remote-path' and `tramp-own-remote-path'.  The old
`tramp-rpc-own-remote-path' placeholder is treated like
`tramp-own-remote-path'.  Duplicate, unsupported, and nonexistent
entries are removed."
  (let ((own-path nil)
        (default-path nil)
        (result nil))
    (dolist (entry (tramp-rpc--effective-remote-path-spec vec))
      (setq entry (tramp-rpc--expand-remote-path-entry vec entry))
      (cond
       ((eq entry 'tramp-default-remote-path)
        (unless default-path
          (setq default-path (tramp-rpc--fetch-default-remote-path vec)))
        (setq result (tramp-rpc--append-path-entries default-path result)))
       ((memq entry '(tramp-own-remote-path tramp-rpc-own-remote-path))
        (unless own-path
          (setq own-path (or (tramp-rpc--fetch-remote-exec-path vec) '())))
        (setq result (tramp-rpc--append-path-entries own-path result)))
       ((stringp entry)
        (setq result (tramp-rpc--append-path-entries (list entry) result)))
       (t
        (tramp-rpc--debug "Ignoring unsupported remote PATH entry: %S" entry))))
    ;; Remove non-existing directories (matches tramp-sh behavior).
    (delq nil (mapcar (lambda (x)
                        (and (stringp x)
                             (file-directory-p
                              (tramp-make-tramp-file-name vec x))
                             x))
                      result))))

(defun tramp-rpc--get-remote-login-shell (vec)
  "Return the login shell for the remote user on VEC.
Tries the `shell' field from system.info (populated via getpwuid on
the server).  Falls back to looking up the user via `getent passwd'
and extracting field 7.  Returns \"/bin/sh\" if all lookups fail.
Result is cached per connection."
  (let* ((key (tramp-rpc--connection-key vec))
         (cached (gethash key tramp-rpc--login-shell-cache)))
    (or cached
        (let ((shell
               (condition-case nil
                   (let* ((info (tramp-rpc--system-info vec))
                          (sh (alist-get 'shell info)))
                     (if (and sh (stringp sh) (> (length sh) 0))
                         sh
                       (tramp-rpc--get-remote-login-shell-via-getent vec)))
                 (error (tramp-rpc--get-remote-login-shell-via-getent vec)))))
          (puthash key shell tramp-rpc--login-shell-cache)
          shell))))

(defun tramp-rpc--get-remote-login-shell-via-getent (vec)
  "Look up the login shell for the remote user on VEC via getent.
Returns \"/bin/sh\" if the lookup fails."
  (condition-case nil
      (let* ((user (or (tramp-file-name-user vec) ""))
             ;; If no user in the vec, fall back to system.info user
             (target-user (if (string-empty-p user)
                              (tramp-rpc--decode-string
                               (alist-get 'user (tramp-rpc--system-info vec)))
                            user))
             (result (tramp-rpc--call vec "process.run"
                                       `((cmd . "getent")
                                         (args . ["passwd" ,target-user])
                                         (cwd . "/"))))
             (exit-code (alist-get 'exit_code result))
             (stdout (tramp-rpc--decode-output
                      (alist-get 'stdout result)
                      (alist-get 'stdout_encoding result))))
        (if (and (eq exit-code 0) (> (length stdout) 0))
            ;; getent passwd format: name:x:uid:gid:gecos:home:shell
            (let* ((fields (split-string (string-trim stdout) ":"))
                   (shell (and (>= (length fields) 7) (nth 6 fields))))
              (if (and shell (> (length shell) 0))
                  shell
                "/bin/sh"))
          "/bin/sh"))
    (error "/bin/sh")))

(defun tramp-rpc--fetch-remote-exec-path (vec)
  "Fetch the remote PATH from VEC using the user's login shell.
Invokes the login shell with `-l' to source shell configuration files.
A marker separates shell startup output, MOTD text, or banners from the
actual PATH line, matching the robustness of upstream TRAMP."
  (condition-case nil
      (let* ((marker (md5 (format "tramp-rpc-path-%s" (float-time))))
             (shell (tramp-rpc--get-remote-login-shell vec))
             (result (tramp-rpc--call vec "process.run"
                                       `((cmd . ,shell)
                                         (args . ["-l" "-c"
                                                  ,(format "echo %s; printenv PATH" marker)])
                                         (cwd . "/"))))
             (exit-code (alist-get 'exit_code result))
             (stdout (tramp-rpc--decode-output
                      (alist-get 'stdout result)
                      (alist-get 'stdout_encoding result))))
        (when (and (eq exit-code 0) (> (length stdout) 0)
                   (string-match
                    (concat (regexp-quote marker) "\r?\n\\([^\r\n]+\\)")
                    stdout))
          (split-string (string-trim (match-string 1 stdout)) ":" t)))
    (error nil)))

(defun tramp-rpc-handle-insert-file-contents
    (filename &optional visit beg end replace)
  "Like `insert-file-contents' for TRAMP-RPC files.
Reads directly through `file.read' instead of going through
`file-local-copy', avoiding the generic TRAMP temp-file path and its extra
round-trips for the common non-VISIT case."
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
        (let* ((params (tramp-rpc--file-read-params localname)))
          (when beg
            (push `(offset . ,beg) params))
          (when end
            (push `(length . ,(- end (or beg 0))) params))
          (let* ((rpc-result (tramp-rpc--call v "file.read" params))
                 (content (tramp-rpc--extract-file-read-content rpc-result))
                 (decoded-content
                  (if enable-multibyte-characters
                      (decode-coding-string
                       content (or coding-system-for-read 'undecided))
                    content)))
            (insert decoded-content)
            (setq result (list filename (- (point) start)))
            (goto-char start))))
      result)))

(defun tramp-rpc-handle-file-local-copy (filename)
  "Create a local copy of remote FILENAME using RPC."
  (tramp-skeleton-file-local-copy filename
    (let* ((params (tramp-rpc--file-read-params localname))
           (result (tramp-rpc--call v "file.read" params))
           (content (tramp-rpc--extract-file-read-content result)))
      (with-temp-file tmpfile
        (set-buffer-multibyte nil)
        (insert content)))))

(defconst tramp-rpc--system-info-property "tramp-rpc-system-info"
  "TRAMP connection property storing the cached system.info response.")

(defun tramp-rpc--cache-system-info (vec info)
  "Store system.info INFO for VEC and seed related TRAMP properties."
  (when info
    (tramp-set-connection-property vec tramp-rpc--system-info-property info)
    ;; Store remote uname so `tramp-check-remote-uname' works.  The server
    ;; returns "linux" or "macos"; map to the kernel names tramp-sh expects.
    (when-let* ((os (alist-get 'os info)))
      (tramp-set-connection-property
       vec "uname"
       (pcase os
         ("macos" "Darwin")
         ("linux" "Linux")
         (_ os))))
    ;; Match this backend's existing string uid/gid behavior: string format is
    ;; the numeric id rendered as text, not the login/group name.
    (when-let* ((uid (alist-get 'uid info)))
      (tramp-set-connection-property vec "uid-integer" uid)
      (tramp-set-connection-property vec "uid-string" (number-to-string uid)))
    (when-let* ((gid (alist-get 'gid info)))
      (tramp-set-connection-property vec "gid-integer" gid)
      (tramp-set-connection-property vec "gid-string" (number-to-string gid)))
    (when-let* ((home (tramp-rpc--decode-string (alist-get 'home info))))
      ;; `tramp-get-home-directory' caches under "~" when USER is nil and
      ;; under "~USER" when USER is explicit.  Seed both current-user forms.
      (tramp-set-connection-property vec "~" home)
      (when-let* ((user (tramp-file-name-user vec)))
        (unless (string-empty-p user)
          (tramp-set-connection-property vec (concat "~" user) home)))))
  info)

(defun tramp-rpc--system-info (vec)
  "Return cached system.info for VEC, fetching it at most once per connection."
  (or (tramp-get-connection-property vec tramp-rpc--system-info-property nil)
      (progn
        ;; Establishing a new connection already performs and caches a
        ;; `system.info' call as its readiness ping.  Re-check the property after
        ;; connection setup so a cold caller does not immediately send a second
        ;; identical RPC.
        (tramp-rpc--ensure-connection vec)
        (or (tramp-get-connection-property
             vec tramp-rpc--system-info-property nil)
            (tramp-rpc--cache-system-info
             vec (tramp-rpc--call vec "system.info" nil))))))

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
                      (alist-get 'stdout result)
                      (alist-get 'stdout_encoding result))))
        (when (and (eq exit-code 0) (> (length stdout) 0))
          ;; getent passwd format: name:x:uid:gid:gecos:home:shell
          (let ((fields (split-string (string-trim stdout) ":")))
            (when (>= (length fields) 6)
              (nth 5 fields))))))))

(defun tramp-rpc-handle-get-remote-uid (vec id-format)
  "Return remote UID using RPC."
  (let* ((result (tramp-rpc--system-info vec))
         (uid (alist-get 'uid result)))
    (if (eq id-format 'integer)
        uid
      (number-to-string uid))))

(defun tramp-rpc-handle-get-remote-gid (vec id-format)
  "Return remote GID using RPC."
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
would otherwise flood the echo area with \"Cannot expand tilde\"."
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
  (let* ((info (ignore-errors (tramp-rpc--system-info vec)))
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
  "Create a TRAMP-style process descriptor for a file notification watch."
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
         (let* ((watch-key (plist-get data :watch-key))
                (watch-entry (and watch-key
                                  (gethash watch-key
                                           tramp-rpc--file-notify-watch-counts)))
                (canonical-directory
                 (or (plist-get watch-entry :canonical-directory)
                     (plist-get data :canonical-directory)))
                (directory (plist-get data :directory))
                (relative (and canonical-directory directory
                               (tramp-rpc--file-notify-relative-name
                                canonical-directory file-name))))
           (when relative
             (let ((alias (if (string-empty-p relative)
                              (directory-file-name directory)
                            (expand-file-name relative directory))))
               (unless (string= alias file-name)
                 (cl-pushnew alias aliases :test #'string=))))))
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
we cannot derive one."
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

(defun tramp-rpc--file-notify-dispatch (action file-name &optional file-name1 cookie)
  "Dispatch a `file-notify' ACTION for TRAMP FILE-NAME.
FILE-NAME1 is the destination for `renamed' events.  COOKIE pairs
`renamed-from' and `renamed-to' events when the server provides one."
  (when (and (hash-table-p tramp-rpc--file-notify-descriptors)
             (> (hash-table-count tramp-rpc--file-notify-descriptors) 0)
             (tramp-rpc--file-notify-callback-action action))
    ;; `file-notify-callback' and the special-event handler live in
    ;; filenotify.el.  It is normally loaded before file notifications are
    ;; registered, but require it defensively before constructing events.
    (require 'filenotify)
    (let ((descriptors nil)
          (callback-action (tramp-rpc--file-notify-callback-action action)))
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
        (let* ((descriptor (car descriptor-data))
               (data (cdr descriptor-data))
               (display-file-name
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
            (funcall (lookup-key special-event-map [file-notify]) event)))))))

(defun tramp-rpc-handle-file-notify-add-watch (directory flags _callback)
  "Like `file-notify-add-watch' for TRAMP-RPC files.
DIRECTORY is the remote directory passed by `file-notify-add-watch'."
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
           (symlink-watch (ignore-errors (file-symlink-p directory)))
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
                                      (ignore-errors
                                        (file-truename directory))))))
          (puthash watch-key
                   (list :count 1
                         :owned (and (not preexisting) (not synthetic))
                         :synthetic synthetic
                         :directory directory
                         :canonical-directory canonical-directory
                         :connection-process (plist-get (tramp-rpc--get-connection v)
                                                        :process))
                   tramp-rpc--file-notify-watch-counts)))
      (let ((watch-entry (gethash watch-key tramp-rpc--file-notify-watch-counts)))
        (puthash descriptor
                 (list :directory directory
                       :canonical-directory (plist-get watch-entry
                                                       :canonical-directory)
                       :flags flags
                       :localname localname
                       :watch-key watch-key
                       :connection-process (plist-get (tramp-rpc--get-connection v)
                                                      :process))
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
;; Process and advice modules (extracted)
;; ============================================================================

(defvar tramp-rpc--delivering-output nil
  "Non-nil while delivering process output to the local relay.
Used by advice functions to bypass interception during output delivery.")

(defvar tramp-rpc--closing-local-relay nil
  "Non-nil while sending EOF to a local cat relay process.
Tells the `process-send-eof' advice to call the original function
instead of routing to the remote process.")

(defcustom tramp-rpc-async-read-timeout-ms 200
  "Timeout in milliseconds for async process reads.
The server will block for this long waiting for data before returning.
Lower values mean more responsive but higher CPU usage.
Also controls process exit detection latency."
  :type 'integer
  :group 'tramp-rpc)

;; Process support, advice functions, and magit integration are now in
;; separate modules for better organization and maintainability.
(require 'tramp-rpc-process)
;; Loading tramp-rpc-advice while this file is being byte-compiled can
;; recurse on some Emacs/TRAMP combinations.  Advice is still loaded at
;; runtime when `tramp-rpc' is required normally.
(unless (bound-and-true-p byte-compile-current-file)
  (require 'tramp-rpc-advice))
(require 'tramp-rpc-magit)

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
    (dired-compress-file . tramp-rpc-handle-dired-compress-file)
    (insert-directory . tramp-handle-insert-directory)
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

;; Defer registration until tramp-rpc is fully loaded so
;; `tramp-add-external-operation' can safely `(require 'tramp-rpc)'.
(with-eval-after-load 'tramp-rpc
  (tramp-add-external-operation 'locate-dominating-file 'tramp-rpc-handle-locate-dominating-file 'tramp-rpc)
  (tramp-add-external-operation 'dir-locals--all-files 'tramp-rpc-handle-dir-locals--all-files 'tramp-rpc)
  (tramp-add-external-operation 'dir-locals-find-file 'tramp-rpc-handle-dir-locals-find-file 'tramp-rpc)
  (tramp-add-external-operation 'move-file-to-trash 'tramp-rpc-handle-move-file-to-trash 'tramp-rpc 'file))

;;;###autoload
(defun tramp-rpc-file-name-handler (operation &rest args)
  "Invoke TRAMP-RPC file name handler for OPERATION with ARGS.
Falls back to the local handler when `non-essential' is non-nil and
a backend function throws `non-essential' (e.g. because no connection
exists and opening one would block).  This mirrors the catch/throw
pattern in `tramp-file-name-handler'."
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

;; `tramp-rpc-file-name-p' is defined as defsubst in the with-eval-after-load
;; block above (extracted into autoloads).  Re-define it here as defun for
;; the full-load case so it gets proper byte-compilation.
(defun tramp-rpc-file-name-p (vec-or-filename)
  "Check if VEC-OR-FILENAME is handled by TRAMP-RPC.
VEC-OR-FILENAME can be either a tramp-file-name struct or a filename string."
  (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename)))
    (string= (tramp-file-name-method vec) tramp-rpc-method)))

;; Re-register with the full defun now that the file is loaded.
;; (Already registered via with-eval-after-load, but this ensures the
;; byte-compiled defun version is used.)
(tramp-register-foreign-file-name-handler
 #'tramp-rpc-file-name-p #'tramp-rpc-file-name-handler)

;; ============================================================================
;; Connection cleanup support
;; ============================================================================

(defun tramp-rpc--managed-file-name-p (vec-or-filename)
  "Return non-nil when VEC-OR-FILENAME is managed by TRAMP-RPC."
  (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename)))
    (or (tramp-rpc-file-name-p vec)
        (tramp-rpc--sudo-file-name-p vec))))

(defun tramp-rpc-cleanup-connection (vec)
  "Clean up TRAMP-RPC resources for connection VEC.
This is called from `tramp-cleanup-connection-hook' after TRAMP's
generic cleanup has already run (passwords cleared, timers cancelled,
connection buffer killed, TRAMP caches flushed).

Handles RPC-specific state: the connection hash table, async/PTY
processes, file watches, ControlMaster process/socket, pending RPC
responses, and RPC-specific caches (direnv, executable, file-exists,
file-truename)."
  (when (tramp-rpc--managed-file-name-p vec)
    ;; Save buffer reference before disconnect removes the connection
    ;; entry.  The buffer is already killed by TRAMP's generic cleanup,
    ;; but we need the object to remove its pending-responses hash entry.
    (let ((conn-buffer (when-let* ((conn (tramp-rpc--get-connection vec)))
                         (plist-get conn :buffer))))
      ;; Delegate to disconnect for the common cleanup: async/PTY
      ;; processes, watches, connection hash, executable cache.
      ;; The redundant tramp-flush-* calls in disconnect are harmless.
      (tramp-rpc--disconnect vec)
      ;; Clean up pending responses keyed by the (now-dead) buffer.
      (when conn-buffer
        (remhash conn-buffer tramp-rpc--pending-responses)))
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
               (when-let* ((process (plist-get conn :process))
                           (vec (process-get process :tramp-rpc-vec)))
                 (push (cons vec process) generations)))
             tramp-rpc--connections)
    (dolist (generation generations)
      (let ((vec (car generation))
            (process (cdr generation)))
        (when (processp process)
          (process-put process :tramp-rpc-disconnecting t)
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
    (clrhash tramp-rpc--connections))
  ;; Clear all RPC-specific caches.
  (clrhash tramp-rpc--pending-responses)
  (clrhash tramp-rpc--async-callbacks)
  (clrhash tramp-rpc--async-callback-processes)
  (tramp-rpc--clear-direnv-cache)
  (tramp-rpc--clear-file-metadata-caches)
  ;; Note: recentf cleanup is handled by `tramp-recentf-cleanup-all'
  ;; from tramp-integration.el, registered on the same
  ;; `tramp-cleanup-all-connections-hook'.
  )

;; Register cleanup hooks.
(add-hook 'tramp-cleanup-connection-hook #'tramp-rpc-cleanup-connection)
(add-hook 'tramp-cleanup-all-connections-hook #'tramp-rpc-cleanup-all-connections)

;; ============================================================================
;; Unload support
;; ============================================================================

(defvar tramp-rpc-unload-hook nil
  "Hook run by `tramp-rpc-unload-function' to unload helper modules.")

(defun tramp-rpc-unload-function ()
  "Unload function for tramp-rpc.
Removes advice and cleans up async processes."
  ;; Remove high-level external operations from tramp-rpc core.
  (tramp-remove-external-operation 'locate-dominating-file 'tramp-rpc)
  (tramp-remove-external-operation 'dir-locals--all-files 'tramp-rpc)
  (tramp-remove-external-operation 'dir-locals-find-file 'tramp-rpc)
  (tramp-remove-external-operation 'move-file-to-trash 'tramp-rpc)
  ;; Unload helper modules.  `tramp-rpc.el' requires these modules, so
  ;; unloading only the top-level feature and loading it again would otherwise
  ;; leave stale definitions in place (notably `tramp-rpc-process.el').  Helper
  ;; unload functions also perform their own cleanup before removing symbols.
  ;; Helper hook entries are idempotent because unloading one helper can unload
  ;; another as a dependency before its hook entry is reached.
  (run-hooks 'tramp-rpc-unload-hook)
  ;; Remove multi-hop hook and cleanup hooks.
  (remove-hook 'tramp-multi-hop-p-hook #'tramp-rpc-multi-hop-p)
  (remove-hook 'tramp-cleanup-connection-hook #'tramp-rpc-cleanup-connection)
  (remove-hook 'tramp-cleanup-all-connections-hook #'tramp-rpc-cleanup-all-connections)
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

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-rpc 'force)))

(provide 'tramp-rpc)
;;; tramp-rpc.el ends here
