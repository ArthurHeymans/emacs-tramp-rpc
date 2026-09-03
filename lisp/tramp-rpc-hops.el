;;; tramp-rpc-hops.el --- Hop-chain analysis for TRAMP-RPC -*- lexical-binding: t; -*-

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

;; Pure analysis of a TRAMP vector's hop chain: which hops are rpc, which
;; hop the SSH connection actually targets, whether the final hop is a
;; sudo privilege elevation carried over an rpc hop, and how to spell the
;; proxy chain for ssh.  It also reads and caches the sudo password.
;;
;; This sits below both the transport and the deployment modules, which
;; need the same answers about a vector.

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 'tramp-rpc-protocol)

;; These predicates are emitted inside the autoload form in tramp-rpc.el.
(declare-function tramp-rpc--sudo-file-name-p "tramp-rpc")
(declare-function tramp-rpc-file-name-p "tramp-rpc")
(declare-function tramp-rpc-multi-hop-p "tramp-rpc")
(defvar tramp-rpc-method)
(defvar tramp-rpc--sudo-file-name-p-in-progress)

(defun tramp-rpc--managed-file-name-p (vec-or-filename)
  "Return non-nil when VEC-OR-FILENAME is managed by TRAMP-RPC."
  (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename)))
    (or (tramp-rpc-file-name-p vec)
        (tramp-rpc--sudo-file-name-p vec))))

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
  "Return the SSH user for VEC, using sudo-via-RPC hop details.
DEFAULT is the fallback value."
  (or (tramp-file-name-user (tramp-rpc--ssh-detail-vec vec)) default))

(defun tramp-rpc--ssh-detail-port (vec)
  "Return the SSH port for VEC, using sudo-via-RPC hop details."
  (tramp-file-name-port (tramp-rpc--ssh-detail-vec vec)))

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
    (error "Sudo password is not a string: %S" password))
  password)

(defun tramp-rpc--sudo-read-password (vec ssh-user)
  "Read sudo password for SSH-USER on VEC using TRAMP auth machinery."
  (let* ((host (tramp-file-name-host vec))
         (port (tramp-rpc--port-to-string (tramp-rpc--ssh-detail-port vec)))
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

(define-error 'tramp-rpc-sudo-auth-rejected
  "Sudo authentication was rejected" 'remote-file-error)

(defun tramp-rpc--sudo-auth-rejected-p (stderr-buffer)
  "Return non-nil when STDERR-BUFFER confirms sudo authentication rejection."
  (when (buffer-live-p stderr-buffer)
    (with-current-buffer stderr-buffer
      (let ((case-fold-search t))
        (string-match-p
         (rx (or "sorry, try again"
                 (seq (+ digit) " incorrect password attempt")
                 "authentication failure"))
         (buffer-substring-no-properties (point-min) (point-max)))))))

(defun tramp-rpc--clear-sudo-password (vec)
  "Clear the cached sudo password for VEC.
Called when sudo explicitly rejects a password so the next attempt prompts
for a fresh password instead of silently reusing the rejected one."
  (tramp-clear-passwd vec))

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

(provide 'tramp-rpc-hops)
;;; tramp-rpc-hops.el ends here
