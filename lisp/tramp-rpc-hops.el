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
;; proxy chain for ssh.
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

(defun tramp-rpc--proxy-handler-alist ()
  "Return foreign handlers excluding TRAMP-RPC during proxy expansion.
TRAMP only expands proxies for shell handlers.  Temporarily hiding both
RPC predicates lets it compute the route without recursing into sudo
handler detection or treating an RPC target as an opaque foreign method."
  (cl-remove-if
   (lambda (entry)
     (memq (car entry) '(tramp-rpc-file-name-p tramp-rpc--sudo-file-name-p)))
   tramp-foreign-file-name-handler-alist))

(defun tramp-rpc--computed-hop-pairs (vec)
  "Return hidden TRAMP proxy hops of VEC as (HOP-STRING . HOP-VEC) pairs.
Native TRAMP helpers, including `tramp-file-name-with-sudo', can store
ad-hoc hops in `tramp-default-proxies-alist' instead of VEC's hop slot.
Invalid proxy configurations signal rather than silently using a direct route."
  (when (and (or (tramp-rpc-file-name-p vec)
                 (tramp-rpc--privilege-elevation-vec-p vec))
             (not tramp-rpc--sudo-file-name-p-in-progress))
    (let ((tramp-rpc--sudo-file-name-p-in-progress t)
          (tramp-foreign-file-name-handler-alist
           (tramp-rpc--proxy-handler-alist)))
      (mapcar
       (lambda (hop-vec)
         (cons (tramp-rpc--hop-vec-to-string hop-vec) hop-vec))
       (butlast (tramp-compute-multi-hops vec))))))

(defun tramp-rpc--hop-pairs (vec)
  "Return VEC's explicit or hidden proxy hops as pairs.
Explicit hops are preferred because they preserve the exact user spelling
from the filename.  Hidden proxy expansion recovers ordinary RPC routes
and native sudo helper routes stored in `tramp-default-proxies-alist'."
  (or (tramp-rpc--explicit-hop-pairs vec)
      (tramp-rpc--computed-hop-pairs vec)))

(defun tramp-rpc--hop-component-string (value)
  "Return tramp hop component VALUE as a comparison string."
  (cond ((null value) "")
        ((stringp value) (substring-no-properties value))
        (t (format "%s" value))))

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

(cl-defstruct (tramp-rpc-route
               (:constructor tramp-rpc--make-route)
               (:copier nil))
  "Parsed hop-chain and SSH target details for one TRAMP-RPC vector."
  vec
  hop-pairs
  sudo-hop
  proxy-hops
  ssh-vec
  host
  ssh-user
  ssh-port
  proxy-hop-string
  proxyjump)

(defun tramp-rpc--format-proxy-hops-string (proxy-hops)
  "Format PROXY-HOPS pairs into a TRAMP hop string with trailing delimiter."
  (when proxy-hops
    (concat (mapconcat #'car proxy-hops tramp-postfix-hop-format)
            tramp-postfix-hop-format)))

(defun tramp-rpc--format-proxyjump (proxy-hops)
  "Format PROXY-HOPS pairs into an OpenSSH ProxyJump (-J) string."
  (when proxy-hops
    (mapconcat
     (lambda (hop)
       (let ((hop-vec (cdr hop)))
         (concat
          (when (tramp-file-name-user hop-vec)
            (concat (tramp-file-name-user hop-vec) "@"))
          (tramp-file-name-host hop-vec)
          (when-let* ((port (tramp-rpc--port-to-string
                             (tramp-file-name-port hop-vec))))
            (concat ":" port)))))
     proxy-hops
     ",")))

(defun tramp-rpc--analyze-route (vec)
  "Analyze VEC's hop chain and return a `tramp-rpc-route' struct."
  (let* ((hop-pairs (tramp-rpc--hop-pairs vec))
         (target-host (tramp-file-name-host vec))
         (last-hop (car (last hop-pairs)))
         (sudo-hop
          (when (and target-host
                     last-hop
                     (tramp-rpc--privilege-elevation-vec-p vec))
            (let ((hop-vec (cdr last-hop)))
              ;; Only the final hop before the privilege-elevation method
              ;; carries the SSH connection details for that sudo target.
              ;; Earlier same-host rpc hops are still real proxy hops and
              ;; must not be stripped.
              (when (and (string= (tramp-file-name-method hop-vec) "rpc")
                         (string= (tramp-file-name-host hop-vec) target-host))
                last-hop))))
         (proxy-hops (if sudo-hop (butlast hop-pairs) hop-pairs))
         (ssh-vec (if sudo-hop (cdr sudo-hop) vec)))
    (tramp-rpc--make-route
     :vec vec
     :hop-pairs hop-pairs
     :sudo-hop sudo-hop
     :proxy-hops proxy-hops
     :ssh-vec ssh-vec
     :host target-host
     :ssh-user (tramp-file-name-user ssh-vec)
     :ssh-port (tramp-rpc--port-to-string (tramp-file-name-port ssh-vec))
     :proxy-hop-string (tramp-rpc--format-proxy-hops-string proxy-hops)
     :proxyjump (tramp-rpc--format-proxyjump proxy-hops))))

(defun tramp-rpc--same-host-rpc-hop (vec &optional return-string)
  "Return VEC's matching same-host rpc hop for privilege elevation.
When RETURN-STRING is non-nil, return (HOP-STRING . HOP-VEC).  Return
nil for non-privilege targets, so same-host rpc|rpc chains are not
misclassified as sudo-via-RPC."
  (when-let* ((hop (tramp-rpc-route-sudo-hop (tramp-rpc--analyze-route vec))))
    (if return-string hop (cdr hop))))

(defun tramp-rpc--sudo-rpc-hop-vec (vec)
  "Return VEC's same-host rpc hop vec for sudo-via-RPC, or nil."
  (tramp-rpc--same-host-rpc-hop vec))

(defun tramp-rpc--detect-sudo-elevation (vec)
  "Return non-nil if VEC needs sudo elevation via RPC.
Detects privilege-elevation targets such as
/rpc:user@host|sudo:root@host:/path.  The value is the SSH user named in
the rpc hop, or t when the hop names none: ssh then takes the user from
its configuration.  Same-host rpc|rpc chains are not sudo elevation and
return nil."
  (when-let* ((hop-vec (tramp-rpc--sudo-rpc-hop-vec vec)))
    (or (tramp-file-name-user hop-vec) t)))

(defun tramp-rpc--ssh-detail-vec (vec)
  "Return the vec carrying SSH connection details for VEC.
For sudo-via-RPC this is the matching same-host rpc hop.  Otherwise it
is VEC itself."
  (tramp-rpc-route-ssh-vec (tramp-rpc--analyze-route vec)))

(defun tramp-rpc--ssh-detail-user (vec &optional default)
  "Return the SSH user for VEC, using sudo-via-RPC hop details.
DEFAULT is the fallback value."
  (or (tramp-rpc-route-ssh-user (tramp-rpc--analyze-route vec)) default))

(defun tramp-rpc--ssh-detail-port (vec)
  "Return the SSH port for VEC, using sudo-via-RPC hop details."
  (tramp-file-name-port (tramp-rpc--ssh-detail-vec vec)))

(defun tramp-rpc--sudo-auth-vec (vec)
  "Return the unprivileged rpc vector used to validate sudo for VEC."
  (let ((route (tramp-rpc--analyze-route vec)))
    (when-let* ((sudo-hop (cdr (tramp-rpc-route-sudo-hop route))))
      (make-tramp-file-name
       :method tramp-rpc-method
       :user (tramp-file-name-user sudo-hop)
       :domain (tramp-file-name-domain sudo-hop)
       :host (tramp-file-name-host sudo-hop)
       :port (tramp-file-name-port sudo-hop)
       :localname (tramp-file-name-localname vec)
       :hop (tramp-rpc-route-proxy-hop-string route)))))

(defun tramp-rpc--proxy-hop-string (vec)
  "Return VEC's hop string with its sudo rpc hop removed.
For /rpc:gw|rpc:user@host|sudo:root@host:/path, returns \"rpc:gw|\".
For non-sudo paths, returns the original hop string.  Returns nil if no
proxy hops remain."
  (tramp-rpc-route-proxy-hop-string (tramp-rpc--analyze-route vec)))

(defun tramp-rpc--hops-to-proxyjump (vec)
  "Convert VEC's hop chain to an SSH ProxyJump (-J) string.
Parses the TRAMP hop field (for example, `rpc:user@gateway|') and converts
each hop to the SSH ProxyJump format (for example, `user@gateway').
Returns nil if there are no hops.

For sudo-via-RPC paths, the same-host rpc hop carrying the SSH details is
not a ProxyJump; earlier same-host rpc hops are preserved.  Supports mixed
methods: both `rpc:' and `ssh:' hops are accepted since ProxyJump only
needs host connectivity."
  (tramp-rpc-route-proxyjump (tramp-rpc--analyze-route vec)))

(defvar tramp-rpc--inhibit-add-hops nil
  "Non-nil while `tramp-add-hops' must be suppressed.")

(defun tramp-rpc--path-has-explicit-hop-p (path)
  "Return non-nil when PATH (or `default-directory') carries an explicit hop."
  (when (stringp path)
    (let ((effective (if (file-name-absolute-p path) path default-directory)))
      (and (stringp effective)
           (tramp-tramp-file-p effective)
           (string-search tramp-postfix-hop-format effective)
           (let ((tramp-default-proxies-alist tramp-cache-undefined))
             (ignore-errors
               (and (tramp-file-name-hop (tramp-dissect-file-name effective))
                    t)))))))

(defun tramp-rpc--copy-proxies-alist ()
  "Return a shallow copy of `tramp-default-proxies-alist' when it is a list.
Preserves sentinel values such as `tramp-cache-undefined'."
  (if (listp tramp-default-proxies-alist)
      (copy-sequence tramp-default-proxies-alist)
    tramp-default-proxies-alist))

(defun tramp-rpc--expand-preserving-hops (filename &optional default-dir)
  "Expand FILENAME relative to DEFAULT-DIR while preserving explicit proxy hops.
Avoids mutating `tramp-default-proxies-alist' via `tramp-add-hops'."
  (let ((tramp-show-ad-hoc-proxies t)
        (tramp-rpc--inhibit-add-hops t)
        (tramp-default-proxies-alist (tramp-rpc--copy-proxies-alist)))
    (expand-file-name filename default-dir)))

(defun tramp-rpc--materialize-route (filename &optional default-dir)
  "Expand FILENAME relative to DEFAULT-DIR and make hidden proxy hops explicit.
Preserves explicit hops without mutating `tramp-default-proxies-alist'.
For a hopless TRAMP-RPC filename routed through hidden proxy rules, attaches
the resolved hop chain explicitly so operations on another explicit route
cannot redirect it."
  (let* ((tramp-rpc--inhibit-add-hops t)
         (tramp-default-proxies-alist (tramp-rpc--copy-proxies-alist))
         (expanded (tramp-rpc--expand-preserving-hops filename default-dir))
         (quoted-p (file-name-quoted-p expanded))
         (unquoted (if quoted-p (file-name-unquote expanded) expanded)))
    (if (not (tramp-tramp-file-p unquoted))
        expanded
      (let* ((vec (tramp-dissect-file-name unquoted))
             (computed-hops (unless (tramp-file-name-hop vec)
                              (tramp-rpc--computed-hop-pairs vec))))
        (if (not computed-hops)
            expanded
          (let* ((materialized-vec
                  (make-tramp-file-name
                   :method (tramp-file-name-method vec)
                   :user (tramp-file-name-user vec)
                   :domain (tramp-file-name-domain vec)
                   :host (tramp-file-name-host vec)
                   :port (tramp-file-name-port vec)
                   :localname (tramp-file-name-localname vec)
                   :hop (tramp-rpc--format-proxy-hops-string computed-hops)))
                 (result
                  (let ((tramp-default-proxies-alist tramp-cache-undefined)
                        (tramp-show-ad-hoc-proxies t))
                    (tramp-make-tramp-file-name materialized-vec))))
            (if quoted-p (file-name-quote result) result)))))))

(provide 'tramp-rpc-hops)
;;; tramp-rpc-hops.el ends here
