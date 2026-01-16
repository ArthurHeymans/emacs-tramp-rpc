;;; tramp-rpc-deploy.el --- Binary deployment for TRAMP-RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Arthur

;; Author: Arthur
;; Keywords: comm, processes
;; Package-Requires: ((emacs "30.1"))

;; This file is part of tramp-rpc.

;;; Commentary:

;; This file handles deployment of the tramp-rpc-server binary to
;; remote hosts.  It supports:
;; - Automatic detection of remote architecture
;; - Transfer of pre-compiled binaries
;; - Caching of deployed binaries with version checking

;;; Code:

(require 'tramp)

;; Silence byte-compiler warnings for functions defined in tramp-sh
(declare-function tramp-send-command "tramp-sh")
(declare-function tramp-send-command-and-check "tramp-sh")
(declare-function tramp-send-command-and-read "tramp-sh")

(defgroup tramp-rpc-deploy nil
  "Deployment settings for TRAMP-RPC."
  :group 'tramp)

(defcustom tramp-rpc-deploy-binary-directory
  (expand-file-name "binaries" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing pre-compiled tramp-rpc-server binaries.
Should contain subdirectories like x86_64-linux, aarch64-linux, etc."
  :type 'directory
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-remote-directory "~/.cache/tramp-rpc"
  "Remote directory where the server binary will be installed."
  :type 'string
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-auto-deploy t
  "If non-nil, automatically deploy the server binary when needed."
  :type 'boolean
  :group 'tramp-rpc-deploy)

(defconst tramp-rpc-deploy-version "0.1.0"
  "Current version of tramp-rpc-server.")

(defconst tramp-rpc-deploy-binary-name "tramp-rpc-server"
  "Name of the server binary.")

(defcustom tramp-rpc-deploy-bootstrap-method "sshx"
  "TRAMP method to use for bootstrapping (deploying the binary).
Use \"sshx\" to avoid PTY allocation issues, or \"ssh\" for standard SSH."
  :type 'string
  :group 'tramp-rpc-deploy)

(defun tramp-rpc-deploy--bootstrap-vec (vec)
  "Convert VEC to use the bootstrap method for deployment operations.
This allows us to use sshx for deployment even when the main method is rpc."
  (let ((method (tramp-file-name-method vec)))
    (if (member method '("ssh" "sshx" "scpx"))
        vec  ; Already a shell-based method
      ;; Convert to bootstrap method - create a new tramp-file-name struct
      (make-tramp-file-name
       :method tramp-rpc-deploy-bootstrap-method
       :user (tramp-file-name-user vec)
       :domain (tramp-file-name-domain vec)
       :host (tramp-file-name-host vec)
       :port (tramp-file-name-port vec)
       :localname (tramp-file-name-localname vec)
       :hop (tramp-file-name-hop vec)))))

(defun tramp-rpc-deploy--detect-arch (vec)
  "Detect the architecture of remote host specified by VEC.
Returns a string like \"x86_64-linux\" or \"aarch64-darwin\"."
  (let* ((uname-m (string-trim
                   (tramp-send-command-and-read
                    vec "echo \\\"`uname -m`\\\"")))
         (uname-s (string-trim
                   (tramp-send-command-and-read
                    vec "echo \\\"`uname -s`\\\"")))
         (arch (pcase uname-m
                 ("x86_64" "x86_64")
                 ("amd64" "x86_64")
                 ("aarch64" "aarch64")
                 ("arm64" "aarch64")
                 (_ uname-m)))
         (os (pcase (downcase uname-s)
               ("linux" "linux")
               ("darwin" "darwin")
               (_ (downcase uname-s)))))
    (format "%s-%s" arch os)))

(defun tramp-rpc-deploy--local-binary-path (arch)
  "Return the path to the local binary for ARCH."
  (expand-file-name
   tramp-rpc-deploy-binary-name
   (expand-file-name arch tramp-rpc-deploy-binary-directory)))

(defun tramp-rpc-deploy--remote-binary-path (vec)
  "Return the remote path where the binary should be installed for VEC."
  (tramp-make-tramp-file-name
   vec
   (expand-file-name
    (format "%s-%s" tramp-rpc-deploy-binary-name tramp-rpc-deploy-version)
    tramp-rpc-deploy-remote-directory)))

(defun tramp-rpc-deploy--remote-binary-exists-p (vec)
  "Check if the correct version of the binary exists on remote VEC."
  (let ((remote-path (tramp-rpc-deploy--remote-binary-path vec)))
    ;; Use tramp-sh operations for checking since we're bootstrapping
    (tramp-send-command-and-check
     vec
     (format "test -x %s"
             (tramp-shell-quote-argument
              (tramp-file-local-name remote-path))))))

(defun tramp-rpc-deploy--ensure-remote-directory (vec)
  "Ensure the remote deployment directory exists on VEC."
  (let ((dir (tramp-file-local-name
              (tramp-make-tramp-file-name vec tramp-rpc-deploy-remote-directory))))
    (tramp-send-command vec (format "mkdir -p %s" (tramp-shell-quote-argument dir)))))

(defcustom tramp-rpc-deploy-max-retries 3
  "Maximum number of retries for binary transfer."
  :type 'integer
  :group 'tramp-rpc-deploy)

(defun tramp-rpc-deploy--compute-checksum (file)
  "Compute SHA256 checksum of local FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun tramp-rpc-deploy--remote-checksum (vec path)
  "Get SHA256 checksum of remote PATH on VEC."
  (tramp-send-command vec
   (format "sha256sum %s 2>/dev/null | cut -d' ' -f1"
           (tramp-shell-quote-argument path)))
  (with-current-buffer (tramp-get-connection-buffer vec)
    (goto-char (point-min))
    (when (looking-at "\\([a-f0-9]+\\)")
      (match-string 1))))

(defun tramp-rpc-deploy--transfer-binary (vec arch)
  "Transfer the binary for ARCH to the remote host VEC.
Uses atomic write with checksum verification for reliability."
  (let* ((local-path (tramp-rpc-deploy--local-binary-path arch))
         (remote-path (tramp-rpc-deploy--remote-binary-path vec))
         (remote-local (tramp-file-local-name remote-path))
         (remote-tmp (concat remote-local ".tmp." (format "%d" (random 100000))))
         (local-checksum (tramp-rpc-deploy--compute-checksum local-path)))
    
    (unless (file-exists-p local-path)
      (error "No binary found for architecture %s at %s" arch local-path))
    
    ;; Ensure remote directory exists
    (tramp-rpc-deploy--ensure-remote-directory vec)
    
    ;; Read local binary and encode to base64
    (let ((binary-data (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally local-path)
                         (base64-encode-region (point-min) (point-max))
                         (buffer-string)))
          (retries 0)
          (success nil))
      
      ;; Retry loop for reliability
      (while (and (not success) (< retries tramp-rpc-deploy-max-retries))
        (condition-case err
            (progn
              ;; Write to temp file using base64 decode
              (tramp-send-command
               vec
               (format "base64 -d > %s << 'TRAMP_RPC_EOF'\n%s\nTRAMP_RPC_EOF"
                       (tramp-shell-quote-argument remote-tmp)
                       binary-data))
              
              ;; Verify checksum
              (let ((remote-checksum (tramp-rpc-deploy--remote-checksum vec remote-tmp)))
                (if (string= local-checksum remote-checksum)
                    (progn
                      ;; Checksum matches - make executable and atomically move
                      (tramp-send-command
                       vec
                       (format "chmod +x %s && mv -f %s %s"
                               (tramp-shell-quote-argument remote-tmp)
                               (tramp-shell-quote-argument remote-tmp)
                               (tramp-shell-quote-argument remote-local)))
                      (setq success t))
                  ;; Checksum mismatch - clean up and retry
                  (tramp-send-command
                   vec
                   (format "rm -f %s" (tramp-shell-quote-argument remote-tmp)))
                  (setq retries (1+ retries))
                  (when (< retries tramp-rpc-deploy-max-retries)
                    (message "Checksum mismatch, retrying (%d/%d)..."
                             retries tramp-rpc-deploy-max-retries)))))
          (error
           ;; Clean up on error and retry
           (ignore-errors
             (tramp-send-command
              vec
              (format "rm -f %s" (tramp-shell-quote-argument remote-tmp))))
           (setq retries (1+ retries))
           (when (< retries tramp-rpc-deploy-max-retries)
             (message "Transfer failed (%s), retrying (%d/%d)..."
                      (error-message-string err)
                      retries tramp-rpc-deploy-max-retries)))))
      
      (unless success
        (error "Failed to transfer binary after %d attempts" tramp-rpc-deploy-max-retries)))
    
    remote-path))

(defun tramp-rpc-deploy-ensure-binary (vec)
  "Ensure the tramp-rpc-server binary is available on remote VEC.
Returns the remote path to the binary.
If `tramp-rpc-deploy-auto-deploy' is nil and the binary is missing,
signals an error."
  (let ((bootstrap-vec (tramp-rpc-deploy--bootstrap-vec vec)))
    (if (tramp-rpc-deploy--remote-binary-exists-p bootstrap-vec)
        ;; Binary already exists
        (tramp-file-local-name (tramp-rpc-deploy--remote-binary-path bootstrap-vec))
      ;; Need to deploy
      (if tramp-rpc-deploy-auto-deploy
          (let ((arch (tramp-rpc-deploy--detect-arch bootstrap-vec)))
            (message "Deploying tramp-rpc-server (%s) to %s..."
                     arch (tramp-file-name-host vec))
            (tramp-file-local-name
             (tramp-rpc-deploy--transfer-binary bootstrap-vec arch)))
        (error "tramp-rpc-server not found on %s and auto-deploy is disabled"
               (tramp-file-name-host vec))))))

(defun tramp-rpc-deploy-remove-binary (vec)
  "Remove the tramp-rpc-server binary from remote VEC."
  (interactive
   (list (tramp-dissect-file-name
          (read-file-name "Remote host: " "/ssh:"))))
  (let ((remote-path (tramp-rpc-deploy--remote-binary-path vec)))
    (when (tramp-rpc-deploy--remote-binary-exists-p vec)
      (tramp-send-command
       vec
       (format "rm -f %s"
               (tramp-shell-quote-argument
                (tramp-file-local-name remote-path))))
      (message "Removed %s from %s"
               tramp-rpc-deploy-binary-name
               (tramp-file-name-host vec)))))

(provide 'tramp-rpc-deploy)
;;; tramp-rpc-deploy.el ends here
