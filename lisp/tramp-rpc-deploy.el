;;; tramp-rpc-deploy.el --- Binary deployment for TRAMP-RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Keywords: comm, processes
;; Package-Requires: ((emacs "30.1"))

;; This file is part of tramp-rpc.

;;; Commentary:

;; This file handles deployment of the tramp-rpc-server binary to
;; remote hosts.  It supports:
;; - Automatic detection of remote architecture
;; - Downloading pre-compiled binaries from GitHub releases
;; - Building from source as fallback (requires Rust)
;; - Local caching of binaries
;; - Transfer to remote hosts with checksum verification

;;; Code:

(require 'tramp)
(require 'url)

;; Silence byte-compiler warnings for functions defined in tramp-sh
(declare-function tramp-send-command "tramp-sh")
(declare-function tramp-send-command-and-check "tramp-sh")
(declare-function tramp-send-command-and-read "tramp-sh")

;;; ============================================================================
;;; Customization
;;; ============================================================================

(defgroup tramp-rpc-deploy nil
  "Deployment settings for TRAMP-RPC."
  :group 'tramp)

(defconst tramp-rpc-deploy-version "0.1.0"
  "Current version of tramp-rpc-server.")

(defconst tramp-rpc-deploy-binary-name "tramp-rpc-server"
  "Name of the server binary.")

(defcustom tramp-rpc-deploy-github-repo "ArthurHeymans/emacs-tramp-rpc"
  "GitHub repository for downloading pre-compiled binaries.
Format: \"owner/repo\"."
  :type 'string
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-release-url-format
  "https://github.com/%s/releases/download/v%s/%s"
  "URL format for downloading release assets.
Arguments: repo, version, filename."
  :type 'string
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-local-cache-directory
  (expand-file-name "tramp-rpc" user-emacs-directory)
  "Local directory for caching downloaded/built binaries.
Binaries are stored as CACHE-DIR/VERSION/ARCH/tramp-rpc-server."
  :type 'directory
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-source-directory
  (when load-file-name
    (expand-file-name ".." (file-name-directory load-file-name)))
  "Directory containing the tramp-rpc source code.
Used for building from source.  Set to nil to disable source builds."
  :type '(choice directory (const nil))
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-remote-directory "~/.cache/tramp-rpc"
  "Remote directory where the server binary will be installed."
  :type 'string
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-auto-deploy t
  "If non-nil, automatically deploy the server binary when needed."
  :type 'boolean
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-prefer-build nil
  "If non-nil, prefer building from source over downloading.
By default, downloading is attempted first as it's faster."
  :type 'boolean
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-bootstrap-method "sshx"
  "TRAMP method to use for bootstrapping (deploying the binary).
Use \"sshx\" to avoid PTY allocation issues, or \"ssh\" for standard SSH."
  :type 'string
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-max-retries 3
  "Maximum number of retries for binary transfer."
  :type 'integer
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-download-timeout 120
  "Timeout in seconds for downloading binaries."
  :type 'integer
  :group 'tramp-rpc-deploy)

;;; ============================================================================
;;; Architecture detection and path helpers
;;; ============================================================================

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

(defun tramp-rpc-deploy--detect-remote-arch (vec)
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

(defun tramp-rpc-deploy--detect-local-arch ()
  "Detect the architecture of the local system.
Returns a string like \"x86_64-linux\" or \"aarch64-darwin\"."
  (let* ((arch (pcase system-type
                 ('gnu/linux "linux")
                 ('darwin "darwin")
                 (_ (symbol-name system-type))))
         (machine (car (split-string system-configuration "-")))
         (normalized-machine (pcase machine
                               ("x86_64" "x86_64")
                               ("aarch64" "aarch64")
                               ("arm64" "aarch64")
                               (_ machine))))
    (format "%s-%s" normalized-machine arch)))

(defun tramp-rpc-deploy--arch-to-rust-target (arch)
  "Convert ARCH string to Rust target triple.
E.g., \"x86_64-linux\" -> \"x86_64-unknown-linux-gnu\"."
  (pcase arch
    ("x86_64-linux" "x86_64-unknown-linux-gnu")
    ("aarch64-linux" "aarch64-unknown-linux-gnu")
    ("x86_64-darwin" "x86_64-apple-darwin")
    ("aarch64-darwin" "aarch64-apple-darwin")
    (_ (error "Unknown architecture: %s" arch))))

(defun tramp-rpc-deploy--local-cache-path (arch)
  "Return the local cache path for binary of ARCH."
  (expand-file-name
   tramp-rpc-deploy-binary-name
   (expand-file-name
    arch
    (expand-file-name
     tramp-rpc-deploy-version
     tramp-rpc-deploy-local-cache-directory))))

(defun tramp-rpc-deploy--remote-binary-path (vec)
  "Return the remote path where the binary should be installed for VEC."
  (tramp-make-tramp-file-name
   vec
   ;; Use concat instead of expand-file-name to preserve ~ for remote expansion.
   ;; expand-file-name would expand ~ to the LOCAL user's home directory,
   ;; causing failures when local and remote usernames differ.
   (concat (file-name-as-directory tramp-rpc-deploy-remote-directory)
           (format "%s-%s" tramp-rpc-deploy-binary-name tramp-rpc-deploy-version))))

;;; ============================================================================
;;; Download from GitHub Releases
;;; ============================================================================

(defun tramp-rpc-deploy--release-asset-name (arch)
  "Return the release asset filename for ARCH."
  (format "tramp-rpc-server-%s-%s.tar.gz" arch tramp-rpc-deploy-version))

(defun tramp-rpc-deploy--download-url (arch)
  "Return the download URL for binary of ARCH."
  (format tramp-rpc-deploy-release-url-format
          tramp-rpc-deploy-github-repo
          tramp-rpc-deploy-version
          (tramp-rpc-deploy--release-asset-name arch)))

(defun tramp-rpc-deploy--checksum-url (arch)
  "Return the checksum file URL for binary of ARCH."
  (format tramp-rpc-deploy-release-url-format
          tramp-rpc-deploy-github-repo
          tramp-rpc-deploy-version
          (format "tramp-rpc-server-%s-%s.tar.gz.sha256" arch tramp-rpc-deploy-version)))

(defun tramp-rpc-deploy--download-file (url dest)
  "Download URL to DEST synchronously.
Returns t on success, nil on failure."
  (condition-case err
      (let ((url-request-method "GET")
            (url-show-status nil))
        (message "Downloading %s..." url)
        (with-timeout (tramp-rpc-deploy-download-timeout
                       (error "Download timed out after %d seconds"
                              tramp-rpc-deploy-download-timeout))
          (with-current-buffer (url-retrieve-synchronously url t t)
            (goto-char (point-min))
            ;; Check for HTTP errors
            (unless (re-search-forward "^HTTP/[0-9.]+ 200" nil t)
              (if (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                  (error "HTTP error %s" (match-string 1))
                (error "Invalid HTTP response")))
            ;; Find body (after blank line)
            (re-search-forward "^\r?\n" nil t)
            ;; Write body to file
            (let ((coding-system-for-write 'binary))
              (write-region (point) (point-max) dest nil 'silent))
            (kill-buffer)
            t)))
    (error
     (message "Download failed: %s" (error-message-string err))
     nil)))

(defun tramp-rpc-deploy--verify-checksum (file expected-checksum)
  "Verify that FILE has EXPECTED-CHECKSUM.
Returns t if checksum matches, nil otherwise."
  (when (and file (file-exists-p file) expected-checksum)
    (let ((actual (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally file)
                    (secure-hash 'sha256 (current-buffer)))))
      (string= actual (car (split-string expected-checksum))))))

(defun tramp-rpc-deploy--extract-tarball (tarball dest-dir)
  "Extract TARBALL to DEST-DIR.
Returns the path to the extracted binary, or nil on failure."
  (let ((default-directory dest-dir))
    (make-directory dest-dir t)
    (if (zerop (call-process "tar" nil nil nil "-xzf" tarball "-C" dest-dir))
        (let ((binary (expand-file-name tramp-rpc-deploy-binary-name dest-dir)))
          (when (file-exists-p binary)
            (set-file-modes binary #o755)
            binary))
      nil)))

(defun tramp-rpc-deploy--download-binary (arch)
  "Download pre-compiled binary for ARCH from GitHub releases.
Returns the path to the binary on success, signals error on failure."
  (let* ((cache-path (tramp-rpc-deploy--local-cache-path arch))
         (cache-dir (file-name-directory cache-path))
         (tarball-url (tramp-rpc-deploy--download-url arch))
         (checksum-url (tramp-rpc-deploy--checksum-url arch))
         (temp-dir (make-temp-file "tramp-rpc-" t))
         (tarball-path (expand-file-name "server.tar.gz" temp-dir))
         (checksum-path (expand-file-name "server.tar.gz.sha256" temp-dir)))
    (unwind-protect
        (progn
          ;; Download checksum first
          (message "Fetching checksum for %s..." arch)
          (let ((checksum-ok (tramp-rpc-deploy--download-file checksum-url checksum-path)))
            ;; Download tarball
            (message "Downloading tramp-rpc-server for %s..." arch)
            (unless (tramp-rpc-deploy--download-file tarball-url tarball-path)
              (error "Download failed from %s (release may not exist)" tarball-url))
            ;; Verify checksum if we got one
            (when checksum-ok
              (let ((expected (with-temp-buffer
                                (insert-file-contents checksum-path)
                                (buffer-string))))
                (unless (tramp-rpc-deploy--verify-checksum tarball-path expected)
                  (error "Checksum verification failed"))))
            ;; Extract
            (message "Extracting binary...")
            (make-directory cache-dir t)
            (unless (tramp-rpc-deploy--extract-tarball tarball-path cache-dir)
              (error "Failed to extract tarball"))
            (message "Downloaded tramp-rpc-server for %s" arch)
            cache-path))
      ;; Cleanup temp dir
      (delete-directory temp-dir t))))

;;; ============================================================================
;;; Build from source
;;; ============================================================================

(defun tramp-rpc-deploy--cargo-available-p ()
  "Check if cargo (Rust) is available."
  (executable-find "cargo"))

(defun tramp-rpc-deploy--can-build-for-arch-p (arch)
  "Check if we can build for ARCH on this system.
Cross-compilation requires additional setup, so we only build natively."
  (string= arch (tramp-rpc-deploy--detect-local-arch)))

(defun tramp-rpc-deploy--build-binary (arch)
  "Build the binary for ARCH from source.
Returns the path to the binary on success, nil on failure."
  (unless tramp-rpc-deploy-source-directory
    (error "Source directory not configured"))
  (unless (tramp-rpc-deploy--cargo-available-p)
    (error "Rust toolchain (cargo) not found"))
  (unless (tramp-rpc-deploy--can-build-for-arch-p arch)
    (error "Cannot cross-compile for %s on %s"
           arch (tramp-rpc-deploy--detect-local-arch)))
  
  (let* ((default-directory tramp-rpc-deploy-source-directory)
         (target (tramp-rpc-deploy--arch-to-rust-target arch))
         (cache-path (tramp-rpc-deploy--local-cache-path arch))
         (cache-dir (file-name-directory cache-path))
         (build-output (expand-file-name
                        (format "target/%s/release/%s"
                                target tramp-rpc-deploy-binary-name)
                        tramp-rpc-deploy-source-directory))
         (build-buffer (get-buffer-create "*tramp-rpc-build*")))
    
    (message "Building tramp-rpc-server for %s (this may take a minute)..." arch)
    
    (with-current-buffer build-buffer
      (erase-buffer))
    
    (let ((exit-code
           (call-process "cargo" nil build-buffer nil
                         "build" "--release"
                         "--target" target
                         "--manifest-path"
                         (expand-file-name "Cargo.toml" tramp-rpc-deploy-source-directory))))
      (if (zerop exit-code)
          (progn
            ;; Copy to cache
            (make-directory cache-dir t)
            (copy-file build-output cache-path t)
            (set-file-modes cache-path #o755)
            (message "Built tramp-rpc-server for %s" arch)
            cache-path)
        (with-current-buffer build-buffer
          (error "Build failed (exit %d):\n%s" exit-code (buffer-string)))))))

;;; ============================================================================
;;; Main logic: ensure local binary exists
;;; ============================================================================

(defun tramp-rpc-deploy--ensure-local-binary (arch)
  "Ensure a local binary exists for ARCH.
Tries in order:
1. Check local cache
2. Download from GitHub releases
3. Build from source (if on same architecture)

Returns the path to the local binary."
  (let ((cache-path (tramp-rpc-deploy--local-cache-path arch)))
    ;; Check cache first
    (if (and (file-exists-p cache-path)
             (file-executable-p cache-path))
        (progn
          (message "Using cached binary for %s" arch)
          cache-path)
      
      ;; Need to obtain binary
      (let ((methods (if tramp-rpc-deploy-prefer-build
                         '(build download)
                       '(download build)))
            (result nil)
            (errors nil))
        
        (dolist (method methods)
          (unless result
            (condition-case err
                (setq result
                      (pcase method
                        ('download
                         (tramp-rpc-deploy--download-binary arch))
                        ('build
                         (when (and tramp-rpc-deploy-source-directory
                                    (tramp-rpc-deploy--cargo-available-p)
                                    (tramp-rpc-deploy--can-build-for-arch-p arch))
                           (tramp-rpc-deploy--build-binary arch)))))
              (error
               (push (cons method (error-message-string err)) errors)))))
        
        (or result
            (error "Failed to obtain tramp-rpc-server for %s.\n\nErrors:\n%s\n\n%s"
                   arch
                   (mapconcat (lambda (e)
                                (format "  %s: %s" (car e) (cdr e)))
                              (reverse errors)
                              "\n")
                   (tramp-rpc-deploy--help-message arch)))))))

(defun tramp-rpc-deploy--help-message (arch)
  "Return a help message for obtaining binary for ARCH."
  (let ((local-arch (tramp-rpc-deploy--detect-local-arch)))
    (concat
     "To resolve this, you can:\n\n"
     (format "1. Download manually from:\n   %s\n\n"
             (tramp-rpc-deploy--download-url arch))
     (if (string= arch local-arch)
         (concat
          "2. Install Rust and build from source:\n"
          "   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh\n"
          "   Then restart Emacs and try again.\n\n")
       (format
        "2. Build on a %s machine and copy to:\n   %s\n\n"
        arch
        (tramp-rpc-deploy--local-cache-path arch)))
     (format "Binary should be placed at:\n   %s"
             (tramp-rpc-deploy--local-cache-path arch)))))

;;; ============================================================================
;;; Remote deployment
;;; ============================================================================

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

(defun tramp-rpc-deploy--transfer-binary (vec local-path)
  "Transfer the binary at LOCAL-PATH to the remote host VEC.
Uses atomic write with checksum verification for reliability."
  (let* ((remote-path (tramp-rpc-deploy--remote-binary-path vec))
         (remote-local (tramp-file-local-name remote-path))
         (remote-tmp (concat remote-local ".tmp." (format "%d" (random 100000))))
         (local-checksum (tramp-rpc-deploy--compute-checksum local-path)))
    
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

;;; ============================================================================
;;; Public API
;;; ============================================================================

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
          (let* ((arch (tramp-rpc-deploy--detect-remote-arch bootstrap-vec))
                 (local-binary (tramp-rpc-deploy--ensure-local-binary arch)))
            (message "Deploying tramp-rpc-server (%s) to %s..."
                     arch (tramp-file-name-host vec))
            (tramp-file-local-name
             (tramp-rpc-deploy--transfer-binary bootstrap-vec local-binary)))
        (error "tramp-rpc-server not found on %s and auto-deploy is disabled"
               (tramp-file-name-host vec))))))

(defun tramp-rpc-deploy-remove-binary (vec)
  "Remove the tramp-rpc-server binary from remote VEC."
  (interactive
   (list (tramp-dissect-file-name
          (read-file-name "Remote host: " "/ssh:"))))
  (let ((bootstrap-vec (tramp-rpc-deploy--bootstrap-vec vec)))
    (when (tramp-rpc-deploy--remote-binary-exists-p bootstrap-vec)
      (tramp-send-command
       bootstrap-vec
       (format "rm -f %s"
               (tramp-shell-quote-argument
                (tramp-file-local-name
                 (tramp-rpc-deploy--remote-binary-path bootstrap-vec)))))
      (message "Removed %s from %s"
               tramp-rpc-deploy-binary-name
               (tramp-file-name-host vec)))))

(defun tramp-rpc-deploy-clear-cache ()
  "Clear the local binary cache."
  (interactive)
  (when (file-exists-p tramp-rpc-deploy-local-cache-directory)
    (delete-directory tramp-rpc-deploy-local-cache-directory t)
    (message "Cleared tramp-rpc binary cache")))

(defun tramp-rpc-deploy-status ()
  "Show the status of tramp-rpc-server binaries."
  (interactive)
  (let ((buf (get-buffer-create "*tramp-rpc-deploy-status*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "TRAMP-RPC Server Deployment Status\n")
      (insert "===================================\n\n")
      (insert (format "Version: %s\n" tramp-rpc-deploy-version))
      (insert (format "Local arch: %s\n" (tramp-rpc-deploy--detect-local-arch)))
      (insert (format "Cargo available: %s\n"
                      (if (tramp-rpc-deploy--cargo-available-p) "yes" "no")))
      (insert (format "Source directory: %s\n"
                      (or tramp-rpc-deploy-source-directory "not set")))
      (insert (format "Cache directory: %s\n\n" tramp-rpc-deploy-local-cache-directory))
      
      (insert "Cached Binaries:\n")
      (insert "----------------\n")
      (dolist (arch '("x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"))
        (let ((path (tramp-rpc-deploy--local-cache-path arch)))
          (insert (format "  %s: %s\n"
                          arch
                          (if (file-exists-p path)
                              (format "cached (%s)"
                                      (file-size-human-readable
                                       (file-attribute-size (file-attributes path))))
                            "not cached")))))
      (insert "\n")
      (insert "Download URLs:\n")
      (insert "--------------\n")
      (dolist (arch '("x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"))
        (insert (format "  %s:\n    %s\n" arch (tramp-rpc-deploy--download-url arch)))))
    (display-buffer buf)))

(provide 'tramp-rpc-deploy)
;;; tramp-rpc-deploy.el ends here
