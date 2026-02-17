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

(require 'cl-lib)
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

(defconst tramp-rpc-deploy-version "0.5.0"
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

(defconst tramp-rpc-deploy-bundled-binary-directory
  (when load-file-name
    (expand-file-name "binaries" (file-name-directory load-file-name)))
  "Directory containing pre-built binaries bundled with the package.
This is useful for development - binaries built by scripts/build-all.sh
are placed here and used directly without needing to download or cache.")

(defcustom tramp-rpc-deploy-remote-directory "~/.cache/tramp-rpc"
  "Remote directory where the server binary will be installed."
  :type 'string
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-auto-deploy t
  "If non-nil, automatically deploy the server binary when needed.
This has no effect when `tramp-rpc-deploy-never-deploy' is non-nil,
since that option takes precedence and disables all deployment."
  :type 'boolean
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-never-deploy nil
  "If non-nil, never deploy binaries to remote hosts.
This completely disables all binary deployment (downloading from
GitHub, building from source, and transferring to the remote).
When this is set, `tramp-rpc-deploy-auto-deploy' has no effect.

The server binary must already be installed on the remote host.
Use `tramp-rpc-deploy-remote-binary-path' to specify the full
path to the binary on the remote.  If that variable is nil, the
bare name \"tramp-rpc-server\" is used, which requires the binary
to be in the remote shell's PATH.

Note: SSH with BatchMode=yes may not source login shell profiles
\(e.g., ~/.profile), so PATH may be limited.  Setting
`tramp-rpc-deploy-remote-binary-path' to an absolute path is
recommended for reliability.

This option is useful for security-conscious setups where the
server is managed by the system package manager (e.g., Nix, Guix)
or manually installed.

To configure different paths for different hosts, use Emacs
connection-local variables."
  :type 'boolean
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-remote-binary-path nil
  "Explicit path to the tramp-rpc-server binary on the remote host.
When `tramp-rpc-deploy-never-deploy' is non-nil and this is set,
this path is used directly as the command in the SSH invocation.

Examples:
  \"/usr/bin/tramp-rpc-server\"
  \"/run/current-system/sw/bin/tramp-rpc-server\"
  \"/home/user/.nix-profile/bin/tramp-rpc-server\"

When nil, the bare name \"tramp-rpc-server\" is used, relying on
the remote shell's PATH to locate it."
  :type '(choice (const :tag "Use PATH lookup" nil)
                 (string :tag "Absolute path"))
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-prefer-build nil
  "If non-nil, prefer building from source over downloading.
By default, downloading is attempted first as it's faster."
  :type 'boolean
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-bootstrap-method "scpx"
  "TRAMP method to use for bootstrapping (deploying the binary).
This controls how the server binary is transferred to the remote host
and how shell commands are run during deployment.

Recommended methods:
  \"scp\"   - Uses the scp protocol for file transfer (out-of-band).
             Shell commands use a separate SSH session.  This is the
             default and most reliable option for transferring large
             binaries.
  \"rsync\" - Uses rsync for file transfer (out-of-band).  Requires
             rsync to be installed on both local and remote hosts.
             Efficient for repeated deployments due to delta transfer.

Legacy methods (use inline encoding for file transfer):
  \"sshx\"  - Encodes the binary as base64 and sends it through the
             shell session.  This can be fragile with large files due
             to PTY input buffer size limits.
  \"ssh\"   - Similar to sshx but with PTY allocation.  Same inline
             encoding limitations apply.
  \"scpx\"  - Like scp but uses a PTY for the shell session."
  :type '(choice (const :tag "SCP - out-of-band transfer (recommended)" "scp")
                 (const :tag "rsync - out-of-band transfer (requires rsync)" "rsync")
                 (const :tag "sshx - inline encoding (legacy)" "sshx")
                 (const :tag "ssh - inline encoding (legacy)" "ssh")
                 (const :tag "scpx - out-of-band with PTY shell" "scpx")
                 (string :tag "Other TRAMP method"))
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-max-retries 3
  "Maximum number of retries for binary transfer."
  :type 'integer
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-download-timeout 120
  "Timeout in seconds for downloading binaries."
  :type 'integer
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-debug nil
  "When non-nil, log verbose debug messages during deployment.
Messages are logged to *tramp-rpc-deploy* buffer."
  :type 'boolean
  :group 'tramp-rpc-deploy)

(defun tramp-rpc-deploy--log (format-string &rest args)
  "Log a debug message if `tramp-rpc-deploy-debug' is non-nil.
FORMAT-STRING and ARGS are passed to `format'."
  (when tramp-rpc-deploy-debug
    (with-current-buffer (get-buffer-create "*tramp-rpc-deploy*")
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] ")
              (apply #'format format-string args)
              "\n"))))

;;; ============================================================================
;;; Architecture detection and path helpers
;;; ============================================================================

(defun tramp-rpc-deploy--normalize-hops (hop-string)
  "Convert \"rpc:\" method references in HOP-STRING to \"ssh:\" for bootstrap.
The bootstrap vec uses standard TRAMP methods (sshx) which need ssh-compatible
hop methods for their own multi-hop traversal.
Preserves the trailing \"|\" that TRAMP uses in canonical hop format."
  (when hop-string
    (concat
     (mapconcat
      (lambda (hop-str)
        (replace-regexp-in-string
         (rx bos "rpc:") "ssh:" hop-str))
      (split-string hop-string tramp-postfix-hop-regexp 'omit)
      tramp-postfix-hop-format)
     tramp-postfix-hop-format)))

(defun tramp-rpc-deploy--bootstrap-vec (vec)
  "Convert VEC to use the bootstrap method for deployment operations.
This converts the rpc method to a standard TRAMP method for deployment.
The method used is controlled by `tramp-rpc-deploy-bootstrap-method'.
Methods like \"scp\" and \"rsync\" use out-of-band transfer for `copy-file',
while \"ssh\" and \"sshx\" use inline encoding (base64 through the shell).
Any \"rpc:\" hops in the hop chain are normalized to \"ssh:\" so that
standard TRAMP can traverse them."
  (let ((method (tramp-file-name-method vec)))
    (if (member method '("ssh" "sshx" "scp" "scpx" "rsync"))
        vec  ; Already a TRAMP method that supports shell commands and file transfer
      ;; Convert to bootstrap method - create a new tramp-file-name struct
      (make-tramp-file-name
       :method tramp-rpc-deploy-bootstrap-method
       :user (tramp-file-name-user vec)
       :domain (tramp-file-name-domain vec)
       :host (tramp-file-name-host vec)
       :port (tramp-file-name-port vec)
       :localname (tramp-file-name-localname vec)
       :hop (tramp-rpc-deploy--normalize-hops
             (tramp-file-name-hop vec))))))

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
E.g., \"x86_64-linux\" -> \"x86_64-unknown-linux-musl\".
Linux targets use musl for fully static binaries."
  (pcase arch
    ("x86_64-linux" "x86_64-unknown-linux-musl")
    ("aarch64-linux" "aarch64-unknown-linux-musl")
    ("x86_64-darwin" "x86_64-apple-darwin")
    ("aarch64-darwin" "aarch64-apple-darwin")
    (_ (signal 'remote-file-error (list "Unknown architecture: %s" arch)))))

(defun tramp-rpc-deploy--local-cache-path (arch)
  "Return the local cache path for binary of ARCH."
  (expand-file-name
   tramp-rpc-deploy-binary-name
   (expand-file-name
    arch
    (expand-file-name
     tramp-rpc-deploy-version
     tramp-rpc-deploy-local-cache-directory))))

(defun tramp-rpc-deploy--bundled-binary-path (arch)
  "Return the path to a bundled binary for ARCH, or nil if not available.
Bundled binaries are in lisp/binaries/<arch>/tramp-rpc-server.
This is useful for development - run scripts/build-all.sh to populate."
  (when tramp-rpc-deploy-bundled-binary-directory
    (let ((path (expand-file-name
                 tramp-rpc-deploy-binary-name
                 (expand-file-name arch tramp-rpc-deploy-bundled-binary-directory))))
      (when (and (file-exists-p path) (file-executable-p path))
        path))))

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
  (format "tramp-rpc-server-%s-%s.tar.gz"
          (tramp-rpc-deploy--arch-to-rust-target arch)
          tramp-rpc-deploy-version))

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
          (format "tramp-rpc-server-%s-%s.tar.gz.sha256"
                  (tramp-rpc-deploy--arch-to-rust-target arch)
                  tramp-rpc-deploy-version)))

(defun tramp-rpc-deploy--download-file (url dest)
  "Download URL to DEST synchronously.
Returns t on success, nil on failure."
  (condition-case err
      (let ((url-request-method "GET")
            (url-show-status nil))
        (message "Downloading %s..." url)
        (with-timeout (tramp-rpc-deploy-download-timeout
                       (signal 'remote-file-error
			       (list "Download timed out after %d seconds"
				     tramp-rpc-deploy-download-timeout)))
          (with-current-buffer (url-retrieve-synchronously url t t)
            (goto-char (point-min))
            ;; Check for HTTP errors
            (unless (re-search-forward "^HTTP/[0-9.]+ 200" nil t)
              (if (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                  (signal 'remote-file-error (list "HTTP error %s" (match-string 1)))
                (signal 'remote-file-error (list "Invalid HTTP response"))))
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
              (signal
	       'remote-file-error
	       (list "Download failed from %s (release may not exist)" tarball-url)))
            ;; Verify checksum if we got one
            (when checksum-ok
              (let ((expected (with-temp-buffer
                                (insert-file-contents checksum-path)
                                (buffer-string))))
                (unless (tramp-rpc-deploy--verify-checksum tarball-path expected)
                  (signal 'remote-file-error (list "Checksum verification failed")))))
            ;; Extract
            (message "Extracting binary...")
            (make-directory cache-dir t)
            (unless (tramp-rpc-deploy--extract-tarball tarball-path cache-dir)
              (signal 'remote-file-error (list "Failed to extract tarball")))
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
    (signal 'remote-file-error (list "Source directory not configured")))
  (unless (tramp-rpc-deploy--cargo-available-p)
    (signal 'remote-file-error (list "Rust toolchain (cargo) not found")))
  (unless (tramp-rpc-deploy--can-build-for-arch-p arch)
    (signal
     'remote-file-error
     (list "Cannot cross-compile for %s on %s"
           arch (tramp-rpc-deploy--detect-local-arch))))

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
          (signal
	   'remote-file-error
	   (list "Build failed (exit %d):\n%s" exit-code (buffer-string))))))))

;;; ============================================================================
;;; Main logic: ensure local binary exists
;;; ============================================================================

(defun tramp-rpc-deploy--ensure-local-binary (arch)
  "Ensure a local binary exists for ARCH.
Tries in order:
1. Check bundled binaries (useful for development)
2. Check local cache
3. Download from GitHub releases
4. Build from source (if on same architecture)

Returns the path to the local binary."
  (let ((bundled-path (tramp-rpc-deploy--bundled-binary-path arch))
        (cache-path (tramp-rpc-deploy--local-cache-path arch)))
    (cond
     ;; Check bundled binaries first (useful for development - run
     ;; scripts/build-all.sh to populate lisp/binaries/)
     (bundled-path
      (message "Using bundled binary for %s" arch)
      bundled-path)

     ;; Check cache
     ((and (file-exists-p cache-path)
           (file-executable-p cache-path))
      (message "Using cached binary for %s" arch)
      cache-path)

     ;; Need to obtain binary
     (t
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
            (signal
	     'remote-file-error
	     (list "Failed to obtain tramp-rpc-server for %s.\n\nErrors:\n%s\n\n%s"
                   arch
                   (mapconcat (lambda (e)
                                (format "  %s: %s" (car e) (cdr e)))
                              (reverse errors)
                              "\n")
                   (tramp-rpc-deploy--help-message arch)))))))))

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
  "Get SHA256 checksum of remote PATH on VEC.
Tries sha256sum first, then shasum -a 256 for macOS compatibility."
  ;; Try sha256sum first (Linux), then shasum -a 256 (macOS)
  (tramp-send-command vec
   (format "{ sha256sum %s 2>/dev/null || shasum -a 256 %s 2>/dev/null; } | cut -d' ' -f1"
           (tramp-shell-quote-argument path)
           (tramp-shell-quote-argument path)))
  (with-current-buffer (tramp-get-connection-buffer vec)
    (goto-char (point-min))
    ;; Match exactly 64 hex chars to avoid false positives from error messages
    (when (looking-at "\\([a-f0-9]\\{64\\}\\)")
      (match-string 1))))

(defun tramp-rpc-deploy--transfer-binary (vec local-path)
  "Transfer the binary at LOCAL-PATH to the remote host VEC.
Uses TRAMP's `copy-file' with the bootstrap method for binary transfer.
When the bootstrap method is \"scp\", \"scpx\", or \"rsync\", the transfer
uses out-of-band protocols (the actual scp/rsync binaries) which is fast
and reliable for large files.  With \"ssh\" or \"sshx\", TRAMP falls back
to inline encoding (base64 through the shell), which can be fragile."
  (let* ((remote-path (tramp-rpc-deploy--remote-binary-path vec))
         (remote-local (tramp-file-local-name remote-path))
         (remote-tmp-name (format "%s.tmp.%d"
                                  (file-name-nondirectory remote-local)
                                  (random 100000)))
         (remote-tmp-path (tramp-make-tramp-file-name
                           vec
                           ;; Use concat to preserve ~ for remote expansion
                           (concat (file-name-as-directory tramp-rpc-deploy-remote-directory)
                                   remote-tmp-name)))
         (remote-tmp-local (tramp-file-local-name remote-tmp-path))
         (local-checksum (tramp-rpc-deploy--compute-checksum local-path))
         (retries 0)
         (success nil)
         (errors nil))

    (tramp-rpc-deploy--log "Transfer starting: local=%s remote=%s (method: %s)"
                           local-path remote-local (tramp-file-name-method vec))
    (tramp-rpc-deploy--log "Local binary size: %d bytes, checksum: %s..."
                           (file-attribute-size (file-attributes local-path))
                           (substring local-checksum 0 16))

    ;; Ensure remote directory exists
    (tramp-rpc-deploy--ensure-remote-directory vec)

    (message "Transferring binary to %s:%s..." (tramp-file-name-host vec) remote-local)

    ;; Retry loop for reliability
    (while (and (not success) (< retries tramp-rpc-deploy-max-retries))
      (let ((attempt (1+ retries)))
        (message "Transfer attempt %d/%d..." attempt tramp-rpc-deploy-max-retries)
        (condition-case err
            (progn
              ;; Use TRAMP's copy-file for binary transfer via the bootstrap method.
              ;; With "scp"/"rsync" methods this uses out-of-band transfer
              ;; (actual scp/rsync binaries), avoiding inline base64 encoding.
              (copy-file local-path remote-tmp-path t)

              ;; Verify the file was created and has content
              (unless (tramp-send-command-and-check
                       vec
                       (format "test -s %s" (tramp-shell-quote-argument remote-tmp-local)))
                (signal
		 'remote-file-error
		 (list "Temp file not created or is empty after copy")))

              ;; Verify checksum
              (let ((remote-checksum (tramp-rpc-deploy--remote-checksum vec remote-tmp-local)))
                (unless remote-checksum
                  (signal
		   'remote-file-error
		   (list "Could not compute remote checksum (sha256sum/shasum not available?)")))
                (if (string= local-checksum remote-checksum)
                    (progn
                      ;; Checksum matches - make executable and atomically move
                      (tramp-send-command
                       vec
                       (format "chmod +x %s && mv -f %s %s"
                               (tramp-shell-quote-argument remote-tmp-local)
                               (tramp-shell-quote-argument remote-tmp-local)
                               (tramp-shell-quote-argument remote-local)))
                      (setq success t)
                      (message "Transfer completed successfully"))
                  ;; Checksum mismatch - clean up and retry
                  (let ((err-msg (format "Attempt %d: Checksum mismatch (local: %s, remote: %s)"
                                         attempt
                                         (substring local-checksum 0 12)
                                         (substring remote-checksum 0 12))))
                    (push err-msg errors)
                    (message "%s" err-msg))
                  (ignore-errors (delete-file remote-tmp-path))
                  (setq retries (1+ retries)))))
          (error
           ;; Clean up on error and retry
           (let ((err-msg (format "Attempt %d: %s" attempt (error-message-string err))))
             (push err-msg errors)
             (message "Transfer error: %s" err-msg))
           (ignore-errors (delete-file remote-tmp-path))
           (setq retries (1+ retries))))))

    (unless success
      (signal
       'remote-file-error
       (list "Failed to transfer binary after %d attempts.\n\nErrors:\n%s\n\nTroubleshooting:\n- Verify SSH access: ssh %s@%s echo success\n- Check write permissions to %s on remote host\n- Ensure sha256sum or shasum command is available on remote host"
             tramp-rpc-deploy-max-retries
             (mapconcat #'identity (nreverse errors) "\n")
             (or (tramp-file-name-user vec) "USER")
             (tramp-file-name-host vec)
             tramp-rpc-deploy-remote-directory)))

    remote-path))

;;; ============================================================================
;;; Public API
;;; ============================================================================

(defun tramp-rpc-deploy-expected-binary-localname ()
  "Return the expected remote binary localname without network access.
This computes the path deterministically from customization variables,
allowing `tramp-rpc--connect' to try connecting directly without
opening a bootstrap (scpx) connection for the deploy check."
  (concat (file-name-as-directory tramp-rpc-deploy-remote-directory)
          (format "%s-%s" tramp-rpc-deploy-binary-name tramp-rpc-deploy-version)))

(defun tramp-rpc-deploy-ensure-binary (vec)
  "Ensure the tramp-rpc-server binary is available on remote VEC.
Returns the remote path (or bare binary name) to the binary.

When `tramp-rpc-deploy-never-deploy' is non-nil, no deployment is
attempted.  Returns `tramp-rpc-deploy-remote-binary-path' if set,
otherwise the bare binary name \"tramp-rpc-server\".

Otherwise, if `tramp-rpc-deploy-auto-deploy' is nil and the binary
is missing, signals an error."
  (if tramp-rpc-deploy-never-deploy
      ;; Never deploy mode: use explicit path or bare binary name
      (let ((path (or tramp-rpc-deploy-remote-binary-path
                      tramp-rpc-deploy-binary-name)))
        (message "tramp-rpc: never-deploy mode, using %s on remote" path)
        path)
    ;; Normal deployment flow
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
          (signal
	   'remote-file-error
	   (list "tramp-rpc-server not found on %s and auto-deploy is disabled"
                 (tramp-file-name-host vec))))))))

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
      (insert (format "Never deploy: %s\n" (if tramp-rpc-deploy-never-deploy "yes" "no")))
      (when tramp-rpc-deploy-never-deploy
        (insert (format "Remote binary path: %s\n"
                        (or tramp-rpc-deploy-remote-binary-path
                            (format "%s (PATH lookup)" tramp-rpc-deploy-binary-name)))))
      (insert (format "Auto deploy: %s\n" (if tramp-rpc-deploy-auto-deploy "yes" "no")))
      (insert (format "Bootstrap method: %s%s\n"
                      tramp-rpc-deploy-bootstrap-method
                      (if (member tramp-rpc-deploy-bootstrap-method '("scp" "scpx" "rsync"))
                          " (out-of-band transfer)"
                        " (inline encoding)")))
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

(defun tramp-rpc-deploy-diagnose (host &optional user)
  "Run diagnostics for deploying to HOST.
Optional USER specifies the SSH user.
This helps troubleshoot deployment issues."
  (interactive "sHost: \nsUser (leave empty for default): ")
  (when (string-empty-p user)
    (setq user nil))
  (let ((buf (get-buffer-create "*tramp-rpc-diagnose*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "TRAMP-RPC Deployment Diagnostics for %s%s\n"
                      (if user (concat user "@") "") host))
      (insert "=" (make-string 50 ?=) "\n\n")

      (let ((test-num 1))
        ;; Bootstrap method
        (insert (format "%d. Bootstrap method configuration...\n" test-num))
        (insert (format "   Bootstrap method: %s\n" tramp-rpc-deploy-bootstrap-method))
        (if (member tramp-rpc-deploy-bootstrap-method '("scp" "scpx" "rsync"))
            (progn
              (insert "   [OK] Using out-of-band transfer (fast, reliable)\n")
              (when (string= tramp-rpc-deploy-bootstrap-method "rsync")
                (if (executable-find "rsync")
                    (insert "   [OK] Local rsync found\n")
                  (insert "   [WARN] Local rsync not found - transfer may fail\n"))))
          (insert "   [WARN] Using inline encoding - may be slow/fragile for large binaries\n")
          (insert "   Consider: (setq tramp-rpc-deploy-bootstrap-method \"scp\")\n"))

        ;; SSH connectivity
        (cl-incf test-num)
        (insert (format "\n%d. Testing SSH connectivity...\n" test-num))
        (let* ((ssh-cmd (append
                         (list "ssh" "-o" "BatchMode=yes" "-o" "ConnectTimeout=10")
                         (when user (list "-l" user))
                         (list host "echo 'SSH_OK'")))
               (output (with-temp-buffer
                         (apply #'call-process (car ssh-cmd) nil t nil (cdr ssh-cmd))
                         (buffer-string))))
          (if (string-match-p "SSH_OK" output)
              (insert "   [OK] SSH connection successful\n")
            (insert "   [FAIL] SSH connection failed\n")
            (insert (format "   Output: %s\n" (string-trim output)))))

        ;; Remote architecture
        (cl-incf test-num)
        (insert (format "\n%d. Detecting remote architecture...\n" test-num))
        (let* ((ssh-cmd (append
                         (list "ssh" "-o" "BatchMode=yes")
                         (when user (list "-l" user))
                         (list host "uname -m && uname -s")))
               (output (with-temp-buffer
                         (if (zerop (apply #'call-process (car ssh-cmd) nil t nil (cdr ssh-cmd)))
                             (buffer-string)
                           "FAILED"))))
          (if (string-match-p "FAILED" output)
              (insert "   [FAIL] Could not detect architecture\n")
            (insert (format "   [OK] Architecture: %s\n" (string-trim output)))))

        ;; Remote directory writable
        (cl-incf test-num)
        (insert (format "\n%d. Testing remote directory access...\n" test-num))
        (let* ((dir tramp-rpc-deploy-remote-directory)
               (ssh-cmd (append
                         (list "ssh" "-o" "BatchMode=yes")
                         (when user (list "-l" user))
                         (list host (format "mkdir -p %s && test -w %s && echo 'WRITABLE'"
                                            (shell-quote-argument dir)
                                            (shell-quote-argument dir)))))
               (output (with-temp-buffer
                         (apply #'call-process (car ssh-cmd) nil t nil (cdr ssh-cmd))
                         (buffer-string))))
          (if (string-match-p "WRITABLE" output)
              (insert (format "   [OK] Directory %s is writable\n" dir))
            (insert (format "   [FAIL] Directory %s not writable\n" dir))))

        ;; Checksum command
        (cl-incf test-num)
        (insert (format "\n%d. Testing checksum command availability...\n" test-num))
        (let* ((ssh-cmd (append
                         (list "ssh" "-o" "BatchMode=yes")
                         (when user (list "-l" user))
                         (list host "which sha256sum || which shasum || echo 'NONE'")))
               (output (with-temp-buffer
                         (apply #'call-process (car ssh-cmd) nil t nil (cdr ssh-cmd))
                         (string-trim (buffer-string)))))
          (if (string-match-p "NONE" output)
              (insert "   [FAIL] No checksum command found (need sha256sum or shasum)\n")
            (insert (format "   [OK] Found: %s\n" output))))

        ;; Conditional: rsync availability (when using rsync bootstrap method)
        (when (string= tramp-rpc-deploy-bootstrap-method "rsync")
          (cl-incf test-num)
          (insert (format "\n%d. Testing rsync availability on remote...\n" test-num))
          (let* ((ssh-cmd (append
                           (list "ssh" "-o" "BatchMode=yes")
                           (when user (list "-l" user))
                           (list host "which rsync || echo 'NONE'")))
                 (output (with-temp-buffer
                           (apply #'call-process (car ssh-cmd) nil t nil (cdr ssh-cmd))
                           (string-trim (buffer-string)))))
            (if (string-match-p "NONE" output)
                (insert "   [FAIL] rsync not found on remote (needed for rsync bootstrap method)\n")
              (insert (format "   [OK] Found: %s\n" output)))))

        ;; Local binary availability
        (cl-incf test-num)
        (insert (format "\n%d. Checking local binary cache...\n" test-num))
        (dolist (arch '("x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"))
          (let ((path (tramp-rpc-deploy--local-cache-path arch))
                (bundled (tramp-rpc-deploy--bundled-binary-path arch)))
            (cond
             ((and bundled (file-exists-p bundled))
              (insert (format "   [OK] %s: bundled binary available\n" arch)))
             ((file-exists-p path)
              (insert (format "   [OK] %s: cached at %s\n" arch path)))
             (t
              (insert (format "   [ ] %s: not available locally\n" arch))))))

        (insert "\n\nIf deployment fails, try:\n")
        (insert "  1. Enable debug logging: (setq tramp-rpc-deploy-debug t)\n")
        (insert "  2. Retry the connection and check *tramp-rpc-deploy* buffer\n")
        (insert "  3. Manually test: ssh " (if user (concat user "@") "") host " echo success\n")))
    (display-buffer buf)))

(provide 'tramp-rpc-deploy)
;;; tramp-rpc-deploy.el ends here
