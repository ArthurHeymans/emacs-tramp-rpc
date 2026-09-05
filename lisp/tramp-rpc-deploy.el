;;; tramp-rpc-deploy.el --- Binary deployment for TRAMP-RPC -*- lexical-binding: t; -*-

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
(require 'tramp-rpc-protocol)
(require 'tramp-rpc-hops)
(require 'tramp-sh)
(require 'url)

;; Silence byte-compiler warnings for functions defined in tramp-sh.
(declare-function tramp-get-remote-path "tramp-sh")
(declare-function tramp-get-connection-process "tramp")
(declare-function tramp-check-for-regexp "tramp")
(declare-function tramp-get-remote-null-device "tramp")

;; Functions from sibling modules.  `tramp-rpc-deploy' is loaded by
;; tramp-rpc-transport.el before the complete backend is initialized.
(declare-function tramp-send-command
                  "tramp-sh" (vec command &optional neveropen nooutput))
(declare-function tramp-send-command-and-check
                  "tramp-sh"
                  (vec command &optional subshell dont-suppress-err exit-status))
(declare-function tramp-send-command-and-read
                  "tramp-sh" (vec command &optional noerror marker))
(declare-function tramp-rpc--proxy-hop-string "tramp-rpc-hops" (vec))
(declare-function tramp-rpc--sudo-rpc-hop-vec "tramp-rpc-hops" (vec))
(declare-function tramp-rpc--clear-connection-failure
                  "tramp-rpc-transport" (vec))

;; ============================================================================
;;; Customization
;; ============================================================================

(defun tramp-rpc-deploy--load-source-file-name ()
  "Return the Elisp source file corresponding to `load-file-name'.
When packages are byte-compiled, `load-file-name' points at the .elc in the
build directory.  Package managers such as straight.el keep an adjacent .el
symlink to the real checkout, so prefer that source file and follow symlinks
before deriving the project root."
  (when load-file-name
    (let* ((base (file-name-sans-extension load-file-name))
           (source (concat base ".el"))
           (file (if (file-exists-p source) source load-file-name)))
      (file-truename file))))

(defun tramp-rpc-deploy--default-source-directory ()
  "Return the default tramp-rpc source directory.
MELPA flattens Lisp files into the package root alongside the Rust sources,
while source checkouts keep them in a Lisp subdirectory.  Following source-file
symlinks is important for straight.el/Doom builds: the loaded .elc lives in
straight/build..., while the adjacent .el symlink points back to
straight/repos...."
  (when-let* ((file (tramp-rpc-deploy--load-source-file-name))
              (directory (file-name-directory file)))
    (let ((parent (expand-file-name ".." directory)))
      (cond
       ((and (file-exists-p (expand-file-name "Cargo.toml" directory))
             (file-directory-p (expand-file-name "server" directory)))
        directory)
       ((and (file-exists-p (expand-file-name "Cargo.toml" parent))
             (file-directory-p (expand-file-name "server" parent)))
        parent)
       ((string= (file-name-nondirectory (directory-file-name directory))
                 "lisp")
        parent)
       ;; Archive/package installs commonly flatten the selected Elisp files
       ;; into the package directory.  Keep that directory as the anchor for
       ;; the release-source fallback instead of pointing at its parent.
       (t directory)))))

(defgroup tramp-rpc-deploy nil
  "Deployment settings for TRAMP-RPC."
  :group 'tramp)

(defconst tramp-rpc-deploy-version "0.13.1"
  "Current version of tramp-rpc-server.")

;; Refer to .github/workflows/release.yml for supported architectures
(defconst tramp-rpc-deploy-release-architectures
  '("x86_64-linux" "aarch64-linux" "i686-linux"
    "armv7-linux" "armv5te-linux" "armv6-linux"
    "x86_64-darwin" "aarch64-darwin")
  "Architecture names for which release artifacts are published.")

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

(defcustom tramp-rpc-deploy-source-url-format
  "https://github.com/%s/archive/refs/tags/v%s.tar.gz"
  "URL format for downloading a tagged source archive.
Arguments: repo and version."
  :type 'string
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-local-cache-directory
  (expand-file-name "tramp-rpc" user-emacs-directory)
  "Local directory for caching downloaded/built binaries.
Binaries are stored as CACHE-DIR/VERSION/ARCH/tramp-rpc-server."
  :type 'directory
  :group 'tramp-rpc-deploy)

(defcustom tramp-rpc-deploy-source-directory
  (tramp-rpc-deploy--default-source-directory)
  "Directory containing the tramp-rpc source code, when available.
For archive/package installs this is an automatically inferred package
directory used as the anchor for downloading the matching release source
archive.  Set to nil to disable all source builds."
  :type '(choice directory (const nil))
  :group 'tramp-rpc-deploy)

(defconst tramp-rpc-deploy-bundled-binary-directory
  (when load-file-name
    (expand-file-name "binaries" (file-name-directory load-file-name)))
  "Directory containing pre-built binaries bundled with the package.
This is useful for development - binaries built by scripts/build-all.sh
are placed here and used directly without needing to download or cache.")

(defcustom tramp-rpc-deploy-remote-directory "~/.cache/emacs/tramp-rpc"
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

(defcustom tramp-rpc-deploy-git-build-policy 'auto
  "How to obtain server binaries when running from a git checkout.
This only applies when `tramp-rpc-deploy-source-directory' points at a
git checkout that contains the Rust server sources.

`auto' means use release binaries for release/package installs.  For git
checkouts, existing source builds and caches are reused; when a binary must
be obtained, `tramp-rpc-deploy-install-binary' asks whether to download a
release binary, build locally, build remotely when supported, or stop.  An
interactive automatic deployment asks whether to download or build remotely
when the remote platform is supported and has a usable Rust toolchain.

`release' always uses the release-oriented versioned binary id and obtain
order, preserving the historical behavior.

`build' always uses a source-tree keyed binary id for git checkouts and
only builds from source; release downloads are not used as a fallback."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Release binaries" release)
                 (const :tag "Build from source" build))
  :group 'tramp-rpc-deploy)

(defvar tramp-rpc-deploy--allow-prompt nil
  "Non-nil while deploying the target of an explicit installation.
Only `tramp-rpc-deploy-ensure-binary' binds this, and only for the remote
that `tramp-rpc-deploy-install-binary' was invoked for.")

(defvar tramp-rpc-deploy--force-obtain nil
  "Non-nil while the current deploy must replace existing artifacts.
Scoped exactly like `tramp-rpc-deploy--allow-prompt'.")

(defvar tramp-rpc-deploy--explicit-target nil
  "Target key of the in-progress explicit installation, or nil.
Explicit installation can block for a long time in `read-char-choice',
downloads, or builds, during which timers may deploy to other remotes.
Keying the request to one target keeps those reentrant deploys automatic.")

(defvar tramp-rpc-deploy--explicit-force nil
  "Non-nil when the in-progress explicit installation must replace artifacts.")

(defvar tramp-rpc-deploy--pre-explicit-auto-deploy nil
  "Value of `tramp-rpc-deploy-auto-deploy' before the explicit installation.
Reentrant deploys for other remotes during an explicit installation use this
value so the explicit auto-deploy override does not leak to them.")

(defun tramp-rpc-deploy--target-key (vec)
  "Return a comparable deployment target key for VEC."
  (if (tramp-file-name-p vec)
      (list (tramp-file-name-method vec)
            (tramp-file-name-user vec)
            (tramp-file-name-host vec)
            (tramp-file-name-port vec)
            (tramp-file-name-hop vec))
    vec))

(defun tramp-rpc-deploy--explicit-target-p (vec)
  "Return non-nil when VEC is the target of the explicit installation."
  (and tramp-rpc-deploy--explicit-target
       (equal tramp-rpc-deploy--explicit-target
              (tramp-rpc-deploy--target-key vec))))

(defcustom tramp-rpc-deploy-bootstrap-method "scpx"
  "TRAMP method to use for bootstrapping (deploying the binary).
This controls how the server binary is transferred to the remote host
and how shell commands are run during deployment.

Recommended methods:
  \"scp\"   - Uses the scp protocol for file transfer (out-of-band).
             Shell commands use a separate SSH session.  This is a
             reliable option for transferring large binaries.
  \"rsync\" - Uses rsync for file transfer (out-of-band).  Requires
             rsync to be installed on both local and remote hosts.
             Efficient for repeated deployments due to delta transfer.

Legacy methods (use inline encoding for file transfer):
  \"sshx\"  - Encodes the binary as base64 and sends it through the
             shell session.  This can be fragile with large files due
             to PTY input buffer size limits.
  \"ssh\"   - Similar to sshx but with PTY allocation.  Same inline
             encoding limitations apply.
  \"scpx\"  - Like scp but uses a PTY for the shell session; this is the
             default."
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
Messages are logged to *tramp-rpc-deploy*.  TRAMP_RPC_DEPLOY_DEBUG_LOG names
an additional log file; otherwise TRAMP_RPC_DEBUG_DIR writes
tramp-rpc-deploy-live.log in that directory."
  :type 'boolean
  :group 'tramp-rpc-deploy)

(defvar tramp-rpc-deploy--remote-rust-toolchain-diagnostic nil
  "Diagnostic from the most recent remote Rust toolchain probe.
This is nil after a successful probe and a user-facing explanation when the
probe found no usable toolchain or could not be completed.")

(defun tramp-rpc-deploy--log (format-string &rest args)
  "Log a debug message if `tramp-rpc-deploy-debug' is non-nil.
FORMAT-STRING and ARGS are passed to `format'."
  (when tramp-rpc-deploy-debug
    (let* ((line (concat (format-time-string "[%F %T] ")
                         (apply #'format format-string args)
                         "\n"))
           (log-file (or (getenv "TRAMP_RPC_DEPLOY_DEBUG_LOG")
                         (when-let* ((dir (getenv "TRAMP_RPC_DEBUG_DIR")))
                           (expand-file-name "tramp-rpc-deploy-live.log" dir)))))
      (with-current-buffer (get-buffer-create "*tramp-rpc-deploy*")
        (goto-char (point-max))
        (insert line))
      (when log-file
        (condition-case nil
            (progn
              (when-let* ((directory (file-name-directory log-file)))
                (make-directory directory t))
              (write-region line nil log-file 'append 'silent))
          (error nil))))))

;; ============================================================================
;;; Architecture detection and path helpers
;; ============================================================================

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
         (rx bos "rpc" (literal tramp-postfix-method-format))
	 (concat "ssh" tramp-postfix-method-format) hop-str))
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
  (let ((method (tramp-file-name-method vec))
        (sudo-hop (and (fboundp 'tramp-rpc--sudo-rpc-hop-vec)
                       (tramp-rpc--sudo-rpc-hop-vec vec))))
    (if (and (not sudo-hop)
             (member method '("ssh" "sshx" "scp" "scpx" "rsync")))
        vec  ; Already a TRAMP method that supports shell commands and file transfer
      ;; Convert to bootstrap method - create a new tramp-file-name struct.
      ;; For /rpc:user@host|sudo:root@host: paths, deployment must still happen
      ;; over the SSH user from the rpc hop.  The root RPC server is started via
      ;; sudo later; using root here makes TRAMP try an unrelated scpx root
      ;; login and fails before sudo authentication can help.
      (make-tramp-file-name
       :method tramp-rpc-deploy-bootstrap-method
       :user (or (and sudo-hop
                      (or (tramp-file-name-user sudo-hop) (user-login-name)))
                 (tramp-file-name-user vec))
       :domain (tramp-file-name-domain vec)
       :host (tramp-file-name-host vec)
       :port (if sudo-hop
                 (tramp-file-name-port sudo-hop)
               (tramp-file-name-port vec))
       :localname (tramp-file-name-localname vec)
       :hop (tramp-rpc-deploy--normalize-hops
             (if sudo-hop
                 (tramp-rpc--proxy-hop-string vec)
               (tramp-file-name-hop vec)))))))

(defconst tramp-rpc-deploy--architecture-aliases
  '(("amd64" . "x86_64")
    ("arm64" . "aarch64")
    ("armv7l" . "armv7")
    ("armv6l" . "arm")
    ("armv5tel" . "armv5te"))
  "Machine-name aliases accepted during architecture detection.")

(defconst tramp-rpc-deploy--artifact-targets
  '(("x86_64-linux" . "x86_64-unknown-linux-musl")
    ("aarch64-linux" . "aarch64-unknown-linux-musl")
    ("i686-linux" . "i686-unknown-linux-musl")
    ("armv7-linux" . "armv7-unknown-linux-musleabihf")
    ("armv5te-linux" . "armv5te-unknown-linux-musleabi")
    ("arm-linux" . "arm-unknown-linux-musleabihf")
    ("x86_64-darwin" . "x86_64-apple-darwin")
    ("aarch64-darwin" . "aarch64-apple-darwin"))
  "Supported release artifact architectures and Rust target triples.")

(defun tramp-rpc-deploy--normalize-machine (machine)
  "Return canonical spelling for MACHINE while accepting known aliases."
  (or (cdr (assoc machine tramp-rpc-deploy--architecture-aliases)) machine))

(defun tramp-rpc-deploy--supported-architectures ()
  "Return supported release artifact architecture names."
  (mapcar #'car tramp-rpc-deploy--artifact-targets))

(defun tramp-rpc-deploy--rust-target-for-arch (arch)
  "Return the release Rust target triple for ARCH, or nil.
Accept the historical `armv6-linux' spelling as an alias for `arm-linux'."
  (cdr (assoc (if (string= arch "armv6-linux") "arm-linux" arch)
              tramp-rpc-deploy--artifact-targets)))

(defun tramp-rpc-deploy--detect-remote-arch (vec)
  "Detect the architecture of remote host specified by VEC.
Returns a string like \"x86_64-linux\" or \"aarch64-darwin\"."
  (let* ((uname-m (string-trim
                   (tramp-send-command-and-read
                    vec "echo \\\"`uname -m`\\\"")))
         (uname-s (string-trim
                   (tramp-send-command-and-read
                    vec "echo \\\"`uname -s`\\\"")))
         (machine (tramp-rpc-deploy--normalize-machine uname-m))
         (os (downcase uname-s)))
    (format "%s-%s" machine os)))

(defun tramp-rpc-deploy--detect-local-arch ()
  "Detect the architecture of the local system.
Returns a string like \"x86_64-linux\" or \"aarch64-darwin\"."
  (let* ((os (pcase system-type
               ('gnu/linux "linux")
               ('darwin "darwin")
               (_ (symbol-name system-type))))
         (machine (car (split-string system-configuration "-")))
         (normalized-machine (tramp-rpc-deploy--normalize-machine machine)))
    (format "%s-%s" normalized-machine os)))

(defun tramp-rpc-deploy--arch-to-rust-target (arch)
  "Convert ARCH string to Rust target triple.
E.g., \"x86_64-linux\" -> \"x86_64-unknown-linux-musl\".
Linux targets use musl for fully static binaries."
  (or (tramp-rpc-deploy--rust-target-for-arch arch)
      (signal 'remote-file-error (list "Unknown architecture" arch))))

(defun tramp-rpc-deploy--platform-supported-p (arch)
  "Return non-nil when ARCH has an official release artifact."
  (and (tramp-rpc-deploy--rust-target-for-arch arch) t))

(defun tramp-rpc-deploy--source-root ()
  "Return the configured source root as a directory name, or nil."
  (when tramp-rpc-deploy-source-directory
    (file-name-as-directory (expand-file-name tramp-rpc-deploy-source-directory))))

(defun tramp-rpc-deploy--source-has-server-p (&optional source-root)
  "Return non-nil if SOURCE-ROOT has the Rust server sources.
When SOURCE-ROOT is nil, inspect `tramp-rpc-deploy-source-directory'."
  (let ((root (or source-root (tramp-rpc-deploy--source-root))))
    (and root
         (file-exists-p (expand-file-name "Cargo.toml" root))
         (file-directory-p (expand-file-name "server" root)))))

(defun tramp-rpc-deploy--git-checkout-p ()
  "Return non-nil if the source directory is inside a git checkout."
  (let ((root (tramp-rpc-deploy--source-root)))
    (and root (locate-dominating-file root ".git"))))

(defun tramp-rpc-deploy--source-file-list (&optional source-root)
  "Return files that affect the server build, relative to SOURCE-ROOT.
When SOURCE-ROOT is nil, inspect `tramp-rpc-deploy-source-directory'."
  (let* ((root (or source-root (tramp-rpc-deploy--source-root)))
         (files nil))
    (when root
      (dolist (name '("Cargo.toml" "Cargo.lock"))
        (let ((file (expand-file-name name root)))
          (when (file-regular-p file)
            (push file files))))
      (dolist (name '("server" ".cargo"))
        (let ((dir (expand-file-name name root)))
          (when (file-directory-p dir)
            (dolist (file (directory-files-recursively dir ""))
              (when (and (file-regular-p file)
                         (not (backup-file-name-p file))
                         (not (string-prefix-p
                               ".#" (file-name-nondirectory file))))
                (push file files)))))))
    (sort files #'string<)))

(defun tramp-rpc-deploy--source-tree-hash ()
  "Return a SHA256 hash for files that affect the server build, or nil.
Hash contents on every call: names, sizes, and mtimes cannot reliably detect
same-size edits made within coarse timestamp resolution or by tools that
preserve timestamps."
  (let ((root (tramp-rpc-deploy--source-root))
        (files (tramp-rpc-deploy--source-file-list)))
    (when (and root files)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (dolist (file files)
          (insert (file-relative-name file root) "\0")
          (let ((coding-system-for-read 'binary))
            (insert-file-contents-literally file))
          (insert "\0"))
        (secure-hash 'sha256 (current-buffer))))))

(defun tramp-rpc-deploy--git-revision ()
  "Return the short git revision for the source checkout, or nil."
  (let ((root (tramp-rpc-deploy--source-root)))
    (when (and root
               (tramp-rpc-deploy--git-checkout-p)
               (executable-find "git"))
      (with-temp-buffer
        (if (zerop (call-process "git" nil t nil
                                 "-C" root "rev-parse" "--short=12" "HEAD"))
            (string-trim (buffer-string))
          (tramp-rpc-deploy--log "git rev-parse failed: %s"
                                 (string-trim (buffer-string)))
          nil)))))

(defun tramp-rpc-deploy--use-source-binary-id-p ()
  "Return non-nil when binaries should be keyed by source content."
  (and (memq tramp-rpc-deploy-git-build-policy '(auto build))
       (tramp-rpc-deploy--source-has-server-p)
       (tramp-rpc-deploy--git-checkout-p)
       t))

(defun tramp-rpc-deploy--source-directory-warning ()
  "Return a warning string for suspicious source-build auto-detection."
  (when (and (memq tramp-rpc-deploy-git-build-policy '(auto build))
             tramp-rpc-deploy-source-directory
             (not (tramp-rpc-deploy--source-has-server-p)))
    (format "Source directory %s does not contain Cargo.toml and server/; using release binary id %s.  Release source archives will be used for remote builds.  Set `tramp-rpc-deploy-source-directory' to the package checkout if this is a git install."
            (abbreviate-file-name (tramp-rpc-deploy--source-root))
            tramp-rpc-deploy-version)))

(defun tramp-rpc-deploy--source-binary-id ()
  "Return a binary id derived from the current git checkout contents."
  (let ((hash (tramp-rpc-deploy--source-tree-hash)))
    (when hash
      (format "git-%s-%s%s"
              (or (tramp-rpc-deploy--git-revision) "unknown")
              (substring hash 0 12)
              (if (eq tramp-rpc-deploy-git-build-policy 'build)
                  "-build"
                "")))))

(defun tramp-rpc-deploy--binary-id ()
  "Return the id used for cache and remote binary paths.
Release installs use `tramp-rpc-deploy-version'.  Git checkouts use a
source-tree keyed id so latest-git users do not reuse stale release
artifacts when the Rust server changes without a version bump."
  (or (and (tramp-rpc-deploy--use-source-binary-id-p)
           (tramp-rpc-deploy--source-binary-id))
      tramp-rpc-deploy-version))

(defun tramp-rpc-deploy--local-cache-path (arch)
  "Return the local cache path for binary of ARCH."
  (expand-file-name
   tramp-rpc-deploy-binary-name
   (expand-file-name
    arch
    (expand-file-name
     (tramp-rpc-deploy--binary-id)
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

(defun tramp-rpc-deploy--newer-than-source-p (file)
  "Return non-nil if FILE is newer than all known server source files."
  (let ((file-time (file-attribute-modification-time (file-attributes file)))
        (sources (tramp-rpc-deploy--source-file-list)))
    (cl-loop for source in sources
             always (not (time-less-p
                          file-time
                          (file-attribute-modification-time
                           (file-attributes source)))))))

(defun tramp-rpc-deploy--source-build-output-path (arch)
  "Return an existing source-tree build output for ARCH, or nil.
This lets CI and developers reuse an already-built/downloaded artifact in
TARGET/release without requiring a rebuild, while skipping obviously stale
outputs whose mtime predates the source files."
  (when (and (tramp-rpc-deploy--source-root)
             (tramp-rpc-deploy--source-has-server-p))
    (let* ((target (tramp-rpc-deploy--rust-target-for-arch arch))
           (output-directory
            (cond
             (target (format "target/%s/release" target))
             ;; An unqualified target/release is only native to the local
             ;; host.  Never reuse it for an unknown remote architecture.
             ((string= arch (tramp-rpc-deploy--detect-local-arch))
              "target/release"))))
      (when output-directory
        (let* ((root (tramp-rpc-deploy--source-root))
               (path (expand-file-name
                      tramp-rpc-deploy-binary-name
                      (expand-file-name output-directory root))))
          (when (and (file-exists-p path)
                     (file-executable-p path)
                     (tramp-rpc-deploy--newer-than-source-p path))
            path))))))

(defun tramp-rpc-deploy--remote-binary-path (vec)
  "Return the remote path where the binary should be installed for VEC."
  (tramp-make-tramp-file-name
   vec
   ;; Use concat instead of expand-file-name to preserve ~ for remote expansion.
   ;; expand-file-name would expand ~ to the LOCAL user's home directory,
   ;; causing failures when local and remote usernames differ.
   (concat (file-name-as-directory tramp-rpc-deploy-remote-directory)
           (format "%s-%s"
                   tramp-rpc-deploy-binary-name
                   (tramp-rpc-deploy--binary-id)))))

;; ============================================================================
;;; Download from GitHub Releases
;; ============================================================================

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

(defun tramp-rpc-deploy--source-download-url ()
  "Return the URL for the source archive matching the package version."
  (format tramp-rpc-deploy-source-url-format
          tramp-rpc-deploy-github-repo
          tramp-rpc-deploy-version))

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
                               (list (format "Download timed out after %d seconds"
                                             tramp-rpc-deploy-download-timeout))))
          (let ((buffer (url-retrieve-synchronously url t t)))
            (unless buffer
              (signal 'remote-file-error (list "No HTTP response" url)))
            (unwind-protect
                (with-current-buffer buffer
                  (goto-char (point-min))
                  (unless (looking-at "HTTP/[0-9.]+ 200\\(?:[ \t]\\|$\\)")
                    (if (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
                        (signal 'remote-file-error (list "HTTP error" (match-string 1)))
                      (signal 'remote-file-error (list "Invalid HTTP response"))))
                  (unless (re-search-forward "\r?\n\r?\n" nil t)
                    (signal 'remote-file-error (list "Malformed HTTP response")))
                  (let ((coding-system-for-write 'binary))
                    (write-region (point) (point-max) dest nil 'silent))
                  t)
              (when (buffer-live-p buffer)
                (kill-buffer buffer))))))
    (error
     (message "Download failed: %s" (error-message-string err))
     nil)))

(defun tramp-rpc-deploy--release-checksum (metadata asset)
  "Return the SHA256 in METADATA for exactly ASSET.
Release metadata must be one standard sha256sum line.  Rejecting ambiguous
or differently named entries prevents a valid checksum for another artifact
from authorizing this download."
  (let ((lines (split-string (string-trim metadata) "[\r\n]+" t)))
    (unless (= (length lines) 1)
      (signal 'remote-file-error (list "Malformed checksum metadata")))
    (let ((line (car lines)))
      (unless (string-match
               "\\`\\([[:xdigit:]]\\{64\\}\\)[[:space:]]+\\*?\\([^[:space:]]+\\)\\'"
               line)
        (signal 'remote-file-error (list "Malformed checksum metadata")))
      (let ((checksum (downcase (match-string 1 line)))
            (named-asset (match-string 2 line)))
        (unless (string= named-asset asset)
          (signal 'remote-file-error
                  (list "Checksum metadata names" named-asset "not" asset)))
        checksum))))

(defun tramp-rpc-deploy--verify-checksum (file expected-checksum)
  "Verify that FILE has the SHA256 EXPECTED-CHECKSUM.
Returns t if checksum matches, nil otherwise."
  (when (and file (file-exists-p file)
             (stringp expected-checksum)
             (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" expected-checksum))
    (string= (tramp-rpc-deploy--compute-checksum file)
             (downcase expected-checksum))))

(defun tramp-rpc-deploy--cache-provenance-path (cache-path)
  "Return the provenance sidecar path for CACHE-PATH."
  (concat cache-path ".provenance"))

(defun tramp-rpc-deploy--write-file-atomically (path contents)
  "Write CONTENTS to PATH without exposing a partial file."
  (let ((temp-path (make-temp-file (concat path ".tmp."))))
    (unwind-protect
        (progn
          (with-temp-file temp-path
            (insert contents))
          (rename-file temp-path path t)
          (setq temp-path nil))
      (when temp-path
        (condition-case nil
            (delete-file temp-path)
          (file-missing nil))))))

(defun tramp-rpc-deploy--write-cache-provenance (cache-path kind digest)
  "Atomically record KIND and SHA256 DIGEST for CACHE-PATH."
  (unless (and (member kind '("release" "source-build"))
               (stringp digest)
               (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" digest))
    (signal 'remote-file-error (list "Invalid cache provenance")))
  (tramp-rpc-deploy--write-file-atomically
   (tramp-rpc-deploy--cache-provenance-path cache-path)
   (format "%s-sha256:%s\n" kind (downcase digest))))

(defun tramp-rpc-deploy--invalidate-cache (cache-path)
  "Remove CACHE-PATH and its provenance after failed source-cache validation."
  (dolist (path (list cache-path
                      (tramp-rpc-deploy--cache-provenance-path cache-path)))
    (condition-case nil
        (delete-file path)
      (file-missing nil))))

(defun tramp-rpc-deploy--promote-cached-binary (source cache-path kind)
  "Atomically promote SOURCE to CACHE-PATH with KIND and a recorded digest.
A crash after removing provenance can leave an untrusted cache entry, but
never one authorized by stale provenance."
  (make-directory (file-name-directory cache-path) t)
  (let ((temp-path (make-temp-file (concat cache-path ".tmp.")))
        (provenance-path (tramp-rpc-deploy--cache-provenance-path cache-path)))
    (unwind-protect
        (progn
          (copy-file source temp-path t)
          (set-file-modes temp-path #o755)
          (let ((digest (tramp-rpc-deploy--compute-checksum temp-path)))
            ;; Remove stale authority before changing the cache binary.
            (condition-case nil
                (delete-file provenance-path)
              (file-missing nil))
            (rename-file temp-path cache-path t)
            (setq temp-path nil)
            (tramp-rpc-deploy--write-cache-provenance cache-path kind digest)
            cache-path))
      (when temp-path
        (condition-case nil
            (delete-file temp-path)
          (file-missing nil))))))

(defun tramp-rpc-deploy--cached-binary-trusted-p (cache-path)
  "Return non-nil when CACHE-PATH matches a recorded provenance digest."
  (let* ((provenance-path (tramp-rpc-deploy--cache-provenance-path cache-path))
         (provenance (and (file-exists-p provenance-path)
                          (with-temp-buffer
                            (insert-file-contents-literally provenance-path)
                            (buffer-string)))))
    (cond
     ((and provenance
           (string-match
            "\\`\\(release\\|source-build\\)-sha256:\\([[:xdigit:]]\\{64\\}\\)\n?\\'"
            provenance))
      (let ((kind (match-string 1 provenance))
            (digest (match-string 2 provenance)))
        (if (tramp-rpc-deploy--verify-checksum cache-path digest)
            t
          (when (string= kind "source-build")
            (tramp-rpc-deploy--invalidate-cache cache-path))
          nil)))
     ;; Legacy and malformed source-build records must never authorize reuse.
     ((and provenance (string-prefix-p "source-build" provenance))
      (tramp-rpc-deploy--invalidate-cache cache-path)
      nil))))

(defun tramp-rpc-deploy--extract-tarball (tarball dest-dir)
  "Extract TARBALL to DEST-DIR.
Returns the path to the extracted binary, or nil on failure."
  (let ((default-directory dest-dir))
    (make-directory dest-dir t)
    ;; Extract only the expected member, so unrelated archive paths cannot
    ;; escape DEST-DIR or place content that influences promotion.
    (if (zerop (call-process "tar" nil nil nil "-xzf" tarball "-C" dest-dir
                             "--" tramp-rpc-deploy-binary-name))
        (let* ((binary (expand-file-name tramp-rpc-deploy-binary-name dest-dir))
               (attributes (and (not (file-symlink-p binary))
                                (file-attributes binary 'integer))))
          (when (and attributes
                     (file-regular-p binary)
                     (= (file-attribute-link-number attributes) 1))
            (set-file-modes binary #o755)
            binary))
      nil)))

(defun tramp-rpc-deploy--download-binary (arch)
  "Download a checksum-verified pre-compiled binary for ARCH.
Returns the cached binary path on success, and never promotes an unverified
release artifact into the cache."
  (let* ((cache-path (tramp-rpc-deploy--local-cache-path arch))
         (asset (tramp-rpc-deploy--release-asset-name arch))
         (tarball-url (tramp-rpc-deploy--download-url arch))
         (checksum-url (tramp-rpc-deploy--checksum-url arch))
         (temp-dir (make-temp-file "tramp-rpc-" t))
         (tarball-path (expand-file-name "server.tar.gz" temp-dir))
         (checksum-path (expand-file-name "server.tar.gz.sha256" temp-dir))
         (extract-dir (expand-file-name "extract" temp-dir)))
    (unwind-protect
        (progn
          (message "Fetching checksum for %s..." arch)
          (unless (tramp-rpc-deploy--download-file checksum-url checksum-path)
            (signal 'remote-file-error
                    (list "Checksum metadata unavailable; refusing unverified release binary"
                          checksum-url)))
          (let ((expected (tramp-rpc-deploy--release-checksum
                           (with-temp-buffer
                             (insert-file-contents-literally checksum-path)
                             (buffer-string))
                           asset)))
            (message "Downloading tramp-rpc-server for %s..." arch)
            (unless (tramp-rpc-deploy--download-file tarball-url tarball-path)
              (signal 'remote-file-error
                      (list "Download failed from" tarball-url "(release may not exist)")))
            (unless (tramp-rpc-deploy--verify-checksum tarball-path expected)
              (signal 'remote-file-error
                      (list "Checksum verification failed for" asset)))
            ;; Extract outside the cache: only a verified release can be promoted.
            (message "Extracting binary...")
            (let ((binary (tramp-rpc-deploy--extract-tarball tarball-path extract-dir)))
              (unless binary
                (signal 'remote-file-error (list "Failed to extract tarball")))
              (tramp-rpc-deploy--promote-cached-binary
               binary cache-path "release"))
            (message "Downloaded and verified tramp-rpc-server for %s" arch)
            cache-path))
      ;; Never leave downloaded archives or unpromoted extraction behind.
      (condition-case nil
          (delete-directory temp-dir t)
        (file-missing nil)))))

(defun tramp-rpc-deploy--safe-source-archive-member-p (member)
  "Return non-nil when MEMBER cannot escape an extraction directory."
  (and (not (file-name-absolute-p member))
       (not (string-match-p
             "\\(?:\\`\\|/\\)\\.\\.\\(?:/\\|\\'\\)" member))))

(defun tramp-rpc-deploy--extract-source-tarball (tarball dest-dir)
  "Extract a tagged source TARBALL below DEST-DIR and return its root.
The source archive is expected to contain one top-level directory with the
workspace Cargo.toml and server directory.  Reject archive members with
paths that could escape DEST-DIR."
  (make-directory dest-dir t)
  (with-temp-buffer
    (unless (and (zerop (call-process "tar" nil t nil "-tzf" tarball))
                 (cl-every #'tramp-rpc-deploy--safe-source-archive-member-p
                           (split-string (buffer-string) "[\r\n]+" t)))
      (signal 'remote-file-error
              (list "Release source archive has invalid archive entries"))))
  (unless (zerop (call-process "tar" nil nil nil "-xzf" tarball
                               "-C" dest-dir))
    (signal 'remote-file-error
            (list "Failed to extract release source archive")))
  (let ((roots (cl-remove-if-not #'file-directory-p
                                 (directory-files dest-dir t "\\`[^.]"))))
    (when (and (= (length roots) 1)
               (tramp-rpc-deploy--source-has-server-p (car roots)))
      (file-name-as-directory (car roots)))))

(defun tramp-rpc-deploy--with-release-source-directory (function)
  "Call FUNCTION with the matching release source directory configured.
The downloaded archive and extracted source are removed after FUNCTION
returns or signals."
  (unless tramp-rpc-deploy-source-directory
    (signal 'remote-file-error
            (list "Source builds are disabled by `tramp-rpc-deploy-source-directory'")))
  (let* ((temp-dir (make-temp-file "tramp-rpc-source-" t))
         (tarball (expand-file-name "source.tar.gz" temp-dir))
         (extract-dir (expand-file-name "extract" temp-dir)))
    (unwind-protect
        (progn
          (message "Downloading tramp-rpc source archive for v%s..."
                   tramp-rpc-deploy-version)
          (unless (tramp-rpc-deploy--download-file
                   (tramp-rpc-deploy--source-download-url) tarball)
            (signal 'remote-file-error
                    (list "Release source archive download failed"
                          (tramp-rpc-deploy--source-download-url))))
          (let ((source-root
                 (tramp-rpc-deploy--extract-source-tarball
                  tarball extract-dir)))
            (unless source-root
              (signal 'remote-file-error
                      (list "Release source archive has no usable Rust workspace")))
            (let ((tramp-rpc-deploy-source-directory source-root))
              (funcall function))))
      (ignore-errors (delete-directory temp-dir t)))))

;;; ============================================================================
;;; Build from source
;; ============================================================================

(defun tramp-rpc-deploy--cargo-available-p ()
  "Check if cargo (Rust) is available."
  (executable-find "cargo"))

(defun tramp-rpc-deploy--rust-version-from-output (output)
  "Return the Rust version from rustc OUTPUT, or \"unknown\"."
  (if (string-match "\\brustc \\([0-9]+\\(?:\\.[0-9]+\\)*\\)" output)
      (match-string 1 output)
    "unknown"))

(defun tramp-rpc-deploy--local-rust-toolchain-version ()
  "Return the local rustc version when a usable Rust toolchain exists.
Both Cargo and rustc must be executable.  A successful toolchain command with
an unexpected version format is still usable and is reported as \"unknown\"."
  (when (and (tramp-rpc-deploy--cargo-available-p)
             (executable-find "rustc"))
    (with-temp-buffer
      (let ((cargo-status (call-process "cargo" nil t nil "--version"))
            (rustc-status (call-process "rustc" nil t nil "--version")))
        (when (and (integerp cargo-status)
                   (zerop cargo-status)
                   (integerp rustc-status)
                   (zerop rustc-status))
          (tramp-rpc-deploy--rust-version-from-output (buffer-string)))))))

(defun tramp-rpc-deploy--can-build-for-arch-p (arch)
  "Check if we can build for ARCH on this system.
Local builds are deliberately native; a different target is handled by the
remote source-build fallback instead of requiring cross-compilation setup."
  (string= arch (tramp-rpc-deploy--detect-local-arch)))

(defun tramp-rpc-deploy--confirm-source-build (arch toolchain location)
  "Confirm a source build for ARCH using TOOLCHAIN at LOCATION.
Explicit installations ask before building an unknown platform.  Automatic
deployment proceeds and lets Cargo report whether the platform is usable."
  (let ((version (or toolchain "unknown")))
    (if (tramp-rpc-deploy--platform-supported-p arch)
        (progn
          (message "Building tramp-rpc-server for %s on %s using Rust toolchain %s"
                   arch location version)
          t)
      (if tramp-rpc-deploy--allow-prompt
          (if (y-or-n-p
               (format
                "Platform %s might not be supported.  Build tramp-rpc-server on %s using Rust toolchain %s? "
                arch location version))
              t
            (signal
             'remote-file-error
             (list (format "Source build for platform %s was not confirmed"
                           arch))))
        (message
         "Building tramp-rpc-server for %s on %s using Rust toolchain %s"
         arch location version)
        t))))

(defun tramp-rpc-deploy--build-binary (arch)
  "Build the binary for ARCH from source.
Returns the path to the binary on success, nil on failure."
  (unless (tramp-rpc-deploy--source-has-server-p)
    (signal 'remote-file-error
            (list "Source directory does not contain the Rust server sources")))
  (unless (tramp-rpc-deploy--cargo-available-p)
    (signal 'remote-file-error (list "Rust toolchain (cargo) not found")))
  (unless (tramp-rpc-deploy--can-build-for-arch-p arch)
    (signal
     'remote-file-error
     (list "Cannot cross-compile for" arch "on"
           (tramp-rpc-deploy--detect-local-arch))))

  (let* ((toolchain (tramp-rpc-deploy--local-rust-toolchain-version))
         (default-directory tramp-rpc-deploy-source-directory)
         (target (tramp-rpc-deploy--rust-target-for-arch arch))
         (cache-path (tramp-rpc-deploy--local-cache-path arch))
         (output-directory
          (if target
              (format "target/%s/release" target)
            "target/release"))
         (build-output (expand-file-name
                        (expand-file-name tramp-rpc-deploy-binary-name
                                           output-directory)
                        tramp-rpc-deploy-source-directory))
         (build-buffer (get-buffer-create "*tramp-rpc-build*")))

    (tramp-rpc-deploy--confirm-source-build arch toolchain "the local host")
    (message "Building tramp-rpc-server for %s (this may take a minute)..." arch)

    (with-current-buffer build-buffer
      (erase-buffer))

    (let* ((args (append '("build" "--release")
                         (when target (list "--target" target))
                         (list "--manifest-path"
                               (expand-file-name
                                "Cargo.toml" tramp-rpc-deploy-source-directory))))
           (exit-code (apply #'call-process "cargo" nil build-buffer nil args)))
      (if (zerop exit-code)
          (progn
            ;; Promote only a complete binary with its recorded digest.
            (tramp-rpc-deploy--promote-cached-binary
             build-output cache-path "source-build")
            (message "Built tramp-rpc-server for %s" arch)
            cache-path)
        (with-current-buffer build-buffer
          (signal
	   'remote-file-error
	   (list (format "Build failed (exit %d):\n%s" exit-code (buffer-string)))))))))

(defun tramp-rpc-deploy--remote-path-prefix (vec)
  "Return a shell prefix exporting TRAMP's resolved remote PATH for VEC.
`tramp-get-remote-path' resolves `tramp-remote-path', including the login
shell environment represented by `tramp-own-remote-path'.  Commands sent
through the existing bootstrap shell do not automatically inherit that
resolved value, so export it explicitly for the source build."
  (when-let* ((remote-path
              (condition-case nil
                  (tramp-get-remote-path vec)
                (error nil)))
              (path (mapconcat #'identity remote-path ":")))
    (format "PATH=%s:$PATH; export PATH; "
            (tramp-shell-quote-argument path))))

(defun tramp-rpc-deploy--remote-rust-toolchain-version (vec)
  "Return the remote rustc version for VEC, or nil if Cargo is unusable."
  (setq tramp-rpc-deploy--remote-rust-toolchain-diagnostic nil)
  (let ((inhibit-message nil))
    (message "Checking for Cargo and rustc on %s..."
             (tramp-file-name-host vec)))
  (condition-case err
      (let ((command
             (concat (or (tramp-rpc-deploy--remote-path-prefix vec) "")
                     "if command -v cargo >/dev/null 2>&1 && "
                     "command -v rustc >/dev/null 2>&1; then "
                     "cargo --version && rustc --version; fi")))
        ;; `tramp-send-command-and-read' is only for commands which print a
        ;; Lisp expression.  Version commands print ordinary shell text, so
        ;; use the status-checking API and read its raw connection buffer.
        (if (tramp-send-command-and-check vec command)
            (with-current-buffer (tramp-get-connection-buffer vec)
              (let ((output (buffer-string)))
                (if (string-match
                     "\\brustc \\([0-9]+\\(?:\\.[0-9]+\\)*\\)"
                     output)
                    (tramp-rpc-deploy--rust-version-from-output output)
                  (setq tramp-rpc-deploy--remote-rust-toolchain-diagnostic
                        (if (string-empty-p (string-trim output))
                            "cargo and rustc were not both found on the remote PATH"
                          "cargo and rustc did not return a usable rustc version"))
                  nil)))
          (setq tramp-rpc-deploy--remote-rust-toolchain-diagnostic
                "the remote Cargo/rustc probe command failed")
          nil))
    (error
     (setq tramp-rpc-deploy--remote-rust-toolchain-diagnostic
           (format "the remote Cargo/rustc probe failed: %s"
                   (error-message-string err)))
     (tramp-rpc-deploy--log "Rust toolchain probe failed on %s: %s"
                            (tramp-file-name-host vec)
                            tramp-rpc-deploy--remote-rust-toolchain-diagnostic)
     nil)))

(defun tramp-rpc-deploy--remote-source-directories (files root)
  "Return remote source directories needed for FILES below ROOT."
  (delete-dups
   (delq nil
         (mapcar
          (lambda (file)
            (let ((directory
                   (file-name-directory (file-relative-name file root))))
              (when directory
                (directory-file-name directory))))
          files))))

(defun tramp-rpc-deploy--copy-source-files-to-remote (vec remote-root root files)
  "Copy FILES from ROOT below REMOTE-ROOT on VEC, one file at a time.
This is the compatibility path for hosts without a usable local or remote
tar command."
  (let ((directories (tramp-rpc-deploy--remote-source-directories files root)))
    (let ((directory-arguments
           (mapconcat
            (lambda (directory)
              (tramp-shell-quote-argument
               (expand-file-name directory remote-root)))
            directories " ")))
      (unless (tramp-send-command-and-check
               vec
               (format "mkdir -p %s"
                       (tramp-shell-quote-argument remote-root)))
        (signal 'remote-file-error (list "Could not create remote source directory")))
      (when (and directories
                 (not (tramp-send-command-and-check
                       vec (format "mkdir -p %s" directory-arguments))))
        (signal 'remote-file-error
                (list "Could not create remote source subdirectories"))))
    (dolist (file files)
      (let* ((relative (file-relative-name file root))
             (remote-file (expand-file-name relative remote-root))
             (remote-path (tramp-make-tramp-file-name vec remote-file)))
        ;; Tramp normally suspends timers while it owns the connection.  The
        ;; source-copy reporter only updates the minibuffer, so keep its timer
        ;; live without allowing another Tramp progress reporter to re-enter.
        (let ((tramp-dont-suspend-timers t)
              (tramp-inhibit-progress-reporter t))
          (copy-file file remote-path t))))))

(defun tramp-rpc-deploy--make-source-archive (root files)
  "Create a tar archive containing FILES below ROOT.
Return the temporary archive path, or signal when the archive cannot be
created.  The archive is deliberately uncompressed: eliminating the
per-file SSH handshakes dominates this transfer, and remote source-build
hosts are only assumed to provide the basic tar extractor."
  (unless (executable-find "tar")
    (signal 'remote-file-error
            (list "Local tar command is unavailable for source transfer")))
  (let* ((archive (make-temp-file "tramp-rpc-source-" nil ".tar"))
         (output (generate-new-buffer " *tramp-rpc-source-archive*"))
         (relative-files (mapcar (lambda (file)
                                   (file-relative-name file root))
                                 files))
         (success nil))
    (unwind-protect
        (let ((exit-code
               (apply #'call-process
                      "tar" nil output nil
                      (append (list "-cf" archive "-C" root "--")
                              relative-files))))
          (if (and (integerp exit-code) (zerop exit-code))
              (progn
                (setq success t)
                archive)
            (let ((details (with-current-buffer output
                             (string-trim (buffer-string)))))
              (signal
               'remote-file-error
               (list (format "Could not create source archive%s"
                             (if (string-empty-p details)
                                 ""
                               (concat ": " details))))))))
      (unless success
        (ignore-errors (delete-file archive)))
      (when (buffer-live-p output)
        (kill-buffer output)))))

(defun tramp-rpc-deploy--copy-source-to-remote (vec remote-root)
  "Copy the build-relevant source tree below REMOTE-ROOT on VEC.
Use one tar transfer followed by one remote extraction so high-latency SSH
connections do not pay a connection setup for every source file.  Fall back
to the original per-file transfer when tar is unavailable on either side."
  (let* ((root (tramp-rpc-deploy--source-root))
         (files (tramp-rpc-deploy--source-file-list))
         (archive nil)
         (remote-archive (expand-file-name ".tramp-rpc-source.tar" remote-root)))
    (unless (and root files)
      (signal 'remote-file-error
              (list "Source directory is unavailable or contains no server sources")))
    (condition-case archive-error
        (setq archive (tramp-rpc-deploy--make-source-archive root files))
      (remote-file-error
       (tramp-rpc-deploy--log "Source archive unavailable; using serial copy: %s"
                              (error-message-string archive-error))))
    (if (not archive)
        (tramp-rpc-deploy--copy-source-files-to-remote vec remote-root root files)
      (unwind-protect
          (progn
            (let ((tramp-dont-suspend-timers t)
                  (tramp-inhibit-progress-reporter t))
              (copy-file archive
                         (tramp-make-tramp-file-name vec remote-archive)
                         t))
            (if (tramp-send-command-and-check
                 vec
                 (format "tar -xf %s -C %s"
                         (tramp-shell-quote-argument remote-archive)
                         (tramp-shell-quote-argument remote-root)))
                t
              (tramp-rpc-deploy--log
               "Remote tar extraction unavailable; using serial source copy")
              (tramp-rpc-deploy--copy-source-files-to-remote
               vec remote-root root files)))
        (delete-file archive)))))

(defun tramp-rpc-deploy--run-with-progress-reporter
    (vec arch function &optional progress-message)
  "Call FUNCTION while reporting a remote build for ARCH on VEC.
This uses a local reporter instead of `with-tramp-progress-reporter'.
Deployment is invoked from inside a TRAMP file operation, where TRAMP can
intentionally suppress its nested progress reporters and messages.  The
operation is synchronous, so FUNCTION receives a tick function which it should
call while waiting for the remote command.  PROGRESS-MESSAGE overrides the
default build message and is used for source transfer progress."
  (if noninteractive
      (funcall function nil)
    (let* ((text (or progress-message
                     (format "Building tramp-rpc-server remotely for %s on %s"
                             arch (tramp-file-name-host vec))))
           (reporter
            ;; This status is specifically the long-running operation the
            ;; user initiated.  Do not inherit TRAMP's message suppression.
            (let ((inhibit-message nil))
              (make-progress-reporter text nil nil nil nil 0.2)))
           (last-update nil))
      (cl-labels
          ((tick ()
             (let ((now (float-time)))
               (when (or (null last-update)
                         (>= (- now last-update) 0.5))
                 (setq last-update now)
                 (let ((inhibit-message nil))
                   (progress-reporter-force-update reporter))
                 ;; The caller may currently be in a minibuffer.  TRAMP's
                 ;; normal wait loop uses `nodisp', so redisplay explicitly.
                 (redisplay t)))))
        (let ((timer (run-at-time 0.2 0.2 #'tick)))
          (unwind-protect
              (progn
                (tick)
                (funcall function #'tick))
            (cancel-timer timer)
            (let ((inhibit-message nil)
                  (message-log-max nil))
              (progress-reporter-done reporter))))))))

(defun tramp-rpc-deploy--send-command-and-check-with-progress
    (vec command tick)
  "Send COMMAND on VEC and call TICK while waiting for its exit status.
Unlike `tramp-send-command-and-check', this wait does not use
`tramp-accept-process-output', whose timer suspension prevents a progress
reporter from moving during a long remote build.  It is intentionally limited
to the Cargo build command; the regular TRAMP command path remains unchanged."
  (let* ((process (tramp-get-connection-process vec))
         (buffer (tramp-get-connection-buffer vec))
         (status-regexp "tramp_rpc_build_exit_status \\([0-9]+\\)")
         status)
    ;; Start the command without waiting for the shell prompt.  Preserve
    ;; Cargo's stderr in the connection buffer so a failed build can report
    ;; the compiler diagnostics to the user.
    (tramp-send-command
     vec
     (concat command
             "; printf '\\ntramp_rpc_build_exit_status %s\\n' $?")
     nil t)
    (with-current-buffer buffer
      (while (and (null status) (process-live-p process))
        (when (tramp-check-for-regexp process status-regexp)
          (let ((status-start (match-beginning 0)))
            (setq status (string-to-number (match-string-no-properties 1)))
            ;; Keep build diagnostics in the connection buffer, but remove
            ;; the private status marker and the shell prompt that follows it.
            (goto-char status-start)
            (delete-region status-start (point-max))))
        (unless status
          (funcall tick)
          (with-local-quit
            ;; A short timeout gives the reporter a chance to move even when
            ;; Cargo produces no output for a while.  `with-local-quit' keeps
            ;; C-g responsive and propagates the quit to the caller.
            (accept-process-output process 0.1 nil t))))
      (unless status
        (signal 'remote-file-error
                (list "Remote Cargo build process exited before reporting status")))
      (zerop status))))

(defun tramp-rpc-deploy--build-binary-on-remote (vec arch)
  "Build and install the server for ARCH using VEC's native Rust toolchain.
The source is copied to a private temporary directory on the remote host and
removed after installation, leaving only the versioned deployed binary."
  (unless (tramp-rpc-deploy--source-has-server-p)
    (signal 'remote-file-error (list "Source directory not configured")))
  (let* ((toolchain (tramp-rpc-deploy--remote-rust-toolchain-version vec))
         (diagnostic
          (or tramp-rpc-deploy--remote-rust-toolchain-diagnostic
              "cargo and rustc were not both found on the remote PATH"))
         (host (tramp-file-name-host vec)))
    (unless toolchain
      (let ((inhibit-message nil))
        (message "Remote source build unavailable on %s: %s" host diagnostic))
      (signal 'remote-file-error
              (list (format "No usable remote Rust toolchain on %s: %s"
                            host diagnostic))))
    (tramp-rpc-deploy--confirm-source-build arch toolchain "the remote host")
    (let* ((remote-path (tramp-rpc-deploy--remote-binary-path vec))
           (remote-local (tramp-file-local-name remote-path))
           (directory nil)
           (build-output nil))
      (unwind-protect
          (progn
            (setq directory
                  (progn
                    (unless
                        (tramp-send-command-and-check
                         vec "umask 077 && mktemp -d /tmp/tramp-rpc-build.XXXXXXXXXX")
                      (signal 'remote-file-error
                              (list "Could not create remote source-build directory")))
                    (with-current-buffer (tramp-get-connection-buffer vec)
                      (string-trim (buffer-string)))))
            (unless (and (file-name-absolute-p directory)
                         (string-match-p
                          "\\`/tmp/tramp-rpc-build\\.[^/[:space:]]+\\'"
                          directory))
              (signal 'remote-file-error
                      (list "Could not create remote source-build directory")))
            (tramp-rpc-deploy--run-with-progress-reporter
             vec arch
             (lambda (_tick)
               (tramp-rpc-deploy--copy-source-to-remote vec directory))
             (format "Copying tramp-rpc-server sources to %s"
                     (tramp-file-name-host vec)))
            (setq build-output
                  (expand-file-name
                   (format "target/release/%s" tramp-rpc-deploy-binary-name)
                   directory))
            (let* ((manifest (tramp-shell-quote-argument
                              (expand-file-name "Cargo.toml" directory)))
                   (root (tramp-shell-quote-argument directory))
                   (output (tramp-shell-quote-argument build-output))
                   (destination (tramp-shell-quote-argument remote-local))
                   (parent (tramp-shell-quote-argument
                            (file-name-directory remote-local)))
                   (path-prefix (or (tramp-rpc-deploy--remote-path-prefix vec)
                                    ""))
                   (command
                    (format
                     (concat "%s"
                             "cd %s && cargo build --release --manifest-path %s && "
                             "test -f %s && ! test -L %s && mkdir -p %s && "
                             "chmod +x %s && "
                             "if test ! -e %s && ! test -L %s; then "
                             "mv -f %s %s; fi && "
                             "test -f %s && ! test -L %s && test -x %s")
                     path-prefix root manifest output output parent output
                     destination destination output destination
                     destination destination destination)))
              (unless
                  (tramp-rpc-deploy--run-with-progress-reporter
                   vec arch
                   (lambda (tick)
                     (tramp-rpc-deploy--send-command-and-check-with-progress
                      vec command tick)))
                (let ((details
                       (with-current-buffer (tramp-get-connection-buffer vec)
                         (string-trim (buffer-string)))))
                  (signal
                   'remote-file-error
                   (list (format "Remote source build failed%s"
                                 (if (string-empty-p details)
                                     ""
                                   (concat ":\n" details))))))))
            (message "Built and installed tramp-rpc-server for %s on %s"
                     arch (tramp-file-name-host vec))
            remote-path)
        (when directory
          (ignore-errors
            (tramp-rpc-deploy--remove-remote-staging-directory vec directory)))))))

(defun tramp-rpc-deploy--remote-source-build-available-p ()
  "Return non-nil when a remote source build may be attempted.
A configured source checkout is used directly.  For an archive/package
installation, the configured default directory acts as the opt-in to obtain
the matching release source archive.  Setting it to nil disables both forms
of source build."
  (and tramp-rpc-deploy-source-directory t))

(defun tramp-rpc-deploy--ask-remote-source-build-action (vec arch)
  "Ask whether to download or build remotely for supported ARCH on VEC.
Return `download', `remote-build', or `skip'.  Return nil when the remote
platform is unsupported, the source is unavailable, the remote toolchain is
missing, or this is a noninteractive/reentrant deployment."
  (when (and (not noninteractive)
             (or tramp-rpc-deploy--allow-prompt
                 (not tramp-rpc-deploy--explicit-target))
             (tramp-rpc-deploy--platform-supported-p arch)
             (tramp-rpc-deploy--remote-source-build-available-p))
    (let ((toolchain (tramp-rpc-deploy--remote-rust-toolchain-version vec)))
      (when toolchain
        (pcase
            (read-char-choice
             (concat
              (format
               "TRAMP-RPC could not obtain a local server binary for %s.\n\n"
               arch)
              "  [d] Download the release binary\n"
              (when (tramp-rpc-deploy--use-source-binary-id-p)
                "      Warning: it may not exactly match the checked-out sources\n")
              (format
               "  [r] Build the sources remotely with Cargo (Rust toolchain %s)\n"
               toolchain)
              "  [s] Skip installation\n\n"
              "Choice: ")
             '(?d ?r ?s))
          (?d 'download)
          (?r 'remote-build)
          (?s 'skip))))))

(defun tramp-rpc-deploy--build-binary-on-remote-with-source-fallback
    (vec arch)
  "Build ARCH on VEC from a checkout or the matching release source archive."
  (if (tramp-rpc-deploy--source-has-server-p)
      (tramp-rpc-deploy--build-binary-on-remote vec arch)
    (tramp-rpc-deploy--with-release-source-directory
     (lambda ()
       (tramp-rpc-deploy--build-binary-on-remote vec arch)))))

;;; ============================================================================
;;; Main logic: ensure local binary exists
;; ============================================================================

(defun tramp-rpc-deploy--ask-git-install-action (arch &optional vec)
  "Ask how to obtain a git-checkout server binary for ARCH.
Return `download', `build', `remote-build', or nil.  A local build is offered
when Cargo is available and ARCH can be built natively.  When VEC is supplied,
a remote build is offered if the remote has a usable Rust toolchain."
  (unless tramp-rpc-deploy--allow-prompt
    (signal
     'remote-file-error
     (list
      "TRAMP-RPC needs a server binary for this git checkout.  Run M-x tramp-rpc-deploy-install-binary to choose whether to download or build it, or customize `tramp-rpc-deploy-git-build-policy' to `release' or `build'")))
  (let* ((cargo-available (tramp-rpc-deploy--cargo-available-p))
         (can-build (and cargo-available
                         (tramp-rpc-deploy--can-build-for-arch-p arch)))
         (remote-toolchain
          (when (and vec
                     (tramp-rpc-deploy--remote-source-build-available-p))
            (tramp-rpc-deploy--remote-rust-toolchain-version vec)))
         (can-build-remotely (and remote-toolchain t))
         (choices (append '(?d)
                          (when can-build '(?b))
                          (when can-build-remotely '(?r))
                          '(?s)))
         (build-line
          (cond
           (can-build "  [b] Build the checked-out sources with Cargo\n")
           ((not cargo-available)
            "      Build unavailable: Cargo was not found\n")
           (t
            (format "      Build unavailable: cannot build %s natively\n" arch))))
         (remote-build-line
          (cond
           (can-build-remotely
            (format "  [r] Build the checked-out sources remotely with Cargo for %s (Rust toolchain %s)\n"
                    arch remote-toolchain))
           ((and vec (tramp-rpc-deploy--remote-source-build-available-p))
            (format "      Remote build unavailable on %s: %s\n"
                    (tramp-file-name-host vec)
                    (or tramp-rpc-deploy--remote-rust-toolchain-diagnostic
                        "cargo and rustc were not both found on the remote PATH")))))
         (choice
          (read-char-choice
           (concat
            "TRAMP-RPC needs a server binary for this git checkout.\n\n"
            "  [d] Download the checksum-verified release binary\n"
            "      Warning: it may not exactly match the checked-out sources\n"
            build-line
            (or remote-build-line "")
            "  [s] Skip and install the server manually\n\n"
            "Choice: ")
           choices)))
    (pcase choice
      (?d 'download)
      (?b 'build)
      (?r 'remote-build)
      (?s nil))))

(defun tramp-rpc-deploy--git-install-action (arch &optional vec)
  "Ask for the action used to obtain a git binary for ARCH and optionally VEC."
  (or (tramp-rpc-deploy--ask-git-install-action arch vec)
      (signal
       'remote-file-error
       (list "TRAMP-RPC server installation skipped; install it manually or run M-x tramp-rpc-deploy-install-binary again"))))

(defun tramp-rpc-deploy--obtain-methods (arch)
  "Return the methods to use for obtaining a missing binary for ARCH."
  (cond
   ((tramp-rpc-deploy--use-source-binary-id-p)
    (list (if (eq tramp-rpc-deploy-git-build-policy 'build)
              'build
            (tramp-rpc-deploy--git-install-action arch))))
   (tramp-rpc-deploy-prefer-build
    '(build download))
   (t
    '(download build))))

(defun tramp-rpc-deploy--ensure-local-binary (arch &optional install-action)
  "Ensure a local binary exists for ARCH with optional INSTALL-ACTION.
Tries in order:
1. Check bundled binaries (useful for development)
2. Check source-tree build output for source-build policies
3. Check local cache
4. Download from GitHub releases or build from source according to policy.

When INSTALL-ACTION is non-nil, use it as the acquisition method.

Returns the path to the local binary."
  (let ((bundled-path (tramp-rpc-deploy--bundled-binary-path arch))
        (source-build-path
         (when (or (tramp-rpc-deploy--use-source-binary-id-p)
                   tramp-rpc-deploy-prefer-build)
           (tramp-rpc-deploy--source-build-output-path arch)))
        (cache-path (tramp-rpc-deploy--local-cache-path arch)))
    (cond
     ;; Check bundled binaries first (useful for development - run
     ;; scripts/build-all.sh to populate lisp/binaries/).  In git-checkout
     ;; source-id mode, only trust a bundled binary when it is newer than the
     ;; source files; otherwise a stale bundled artifact can be deployed under
     ;; the fresh git hash and recreate the exact mismatch source-id mode avoids.
     ((and (not tramp-rpc-deploy--force-obtain)
           bundled-path
           (or (not (tramp-rpc-deploy--use-source-binary-id-p))
               (tramp-rpc-deploy--newer-than-source-p bundled-path)))
      (message "Using bundled binary for %s" arch)
      bundled-path)

     ;; Check source-tree build output.  This supports CI jobs that download a
     ;; just-built server artifact into target/<triple>/release/.
     ((and (not tramp-rpc-deploy--force-obtain) source-build-path)
      (message "Using source-tree build output for %s" arch)
      source-build-path)

     ;; Check cache
     ((and (not tramp-rpc-deploy--force-obtain)
           (file-exists-p cache-path)
           (file-executable-p cache-path)
           (tramp-rpc-deploy--cached-binary-trusted-p cache-path))
      (message "Using cached binary for %s" arch)
      cache-path)

     ;; Need to obtain binary
     (t
      (let ((methods (or (and install-action (list install-action))
                         (tramp-rpc-deploy--obtain-methods arch)))
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
                         (tramp-rpc-deploy--build-binary arch))))
              (error
               (push (cons method (error-message-string err)) errors)))))

        (or result
            (signal
	     'remote-file-error
	     (list (format
		    "Failed to obtain tramp-rpc-server for %s.\n\nErrors:\n%s\n\n%s"
                    arch
                    (mapconcat (lambda (e)
                                 (format "  %s: %s" (car e) (cdr e)))
                               (reverse errors)
                               "\n")
                    (tramp-rpc-deploy--help-message arch))))))))))

(defun tramp-rpc-deploy--help-message (arch)
  "Return a help message for obtaining binary for ARCH."
  (let ((local-arch (tramp-rpc-deploy--detect-local-arch)))
    (if (tramp-rpc-deploy--use-source-binary-id-p)
        (concat
         "This installation is using a git-checkout binary id.  Automatic\n"
         "release fallback is disabled because the release server may be stale\n"
         "when checkout sources changed without a version bump.\n\n"
         "To resolve this, you can:\n\n"
         (if (string= arch local-arch)
             (concat
              "1. Install Rust and build from source:\n"
              "   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh\n"
              "   Then restart Emacs and try again.\n\n")
           (format
            "1. Build on a %s machine and copy to:\n   %s\n\n"
            arch
            (tramp-rpc-deploy--local-cache-path arch)))
         "2. Run M-x tramp-rpc-deploy-install-binary and choose download.\n"
         "   The release fallback will remain keyed to this source tree.\n\n"
         "3. To always use release-version paths for checkouts, customize:\n"
         "   (setq tramp-rpc-deploy-git-build-policy 'release)\n\n"
         (format "Binary should be placed at:\n   %s"
                 (tramp-rpc-deploy--local-cache-path arch)))
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
               (tramp-rpc-deploy--local-cache-path arch))))))

;; ============================================================================
;;; Remote deployment
;; ============================================================================

(defun tramp-rpc-deploy--regular-nonsymlink-test (path &optional executable)
  "Return a shell test for regular, non-symlink PATH.
When EXECUTABLE is non-nil, require execute permission too."
  (let ((quoted (tramp-shell-quote-argument path)))
    (format "test -f %s && ! test -L %s%s"
            quoted quoted (if executable (format " && test -x %s" quoted) ""))))

(defun tramp-rpc-deploy--checksum-shell-fragment (path)
  "Return a shell fragment that prints PATH's SHA256 digest."
  (let ((quoted (tramp-shell-quote-argument path)))
    (format "{ sha256sum %s 2>/dev/null || shasum -a 256 %s 2>/dev/null; } | cut -d' ' -f1"
            quoted quoted)))

(defun tramp-rpc-deploy--activation-command (temporary destination checksum)
  "Build the atomic remote activation transaction.
TEMPORARY and DESTINATION are remote local names.  CHECKSUM is the expected
SHA256 digest."
  (let ((tmp (tramp-shell-quote-argument temporary))
        (dest (tramp-shell-quote-argument destination))
        (digest (tramp-shell-quote-argument checksum)))
    (format
     "%s && chmod +x %s && %s && (test ! -e %s && ! test -L %s || %s) && actual=$(%s) && test \"$actual\" = %s && mv -f %s %s && %s"
     (tramp-rpc-deploy--regular-nonsymlink-test temporary)
     tmp
     (tramp-rpc-deploy--regular-nonsymlink-test temporary)
     dest dest
     (tramp-rpc-deploy--regular-nonsymlink-test destination)
     (tramp-rpc-deploy--checksum-shell-fragment temporary)
     digest tmp dest
     (tramp-rpc-deploy--regular-nonsymlink-test destination t))))

(defun tramp-rpc-deploy--remote-binary-exists-p (vec)
  "Check if a regular non-symlink executable binary exists on remote VEC."
  (let* ((remote-path (tramp-rpc-deploy--remote-binary-path vec))
         (path (tramp-file-local-name remote-path)))
    ;; Use tramp-sh operations for checking since we're bootstrapping.
    (tramp-send-command-and-check
     vec (tramp-rpc-deploy--regular-nonsymlink-test path t))))

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
  (tramp-send-command vec (tramp-rpc-deploy--checksum-shell-fragment path))
  (with-current-buffer (tramp-get-connection-buffer vec)
    (goto-char (point-min))
    ;; Match exactly 64 hex chars to avoid false positives from error messages
    (when (looking-at "\\([a-f0-9]\\{64\\}\\)")
      (match-string 1))))

(defun tramp-rpc-deploy--make-remote-staging-directory (vec remote-local)
  "Create and return a private staging directory beside REMOTE-LOCAL on VEC."
  (let ((parent (file-name-directory remote-local)))
    ;; Ask the remote shell to resolve `~' (and any symlinked parent) before
    ;; invoking mktemp.  The default deployment directory is home-relative, so
    ;; comparing mktemp's absolute output with the unexpanded `~/' spelling
    ;; would reject every automatic deployment.
    (tramp-send-command
     vec
     (format "umask 077 && parent=$(cd %s && pwd -P) && \
directory=$(mktemp -d \"$parent/.tramp-rpc-transfer.XXXXXX\") && \
printf '%%s\\n%%s\\n' \"$parent\" \"$directory\""
             (tramp-shell-quote-argument parent)))
    (with-current-buffer (tramp-get-connection-buffer vec)
      (goto-char (point-min))
      (let ((resolved-parent
             (string-trim (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
            directory)
        (forward-line 1)
        (setq directory
              (string-trim (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))))
        (unless (and (file-name-absolute-p resolved-parent)
                     (file-name-absolute-p directory)
                     (string= (file-name-as-directory resolved-parent)
                              (file-name-directory directory)))
          (signal 'remote-file-error
                  (list "Could not create private remote staging directory")))
        directory))))

(defun tramp-rpc-deploy--remove-remote-staging-directory (vec directory)
  "Remove remote staging DIRECTORY on VEC."
  (when directory
    (tramp-send-command-and-check
     vec (format "rm -rf %s" (tramp-shell-quote-argument directory)))))

(defun tramp-rpc-deploy--remote-binary-matches-p (vec local-path)
  "Return non-nil when VEC's deployed binary matches LOCAL-PATH."
  (let* ((remote-local
          (tramp-file-local-name (tramp-rpc-deploy--remote-binary-path vec)))
         (local-checksum (tramp-rpc-deploy--compute-checksum local-path))
         (remote-checksum (tramp-rpc-deploy--remote-checksum vec remote-local)))
    (and remote-checksum (string= local-checksum remote-checksum))))


(defun tramp-rpc-deploy--transfer-binary (vec local-path)
  "Transfer the binary at LOCAL-PATH to the remote host VEC.
Uses TRAMP's `copy-file' with the bootstrap method for binary transfer.
When the bootstrap method is \"scp\", \"scpx\", or \"rsync\", the transfer
uses out-of-band protocols (the actual scp/rsync binaries) which is fast
and reliable for large files.  With \"ssh\" or \"sshx\", TRAMP falls back
to inline encoding (base64 through the shell), which can be fragile."
  (let* ((remote-path (tramp-rpc-deploy--remote-binary-path vec))
         (remote-local (tramp-file-local-name remote-path))
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
      (let ((attempt (1+ retries))
            staging-directory)
        (message "Transfer attempt %d/%d..." attempt tramp-rpc-deploy-max-retries)
        (unwind-protect
            (condition-case err
                (let* ((directory
                        (setq staging-directory
                              (tramp-rpc-deploy--make-remote-staging-directory
                               vec remote-local)))
                       (remote-tmp-local
                        (expand-file-name
                         (file-name-nondirectory remote-local) directory))
                       (remote-tmp-path
                        (tramp-make-tramp-file-name vec remote-tmp-local)))
                  ;; The atomically-created private directory guarantees that
                  ;; the destination does not exist.  Do not permit `copy-file'
                  ;; to follow or replace a pre-planted pathname.
                  (copy-file local-path remote-tmp-path nil)

                  (unless (tramp-send-command-and-check
                           vec
                           (format "test -s %s"
                                   (tramp-shell-quote-argument remote-tmp-local)))
                    (signal 'remote-file-error
                            (list "Temp file not created or is empty after copy")))

                  (let ((remote-checksum
                         (tramp-rpc-deploy--remote-checksum vec remote-tmp-local)))
                    (unless remote-checksum
                      (signal
                       'remote-file-error
                       (list "Could not compute remote checksum (sha256sum/shasum not available?)")))
                    (unless (string= local-checksum remote-checksum)
                      (signal
                       'remote-file-error
                       (list (format "Checksum mismatch (local: %s, remote: %s)"
                                     (substring local-checksum 0 12)
                                     (substring remote-checksum 0 12))))))

                  ;; Recheck the type and digest in the same remote shell
                  ;; operation as activation.  This closes the interval between
                  ;; the earlier diagnostic checksum and the atomic rename.
                  ;; The staging directory is a child of the deployment
                  ;; directory, so promotion remains a same-filesystem rename
                  ;; while concurrent deployments stay isolated.
                  (unless
                      (tramp-send-command-and-check
                       vec
                       (tramp-rpc-deploy--activation-command
                        remote-tmp-local remote-local local-checksum))
                    (signal 'remote-file-error
                            (list "Remote activation failed; existing binary was preserved")))
                  (setq success t)
                  (message "Transfer completed successfully"))
              (error
               (let ((err-msg
                      (format "Attempt %d: %s" attempt (error-message-string err))))
                 (push err-msg errors)
                 (message "Transfer error: %s" err-msg))
               (setq retries (1+ retries))))
          (when staging-directory
            (condition-case cleanup-error
                (tramp-rpc-deploy--remove-remote-staging-directory
                 vec staging-directory)
              ((file-error remote-file-error)
               (tramp-rpc-deploy--log
                "Failed to remove remote staging directory %s: %s"
                staging-directory
                (error-message-string cleanup-error))))))))

    (unless success
      (signal
       'remote-file-error
       (list (format
	      "Failed to transfer binary after %d attempts.\n\nErrors:\n%s\n\nTroubleshooting:\n- Verify SSH access: ssh %s@%s echo success\n- Check write permissions to %s on remote host\n- Ensure sha256sum or shasum command is available on remote host"
              tramp-rpc-deploy-max-retries
              (mapconcat #'identity (nreverse errors) "\n")
              (or (tramp-file-name-user vec) "USER")
              (tramp-file-name-host vec)
              tramp-rpc-deploy-remote-directory))))

    remote-path))

;; ============================================================================
;;; Public API
;; ============================================================================

(defun tramp-rpc-deploy-expected-binary-localname ()
  "Return the expected remote binary localname without network access.
This computes the path deterministically from customization variables,
allowing `tramp-rpc--connect' to try connecting directly without
opening a bootstrap (scpx) connection for the deploy check."
  (concat (file-name-as-directory tramp-rpc-deploy-remote-directory)
          (format "%s-%s"
                  tramp-rpc-deploy-binary-name
                  (tramp-rpc-deploy--binary-id))))

(defun tramp-rpc-deploy-ensure-binary (vec)
  "Ensure the tramp-rpc-server binary is available on remote VEC.
Returns the remote path (or bare binary name) to the binary.

When `tramp-rpc-deploy-never-deploy' is non-nil, no deployment is
attempted.  Returns `tramp-rpc-deploy-remote-binary-path' if set,
otherwise the bare binary name \"tramp-rpc-server\".

Otherwise, if `tramp-rpc-deploy-auto-deploy' is nil and the binary
is missing, signals an error.

An existing executable is reused when no trusted local artifact can be
obtained (for example, download, build, or cache access is unavailable).
When a trusted local artifact is available, its checksum is always compared:
a mismatch is replaced only with auto-deploy enabled, and otherwise signals
an explicit error.  A missing remote binary never uses this fallback."
  (if tramp-rpc-deploy-never-deploy
      ;; Never deploy mode: use explicit path or bare binary name
      (let ((path (or tramp-rpc-deploy-remote-binary-path
                      tramp-rpc-deploy-binary-name)))
        (message "tramp-rpc: never-deploy mode, using %s on remote" path)
        path)
    ;; Normal deployment flow.  Prompting and forced replacement apply only to
    ;; the remote an explicit installation was requested for; deploys that run
    ;; reentrantly for other remotes stay fully automatic.
    (let* ((explicit (tramp-rpc-deploy--explicit-target-p vec))
           (tramp-rpc-deploy--allow-prompt explicit)
           (tramp-rpc-deploy--force-obtain
            (and explicit tramp-rpc-deploy--explicit-force))
           (tramp-rpc-deploy-auto-deploy
            (cond (explicit t)
                  ;; Inside another remote's explicit installation window:
                  ;; use the pre-override value so the explicit auto-deploy
                  ;; override does not leak to unrelated remotes.
                  (tramp-rpc-deploy--explicit-target
                   tramp-rpc-deploy--pre-explicit-auto-deploy)
                  (t tramp-rpc-deploy-auto-deploy)))
           (bootstrap-vec (tramp-rpc-deploy--bootstrap-vec vec))
	   ;; For simplified Tramp syntax.
	   (tramp-default-method (tramp-file-name-method bootstrap-vec))
	   tramp-default-method-alist)
      (let* ((remote-present
              (and (not tramp-rpc-deploy--force-obtain)
                   (tramp-rpc-deploy--remote-binary-exists-p bootstrap-vec)))
             (remote-local
              (tramp-file-local-name
               (tramp-rpc-deploy--remote-binary-path bootstrap-vec))))
        (cond
         ((and (not remote-present) (not tramp-rpc-deploy-auto-deploy))
          (signal
           'remote-file-error
           (list "tramp-rpc-server not found on"
                 (tramp-file-name-host vec)
                 "and auto-deploy is disabled")))
         (t
          ;; The pre-existing remote executable is a usable fallback only when
          ;; local artifact acquisition itself failed.  Once we have a trusted
          ;; artifact, retain strict checksum comparison and replacement.
          (let* ((arch (tramp-rpc-deploy--detect-remote-arch bootstrap-vec))
                 ;; An explicit install is the one place where the user can
                 ;; choose a remote source build before local artifact lookup.
                 ;; Automatic deployment retains the existing local-first,
                 ;; remote-fallback behavior below.
                 (install-action
                  (when (and explicit
                             (tramp-rpc-deploy--use-source-binary-id-p)
                             (eq tramp-rpc-deploy-git-build-policy 'auto))
                    (tramp-rpc-deploy--git-install-action
                     arch bootstrap-vec)))
                 (artifact
                  (if (eq install-action 'remote-build)
                      (list :remote-binary
                            (tramp-rpc-deploy--build-binary-on-remote-with-source-fallback
                             bootstrap-vec arch))
                    (condition-case local-error
                        (list :local-binary
                              (if install-action
                                  (tramp-rpc-deploy--ensure-local-binary
                                   arch install-action)
                                (tramp-rpc-deploy--ensure-local-binary arch)))
                      (remote-file-error
                       ;; A remote native build is the fallback for unsupported
                       ;; platforms and for hosts that cannot be cross-compiled
                       ;; by the local toolchain.  An existing executable is
                       ;; already a usable fallback, and remote building is
                       ;; itself a deployment, so only try it for a missing
                       ;; binary when auto-deploy is enabled.
                       (if (and (not remote-present)
                                tramp-rpc-deploy-auto-deploy
                                (tramp-rpc-deploy--remote-source-build-available-p))
                           (pcase (tramp-rpc-deploy--ask-remote-source-build-action
                                   bootstrap-vec arch)
                             ('download
                              (condition-case download-error
                                  (list :local-binary
                                        (tramp-rpc-deploy--ensure-local-binary
                                         arch 'download))
                                (remote-file-error
                                 (signal
                                  'remote-file-error
                                  (list
                                   (format
                                    "Local artifact and release download failed for %s.\n\nLocal error: %s\n\nDownload error: %s"
                                    arch
                                    (error-message-string local-error)
                                    (error-message-string download-error)))))))
                             ('skip
                              (signal
                               'remote-file-error
                               (list
                                (format
                                 "Deployment skipped for %s.\n\nLocal artifact error: %s"
                                 arch
                                 (error-message-string local-error)))))
                             (_
                              (message
                               "No usable local artifact for %s; trying a native source build on %s"
                               arch (tramp-file-name-host bootstrap-vec))
                              (condition-case remote-error
                                  (list :remote-binary
                                        (tramp-rpc-deploy--build-binary-on-remote-with-source-fallback
                                         bootstrap-vec arch))
                                (remote-file-error
                                 (if remote-present
                                     (list :unavailable local-error)
                                   (signal
                                    'remote-file-error
                                    (list
                                     (format
                                      "Local artifact and remote source build failed for %s.\n\nLocal error: %s\n\nRemote error: %s"
                                      arch
                                      (error-message-string local-error)
                                      (error-message-string remote-error)))))))))
                         (if remote-present
                             (list :unavailable local-error)
                           (signal (car local-error) (cdr local-error))))))))
                 (local-binary (plist-get artifact :local-binary))
                 (remote-binary (plist-get artifact :remote-binary)))
            (cond
             ((eq (car artifact) :unavailable)
              (message
               "Using existing remote tramp-rpc-server; no trusted local artifact is available: %s"
               (error-message-string (cadr artifact)))
              remote-local)
             (remote-binary
              (message "Using remotely built tramp-rpc-server for %s" arch)
              remote-local)
             ((and remote-present
                   (tramp-rpc-deploy--remote-binary-matches-p
                    bootstrap-vec local-binary))
              remote-local)
             (tramp-rpc-deploy-auto-deploy
              (when remote-present
                (message "Existing remote tramp-rpc-server failed checksum verification; replacing it"))
              (message "Deploying tramp-rpc-server (%s) to %s..."
                       arch (tramp-file-name-host vec))
              (tramp-file-local-name
               (tramp-rpc-deploy--transfer-binary bootstrap-vec local-binary)))
             (remote-present
              (signal
               'remote-file-error
               (list "Existing tramp-rpc-server on"
                     (tramp-file-name-host vec)
                     "failed checksum verification and auto-deploy is disabled")))))))))))

;;;###autoload
(defun tramp-rpc-deploy-install-binary (vec &optional force)
  "Interactively obtain and deploy tramp-rpc-server for remote VEC.
For git checkouts using the `auto' policy, this asks whether to download a
release binary, build the checked-out sources locally or remotely when
available, or skip installation.

With prefix argument FORCE, replace an existing remote or cached artifact and
ask again in `auto' mode.  Explicit installation overrides
`tramp-rpc-deploy-auto-deploy', but refuses to run when
`tramp-rpc-deploy-never-deploy' is non-nil."
  (interactive
   (list (tramp-dissect-file-name
          (read-file-name "Remote TRAMP-RPC host: " "/rpc:"))
         current-prefix-arg))
  (when tramp-rpc-deploy-never-deploy
    (user-error "Deployment is disabled by `tramp-rpc-deploy-never-deploy'"))
  ;; An explicit installation is the user's request to try again now, even if
  ;; a preceding completion probe cached a failed connection attempt.  Keep
  ;; this optional so the autoloaded deployment module remains usable before
  ;; the main tramp-rpc module has been loaded.
  (when (fboundp 'tramp-rpc--clear-connection-failure)
    (tramp-rpc--clear-connection-failure vec))
  (let ((tramp-rpc-deploy--explicit-target (tramp-rpc-deploy--target-key vec))
        (tramp-rpc-deploy--explicit-force force)
        (tramp-rpc-deploy--pre-explicit-auto-deploy tramp-rpc-deploy-auto-deploy))
    (tramp-rpc-deploy-ensure-binary vec)))

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

(defun tramp-rpc-deploy-show-binary-paths (vec)
  "Show resolved deployment paths for remote VEC.
Reports remote architecture and the paths used for local cache, bundled
binary lookup, and remote installation target."
  (interactive
   (list (tramp-dissect-file-name
          (read-file-name "Remote host: " "/rpc:"))))
  (let* ((bootstrap-vec (tramp-rpc-deploy--bootstrap-vec vec))
         (arch (tramp-rpc-deploy--detect-remote-arch bootstrap-vec))
         (cache (tramp-rpc-deploy--local-cache-path arch))
         (bundled (tramp-rpc-deploy--bundled-binary-path arch))
         (remote (tramp-rpc-deploy--remote-binary-path bootstrap-vec))
         (buf (get-buffer-create "*tramp-rpc-deploy-paths*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "TRAMP-RPC Binary Path Resolution\n")
      (insert "===============================\n\n")
      (insert (format "Host:    %s\n" (tramp-file-name-host bootstrap-vec)))
      (insert (format "User:    %s\n" (or (tramp-file-name-user bootstrap-vec) "<default>")))
      (insert (format "Method:  %s (bootstrap)\n" (tramp-file-name-method bootstrap-vec)))
      (insert (format "Arch:    %s\n" arch))
      (insert (format "Binary id: %s\n" (tramp-rpc-deploy--binary-id)))
      (insert (format "Git build policy: %s\n\n" tramp-rpc-deploy-git-build-policy))
      (insert (format "Cache:   %s\n" cache))
      (insert (format "Bundled: %s\n"
                      (or bundled "<none>")))
      (insert (format "Remote:  %s\n" (tramp-file-local-name remote))))
    (display-buffer buf)
    (message "Resolved binary paths for %s (arch=%s)"
             (tramp-file-name-host bootstrap-vec) arch)
    `((arch . ,arch)
      (cache . ,cache)
      (bundled . ,bundled)
      (remote . ,(tramp-file-local-name remote)))))

(defun tramp-rpc-deploy-status ()
  "Show the status of tramp-rpc-server binaries."
  (interactive)
  (let ((buf (get-buffer-create "*tramp-rpc-deploy-status*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "TRAMP-RPC Server Deployment Status\n")
      (insert "===================================\n\n")
      (insert (format "Version: %s\n" tramp-rpc-deploy-version))
      (insert (format "Binary id: %s\n" (tramp-rpc-deploy--binary-id)))
      (insert (format "Git build policy: %s\n" tramp-rpc-deploy-git-build-policy))
      (insert (format "Git checkout with server sources: %s\n"
                      (if (tramp-rpc-deploy--use-source-binary-id-p) "yes" "no")))
      (when-let* ((warning (tramp-rpc-deploy--source-directory-warning)))
        (insert (format "WARNING: %s\n" warning)))
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
      (dolist (arch (tramp-rpc-deploy--supported-architectures))
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
      (dolist (arch (tramp-rpc-deploy--supported-architectures))
        (insert (format "  %s:\n    %s\n" arch (tramp-rpc-deploy--download-url arch)))))
    (display-buffer buf)))

(defun tramp-rpc-deploy--diagnose-ssh (host user command &optional connect-timeout)
  "Run SSH COMMAND on HOST as USER and return (STATUS . OUTPUT).
STATUS is always numeric; signal termination is reported as failure in OUTPUT.
When CONNECT-TIMEOUT is non-nil, use a ten-second connection timeout."
  (let ((args (append
               (list "-o" "BatchMode=yes")
               (when connect-timeout (list "-o" "ConnectTimeout=10"))
               (when user (list "-l" user))
               (list "--" host command))))
    (with-temp-buffer
      (condition-case error-data
          (let ((status (apply #'call-process "ssh" nil t nil args))
                (output (buffer-string)))
            (if (integerp status)
                (cons status output)
              (cons 128
                    (concat output
                            (unless (string-empty-p output) "\n")
                            status))))
        (file-error
         (cons 127 (error-message-string error-data)))))))

(defun tramp-rpc-deploy-diagnose (host &optional user)
  "Run diagnostics for deploying to HOST.
Optional USER specifies the SSH user.
This helps troubleshoot deployment issues."
  (interactive "sHost: \nsUser (leave empty for default): ")
  (when (string-empty-p user)
    (setq user nil))
  (let ((buf (get-buffer-create "*tramp-rpc-diagnose*")))
    (with-current-buffer buf
      (special-mode) ; For "q" and alike.
      (let ((test-num 1)
	    (inhibit-read-only t))
	(erase-buffer)
	(insert (format "TRAMP-RPC Deployment Diagnostics for %s%s\n"
			(if user (concat user "@") "") host))
	(insert "=" (make-string 50 ?=) "\n\n")
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
        (let* ((result (tramp-rpc-deploy--diagnose-ssh
                        host user "echo 'SSH_OK'" t))
               (output (cdr result)))
          (if (and (zerop (car result)) (string-match-p "SSH_OK" output))
              (insert "   [OK] SSH connection successful\n")
            (insert "   [FAIL] SSH connection failed\n")
            (insert (format "   Output: %s\n" (string-trim output)))))

        ;; Remote architecture
        (cl-incf test-num)
        (insert (format "\n%d. Detecting remote architecture...\n" test-num))
        (let* ((result (tramp-rpc-deploy--diagnose-ssh
                        host user "uname -m && uname -s"))
               (output (cdr result)))
          (if (not (zerop (car result)))
              (progn
                (insert "   [FAIL] Could not detect architecture\n")
                (insert (format "   Output: %s\n" (string-trim output))))
            (insert (format "   [OK] Architecture: %s\n" (string-trim output)))))

        ;; Remote directory writable
        (cl-incf test-num)
        (insert (format "\n%d. Testing remote directory access...\n" test-num))
        (let* ((dir tramp-rpc-deploy-remote-directory)
               (result
                (tramp-rpc-deploy--diagnose-ssh
                 host user
                 (format "mkdir -p %s && test -w %s && echo 'WRITABLE'"
                         (tramp-shell-quote-argument dir)
                         (tramp-shell-quote-argument dir))))
               (output (cdr result)))
          (if (and (zerop (car result)) (string-match-p "WRITABLE" output))
              (insert (format "   [OK] Directory %s is writable\n" dir))
            (insert (format "   [FAIL] Directory %s not writable\n" dir))
            (insert (format "   Output: %s\n" (string-trim output)))))

        ;; Checksum command
        (cl-incf test-num)
        (insert (format "\n%d. Testing checksum command availability...\n" test-num))
        (let* ((result (tramp-rpc-deploy--diagnose-ssh
                        host user
                        "which sha256sum || which shasum || echo 'NONE'"))
               (output (string-trim (cdr result))))
          (if (or (not (zerop (car result)))
                  (string-match-p "NONE" output))
              (progn
                (insert "   [FAIL] No checksum command found (need sha256sum or shasum)\n")
                (insert (format "   Output: %s\n" output)))
            (insert (format "   [OK] Found: %s\n" output))))

        ;; Conditional: rsync availability (when using rsync bootstrap method)
        (when (string= tramp-rpc-deploy-bootstrap-method "rsync")
          (cl-incf test-num)
          (insert (format "\n%d. Testing rsync availability on remote...\n" test-num))
          (let* ((result (tramp-rpc-deploy--diagnose-ssh
                          host user "which rsync || echo 'NONE'"))
                 (output (string-trim (cdr result))))
            (if (or (not (zerop (car result)))
                    (string-match-p "NONE" output))
                (progn
                  (insert "   [FAIL] rsync not found on remote (needed for rsync bootstrap method)\n")
                  (insert (format "   Output: %s\n" output)))
              (insert (format "   [OK] Found: %s\n" output)))))

        ;; Local binary availability
        (cl-incf test-num)
        (insert (format "\n%d. Checking local binary cache...\n" test-num))
        (dolist (arch (tramp-rpc-deploy--supported-architectures))
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
