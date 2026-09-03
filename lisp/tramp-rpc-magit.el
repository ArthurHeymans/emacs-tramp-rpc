;;; tramp-rpc-magit.el --- Magit and Projectile support for TRAMP-RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Assisted-by: various LLMs
;; Keywords: comm, processes, vc

;; This file is part of tramp-rpc.

;; tramp-rpc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This file provides magit and projectile integration for tramp-rpc:
;; - Parallel git command prefetch for fast magit-status
;; - Process-file cache for serving git commands from prefetched data
;; - Ancestor directory scanning for project/VC detection
;; - Lazy Magit section expansion prefetch
;; - Projectile optimizations for remote directories
;;
;; The generic metadata caches and fs.events handling that this module
;; builds on live in tramp-rpc-cache.el.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'tramp)
(require 'tramp-cache)
(require 'tramp-rpc-protocol)
(require 'tramp-rpc-connection)
(require 'tramp-rpc-cache)

;; Functions from tramp-rpc.el
(declare-function tramp-rpc--call "tramp-rpc")
(declare-function tramp-rpc--get-connection "tramp-rpc" (vec))
(declare-function tramp-rpc--call-batch "tramp-rpc")
(declare-function tramp-rpc--decode-output "tramp-rpc")
(declare-function tramp-rpc--process-environment "tramp-rpc")
(declare-function tramp-rpc--decode-string "tramp-rpc")
(declare-function tramp-rpc--binary-bytes "tramp-rpc")
(declare-function tramp-rpc--path-to-bytes "tramp-rpc")
(declare-function tramp-rpc--path-to-compatible-value "tramp-rpc")
(declare-function tramp-rpc--encode-path "tramp-rpc")
(declare-function tramp-rpc--convert-file-attributes "tramp-rpc")
(declare-function tramp-rpc-file-name-p "tramp-rpc")
(declare-function tramp-rpc--add-external-operation "tramp-rpc")
(declare-function tramp-rpc--remove-external-operation "tramp-rpc")

;; Functions from magit-section.el.
(autoload 'magit-section-show "magit-section")

;; Silence byte-compiler warnings for external functions
(autoload 'projectile-dir-files-alien "projectile")
(autoload 'projectile-time-seconds "projectile")

;; Variables from magit-diff.el.
(defvar magit-diff-adjust-tab-width)

(defvar tramp-rpc-magit--magit-enabled nil
  "Non-nil when Magit integrations are installed.")

(defvar tramp-rpc-magit--projectile-enabled nil
  "Non-nil when Projectile integrations are installed.")

;; ============================================================================
;; Magit integration - client-side parallel prefetch
;; ============================================================================

;; The prefetch sends all git commands magit will need via a single
;; commands.run_parallel RPC call.  The server runs them in parallel
;; using Tokio-managed child processes and returns
;; {key: {exit_code, stdout, stderr}}.
;; The results are stored directly as the process-file cache --
;; no reconstruction or key normalization needed.

(defcustom tramp-rpc-magit-optimize t
  "Whether to enable magit prefetch optimizations.
When non-nil, tramp-rpc will automatically install handlers on
`magit-status-setup-buffer' and `magit-status-refresh-buffer' to
prefetch git commands in parallel, dramatically speeding up
magit-status on remote repositories."
  :type 'boolean
  :group 'tramp-rpc)

(defvar tramp-rpc-magit--process-caches (make-hash-table :test 'equal)
  "Hash table mapping (conn-key . directory) to `process-file' cache entries.
Each value stores a timestamp and a hash table mapping git arg keys to
\(exit-code . stdout-string).  Keyed per connection and per directory to
support multiple remotes and repos.")

(defcustom tramp-rpc-magit-process-cache-ttl 120
  "Seconds to keep prefetched Magit git command output.

Magit creates some expensive status sections lazily.  For example,
pressing TAB on the Unstaged changes or Untracked files section
can run git after the initial status refresh has completed.  Keeping the
prefetched command output briefly lets those lazy expansions reuse the
same batched status snapshot instead of making another remote round-trip.

Filesystem watch notifications and the next status refresh still clear or
replace this cache; this TTL is only a backstop for unwatched changes."
  :type 'number
  :group 'tramp-rpc)

(defcustom tramp-rpc-magit-disable-remote-diff-tab-width-detection t
  "Whether remote Magit status expansion should skip per-file tab-width probing.

Magit can inspect each changed file while washing diffs to derive its
buffer-local `tab-width'.  On TRAMP-RPC remotes that may open many files
and trigger many serial `file.stat', `file.truename', and process RPCs
when pressing TAB in `magit-status'.  When this option is non-nil,
TRAMP-RPC binds `magit-diff-adjust-tab-width' to nil while refreshing or
expanding remote Magit status sections, avoiding those round-trips and
using the current `tab-width' instead."
  :type 'boolean
  :group 'tramp-rpc)

(defconst tramp-rpc-magit--metadata-batch-size 64
  "Maximum metadata requests sent in one RPC batch.
The server rejects batches larger than 64 entries, so status prefetches for
large worktrees must be split without losing item/result ordering.")

(defconst tramp-rpc-magit--ancestor-marker-names
  '(".git" ".svn" ".hg" ".bzr" "_darcs"
    ".fslckout" "_FOSSIL_" ".pijul" ".sl" ".jj"
    ".projectile" ".project" ".dir-locals.el" ".editorconfig")
  "Marker names checked by project, VC, Projectile, and editorconfig code.")

(defvar tramp-rpc-magit--ancestor-scan-caches (make-hash-table :test 'equal)
  "Cached ancestor scans keyed by remote search directory.")

(defcustom tramp-rpc-magit-ancestor-cache-max-size 128
  "Maximum number of cached ancestor scans."
  :type 'integer
  :group 'tramp-rpc)

(defvar tramp-rpc-magit--prefetch-directories (make-hash-table :test 'equal)
  "Remote directories with active prefetch snapshots, keyed by directory.
Values are creation timestamps so independent repositories cannot clobber
one another's ancestor-discovery state.")

(defcustom tramp-rpc-magit-prefetch-directory-max-size 128
  "Maximum number of active prefetch directory snapshots."
  :type 'integer
  :group 'tramp-rpc)

(defcustom tramp-rpc-magit-process-cache-max-size 64
  "Maximum number of per-directory Magit process caches."
  :type 'integer
  :group 'tramp-rpc)

(defun tramp-rpc-magit--clear-ancestor-caches ()
  "Clear cached ancestor marker scans."
  (when (boundp 'tramp-rpc-magit--ancestor-scan-caches)
    (clrhash tramp-rpc-magit--ancestor-scan-caches))
  (when (boundp 'tramp-rpc-magit--prefetch-directories)
    (clrhash tramp-rpc-magit--prefetch-directories)))

(defun tramp-rpc-magit--clear-ancestor-caches-for-connection (vec)
  "Clear ancestor caches belonging to the connection identified by VEC."
  (let ((connection-key (tramp-rpc--connection-key-string vec)))
    (tramp-rpc--hash-remove-if
     (lambda (key _entry)
       (and (consp key) (equal (car key) connection-key)))
     tramp-rpc-magit--ancestor-scan-caches)
    (tramp-rpc--hash-remove-if
     (lambda (directory _timestamp)
       (equal (tramp-rpc-magit--file-connection-key directory)
              connection-key))
     tramp-rpc-magit--prefetch-directories)))

(defun tramp-rpc-magit--bound-table
    (table max-size &optional timestamp-function timestamp-valid-p)
  "Prune expired entries and bound TABLE to MAX-SIZE.
TIMESTAMP-FUNCTION extracts an entry timestamp.  TIMESTAMP-VALID-P checks
that timestamp and defaults to the file metadata cache policy.  Overflow
eviction follows the oldest timestamps without a full sort on normal cache
admission."
  (when timestamp-function
    (let ((valid-p (or timestamp-valid-p
                       #'tramp-rpc--cache-entry-valid-p)))
      (tramp-rpc--hash-remove-if
       (lambda (_key value)
         (when-let* ((timestamp (funcall timestamp-function value)))
           (not (funcall valid-p timestamp))))
       table)))
  (let ((excess (- (hash-table-count table) (max 0 max-size))))
    (dotimes (_ excess)
      (let (oldest-key oldest-time)
        (maphash
         (lambda (key value)
           (let ((time (and timestamp-function
                            (funcall timestamp-function value))))
             (when (or (null oldest-key)
                       (null time)
                       (and oldest-time (< time oldest-time)))
               (setq oldest-key key oldest-time time))))
         table)
        (when oldest-key
          (remhash oldest-key table))))))

(defun tramp-rpc-magit--prune-prefetch-directories ()
  "Remove expired entries from `tramp-rpc-magit--prefetch-directories'."
  (tramp-rpc-magit--bound-table
   tramp-rpc-magit--prefetch-directories
   tramp-rpc-magit-prefetch-directory-max-size #'identity))

(defvar tramp-rpc-magit--debug nil
  "When non-nil, log cache hits/misses for debugging.")

(defvar tramp-rpc-magit--allow-process-cache nil
  "Non-nil when prefetched git output may satisfy `process-file'.

This is dynamically bound while Magit is refreshing or lazily expanding a
status section.  Outside those windows, exact git command matches should
run normally instead of using a possibly stale status snapshot.")

(defvar tramp-rpc-magit--status-setup-prefetch-active nil
  "Non-nil while a status setup prefetch covers nested refresh calls.")


(defun tramp-rpc-magit--file-connection-key (filename)
  "Return the normalized connection key for remote FILENAME, or nil."
  (when (file-remote-p filename)
    (with-parsed-tramp-file-name filename nil
      (tramp-rpc--connection-key-string v))))

(defun tramp-rpc-magit--get-cache-key (vec directory)
  "Build a cache key for VEC and DIRECTORY.
Returns a cons cell (connection-key . directory) for hash table lookups."
  (cons (tramp-rpc--connection-key-string vec)
        (expand-file-name directory)))

(defun tramp-rpc-magit--valid-process-cache (key)
  "Return the non-expired process cache for KEY, or nil."
  (let* ((entry (gethash key tramp-rpc-magit--process-caches))
         ;; Older sessions may still contain the pre-TTL representation.
         (cache (if (hash-table-p entry)
                    entry
                  (plist-get entry :cache)))
         (time (and (not (hash-table-p entry))
                    (plist-get entry :time))))
    (cond
     ((not cache) nil)
     ((and time
           (not (tramp-rpc-magit--process-cache-timestamp-valid-p time)))
      (remhash key tramp-rpc-magit--process-caches)
      nil)
     (t cache))))

(defun tramp-rpc-magit--get-process-cache ()
  "Get the `process-file' cache for the current `default-directory'.
Returns the cache hash table, or nil if none."
  (when (file-remote-p default-directory)
    (with-parsed-tramp-file-name default-directory nil
      (tramp-rpc-magit--valid-process-cache
       (tramp-rpc-magit--get-cache-key v default-directory)))))

(defun tramp-rpc-magit--process-cache-timestamp-valid-p (timestamp)
  "Return non-nil when process cache TIMESTAMP is within its own TTL.
When push notifications are unavailable, cap to
`tramp-rpc--watcher-unavailable-ttl' so unwatched changes surface promptly."
  (or (null tramp-rpc-magit-process-cache-ttl)
      (let ((ttl tramp-rpc-magit-process-cache-ttl))
        (when (and (boundp 'tramp-rpc--watcher-degraded)
                   tramp-rpc--watcher-degraded
                   (boundp 'tramp-rpc--watcher-unavailable-ttl))
          (setq ttl (min ttl tramp-rpc--watcher-unavailable-ttl)))
        (<= (- (float-time) timestamp) ttl))))

(defun tramp-rpc-magit--set-process-cache (vec directory cache)
  "Set the `process-file' CACHE for VEC and DIRECTORY."
  (let ((key (tramp-rpc-magit--get-cache-key vec directory)))
    (puthash key (list :time (float-time) :cache cache)
             tramp-rpc-magit--process-caches)
    (tramp-rpc-magit--bound-table
     tramp-rpc-magit--process-caches tramp-rpc-magit-process-cache-max-size
     (lambda (entry)
       (and (listp entry) (plist-get entry :time)))
     #'tramp-rpc-magit--process-cache-timestamp-valid-p)))


(defun tramp-rpc-magit--process-cache-key (&rest args)
  "Build a cache key from git ARGS.
Use a printed argv list rather than joining with a separator, so pathspecs
containing characters such as `|' cannot collide with a different argv vector."
  (prin1-to-string (mapcar (lambda (arg)
                             (if (stringp arg)
                                 (substring-no-properties arg)
                               arg))
                           args)))

(defconst tramp-rpc-magit--ignorable-git-global-config
  (append '("core.preloadIndex=true"
            "log.showSignature=false"
            "color.ui=false"
            "color.diff=false"
            "diff.noPrefix=false")
          (when (eq system-type 'windows-nt)
            '("i18n.logOutputEncoding=UTF-8")))
  "Git `-c' assignments that Magit adds and prefetch reproduces.")

(defconst tramp-rpc-magit--git-prefetch-prefix-args
  (append '("--no-pager" "--literal-pathspecs")
          (apply #'append
                 (mapcar (lambda (assignment) (list "-c" assignment))
                         tramp-rpc-magit--ignorable-git-global-config)))
  "Global git arguments used for prefetched git commands.")

(defconst tramp-rpc-magit--uncacheable-git-subcommands
  '("update-index")
  "Git subcommands that must never be served from the Magit process cache.")

(defconst tramp-rpc-magit--state-files
  '("MERGE_HEAD" "REVERT_HEAD" "CHERRY_PICK_HEAD" "ORIG_HEAD"
    "FETCH_HEAD" "AUTO_MERGE" "SQUASH_MSG"
    "BISECT_LOG" "BISECT_CMD_OUTPUT" "BISECT_TERMS"
    "rebase-merge" "rebase-merge/git-rebase-todo"
    "rebase-merge/done" "rebase-merge/onto"
    "rebase-merge/orig-head" "rebase-merge/head-name"
    "rebase-merge/amend" "rebase-merge/stopped-sha"
    "rebase-merge/rewritten-pending"
    "rebase-apply" "rebase-apply/onto"
    "rebase-apply/head-name" "rebase-apply/applying"
    "rebase-apply/original-commit" "rebase-apply/rewritten"
    "sequencer" "sequencer/todo" "sequencer/head"
    "HEAD" "config" "index" "refs/stash"
    "info/exclude" "NOTES_MERGE_WORKTREE")
  "Git state files that magit checks for existence under .git/.
These are checked speculatively during prefetch (assuming .git as
the gitdir) and the results are cached in `tramp-rpc--file-exists-cache'.")

(defun tramp-rpc-magit--state-file-entry (gitdir relative-path)
  "Return a commands.run_parallel entry testing RELATIVE-PATH under GITDIR."
  (let ((full-path (concat (file-name-as-directory gitdir) relative-path)))
    `((key . ,(concat "state_file:" full-path))
      (cmd . "test")
      (args . ["-e" ,full-path]))))

(defun tramp-rpc-magit--prefetch-git-commands (directory &optional _vec)
  "Build the list of git commands to prefetch for DIRECTORY.
Returns a vector of command entries for commands.run_parallel.
Each entry has key, cmd, args, and cwd fields.  Git command keys
match what `tramp-rpc-magit--process-cache-lookup' will look up.
State file checks use \"state_file:PATH\" keys.
The batch RPC supplies the same effective environment as `process-file'."
  (let ((cmds nil)
        (gitdir (concat (file-name-as-directory directory) ".git")))
    (cl-flet ((add-git (&rest args)
                (push `((key . ,(apply #'tramp-rpc-magit--process-cache-key args))
                        (cmd . "git")
                        (args . ,(vconcat (append tramp-rpc-magit--git-prefetch-prefix-args
                                                   args)))
                        (cwd . ,directory))
                      cmds))
              (add-state-file (relative-path)
                (push (tramp-rpc-magit--state-file-entry gitdir relative-path)
                      cmds)))
      ;; State file existence checks (speculative, assuming .git gitdir)
      (dolist (sf tramp-rpc-magit--state-files)
        (add-state-file sf))

      ;; Basic repo info
      (add-git "rev-parse" "--show-toplevel")
      (add-git "rev-parse" "--git-dir")

      ;; HEAD info
      (add-git "rev-parse" "HEAD")
      (add-git "rev-parse" "--short" "HEAD")
      (add-git "rev-parse" "--short=9" "HEAD")
      (add-git "symbolic-ref" "--short" "HEAD")
      (add-git "log" "-1" "--format=%s" "HEAD")
      (add-git "rev-parse" "--verify" "HEAD")
      (add-git "symbolic-ref" "HEAD")

      ;; Upstream / push
      (add-git "rev-parse" "--abbrev-ref" "@{upstream}")
      (add-git "rev-list" "--count" "--left-right" "@{upstream}...HEAD")
      (add-git "rev-parse" "--abbrev-ref" "@{push}")
      (add-git "rev-list" "--count" "--left-right" "@{push}...HEAD")

      ;; Diffs
      (add-git "diff" "--ita-visible-in-index" "--no-ext-diff" "--no-prefix" "--")
      (add-git "diff" "--ita-visible-in-index" "--cached" "--no-ext-diff" "--no-prefix" "--")
      (add-git "diff" "--cached" "--stat" "--no-color")
      (add-git "diff" "--stat" "--no-color")

      ;; Untracked files
      (add-git "ls-files" "--others" "--exclude-standard" "--directory" "--no-empty-directory")
      (add-git "status" "-z" "--porcelain" "--untracked-files=all" "--")

      ;; File-list sections are inserted lazily when expanding collapsed
      ;; Magit status sections.  Prefetch their common commands so TAB can be
      ;; served from the same batched snapshot.
      (add-git "ls-files" "-z" "--full-name")
      (add-git "ls-files" "-z" "--full-name" "--cached")
      (add-git "ls-files" "-z" "--full-name" "--others" "--ignored" "--exclude-standard")
      (add-git "ls-files" "-z" "--full-name" "-t")
      (add-git "ls-files" "-z" "--full-name" "-v")

      ;; Tags
      (add-git "describe" "--tags" "--exact-match" "HEAD")
      (add-git "describe" "--tags" "--abbrev=0")
      (add-git "describe" "--long" "--tags")
      (add-git "describe" "--contains" "HEAD")

      ;; Remotes
      (add-git "remote")
      (add-git "remote" "get-url" "origin")

      ;; Config
      (add-git "config" "user.name")
      (add-git "config" "user.email")
      (add-git "config" "remote.origin.url")
      (add-git "config" "--bool" "--default" "false" "core.bare")
      (add-git "config" "--list" "-z")
      (add-git "config" "--local" "-z" "--get-all" "--include" "status.showUntrackedFiles")
      (add-git "config" "-z" "--get-all" "--include" "core.abbrev")
      (add-git "config" "-z" "--get-all" "--include" "forge.remote")
      (add-git "config" "--get" "remote.upstream.url")
      (add-git "config" "--get" "remote.origin.url")

      ;; Revision/name formatting used while washing expanded file sections.
      (add-git "for-each-ref" "--format=%(symref)\f%(refname)" "refs/")
      (add-git "for-each-ref" "--format=%(symref)\f%(refname:short)" "refs/")
      (add-git "symbolic-ref" "refs/remotes/origin/HEAD")

      ;; Porcelain status
      (add-git "status" "-z" "--porcelain" "--untracked-files=normal" "--")
      (add-git "status" "--porcelain" "--branch")

      ;; Bare repo check
      (add-git "rev-parse" "--is-bare-repository")

      ;; Stash
      (add-git "rev-parse" "--verify" "refs/stash")
      (add-git "reflog" "--format=%gd%x00%aN%x00%at%x00%gs" "refs/stash")

      ;; Parent commits
      (add-git "rev-parse" "--short" "HEAD~")
      (add-git "rev-parse" "--short=9" "HEAD~")
      (add-git "rev-parse" "--verify" "HEAD~10")

      ;; Recent log with decorations
      (add-git "log" "--format=%h%x0c%D%x0c%x0c%aN%x0c%at%x0c%x0c%s"
               "--decorate=full" "-n10" "--use-mailmap" "--no-prefix" "--")

      ;; Log for header line
      (add-git "log" "--no-walk" "--format=%h %s" "HEAD^{commit}" "--"))

    (vconcat (nreverse cmds))))

(defun tramp-rpc-magit--git-command-entry (directory args &optional _vec)
  "Return a commands.run_parallel entry for git ARGS in DIRECTORY."
  `((key . ,(apply #'tramp-rpc-magit--process-cache-key args))
    (cmd . "git")
    (args . ,(vconcat (append tramp-rpc-magit--git-prefetch-prefix-args args)))
    (cwd . ,directory)))

(defun tramp-rpc-magit--store-command-results (vec directory results &optional replace)
  "Merge commands.run_parallel RESULTS into DIRECTORY's process cache.
When REPLACE is non-nil, build a fresh cache instead of merging into an
existing one.
VEC is the TRAMP connection vector."
  (when results
    (let* ((key (tramp-rpc-magit--get-cache-key vec directory))
           (cache (if replace
                      (make-hash-table :test 'equal)
                    (or (tramp-rpc-magit--valid-process-cache key)
                        (make-hash-table :test 'equal))))
           (remote-prefix (file-remote-p directory)))
      (dolist (entry results)
        (let* ((cmd-key (if (symbolp (car entry))
                            (symbol-name (car entry))
                          (car entry)))
               (data (cdr entry))
               (exit-code (alist-get 'exit_code data)))
          ;; Older bounded-admission servers reported transient load this way.
          ;; Keep accepting those responses, but never cache them as git results
          ;; for the whole TTL.
          (unless (eq (alist-get 'not_admitted data) t)
            (if (string-prefix-p "state_file:" cmd-key)
                (let* ((remote-path (substring cmd-key (length "state_file:")))
                       (tramp-path (concat remote-prefix remote-path)))
                  (tramp-rpc--cache-put tramp-rpc--file-exists-cache
                                        tramp-path
                                        (= exit-code 0)))
              (let* ((stdout-raw (alist-get 'stdout data))
                     (stdout (tramp-rpc--decode-output stdout-raw)))
                (puthash cmd-key (cons exit-code stdout) cache))))))
      (tramp-rpc-magit--set-process-cache vec directory cache)
      cache)))

(defconst tramp-rpc-magit--run-parallel-command-limit 200
  "Maximum commands per Magit `commands.run_parallel' RPC.
The server currently rejects batches above 256; keep headroom so dynamic
prefetch growth does not trip that hard limit.")

(defun tramp-rpc-magit--run-parallel (vec directory commands)
  "Run COMMANDS on VEC in DIRECTORY with the normal RPC process environment."
  (let ((localname (or (file-remote-p (expand-file-name directory) 'localname)
                       directory)))
    (tramp-rpc--call
     vec "commands.run_parallel"
     `((commands . ,commands)
       (env . ,(tramp-rpc--process-environment vec localname))))))

(defun tramp-rpc-magit--run-command-entries (vec directory commands)
  "Run COMMANDS in chunks and merge their results into DIRECTORY's cache.
VEC is the TRAMP connection vector."
  (let ((remaining (append commands nil))
        cache)
    (while remaining
      (let ((chunk nil)
            (count 0))
        (while (and remaining
                    (< count tramp-rpc-magit--run-parallel-command-limit))
          (push (pop remaining) chunk)
          (setq count (1+ count)))
        (setq chunk (nreverse chunk))
        (setq cache
              (tramp-rpc-magit--store-command-results
               vec directory
               (tramp-rpc-magit--run-parallel
                vec directory (vconcat chunk))))))
    cache))

(defun tramp-rpc-magit--cached-git-stdout (cache &rest args)
  "Return cached stdout for git ARGS in CACHE, or nil on miss/error.
If ARGS ends with `:raw', preserve stdout exactly instead of trimming
whitespace."
  (let ((raw (eq (car (last args)) :raw)))
    (when raw
      (setq args (butlast args)))
    (when-let* ((entry (gethash (apply #'tramp-rpc-magit--process-cache-key args)
                                cache)))
      (when (= 0 (car entry))
        (if raw
            (cdr entry)
          (string-trim (cdr entry)))))))

(defun tramp-rpc-magit--status-files-from-porcelain (porcelain)
  "Extract touched files from NUL-delimited git PORCELAIN status output."
  (let ((records (split-string (or porcelain "") "\0" t))
        files)
    (while records
      (let ((record (pop records)))
        (when (>= (length record) 4)
          (let ((x (aref record 0))
                (y (aref record 1))
                (path (substring record 3)))
            (cond
             ;; Rename/copy porcelain v1 -z uses the next NUL record as the
             ;; original path.  The first path is the one Magit displays in
             ;; status and later expands.
             ((or (memq x '(?R ?C)) (memq y '(?R ?C)))
              (push path files)
              (when records (pop records)))
             ((not (string-empty-p path))
              (push path files)))))))
    (delete-dups (nreverse files))))

(defun tramp-rpc-magit--remote-path (vec localname)
  "Return the TRAMP filename for LOCALNAME on VEC."
  (tramp-make-tramp-file-name vec localname))

(defun tramp-rpc-magit--cache-file-stat (vec localname stat)
  "Populate TRAMP/file-exists caches for LOCALNAME from STAT.
VEC is the TRAMP connection vector."
  (let ((localnames (list localname)))
    ;; TRAMP distinguishes directory spellings with and without trailing slash
    ;; in its property keys.  Magit tends to ask for the slash spelling later,
    ;; while our metadata prefetch naturally de-duplicates to the canonical
    ;; no-slash spelling, so populate both.
    (when (and stat (equal (alist-get 'type stat) "directory"))
      (push (file-name-as-directory (directory-file-name localname))
            localnames))
    (dolist (ln (delete-dups localnames))
      (let ((filename (tramp-rpc-magit--remote-path vec ln))
            (symlink-p (and stat (equal (alist-get 'type stat) "symlink"))))
        ;; This metadata batch uses lstat.  A symlink lstat does not tell us
        ;; whether following the symlink succeeds, so don't populate
        ;; `file-exists-p' or follow-stat from it.
        (unless symlink-p
          (tramp-rpc--cache-put tramp-rpc--file-exists-cache filename (if stat t nil)))
        (tramp-rpc--cache-file-stat-result vec ln stat t)
        (when stat
          (tramp-set-file-property
           vec ln "file-attributes-nil"
           (tramp-rpc--convert-file-attributes stat nil))
          (tramp-set-file-property
           vec ln "file-attributes-integer"
           (tramp-rpc--convert-file-attributes stat 'integer))
          (pcase (alist-get 'type stat)
            ("directory" (tramp-set-file-property vec ln "file-directory-p" t))
            ((or "file" (pred null))
             (tramp-set-file-property vec ln "file-directory-p" nil))))))))

(defun tramp-rpc-magit--ref-short-names (cache)
  "Return short ref names from cached `for-each-ref' output in CACHE."
  (when-let* ((entry (gethash (tramp-rpc-magit--process-cache-key
                               "for-each-ref"
                               "--format=%(symref)\f%(refname:short)" "refs/")
                              cache))
              ((= 0 (car entry))))
    (let ((sep (string ?\f)))
      (delq nil
            (mapcar (lambda (line)
                      (let ((parts (split-string line (regexp-quote sep))))
                        (cadr parts)))
                    (split-string (cdr entry) "\n" t))))))

(defun tramp-rpc-magit--remote-branch-candidates (cache branch)
  "Return remote branch names in CACHE likely related to BRANCH."
  (when (and branch (not (string-empty-p branch)))
    (let* ((remotes (when-let* ((remote-output
                                 (tramp-rpc-magit--cached-git-stdout
                                  cache "remote")))
                      (split-string remote-output "\n" t)))
           (from-remotes (mapcar (lambda (remote)
                                   (concat remote "/" branch))
                                 remotes))
           (suffix (concat "/" branch))
           (from-refs
            (cl-remove-if-not
             (lambda (name)
               (and (string-match-p "/" name)
                    (string-suffix-p suffix name)))
             (tramp-rpc-magit--ref-short-names cache))))
      (delete-dups (append from-remotes from-refs)))))

(defun tramp-rpc-magit--cache-file-truename (vec localname result)
  "Populate `file-truename' cache for LOCALNAME from RESULT.
VEC is the TRAMP connection vector."
  (when result
    (let* ((truename-local (tramp-rpc--decode-string
                            (if (consp result)
                                (alist-get 'path result)
                              result)))
           (filename (tramp-rpc-magit--remote-path vec localname))
           (truename (tramp-rpc-magit--remote-path vec truename-local)))
      (tramp-rpc--cache-put tramp-rpc--file-truename-cache filename truename))))

(defun tramp-rpc-magit--prefetch-file-metadata (vec files)
  "Batch file.stat/file.truename for local FILES on VEC and cache them."
  (let (items requests)
    (dolist (file files)
      (let ((local-file (directory-file-name file))
            (local-dir (directory-file-name (file-name-directory file))))
        (dolist (path (list local-file local-dir))
          (when (and path (not (member (list 'stat path) items)))
            (push (list 'stat path) items)
            (push (cons "file.stat"
                        (append (tramp-rpc--encode-path path) '((lstat . t))))
                  requests)))
        (unless (member (list 'truename local-file) items)
          (push (list 'truename local-file) items)
          (push (cons "file.truename" (tramp-rpc--encode-path local-file))
                requests))))
    (when requests
      (setq items (nreverse items)
            requests (nreverse requests))
      (while requests
        (let (batch-items batch-requests)
          (dotimes (_ tramp-rpc-magit--metadata-batch-size)
            (when requests
              (push (pop items) batch-items)
              (push (pop requests) batch-requests)))
          (setq batch-items (nreverse batch-items)
                batch-requests (nreverse batch-requests))
          (cl-mapc
           (lambda (item result)
             (unless (and (consp result) (plist-get result :error))
               (pcase (car item)
                 ('stat (tramp-rpc-magit--cache-file-stat
                         vec (cadr item) result))
                 ('truename (tramp-rpc-magit--cache-file-truename
                             vec (cadr item) result)))))
           batch-items
           (tramp-rpc--call-batch vec batch-requests)))))))

(defun tramp-rpc-magit--prefetch-dynamic-status (vec directory root-local)
  "Prefetch status data that depends on initial git output.
This second-stage batch covers worktree-specific gitdirs, current branch and
upstream names, and commands used to wash already-expanded file sections.
VEC is the TRAMP connection vector.
DIRECTORY is the directory being handled.
ROOT-LOCAL is the local form of the repository root."
  (when-let* ((cache (tramp-rpc-magit--get-process-cache)))
    (let ((commands nil)
          (expanded-files (list (directory-file-name root-local))))
      (cl-labels
          ((cached (&rest args)
             (apply #'tramp-rpc-magit--cached-git-stdout cache args))
           (add-git (&rest args)
             (let ((key (apply #'tramp-rpc-magit--process-cache-key args)))
               (unless (gethash key cache)
                 (push (tramp-rpc-magit--git-command-entry root-local args vec)
                       commands))))
           (add-state-dir (gitdir)
             (dolist (sf tramp-rpc-magit--state-files)
               (push (tramp-rpc-magit--state-file-entry gitdir sf) commands)))
           (add-log-range (range)
             (add-git "log" "--format=%h%x0c%D%x0c%x0c%aN%x0c%at%x0c%x0c%s"
                      "--decorate=full" "-n256" "--use-mailmap"
                      "--no-prefix" range "--"))
           (add-ref-name (name)
             (when (and name (not (string-empty-p name)))
               (add-git "rev-parse" "--verify" name)
               (add-git "rev-parse" "--verify" "--abbrev-ref" name)
               (add-git "rev-parse" "--verify" (concat "refs/tags/" name)))))
        ;; Magit checks state files in the real gitdir.  In linked worktrees,
        ;; that is not WORKTREE/.git, so use the prefetched rev-parse result.
        (when-let* ((gitdir (cached "rev-parse" "--git-dir")))
          (add-state-dir (if (file-name-absolute-p gitdir)
                             gitdir
                           (expand-file-name gitdir root-local))))

        ;; Current branch/upstream/ref-name probes.
        (let* ((branch (cached "symbolic-ref" "--short" "HEAD"))
               (upstream (cached "rev-parse" "--abbrev-ref" "@{upstream}"))
               (origin-head-ref (cached "symbolic-ref" "refs/remotes/origin/HEAD"))
               (origin-head-short (and origin-head-ref
                                       (string-remove-prefix
                                        "refs/remotes/" origin-head-ref)))
               (origin-head-branch (and origin-head-short
                                        (file-name-nondirectory origin-head-short)))
               (names (delete-dups
                       (delq nil (append
                                  (list branch
                                        (and branch (concat branch "@{upstream}"))
                                        upstream
                                        "origin/HEAD"
                                        origin-head-short
                                        origin-head-branch)
                                  (tramp-rpc-magit--remote-branch-candidates
                                   cache branch))))))
          (dolist (name names)
            (add-ref-name name)
            (when (and name (not (string-suffix-p "@{upstream}" name)))
              (add-log-range (concat name ".."))
              (add-log-range (concat ".." name)))))

        ;; File-section wash commands for files already expanded in status.
        (let* ((status (or (cached "status" "-z" "--porcelain"
                                   "--untracked-files=normal" "--" :raw)
                           (cached "status" "-z" "--porcelain"
                                   "--untracked-files=all" "--" :raw)))
               (files (tramp-rpc-magit--status-files-from-porcelain status))
               (head (cached "rev-parse" "HEAD")))
          (when head
            (add-git "rev-parse" "--short=9" head)
            (add-git "cat-file" "-t" head)
            (add-git "rev-parse" "--verify" (concat head "^{commit}")))
          (dolist (file files)
            (let ((abs-file (expand-file-name file root-local)))
              (push abs-file expanded-files)
              (add-git "diff" "--quiet" "--cached" "--submodule=short"
                       "--" file)
              (add-git "ls-files" "-c" "-z" "--" file)
              (when head
                (add-git "ls-tree" "--full-tree" head "--" abs-file)
                (add-git "cat-file" "-p" (format "%s:%s" head file))))))

        (when commands
          (tramp-rpc-magit--run-command-entries
           vec directory (nreverse commands)))
        (when expanded-files
          (tramp-rpc-magit--prefetch-file-metadata
           vec (delete-dups (nreverse expanded-files))))))))

(defun tramp-rpc-magit--prefetch-file-section (section)
  "Prefetch the git commands needed to expand Magit file SECTION.
This is intentionally much smaller than the full status prefetch and is used
when the status cache has expired but TAB is expanding a single file section."
  (when-let* ((file (tramp-rpc-magit--section-slot section 'value))
              ((stringp file))
              (directory default-directory)
              ((file-remote-p directory))
              ((tramp-rpc-file-name-p directory)))
    (with-parsed-tramp-file-name directory nil
      (let* ((root-local (file-name-as-directory localname))
             (rel-file (if (file-name-absolute-p file)
                           (file-relative-name file root-local)
                         file))
             (abs-file (expand-file-name rel-file root-local))
             (cache (tramp-rpc-magit--get-process-cache))
             (diff-key (tramp-rpc-magit--process-cache-key
                        "diff" "--quiet" "--cached" "--submodule=short"
                        "--" rel-file)))
        (unless (and cache (gethash diff-key cache))
          ;; First ensure we know the full HEAD object name; several Magit
          ;; helpers subsequently ask about that exact object, not the symbol
          ;; HEAD, so the cache key must contain the resolved object name.
          (unless (and cache (gethash (tramp-rpc-magit--process-cache-key
                                       "rev-parse" "HEAD")
                                      cache))
            (tramp-rpc-magit--store-command-results
             v directory
             (tramp-rpc-magit--run-parallel
              v directory
              (vector (tramp-rpc-magit--git-command-entry
                       root-local '("rev-parse" "HEAD") v)))))
          (setq cache (tramp-rpc-magit--get-process-cache))
          (let* ((head-entry (and cache
                                  (gethash (tramp-rpc-magit--process-cache-key
                                            "rev-parse" "HEAD")
                                           cache)))
                 (head (and head-entry (= 0 (car head-entry))
                            (string-trim (cdr head-entry))))
                 (commands nil))
            (cl-labels ((add (&rest args)
                          (push (tramp-rpc-magit--git-command-entry
                                 root-local args v)
                                commands)))
              (add "diff" "--quiet" "--cached" "--submodule=short"
                   "--" rel-file)
              (add "ls-files" "-c" "-z" "--" rel-file)
              (add "config" "-z" "--get-all" "--include" "core.abbrev")
              (add "for-each-ref" "--format=%(symref)\f%(refname)" "refs/")
              (add "for-each-ref" "--format=%(symref)\f%(refname:short)" "refs/")
              (when head
                (add "rev-parse" "--short=9" head)
                (add "cat-file" "-t" head)
                (add "rev-parse" "--verify" (concat head "^{commit}"))
                (add "ls-tree" "--full-tree" head "--" abs-file)
                (add "cat-file" "-p" (format "%s:%s" head rel-file))))
            (tramp-rpc-magit--store-command-results
             v directory
             (tramp-rpc-magit--run-parallel
              v directory (vconcat (nreverse commands))))))))))

(defun tramp-rpc-magit--strip-git-prefix-args (args)
  "Strip cache-neutral Magit git prefix flags from ARGS.
Return nil if ARGS contain semantic global flags that are not represented by
our prefetch command/key."
  (let ((rest (mapcar (lambda (arg)
                        (if (stringp arg) (substring-no-properties arg) arg))
                      (append args nil)))
        (safe t))
    (while (and safe rest
                (let ((arg (car rest)))
                  (cond
                   ;; `--literal-pathspecs' is cache-neutral for the prefetched
                   ;; commands we serve: commands with real pathspecs are
                   ;; prefetched in the literal form Magit uses, and commands
                   ;; without pathspecs are unaffected by the flag.
                   ((member arg '("--no-pager" "--literal-pathspecs")) t)
                   ((string= "-c" arg)
                    (let ((assignment (cadr rest)))
                      (if (member assignment
                                  tramp-rpc-magit--ignorable-git-global-config)
                          (progn (setq rest (cdr rest)) t)
                        (setq safe nil)
                        nil)))
                   ;; These global arguments change pathspec/repository
                   ;; semantics and are not modeled by the cache key.
                   ((member arg '("--glob-pathspecs" "--noglob-pathspecs" "-C"))
                    (setq safe nil)
                    nil)
                   (t nil))))
      (setq rest (cdr rest)))
    (and safe rest)))

(defun tramp-rpc-magit--git-cacheable-args-p (args)
  "Return non-nil if normalized git ARGS may be cached."
  (let ((subcommand (car args)))
    (and subcommand
         (not (string-prefix-p "-" subcommand))
         (not (member subcommand tramp-rpc-magit--uncacheable-git-subcommands)))))

(defun tramp-rpc-magit--git-cache-safe-environment-p ()
  "Return non-nil if the dynamic environment is safe for git cache reuse."
  (let ((baseline (default-toplevel-value 'process-environment))
        (safe t))
    (dolist (entry process-environment safe)
      (when (and safe
                 (stringp entry)
                 (not (member entry baseline))
                 (string-match-p "\\`GIT_[^=]*=" entry))
        (setq safe nil)))))

(defun tramp-rpc-magit--process-cache-lookup (program args)
  "Look up PROGRAM ARGS in the `process-file' cache.
Returns (exit-code . stdout) if found, nil otherwise."
  (when-let* (((bound-and-true-p tramp-rpc-magit--allow-process-cache))
              ((tramp-rpc-magit--git-cache-safe-environment-p))
              (cache (tramp-rpc-magit--get-process-cache)))
    (when (or (string-suffix-p "/git" program)
              (string= "git" program))
      (let* ((core-args (tramp-rpc-magit--strip-git-prefix-args args))
             (key (and (tramp-rpc-magit--git-cacheable-args-p core-args)
                       (apply #'tramp-rpc-magit--process-cache-key core-args)))
             (result (and key (gethash key cache))))
        (when tramp-rpc-magit--debug
          (if result
              (tramp-rpc--debug "process-file HIT (prefetch): git %s -> exit %d"
                                key (car result))
            (tramp-rpc--debug "process-file MISS (prefetch): git %s" key)))
        result))))

(defun tramp-rpc-magit--process-cache-store (program args exit-code stdout)
  "Store a just-run git PROGRAM ARGS result in the active Magit cache.
EXIT-CODE and STDOUT are the values returned by `process-file'."
  (when-let* (((bound-and-true-p tramp-rpc-magit--allow-process-cache))
              ((tramp-rpc-magit--git-cache-safe-environment-p))
              (cache (tramp-rpc-magit--get-process-cache)))
    (when (or (string-suffix-p "/git" program)
              (string= "git" program))
      (let* ((core-args (tramp-rpc-magit--strip-git-prefix-args args))
             (key (and (tramp-rpc-magit--git-cacheable-args-p core-args)
                       (apply #'tramp-rpc-magit--process-cache-key core-args))))
        (when key
          (puthash key (cons exit-code stdout) cache))))))

(defun tramp-rpc-magit--prefetch (directory)
  "Prefetch magit status and ancestor data for DIRECTORY.
Sends all git commands magit will need via a single
commands.run_parallel RPC call, then stores the results directly
as the `process-file' cache.  Also fetches ancestor markers."
  (when (and (file-remote-p directory)
             (tramp-rpc-file-name-p directory))
    ;; Suppress fs.events cache handling during prefetch.  The git commands
    ;; we run on the server touch .git/index etc., triggering inotify events
    ;; that would clear the cache we're building.
    (let ((tramp-rpc--suppress-fs-notifications t))
      ;; Remember every active repository independently.  Magit can refresh
      ;; multiple repositories from different threads or nested callbacks.
      (tramp-rpc-magit--prune-prefetch-directories)
      (puthash (expand-file-name directory) (float-time)
               tramp-rpc-magit--prefetch-directories)
      (tramp-rpc-magit--bound-table
       tramp-rpc-magit--prefetch-directories
       tramp-rpc-magit-prefetch-directory-max-size #'identity)
      (with-parsed-tramp-file-name directory nil
        ;; Build command list and run in parallel on server.  `update-index
        ;; --refresh' is intentionally not part of this prefetch; it is run by
        ;; Magit in the real refresh sequence, and `tramp-rpc-handle-process-file'
        ;; triggers this prefetch immediately after that command completes.
        (let* ((commands (tramp-rpc-magit--prefetch-git-commands localname v))
               (results (tramp-rpc-magit--run-parallel
                         v directory commands)))
          (when results
            ;; Each result entry is (key . {exit_code, stdout, stderr}).  Git
            ;; command results are stored as (exit-code . decoded-stdout), while
            ;; state file checks (key starts with "state_file:") populate the
            ;; file-exists cache.
            (tramp-rpc-magit--store-command-results v directory results t)
            ;; Second-stage prefetch for data whose names are only known after
            ;; the first batch (real gitdir for worktrees, branch/upstream
            ;; names, and the files reported by status porcelain).
            (tramp-rpc-magit--prefetch-dynamic-status v directory localname)
            ;; Auto-watch the git worktree
            (let* ((cache (tramp-rpc-magit--get-process-cache))
                   (toplevel-key (tramp-rpc-magit--process-cache-key
                                  "rev-parse" "--show-toplevel"))
                   (toplevel-entry (when cache
                                     (gethash toplevel-key cache)))
                   (toplevel (when (and toplevel-entry (= 0 (car toplevel-entry)))
                               (string-trim (cdr toplevel-entry)))))
              (when toplevel
                (tramp-rpc--auto-watch-git-worktree v toplevel)))))
        ;; Fetch ancestor markers for project/VC detection.  Normalize the
        ;; directory so the stored key matches the one
        ;; `tramp-rpc-magit--ancestor-scan-for-directory' looks up.
        (let ((scan (tramp-rpc-ancestors-scan
                     directory tramp-rpc-magit--ancestor-marker-names)))
          (puthash (tramp-rpc-magit--ancestor-scan-cache-key
                    (file-name-as-directory (expand-file-name directory)))
                   (cons (float-time) scan)
                   tramp-rpc-magit--ancestor-scan-caches)
          (tramp-rpc-magit--bound-table
           tramp-rpc-magit--ancestor-scan-caches
           tramp-rpc-magit-ancestor-cache-max-size #'car))
        (when tramp-rpc-magit--debug
          (let ((cache (tramp-rpc-magit--get-process-cache)))
            (tramp-rpc--debug "tramp-rpc-magit: prefetched %d commands + ancestors for %s"
                              (if cache (hash-table-count cache) 0)
                              directory)))))))

(defun tramp-rpc--auto-watch-git-worktree (vec toplevel)
  "Automatically watch a git worktree after prefetch.
VEC is the TRAMP connection vector.  TOPLEVEL is the local path
of the git worktree root on the remote."
  (when toplevel
    (let* ((key (format "%s:%s" (tramp-rpc--connection-key-string vec) toplevel))
           (entry (gethash key tramp-rpc--watched-directories)))
      (unless (tramp-rpc--watch-entry-recursive-p entry)
        ;; Not yet watching this worktree recursively - start watching.
        (condition-case err
            (progn
              (tramp-rpc-watch-directory
               (tramp-make-tramp-file-name vec toplevel) t)
              (tramp-rpc--debug "auto-watching git worktree %s" toplevel))
          (error
           (tramp-rpc--debug "failed to auto-watch %s: %s"
                             toplevel (error-message-string err))))))))

;; ============================================================================
;; Ancestor directory scanning
;; ============================================================================

(defun tramp-rpc-ancestors-scan (directory markers &optional max-depth)
  "Scan ancestor directories of DIRECTORY for MARKERS using server-side RPC.
MARKERS is a list of file/directory names to look for (e.g., \".git\" \".svn\").
MAX-DEPTH limits how far up the tree to search (default 10).

Returns an alist of (marker . found-directory) where found-directory is
the closest ancestor containing that marker, or nil if not found.

This is much faster than checking each ancestor individually because
the server scans the entire tree in one operation."
  (when (and (file-remote-p directory)
             (tramp-rpc-file-name-p directory))
    (with-parsed-tramp-file-name directory nil
      (let ((result (tramp-rpc--call v "ancestors.scan"
                                     `((directory . ,(tramp-rpc--path-to-compatible-value
                                                      localname))
                                       (markers . ,(vconcat markers))
                                       (max_depth . ,(or max-depth 10))))))
        ;; Marker names are text, but paths must remain raw bytes so invalid
        ;; UTF-8 can be compared with TRAMP localnames without replacement.
        (mapcar (lambda (pair)
                  (let ((key (car pair))
                        (val (cdr pair)))
                    (cons (if (symbolp key) (symbol-name key) key)
                          (when val
                            (tramp-rpc--binary-bytes val)))))
                result)))))

(defun tramp-rpc-magit--ancestor-scan-cache-key (directory)
  "Return cache key for ancestor scan rooted at DIRECTORY."
  (cons (tramp-rpc-magit--file-connection-key directory)
        (tramp-file-local-name directory)))

(defun tramp-rpc-magit--ancestor-scan-for-directory (directory)
  "Return cached ancestor marker scan for DIRECTORY, fetching it if needed."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (key (tramp-rpc-magit--ancestor-scan-cache-key directory))
         (entry (gethash key tramp-rpc-magit--ancestor-scan-caches)))
    (if (and entry (tramp-rpc--cache-entry-valid-p (car entry)))
        (cdr entry)
      (when entry
        (remhash key tramp-rpc-magit--ancestor-scan-caches))
      (let ((scan (tramp-rpc-ancestors-scan
                   directory tramp-rpc-magit--ancestor-marker-names)))
        (puthash key (cons (float-time) scan)
                 tramp-rpc-magit--ancestor-scan-caches)
        (tramp-rpc-magit--bound-table
         tramp-rpc-magit--ancestor-scan-caches
         tramp-rpc-magit-ancestor-cache-max-size #'car)
        scan))))

(defun tramp-rpc-magit--ancestor-cache-covers-p (scan-directory candidate-dir)
  "Return non-nil if SCAN-DIRECTORY's ancestor scan covers CANDIDATE-DIR."
  (let ((scan (file-name-as-directory (directory-file-name scan-directory)))
        (candidate (file-name-as-directory (directory-file-name candidate-dir))))
    (string-prefix-p candidate scan)))

(defun tramp-rpc-magit--file-exists-in-ancestor-scan (filename scan)
  "Return FILENAME existence using ancestor SCAN, or `not-cached'."
  (let* ((expanded (expand-file-name filename))
         (basename (file-name-nondirectory expanded))
         (entry (assoc basename scan)))
    (if entry
        (if (cdr entry)
            (let ((found-dir (directory-file-name (cdr entry)))
                  (candidate-dir
                   (directory-file-name
                    (file-name-directory
                     (tramp-rpc--path-to-bytes
                      (tramp-file-local-name expanded))))))
              (cond
               ((string= found-dir candidate-dir) t)
               ;; The closest hit proves there is no matching marker below
               ;; FOUND-DIR in the scanned ancestor chain, but it says nothing
               ;; about ancestors above FOUND-DIR.  Let those lookups fall
               ;; through to a direct stat instead of caching a false nil.
               ((string-prefix-p
                 (file-name-as-directory found-dir)
                 (file-name-as-directory candidate-dir))
                nil)
               (t 'not-cached)))
          nil)
      'not-cached)))

(defun tramp-rpc-magit--file-exists-p (filename)
  "Check if FILENAME exists using cached ancestor data.
Returns t, nil, or \\='not-cached if not in cache."
  (let* ((expanded (expand-file-name filename))
         (basename (file-name-nondirectory expanded))
         (file-dir (file-name-as-directory
                    (directory-file-name
                     (or (file-name-directory (tramp-file-local-name expanded))
                         (tramp-file-local-name expanded)))))
         (connection-key (tramp-rpc-magit--file-connection-key expanded))
         (answer 'not-cached))
    (when (member basename tramp-rpc-magit--ancestor-marker-names)
      ;; Reuse any dynamic scan whose root is below this candidate directory;
      ;; ancestor scans cover all parents of their search root.
      (maphash
       (lambda (key entry)
         (when (and (eq answer 'not-cached)
                    (tramp-rpc--cache-entry-valid-p (car entry))
                    (equal (car key) connection-key)
                    (tramp-rpc-magit--ancestor-cache-covers-p
                     (cdr key) file-dir))
           (setq answer
                 (tramp-rpc-magit--file-exists-in-ancestor-scan
                  expanded (cdr entry)))))
       tramp-rpc-magit--ancestor-scan-caches)
      ;; If this is a marker under the prefetched repository, one high-level
      ;; ancestor scan from the queried directory replaces dozens of serial
      ;; file.stat calls as project.el/Projectile walk upward.  Preserve nil as
      ;; a real cached answer, not as "try the next fallback".
      (when (eq answer 'not-cached)
        (tramp-rpc-magit--prune-prefetch-directories)
        (let (covering-prefetch)
          ;; Only select a cache entry here.  The scan below can dispatch
          ;; fs.events and mutate this table, which is unsafe during `maphash'.
          (maphash
           (lambda (prefetch-directory timestamp)
             (when (and (not covering-prefetch)
                        (tramp-rpc--cache-entry-valid-p timestamp)
                        (equal connection-key
                               (tramp-rpc-magit--file-connection-key
                                prefetch-directory))
                        (string-prefix-p
                         (file-name-as-directory
                          (directory-file-name
                           (tramp-file-local-name prefetch-directory)))
                         (tramp-file-local-name expanded)))
               (setq covering-prefetch prefetch-directory)))
           tramp-rpc-magit--prefetch-directories)
          (when covering-prefetch
            (setq answer
                  (tramp-rpc-magit--file-exists-in-ancestor-scan
                   expanded
                   (tramp-rpc-magit--ancestor-scan-for-directory
                    (file-name-directory expanded))))))))
    answer))

;; ============================================================================
;; Cache clearing
;; ============================================================================

(defun tramp-rpc-magit--clear-status-cache-for-connection (vec)
  "Clear status caches belonging to the connection identified by VEC."
  (let ((connection-key (tramp-rpc--connection-key-string vec)))
    (tramp-rpc--hash-remove-if
     (lambda (key _entry)
       (and (consp key) (equal (car key) connection-key)))
     tramp-rpc-magit--process-caches)))

(defun tramp-rpc-magit--clear-cache-for-connection (vec)
  "Clear Magit caches belonging to the connection identified by VEC."
  (tramp-rpc-magit--clear-status-cache-for-connection vec)
  (tramp-rpc-magit--clear-ancestor-caches-for-connection vec))

(defun tramp-rpc-magit--clear-caches-for-directory (directory)
  "Clear Magit and file metadata caches for remote DIRECTORY only."
  (when (file-remote-p directory)
    (with-parsed-tramp-file-name directory nil
      (tramp-rpc-magit--clear-cache-for-connection v)
      (tramp-rpc--clear-file-caches-for-connection v))))

(defun tramp-rpc-magit--clear-cache ()
  "Clear all magit-related caches."
  (clrhash tramp-rpc-magit--process-caches)
  (tramp-rpc-magit--clear-ancestor-caches))

;; ============================================================================
;; Lazy Magit section expansion
;; ============================================================================

(defconst tramp-rpc-magit--lazy-status-section-types
  '(unstaged staged untracked tracked ignored skip-worktree assume-unchanged file)
  "Magit status section types whose bodies may run git lazily on expansion.")

(defun tramp-rpc-magit--section-slot (section slot)
  "Return SECTION's SLOT value, or nil if unavailable."
  (when (and (eieio-object-p section)
             (slot-exists-p section slot)
             (slot-boundp section slot))
    (slot-value section slot)))

(defun tramp-rpc-magit--maybe-prefetch-for-section (section)
  "Ensure batched data exists before expanding lazy Magit SECTION."
  (when (and tramp-rpc-magit-optimize
             (derived-mode-p 'magit-status-mode)
             (file-remote-p default-directory)
             (tramp-rpc-file-name-p default-directory)
             (tramp-rpc-magit--section-slot section 'hidden)
             (memq (tramp-rpc-magit--section-slot section 'type)
                   tramp-rpc-magit--lazy-status-section-types))
    (if (eq (tramp-rpc-magit--section-slot section 'type) 'file)
        (tramp-rpc-magit--prefetch-file-section section)
      (when (null (tramp-rpc-magit--get-process-cache))
        (tramp-rpc-magit--prefetch default-directory)))))

(defun tramp-rpc-magit--section-show-advice (orig section)
  "Advice around `magit-section-show' for lazy remote status sections.
ORIG is the original advised function.
SECTION is the Magit section being handled."
  (let ((tramp-rpc-magit--allow-process-cache t)
        (process-file-side-effects nil))
    (tramp-rpc-magit--maybe-prefetch-for-section section)
    (let ((magit-diff-adjust-tab-width
           (if (and tramp-rpc-magit-disable-remote-diff-tab-width-detection
                    (derived-mode-p 'magit-status-mode)
                    (file-remote-p default-directory)
                    (tramp-rpc-file-name-p default-directory))
               nil
             (and (boundp 'magit-diff-adjust-tab-width)
                  magit-diff-adjust-tab-width))))
      (funcall orig section))))

;; ============================================================================
;; Magit handlers
;; ============================================================================

(defun tramp-rpc-handle-magit-status-setup-buffer (&optional directory)
  "Handler for `magit-status-setup-buffer' to prefetch data.
Suppresses fs.events cache handling during refresh to prevent
inotify events (from git commands touching .git/index etc.) from
clearing caches mid-refresh.
DIRECTORY is the directory being handled."
  (let* ((directory (or directory default-directory))
         (tramp-rpc--suppress-fs-notifications t)
         (tramp-rpc-magit--allow-process-cache t)
         (process-file-side-effects nil)
         (tramp-rpc-magit--status-setup-prefetch-active t)
         (magit-diff-adjust-tab-width
          (if (and tramp-rpc-magit-disable-remote-diff-tab-width-detection
                   (file-remote-p directory)
                   (tramp-rpc-file-name-p directory))
              nil
            (and (boundp 'magit-diff-adjust-tab-width)
                 magit-diff-adjust-tab-width))))
    ;; Drop only this connection's stale Magit and metadata state.  Fresh
    ;; metadata prefetched during the refresh must survive for lazy sections.
    (tramp-rpc-magit--clear-caches-for-directory directory)
    (condition-case err
        (tramp-run-real-handler 'magit-status-setup-buffer (list directory))
      (error
       (tramp-rpc-magit--clear-caches-for-directory directory)
       (signal (car err) (cdr err))))))

(defun tramp-rpc-handle-magit-status-refresh-buffer ()
  "Handler for `magit-status-refresh-buffer' to prefetch data.
Suppresses fs.events cache handling during refresh to prevent
inotify events from clearing caches mid-refresh."
  (unless tramp-rpc-magit--status-setup-prefetch-active
    (tramp-rpc-magit--clear-caches-for-directory default-directory))
  (let ((tramp-rpc--suppress-fs-notifications t)
        (tramp-rpc-magit--allow-process-cache t)
        (process-file-side-effects nil)
        (magit-diff-adjust-tab-width
         (if (and tramp-rpc-magit-disable-remote-diff-tab-width-detection
                  (file-remote-p default-directory)
                  (tramp-rpc-file-name-p default-directory))
             nil
           (and (boundp 'magit-diff-adjust-tab-width)
                magit-diff-adjust-tab-width))))
    ;; The scoped clear above drops stale metadata.  Fresh metadata prefetched
    ;; during the refresh must survive so lazy Magit sections can reuse it.
    (condition-case err
        (tramp-run-real-handler 'magit-status-refresh-buffer nil)
      (error
       (tramp-rpc-magit--clear-caches-for-directory default-directory)
       (signal (car err) (cdr err))))))

;;;###autoload
(defun tramp-rpc-magit-enable ()
  "Enable tramp-rpc magit optimizations.
This uses parallel command prefetching to dramatically speed up
magit-status on remote repositories."
  (interactive)
  ;; Make repeated enable calls idempotent.
  (advice-remove 'magit-section-show #'tramp-rpc-magit--section-show-advice)
  (when (fboundp 'magit-section-show)
    (advice-add 'magit-section-show :around #'tramp-rpc-magit--section-show-advice))

  (tramp-rpc--add-external-operation
   'magit-status-setup-buffer
   #'tramp-rpc-handle-magit-status-setup-buffer 'tramp-rpc)
  (tramp-rpc--add-external-operation
   'magit-status-refresh-buffer
   #'tramp-rpc-handle-magit-status-refresh-buffer 'tramp-rpc)
  (setq tramp-rpc-magit--magit-enabled t)
  (message "tramp-rpc magit optimizations enabled"))

;;;###autoload
(defun tramp-rpc-magit-disable ()
  "Disable tramp-rpc magit optimizations."
  (interactive)
  (advice-remove 'magit-section-show #'tramp-rpc-magit--section-show-advice)
  (tramp-rpc--remove-external-operation 'magit-status-setup-buffer 'tramp-rpc)
  (tramp-rpc--remove-external-operation 'magit-status-refresh-buffer 'tramp-rpc)
  (tramp-rpc-magit--clear-cache)
  (setq tramp-rpc-magit--magit-enabled nil)
  (message "tramp-rpc magit optimizations disabled"))

;;;###autoload
(defun tramp-rpc-magit-enable-debug ()
  "Enable debug logging for tramp-rpc magit."
  (interactive)
  (setq tramp-rpc-magit--debug t)
  (message "tramp-rpc magit debug enabled"))

;;;###autoload
(defun tramp-rpc-magit-disable-debug ()
  "Disable debug logging for tramp-rpc magit."
  (interactive)
  (setq tramp-rpc-magit--debug nil)
  (message "tramp-rpc magit debug disabled"))


;; ============================================================================
;; Projectile optimizations
;; ============================================================================

(defvar projectile-projects-cache)
(defvar projectile-projects-cache-time)
(defvar projectile-git-use-fd)

(defun tramp-rpc-handle-projectile-dir-files (directory)
  "Handler to use alien indexing for remote project files.
`projectile-dir-files-alien' (via `projectile-get-ext-command') checks
fd availability via `executable-find' on the LOCAL machine, but fd may
not be available on the REMOTE.  Binding `projectile-git-use-fd' to nil
forces git ls-files instead.
DIRECTORY is the directory being handled."
  (let ((projectile-git-use-fd nil))
    (projectile-dir-files-alien directory)))

(defun tramp-rpc-handle-projectile-project-files (project-root)
  "Handler to use alien indexing for remote project files.
This bypasses the expensive `file-relative-name' calls in hybrid mode.
PROJECT-ROOT is the project root directory."
  ;; For remote RPC directories, use alien indexing directly
  (let ((files nil))
    ;; Check cache first (like projectile-project-files does)
    (when (and (bound-and-true-p projectile-enable-caching)
               (boundp 'projectile-projects-cache))
      (setq files (gethash project-root projectile-projects-cache)))
    ;; If not cached, fetch and cache
    (unless files
      (setq files (tramp-rpc-handle-projectile-dir-files project-root))
      (when (and (bound-and-true-p projectile-enable-caching)
                 (boundp 'projectile-projects-cache)
                 (boundp 'projectile-projects-cache-time)
                 (fboundp 'projectile-time-seconds))
        (puthash project-root files projectile-projects-cache)
        (puthash project-root (projectile-time-seconds) projectile-projects-cache-time)))
    files))

;;;###autoload
(defun tramp-rpc-projectile-enable ()
  "Enable tramp-rpc projectile optimizations.
This ensures fd is not used for remote directories where it may not
be available, and uses alien indexing for better performance."
  (interactive)
  (tramp-rpc--add-external-operation
   'projectile-dir-files
   #'tramp-rpc-handle-projectile-dir-files 'tramp-rpc)
  (tramp-rpc--add-external-operation
   'projectile-project-files
   #'tramp-rpc-handle-projectile-project-files 'tramp-rpc)
  (setq tramp-rpc-magit--projectile-enabled t)
  (message "tramp-rpc projectile optimizations enabled"))

;;;###autoload
(defun tramp-rpc-projectile-disable ()
  "Disable tramp-rpc projectile optimizations."
  (interactive)
  (tramp-rpc--remove-external-operation 'projectile-dir-files 'tramp-rpc)
  (tramp-rpc--remove-external-operation 'projectile-project-files 'tramp-rpc)
  (setq tramp-rpc-magit--projectile-enabled nil)
  (message "tramp-rpc projectile optimizations disabled"))

(defun tramp-rpc-magit-install-optional-handlers ()
  "Install handlers for loaded Magit and Projectile packages."
  (when (and (featurep 'magit) tramp-rpc-magit-optimize
             (not tramp-rpc-magit--magit-enabled))
    (tramp-rpc-magit-enable))
  (when (and (featurep 'projectile)
             (not tramp-rpc-magit--projectile-enabled))
    (tramp-rpc-projectile-enable)))

;; ============================================================================
;; Unload support
;; ============================================================================

(defun tramp-rpc-magit-unload-function ()
  "Unload function for tramp-rpc-magit.
Removes handlers."
  ;; Remove all handlers.
  (tramp-rpc-magit-disable)
  (tramp-rpc-projectile-disable)
  ;; Return nil to allow normal unload to proceed
  nil)

(provide 'tramp-rpc-magit)
;;; tramp-rpc-magit.el ends here
