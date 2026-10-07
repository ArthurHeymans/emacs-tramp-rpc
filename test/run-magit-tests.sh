#!/usr/bin/env bash
# Real Magit end-to-end tests over SSH/RPC, using disposable repositories.
# Required: MAGIT_SOURCE, TRAMP_RPC_TEST_SERVER (a matching, separately copied
# remote binary). Optional: TRAMP_SOURCE, MSGPACK_SOURCE,
# MAGIT_DEPENDENCY_DIRS (colon-separated), TRAMP_RPC_TEST_HOST (localhost),
# TRAMP_RPC_MAGIT_TEST_SELECTOR (ERT regexp; defaults to the complete suite).
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
: "${MAGIT_SOURCE:?Set MAGIT_SOURCE to the Magit checkout}"
: "${TRAMP_RPC_TEST_SERVER:?Copy the matching server to a temporary remote path and set TRAMP_RPC_TEST_SERVER}"
export TRAMP_RPC_TEST_SERVER TRAMP_RPC_TEST_HOST="${TRAMP_RPC_TEST_HOST:-localhost}"
args=(-L "$ROOT/lisp" -L "$MAGIT_SOURCE/lisp")
if [[ -n "${TRAMP_SOURCE:-}" ]]; then args+=(-L "$TRAMP_SOURCE/lisp"); fi
if [[ -n "${MSGPACK_SOURCE:-}" ]]; then args+=(-L "$MSGPACK_SOURCE"); fi
if [[ -n "${MAGIT_DEPENDENCY_DIRS:-}" ]]; then
    IFS=: read -ra deps <<< "$MAGIT_DEPENDENCY_DIRS"
    for dep in "${deps[@]}"; do
        if [[ -n "$dep" ]]; then args+=(-L "$dep"); fi
    done
fi
"${EMACS:-emacs}" -Q --batch "${args[@]}" \
    --eval '(setq tramp-rpc-deploy-never-deploy t
                  tramp-rpc-deploy-remote-binary-path (getenv "TRAMP_RPC_TEST_SERVER")
                  tramp-rpc-deploy-remote-directory
                  (file-name-directory (getenv "TRAMP_RPC_TEST_SERVER")))' \
    -l "$ROOT/test/tramp-rpc-magit-tests.el" \
    --eval '(let* ((selector (or (getenv "TRAMP_RPC_MAGIT_TEST_SELECTOR")
                                "^tramp-rpc-magit-test-"))
                   (tests (ert-select-tests selector t)))
              (unless tests (error "No Magit tests selected"))
              (message "Real Magit source: %s; TRAMP %s; remote: %s"
                       (symbol-file (quote magit-status-setup-buffer))
                       tramp-version (getenv "TRAMP_RPC_TEST_HOST"))
              (let ((stats (ert-run-tests-batch selector)))
                (kill-emacs (if (and (zerop (ert-stats-completed-unexpected stats))
                                     (zerop (ert-stats-skipped stats))) 0 1))))'
