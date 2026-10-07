#!/usr/bin/env bash
# Real Majutsu integration tests over SSH/RPC in disposable workspaces.
# Required: MAJUTSU_SOURCE, MAGIT_SOURCE, TRAMP_RPC_TEST_SERVER (a matching
# server copied to a temporary remote path). Optional: TRAMP_SOURCE,
# MSGPACK_SOURCE, MAJUTSU_DEPENDENCY_DIRS (colon-separated),
# TRAMP_RPC_TEST_HOST (localhost), TRAMP_RPC_MAJUTSU_TEST_SELECTOR.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
: "${MAJUTSU_SOURCE:?Set MAJUTSU_SOURCE to the Majutsu checkout}"
: "${MAGIT_SOURCE:?Set MAGIT_SOURCE to the Magit checkout}"
: "${TRAMP_RPC_TEST_SERVER:?Copy the matching server to a temporary remote path}"
export TRAMP_RPC_TEST_SERVER TRAMP_RPC_TEST_HOST="${TRAMP_RPC_TEST_HOST:-localhost}"
args=(-L "$ROOT/lisp" -L "$MAJUTSU_SOURCE" -L "$MAGIT_SOURCE/lisp")
if [[ -n "${TRAMP_SOURCE:-}" ]]; then args+=(-L "$TRAMP_SOURCE/lisp"); fi
if [[ -n "${MSGPACK_SOURCE:-}" ]]; then args+=(-L "$MSGPACK_SOURCE"); fi
if [[ -n "${MAJUTSU_DEPENDENCY_DIRS:-}" ]]; then
    IFS=: read -ra deps <<< "$MAJUTSU_DEPENDENCY_DIRS"
    for dep in "${deps[@]}"; do
        if [[ -n "$dep" ]]; then args+=(-L "$dep"); fi
    done
fi
"${EMACS:-emacs}" -Q --batch "${args[@]}" \
    --eval '(setq load-prefer-newer t)' \
    -l "$ROOT/test/tramp-rpc-majutsu-tests.el" \
    --eval '(let* ((selector (or (getenv "TRAMP_RPC_MAJUTSU_TEST_SELECTOR")
                                "^tramp-rpc-majutsu-test-"))
                   (tests (ert-select-tests selector t)))
              (unless tests (error "No Majutsu tests selected"))
              (let ((stats (ert-run-tests-batch selector)))
                (kill-emacs (if (and (zerop (ert-stats-completed-unexpected stats))
                                     (zerop (ert-stats-skipped stats))) 0 1))))'
