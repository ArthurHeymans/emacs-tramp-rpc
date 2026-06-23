#!/bin/bash
# Run autoload tests for tramp-rpc
#
# These tests verify the autoload mechanism works correctly.
# They must run in a fresh Emacs without tramp-rpc pre-loaded.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
TRAMP_SOURCE_CONFIGURED=0
if [[ -n "${TRAMP_SOURCE:-}" ]]; then
    TRAMP_SOURCE_CONFIGURED=1
fi
TRAMP_SOURCE="${TRAMP_SOURCE:-$HOME/src/tramp}"

EMACS_LOAD_PATH_ARGS=(-L "$PROJECT_ROOT/lisp")
if [[ -d "$TRAMP_SOURCE/lisp" ]]; then
    # The sudo autoload tests exercise TRAMP >= 2.8 hidden ad-hoc proxy
    # behavior.  Prefer the configured TRAMP checkout over the bundled Emacs
    # TRAMP, which can be too old for this project.
    EMACS_LOAD_PATH_ARGS=(-L "$TRAMP_SOURCE/lisp" "${EMACS_LOAD_PATH_ARGS[@]}")
elif (( TRAMP_SOURCE_CONFIGURED )); then
    echo "TRAMP_SOURCE does not contain a lisp/ directory: $TRAMP_SOURCE" >&2
    exit 1
fi

echo "Running tramp-rpc autoload tests..."
echo "Project root: $PROJECT_ROOT"
echo "TRAMP source: $TRAMP_SOURCE"
echo

# Install msgpack if needed, then run tests
emacs -Q --batch \
    "${EMACS_LOAD_PATH_ARGS[@]}" \
    --eval "(require 'package)" \
    --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
    --eval "(package-initialize)" \
    --eval "(unless (require 'msgpack nil t)
              (package-refresh-contents)
              (package-install 'msgpack))" \
    -l "$SCRIPT_DIR/tramp-rpc-autoload-tests.el" \
    -f ert-run-tests-batch-and-exit

echo
echo "All autoload tests passed!"
