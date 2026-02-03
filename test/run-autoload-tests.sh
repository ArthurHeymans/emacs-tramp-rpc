#!/bin/bash
# Run autoload tests for tramp-rpc
#
# These tests verify the autoload mechanism works correctly.
# They must run in a fresh Emacs without tramp-rpc pre-loaded.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "Running tramp-rpc autoload tests..."
echo "Project root: $PROJECT_ROOT"
echo

# Install msgpack if needed, then run tests
emacs -Q --batch \
    -L "$PROJECT_ROOT/lisp" \
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
