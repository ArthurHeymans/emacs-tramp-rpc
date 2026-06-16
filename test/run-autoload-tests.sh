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

# Install messagepack if needed, then run tests
emacs -Q --batch \
    -L "$PROJECT_ROOT/lisp" \
    -L "$PROJECT_ROOT/../emacs-messagepack" \
    --eval "(require 'package)" \
    --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
    --eval "(package-initialize)" \
    --eval "(unless (require 'messagepack nil t)
              (package-refresh-contents)
              (package-vc-install '(messagepack :url \"https://github.com/ArthurHeymans/emacs-messagepack\") \"91deebe5\"))" \
    -l "$SCRIPT_DIR/tramp-rpc-autoload-tests.el" \
    -f ert-run-tests-batch-and-exit

echo
echo "All autoload tests passed!"
