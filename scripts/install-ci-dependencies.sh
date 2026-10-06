#!/usr/bin/env bash
# Install pinned dependency sources for the Emacs version selected by CI.
set -euo pipefail

dependencies="$(nix build --no-link --print-out-paths .#ci-elisp-dependencies)"
elpa="$HOME/.emacs.d/elpa"
mkdir -p "$elpa"
cp -RL "$dependencies/share/emacs/site-lisp/elpa/." "$elpa/"
chmod -R u+w "$elpa"
# Nix may compile with a different Emacs.  Let the matrix's Emacs load source.
find "$elpa" -type f \( -name '*.elc' -o -name '*.eln' \) -delete

emacs -Q --batch \
  --eval "(require 'package)" \
  --eval "(package-initialize)" \
  --eval "(dolist (name '(tramp msgpack package-lint)) (unless (assq name package-alist) (error \"Missing pinned dependency: %s\" name)))" \
  --eval "(require 'trampver)" \
  --eval "(unless (version<= \"2.8.1.4\" tramp-version) (error \"TRAMP dependency too old: %s\" tramp-version))"
