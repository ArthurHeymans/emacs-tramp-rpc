#!/usr/bin/env bash
# Verify the package layout expected by NonGNU ELPA.
set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)
workdir=$(mktemp -d)
trap 'rm -rf "$workdir"' EXIT
pkg="$workdir/tramp-rpc"
mkdir -p "$pkg"

cd "$repo"

# The ELPA archive must include the Rust server sources so package installs can
# build tramp-rpc-server without relying on downloaded release binaries.
included=$(tar -cvhf /dev/null --exclude-ignore=.elpaignore --exclude-vcs . 2>&1)
for path in \
	./Cargo.toml \
	./Cargo.lock \
	./.cargo/config.toml \
	./server/Cargo.toml \
	./server/src/main.rs; do
	if ! grep -Fxq "$path" <<<"$included"; then
		echo "ELPA layout check failed: $path would not be included" >&2
		exit 1
	fi
done

for path in ./test/run-tests.sh ./benchmark/benchmark.el ./.github/workflows/ci.yml ./scripts/build-all.sh; do
	if grep -Fxq "$path" <<<"$included"; then
		echo "ELPA layout check failed: $path should not be included" >&2
		exit 1
	fi
done

# Simulate :lisp-dir "lisp", which flattens Lisp files into the package root,
# while preserving the Rust workspace files used for local server builds.
cp lisp/*.el "$pkg"/
cp README.org LICENSE Cargo.toml Cargo.lock "$pkg"/
cp -R .cargo server "$pkg"/

for path in \
	tramp-rpc.el \
	tramp-rpc-deploy.el \
	Cargo.toml \
	Cargo.lock \
	.cargo/config.toml \
	server/Cargo.toml \
	server/src/main.rs; do
	test -e "$pkg/$path"
done

emacs -Q --batch \
	--eval "(require 'package)" \
	--eval "(package-initialize)" \
	--eval "(unless (and (package-installed-p 'tramp) (package-installed-p 'msgpack)) (add-to-list 'package-archives '(\"gnu\" . \"https://elpa.gnu.org/packages/\") t) (add-to-list 'package-archives '(\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\") t) (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) (package-refresh-contents) (setq package-install-upgrade-built-in t) (unless (package-installed-p 'tramp) (package-install 'tramp)) (unless (package-installed-p 'msgpack) (package-install 'msgpack)))"

emacs -Q --batch \
	--eval "(require 'package)" \
	--eval "(add-to-list 'package-archives '(\"gnu\" . \"https://elpa.gnu.org/packages/\") t)" \
	--eval "(add-to-list 'package-archives '(\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\") t)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
	--eval "(package-initialize)" \
	--eval "(setq byte-compile-error-on-warn t)" \
	--eval "(push \"$pkg\" load-path)" \
	-f batch-byte-compile "$pkg"/*.el

emacs -Q --batch -L "$pkg" \
	-l "$pkg/tramp-rpc-deploy.el" \
	--eval "(unless (equal tramp-rpc-deploy-allow-download 'ask) (error \"downloads should default to ask\"))" \
	--eval "(unless (equal (file-truename (tramp-rpc-deploy--source-root)) (file-truename \"$pkg/\")) (error \"wrong source directory: %S\" (tramp-rpc-deploy--source-root)))" \
	--eval "(unless (tramp-rpc-deploy--source-has-server-p) (error \"server sources not found\"))" \
	--eval "(unless (equal (tramp-rpc-deploy--obtain-methods) '(build)) (error \"unexpected noninteractive methods: %S\" (tramp-rpc-deploy--obtain-methods)))"

echo "NonGNU ELPA layout check passed"
