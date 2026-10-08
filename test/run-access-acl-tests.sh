#!/usr/bin/env bash
# Linux-only, real non-owner ACL checks without host-root privileges.
# Requires subordinate UID/GID mappings, unshare/newuidmap/newgidmap,
# setpriv and setfacl.  Missing prerequisites fail rather than skip tests.
# TRAMP_SOURCE and MSGPACK_SOURCE may supply dependency load paths.
# TRAMP_RPC_ACCESS_TEST_SERVER must name the server to test; it is copied.
set -euo pipefail

project_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
: "${TRAMP_RPC_ACCESS_TEST_SERVER:?Set this to the rebuilt server binary}"
for command in unshare newuidmap newgidmap setpriv setfacl "${EMACS:-emacs}"; do
    command -v "$command" >/dev/null
done

sandbox=$(mktemp -d /tmp/tramp-rpc-access-acl.XXXXXXXX)
trap 'rm -rf "$sandbox"' EXIT
cp "$TRAMP_RPC_ACCESS_TEST_SERVER" "$sandbox/tramp-rpc-server"
export TRAMP_RPC_ACCESS_TEST_SERVER="$sandbox/tramp-rpc-server"
export TRAMP_RPC_ACCESS_TEST_ACL=1

load_path=(-L "$project_dir/lisp")
if [[ -n "${TRAMP_SOURCE:-}" ]]; then load_path+=(-L "$TRAMP_SOURCE/lisp"); fi
if [[ -n "${MSGPACK_SOURCE:-}" ]]; then load_path+=(-L "$MSGPACK_SOURCE"); fi

# Map namespace UID/GID 0 to the caller, and UID/GID 1 to subordinate IDs.
# Keep CHOWN/FOWNER for fixture setup, but remove both DAC bypass capabilities
# from the bounding set so neither Emacs nor an exec'ed server can regain them.
unshare --map-auto --map-root-user \
    setpriv --bounding-set=-dac_override,-dac_read_search \
    "${EMACS:-emacs}" -Q --batch "${load_path[@]}" \
    -l "$project_dir/test/run-access-tests.el"
