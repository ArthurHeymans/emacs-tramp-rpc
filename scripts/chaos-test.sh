#!/usr/bin/env bash
# Run the randomized process lifecycle tests against a tramp-rpc server.
#
# Usage: scripts/chaos-test.sh [options]
#   --seed N        first seed (default 1)
#   --seeds N       number of seeds (default 20)
#   --split N       percentage of connection filter calls split (default 50)
#   --no-relay      run the server directly instead of behind chaos-relay.py
#   --cpus LIST     pin the server to these CPUs (taskset list, e.g. 0)
#   --load N        run N busy loops on those CPUs while testing
#   --no-build      do not build the server first
#   --server PATH   server binary (default target/release/tramp-rpc-server)
#   -- ARGS...      pass ARGS to Emacs before the test runs (e.g. --eval)
#
# TRAMP_RPC_TEST_HOST selects the host (default localhost); it needs python3
# unless --no-relay is given.  MSGPACK_SOURCE and TRAMP_SOURCE are honored
# like in test/run-tests.sh.  The server and relay are copied to a temporary
# remote directory, so running sessions are not affected.

set -euo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
HOST="${TRAMP_RPC_TEST_HOST:-localhost}"
SEED=1 SEEDS=20 SPLIT=50 RELAY=1 CPUS="" LOAD=0 BUILD=1
SERVER="$PROJECT_DIR/target/release/tramp-rpc-server"

while [[ $# -gt 0 ]]; do
    case "$1" in
        --seed) SEED="$2"; shift 2 ;;
        --seeds) SEEDS="$2"; shift 2 ;;
        --split) SPLIT="$2"; shift 2 ;;
        --no-relay) RELAY=0; shift ;;
        --cpus) CPUS="$2"; shift 2 ;;
        --load) LOAD="$2"; shift 2 ;;
        --no-build) BUILD=0; shift ;;
        --server) SERVER="$2"; shift 2 ;;
        --) shift; break ;;
        *) sed -n '2,19p' "$0"; exit 2 ;;
    esac
done

if [[ "$BUILD" == 1 ]]; then
    cargo build --release --manifest-path "$PROJECT_DIR/Cargo.toml"
fi

LOCAL_DIR="$(mktemp -d /tmp/tramp-rpc-chaos.XXXXXX)"
REMOTE_DIR="$(ssh "$HOST" mktemp -d /tmp/tramp-rpc-chaos.XXXXXX)"
LOADERS=()
cleanup() {
    for pid in "${LOADERS[@]}"; do kill "$pid" 2>/dev/null || true; done
    for socket in "$LOCAL_DIR"/*; do
        if [[ -S "$socket" ]]; then
            ssh -o ControlPath="$socket" -O exit "$HOST" 2>/dev/null || true
        fi
    done
    ssh "$HOST" rm -rf "$REMOTE_DIR" || true
    rm -rf "$LOCAL_DIR"
}
trap cleanup EXIT

PIN=""
if [[ -n "$CPUS" ]]; then PIN="taskset -c $CPUS "; fi
if [[ "$RELAY" == 1 ]]; then
    # A fixed relay seed per run; vary it with --seed to get other byte streams.
    LAUNCH="${PIN}python3 $REMOTE_DIR/chaos-relay.py --seed $SEED $REMOTE_DIR/tramp-rpc-server"
else
    LAUNCH="${PIN}$REMOTE_DIR/tramp-rpc-server"
fi
printf '#!/bin/sh\nexec %s "$@"\n' "$LAUNCH" > "$LOCAL_DIR/server.sh"
chmod +x "$LOCAL_DIR/server.sh"
scp -q "$SERVER" "$PROJECT_DIR/scripts/chaos-relay.py" "$LOCAL_DIR/server.sh" \
    "$HOST:$REMOTE_DIR/"

for _ in $(seq 1 "$LOAD"); do
    ssh "$HOST" "${PIN}sh -c 'while :; do :; done'" &
    LOADERS+=($!)
done

LOAD_PATH=()
if [[ -n "${MSGPACK_SOURCE:-}" ]]; then LOAD_PATH+=(-L "$MSGPACK_SOURCE"); fi
if [[ -n "${TRAMP_SOURCE:-}" ]]; then LOAD_PATH+=(-L "$TRAMP_SOURCE/lisp"); fi

export TRAMP_RPC_TEST_HOST="$HOST" TRAMP_RPC_CHAOS_SEED="$SEED" \
       TRAMP_RPC_CHAOS_SEEDS="$SEEDS" TRAMP_RPC_CHAOS_SPLIT="$SPLIT"
set +e
# Interactive Emacs ignores SIGPIPE; batch Emacs would die when it writes
# to a transport the test just killed.
trap '' PIPE
emacs -Q --batch "${LOAD_PATH[@]}" \
    --eval "(require 'package)" --eval "(package-initialize)" \
    -l "$PROJECT_DIR/test/tramp-rpc-chaos-tests.el" \
    --eval "(setq tramp-rpc-deploy-never-deploy t
                  tramp-rpc-deploy-remote-binary-path \"$REMOTE_DIR/server.sh\"
                  tramp-rpc-controlmaster-path \"$LOCAL_DIR/%C\")" \
    "$@" \
    --eval '(tramp-rpc-test--run-guarded "^tramp-rpc-chaos-test")' \
    2>&1 | tee "$LOCAL_DIR/log"
status=${PIPESTATUS[0]}
set -e

# Errors escaping tramp-rpc's own filters and sentinels are only printed.
if grep -E 'error in process (filter|sentinel)' "$LOCAL_DIR/log"; then
    echo "chaos-test: errors escaped process filters or sentinels" >&2
    status=1
fi
exit "$status"
