#!/bin/bash
# Test runner for tramp-rpc
# Usage:
#   ./test/run-tests.sh [OPTIONS]
#
# Options:
#   --mock      Run mock tests only (no SSH required)
#   --protocol  Run protocol tests only (no server required)
#   --server    Run server tests (requires built server)
#   --remote    Run remote tests (requires SSH to TRAMP_RPC_TEST_HOST)
#   --upstream  Run upstream tests (requires SSH to TRAMP_RPC_TEST_HOST)
#   --all       Run protocol and server tests (requires SSH to TRAMP_RPC_TEST_HOST)
#   --help      Show this help

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
MIN_TRAMP_VERSION="$(<"$SCRIPT_DIR/min-tramp-version")"

# Local shell configuration is optional; exported environment variables win.
TRAMP_SOURCE_ENV="${TRAMP_SOURCE-}"
TRAMP_SOURCE_ENV_SET="${TRAMP_SOURCE+x}"
TRAMP_TEST_SOURCE_ENV="${TRAMP_TEST_SOURCE-}"
TRAMP_TEST_SOURCE_ENV_SET="${TRAMP_TEST_SOURCE+x}"
TRAMP_RPC_TEST_HOST_ENV="${TRAMP_RPC_TEST_HOST-}"
TRAMP_RPC_TEST_HOST_ENV_SET="${TRAMP_RPC_TEST_HOST+x}"
if [[ -f "$PROJECT_DIR/.config.local" ]]; then
    source "$PROJECT_DIR/.config.local"
fi
if [[ -n "$TRAMP_SOURCE_ENV_SET" ]]; then TRAMP_SOURCE="$TRAMP_SOURCE_ENV"; fi
if [[ -n "$TRAMP_TEST_SOURCE_ENV_SET" ]]; then TRAMP_TEST_SOURCE="$TRAMP_TEST_SOURCE_ENV"; fi
if [[ -n "$TRAMP_RPC_TEST_HOST_ENV_SET" ]]; then TRAMP_RPC_TEST_HOST="$TRAMP_RPC_TEST_HOST_ENV"; fi

expand_home() {
    case "$1" in
        "~") printf '%s' "$HOME" ;;
        "~/"*) printf '%s/%s' "$HOME" "${1#\~/}" ;;
        *) printf '%s' "$1" ;;
    esac
}

TRAMP_SOURCE="$(expand_home "${TRAMP_SOURCE:-}")"
TRAMP_TEST_SOURCE="$(expand_home "${TRAMP_TEST_SOURCE:-}")"
export TRAMP_SOURCE TRAMP_TEST_SOURCE
if [[ -n "${TRAMP_RPC_TEST_HOST+x}" ]]; then export TRAMP_RPC_TEST_HOST; fi
EMACS_LOAD_PATH_ARGS=()

if [[ -n "$TRAMP_SOURCE" && -d "$TRAMP_SOURCE/lisp" ]]; then
    EMACS_LOAD_PATH_ARGS=(-L "$TRAMP_SOURCE/lisp")
elif [[ -n "$TRAMP_SOURCE" ]]; then
    echo "TRAMP_SOURCE does not contain a lisp/ directory: $TRAMP_SOURCE" >&2
    exit 1
fi

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --mock      Run mock tests only (no SSH required)"
    echo "  --protocol  Run protocol tests only (no server required)"
    echo "  --server    Run server tests (requires built server)"
    echo "  --remote    Run remote tests (requires SSH to TRAMP_RPC_TEST_HOST)"
    echo "  --upstream  Run upstream tests (requires SSH to TRAMP_RPC_TEST_HOST)"
    echo "  --all       Run protocol and server tests (requires SSH to TRAMP_RPC_TEST_HOST)"
    echo "  --help      Show this help"
    echo ""
    echo "Environment variables:"
    echo "  TRAMP_SOURCE          Supported TRAMP source tree (optional; bundled TRAMP otherwise)"
    echo "  TRAMP_TEST_SOURCE     TRAMP source tree for upstream tests (default: TRAMP_SOURCE)"
    echo "  TRAMP_RPC_TEST_HOST   Remote host for testing (default: localhost)"
    echo "  TRAMP_RPC_TEST_USER   User for remote testing"
    echo "  EMACS                 Emacs executable (default: emacs)"
    echo ""
    echo "Local config:"
    echo "  $PROJECT_DIR/.config.local (optional shell syntax; environment variables take precedence)"
}

require_supported_tramp() {
    ${EMACS:-emacs} -Q --batch "$@" \
        --eval "(progn (require 'tramp) (unless (version<= \"$MIN_TRAMP_VERSION\" tramp-version) (error \"tramp-rpc tests require Tramp >= $MIN_TRAMP_VERSION, but %s is loaded; set TRAMP_SOURCE to a supported checkout\" tramp-version)))"
}

run_ert_selector() {
    local test_file="$1"
    local selector="$2"
    local expected_count="${3:-nil}"
    shift 3

    ${EMACS:-emacs} -Q --batch "$@" \
        -l "$test_file" \
        --eval "(let ((tramp-rpc-mock-test--isolate-tests t)) (let* ((selector $selector) (tests (ert-select-tests selector t)) (selected (length tests))) (unless (> selected 0) (error \"ERT selector %S selected zero tests\" selector)) (when $expected_count (unless (= selected $expected_count) (error \"ERT selector %S selected %d tests, expected $expected_count\" selector selected))) (let* ((stats (ert-run-tests-batch selector)) (skipped (ert-stats-skipped stats)) (executed (- (ert-stats-completed stats) skipped))) (message \"ERT counts: selected=%d executed=%d skipped=%d\" selected executed skipped) (when (= executed 0) (error \"ERT selector %S executed zero tests (all %d selected tests skipped)\" selector skipped)) (kill-emacs (if (> (ert-stats-completed-unexpected stats) 0) 1 0)))))"
}

run_mock_selector() {
    local selector="$1"
    local expected_count="${2:-nil}"

    require_supported_tramp "${EMACS_LOAD_PATH_ARGS[@]}"
    run_ert_selector "$SCRIPT_DIR/tramp-rpc-mock-tests.el" "\"$selector\"" "$expected_count" \
        "${EMACS_LOAD_PATH_ARGS[@]}"
}

run_protocol_tests() {
    echo -e "${YELLOW}Running protocol tests...${NC}"
    run_mock_selector "^tramp-rpc-mock-test-protocol" 8
}

server_available() {
    [[ -x "$PROJECT_DIR/target/release/tramp-rpc-server" ]] || \
        [[ -x "$PROJECT_DIR/server/target/release/tramp-rpc-server" ]] || \
        [[ -x "$PROJECT_DIR/target/x86_64-unknown-linux-musl/release/tramp-rpc-server" ]] || \
        [[ -x "$PROJECT_DIR/target/debug/tramp-rpc-server" ]] || \
        [[ -x "$PROJECT_DIR/server/target/debug/tramp-rpc-server" ]]
}

run_server_tests() {
    echo -e "${YELLOW}Running server tests...${NC}"
    if server_available; then
        require_supported_tramp "${EMACS_LOAD_PATH_ARGS[@]}"
        run_ert_selector "$SCRIPT_DIR/tramp-rpc-mock-tests.el" "'(tag :server)" nil \
            "${EMACS_LOAD_PATH_ARGS[@]}"
    else
        echo -e "${RED}No server found. Build with 'cargo build'.${NC}"
        exit 1
    fi
}

run_mock_tests() {
    echo -e "${YELLOW}Running all mock tests...${NC}"
    run_mock_selector "^tramp-rpc-mock-test"
}

run_remote_tests() {
    echo -e "${YELLOW}Running remote tests against ${TRAMP_RPC_TEST_HOST:-localhost}...${NC}"
    require_supported_tramp "${EMACS_LOAD_PATH_ARGS[@]}"
    run_ert_selector "$SCRIPT_DIR/tramp-rpc-tests.el" "\"^tramp-rpc-test\"" nil \
        "${EMACS_LOAD_PATH_ARGS[@]}"
}

run_upstream_tests() {
    local upstream_tramp_source="${TRAMP_TEST_SOURCE:-$TRAMP_SOURCE}"
    local upstream_load_path_args=()

    echo -e "${YELLOW}Running upstream tests against ${TRAMP_RPC_TEST_HOST:-localhost}...${NC}"
    if [[ -z "$upstream_tramp_source" ]]; then
        echo "Upstream tests require TRAMP_TEST_SOURCE or TRAMP_SOURCE" >&2
        exit 1
    elif [[ -d "$upstream_tramp_source/lisp" ]]; then
        upstream_load_path_args=(-L "$upstream_tramp_source/lisp")
    else
        echo "TRAMP_TEST_SOURCE does not contain a lisp/ directory: $upstream_tramp_source" >&2
        exit 1
    fi
    require_supported_tramp "${upstream_load_path_args[@]}"
    export TRAMP_TEST_SOURCE="$upstream_tramp_source"
    run_ert_selector "$SCRIPT_DIR/run-tramp-tests.el" "'(not (tag :unstable))" nil \
        "${upstream_load_path_args[@]}"
}

run_all_tests() {
    echo -e "${YELLOW}Running all tests...${NC}"
    local failed=0

    echo ""
    echo "=== Protocol Tests ==="
    if run_protocol_tests; then
        echo -e "${GREEN}Protocol tests passed${NC}"
    else
        echo -e "${RED}Protocol tests failed${NC}"
        failed=1
    fi

    echo ""
    echo "=== Server Tests ==="
    if run_server_tests; then
        echo -e "${GREEN}Server tests passed${NC}"
    else
        echo -e "${RED}Server tests failed${NC}"
        failed=1
    fi

    return $failed
}

# Parse arguments
if [[ $# -eq 0 ]]; then
    # Default: run mock tests
    run_mock_tests
    exit $?
fi

case "$1" in
    --mock)
        run_mock_tests
        ;;
    --protocol)
        run_protocol_tests
        ;;
    --server)
        run_server_tests
        ;;
    --remote)
        run_remote_tests
        ;;
    --upstream)
        run_upstream_tests
        ;;
    --all)
        run_all_tests
        ;;
    --help|-h)
        usage
        exit 0
        ;;
    *)
        echo "Unknown option: $1"
        usage
        exit 1
        ;;
esac
