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
#   --all       Run all tests
#   --help      Show this help

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

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
    echo "  --all       Run all tests"
    echo "  --help      Show this help"
    echo ""
    echo "Environment variables:"
    echo "  TRAMP_RPC_TEST_HOST   Remote host for testing (default: localhost)"
    echo "  TRAMP_RPC_TEST_USER   User for remote testing"
    echo "  EMACS                 Emacs executable (default: emacs)"
}

run_protocol_tests() {
    echo -e "${YELLOW}Running protocol tests...${NC}"
    ${EMACS:-emacs} -Q --batch \
        -l "$SCRIPT_DIR/tramp-rpc-mock-tests.el" \
        --eval "(ert-run-tests-batch-and-exit '(and (tag tramp-rpc-mock-test) (not (tag :server))))"
}

run_server_tests() {
    echo -e "${YELLOW}Running server tests...${NC}"
    # Check if server is available
    if [[ -x "$PROJECT_DIR/target/release/tramp-rpc-server" ]] || \
       [[ -x "$PROJECT_DIR/target/debug/tramp-rpc-server" ]] || \
       [[ -f "$PROJECT_DIR/server/tramp-rpc-server.py" ]]; then
        ${EMACS:-emacs} -Q --batch \
            -l "$SCRIPT_DIR/tramp-rpc-mock-tests.el" \
            --eval "(ert-run-tests-batch-and-exit '(tag :server))"
    else
        echo -e "${RED}No server found. Build with 'cargo build' or use Python server.${NC}"
        exit 1
    fi
}

run_mock_tests() {
    echo -e "${YELLOW}Running all mock tests...${NC}"
    ${EMACS:-emacs} -Q --batch \
        -l "$SCRIPT_DIR/tramp-rpc-mock-tests.el" \
        --eval "(ert-run-tests-batch-and-exit \"^tramp-rpc-mock-test\")"
}

run_remote_tests() {
    echo -e "${YELLOW}Running remote tests against ${TRAMP_RPC_TEST_HOST:-localhost}...${NC}"
    ${EMACS:-emacs} -Q --batch \
        -l "$SCRIPT_DIR/tramp-rpc-tests.el" \
        --eval "(ert-run-tests-batch-and-exit \"^tramp-rpc-test\")"
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
