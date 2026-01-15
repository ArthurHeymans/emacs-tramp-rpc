#!/bin/bash
# Build tramp-rpc-server for multiple architectures

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
OUTPUT_DIR="$PROJECT_DIR/lisp/binaries"

# Targets to build
TARGETS=(
    "x86_64-unknown-linux-gnu"
    "aarch64-unknown-linux-gnu"
    "x86_64-apple-darwin"
    "aarch64-apple-darwin"
)

# Map target triple to directory name
target_to_dir() {
    case "$1" in
        x86_64-unknown-linux-gnu) echo "x86_64-linux" ;;
        aarch64-unknown-linux-gnu) echo "aarch64-linux" ;;
        x86_64-apple-darwin) echo "x86_64-darwin" ;;
        aarch64-apple-darwin) echo "aarch64-darwin" ;;
        *) echo "$1" ;;
    esac
}

# Check if we have the necessary tools
check_requirements() {
    if ! command -v cargo &> /dev/null; then
        echo "Error: cargo not found. Please install Rust."
        exit 1
    fi

    if ! command -v rustup &> /dev/null; then
        echo "Error: rustup not found. Please install rustup."
        exit 1
    fi
}

# Install target if needed
ensure_target() {
    local target="$1"
    if ! rustup target list --installed | grep -q "$target"; then
        echo "Installing target: $target"
        rustup target add "$target"
    fi
}

# Build for a specific target
build_target() {
    local target="$1"
    local dir_name=$(target_to_dir "$target")
    local output_subdir="$OUTPUT_DIR/$dir_name"

    echo "Building for $target..."

    # Ensure target is installed
    ensure_target "$target"

    # Create output directory
    mkdir -p "$output_subdir"

    # Build
    cd "$PROJECT_DIR"

    # Set linker for cross-compilation if needed
    case "$target" in
        aarch64-unknown-linux-gnu)
            if command -v aarch64-linux-gnu-gcc &> /dev/null; then
                export CARGO_TARGET_AARCH64_UNKNOWN_LINUX_GNU_LINKER=aarch64-linux-gnu-gcc
            fi
            ;;
        x86_64-apple-darwin|aarch64-apple-darwin)
            # Darwin targets typically need special setup
            echo "Note: Building for Darwin requires macOS or cross-compilation setup"
            ;;
    esac

    # Build release
    if cargo build --release --target "$target" 2>/dev/null; then
        # Copy binary
        cp "target/$target/release/tramp-rpc-server" "$output_subdir/"
        echo "  Built: $output_subdir/tramp-rpc-server"

        # Show size
        local size=$(du -h "$output_subdir/tramp-rpc-server" | cut -f1)
        echo "  Size: $size"
    else
        echo "  Warning: Failed to build for $target (may need cross-compilation toolchain)"
    fi
}

# Build for current host
build_native() {
    echo "Building native release..."

    cd "$PROJECT_DIR"
    cargo build --release

    # Determine native target
    local native_target=$(rustc -vV | grep host | cut -d' ' -f2)
    local dir_name=$(target_to_dir "$native_target")
    local output_subdir="$OUTPUT_DIR/$dir_name"

    mkdir -p "$output_subdir"
    cp "target/release/tramp-rpc-server" "$output_subdir/"

    echo "  Built: $output_subdir/tramp-rpc-server"
    local size=$(du -h "$output_subdir/tramp-rpc-server" | cut -f1)
    echo "  Size: $size"
}

# Main
main() {
    check_requirements

    echo "TRAMP-RPC Server Build Script"
    echo "=============================="
    echo ""

    case "${1:-}" in
        --native)
            build_native
            ;;
        --all)
            for target in "${TARGETS[@]}"; do
                build_target "$target"
                echo ""
            done
            ;;
        --target)
            if [ -z "${2:-}" ]; then
                echo "Usage: $0 --target <target>"
                echo "Available targets: ${TARGETS[*]}"
                exit 1
            fi
            build_target "$2"
            ;;
        *)
            echo "Usage: $0 [--native|--all|--target <target>]"
            echo ""
            echo "Options:"
            echo "  --native        Build for the current host only"
            echo "  --all           Build for all supported targets"
            echo "  --target <t>    Build for a specific target"
            echo ""
            echo "Supported targets:"
            for target in "${TARGETS[@]}"; do
                echo "  - $target"
            done
            echo ""
            echo "Example: $0 --native"
            exit 0
            ;;
    esac

    echo ""
    echo "Done!"
}

main "$@"
