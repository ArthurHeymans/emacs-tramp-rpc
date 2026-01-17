#!/bin/bash
# Build tramp-rpc-server with size-optimized static musl linking

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
OUTPUT_DIR="$PROJECT_DIR/lisp/binaries"

# Supported targets
TARGETS=(
    "x86_64-unknown-linux-musl"
    "aarch64-unknown-linux-musl"
    "x86_64-apple-darwin"
    "aarch64-apple-darwin"
)

# Map target triple to directory name
target_to_dir() {
    case "$1" in
        x86_64-unknown-linux-musl) echo "x86_64-linux" ;;
        aarch64-unknown-linux-musl) echo "aarch64-linux" ;;
        x86_64-apple-darwin) echo "x86_64-darwin" ;;
        aarch64-apple-darwin) echo "aarch64-darwin" ;;
        *) echo "$1" ;;
    esac
}

# Build size-optimized static musl binary (requires nightly + rust-src)
build() {
    local target="${1:-x86_64-unknown-linux-musl}"
    local dir_name="$(target_to_dir "$target")"
    local output_subdir="$OUTPUT_DIR/$dir_name"

    echo "Building size-optimized static binary for $target..."

    # Determine if we're using rustup-managed cargo or standalone (nix)
    local cargo_cmd="cargo"

    # Check if current cargo is nightly
    if cargo --version 2>&1 | grep -q nightly || rustc --version 2>&1 | grep -q nightly; then
        echo "  Using nightly toolchain"
        cargo_cmd="cargo"
    elif command -v rustup &> /dev/null; then
        echo "  Using rustup toolchain management"

        # Check for nightly
        if ! rustup run nightly rustc --version &> /dev/null; then
            echo "Error: nightly toolchain not installed. Run: rustup toolchain install nightly"
            exit 1
        fi

        # Check for rust-src
        if ! rustup +nightly component list --installed | grep -q rust-src; then
            echo "Installing rust-src component..."
            rustup +nightly component add rust-src
        fi

        # Ensure target is installed for nightly
        if ! rustup +nightly target list --installed | grep -q "$target"; then
            echo "Installing target $target for nightly..."
            rustup +nightly target add "$target"
        fi

        cargo_cmd="cargo +nightly"
    else
        echo "Error: nightly toolchain required for build-std"
        echo "  Use: nix develop  or  rustup toolchain install nightly"
        exit 1
    fi

    cd "$PROJECT_DIR"
    mkdir -p "$output_subdir"

    # Set up linker and RUSTFLAGS based on target
    local rustflags="-Zlocation-detail=none -Zunstable-options -Cpanic=immediate-abort"
    
    case "$target" in
        x86_64-unknown-linux-musl)
            rustflags="-C target-feature=+crt-static $rustflags"
            if command -v x86_64-unknown-linux-musl-gcc &> /dev/null; then
                export CARGO_TARGET_X86_64_UNKNOWN_LINUX_MUSL_LINKER=x86_64-unknown-linux-musl-gcc
            elif command -v musl-gcc &> /dev/null; then
                export CARGO_TARGET_X86_64_UNKNOWN_LINUX_MUSL_LINKER=musl-gcc
            fi
            ;;
        aarch64-unknown-linux-musl)
            rustflags="-C target-feature=+crt-static $rustflags"
            if command -v aarch64-unknown-linux-musl-gcc &> /dev/null; then
                export CARGO_TARGET_AARCH64_UNKNOWN_LINUX_MUSL_LINKER=aarch64-unknown-linux-musl-gcc
            elif command -v aarch64-linux-musl-gcc &> /dev/null; then
                export CARGO_TARGET_AARCH64_UNKNOWN_LINUX_MUSL_LINKER=aarch64-linux-musl-gcc
            fi
            ;;
        *-apple-darwin)
            # Darwin doesn't use musl, but still benefits from build-std
            ;;
    esac

    # Build with build-std for maximum size optimization
    if RUSTFLAGS="$rustflags" \
       $cargo_cmd build --release --target "$target" \
         -Z build-std=std,panic_abort \
         -Z build-std-features="optimize_for_size"; then

        cp "target/$target/release/tramp-rpc-server" "$output_subdir/"
        echo "  Built: $output_subdir/tramp-rpc-server"

        local size=$(du -h "$output_subdir/tramp-rpc-server" | cut -f1)
        
        case "$target" in
            *-linux-musl)
                echo "  Size: $size (static musl, size-optimized)"
                # Verify it's static
                if ldd "$output_subdir/tramp-rpc-server" 2>&1 | grep -q "statically linked"; then
                    echo "  Status: Fully statically linked"
                fi
                ;;
            *-apple-darwin)
                echo "  Size: $size (size-optimized)"
                ;;
        esac
    else
        echo "  Error: Build failed"
        echo "  For rustup: rustup +nightly component add rust-src"
        echo "  For nix: nix develop"
        exit 1
    fi
}

# Main
main() {
    echo "TRAMP-RPC Server Build Script"
    echo "=============================="
    echo ""

    case "${1:-}" in
        --all)
            for target in "${TARGETS[@]}"; do
                build "$target"
                echo ""
            done
            ;;
        --help|-h)
            echo "Usage: $0 [target]"
            echo ""
            echo "Builds size-optimized binaries using nightly + build-std."
            echo "Linux targets use static musl linking."
            echo ""
            echo "Arguments:"
            echo "  (none)                       Build for x86_64-unknown-linux-musl"
            echo "  x86_64-unknown-linux-musl    Build for x86_64 Linux (static)"
            echo "  aarch64-unknown-linux-musl   Build for aarch64 Linux (static)"
            echo "  x86_64-apple-darwin          Build for x86_64 macOS"
            echo "  aarch64-apple-darwin         Build for aarch64 macOS (Apple Silicon)"
            echo "  --all                        Build for all targets"
            echo ""
            echo "Requirements:"
            echo "  - nix develop (recommended), or"
            echo "  - rustup with nightly + rust-src"
            exit 0
            ;;
        *)
            build "${1:-x86_64-unknown-linux-musl}"
            ;;
    esac

    echo ""
    echo "Done!"
}

main "$@"
