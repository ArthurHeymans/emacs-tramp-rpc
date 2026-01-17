{
  description = "TRAMP RPC server";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      rust-overlay,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };

        # Nightly toolchain with rust-src for build-std (size-optimized builds)
        rustToolchain = pkgs.rust-bin.nightly.latest.default.override {
          targets = [
            "x86_64-unknown-linux-musl"
            "aarch64-unknown-linux-musl"
            "x86_64-apple-darwin"
            "aarch64-apple-darwin"
          ];
          extensions = [ "rust-src" ];
        };

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            rustToolchain
            pkgs.pkg-config
            pkgs.pkgsCross.musl64.stdenv.cc
            pkgs.pkgsCross.aarch64-multiplatform-musl.stdenv.cc
            pkgs.rust-analyzer
          ];

          shellHook = ''
            echo "TRAMP-RPC development shell (nightly + build-std)"
            echo ""
            echo "Build:"
            echo "  ./scripts/build-all.sh                         # x86_64 Linux (static musl)"
            echo "  ./scripts/build-all.sh aarch64-unknown-linux-musl"
            echo "  ./scripts/build-all.sh x86_64-apple-darwin"
            echo "  ./scripts/build-all.sh --all"

            export CARGO_TARGET_X86_64_UNKNOWN_LINUX_MUSL_LINKER="${pkgs.pkgsCross.musl64.stdenv.cc}/bin/x86_64-unknown-linux-musl-gcc"
            export CARGO_TARGET_AARCH64_UNKNOWN_LINUX_MUSL_LINKER="${pkgs.pkgsCross.aarch64-multiplatform-musl.stdenv.cc}/bin/aarch64-unknown-linux-musl-gcc"
          '';
        };
      }
    );
}
