{
  description = "TRAMP RPC server - cross-compilation flake";

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

        # Rust toolchain with all cross-compilation targets
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          targets = [
            "x86_64-unknown-linux-gnu"
            "aarch64-unknown-linux-gnu"
            "x86_64-apple-darwin"
            "aarch64-apple-darwin"
          ];
        };

        # Common build inputs
        commonBuildInputs = with pkgs; [
          rustToolchain
          pkg-config
        ];

        # Build the server for a specific target using cargo
        buildServer =
          {
            target,
            crossPkgs ? null,
          }:
          let
            # Use cross pkgs if provided, otherwise native
            buildPkgs = if crossPkgs != null then crossPkgs else pkgs;
          in
          buildPkgs.rustPlatform.buildRustPackage {
            pname = "tramp-rpc-server";
            version = "0.1.0";
            src = ./.;
            cargoLock.lockFile = ./Cargo.lock;

            buildAndTestSubdir = "server";

            # Skip tests for cross-compilation
            doCheck = crossPkgs == null;

            meta = {
              description = "RPC server for TRAMP remote file access";
              license = pkgs.lib.licenses.gpl3Plus;
            };
          };

        # Cross-compilation packages
        pkgsCrossAarch64Linux = import nixpkgs {
          inherit system overlays;
          crossSystem.config = "aarch64-unknown-linux-gnu";
        };

        pkgsCrossx86_64Linux = import nixpkgs {
          inherit system overlays;
          crossSystem.config = "x86_64-unknown-linux-gnu";
        };

      in
      {
        packages = {
          # Native build
          default = buildServer { target = null; };
          tramp-rpc-server = buildServer { target = null; };

          # Cross-compiled Linux builds
          tramp-rpc-server-x86_64-linux = buildServer {
            target = "x86_64-unknown-linux-gnu";
            crossPkgs = pkgsCrossx86_64Linux;
          };

          tramp-rpc-server-aarch64-linux = buildServer {
            target = "aarch64-unknown-linux-gnu";
            crossPkgs = pkgsCrossAarch64Linux;
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs =
            commonBuildInputs
            ++ (with pkgs; [
              # For aarch64-linux cross-compilation
              pkgsCross.aarch64-multiplatform.stdenv.cc

              # Development tools
              rust-analyzer
              cargo-watch
              cargo-edit
            ]);

          shellHook = ''
            echo "TRAMP-RPC development shell"
            echo ""
            echo "Available targets:"
            echo "  nix build              - Build native"
            echo "  nix build .#tramp-rpc-server-x86_64-linux"
            echo "  nix build .#tramp-rpc-server-aarch64-linux"
            echo ""
            echo "Manual cross-compilation:"
            echo "  cargo build --release --target x86_64-unknown-linux-gnu"
            echo "  cargo build --release --target aarch64-unknown-linux-gnu"
            echo ""
            echo "Environment variables for cross-compilation are set."

            # Set up cross-compilation linkers
            export CARGO_TARGET_AARCH64_UNKNOWN_LINUX_GNU_LINKER="${pkgs.pkgsCross.aarch64-multiplatform.stdenv.cc}/bin/aarch64-unknown-linux-gnu-gcc"
          '';

          RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";
        };

        # App for running the server directly
        apps.default = flake-utils.lib.mkApp {
          drv = self.packages.${system}.default;
          exePath = "/bin/tramp-rpc-server";
        };
      }
    )
    // {
      # Overlays for use in other flakes
      overlays.default = final: prev: {
        tramp-rpc-server = self.packages.${prev.system}.default;
      };
    };
}
