{
  description = "TRAMP RPC server";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      rust-overlay,
    }:
    let
      inherit (nixpkgs) lib;
      forAllSystems = lib.genAttrs lib.systems.flakeExposed;

      rustTargets = [
        "x86_64-unknown-linux-musl"
        "aarch64-unknown-linux-musl"
        "x86_64-apple-darwin"
        "aarch64-apple-darwin"
        "i686-unknown-linux-musl"
        "armv7-unknown-linux-musleabihf"
        "armv5te-unknown-linux-musleabi"
        "arm-unknown-linux-musleabihf"
      ];
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          self' = self.packages.${system};
        in
        {
          tramp-rpc-server = pkgs.pkgsStatic.callPackage ./default.nix { };
          default = self'.tramp-rpc-server;
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          # Cross-compilation toolchains for extra Linux targets.  Keep these
          # lazy so CI can enter a target-specific shell without realizing all
          # cross compilers.
          pkgsCrossI686Musl = pkgs.pkgsCross.musl32;
          pkgsCrossArmv5teMusl = import nixpkgs {
            inherit system;
            crossSystem = lib.systems.elaborate {
              config = "armv5tel-unknown-linux-musleabi";
              libc = "musl";
            };
          };
          # ARMv7 hard-float: Allwinner A20/H3 (Cortex-A7), Raspberry Pi 2+
          pkgsCrossArmv7Musl = import nixpkgs {
            inherit system;
            crossSystem = lib.systems.elaborate {
              config = "armv7l-unknown-linux-musleabihf";
            };
          };
          # ARMv6 hard-float: original Raspberry Pi (ARM1176JZF-S)
          pkgsCrossArmMusl = import nixpkgs {
            inherit system;
            crossSystem = lib.systems.elaborate {
              config = "armv6l-unknown-linux-musleabihf";
            };
          };

          mkRustToolchain =
            targets:
            rust-overlay.packages.${system}.rust-nightly.override {
              inherit targets;
              extensions = [ "rust-src" ];
            };

          targetLinkers = {
            "x86_64-unknown-linux-musl" = {
              package = pkgs.pkgsCross.musl64.stdenv.cc;
              hook = ''
                export CARGO_TARGET_X86_64_UNKNOWN_LINUX_MUSL_LINKER="${pkgs.pkgsCross.musl64.stdenv.cc}/bin/x86_64-unknown-linux-musl-gcc"
              '';
            };
            "aarch64-unknown-linux-musl" = {
              package = pkgs.pkgsCross.aarch64-multiplatform-musl.stdenv.cc;
              hook = ''
                export CARGO_TARGET_AARCH64_UNKNOWN_LINUX_MUSL_LINKER="${pkgs.pkgsCross.aarch64-multiplatform-musl.stdenv.cc}/bin/aarch64-unknown-linux-musl-gcc"
              '';
            };
            "i686-unknown-linux-musl" = {
              package = pkgsCrossI686Musl.stdenv.cc;
              hook = ''
                export CARGO_TARGET_I686_UNKNOWN_LINUX_MUSL_LINKER="${pkgsCrossI686Musl.stdenv.cc}/bin/${pkgsCrossI686Musl.stdenv.cc.targetPrefix}gcc"
              '';
            };
            "armv7-unknown-linux-musleabihf" = {
              package = pkgsCrossArmv7Musl.stdenv.cc;
              hook = ''
                export CARGO_TARGET_ARMV7_UNKNOWN_LINUX_MUSLEABIHF_LINKER="${pkgsCrossArmv7Musl.stdenv.cc}/bin/${pkgsCrossArmv7Musl.stdenv.cc.targetPrefix}gcc"
              '';
            };
            "armv5te-unknown-linux-musleabi" = {
              package = pkgsCrossArmv5teMusl.stdenv.cc;
              hook = ''
                export CARGO_TARGET_ARMV5TE_UNKNOWN_LINUX_MUSLEABI_LINKER="${pkgsCrossArmv5teMusl.stdenv.cc}/bin/${pkgsCrossArmv5teMusl.stdenv.cc.targetPrefix}gcc"
              '';
            };
            "arm-unknown-linux-musleabihf" = {
              package = pkgsCrossArmMusl.stdenv.cc;
              hook = ''
                export CARGO_TARGET_ARM_UNKNOWN_LINUX_MUSLEABIHF_LINKER="${pkgsCrossArmMusl.stdenv.cc}/bin/${pkgsCrossArmMusl.stdenv.cc.targetPrefix}gcc"
              '';
            };
          };

          linkerPackages = lib.mapAttrsToList (_: linker: linker.package) targetLinkers;
          linkerHook = lib.concatStringsSep "\n" (lib.mapAttrsToList (_: linker: linker.hook) targetLinkers);

          mkTargetShell =
            target:
            let
              linker = targetLinkers.${target} or null;
            in
            pkgs.mkShell {
              packages = [
                (mkRustToolchain [ target ])
                pkgs.pkg-config
              ]
              ++ lib.optional (linker != null) linker.package;

              shellHook = ''
                echo "TRAMP-RPC CI shell for ${target}"
                ${lib.optionalString (linker != null) linker.hook}
              '';
            };
        in
        (lib.genAttrs rustTargets mkTargetShell)
        // {
          default = pkgs.mkShell {
            packages = [
              (mkRustToolchain rustTargets)
              pkgs.pkg-config
              pkgs.rust-analyzer
            ]
            ++ linkerPackages;

            shellHook = ''
              echo "TRAMP-RPC development shell (nightly + build-std)"
              echo ""
              echo "Build:"
              echo "  ./scripts/build-all.sh                         # x86_64 Linux (static musl)"
              echo "  ./scripts/build-all.sh aarch64-unknown-linux-musl"
              echo "  ./scripts/build-all.sh x86_64-apple-darwin"
              echo "  ./scripts/build-all.sh --all"

              ${linkerHook}
            '';
          };
        }
      );
    };
}
