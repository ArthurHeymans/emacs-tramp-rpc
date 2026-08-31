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

      trampRpcVersion =
        let
          versionPrefix = ";; Version: ";
        in
        lib.removePrefix versionPrefix (
          lib.findFirst (lib.hasPrefix versionPrefix)
            (throw "Could not find Version header in lisp/tramp-rpc.el")
            (lib.splitString "\n" (builtins.readFile "${self}/lisp/tramp-rpc.el"))
        );
      trampVersion = "2.8.2";
      defaultServerArchs =
        pkgs: with pkgs.pkgsCross; [
          musl64
          aarch64-multiplatform-musl
        ];

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
      overlays.default = _: super: {
        emacs-tramp-rpc-server = super.callPackage ./default.nix { };

        emacsPackagesFor =
          emacs:
          ((super.emacsPackagesFor emacs).overrideScope (
            eself: _: {
              tramp-rpc = eself.callPackage (
                {
                  archs ? defaultServerArchs super,
                  serverPackages ? map (arch: {
                    package = arch.callPackage ./default.nix { };
                    system = arch.stdenv.hostPlatform.system;
                  }) archs,
                  lib,
                  melpaBuild,
                  msgpack,
                }:
                let
                  # nixpkgs' GNU ELPA snapshot is older than the minimum
                  # version required by tramp-rpc.
                  trampForTrampRpc = melpaBuild {
                    pname = "tramp";
                    version = trampVersion;
                    src = super.fetchFromGitHub {
                      owner = "emacsmirror";
                      repo = "tramp";
                      rev = "904779d264156bfb4ecfc7fa6c40910354cf4ee8";
                      hash = "sha256-Bw/uysv6SbTmfwg/uSBga+wQRD+tEB/3f6FqE02Wqeg=";
                    };
                    files = ''("lisp/*.el")'';
                    postPatch = ''
                      substitute lisp/trampver.el.in lisp/trampver.el \
                        --replace-fail '@configure_input@' 'Generated for the tramp-rpc Nix package' \
                        --replace-fail '@PACKAGE_VERSION@' '${trampVersion}' \
                        --replace-fail '@PACKAGE_BUGREPORT@' 'tramp-devel@gnu.org' \
                        --replace-fail '@EMACS_REQUIRED_VERSION@' '28.1' \
                        --replace-fail '@PACKAGE_URL@' 'https://www.gnu.org/software/tramp/' \
                        --replace-fail '@TRAMP_EMACS_VERSION_CHECK@' '"ok"'
                    '';
                    packageRequires = [ ];
                  };
                in
                melpaBuild rec {
                  pname = "tramp-rpc";
                  version = trampRpcVersion;
                  src = self;
                  files = ''("lisp/*")'';

                  postInstall = lib.concatMapStringsSep "\n" (server: ''
                    install -m755 -D ${server.package}/bin/tramp-rpc-server $out/share/emacs/site-lisp/elpa/${pname}-${version}/binaries/${server.system}/tramp-rpc-server
                  '') serverPackages;

                  packageRequires = [
                    trampForTrampRpc
                    msgpack
                  ];
                }
              ) { };
            }
          ));
      };
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

      checks = lib.genAttrs [ "x86_64-linux" ] (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
          };
          emacsPackages = pkgs.emacsPackagesFor pkgs.emacs;
          emacsWithTrampRpc = emacsPackages.emacsWithPackages (epkgs: [
            (epkgs.tramp-rpc.override {
              serverPackages = [
                {
                  package = pkgs.writeShellScriptBin "tramp-rpc-server" "exit 0";
                  system = pkgs.stdenv.hostPlatform.system;
                }
              ];
            })
          ]);

        in
        {
          emacs-package =
            pkgs.runCommand "tramp-rpc-emacs-package-check"
              {
                nativeBuildInputs = [ emacsWithTrampRpc ];
              }
              ''
                export HOME="$TMPDIR"
                emacs --batch --eval '
                  (progn
                    (require (quote tramp-rpc))
                    (let* ((arch (tramp-rpc-deploy--detect-local-arch))
                           (binary (tramp-rpc-deploy--bundled-binary-path arch)))
                      (unless (and binary (file-executable-p binary))
                        (error "Missing executable bundled server binary for %s: %S"
                               arch binary))))'
                touch "$out"
              '';
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
