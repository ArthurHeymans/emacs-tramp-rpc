{
  buildPackages,
  lib,
  rustPlatform,
}:
rustPlatform.buildRustPackage {
  pname = "emacs-tramp-rpc";
  version = "dev";
  src = lib.cleanSource ./.;
  cargoDeps = rustPlatform.importCargoLock {
    lockFile = ./Cargo.lock;
  };
  nativeCheckInputs = [
    buildPackages.coreutils
    buildPackages.python3
  ];
}
