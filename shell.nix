{ pkgs ? import ./nixpkgs.nix {} }:
with pkgs;

mkShell {
  BAZEL_USE_CPP_ONLY_TOOLCHAIN=1;

  buildInputs = [
    bat
    bazel-watcher
    bazel_5
    binutils
    cabal-install
    cacert
    ghc
    go
    ghcid
    hpack
    nix
    nodejs
    openjdk11
    python3
    zlib
    zlib.dev
  ];

  shellHook = ''
    # Add nix config flags to .bazelrc.local.
    #
    BAZELRC_LOCAL=".bazelrc.local"
    if [ ! -e "$BAZELRC_LOCAL" ]
    then
      echo "[!] It looks like you are using a Nix-based system."
      echo "In order to build this project, you need to add the two"
      echo "following host_platform entries to your .bazelrc.local file:"
      echo
      echo "build --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host"
      echo "run --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host"
    fi
    '';
}
