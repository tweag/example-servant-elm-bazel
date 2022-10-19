{ pkgs ? import ./nixpkgs.nix {} }:
with pkgs;

mkShell {
  BAZEL_USE_CPP_ONLY_TOOLCHAIN=1;

  buildInputs = [
    bazel_4
    binutils
    cacert
    nix
    openjdk11
    python3
    go
    hpack
    bat
    nodejs
    cabal-install
    ghc
    zlib
    zlib.dev
    # convenience dependencies
    less
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
