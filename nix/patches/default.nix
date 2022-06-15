{ runCommand }:

{
  applyPatches =
    name: src: patches:
    runCommand
      name
      { inherit src patches; }
      ''
        set -eou pipefail

        cp -r $src $out
        chmod -R u+w $out

        for patch in $patches; do
          echo "Applying patch $patch"
          patch -d "$out" -p1 < "$patch"
        done
      '';

  # See: https://github.com/NixOS/nixpkgs/pull/87879
  nixpkgs-openssl-split-runtime-dependencies-of-static-builds =
    ./nixpkgs-openssl-split-runtime-dependencies-of-static-builds.patch;

  static-haskell-nix-ncurses =
    ./static-haskell-nix-ncurses.patch;
  static-haskell-nix-ghc-bignum =
    ./static-haskell-nix-ghc-bignum.patch;
}
