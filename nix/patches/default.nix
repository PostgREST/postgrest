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

  static-haskell-nix-ncurses =
    ./static-haskell-nix-ncurses.patch;
  static-haskell-nix-ghc-bignum =
    ./static-haskell-nix-ghc-bignum.patch;
  static-haskell-nix-openssl =
    ./static-haskell-nix-openssl.patch;
  postgresql-atexit =
    ./postgresql-atexit.patch;
}
