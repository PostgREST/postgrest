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

  # See: https://github.com/NixOS/nixpkgs/issues/104133
  nixpkgs-gdb-fix-libintl =
    ./nixpkgs-gdb-fix-libintl.patch;

  # Fix how openssl is linked on static builds, see:
  # https://github.com/nh2/static-haskell-nix/pull/91
  static-haskell-nix-postgrest-openssl-linking-fix =
    ./static-haskell-nix-postgrest-openssl-linking-fix.patch;

  static-haskell-nix-hasql-notifications-openssl-linking-fix =
    ./static-haskell-nix-hasql-notifications-openssl-linking-fix.patch;
}
