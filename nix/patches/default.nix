{ runCommand }:

{
  # Apply patches to a directory.
  applyPatches =
    name: path: patches:
      runCommand name { inherit patches; }
        ''
          set -eou pipefail

          cp -r ${path} $out
          chmod -R u+w $out

          for patch in $patches; do
            echo "Applying patch $patch"
            patch -d "$out" -p1 < "$patch"
          done
        '';

  static-haskell-nix-postgrest-libpq =
    ./static-haskell-nix-postgrest-libpq.patch;

  nixpkgs-revert-ghc-bootstrap =
    ./nixpkgs-revert-ghc-bootstrap.patch;

  openssl-split-runtime-dependencies-of-static-builds =
    ./openssl-split-runtime-dependencies-of-static-builds.patch;
}
