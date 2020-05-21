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

  # Patch is required for static builds on GHC 8.8.3, see:
  # https://github.com/NixOS/nixpkgs/issues/85924
  nixpkgs-revert-ghc-bootstrap =
    ./nixpkgs-revert-ghc-bootstrap.patch;

  # See: https://github.com/NixOS/nixpkgs/pull/87879
  nixpkgs-openssl-split-runtime-dependencies-of-static-builds =
    ./nixpkgs-openssl-split-runtime-dependencies-of-static-builds.patch;
}
