{ compiler }:

self: super:

{
  haskellPackages =
    super.haskell.packages."${compiler}".override {
      overrides =
        newPkgs: oldPkgs: rec {
          hasql-pool =
            newPkgs.callPackage ../haskell-packages/hasql-pool.nix {};
        };
    };
}
