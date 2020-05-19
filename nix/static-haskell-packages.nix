# Derive a fully static set of Haskell packages based on musl instead of glibc.
{ nixpkgs, compiler, patches }:
let
  # The nh2/static-haskell-nix project does all the hard work for us.
  static-haskell-nix =
    let
      rev = "749707fc90b781c3e653e67917a7d571fe82ae7b";
    in
      builtins.fetchTarball {
        url = "https://github.com/nh2/static-haskell-nix/archive/${rev}.tar.gz";
        sha256 = "155spda2lww378bhx68w6dxwqd5y6s9kin3qbgl2m23r3vmk3m3w";
      };

  patchedNixpkgs =
    patches.applyPatches "patched-nixpkgs" nixpkgs
      [
        patches.nixpkgs-revert-ghc-bootstrap
        patches.nixpkgs-openssl-split-runtime-dependencies-of-static-builds
      ];

  overlays =
    [
      (import overlays/haskell-packages { inherit compiler; static = true; })
    ];

  # Apply our overlay to the given pkgs.
  normalPkgs =
    import patchedNixpkgs { inherit overlays; };

  # Each version of GHC needs a specific version of Cabal.
  defaultCabalPackageVersionComingWithGhc =
    {
      ghc883 = "Cabal_3_2_0_0";
    }."${compiler}";

  # The static-haskell-nix 'survey' derives a full static set of Haskell
  # packages, applying fixes where necessary.
  survey =
    import "${static-haskell-nix}/survey"
      { inherit normalPkgs compiler defaultCabalPackageVersionComingWithGhc; };
in
survey.haskellPackages
