# Turn a Haskell source package (given its name and source) into a derivation
# for a fully static executable.
{ nixpkgs, compiler, name, src, patches }:
let
  # The nh2/static-haskell-nix project does all the hard work for us for
  # building static Haskell executables. We apply a patch for PostgREST below
  # until the respective pull request is merged. See:
  # https://github.com/nh2/static-haskell-nix/pull/91
  # statix-haskell-nix builds everything based on Musl instead of glibc, so
  # there will be a _lot_ to rebuild if you don't use a binary cache.
  static-haskell-nix =
    let
      rev = "749707fc90b781c3e653e67917a7d571fe82ae7b";
    in
      builtins.fetchTarball {
        url = "https://github.com/nh2/static-haskell-nix/archive/${rev}.tar.gz";
        sha256 = "155spda2lww378bhx68w6dxwqd5y6s9kin3qbgl2m23r3vmk3m3w";
      };

  patched-static-haskell-nix =
    patches.applyPatches "patched-static-haskell-nix" static-haskell-nix
      [
        patches.static-haskell-nix-postgrest-libpq
      ];

  haskellPackagesOverrides =
    lib: final: prev:
      {
        # Add our source package.
        "${name}" = prev.callCabal2nix name src {};

        # cabal2nix depends on Cabal 3.0.*, while our pinned version of Nixpkgs
        # only provides 2.4 or 3.2. So we pinned 3.0.0.0 in ./Cabal.nix
        cabal2nix =
          prev.cabal2nix.overrideScope
            (self: super: { Cabal = self.callPackage ./Cabal.nix {}; });

        # Pinned version of protolude.
        protolude =
          prev.callPackage ../overlays/haskell-packages/protolude.nix {};

        # The tests for the packages below took a long time on static
        # builds, so we disable them for now - to be investigated.
        happy = lib.dontCheck prev.happy;
      };

  # This overlay adds our source package and applies adjustments to the
  # derivation of other packages that it depends on. The overlay applies to
  # Haskell packages for the given compiler, which we will later use
  # with the static-haskell-nix survey.
  overlay =
    self: super:
    # Override the set of Haskell packages at
    # pkgs.haskell.packages."${compiler}".
      {
        haskell = super.haskell // {
          packages = super.haskell.packages // {
            "${compiler}" =
              super.haskell.packages."${compiler}".override
                { overrides = haskellPackagesOverrides self.haskell.lib; };
          };
        };
      };

  patchedNixpkgs =
    patches.applyPatches "patched-nixpkgs" nixpkgs
      [
        patches.nixpkgs-revert-ghc-bootstrap
        patches.openssl-split-runtime-dependencies-of-static-builds
      ];

  # Apply our overlay to the given pkgs.
  normalPkgs =
    (import patchedNixpkgs {}).appendOverlays [ overlay ];

  # Each version of GHC needs a specific version of Cabal.
  defaultCabalPackageVersionComingWithGhc =
    {
      ghc883 = "Cabal_3_2_0_0";
    }."${compiler}";

  # Let the static-haskell-nix project do the hard work of deriving a set of
  # fully static Haskell executables, including one for the our source package
  # that we added through the overlay.
  survey =
    import "${patched-static-haskell-nix}/survey"
      { inherit normalPkgs compiler defaultCabalPackageVersionComingWithGhc; };
in
  # Return the fully static derivation of our source package.
survey.haskellPackages."${name}"
