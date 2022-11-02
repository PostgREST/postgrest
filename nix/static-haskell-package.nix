# Derive a fully static Haskell package based on musl instead of glibc.
{ nixpkgs, system, compiler, patches, allOverlays }:

name: src:
let
  # The nh2/static-haskell-nix project does all the hard work for us.
  static-haskell-nix =
    let
      rev = "bd66b86b72cff4479e1c76d5916a853c38d09837";
    in
    builtins.fetchTarball {
      url = "https://github.com/nh2/static-haskell-nix/archive/${rev}.tar.gz";
      sha256 = "0rnsxaw7v27znsg9lgqk1i4007ydqrc8gfgimrmhf24lv6galbjh";
    };

  patched-static-haskell-nix =
    patches.applyPatches "patched-static-haskell-nix"
      static-haskell-nix
      [
        patches.static-haskell-nix-ncurses
        patches.static-haskell-nix-ghc-bignum
        patches.static-haskell-nix-openssl
      ];

  extraOverrides =
    final: prev:
    rec {
      # We need to add our package needs to the package set that we pass to
      # static-haskell-nix. Using callCabal2nix on the haskellPackages that
      # it returns would result in a dynamic build based on musl, and not the
      # fully static build that we want.
      "${name}" = prev.callCabal2nix name src { };
    };

  overlays =
    [
      allOverlays.postgresql-future
      (allOverlays.postgresql-default { inherit patches; })
      (allOverlays.haskell-packages { inherit compiler extraOverrides; })
      # Disable failing tests for postgresql on musl that should have no impact
      # on the libpq that we need (collate.icu.utf8 and foreign regression
      # tests)
      (self: super:
        { postgresql = super.postgresql.overrideAttrs (_: { doCheck = false; }); }
      )
    ];

  # Apply our overlay to nixpkgs.
  normalPkgs =
    import nixpkgs { inherit overlays system; };

  defaultCabalPackageVersionComingWithGhc =
    {
      ghc924 = "Cabal_3_6_3_0";
    }."${compiler}";

  # The static-haskell-nix 'survey' derives a full static set of Haskell
  # packages, applying fixes where necessary.
  survey =
    import "${patched-static-haskell-nix}/survey" { inherit normalPkgs compiler defaultCabalPackageVersionComingWithGhc; };
in
survey.haskellPackages."${name}"
