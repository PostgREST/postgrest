# Derive a fully static Haskell package based on musl instead of glibc.
{ nixpkgs, compiler, patches, allOverlays }:

name: src:
let
  # The nh2/static-haskell-nix project does all the hard work for us.
  static-haskell-nix =
    let
      rev = "66fce684a20451514cb68d71bcb62fb246cb2080";
    in
    builtins.fetchTarball {
      url = "https://github.com/nh2/static-haskell-nix/archive/${rev}.tar.gz";
      sha256 = "0322s6hpdwfyw56galvcgla97rnxd2xigcmc0pkiahzs061dcykn";
    };

  patched-static-haskell-nix =
    patches.applyPatches "patched-static-haskell-nix"
      static-haskell-nix
      [
        # No patches currently required.
      ];

  patchedNixpkgs =
    patches.applyPatches "patched-nixpkgs"
      nixpkgs
      [
        patches.nixpkgs-openssl-split-runtime-dependencies-of-static-builds
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
      (allOverlays.haskell-packages { inherit compiler extraOverrides; })
    ];

  # Apply our overlay to the given pkgs.
  normalPkgs =
    import patchedNixpkgs { inherit overlays; };

  # The static-haskell-nix 'survey' derives a full static set of Haskell
  # packages, applying fixes where necessary.
  survey =
    import "${patched-static-haskell-nix}/survey" { inherit normalPkgs compiler; };
in
survey.haskellPackages."${name}"
