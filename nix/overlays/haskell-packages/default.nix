{ compiler, static ? false }:

self: super:

let
  lib =
    self.haskell.lib;

  overrides =
    final: prev:
      rec {
        protolude =
          prev.callHackageDirect
            {
              pkg = "protolude";
              ver = "0.3.0";
              sha256 = "0iwh4wsjhb7pms88lw1afhdal9f86nrrkkvv65f9wxbd1b159n72";
            } {};

        cabal2nix =
          if static then
            # cabal2nix depends on Cabal 3.0.*, while our pinned version of
            # Nixpkgs only provides 2.4 or 3.2. So we pinned 3.0.0.0 in
            # ./Cabal.nix
            prev.cabal2nix.overrideScope
              (self: super: { Cabal = self.callPackage ./Cabal.nix {}; })
          else
            prev.cabal2nix;
      };
in
{
  haskell =
    super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" =
          super.haskell.packages."${compiler}".override { inherit overrides; };
      };
    };
}
