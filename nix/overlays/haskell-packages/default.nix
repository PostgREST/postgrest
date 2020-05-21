{ compiler, extraOverrides ? (final: prev: { }) }:

self: super:
let
  overrides =
    final: prev:
    rec {
      protolude =
        prev.callHackageDirect
          {
            pkg = "protolude";
            ver = "0.3.0";
            sha256 = "0iwh4wsjhb7pms88lw1afhdal9f86nrrkkvv65f9wxbd1b159n72";
          } { };
    } // extraOverrides final prev;
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
