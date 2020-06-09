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
      # To get the sha256
      # nix-prefetch-url --unpack https://hackage.haskell.org/package/hasql-notifications-0.1.0.0/hasql-notifications-0.1.0.0.tar.gz
      hasql-notifications =
        self.haskell.lib.overrideCabal
          (
            prev.callHackageDirect
              {
                pkg = "hasql-notifications";
                ver = "0.1.0.0";
                sha256 = "1z17gsqvvzzi0yipc3qy3jz8vzpww4vsc4vaj2kbzr2mfliq6fx3";
              } { }
          )
          (old: { doCheck = false; });
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
