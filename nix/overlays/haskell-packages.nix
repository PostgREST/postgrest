{ compiler, extraOverrides ? (final: prev: { }) }:

self: super:
let
  inherit (self.haskell) lib;

  overrides =
    final: prev:
    rec {
      # To pin custom versions of Haskell packages:
      #   protolude =
      #     prev.callHackageDirect
      #       {
      #         pkg = "protolude";
      #         ver = "0.3.0";
      #         sha256 = "0iwh4wsjhb7pms88lw1afhdal9f86nrrkkvv65f9wxbd1b159n72";
      #       }
      #       { };
      #
      # To get the sha256:
      #   nix-prefetch-url --unpack https://hackage.haskell.org/package/protolude-0.3.0/protolude-0.3.0.tar.gz

      # To temporarily pin unreleased versions from GitHub:
      #   <name> =
      #     prev.callCabal2nixWithOptions "<name>" (super.fetchFromGitHub {
      #       owner = "<owner>";
      #       repo  = "<repo>";
      #       rev = "<commit>";
      #       sha256 = "<sha256>";
      #    }) "--subpath=<subpath>" {};
      #
      # To get the sha256:
      #   nix-prefetch-url --unpack https://github.com/<owner>/<repo>/archive/<commit>.tar.gz

      hasql-pool = lib.dontCheck (
        prev.callHackageDirect
          {
            pkg = "hasql-pool";
            ver = "0.7.2";
            sha256 = "sha256-JHTYBmghCb4fhoAoU6TBu8otSOiHrsdyHi/TDAittZY=";
          }
          { });
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
