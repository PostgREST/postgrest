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

      protolude =
        prev.callHackageDirect
          {
            pkg = "protolude";
            ver = "0.3.1";
            sha256 = "0gf0mn1ycllr69kdq1p07qf7935s10jz0nnhynwqy3d6nmycxr5j";
          }
          { };

      wai-extra =
        prev.callHackageDirect
          {
            pkg = "wai-extra";
            ver = "3.1.8";
            sha256 = "1ha8sxc2ii7k7xs5nm06wfwqmf4f1p2acp4ya0jnx6yn6551qps4";
          }
          { };

      wai-logger =
        prev.callHackageDirect
          {
            pkg = "wai-logger";
            ver = "2.3.7";
            sha256 = "1d23fdbwbahr3y1vdyn57m1qhljy22pm5cpgb20dy6mlxzdb30xd";
          }
          { };

      warp =
        lib.dontCheck (prev.callHackageDirect
          {
            pkg = "warp";
            ver = "3.3.19";
            sha256 = "0y3jj4bhviss6ff9lwxki0zbdcl1rb398bk4s80zvfpnpy7p94cx";
          }
          { });

      hasql-dynamic-statements =
        lib.dontCheck (lib.unmarkBroken prev.hasql-dynamic-statements);

      hasql-implicits =
        lib.dontCheck (lib.unmarkBroken prev.hasql-implicits);

      ptr =
        lib.dontCheck (lib.unmarkBroken prev.ptr);
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
