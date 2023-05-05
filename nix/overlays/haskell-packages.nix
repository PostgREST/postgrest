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
      #         sha256 = "<sha256>";
      #       }
      #       { };
      #
      # To temporarily pin unreleased versions from GitHub:
      #   <name> =
      #     prev.callCabal2nixWithOptions "<name>" (super.fetchFromGitHub {
      #       owner = "<owner>";
      #       repo  = "<repo>";
      #       rev = "<commit>";
      #       sha256 = "<sha256>";
      #    }) "--subpath=<subpath>" {};
      #
      # To fill in the sha256:
      #   update-nix-fetchgit nix/overlays/haskell-packages.nix

      postgresql-libpq = lib.dontCheck
        (prev.callCabal2nix "postgresql-libpq"
          (super.fetchFromGitHub {
            owner = "robx";
            repo = "postgresql-libpq";
            rev = "6196681b97ae3b9f38c00a2909f643724d9068dc"; # pipeline
            sha256 = "094c8jkk9prx831h8rjhv5x1gxwllw27y4al2zhhmc2warsxk2n2";
          })
          { });
      hasql = lib.dontCheck
        (prev.callCabal2nix "hasql"
          (super.fetchFromGitHub {
            owner = "robx";
            repo = "hasql";
            rev = "97ff7ce53e2f4586985a86694e586e077fad1718"; # pipeline2
            sha256 = "0kn1zq4nmbb9kg6bl54w1k69pl5kiaffc8mh6iiiv06q6w8gyr34";
          })
          { });
      hasql-transaction = lib.dontCheck
        (prev.callCabal2nix "hasql-transaction"
          (super.fetchFromGitHub {
            owner = "robx";
            repo = "hasql-transaction";
            rev = "ae5f73438edd4fe424add51fcfd8c855747bee7d"; # pipeline
            sha256 = "0vkmjr3m4xpial1lj7c420lsnjh5f5m4r7s6bimm9prvlyf522a5";
          })
          { });

      hasql-notifications = lib.dontCheck
        (prev.callHackageDirect
          {
            pkg = "hasql-notifications";
            ver = "0.2.0.4";
            sha256 = "sha256-fm1xiDyvDkb5WLOJ73/s8wrWEW23XFS7luAv2brfr8I=";
          }
          { });

      hasql-pool = lib.dontCheck
        (prev.callHackageDirect
          {
            pkg = "hasql-pool";
            ver = "0.9";
            sha256 = "sha256-5UshbbaBVY8eJ/9VagNVVxonRwMcd7UmGqDc35pJNFY=";
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
