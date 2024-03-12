{ compiler }:

self: super:
let
  inherit (self.haskell) lib;

  overrides =
    _: prev:
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
      #     lib.dontCheck (prev.callCabal2nixWithOptions "<name>" (super.fetchFromGitHub {
      #       owner = "<owner>";
      #       repo  = "<repo>";
      #       rev = "<commit>";
      #       sha256 = "<sha256>";
      #    }) "--subpath=." {});
      #
      # To fill in the sha256:
      #   update-nix-fetchgit nix/overlays/haskell-packages.nix
      #
      # Nowadays you can just delete the sha256 attribute above and nix will assume a fake sha.
      # Once you build the derivation it will suggest the correct sha.

      configurator-pg =
        prev.callHackageDirect
          {
            pkg = "configurator-pg";
            ver = "0.2.9";
            sha256 = "sha256-UqFiOgPlksbIdHBVO0wYhCnboB+mxKJcXVhY9C1V7Hg=";
          }
          { };

      # Marked as broken (?)
      fuzzyset = lib.markUnbroken prev.fuzzyset;

      postgresql-libpq = lib.dontCheck
        (prev.postgresql-libpq_0_10_0_0.override {
          postgresql = super.libpq;
        });

      hasql-pool = lib.dontCheck prev.hasql-pool_0_10;

      hasql-notifications = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "hasql-notifications";
          ver = "0.2.1.0";
          sha256 = "sha256-MEIirDKR81KpiBOnWJbVInWevL6Kdb/XD1Qtd8e6KsQ=";
        }
        { }
      );

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
