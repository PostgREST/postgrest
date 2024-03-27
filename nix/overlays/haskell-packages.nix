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
      #     prev.callCabal2nixWithOptions "<name>" (super.fetchFromGitHub {
      #       owner = "<owner>";
      #       repo  = "<repo>";
      #       rev = "<commit>";
      #       sha256 = "<sha256>";
      #    }) "--subpath=<subpath>" {};
      #
      # To fill in the sha256:
      #   update-nix-fetchgit nix/overlays/haskell-packages.nix
      #
      # - Nowadays you can just delete the sha256 attribute above and nix will assume a fake sha.
      # Once you build the derivation it will suggest the correct sha.
      # - If the library fails its test suite (usually when it runs IO tests), wrap the expression with `lib.dontCheck ()`
      # - <subpath> is usually "."
      # - When adding a new library version here, postgrest.cabal and stack.yaml must also be updated
      #
      # Note:
      # - This should NOT be the first place to start managing dependencies. Check postgrest.cabal.
      # - To modify and try packages locally, see "Working with locally modified Haskell packages" in the Nix README.
      #


      configurator-pg =
        prev.callHackageDirect
          {
            pkg = "configurator-pg";
            ver = "0.2.9";
            sha256 = "sha256-UqFiOgPlksbIdHBVO0wYhCnboB+mxKJcXVhY9C1V7Hg=";
          }
          { };

      # Before upgrading fuzzyset to 0.3, check: https://github.com/PostgREST/postgrest/issues/3329
      fuzzyset =
        prev.callHackageDirect
          {
            pkg = "fuzzyset";
            ver = "0.2.4";
            sha256 = "sha256-lpkrTFcR0B4rT/P6x7ui31Twgq7BBj6KIvjKyqXKdpc=";
          }
          { };

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
