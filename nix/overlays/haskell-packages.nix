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
      # Notes:
      # - When adding a new package version here, update cabal.
      #   + Update postgrest.cabal with the package version
      #   + Update the index-state in cabal.project.freeze. Run `cabal update` which should return the latest index state.
      # - When adding a new package version here, you have to update stack.
      #   + To update stack.yaml add:
      #   extra-deps:
      #     - <package>-<ver>
      #   + For stack.yaml.lock, CI should report an error with the correct lock, copy/paste that one into the file
      # - To modify and try packages locally, see "Working with locally modified Haskell packages" in the Nix README.

      # Before upgrading fuzzyset to 0.3, check: https://github.com/PostgREST/postgrest/issues/3329
      # jailbreak, because hspec limit for tests
      fuzzyset = prev.fuzzyset_0_2_4;

      hasql-pool = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "hasql-pool";
          ver = "1.0.1";
          sha256 = "sha256-Hf1f7lX0LWkjrb25SDBovCYPRdmUP1H6pAxzi7kT4Gg=";
        }
        { });

      hasql-notifications = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "hasql-notifications";
          ver = "0.2.2.2";
          sha256 = "sha256-myKwlug7OgTa/qP6mHfCD+5Q8IhM17JvpJBfSo+M01k=";
        }
        { });

      jose-jwt = prev.jose-jwt_0_10_0;

      postgresql-libpq = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "postgresql-libpq";
          ver = "0.10.1.0";
          sha256 = "sha256-tXOMqCO8opMilI9rx0D+njqjIjbZsH168Bzb8Aq8Ff4=";
        }
        {
          postgresql = super.libpq;
        });
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
