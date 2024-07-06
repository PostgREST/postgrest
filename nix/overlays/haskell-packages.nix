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
      fuzzyset = lib.doJailbreak
        (prev.callHackageDirect
          {
            pkg = "fuzzyset";
            ver = "0.2.4";
            sha256 = "sha256-lpkrTFcR0B4rT/P6x7ui31Twgq7BBj6KIvjKyqXKdpc=";
          }
          { });

      hasql-pool = lib.dontCheck prev.hasql-pool_1_0_1;

      jose-jwt = prev.jose-jwt_0_10_0;

      postgresql-libpq = lib.dontCheck
        (prev.postgresql-libpq.override {
          postgresql = super.libpq;
        });

      hasql-notifications = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "hasql-notifications";
          ver = "0.2.2.0";
          sha256 = "sha256-73OQ9/su2qvO7HavF3xuuNWLXSXyB9reBUQDaHys06I=";
        }
        { }
      );

      wai-cors = lib.dontCheck (
        prev.callCabal2nixWithOptions "wai-cors" (super.fetchFromGitHub {
          owner = "laurenceisla";
          repo  = "wai-cors";
          rev = "e2da0d7a3e56592a5f4b35fd3aa6ccd3175525cc";
          sha256 = "sha256-lTzFiH7c7ZpR+hg6jJXvPKM2DtwMs8gdjdk68GTmtaQ=";
        }) "--subpath=." {}
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
