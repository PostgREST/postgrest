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
      #   + Update cabal.project.freeze. Just set it to the current timestamp then run `cabal build`. It will tell you the correct timestamp for the index state.
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

      hasql-pool = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "hasql-pool";
          ver = "1.1";
          sha256 = "sha256-TJZdUwsBo4pLLYydc8FgxYFI37Tj5K+E7idf4fQYEjU=";
        }
        { }
      );

      postgresql-libpq = lib.dontCheck
        (prev.postgresql-libpq.override {
          postgresql = super.libpq;
        });

      hasql-notifications = lib.dontCheck
        (prev.callCabal2nixWithOptions "hasql-notifications" (super.fetchFromGitHub {
          owner = "laurenceisla";
          repo  = "hasql-notifications";
          rev = "71059bb9e992160dae9ef966e05bc7373ea002ab";
          sha256 = "sha256-4BF/G1Tmnmzk3wDCSezmn86CB5IMAZ4XUffEv95v208=";
        }) "--subpath=." {});

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
