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

      # TODO: Remove once available in nixpkgs haskellPackages
      configurator-pg =
        prev.callHackageDirect
          {
            pkg = "configurator-pg";
            ver = "0.2.11";
            sha256 = "sha256-mtGtNawDJgz2ZIEVca+IYXVu4oNw9xsfJiYWAqAbbgc=";
          }
          { };

      # TODO: Remove once available in nixpkgs haskellPackages
      streaming-commons =
        prev.callHackageDirect
          {
            pkg = "streaming-commons";
            ver = "0.2.3.1";
            sha256 = "sha256-Gl2eaJcWe1sxmcE/octWlH9uSnERguf+5H66K4fV87s=";
          }
          { };

      # Downgrade hasql and related packages while we are still on GHC 9.4 for the static build.
      hasql = lib.dontCheck (lib.doJailbreak prev.hasql_1_6_4_4);
      hasql-dynamic-statements = lib.dontCheck prev.hasql-dynamic-statements_0_3_1_5;
      hasql-implicits = lib.dontCheck prev.hasql-implicits_0_1_1_3;
      hasql-notifications = lib.dontCheck prev.hasql-notifications_0_2_2_2;
      hasql-pool = lib.dontCheck prev.hasql-pool_1_0_1;
      hasql-transaction = lib.dontCheck prev.hasql-transaction_1_1_0_1;
      postgresql-binary = lib.dontCheck (lib.doJailbreak prev.postgresql-binary_0_13_1_3);

      http2 =
        prev.callHackageDirect
          {
            pkg = "http2";
            ver = "5.4.0";
            sha256 = "sha256-PeEWVd61bQ8G7LvfLeXklzXqNJFaAjE2ecRMWJZESPE=";
          }
          { };

      http-semantics =
        prev.callHackageDirect
          {
            pkg = "http-semantics";
            ver = "0.4.0";
            sha256 = "sha256-rh0z51EKvsu5rQd5n2z3fSRjjEObouNZSBPO9NFYOF0=";
          }
          { };

      network-run =
        prev.callHackageDirect
          {
            pkg = "network-run";
            ver = "0.5.0";
            sha256 = "sha256-vbXh+CzxDsGApjqHxCYf/ijpZtUCApFbkcF5gyN0THU=";
          }
          { };

      time-manager =
        prev.callHackageDirect
          {
            pkg = "time-manager";
            ver = "0.2.4";
            sha256 = "sha256-sAt/331YLQ2IU3z90aKYSq1nxoazv87irsuJp7ZG3pw=";
          }
          { };

      warp =
        lib.dontCheck (prev.callCabal2nixWithOptions "warp"
          (super.fetchFromGitHub {
            owner = "mkleczek";
            repo = "wai";
            rev = "7ca66f023ccaf2e3862ad97392f1f11afea3b6ff";
            #sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
            sha256 = "sha256-Z/1yikmlDZhjv4LhewjRvW7g5s8KZrHztFRnefEDu7Y=";
          }) "--subpath=warp"
          { });
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
