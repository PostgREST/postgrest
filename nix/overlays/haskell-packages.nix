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

      hs-opentelemetry-sdk = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "hs-opentelemetry-sdk";
          ver = "0.1.0.0";
          sha256 = "sha256-kg6iYyEW2a/qb7FFXbph/xKPFW/6Wqhl5P9NZotgbVs=";
        }
        { });
      hs-opentelemetry-propagator-datadog = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "hs-opentelemetry-propagator-datadog";
          ver = "0.0.1.0";
          sha256 = "sha256-V2FOsdyrR3X44FILTRpDIDNghc5vPIDx7z0CUGyJXQk=";
        }
        { });
      hs-opentelemetry-exporter-otlp = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "hs-opentelemetry-exporter-otlp";
          ver = "0.1.0.0";
          sha256 = "sha256-Y0ihGMDIu3GcN7wkjnth4z72WfyBUmqSNrJEvoGxi6M=";
        }
        { });
      hs-opentelemetry-instrumentation-wai = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "hs-opentelemetry-instrumentation-wai";
          ver = "0.1.1.0";
          sha256 = "sha256-9jz06jEOAfuDtk7RS7cntCDPmORukeS7hHYP04vxGXA=";
        }
        { });
      hs-opentelemetry-api = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "hs-opentelemetry-api";
          ver = "0.2.0.0";
          sha256 = "sha256-IgyI6J9ZiN9x0A/Jdp9fsdhJTqX3AJyTLlKmk8hFsTk=";
        }
        { });
      hs-opentelemetry-propagator-b3 = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "hs-opentelemetry-propagator-b3";
          ver = "0.0.1.2";
          sha256 = "sha256-hUk4f/xngG5NujSJGGb7lWawNE6EAbvw/8krKsGGsPY=";
        }
        { });
      hs-opentelemetry-propagator-w3c = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "hs-opentelemetry-propagator-w3c";
          ver = "0.0.1.4";
          sha256 = "sha256-Rq+bcerTD4Pqzr1sznvoOtkKanlV+0Blq3EXXN2HQuU=";
        }
        { });
      hs-opentelemetry-otlp = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "hs-opentelemetry-otlp";
          ver = "0.1.0.0";
          sha256 = "sha256-xZFIlyx2BKnwo6XCblCZTukNsjv/uG5T3u8uKlKJ1yc=";
        }
        { });
      hs-opentelemetry-utils-exceptions = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "hs-opentelemetry-utils-exceptions";
          ver = "0.2.0.1";
          sha256 = "sha256-gukjbleRa4PKWcyBXC1J0kSQzohF5Or+ayvp5wxrzT0=";
        }
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
