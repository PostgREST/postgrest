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

      configurator-pg =
        prev.callHackageDirect
          {
            pkg = "configurator-pg";
            ver = "0.2.6";
            sha256 = "sha256-nkamTOpP/w0vQfOXsoQKEstW3n9qyRsv0TocrEerKlU=";
          }
          { };

      hasql-dynamic-statements =
        lib.dontCheck (prev.callHackageDirect
          {
            pkg = "hasql-dynamic-statements";
            ver = "0.3.1.1";
            sha256 = "sha256-jF50GcCtEUV3TN1UsD4LaSBH6arcqfKhxOk+b+c8Bl8=";
          }
          { });

      hasql-implicits =
        lib.dontCheck (prev.callHackageDirect
          {
            pkg = "hasql-implicits";
            ver = "0.1.0.3";
            sha256 = "sha256-IpAOVHNdXJ53B/fmo+DeNUKiBSS6Bo7Uha/krpMt64g=";
          }
          { });

      hspec-wai-json =
        lib.dontCheck (lib.unmarkBroken prev.hspec-wai-json);

      ptr =
        prev.callHackageDirect
          {
            pkg = "ptr";
            ver = "0.16.8.2";
            sha256 = "sha256-Ei2GeQ0AjoxvsvmWbdPELPLtSaowoaj9IzsIiySgkAQ=";
          }
          { };

      weeder =
        lib.dontCheck (prev.callHackageDirect
          {
            pkg = "weeder";
            ver = "2.4.0";
            sha256 = "sha256-Nhp8EogHJ5SIr67060TPEvQbN/ECg3cRJFQnUtJUyC0=";
          }
          { });

      bytestring-strict-builder =
        lib.dontCheck (prev.callHackageDirect
          {
            pkg = "bytestring-strict-builder";
            ver = "0.4.5.6";
            sha256 = "sha256-NxDR5z+SMJ6d+w3xVnP6nKJ+FHa50vop/vCCnJmnbw4\=";
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
