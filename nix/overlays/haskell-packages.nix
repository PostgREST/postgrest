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

      hashtables = lib.dontCheck prev.hashtables_1_3_1;
      hasql = lib.dontCheck prev.hasql_1_6_1_4;
      hasql-dynamic-statements = lib.dontCheck prev.hasql-dynamic-statements_0_3_1_2;
      hasql-pool = lib.dontCheck
        (prev.callHackageDirect
          {
            pkg = "hasql-pool";
            ver = "0.8.0.6";
            sha256 = "sha256-2u/cwPk8XfXffaDRzGeyzhL+9k2+2T4b8bGOZwz8AX0=";
          }
          { });
      hasql-transaction = lib.dontCheck prev.hasql-transaction_1_0_1_2;
      isomorphism-class = lib.unmarkBroken prev.isomorphism-class;
      lens = lib.dontCheck prev.lens_5_2;
      postgresql-binary = lib.dontCheck prev.postgresql-binary_0_13_1;
      text-builder = lib.dontCheck prev.text-builder_0_6_7;
      text-builder-dev = lib.dontCheck prev.text-builder-dev_0_3_3;

      postgresql-libpq = lib.dontCheck
        (prev.callCabal2nix "postgresql-libpq"
          (super.fetchFromGitHub {
            owner = "PostgREST";
            repo = "postgresql-libpq";
            rev = "cef92cb4c07b56568dffdbf4b719258b82183119"; # master
            sha256 = "0r59klrz47qcnd22s47h612mlz3jbg40wwalfj3f6djwg0cdyr85";
          })
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
