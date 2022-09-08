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

      hashtables = lib.dontCheck prev.hashtables_1_3;
      isomorphism-class = lib.unmarkBroken prev.isomorphism-class;
      text-builder = lib.dontCheck prev.text-builder_0_6_7;
      text-builder-dev = lib.dontCheck prev.text-builder-dev_0_3_3;

      postgresql-binary = lib.dontCheck
        (prev.callHackageDirect
          {
            pkg = "postgresql-binary";
            ver = "0.12.5";
            sha256 = "1vk97lw25i7d0pvjzd7s3m13nya9ycnrjr8y4qhw2jgjnvkblnzv";
          }
          { });

      hasql = lib.dontCheck
        (prev.callHackageDirect
          {
            pkg = "hasql";
            ver = "1.6.1.1";
            sha256 = "1sv0500dvfln9ljxkd2jrfl9nbpkax7z5b8zjy9yjps1r6s1cmj0";
          }
          { });

      lens = lib.dontCheck prev.lens_5_2;

      hasql-dynamic-statements = lib.dontCheck prev.hasql-dynamic-statements_0_3_1_2;
      hasql-transaction = lib.dontCheck prev.hasql-transaction_1_0_1_2;

      hasql-notifications = lib.dontCheck
        (prev.callHackageDirect
          {
            pkg = "hasql-notifications";
            ver = "0.2.0.3";
            sha256 = "1v44fp03685ngs1l9a7ihkfg5zgvh49k7ym9sh70wjsqql80dhf7";
          }
          { });

      hasql-pool = lib.dontCheck
        (prev.callHackageDirect
          {
            pkg = "hasql-pool";
            ver = "0.8.0.2";
            sha256 = "0a646w7m1p430hp52q8d52sgd14zcyrjvflmw7qbak565jmksqgl";
          }
          { });

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
