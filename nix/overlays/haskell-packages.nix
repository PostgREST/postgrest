{ compiler, extraOverrides ? (final: prev: { }) }:

self: super:
let
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

      # TODO: We need to patch upstream for unbreaking hasql-dynamic-statements, hasql-implicits, ptr
      hasql-dynamic-statements =
        self.haskell.lib.dontCheck (prev.callHackageDirect
          {
            pkg = "hasql-dynamic-statements";
            ver = "0.3.1";
            sha256 = "1zjv91xlfkyxwq6mhzj7rsfm4kjvs9ygkgbl6jbbg19jihcn2kiy";
          }
          { }
        );
      hasql-implicits =
        prev.callHackageDirect
          {
            pkg = "hasql-implicits";
            ver = "0.1.0.2";
            sha256 = "1z05amiy5zmf8fmr3dqp8b4svb0sj037gdjc5b9va5d5kdi95bv7";
          }
          { };
      ptr =
        prev.callHackageDirect
          {
            pkg = "ptr";
            ver = "0.16.7.2";
            sha256 = "1njb05jc1bdyxk7qh7s1y4ivn5nrpy3rhlkf4jlfamvlg8idkavc";
          }
          { };
      protolude = prev.protolude_0_3_0;
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
