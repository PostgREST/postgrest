{ compiler
, name
, pkgs
, src
}:
let
  # This builds a static PostgREST executable based on pkgsStatic.
  inherit (pkgs) pkgsStatic;
  inherit (pkgsStatic.haskell) lib;

  packagesStatic = pkgsStatic.haskell.packages.native-bignum."${compiler}";

  makeExecutableStatic = drv: pkgs.lib.pipe drv [
    lib.compose.justStaticExecutables

    # To successfully compile a redistributable, fully static executable we need to:
    # 1. avoid any references to /nix/store to prevent blowing up the closure size.
    # 2. be able to run the executable.
    (drv: drv.overrideAttrs (finalAttrs: {
      allowedReferences = [
        pkgsStatic.openssl.etc
      ];

      passthru.tests.version = pkgsStatic.testers.testVersion {
        package = finalAttrs.finalPackage;
      };
    }))
  ];

in
{
  inherit packagesStatic;

  postgrestStatic = makeExecutableStatic (packagesStatic.callCabal2nix name src { });
}
