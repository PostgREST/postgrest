{ compiler
, name
, pkgs
, src
}:
let
  # This builds a static PostgREST executable based on pkgsStatic.
  inherit (pkgs) pkgsStatic;
  inherit (pkgsStatic.haskell) lib;

  packagesStatic =
    pkgsStatic.haskell.packages."${compiler}".override (old: {
      ghc = pkgsStatic.pkgsBuildHost.haskell.compiler."${compiler}".override {
        # Using the bundled libffi generally works better for cross-compiling
        libffi = null;
        # Building sphinx fails on some platforms
        enableDocs = false;
        # Cross compiling with native bignum works better than with gmp
        enableNativeBignum = true;
      };
    });

  makeExecutableStatic = drv: pkgs.lib.pipe drv [
    lib.compose.justStaticExecutables

    # To successfully compile a redistributable, fully static executable we need to:
    # 1. avoid any references to /nix/store to prevent blowing up the closure size.
    (drv: drv.overrideAttrs {
      allowedReferences = [
        pkgsStatic.openssl.etc
      ];
    })

    # 2. be able to run the executable.
    (drv: drv.overrideAttrs {
      passthru.tests.version = pkgsStatic.testers.testVersion {
        package = drv;
      };
    })
  ];

in
{
  inherit packagesStatic;

  postgrestStatic = makeExecutableStatic (packagesStatic.callCabal2nix name src { });
}
