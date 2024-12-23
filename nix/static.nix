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

      overrides = pkgs.lib.composeExtensions old.overrides (_: prev: {
        postgresql-libpq = (lib.overrideCabal prev.postgresql-libpq {
          # TODO: This section can be simplified when this PR has made it's way to us:
          #  https://github.com/NixOS/nixpkgs/pull/286370
          # Additionally, we need to use the default version in nixpkgs, otherwise the
          # override will not be active as well.
          # Using use-pkg-config flag, because pg_config won't work when cross-compiling
          configureFlags = [ "-fuse-pkg-config" ];
          # postgresql doesn't build in the fully static overlay - but the default
          # derivation is built with static libraries anyway.
          libraryPkgconfigDepends = [ pkgsStatic.libpq ];
          librarySystemDepends = [ ];
        }).overrideAttrs (_: prevAttrs: {
          buildInputs = prevAttrs.buildInputs ++ [ pkgsStatic.openssl ];
        });
      });
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
