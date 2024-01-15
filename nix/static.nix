{ compiler
, name
, pkgs
, src
}:
let
  # This builds a static PostgREST exectuable based on pkgsStatic.
  # pkgsStatic is based on musl, so is a kind of cross-compilation.
  # We still make this explicit here via pkgsCross, because we need
  # to get postgresql/libpq for musl, too.
  pkgsCross = pkgs.pkgsCross.musl64;
  inherit (pkgsCross) pkgsStatic;
  inherit (pkgsStatic.haskell) lib;

  # postgresql doesn't build in the fully static overlay - but the default
  # derivation is built with static libraries anyway.
  libpq = (pkgsCross.postgresql.override {
    # disable server-side only stuff to improve compilation times
    enableSystemd = false;
    jitSupport = false;

    # disable gssapi support, because it leads to linking errors
    gssSupport = false;
  }).overrideAttrs (finalAttrs: prevAttrs: {
    dontDisableStatic = true;
    # Tests fail for initdb and other server-side code which we don't care about
    doCheck = false;
  });

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

      overrides = pkgs.lib.composeExtensions old.overrides (final: prev: {
        postgresql-libpq = (prev.postgresql-libpq.override {
          postgresql = libpq;
        }).overrideAttrs (finalAttrs: prevAttrs: {
          # Using use-pkg-config flag, because pg_config won't work when cross-compiling
          configureFlags = prevAttrs.configureFlags ++ [ "-fuse-pkg-config" ];
          # Using pkg-config without pkgsCross, because "pkg-config" is hardcoded in
          # postgresql-libpq's Setup.hs. Using pkgsStatic to make pkg-config return the
          # static libs for libpq.
          nativeBuildInputs = prevAttrs.nativeBuildInputs ++ [ pkgs.pkgsStatic.pkg-config ];

          buildInputs = [
            pkgsStatic.openssl
          ];
        });
      });
    });

  makeExecutableStatic = drv:
    lib.justStaticExecutables
      (lib.appendConfigureFlag drv "--enable-executable-static");

in
{
  inherit packagesStatic;

  postgrestStatic = makeExecutableStatic (packagesStatic.callCabal2nix name src { });
}
