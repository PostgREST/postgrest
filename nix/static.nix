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
          # postgresql doesn't build in the fully static overlay - but the default
          # derivation is built with static libraries anyway.
          postgresql = pkgsCross.libpq;
        }).overrideAttrs (finalAttrs: prevAttrs: {
          # Using use-pkg-config flag, because pg_config won't work when cross-compiling
          configureFlags = prevAttrs.configureFlags ++ [ "-fuse-pkg-config" ];
          # Using pkg-config without pkgsCross, because "pkg-config" is hardcoded in
          # postgresql-libpq's Setup.hs. Using pkgsStatic to make pkg-config return the
          # static libs for libpq.
          nativeBuildInputs = prevAttrs.nativeBuildInputs ++ [ pkgs.pkgsStatic.pkg-config ];

          buildInputs = prevAttrs.buildInputs ++ [
            (pkgsStatic.libkrb5.overrideAttrs (finalAttrs: prevAttrs: {
              configureFlags = prevAttrs.configureFlags ++ [
                "--sysconfdir=/etc"
                # disable keyutils dependency, to avoid linking errors
                "--without-keyutils"
              ];

              postInstall = prevAttrs.postInstall + ''
                ${pkgsStatic.pkgsBuildHost.removeReferencesTo}/bin/remove-references-to -t $out $out/lib/*.a
              '';
            }))
          ];
        });
      });
    });

  makeExecutableStatic = drv: pkgs.lib.pipe drv [
    (lib.compose.appendConfigureFlags [ "--enable-executable-static" ])
    lib.compose.justStaticExecutables

    # To successfully compile a redistributable, fully static executable we need to:
    # 1. make executable really statically linked.
    # 2. avoid any references to /nix/store to prevent blowing up the closure size.
    # 3. be able to run the executable.
    # When checking for references, we ignore the following:
    # - eeee... are removed references which don't actually exist
    # - openssl-etc references are purposely designed to be very small
    (lib.compose.overrideCabal (drv: {
      postFixup = drv.postFixup + ''
        exe="$out/bin/postgrest"

        if ! (file "$exe" | grep 'statically linked') then
          echo "not a static executable, ldd output:"
          ldd "$exe"
          exit 1
        fi

        echo "Checking for references to /nix/store..."
        (${pkgsStatic.binutils}/bin/strings "$exe" \
          | grep -v /nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee \
          | grep -v -etc/etc/ssl \
          | grep /nix/store || exit 0 && exit 1)
        echo "No references to /nix/store found"

        "$exe" --help
      '';
    }))
  ];

in
{
  inherit packagesStatic;

  postgrestStatic = makeExecutableStatic (packagesStatic.callCabal2nix name src { });
}
