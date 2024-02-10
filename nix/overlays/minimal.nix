# This overlay reduces the closure size of our nix tooling to as much as possible.
# This makes rebuilding, downloading from cachix and thus CI faster.
{ compiler }:
self: super:
let
  haskellLib = super.haskell.lib;

  minimizePostgresql = drv: drv.overrideAttrs (finalAttrs: prevAttrs: {
    buildInputs = [
      self.readline
      self.zlib
      self.openssl
      self.libxml2
    ];

    configureFlags = [
      "--libdir=$(lib)/lib"
      "--sysconfdir=/etc"
      "--with-libxml"
      "--with-openssl"
      "--with-system-tzdata=${self.tzdata}/share/zoneinfo"
      "--without-icu"
    ];
  });
in
{
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      "${compiler}" = super.haskell.packages."${compiler}".override (prev: {
        ghc = prev.ghc.override {
          enableDocs = false;
          enableHaddockProgram = false;
          enableProfiledLibs = false;
          enableRelocatedStaticLibs = true;
          enableShared = false;
          libffi = null;
        };

        overrides = self.lib.composeExtensions prev.overrides (self: super: {
          mkDerivation = args: super.mkDerivation (args // {
            doCheck = false;
            doHaddock = false;
            doHoogle = false;
            enableExecutableProfiling = false;
            enableLibraryProfiling = false;
          });
        });
      });
    };
  };

  postgresql_16 = minimizePostgresql super.postgresql_16;
  postgresql_15 = minimizePostgresql super.postgresql_15;
  postgresql_14 = minimizePostgresql super.postgresql_14;
  postgresql_13 = minimizePostgresql super.postgresql_13;
  postgresql_12 = minimizePostgresql super.postgresql_12;
  postgresql_11 = minimizePostgresql super.postgresql_11;
}
