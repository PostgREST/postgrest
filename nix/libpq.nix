# Creating a separate libpq package is is discussed in
# https://github.com/NixOS/nixpkgs/issues/61580, but nixpkgs has not moved
# forward, yet.
# This package is passed to postgresql-libpq (haskell) which needs to be
# cross-compiled to the static build and possibly other architectures as
# as well. To reduce the number of dependencies that need to be built with
# it, this derivation focuses on building the client libraries only. No
# server, no tests.
{ stdenv
, lib
, openssl
, zlib
, postgresql
, pkg-config
, tzdata
}:

stdenv.mkDerivation {
  pname = "libpq";
  inherit (postgresql) src version patches;

  __structuredAttrs = true;
  env.CFLAGS = "-fdata-sections -ffunction-sections"
    + (if stdenv.cc.isClang then " -flto" else " -fmerge-constants -Wl,--gc-sections");

  configureFlags = [
    "--without-gssapi"
    "--without-icu"
    "--without-readline"
    "--with-openssl"
    "--with-system-tzdata=${tzdata}/share/zoneinfo"
    "--sysconfdir=/etc/postgresql"
  ];

  nativeBuildInputs = [ pkg-config tzdata ];
  buildInputs = [ openssl zlib ];

  buildFlags = [ "submake-libpq" "submake-libpgport" ];

  installPhase = ''
    runHook preInstall

    make -C src/bin/pg_config install
    make -C src/common install
    make -C src/include install
    make -C src/interfaces/libpq install
    make -C src/port install

    rm -rfv $out/share

    runHook postInstall
  '';

  outputs = [ "out" ];

  meta = with lib; {
    homepage = "https://www.postgresql.org";
    description = "Client API library for PostgreSQL";
    license = licenses.postgresql;
  };
}
