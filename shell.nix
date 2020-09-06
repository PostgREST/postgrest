# The additional modules below have large dependencies and are therefore
# disabled by default. You can activate them by passing arguments to nix-shell,
# e.g.:
#
#    nix-shell --arg release true
#
# This will provide you with a shell where the `postgrest-release-*` scripts
# are available.
#
# We highly recommend that use the PostgREST binary cache by installing cachix
# (https://app.cachix.org/) and running `cachix use postgrest`.
{ ioTests ? false, memoryTests ? false, docker ? false, release ? false }:
let
  postgrest =
    import ./default.nix;

  pkgs =
    postgrest.pkgs;

  lib =
    pkgs.lib;
in
lib.overrideDerivation postgrest.env (
  base: {
    buildInputs =
      base.buildInputs ++ [
        pkgs.cabal-install
        pkgs.cabal2nix
        pkgs.postgresql_9_4
        postgrest.nixpkgsUpgrade
        postgrest.devtools
        postgrest.tests
      ]
      ++ lib.optional ioTests postgrest.tests.ioTests
      ++ lib.optional memoryTests postgrest.tests.memoryTests
      ++ lib.optional docker postgrest.docker
      ++ lib.optional release postgrest.release;
  }
)
