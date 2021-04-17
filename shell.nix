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
{ memoryTests ? false, docker ? false, release ? false }:
let
  postgrest =
    import ./default.nix;

  pkgs =
    postgrest.pkgs;

  lib =
    pkgs.lib;

  modules =
    [
      postgrest.devtools
      postgrest.style
      postgrest.tests
      postgrest.withTools
    ]
    ++ lib.optional release postgrest.release;

in
lib.overrideDerivation postgrest.env (
  base: {
    buildInputs =
      base.buildInputs ++ [
        pkgs.cabal-install
        pkgs.cabal2nix
        pkgs.postgresql
        postgrest.hsie.bin
        postgrest.nixpkgsUpgrade.bin
      ]
      ++ modules
      ++ lib.optional memoryTests postgrest.tests.memoryTests.bin
      ++ lib.optional docker postgrest.docker;

    shellHook =
      ''
        source ${pkgs.bashCompletion}/etc/profile.d/bash_completion.sh

      ''
      + builtins.concatStringsSep "\n" (
        builtins.map (bashCompletion: "source ${bashCompletion}") (
          builtins.concatLists (builtins.map (module: module.bashCompletion) modules)
          ++ [ postgrest.hsie.bashCompletion postgrest.nixpkgsUpgrade.bashCompletion ]
          ++ lib.optional memoryTests postgrest.tests.memoryTests.bashCompletion
          ++ lib.optional docker postgrest.docker.bashCompletion

        )
      );
  }
)
