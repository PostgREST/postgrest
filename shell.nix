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
in
lib.overrideDerivation postgrest.env (
  base: {
    buildInputs =
      base.buildInputs ++ [
        pkgs.cabal-install
        pkgs.cabal2nix
        pkgs.postgresql
        postgrest.devtools
        postgrest.hsie.bin
        postgrest.nixpkgsUpgrade
        postgrest.style
        postgrest.tests
        postgrest.withTools
      ]
      ++ lib.optional memoryTests postgrest.tests.memoryTests
      ++ lib.optional docker postgrest.docker
      ++ lib.optional release postgrest.release;

    shellHook =
      ''
        source ${pkgs.bashCompletion}/etc/profile.d/bash_completion.sh
        complete -F _command postgrest-watch
        complete -F _command postgrest-with-all
        complete -F _command postgrest-with-postgresql-13
        complete -F _command postgrest-with-postgresql-12
        complete -F _command postgrest-with-postgresql-11
        complete -F _command postgrest-with-postgresql-10
        complete -F _command postgrest-with-postgresql-9.6
        complete -F _command postgrest-with-postgresql-9.5
        source ${postgrest.hsie.bashCompletion}
      '';
  }
)
