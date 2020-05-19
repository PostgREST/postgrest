# Utilities for running the PostgREST test suite

{ buildEnv
, cabal-install
, git
, lib
, postgrestBuildEnv
, postgresql
, postgresqlVersions
, runtimeShell
, writeShellScript
, writeShellScriptBin
}:
let
  # Wrap the `test/with_tmp_db` script with the required dependencies from Nix.
  withTmpDb =
    postgresql:
      writeShellScript "postgrest-test-${postgresql.name}"
        ''
          set -euo pipefail

          export PATH=${postgresql}/bin:${git}/bin:${runtimeShell}/bin:"$PATH"

          exec ${../test/with_tmp_db} "$@"
        '';

  # Script to run the Haskell test suite against a specific version of
  # PostgreSQL.
  testSpec =
    name: postgresql:
      writeShellScriptBin name
        ''
          set -euo pipefail

          cat << EOF

          Running spec against ${postgresql.name}...

          EOF

          # TODO: Make this work outside nix-shell when installed with nix-env.
          # Probably using postgrestBuildEnv somehow?
          ${withTmpDb postgresql} ${cabal-install}/bin/cabal v2-test \
            --test-show-detail=direct

          cat << EOF

          Done running spec against ${postgresql.name}.

          EOF
        '';

  defaultTestSpec =
    testSpec "postgrest-test-spec" postgresql;

  # Create a `testSpec` for each PostgreSQL version that we want to test
  # against.
  testSpecVersions =
    lib.mapAttrsToList
      (name: postgresql: testSpec "postgrest-test-spec-${name}" postgresql)
      postgresqlVersions;

  # Helper script for running the tests against all PostgreSQL versions.
  testSpecAllVersions =
    let
      testRunners =
        map (test: "${test}/bin/${test.name}") testSpecVersions;
    in
      writeShellScriptBin "postgrest-test-spec-all"
        ''
          set -euo pipefail

          ${lib.concatStringsSep "\n" testRunners}
        '';
in
  # Create an environment that contains all the utility scripts for running tests
  # that we defined above.
buildEnv {
  name =
    "postgrest-tests";

  paths =
    [
      defaultTestSpec
      testSpecAllVersions
    ] ++ testSpecVersions;
}
