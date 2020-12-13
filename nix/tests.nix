# Utilities for running the PostgREST test suite

{ buildEnv
, cabal-install
, checkedShellScript
, curl
, devCabalOptions
, diffutils
, git
, haskell
, lib
, postgresql
, postgresqlVersions
, postgrest
, postgrestStatic
, postgrestProfiled
, procps
, python3
, runtimeShell
}:
let
  # Wrap the `test/with_tmp_db` script with the required dependencies from Nix.
  withTmpDb =
    postgresql:
    checkedShellScript "postgrest-test-${postgresql.name}"
      ''
        export PATH=${postgresql}/bin:${git}/bin:${runtimeShell}/bin:"$PATH"

        exec ${../test/with_tmp_db} "$@"
      '';

  # Script to run the Haskell test suite against a specific version of
  # PostgreSQL.
  testSpec =
    name: postgresql:
    checkedShellScript
      name
      ''
        env="$(cat ${postgrest.env})"
        export PATH="$env/bin:$PATH"

        cat << EOF

        Running spec against ${postgresql.name}...

        EOF

        trap 'echo "Failed on ${postgresql.name}"' exit

        ${withTmpDb postgresql} ${cabal-install}/bin/cabal v2-test ${devCabalOptions}

        trap "" exit

        cat << EOF

        Done running spec against ${postgresql.name}.

        EOF
      '';

  # Create a `testSpec` for each PostgreSQL version that we want to test
  # against.
  testSpecVersions =
    builtins.map
      ({ name, postgresql }:
        (testSpec "postgrest-test-spec-${name}" postgresql).bin)
      postgresqlVersions;

  # Helper script for running the tests against all PostgreSQL versions.
  testSpecAllVersions =
    let
      testRunners =
        map (test: "${test}/bin/${test.name}") testSpecVersions;
    in
    checkedShellScript "postgrest-test-spec-all"
      (lib.concatStringsSep "\n" testRunners);

  ioTestPython =
    python3.withPackages (ps: [
      ps.pytest
      ps.requests
      ps.requests-unixsocket
      ps.pyjwt
      ps.pyyaml
    ]);

  testIO =
    name: postgresql:
    checkedShellScript
      name
      ''
        env="$(cat ${postgrest.env})"
        export PATH="$env/bin:$PATH"

        rootdir="$(${git}/bin/git rev-parse --show-toplevel)"
        cd "$rootdir"

        ${cabal-install}/bin/cabal v2-build ${devCabalOptions}
        ${cabal-install}/bin/cabal v2-exec ${withTmpDb postgresql} \
          ${ioTestPython}/bin/pytest -- -v "$rootdir"/test/io-tests "$@"
      '';

  testMemory =
    name: postgresql:
    checkedShellScript
      name
      ''
        rootdir="$(${git}/bin/git rev-parse --show-toplevel)"
        cd "$rootdir"

        export PATH="${postgrestProfiled}/bin:${curl}/bin:$PATH"

        ${withTmpDb postgresql} "$rootdir/test/memory-tests.sh"
      '';
in
# Create an environment that contains all the utility scripts for running tests
  # that we defined above.
buildEnv
  {
    name =
      "postgrest-tests";

    paths =
      [
        (testSpec "postgrest-test-spec" postgresql).bin
        testSpecAllVersions.bin
        (testIO "postgrest-test-io" postgresql).bin
      ] ++ testSpecVersions;
  }
  # The memory tests have large dependencies (a profiled build of PostgREST)
  # and are run less often than the spec tests, so we don't include them in
  # the default test environment. We make them available through a separate attribute:
  // {
  memoryTests =
    (testMemory "postgrest-test-memory" postgresql).bin;
}
