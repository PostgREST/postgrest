# Utilities for running the PostgREST test suite

{ buildEnv
, cabal-install
, checkedShellScript
, curl
, devCabalOptions
, diffutils
, haskell
, lib
, postgresql
, postgresqlVersions
, postgrest
, postgrestProfiled
, postgrestStatic
, procps
, python3
, runtimeShell
, yq
}:
let
  # Wrap the `test/with_tmp_db` script with the required dependencies from Nix.
  withTmpDb =
    postgresql:
    checkedShellScript
      {
        name = "postgrest-test-withtmpdb-${postgresql.name}";
        docs = "Run the given command in a temporary database";
        inRootDir = true;
      }
      ''
        export PATH=${postgresql}/bin:"$PATH"

        exec test/with_tmp_db "$@"
      '';

  # Script to run the Haskell test suite against a specific version of
  # PostgreSQL.
  testSpec =
    name: postgresql:
    checkedShellScript
      {
        inherit name;
        docs = "Run the Haskell test suite against ${postgresql.name}.";
        inRootDir = true;
      }
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

  testSpecIdempotence =
    name: postgrestql:
    checkedShellScript
      {
        inherit name;
        docs = "Check that the Haskell tests can be run multiple times against the same db.";
        inRootDir = true;
      }
      ''
        env="$(cat ${postgrest.env})"
        export PATH="$env/bin:$PATH"

        ${withTmpDb postgresql} ${runtimeShell} -c " \
          ${cabal-install}/bin/cabal v2-test ${devCabalOptions} && \
          ${cabal-install}/bin/cabal v2-test ${devCabalOptions}"
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
    checkedShellScript
      {
        name = "postgrest-test-spec-all";
        docs = "Run the Haskell tests against all supported PostgreSQL versions.";
        inRootDir = true;
      }
      (lib.concatStringsSep "\n" testRunners);

  ioTestPython =
    python3.withPackages (ps: [
      ps.pyjwt
      ps.pytest
      ps.pytest_xdist
      ps.pyyaml
      ps.requests
      ps.requests-unixsocket
    ]);

  testIO =
    name: postgresql:
    checkedShellScript
      {
        inherit name;
        docs = "Run the pytest-based IO tests.";
        inRootDir = true;
      }
      ''
        env="$(cat ${postgrest.env})"
        export PATH="$env/bin:$PATH"

        ${cabal-install}/bin/cabal v2-build ${devCabalOptions}
        ${cabal-install}/bin/cabal v2-exec ${withTmpDb postgresql} \
          ${ioTestPython}/bin/pytest -- -v test/io-tests "$@"
      '';

  testMemory =
    name: postgresql:
    checkedShellScript
      {
        inherit name;
        docs = "Run the memory tests.";
        inRootDir = true;
      }
      ''
        export PATH="${postgrestProfiled}/bin:${curl}/bin:$PATH"

        ${withTmpDb postgresql} test/memory-tests.sh
      '';

  dumpSchema =
    name: postgresql:
    checkedShellScript
      {
        inherit name;
        docs = "Dump the loaded schema's DbStructure as a yaml file.";
        inRootDir = true;
      }
      ''
        env="$(cat ${postgrest.env})"
        export PATH="$env/bin:$PATH"

        ${withTmpDb postgresql} \
            ${cabal-install}/bin/cabal v2-run ${devCabalOptions} --verbose=0 -- \
            postgrest --dump-schema \
            | ${yq}/bin/yq -y .
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
        (testSpecIdempotence "postgrest-test-spec-idempotence" postgresql).bin
        testSpecAllVersions.bin
        (testIO "postgrest-test-io" postgresql).bin
        (dumpSchema "postgrest-dump-schema" postgresql).bin
      ] ++ testSpecVersions;
  }
  # The memory tests have large dependencies (a profiled build of PostgREST)
  # and are run less often than the spec tests, so we don't include them in
  # the default test environment. We make them available through a separate attribute:
  // {
  memoryTests =
    (testMemory "postgrest-test-memory" postgresql).bin;
}
