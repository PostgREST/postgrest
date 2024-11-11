{ buildToolbox
, cabal-install
, checkedShellScript
, devCabalOptions
, ghc
, glibcLocales ? null
, gnugrep
, hpc-codecov
, hostPlatform
, jq
, lib
, postgrest
, python3
, runtimeShell
, stdenv
, weeder
, withTools
}:
let
  testSpec =
    checkedShellScript
      {
        name = "postgrest-test-spec";
        docs = "Run the Haskell test suite. Use --match PATTERN for running individual specs";
        args = [ "ARG_LEFTOVERS([hspec arguments])" ];
        workingDir = "/";
        withEnv = postgrest.env;
      }
      ''
        ${cabal-install}/bin/cabal v2-update
        ${withTools.withPg} -f test/spec/fixtures/load.sql \
          ${cabal-install}/bin/cabal v2-run ${devCabalOptions} test:spec -- "''${_arg_leftovers[@]}"
      '';

  testDoctests =
    checkedShellScript
      {
        name = "postgrest-test-doctests";
        docs = "Run the Haskell doctest test suite";
        workingDir = "/";
        withEnv = postgrest.env;
      }
      ''
        ${cabal-install}/bin/cabal v2-update
        # This makes nix-env -iA tests.doctests.bin work.
        export NIX_GHC=${postgrest.env.NIX_GHC}
        ${cabal-install}/bin/cabal v2-run ${devCabalOptions} test:doctests
      '';

  testSpecIdempotence =
    checkedShellScript
      {
        name = "postgrest-test-spec-idempotence";
        docs = "Check that the Haskell tests can be run multiple times against the same db.";
        workingDir = "/";
        withEnv = postgrest.env;
      }
      ''
        ${cabal-install}/bin/cabal v2-update
        ${withTools.withPg} -f test/spec/fixtures/load.sql \
          ${runtimeShell} -c " \
            ${cabal-install}/bin/cabal v2-run ${devCabalOptions} test:spec && \
            ${cabal-install}/bin/cabal v2-run ${devCabalOptions} test:spec"
      '';

  ioTestPython =
    python3.withPackages (ps: [
      ps.pyjwt
      ps.pytest
      ps.pytest-xdist
      ps.pyyaml
      ps.requests
      ps.requests-unixsocket
      ps.syrupy
    ]);

  testIO =
    checkedShellScript
      {
        name = "postgrest-test-io";
        docs = "Run the pytest-based IO tests. Add -k to run tests that match a given expression.";
        args = [ "ARG_LEFTOVERS([pytest arguments])" ];
        workingDir = "/";
        withEnv = postgrest.env;
      }
      ''
        ${cabal-install}/bin/cabal v2-update
        ${cabal-install}/bin/cabal v2-build ${devCabalOptions}
        ${cabal-install}/bin/cabal v2-exec -- ${withTools.withPg} -f test/io/fixtures.sql \
          ${ioTestPython}/bin/pytest --ignore=test/io/test_big_schema.py --ignore=test/io/test_replica.py -v test/io "''${_arg_leftovers[@]}"
      '';

  testBigSchema =
    checkedShellScript
      {
        name = "postgrest-test-big-schema";
        docs = "Run a pytest-based IO test on a big schema. Add -k to run tests that match a given expression.";
        args = [ "ARG_LEFTOVERS([pytest arguments])" ];
        workingDir = "/";
        withEnv = postgrest.env;
      }
      ''
        ${cabal-install}/bin/cabal v2-update
        ${cabal-install}/bin/cabal v2-build ${devCabalOptions}
        ${cabal-install}/bin/cabal v2-exec -- ${withTools.withPg} -f test/io/big_schema.sql \
          ${ioTestPython}/bin/pytest -v test/io/test_big_schema.py "''${_arg_leftovers[@]}"
      '';

  testReplica =
    checkedShellScript
      {
        name = "postgrest-test-replica";
        docs = "Run a pytest-based IO test on a replica. Add -k to run tests that match a given expression.";
        args = [ "ARG_LEFTOVERS([pytest arguments])" ];
        workingDir = "/";
        withEnv = postgrest.env;
      }
      ''
        ${cabal-install}/bin/cabal v2-update
        ${cabal-install}/bin/cabal v2-build ${devCabalOptions}
        ${cabal-install}/bin/cabal v2-exec -- ${withTools.withPg} --replica -f test/io/replica.sql \
          ${ioTestPython}/bin/pytest -v test/io/test_replica.py "''${_arg_leftovers[@]}"
      '';

  dumpSchema =
    checkedShellScript
      {
        name = "postgrest-dump-schema";
        docs = "Dump the loaded schema's SchemaCache in JSON format.";
        workingDir = "/";
        withEnv = postgrest.env;
        withPath = [ jq ];
      }
      ''
        ${cabal-install}/bin/cabal v2-update
        ${withTools.withPg} -f test/spec/fixtures/load.sql \
            ${cabal-install}/bin/cabal v2-run ${devCabalOptions} --verbose=0 -- \
            postgrest --dump-schema
      '';

  coverage =
    checkedShellScript
      {
        name = "postgrest-coverage";
        docs = "Run spec and io tests while collecting hpc coverage data. First runs weeder to detect dead code.";
        args = [ "ARG_LEFTOVERS([hpc report arguments])" ];
        workingDir = "/";
        redirectTixFiles = false;
        withEnv = postgrest.env;
        withTmpDir = true;
      }
      (
        # required for `hpc markup` in CI; glibcLocales is not available e.g. on Darwin
        lib.optionalString (stdenv.isLinux && hostPlatform.libc == "glibc") ''
          export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
        '' +

        ''
          # clean up previous coverage reports
          mkdir -p coverage
          rm -rf coverage/*

          # build once before running all the tests
          ${cabal-install}/bin/cabal v2-update
          ${cabal-install}/bin/cabal v2-build ${devCabalOptions} exe:postgrest lib:postgrest test:spec

          (
            trap 'echo Found dead code: Check file list above.' ERR ;
            ${weeder}/bin/weeder --config=./test/weeder.toml
          )

          # collect all tests
          HPCTIXFILE="$tmpdir"/io.tix \
            ${withTools.withPg} -f test/io/fixtures.sql \
            ${cabal-install}/bin/cabal v2-exec ${devCabalOptions} -- ${ioTestPython}/bin/pytest --ignore=test/io/test_big_schema.py --ignore=test/io/test_replica.py -v test/io

          HPCTIXFILE="$tmpdir"/big_schema.tix \
            ${withTools.withPg} -f test/io/big_schema.sql \
            ${cabal-install}/bin/cabal v2-exec ${devCabalOptions} -- ${ioTestPython}/bin/pytest -v test/io/test_big_schema.py

          HPCTIXFILE="$tmpdir"/replica.tix \
            ${withTools.withPg} --replica -f test/io/replica.sql \
            ${cabal-install}/bin/cabal v2-exec ${devCabalOptions} -- ${ioTestPython}/bin/pytest -v test/io/test_replica.py

          HPCTIXFILE="$tmpdir"/spec.tix \
            ${withTools.withPg} -f test/spec/fixtures/load.sql \
            ${cabal-install}/bin/cabal v2-run ${devCabalOptions} test:spec

          # Note: No coverage for doctests, as doctests leverage GHCi and GHCi does not support hpc

          # collect all the tix files
          ${ghc}/bin/hpc sum  --union --exclude=Paths_postgrest --output="$tmpdir"/tests.tix \
            "$tmpdir"/io*.tix "$tmpdir"/big_schema*.tix "$tmpdir"/replica*.tix "$tmpdir"/spec.tix

          # prepare the overlay
          ${ghc}/bin/hpc overlay --output="$tmpdir"/overlay.tix test/coverage.overlay
          ${ghc}/bin/hpc sum --union --output="$tmpdir"/tests-overlay.tix "$tmpdir"/tests.tix "$tmpdir"/overlay.tix

          # check nothing in the overlay is actually tested
          ${ghc}/bin/hpc map --function=inv --output="$tmpdir"/inverted.tix "$tmpdir"/tests.tix
          ${ghc}/bin/hpc combine --function=sub \
            --output="$tmpdir"/check.tix "$tmpdir"/overlay.tix "$tmpdir"/inverted.tix
          # returns zero exit code if any count="<non-zero>" lines are found, i.e.
          # something is covered by both the overlay and the tests
          if ${ghc}/bin/hpc report --xml "$tmpdir"/check.tix | ${gnugrep}/bin/grep -qP 'count="[^0]'
          then
            ${ghc}/bin/hpc markup --highlight-covered --destdir=coverage/overlay "$tmpdir"/overlay.tix || true
            ${ghc}/bin/hpc markup --highlight-covered --destdir=coverage/check "$tmpdir"/check.tix || true
            echo "ERROR: Something is covered by both the tests and the overlay:"
            echo "postgrest-coverage: To see the results, visit file://$(pwd)/coverage/check/hpc_index.html"
            exit 1
          else
            # copy the result .tix file to the coverage/ dir to make it available to postgrest-coverage-draft-overlay, too
            cp "$tmpdir"/tests-overlay.tix coverage/postgrest.tix
            # prepare codecov json report
            ${hpc-codecov}/bin/hpc-codecov --mix=.hpc --out=coverage/codecov.json coverage/postgrest.tix

            # create html and stdout reports
            ${ghc}/bin/hpc markup --destdir=coverage coverage/postgrest.tix
            echo "postgrest-coverage: To see the results, visit file://$(pwd)/coverage/hpc_index.html"
            ${ghc}/bin/hpc report coverage/postgrest.tix "''${_arg_leftovers[@]}"
          fi
        ''
      );

  coverageDraftOverlay =
    checkedShellScript
      {
        name = "postgrest-coverage-draft-overlay";
        docs = "Create a draft overlay from current coverage report.";
        workingDir = "/";
      }
      ''
        ${ghc}/bin/hpc draft --output=test/coverage.overlay coverage/postgrest.tix
        sed -i 's|^module \(.*\):|module \1/|g' test/coverage.overlay
      '';

in
buildToolbox
{
  name = "postgrest-tests";
  tools = {
    inherit
      testSpec
      testDoctests
      testSpecIdempotence
      testIO
      testBigSchema
      testReplica
      dumpSchema
      coverage
      coverageDraftOverlay;
  };
}
