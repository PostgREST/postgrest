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
, yq
}:
let
  testSpec =
    checkedShellScript
      {
        name = "postgrest-test-spec";
        docs = "Run the Haskell test suite. Use --match PATTERN for running individual specs";
        args = [ "ARG_LEFTOVERS([hspec arguments])" ];
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        ${withTools.withPg} ${cabal-install}/bin/cabal v2-run ${devCabalOptions} \
          test:spec -- "''${_arg_leftovers[@]}"
      '';

  testQuerycost =
    checkedShellScript
      {
        name = "postgrest-test-querycost";
        docs = "Run the Haskell test suite for query costs";
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        ${withTools.withPg} ${cabal-install}/bin/cabal v2-run ${devCabalOptions} test:querycost
      '';

  testDoctests =
    checkedShellScript
      {
        name = "postgrest-test-doctests";
        docs = "Run the Haskell doctest test suite";
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        # For unknown reasons, doctests uses the wrong GHC package database outside
        # nix-shell and fails, so we set the package path explicitly
        #ghcWithPackages="$(cat ${postgrest.env})"
        #ghcVersion="$(ls "$ghcWithPackages/lib")"
        #export GHC_PACKAGE_PATH="$ghcWithPackages/lib/$ghcVersion/package.conf.d/"

        ${cabal-install}/bin/cabal v2-run ${devCabalOptions} test:doctests
      '';

  testSpecIdempotence =
    checkedShellScript
      {
        name = "postgrest-test-spec-idempotence";
        docs = "Check that the Haskell tests can be run multiple times against the same db.";
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        ${withTools.withPg} ${runtimeShell} -c " \
          ${cabal-install}/bin/cabal v2-run ${devCabalOptions} test:spec && \
          ${cabal-install}/bin/cabal v2-run ${devCabalOptions} test:spec"
      '';

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
    checkedShellScript
      {
        name = "postgrest-test-io";
        docs = "Run the pytest-based IO tests. Add -k to run tests that match a given expression.";
        args = [ "ARG_LEFTOVERS([pytest arguments])" ];
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        ${cabal-install}/bin/cabal v2-build ${devCabalOptions}
        ${cabal-install}/bin/cabal v2-exec -- ${withTools.withPg} -f test/io/fixtures.sql \
          ${ioTestPython}/bin/pytest -v test/io "''${_arg_leftovers[@]}"
      '';

  dumpSchema =
    checkedShellScript
      {
        name = "postgrest-dump-schema";
        docs = "Dump the loaded schema's SchemaCache as a yaml file.";
        inRootDir = true;
        withEnv = postgrest.env;
        withPath = [ jq ];
      }
      ''
        ${withTools.withPg} \
            ${cabal-install}/bin/cabal v2-run ${devCabalOptions} --verbose=0 -- \
            postgrest --dump-schema \
            | ${yq}/bin/yq -y .
      '';

  coverage =
    checkedShellScript
      {
        name = "postgrest-coverage";
        docs = "Run spec and io tests while collecting hpc coverage data. First runs weeder to detect dead code.";
        args = [ "ARG_LEFTOVERS([hpc report arguments])" ];
        inRootDir = true;
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
          ${cabal-install}/bin/cabal v2-build ${devCabalOptions} exe:postgrest lib:postgrest test:spec test:querycost

          (
            trap 'echo Found dead code: Check file list above.' ERR ;
            ${weeder}/bin/weeder --config=./test/weeder.dhall
          )

          # collect all tests
          HPCTIXFILE="$tmpdir"/io.tix \
            ${withTools.withPg} -f test/io/fixtures.sql ${cabal-install}/bin/cabal v2-exec ${devCabalOptions} -- \
            ${ioTestPython}/bin/pytest -v test/io

          HPCTIXFILE="$tmpdir"/spec.tix \
            ${withTools.withPg} ${cabal-install}/bin/cabal v2-run ${devCabalOptions} test:spec

          HPCTIXFILE="$tmpdir"/querycost.tix \
            ${withTools.withPg} ${cabal-install}/bin/cabal v2-run ${devCabalOptions} test:querycost

          # Note: No coverage for doctests, as doctests leverage GHCi and GHCi does not support hpc

          # collect all the tix files
          ${ghc}/bin/hpc sum  --union --exclude=Paths_postgrest --output="$tmpdir"/tests.tix \
            "$tmpdir"/io*.tix "$tmpdir"/spec.tix "$tmpdir"/querycost.tix

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
            echo "file://$(pwd)/coverage/check/hpc_index.html"
            exit 1
          else
            # copy the result .tix file to the coverage/ dir to make it available to postgrest-coverage-draft-overlay, too
            cp "$tmpdir"/tests-overlay.tix coverage/postgrest.tix
            # prepare codecov json report
            ${hpc-codecov}/bin/hpc-codecov --mix=.hpc --out=coverage/codecov.json coverage/postgrest.tix

            # create html and stdout reports
            ${ghc}/bin/hpc markup --destdir=coverage coverage/postgrest.tix
            echo "file://$(pwd)/coverage/hpc_index.html"
            ${ghc}/bin/hpc report coverage/postgrest.tix "''${_arg_leftovers[@]}"
          fi
        ''
      );

  coverageDraftOverlay =
    checkedShellScript
      {
        name = "postgrest-coverage-draft-overlay";
        docs = "Create a draft overlay from current coverage report.";
        inRootDir = true;
      }
      ''
        ${ghc}/bin/hpc draft --output=test/coverage.overlay coverage/postgrest.tix
        sed -i 's|^module \(.*\):|module \1/|g' test/coverage.overlay
      '';

  checkStatic =
    checkedShellScript
      {
        name = "postgrest-check-static";
        docs = "Verify that the argument is a static executable.";
        args = [ "ARG_POSITIONAL_SINGLE([executable], [Executable])" ];
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        exe="$_arg_executable"
        ldd_output=$(ldd "$exe" 2>&1 || true)
        if ! grep -q "not a dynamic executable" <<< "$ldd_output"; then
          echo "not a static executable, ldd output:"
          echo "$ldd_output"
          exit 1
        fi
        "$exe" --help
      '';

in
buildToolbox
{
  name = "postgrest-tests";
  tools =
    [
      testSpec
      testQuerycost
      testDoctests
      testSpecIdempotence
      testIO
      dumpSchema
      coverage
      coverageDraftOverlay
      checkStatic
    ];
}
