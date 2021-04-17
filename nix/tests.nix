# Utilities for running the PostgREST test suite

{ buildEnv
, cabal-install
, checkedShellScript
, curl
, devCabalOptions
, ghc
, glibcLocales
, gnugrep
, haskell
, hpc-codecov
, postgrest
, postgrestProfiled
, python3
, runtimeShell
, withTools
, yq
}:
let
  testSpec =
    checkedShellScript
      {
        name = "postgrest-test-spec";
        docs = "Run the Haskell test suite";
        inRootDir = true;
      }
      ''
        env="$(cat ${postgrest.env})"
        export PATH="$env/bin:$PATH"

        ${withTools.latest} ${cabal-install}/bin/cabal v2-test ${devCabalOptions}
      '';

  testSpecIdempotence =
    checkedShellScript
      {
        name = "postgrest-test-spec-idempotence";
        docs = "Check that the Haskell tests can be run multiple times against the same db.";
        inRootDir = true;
      }
      ''
        env="$(cat ${postgrest.env})"
        export PATH="$env/bin:$PATH"

        ${withTools.latest} ${runtimeShell} -c " \
          ${cabal-install}/bin/cabal v2-test ${devCabalOptions} && \
          ${cabal-install}/bin/cabal v2-test ${devCabalOptions}"
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
        docs = "Run the pytest-based IO tests.";
        inRootDir = true;
      }
      ''
        env="$(cat ${postgrest.env})"
        export PATH="$env/bin:$PATH"

        ${cabal-install}/bin/cabal v2-build ${devCabalOptions}
        ${cabal-install}/bin/cabal v2-exec ${withTools.latest} \
          ${ioTestPython}/bin/pytest -- -v test/io-tests "$@"
      '';

  testMemory =
    checkedShellScript
      {
        name = "postgrest-test-memory";
        docs = "Run the memory tests.";
        inRootDir = true;
      }
      ''
        export PATH="${postgrestProfiled}/bin:${curl}/bin:$PATH"

        ${withTools.latest} test/memory-tests.sh
      '';

  dumpSchema =
    checkedShellScript
      {
        name = "postgrest-dump-schema";
        docs = "Dump the loaded schema's DbStructure as a yaml file.";
        inRootDir = true;
      }
      ''
        env="$(cat ${postgrest.env})"
        export PATH="$env/bin:$PATH"

        ${withTools.latest} \
            ${cabal-install}/bin/cabal v2-run ${devCabalOptions} --verbose=0 -- \
            postgrest --dump-schema \
            | ${yq}/bin/yq -y .
      '';

  coverage =
    checkedShellScript
      {
        name = "postgrest-coverage";
        docs = "Run spec and io tests while collecting hpc coverage data.";
        inRootDir = true;
        redirectTixFiles = false;
        withTmpDir = true;
      }
      ''
        env="$(cat ${postgrest.env})"
        export PATH="$env/bin:$PATH"
        export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"

        # clean up previous coverage reports
        mkdir -p coverage
        rm -rf coverage/*

        # build once before running all the tests
        ${cabal-install}/bin/cabal v2-build ${devCabalOptions} --enable-tests all

        # collect all tests
        HPCTIXFILE="$tmpdir"/io.tix \
        ${withTools.latest} ${cabal-install}/bin/cabal v2-exec ${devCabalOptions} \
          ${ioTestPython}/bin/pytest -- -v test/io-tests
          
        HPCTIXFILE="$tmpdir"/spec.tix \
        ${withTools.latest} ${cabal-install}/bin/cabal v2-test ${devCabalOptions}

        # collect all the tix files
        ${ghc}/bin/hpc sum  --union --exclude=Paths_postgrest --output="$tmpdir"/tests.tix "$tmpdir"/io*.tix "$tmpdir"/spec.tix

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
          ${ghc}/bin/hpc report coverage/postgrest.tix "$@"
        fi
      '';

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

in
# Create an environment that contains all the utility scripts for running tests
  # that we defined above.
buildEnv
  {
    name =
      "postgrest-tests";

    paths =
      [
        testSpec.bin
        testSpecIdempotence.bin
        testIO.bin
        dumpSchema.bin
        coverage.bin
        coverageDraftOverlay.bin
      ];
  }
  # The memory tests have large dependencies (a profiled build of PostgREST)
  # and are run less often than the spec tests, so we don't include them in
  # the default test environment. We make them available through a separate attribute:
  // {
  memoryTests = testMemory.bin;
}
