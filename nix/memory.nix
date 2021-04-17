# The memory tests have large dependencies (a profiled build of PostgREST)
# and are run less often than the spec tests, so we don't include them in
# the default test environment. We make them available through a separate module.
{ buildEnv
, checkedShellScript
, curl
, postgrestProfiled
, withTools
}:
let
  test =
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

in
buildEnv
  {
    name = "postgrest-memory";

    paths = [ test.bin ];
  } // { bashCompletion = test.bashCompletion; }
