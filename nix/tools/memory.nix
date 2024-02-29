# The memory tests have large dependencies (a profiled build of PostgREST)
# and are run less often than the spec tests, so we don't include them in
# the default test environment. We make them available through a separate module.
# TODO both of these require reentering the nix-shell if you make a change to the code
{ buildToolbox
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
        workingDir = "/";
        withPath = [ postgrestProfiled curl ];
      }
      ''
        ${withTools.withPg} -f test/spec/fixtures/load.sql test/memory/memory-tests.sh
      '';

  runProfiled =
    checkedShellScript
      {
        name = "postgrest-profiled-run";
        docs = "Run a profiled build of postgREST. This will generate a postgrest.prof file that can be used to do optimization. Note: if you make a change to the code, you must reenter the nix-shell for an updated profiled build.";
        args =
          [
            "ARG_USE_ENV([PGRST_DB_ANON_ROLE], [postgrest_test_anonymous], [PostgREST anonymous role])"
            "ARG_USE_ENV([PGRST_DB_POOL], [1], [PostgREST pool size])"
            "ARG_USE_ENV([PGRST_DB_POOL_ACQUISITION_TIMEOUT], [1], [PostgREST pool timeout])"
            "ARG_LEFTOVERS([PostgREST arguments])"
          ];
        workingDir = "/";
        withPath = [ postgrestProfiled ];
      }
      ''
        export PGRST_DB_ANON_ROLE
        export PGRST_DB_POOL
        export PGRST_DB_POOL_ACQUISITION_TIMEOUT

        postgrest +RTS -p -h -RTS "''${_arg_leftovers[@]}"
      '';
in
buildToolbox
{
  name = "postgrest-memory";
  tools = { inherit test runProfiled; };
}
