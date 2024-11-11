{ buildToolbox
, cabal-install
, checkedShellScript
, devCabalOptions
, postgrest
}:
let
  build =
    checkedShellScript
      {
        name = "postgrest-build";
        docs = "Build PostgREST interactively using cabal-install.";
        args = [ "ARG_LEFTOVERS([Cabal arguments])" ];
        workingDir = "/";
        withEnv = postgrest.env;
      }
      ''
        ${cabal-install}/bin/cabal v2-update
        exec ${cabal-install}/bin/cabal v2-build ${devCabalOptions} "''${_arg_leftovers[@]}"
      '';

  clean =
    checkedShellScript
      {
        name = "postgrest-clean";
        docs = "Clean the PostgREST project, including all cabal-install artifacts.";
        workingDir = "/";
      }
      ''
        # clean old coverage data, too
        rm -rf .hpc coverage
        # clean old hie files
        find . -name "*.hie" -type f -delete
        exec ${cabal-install}/bin/cabal v2-clean
      '';

  run =
    checkedShellScript
      {
        name = "postgrest-run";
        docs = "Run PostgREST after building it interactively with cabal-install";
        args =
          [
            "ARG_USE_ENV([PGRST_DB_ANON_ROLE], [postgrest_test_anonymous], [PostgREST anonymous role])"
            "ARG_USE_ENV([PGRST_DB_POOL], [1], [PostgREST pool size])"
            "ARG_USE_ENV([PGRST_DB_POOL_ACQUISITION_TIMEOUT], [1], [PostgREST pool timeout])"
            "ARG_USE_ENV([PGRST_JWT_SECRET], [reallyreallyreallyreallyverysafe], [PostgREST JWT secret])"
            "ARG_LEFTOVERS([PostgREST arguments])"
          ];
        workingDir = "/";
        withEnv = postgrest.env;
      }
      ''
        export PGRST_DB_ANON_ROLE
        export PGRST_DB_POOL
        export PGRST_DB_POOL_ACQUISITION_TIMEOUT
        export PGRST_JWT_SECRET

        exec ${cabal-install}/bin/cabal v2-run ${devCabalOptions} --verbose=0 -- \
          postgrest "''${_arg_leftovers[@]}"
      '';

  repl =
    checkedShellScript
      {
        name = "postgrest-repl";
        docs = "Interact with PostgREST modules using the cabal repl";
        args = [ "ARG_LEFTOVERS([cabal v2-repl arguments])" ];
        workingDir = "/";
        withEnv = postgrest.env;
      }
      ''
        exec ${cabal-install}/bin/cabal v2-repl "''${_arg_leftovers[@]}"
      '';
in
buildToolbox
{
  name = "postgrest-cabal";
  tools = {
    inherit
      build
      clean
      run
      repl;
  };
}
