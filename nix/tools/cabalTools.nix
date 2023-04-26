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
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        exec ${cabal-install}/bin/cabal v2-build ${devCabalOptions} "''${_arg_leftovers[@]}"
      '';

  clean =
    checkedShellScript
      {
        name = "postgrest-clean";
        docs = "Clean the PostgREST project, including all cabal-install artifacts.";
        inRootDir = true;
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
            "ARG_LEFTOVERS([PostgREST arguments])"
          ];
        inRootDir = true;
        withEnv = postgrest.env;
      }
      ''
        export PGRST_DB_ANON_ROLE

        exec ${cabal-install}/bin/cabal v2-run ${devCabalOptions} --verbose=0 -- \
          postgrest "''${_arg_leftovers[@]}"
      '';

in
buildToolbox
{
  name = "postgrest-cabal";
  tools = [
    build
    clean
    run
  ];
}
