{ buildEnv
, cabal-install
, checkedShellScript
, devCabalOptions
, entr
, graphviz
, silver-searcher
, style
, tests
}:
let
  watch =
    checkedShellScript
      {
        name = "postgrest-watch";
        docs =
          ''
            Watch the project for changes and reinvoke the given command.

            Example:

              postgrest-watch postgrest-test-io

          '';
        inRootDir = true;
      }
      ''
        while true; do
          (! ${silver-searcher}/bin/ag -l . | ${entr}/bin/entr -dr "$@")
        done
      '';

  pushCachix =
    checkedShellScript
      {
        name = "postgrest-push-cachix";
        docs = ''
          Push all build artifacts to cachix.

          Requires that the CACHIX_SIGNING_KEY for postgrest is set as an
          environment variable.
        '';
        inRootDir = true;
      }
      ''
        nix-store -qR --include-outputs "$(nix-instantiate)" | cachix push postgrest
      '';

  build =
    checkedShellScript
      {
        name = "postgrest-build";
        docs = "Build PostgREST interactively using cabal-install.";
        inRootDir = true;
      }
      ''exec ${cabal-install}/bin/cabal v2-build ${devCabalOptions} "$@"'';

  run =
    checkedShellScript
      {
        name = "postgrest-run";
        docs = "Run PostgREST after buidling it interactively with cabal-install";
        inRootDir = true;
      }
      ''exec ${cabal-install}/bin/cabal v2-run ${devCabalOptions} --verbose=0 -- postgrest "$@"'';

  clean =
    checkedShellScript
      {
        name = "postgrest-clean";
        docs = "Clean the PostgREST project, including all cabal-install artifacts.";
        inRootDir = true;
      }
      ''
        ${cabal-install}/bin/cabal v2-clean
        # clean old coverage data, too
        rm -rf .hpc coverage
      '';

  check =
    checkedShellScript
      {
        name = "postgrest-check";
        docs =
          ''
            Run most checks that will also run on CI.

            This currently excludes the memory tests, as those are particularly
            expensive.
          '';
        inRootDir = true;
      }
      ''
        ${tests}/bin/postgrest-with-all ${tests}/bin/postgrest-test-spec
        ${tests}/bin/postgrest-test-spec-idempotence
        ${tests}/bin/postgrest-test-io
        ${style}/bin/postgrest-lint
        ${style}/bin/postgrest-style-check
      '';

  importsgraph =
    checkedShellScript
      {
        name = "postgrest-importsgraph";
        docs =
          ''
            Render the imports between PostgREST modules as a graph.

            Output is written to 'imports.png' in the root directory of the project.
          '';
        inRootDir = true;
      }
      ''
        grep -rE 'import .*PostgREST\.' src \
          | sed -E \
              -e 's|/|\.|g' \
              -e 's/src\.(.*)\.hs:import .*(PostgREST\.\S+)( .*)?/"\1" -> "\2"/' \
              -e '1 i digraph \{' \
              -e '$ a \}' \
          | ${graphviz}/bin/dot -Tpng \
          > imports.png
      '';
in
buildEnv {
  name = "postgrest-devtools";
  paths = [
    watch.bin
    pushCachix.bin
    build.bin
    run.bin
    clean.bin
    check.bin
    importsgraph.bin
  ];
}
