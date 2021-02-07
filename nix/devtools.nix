{ buildEnv
, cabal-install
, checkedShellScript
, devCabalOptions
, entr
, graphviz
, silver-searcher
, style
, tests
, writeText
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
        imports=$(
          grep -rE 'import .*PostgREST\.' src main \
            | sed -E \
                -e 's|/|\.|g' \
                -e 's/(src|main)\.(.*)\.hs:import .*(PostgREST\.\S+)( .*)?/"\2" -> "\3"/'
        )

        labels=$(
          grep -rE '^(--)?\s*Description\s*:' src main \
            | sed -E \
                -e 's|/|\.|g' \
                -e 's/^(src|main)\.(.*)\.hs:(--)?\s*Description\s*:\s*(.*)$/"\2" [label="\2\\n\4"]/'
        )

      cat <<EOF | ${graphviz}/bin/dot -Tpng > imports.png
        digraph {
          $imports
          $labels
        }
      EOF
      '';

  symbolimportsgraph =
    checkedShellScript
      {
        name = "postgrest-symbolimportsgraph";
        docs =
          ''
            Render the imports of symbols between PostgREST modules as a graph.

            Output is written to 'symbolimports.png' in the root directory of the project.
          '';
        inRootDir = true;
      }
      ''
        tmpdir="$(mktemp -d)"
        ${cabal-install}/bin/cabal v2-clean
        ${cabal-install}/bin/cabal v2-build ${devCabalOptions} \
          --ghc-option=-ddump-minimal-imports --ghc-option=-dumpdir="$tmpdir"

        symbols=$(
          for importsfile in "$tmpdir"/*.imports; do
            filename="$(basename "$importsfile")"
            modulename="''${filename%.*}"
            sed -Ef ${symbolImportsSed} < "$importsfile" \
              | sed -Ee '/^\S/{h;d};G' -e 's/ {6}(.*)\n(.*)/\2\:\1/' \
              | sed -Ee "s/^/$modulename:/"
          done
        )

        rm -r "$tmpdir"

        pgrstSymbols=$(
          echo "$symbols" | grep -E '^.*:PostgREST\..*:.*$' | sort
        )

        targetModules=$(
          echo "$pgrstSymbols" | sed -Ee 's/^(.*):(.*):(.*)$/\2/' | uniq
        )

        groups=$(
          for module in $targetModules; do
            echo "$pgrstSymbols" \
              | grep -E "^.*:$module.*:.*$" \
              | sed -E \
                  -e 's/.*:(.*):(.*)/  "\1.\2"/' \
                  -e "1 i subgraph \"cluster_$module\" {\n  \"$module\"" \
                  -e '$ a }'
          done
        )

        edges=$(
          echo "$pgrstSymbols" | sed -Ee 's/^(.*):(.*):(.*)$/"\1" -> "\2\.\3"/'
        )

        cat <<EOF | ${graphviz}/bin/dot -Tpng > "symbolimports.png"
          digraph {
            rankdir=LR
            ranksep=10
            $edges
            $groups
          }
        EOF
      '';

  symbolImportsSed =
    writeText "symbolimports.sed"
      ''
        # Break single-line imports into a new line, removing the paren and with indentation
        s/(import .*) \( /\1\n      /
        # Remove the opening paren of multi-line imports, maintaining a consistent indent
        s/    \( /      /
        # Remove all closing parens from import statements
        s/ \)//
        # Remove all imported constructors (might be '(..)' or an explicit list)
        s/(\w+)\(.*\)/\1/
        # Break all one-liner import lists into multiple lines, maintaining indentation
        s/, /\n      /g
        # Remove commas at the end multi-line import lines
        s/,$//
        # Filter the module name from import statements
        s/^import( qualified)? (\S+)( as \S)?/\2/
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
    symbolimportsgraph.bin
  ];
}
