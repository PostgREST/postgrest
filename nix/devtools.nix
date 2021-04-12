{ buildEnv
, cabal-install
, checkedShellScript
, devCabalOptions
, entr
, graphviz
, hsie
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
        redirectTixFiles = false; # will be done by sub-command
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

  dumpMinimalImports =
    checkedShellScript
      {
        name = "postgrest-dump-minimal-imports";
        docs = "Dump minimal imports into given directory.";
        inRootDir = true;
        withTmpDir = true;
      }
      ''
        dumpdir="''${1:?dumpdir not set}"
        mkdir -p "$dumpdir"
        ${cabal-install}/bin/cabal v2-build ${devCabalOptions} \
          --builddir="$tmpdir" \
          --ghc-option=-ddump-minimal-imports \
          --ghc-option=-dumpdir="$dumpdir" \
          1>&2

        # Fix OverloadedRecordFields imports
        # shellcheck disable=SC2016
        sed -E 's/\$sel:.*://g' -i "$dumpdir"/*
      '';

  hsieMinimalImports =
    checkedShellScript
      {
        name = "postgrest-hsie-minimal-imports";
        docs = "Run hsie with a provided dump of minimal imports.";
        withTmpDir = true;
      }
      ''
        ${dumpMinimalImports} "$tmpdir"
        ${hsie} "$tmpdir" "$@"
      '';

  hsieGraphModules =
    checkedShellScript
      {
        name = "postgrest-hsie-graph-modules";
        docs = "Create a PNG graph of modules imported within the codebase.";
      }
      ''
        outfile="''${1:?outfile not set}"
        ${hsie} graph-modules main src | ${graphviz}/bin/dot -Tpng -o "$outfile"
      '';

  hsieGraphSymbols =
    checkedShellScript
      {
        name = "postgrest-hsie-graph-symbols";
        docs = "Create a PNG graph of symbols imported within the codebase.";
      }
      ''
        outfile="''${1:?outfile not set}"
        ${hsieMinimalImports} graph-symbols | ${graphviz}/bin/dot -Tpng -o "$outfile"
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
    dumpMinimalImports.bin
    hsieMinimalImports.bin
    hsieGraphModules.bin
    hsieGraphSymbols.bin
  ];
}
