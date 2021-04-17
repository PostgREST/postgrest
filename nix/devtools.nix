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
        args =
          [
            "ARG_POSITIONAL_SINGLE([command], [Command to run])"
            "ARG_LEFTOVERS([command arguments])"
          ];
        addCommandCompletion = true;
        redirectTixFiles = false; # will be done by sub-command
        inRootDir = true;
      }
      ''
        while true; do
          (! ${silver-searcher}/bin/ag -l . | ${entr}/bin/entr -dr "$_arg_command" "''${_arg_leftovers[@]}")
        done
      '';

  pushCachix =
    checkedShellScript
      {
        name = "postgrest-push-cachix";
        docs = ''
          Push all build artifacts to cachix.

          Requires authentication with `cachix authtoken ...`.
        '';
        inRootDir = true;
      }
      ''
        nix-instantiate \
          | while read -r drv; do
              nix-store -qR --include-outputs "$drv"
            done \
          | cachix push postgrest
      '';

  build =
    checkedShellScript
      {
        name = "postgrest-build";
        docs = "Build PostgREST interactively using cabal-install.";
        args = [ "ARG_LEFTOVERS([Cabal arguments])" ];
        inRootDir = true;
      }
      ''
        ${cabal-install}/bin/cabal v2-build ${devCabalOptions} "''${_arg_leftovers[@]}"
      '';

  run =
    checkedShellScript
      {
        name = "postgrest-run";
        docs = "Run PostgREST after buidling it interactively with cabal-install";
        args = [ "ARG_LEFTOVERS([PostgREST arguments])" ];
        inRootDir = true;
      }
      ''
        ${cabal-install}/bin/cabal v2-run ${devCabalOptions} --verbose=0 -- \
          postgrest "''${_arg_leftovers[@]}"
      '';

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
        args = [ "ARG_POSITIONAL_SINGLE([dumpdir], [Output directory])" ];
        inRootDir = true;
        withTmpDir = true;
      }
      ''
        mkdir -p "$_arg_dumpdir"
        ${cabal-install}/bin/cabal v2-build ${devCabalOptions} \
          --builddir="$tmpdir" \
          --ghc-option=-ddump-minimal-imports \
          --ghc-option=-dumpdir="$_arg_dumpdir" \
          1>&2

        # Fix OverloadedRecordFields imports
        # shellcheck disable=SC2016
        sed -E 's/\$sel:.*://g' -i "$_arg_dumpdir"/*
      '';

  hsieMinimalImports =
    checkedShellScript
      {
        name = "postgrest-hsie-minimal-imports";
        docs = "Run hsie with a provided dump of minimal imports.";
        args = [ "ARG_LEFTOVERS([hsie arguments])" ];
        withTmpDir = true;
      }
      ''
        ${dumpMinimalImports} "$tmpdir"
        ${hsie} "$tmpdir" "''${_arg_leftovers[@]}"
      '';

  hsieGraphModules =
    checkedShellScript
      {
        name = "postgrest-hsie-graph-modules";
        docs = "Create a PNG graph of modules imported within the codebase.";
        args = [ "ARG_POSITIONAL_SINGLE([outfile], [Output filename])" ];
      }
      ''
        ${hsie} graph-modules main src | ${graphviz}/bin/dot -Tpng -o "$_arg_outfile"
      '';

  hsieGraphSymbols =
    checkedShellScript
      {
        name = "postgrest-hsie-graph-symbols";
        docs = "Create a PNG graph of symbols imported within the codebase.";
        args = [ "ARG_POSITIONAL_SINGLE([outfile], [Output filename])" ];
      }
      ''
        ${hsieMinimalImports} graph-symbols | ${graphviz}/bin/dot -Tpng -o "$_arg_outfile"
      '';

  tools = [
    watch
    pushCachix
    build
    run
    clean
    check
    dumpMinimalImports
    hsieMinimalImports
    hsieGraphModules
    hsieGraphSymbols
  ];

  bashCompletion = builtins.map (tool: tool.bashCompletion) tools;

in
buildEnv
  {
    name = "postgrest-devtools";
    paths = builtins.map (tool: tool.bin) tools;
  } // { inherit bashCompletion; }
