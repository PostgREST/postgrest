{ buildToolbox
, cabal-install
, cachix
, checkedShellScript
, devCabalOptions
, entr
, git
, gnugrep
, graphviz
, haskellPackages
, hsie
, nix
, silver-searcher
, style
, tests
, withTools
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
        positionalCompletion = "_command";
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
        ${nix}/bin/nix-instantiate \
          | while read -r drv; do
              ${nix}/bin/nix-store -qR --include-outputs "$drv"
            done \
          | ${cachix}/bin/cachix push postgrest
      '';

  check =
    checkedShellScript
      {
        name = "postgrest-check";
        docs =
          ''
            Run most checks that will also run on CI, but only against the
            latest PostgreSQL version.

            This currently excludes the memory and spec-idempotence tests,
            as those are particularly expensive.
          '';
        inRootDir = true;
      }
      ''
        ${tests}/bin/postgrest-test-spec
        ${tests}/bin/postgrest-test-querycost
        ${tests}/bin/postgrest-test-doctests
        ${tests}/bin/postgrest-test-io
        ${style}/bin/postgrest-lint
        ${style}/bin/postgrest-style-check
      '';

  gitHooks =
    let
      name = "postgrest-git-hooks";
    in
    checkedShellScript
      {
        inherit name;
        docs =
          ''
            Enable or disable git pre-commit and pre-push hooks.

            Basic is faster and will only run:
              - pre-commit: postgrest-style
              - pre-push: postgrest-lint

            Full takes a lot more time and will run:
              - pre-commit: postgrest-style && postgrest-lint
              - pre-push: postgrest-check

            Changes made by postgrest-style will be staged automatically.

            Example usage:
              postgrest-git-hooks disable
              postgrest-git-hooks enable basic
              postgrest-git-hooks enable full

            The "run" operation and "--hook" argument are only used internally.
          '';
        args =
          [
            "ARG_POSITIONAL_SINGLE([operation], [Operation])"
            "ARG_TYPE_GROUP_SET([OPERATION], [OPERATION], [operation], [disable,enable,run])"
            "ARG_POSITIONAL_SINGLE([mode], [Mode], [basic])"
            "ARG_TYPE_GROUP_SET([MODE], [MODE], [mode], [basic,full])"
            "ARG_OPTIONAL_SINGLE([hook], , [Hook], [pre-commit])"
            "ARG_TYPE_GROUP_SET([HOOK], [HOOK], [hook], [pre-commit,pre-push])"
          ];
        positionalCompletion =
          ''
            if test "$prev" == "${name}"; then
              COMPREPLY=( $(compgen -W "enable disable" -- "$cur") )
            elif test "$prev" == "enable" || test "$prev" == "disable"; then
              COMPREPLY=( $(compgen -W "basic full" -- "$cur") )
            fi
          '';
        inRootDir = true;
      }
      ''
        if [ run != "$_arg_operation" ]; then
          # Remove all hooks first and ignore failures because the file might be missing.
          # This assumes that we're only adding lines that include "postgrest-git-hooks"
          # to the hook file.
          sed -i -e '/postgrest-git-hooks/d' .git/hooks/pre-{commit,push} 2> /dev/null || true

          if [ disabled != "$_arg_mode" ]; then
            # The nix-shell && + nix-shell || pattern makes sure we can run the hook
            # in a pure nix-shell, where nix-shell itself is not available, too.

            # The $(nix-shell --run "command -v ...") pattern ensures we only need to enable
            # the hooks once and still run the latest of our hook scripts, even when we
            # update them in the repo.

            echo 'command -v nix-shell > /dev/null || postgrest-git-hooks --hook=pre-commit run' "$_arg_mode" \
              >> .git/hooks/pre-commit
            # shellcheck disable=SC2016
            echo 'command -v nix-shell > /dev/null && $(nix-shell --quiet -Q --run "command -v postgrest-git-hooks") --hook=pre-commit run' "$_arg_mode" \
             >> .git/hooks/pre-commit
            chmod +x .git/hooks/pre-commit

            echo 'command -v nix-shell > /dev/null || postgrest-git-hooks --hook=pre-push run' "$_arg_mode" \
              >> .git/hooks/pre-push
            # shellcheck disable=SC2016
            echo 'command -v nix-shell > /dev/null && $(nix-shell --quiet -Q --run "command -v postgrest-git-hooks") --hook=pre-push run' "$_arg_mode" \
             >> .git/hooks/pre-push
            chmod +x .git/hooks/pre-push
          fi
        else
          # When run from a git hook, the GIT_ environment variables conflict with our withGit helper.
          # The following unsets all GIT_ variables.
          unset "''${!GIT_@}"

          case "$_arg_mode" in
            basic)
              case "$_arg_hook" in
                pre-commit)
                  # To be able to automatically add only changes from postgrest-style to the staging area,
                  # we need to run postgrest-style twice. Otherwise we'd risk merge conflicts when popping
                  # the stash afterwards.
                  ${style}/bin/postgrest-style

                  stash="postgrest-git-hooks-$RANDOM"
                  ${git}/bin/git stash push --include-untracked --keep-index -m "$stash"
                  if [ "$(git stash list --grep $stash)" ]; then
                    # Only create the stash pop trap, if we actually created a stash.
                    # Otherwise stash pop will cause havoc.
                    trap '${git}/bin/git stash pop $(git stash list --format=format:%gD --grep "$stash" -n1)' EXIT
                  fi

                  ${style}/bin/postgrest-style
                  ${git}/bin/git add .
                  ;;
                pre-push)
                  # Create a clean working tree without any uncomitted changes.
                  ${withTools.withGit} HEAD ${style}/bin/postgrest-lint
                  ;;
              esac
              ;;
            full)
              case "$_arg_hook" in
                pre-commit)
                  # To be able to automatically add only changes from postgrest-style to the staging area,
                  # we need to run postgrest-style twice. Otherwise we'd risk merge conflicts when popping
                  # the stash afterwards.
                  ${style}/bin/postgrest-style

                  stash="postgrest-git-hooks-$RANDOM"
                  ${git}/bin/git stash push --include-untracked --keep-index -m "$stash"
                  if [ "$(git stash list --grep $stash)" ]; then
                    # Only create the stash pop trap, if we actually created a stash.
                    # Otherwise stash pop will cause havoc.
                    trap '${git}/bin/git stash pop $(git stash list --format=format:%gD --grep "$stash" -n1)' EXIT
                  fi

                  ${style}/bin/postgrest-style
                  ${git}/bin/git add .

                  ${style}/bin/postgrest-lint
                  ;;
                pre-push)
                  # Create a clean working tree without any uncomitted changes.
                  ${withTools.withGit} HEAD ${check}
                  ;;
              esac
              ;;
          esac
        fi
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

in
buildToolbox
{
  name = "postgrest-dev";
  tools = [
    watch
    pushCachix
    check
    gitHooks
    dumpMinimalImports
    hsieMinimalImports
    hsieGraphModules
    hsieGraphSymbols
  ];
}
