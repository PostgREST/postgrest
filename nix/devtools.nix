{ buildEnv
, cabal-install
, checkedShellScript
, entr
, git
, silver-searcher
, style
, tests
}:
let
  watch =
    checkedShellScript "postgrest-watch"
      ''
        rootdir="$(${git}/bin/git rev-parse --show-toplevel)"

        ${silver-searcher}/bin/ag -l . "$rootdir" | ${entr}/bin/entr "$@"
      '';

  pushCachix =
    checkedShellScript "postgrest-push-cachix"
      ''
        nix-store -qR --include-outputs "$(nix-instantiate)" \
          | cachix push postgrest
      '';

  build =
    checkedShellScript "postgrest-build"
      ''exec ${cabal-install}/bin/cabal v2-build -f FailOnWarn "$@"'';

  run =
    checkedShellScript "postgrest-run"
      ''exec ${cabal-install}/bin/cabal v2-run postgrest -f FailOnWarn -- "$@"'';

  clean =
    checkedShellScript "postgrest-clean"
      ''
        ${cabal-install}/bin/cabal v2-clean
      '';

  check =
    checkedShellScript "postgrest-check"
      ''
        ${tests}/bin/postgrest-test-spec-all
        ${style}/bin/postgrest-lint
        ${style}/bin/postgrest-style-check
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
  ];
}
