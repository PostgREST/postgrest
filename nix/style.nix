{ black
, buildEnv
, checkedShellScript
, git
, hlint
, nixpkgs-fmt
, silver-searcher
, stylish-haskell
}:
let
  style =
    checkedShellScript "postgrest-style"
      ''
        rootdir="$(${git}/bin/git rev-parse --show-toplevel)"

        # Format Nix files
        ${nixpkgs-fmt}/bin/nixpkgs-fmt "$rootdir" > /dev/null 2> /dev/null

        # Format Haskell files
        # --vimgrep fixes a bug in ag: https://github.com/ggreer/the_silver_searcher/issues/753
        ${silver-searcher}/bin/ag -l --vimgrep -g '\.l?hs$' . "$rootdir" \
          | xargs ${stylish-haskell}/bin/stylish-haskell -i

        # Format Python files
        ${black}/bin/black "$rootdir" 2> /dev/null
      '';

  # Script to check whether any uncommited changes result from postgrest-style
  styleCheck =
    checkedShellScript "postgrest-style-check"
      ''
        ${style}

        ${git}/bin/git diff-index --exit-code HEAD -- '*.hs' '*.lhs' '*.nix'
      '';

  lint =
    checkedShellScript "postgrest-lint"
      ''
        rootdir="$(${git}/bin/git rev-parse --show-toplevel)"

        # Lint Haskell files
        # --vimgrep fixes a bug in ag: https://github.com/ggreer/the_silver_searcher/issues/753
        ${silver-searcher}/bin/ag -l --vimgrep -g '\.l?hs$' "$rootdir" \
          | xargs ${hlint}/bin/hlint -X QuasiQuotes -X NoPatternSynonyms
      '';
in
buildEnv {
  name = "postgrest-devtools";
  paths = [
    style.bin
    styleCheck.bin
    lint.bin
  ];
}
