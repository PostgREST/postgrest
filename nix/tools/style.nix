{ actionlint
, black
, buildToolbox
, checkedShellScript
, git
, hlint
, hsie
, nixpkgs-fmt
, shellcheck
, silver-searcher
, statix
, stylish-haskell
}:
let
  style =
    checkedShellScript
      {
        name = "postgrest-style";
        docs = "Automatically format Haskell, Nix and Python files.";
        inRootDir = true;
      }
      ''
        # Format Nix files
        ${statix}/bin/statix fix
        ${nixpkgs-fmt}/bin/nixpkgs-fmt . > /dev/null 2> /dev/null

        # Format Haskell files
        # --vimgrep fixes a bug in ag: https://github.com/ggreer/the_silver_searcher/issues/753
        ${silver-searcher}/bin/ag -l --vimgrep -g '\.l?hs$' . \
          | xargs ${stylish-haskell}/bin/stylish-haskell -i

        # Format Python files
        ${black}/bin/black . 2> /dev/null
      '';

  # Script to check whether any uncommited changes result from postgrest-style
  styleCheck =
    checkedShellScript
      {
        name = "postgrest-style-check";
        docs = "Check whether postgrest-style results in any uncommited changes.";
        inRootDir = true;
      }
      ''
        ${style}

        trap "echo postgrest-style-check failed. Run postgrest-style to fix issues automatically." ERR

        ${git}/bin/git diff-index --exit-code HEAD -- '*.hs' '*.lhs' '*.nix'
      '';

  lint =
    checkedShellScript
      {
        name = "postgrest-lint";
        docs = "Lint all Haskell files, bash scripts and github workflows.";
        inRootDir = true;
      }
      ''
        echo "Checking consistency of import aliases in Haskell code..."
        ${hsie} check-aliases main src


        echo "Linting Haskell files..."
        # --vimgrep fixes a bug in ag: https://github.com/ggreer/the_silver_searcher/issues/753
        ${silver-searcher}/bin/ag -l --vimgrep -g '\.l?hs$' . \
          | xargs ${hlint}/bin/hlint -X QuasiQuotes -X NoPatternSynonyms

        echo "Linting bash scripts..."
        ${shellcheck}/bin/shellcheck \
          .github/get_cirrusci_freebsd \
          .github/release \
          test/with_tmp_db

        echo "Linting workflows..."
        ${actionlint}/bin/actionlint
      '';

in
buildToolbox
{
  name = "postgrest-style";
  tools = [ style styleCheck lint ];
}
