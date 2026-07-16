{ actionlint
, black
, buildToolbox
, checkedShellScript
, deadnix
, fd
, git
, hlint
, hsie
, nixpkgs-fmt
, python3Packages
, ruff
, statix
, stylish-haskell
, writeText
}:
let
  style =
    checkedShellScript
      {
        name = "postgrest-style";
        docs = "Automatically format Haskell, Nix and Python files.";
        workingDir = "/";
      }
      ''
        # Format Nix files
        ${statix}/bin/statix fix
        ${nixpkgs-fmt}/bin/nixpkgs-fmt .

        # Format Haskell files
        ${fd}/bin/fd '\.l?hs$' \
          | xargs ${stylish-haskell}/bin/stylish-haskell -i

        # Format Python files
        ${black}/bin/black .
      '';

  # Script to check whether any uncommitted changes result from postgrest-style
  styleCheck =
    checkedShellScript
      {
        name = "postgrest-style-check";
        docs = "Check whether postgrest-style results in any uncommitted changes.";
        workingDir = "/";
      }
      ''
        ${style}

        trap "echo postgrest-style-check failed. Run postgrest-style to fix issues automatically." ERR

        ${git}/bin/git diff-index --exit-code HEAD -- '*.hs' '*.lhs' '*.nix' '*.py'
      '';

  hlintConfig = writeText "hlintConfig.yml" ''

    # Arguments passed to hlint
    - arguments: [-j, -XQuasiQuotes, -XNoPatternSynonyms]

    # Warnings
    - warn: { lhs: "a == a", rhs: "True",  note: "This comparison always evaluates to True" }
    - warn: { lhs: "a /= a", rhs: "False", note: "This comparison always evaluates to False" }
    - warn: { lhs: "a < a",  rhs: "False", note: "This comparison always evaluates to False" }
    - warn: { lhs: "a > a",  rhs: "False", note: "This comparison always evaluates to False" }
    - warn: { lhs: "a <= a", rhs: "True",  note: "This comparison always evaluates to True" }
    - warn: { lhs: "a >= a", rhs: "True",  note: "This comparison always evaluates to True" }
  '';

  lint =
    checkedShellScript
      {
        name = "postgrest-lint";
        docs = "Lint all Haskell files, bash scripts and github workflows.";
        workingDir = "/";
      }
      ''
        echo "Linting workflows..."
        ${actionlint}/bin/actionlint

        echo "Scanning nix files for unused code..."
        ${deadnix}/bin/deadnix -f

        # ruff has gaps in scanning for unused code, so we use vulture
        echo "Scanning python files for unused code..."
        ${fd}/bin/fd '\.l?py$' \
          | xargs ${python3Packages.vulture}/bin/vulture --exclude docs/conf.py --min-confidence 80

        echo "Linting python files..."
        ${ruff}/bin/ruff check .

        echo "Checking consistency of import aliases in Haskell code..."
        ${hsie} check-aliases src/library src/executable

        echo "Linting Haskell files..."
        ${fd}/bin/fd '\.l?hs$' \
          | xargs ${hlint}/bin/hlint --hint=${hlintConfig}
      '';

in
buildToolbox
{
  name = "postgrest-style";
  tools = { inherit style styleCheck lint; };
}
