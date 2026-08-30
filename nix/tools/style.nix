{ buildToolbox
, checkedShellScript
, fd
, hlint
, hsie
, python3Packages
, writeText
}:
let
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
        # ruff has gaps in scanning for unused code, so we use vulture
        echo "Scanning python files for unused code..."
        ${fd}/bin/fd '\.l?py$' \
          | xargs ${python3Packages.vulture}/bin/vulture --exclude docs/conf.py --min-confidence 80

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
  tools = { inherit lint; };
}
