{ buildToolbox
, checkedShellScript
, fd
, hsie
, python3Packages

}:
let

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
      '';

in
buildToolbox
{
  name = "postgrest-style";
  tools = { inherit lint; };
}
