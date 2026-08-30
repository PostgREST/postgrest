{ buildToolbox
, checkedShellScript
, hsie


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
        echo "Checking consistency of import aliases in Haskell code..."
        ${hsie} check-aliases src/library src/executable
      '';

in
buildToolbox
{
  name = "postgrest-style";
  tools = { inherit lint; };
}
