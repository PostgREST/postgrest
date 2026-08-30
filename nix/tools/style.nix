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
      '';

in
buildToolbox
{
  name = "postgrest-style";
  tools = { inherit lint; };
}
