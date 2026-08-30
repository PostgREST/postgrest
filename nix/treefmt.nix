{ pkgs, ... }:
let
  hlintConfig = pkgs.writeText "hlintConfig.yml" ''

    # Arguments passed to hlint
    - arguments: [-XQuasiQuotes, -XNoPatternSynonyms]

    # Warnings
    - warn: { lhs: "a == a", rhs: "True",  note: "This comparison always evaluates to True" }
    - warn: { lhs: "a /= a", rhs: "False", note: "This comparison always evaluates to False" }
    - warn: { lhs: "a < a",  rhs: "False", note: "This comparison always evaluates to False" }
    - warn: { lhs: "a > a",  rhs: "False", note: "This comparison always evaluates to False" }
    - warn: { lhs: "a <= a", rhs: "True",  note: "This comparison always evaluates to True" }
    - warn: { lhs: "a >= a", rhs: "True",  note: "This comparison always evaluates to True" }
  '';
in
{
  settings.verbose = 1;
  settings.on-unmatched = "debug";

  programs.actionlint.enable = true;
  programs.black.enable = true;
  programs.deadnix.enable = true;
  programs.hlint.enable = true;
  programs.nixpkgs-fmt.enable = true;
  programs.ruff-check.enable = true;
  programs.statix.enable = true;
  programs.stylish-haskell.enable = true;

  settings.formatter.hlint.options = [
    "--hint=${hlintConfig}"
  ];
}
