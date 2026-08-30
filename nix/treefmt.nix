{
  settings.verbose = 1;
  settings.on-unmatched = "debug";

  programs.actionlint.enable = true;
  programs.black.enable = true;
  programs.deadnix.enable = true;
  programs.nixpkgs-fmt.enable = true;
  programs.ruff-check.enable = true;
  programs.statix.enable = true;
  programs.stylish-haskell.enable = true;
}
