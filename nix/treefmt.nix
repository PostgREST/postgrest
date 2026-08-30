{
  settings.verbose = 1;
  settings.on-unmatched = "debug";

  programs.actionlint.enable = true;
  programs.deadnix.enable = true;
  programs.ruff-check.enable = true;
  programs.statix.enable = true;
}
