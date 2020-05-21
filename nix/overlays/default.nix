{
  gitignore = import ./gitignore.nix;
  haskell-packages = import ./haskell-packages;
  postgresql-default = import ./postgresql-default.nix;
  postgresql-legacy = import ./postgresql-legacy.nix;
}
