{
  gitignore = import ./gitignore.nix;
  ghr = import ./ghr;
  haskell-packages = import ./haskell-packages.nix;
  postgresql-default = import ./postgresql-default.nix;
}
