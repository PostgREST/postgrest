{
  gitignore = import ./gitignore.nix;
  ghr = import ./ghr;
  haskell-packages = import ./haskell-packages.nix;
  locust = import ./locust;
  postgresql-default = import ./postgresql-default.nix;
}
