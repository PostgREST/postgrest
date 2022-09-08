{
  build-toolbox = import ./build-toolbox;
  checked-shell-script = import ./checked-shell-script;
  gitignore = import ./gitignore.nix;
  haskell-packages = import ./haskell-packages.nix;
  postgis = import ./postgis.nix;
  postgresql-default = import ./postgresql-default.nix;
  postgresql-legacy = import ./postgresql-legacy.nix;
  postgresql-future = import ./postgresql-future.nix;
}
