{
  build-toolbox = import ./build-toolbox;
  checked-shell-script = import ./checked-shell-script;
  gitignore = import ./gitignore.nix;
  haskell-packages = import ./haskell-packages.nix;
  postgis = import ./postgis.nix;
  postgresql-libpq = import ./postgresql-libpq.nix;
  postgresql-legacy = import ./postgresql-legacy.nix;
  postgresql-future = import ./postgresql-future.nix;
  slocat = import ./slocat.nix;
}
