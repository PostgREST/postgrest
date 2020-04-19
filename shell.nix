with (import ./default.nix);
pkgs.lib.overrideDerivation postgrest.env (
  base: {
    buildInputs =
      base.buildInputs ++ [
        pkgs.cabal-install
        pkgs.cabal2nix
        pkgs.postgresql
        nixpkgsUpgrade
      ];
  }
)
