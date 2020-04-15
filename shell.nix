with (import ./default.nix {});
pkgs.lib.overrideDerivation postgrest.env (
  base: {
    buildInputs =
      base.buildInputs ++ [
        pkgs.cabal-install
        pkgs.cabal2nix
        pkgs.nixpkgs-fmt
        pkgs.postgresql
        pkgs.silver-searcher
        pkgs.stylish-haskell
        tests
        style
        lint
        nixpkgsUpdate
      ];
  }
)
