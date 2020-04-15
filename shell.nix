with (import ./default.nix {});
pkgs.lib.overrideDerivation postgrest.env (
  base: {
    buildInputs =
      base.buildInputs ++ [
        pkgs.cabal-install
        pkgs.cabal2nix
        pkgs.stack
        pkgs.postgresql
        pkgs.silver-searcher
        tests
        style
        lint
        nixpkgsUpdate
      ];

    shellHook = ''
      # Set the Nix path to our pinned packages, e.g. for the stack integration
      export NIX_PATH="nixpkgs=${pinnedPkgs}"
    '';
  }
)
