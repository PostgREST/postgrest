with (import ./default.nix);
pkgs.lib.overrideDerivation env (
  base: {
    buildInputs =
      base.buildInputs ++ [
        pkgs.cabal-install
        pkgs.stack
        pkgs.cabal2nix
        pkgs.postgresql
        nixpkgsUpgrade
        tests
        style
        lint
      ];

    shellHook =
      ''
        # Set our pinned version of Nixpkgs in the NIX_PATH so that
        # `stack --nix` also uses that version.
        NIX_PATH="nixpkgs=${nixpkgs}"
      '';
  }
)
