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
        # We don't include the `postgrest-docker-load` here, as that would
        # cause the shell to depend on building the Docker images and in turn
        # on the static executable. Use `nix-shell default.nix -A dockerLoad`
        # to get a shell with that script on the PATH.
      ];

    shellHook =
      ''
        # Set our pinned version of Nixpkgs in the NIX_PATH so that
        # `stack --nix` also uses that version.
        NIX_PATH="nixpkgs=${nixpkgs}"
      '';
  }
)
