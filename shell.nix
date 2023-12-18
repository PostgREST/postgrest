# The additional modules below have large dependencies and are therefore
# disabled by default. You can activate them by passing arguments to nix-shell,
# e.g.:
#
#    nix-shell --arg docker true
#
# We highly recommend that use the PostgREST binary cache by installing cachix
# (https://app.cachix.org/) and running `cachix use postgrest`.
{ docker ? false
, memory ? false
}:
let
  postgrest =
    import ./default.nix { };

  inherit (postgrest) pkgs;

  inherit (pkgs) lib;

  toolboxes =
    [
      postgrest.cabalTools
      postgrest.devTools
      postgrest.docs
      postgrest.loadtest
      postgrest.nixpkgsTools
      postgrest.style
      postgrest.tests
      postgrest.withTools
      postgrest.release
    ]
    ++ lib.optional docker postgrest.docker
    ++ lib.optional memory postgrest.memory;

in
lib.overrideDerivation postgrest.env (
  base: {
    buildInputs =
      base.buildInputs ++ [
        pkgs.cabal-install
        pkgs.cabal2nix
        pkgs.git
        pkgs.postgresql
        pkgs.update-nix-fetchgit
        postgrest.hsie.bin
      ]
      ++ toolboxes;

    shellHook =
      ''
        export HISTFILE=.history

        source ${pkgs.bash-completion}/etc/profile.d/bash_completion.sh
        source ${pkgs.git}/share/git/contrib/completion/git-completion.bash
        source ${postgrest.hsie.bash-completion}

      ''
      + builtins.concatStringsSep "\n" (
        builtins.map (bash-completion: "source ${bash-completion}") (
          builtins.concatLists (builtins.map (toolbox: toolbox.bash-completion) toolboxes)
        )
      );
  }
)
