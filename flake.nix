{
  description = "REST API for any Postgres database";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-25.05-darwin";
  };

  nixConfig = {
    extra-substituters = "https://postgrest.cachix.org";
    extra-trusted-public-keys = "postgrest.cachix.org-1:icgW4R15fz1+LqvhPjt4EnX/r19AaqxiVV+1olwlZtI=";
  };

  outputs = { nixpkgs, ... }:
    let
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      pgrstFor = system: import ./default.nix {
        inherit system;
        nixpkgsVersion = {
          owner = "nixos";
          repo = "nixpkgs";
          inherit (nixpkgs) rev;
          tarballHash = nixpkgs.narHash;
        };
      };

      genSystems = f: nixpkgs.lib.genAttrs systems (system: f (pgrstFor system));
    in
    {
      packages = genSystems (attrs: {
        default = attrs.postgrestPackage.bin;
        profiled = attrs.postgrestProfiled.bin;
      } // nixpkgs.lib.optionalAttrs (attrs ? postgrestStatic) {
        static = attrs.postgrestStatic;
      });

      apps = genSystems (attrs: {
        default = {
          type = "app";
          program = "${attrs.postgrestStatic or attrs.postgrestPackage.bin}/bin/postgrest";
          meta.description = "REST API for any Postgres database";
        };
      });

      devShells = genSystems (attrs:
        let
          inherit (attrs) pkgs;
          inherit (pkgs) lib;
          toolboxes = [
            attrs.cabalTools
            attrs.devTools
            attrs.docs
            attrs.gitTools
            attrs.loadtest
            attrs.nixpkgsTools
            attrs.release
            attrs.style
            attrs.tests
            attrs.withTools
          ];
        in
        {
          default = lib.overrideDerivation attrs.env (base: {
            buildInputs =
              base.buildInputs ++ [
                pkgs.cabal-install
                pkgs.cabal2nix
                pkgs.git
                pkgs.postgresql
                pkgs.update-nix-fetchgit
                attrs.hsie.bin
              ]
              ++ toolboxes;

            shellHook =
              ''
                export HISTFILE=.history

                source ${pkgs.bash-completion}/etc/profile.d/bash_completion.sh
                source ${pkgs.git}/share/git/contrib/completion/git-completion.bash
                source ${attrs.hsie.bash-completion}

              ''
              + builtins.concatStringsSep "\n" (
                builtins.map (bash-completion: "source ${bash-completion}") (
                  builtins.concatLists (builtins.map (toolbox: toolbox.bash-completion) toolboxes)
                )
              );
          });
        });
    };
}
