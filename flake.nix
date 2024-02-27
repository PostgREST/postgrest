{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/dbf5322e93bcc6cfc52268367a8ad21c09d76fea";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    postgresql-libpq = {
      url = "github:postgrest/postgresql-libpq";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://community.flake.parts/haskell-flake/package-set
          # basePackages = pkgs.haskellPackages;

          # Extra package information. See https://community.flake.parts/haskell-flake/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          # packages = {
          #   aeson.source = "1.5.0.0"; # Hackage version override
          #   shower.source = inputs.shower;
          # };
          # settings = {
          #   aeson = {
          #     check = false;
          #   };
          #   relude = {
          #     haddock = false;
          #     broken = false;
          #   };
          # };
          packages = {
            hasql-notifications.source = "0.2.0.6";
            hasql-pool.source = "0.10";
            postgrest = inputs.postgresql-libpq;
          };
          settings = {
            configurator-pg.broken = false;
            fuzzyset.broken = false;
          };

          # devShell = {
          #  # Enabled by default
          #  enable = true;
          #
          #  # Programs you want to make available in the shell.
          #  # Default programs can be disabled by setting to 'null'
          #  tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
          #
          #  hlsCheck.enable = true;
          # };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.postgrest;
      };
    };
}
