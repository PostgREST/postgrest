let
  name =
    "postgrest";

  compiler =
    "ghc883";

  # PostgREST source files, filtered based on the rules in the .gitignore files
  # and file extensions. We want to include as litte as possible, as the files
  # added here will increase the space used in the Nix store and trigger the
  # build of new Nix derivations when changed.
  src =
    pkgs.lib.sourceFilesBySuffices
      (pkgs.gitignoreSource ./.)
      [ ".cabal" ".hs" ".lhs" "LICENSE" ];

  # Commit of the Nixpkgs repository that we want to use.
  nixpkgsVersion =
    import nix/nixpkgs-version.nix;

  # Nix files that describe the Nixpkgs repository. We evaluate the expression
  # using `import` below.
  nixpkgs =
    builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
      sha256 = nixpkgsVersion.tarballHash;
    };

  overlays =
    [
      (import nix/overlays/postgresql-default.nix)
      (import nix/overlays/postgresql-legacy.nix)
      (import nix/overlays/gitignore.nix)
      (import nix/overlays/haskell-packages { inherit compiler; })
    ];

  # Evaluated expression of the Nixpkgs repository.
  pkgs =
    import nixpkgs { inherit overlays; };

  postgresqlVersions =
    {
      postgresql-12 = pkgs.postgresql_12;
      postgresql-11 = pkgs.postgresql_11;
      postgresql-10 = pkgs.postgresql_10;
      "postgresql-9.6" = pkgs.postgresql_9_6;
      "postgresql-9.5" = pkgs.postgresql_9_5;
      "postgresql-9.4" = pkgs.postgresql_9_4;
    };

  patches =
    pkgs.callPackage nix/patches {};

  # Base dynamic derivation for the PostgREST package.
  drv =
    pkgs.haskell.packages."${compiler}".callCabal2nix name src {};

  # Static set of Haskell Packages based on nh2/static-haskell-nix
  staticHaskellPackages =
    import nix/static-haskell-packages.nix
      { inherit nixpkgs compiler patches; };

  # Static derivation for the PostgREST executable.
  drvStatic =
    staticHaskellPackages.callCabal2nix name src {};

  lib =
    pkgs.haskell.lib;
in
rec {
  inherit nixpkgs pkgs;

  # Derivation for the PostgREST Haskell package, including the executable,
  # libraries and documentation. We disable running the test suite on Nix
  # builds, as they require a database to be set up.
  postgrestPackage =
    lib.dontCheck (lib.enableCabalFlag drv "FailOnWarn");

  # Derivation for just the PostgREST binary, where we strip all dynamic
  # libraries and documentation, leaving only the executable. Note that the
  # executable is static with regards to Haskell libraries, but not system
  # libraries like glibc and libpq.
  postgrest =
    lib.justStaticExecutables postgrestPackage;

  # Static executable.
  postgrestStatic =
    lib.justStaticExecutables (lib.dontCheck drvStatic);

  # Docker images and loading script.
  docker =
    pkgs.callPackage nix/docker { postgrest = postgrestStatic; };

  # Script `postgrest-docker-load` that loads the images built with Nix into
  # Docker, using `docker load -i` under the hood. Referenced here from the top
  # level so that it gets built by default.
  dockerLoad =
    docker.load;

  # Environment in which PostgREST can be built with cabal, useful e.g. for
  # defining a shell for `nix-shell`.
  env =
    drv.env;

  # Utility for updating the pinned version of Nixpkgs.
  nixpkgsUpgrade =
    pkgs.callPackage nix/nixpkgs-upgrade.nix {};

  # Scripts for running tests.
  tests =
    pkgs.callPackage nix/tests.nix
      {
        inherit postgresqlVersions;
        postgrestBuildEnv = env;
      };

  # Development tools, including linting and styling scripts.
  devtools =
    pkgs.callPackage nix/devtools.nix {};
}
