let
  name =
    "postgrest";

  # PostgREST source files, filtered based on the rules in the .gitignore files
  # and file extensions. We want to include as litte as possible, as the files
  # added here will increase the space used in the Nix store and trigger the
  # build of new Nix derivations when changed.
  src =
    pkgs.lib.sourceFilesBySuffices
      (pkgs.gitignoreSource ./.)
      [ ".cabal" ".hs" ".lhs" ".sql" "LICENSE" ];

  nixpkgsVersion =
    import nix/nixpkgs-version.nix;

  pinnedPkgs =
    builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
      sha256 = nixpkgsVersion.tarballHash;
    };

  overlays =
    [
      (import nix/overlays/postgresql-legacy.nix)
      (import nix/overlays/gitignore.nix)
      (import nix/overlays/haskell-packages)
    ];

  pkgs =
    import pinnedPkgs { inherit overlays; };

  postgresqlVersions =
    [
      pkgs.postgresql_12
      pkgs.postgresql_11
      pkgs.postgresql_10
      pkgs.postgresql_9_6
      pkgs.postgresql_9_5
      pkgs.postgresql_9_4
    ];

  drv =
    pkgs.haskellPackages.callCabal2nix name src {};
in
rec {
  inherit pkgs pinnedPkgs;

  # Derivation for the PostgREST Haskell package, including the executable,
  # libraries and documentation. We disable running the test suite on Nix
  # builds, as they require a database to be set up.
  postgrestWithLib =
    pkgs.haskell.lib.dontCheck drv;

  # Derivation for just the PostgREST binary, where we strip all dynamic
  # libraries and documentation, leaving only the executable.
  postgrest =
    pkgs.haskell.lib.justStaticExecutables postgrestWithLib;

  # Environment in which PostgREST can be built with cabal, useful e.g. for
  # defining a shell for nix-shell.
  env =
    drv.env;

  # Utility for updating the pinned version of Nixpkgs.
  nixpkgsUpgrade =
    pkgs.callPackage nix/nixpkgs-upgrade.nix {};

  tests =
    pkgs.callPackage nix/tests.nix
      {
        inherit postgresqlVersions;
        postgrestBuildEnv = env;
      };

  style =
    pkgs.callPackage nix/style.nix {};

  lint =
    pkgs.callPackage nix/lint.nix {};
}
