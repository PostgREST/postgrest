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

  allOverlays =
    import nix/overlays;

  overlays =
    [
      allOverlays.checked-shell-script
      allOverlays.ghr
      allOverlays.gitignore
      allOverlays.postgresql-default
      (allOverlays.haskell-packages { inherit compiler; })
    ];

  # Evaluated expression of the Nixpkgs repository.
  pkgs =
    import nixpkgs { inherit overlays; };

  postgresqlVersions =
    [
      { name = "postgresql-13"; postgresql = pkgs.postgresql_13; }
      { name = "postgresql-12"; postgresql = pkgs.postgresql_12; }
      { name = "postgresql-11"; postgresql = pkgs.postgresql_11; }
      { name = "postgresql-10"; postgresql = pkgs.postgresql_10; }
      { name = "postgresql-9.6"; postgresql = pkgs.postgresql_9_6; }
      { name = "postgresql-9.5"; postgresql = pkgs.postgresql_9_5; }
    ];

  patches =
    pkgs.callPackage nix/patches { };

  # Dynamic derivation for PostgREST
  postgrest =
    pkgs.haskell.packages."${compiler}".callCabal2nix name src { };

  # Function that derives a fully static Haskell package based on
  # nh2/static-haskell-nix
  staticHaskellPackage =
    import nix/static-haskell-package.nix { inherit nixpkgs compiler patches allOverlays; };

  profiledHaskellPackages =
    pkgs.haskell.packages."${compiler}".extend (self: super:
      {
        mkDerivation =
          args:
          super.mkDerivation (args // { enableLibraryProfiling = true; });
      }
    );

  lib =
    pkgs.haskell.lib;
in
rec {
  inherit nixpkgs pkgs;

  # Derivation for the PostgREST Haskell package, including the executable,
  # libraries and documentation. We disable running the test suite on Nix
  # builds, as they require a database to be set up.
  postgrestPackage =
    lib.dontCheck (lib.enableCabalFlag postgrest "FailOnWarn");

  # Static executable.
  postgrestStatic =
    lib.justStaticExecutables (lib.dontCheck (staticHaskellPackage name src));

  # Profiled dynamic executable.
  postgrestProfiled =
    lib.enableExecutableProfiling (
      lib.dontHaddock (
        lib.dontCheck (profiledHaskellPackages.callCabal2nix name src { })
      )
    );

  # Docker images and loading script.
  docker =
    pkgs.callPackage nix/docker { postgrest = postgrestStatic; };

  env =
    postgrest.env;

  # Utility for updating the pinned version of Nixpkgs.
  nixpkgsUpgrade =
    pkgs.callPackage nix/nixpkgs-upgrade.nix { };

  # Scripts for running tests.
  tests =
    pkgs.callPackage nix/tests.nix { inherit postgrest postgrestStatic postgrestProfiled postgresqlVersions; };

  # Linting and styling scripts.
  style =
    pkgs.callPackage nix/style.nix { };

  # Development tools, including linting and styling scripts.
  devtools =
    pkgs.callPackage nix/devtools.nix { inherit tests style; };

  # Scripts for publishing new releases.
  release =
    pkgs.callPackage nix/release {
      inherit docker;
      postgrest = postgrestStatic;
    };
}
