let
  name =
    "postgrest";

  compiler =
    "ghc8107";

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
      allOverlays.build-toolbox
      allOverlays.checked-shell-script
      allOverlays.gitignore
      allOverlays.postgresql-default
      allOverlays.postgresql-legacy
      allOverlays.postgresql-future
      (allOverlays.haskell-packages { inherit compiler; })
    ];

  # Evaluated expression of the Nixpkgs repository.
  pkgs =
    import nixpkgs { inherit overlays; };

  postgresqlVersions =
    [
      { name = "postgresql-14"; postgresql = pkgs.postgresql_14; }
      { name = "postgresql-13"; postgresql = pkgs.postgresql_13; }
      { name = "postgresql-12"; postgresql = pkgs.postgresql_12; }
      { name = "postgresql-11"; postgresql = pkgs.postgresql_11; }
      { name = "postgresql-10"; postgresql = pkgs.postgresql_10; }
      { name = "postgresql-9.6"; postgresql = pkgs.postgresql_9_6; }
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

  # Options passed to cabal in dev tools and tests
  devCabalOptions =
    "-f dev --test-show-detail=direct";

  profiledHaskellPackages =
    pkgs.haskell.packages."${compiler}".extend (self: super:
      {
        mkDerivation =
          args:
          super.mkDerivation (args // { enableLibraryProfiling = true; });
      }
    );

  inherit (pkgs.haskell) lib;
in
rec {
  inherit nixpkgs pkgs;

  # Derivation for the PostgREST Haskell package, including the executable,
  # libraries and documentation. We disable running the test suite on Nix
  # builds, as they require a database to be set up.
  postgrestPackage =
    lib.dontCheck postgrest;

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

  inherit (postgrest) env;

  # Tooling for analyzing Haskell imports and exports.
  hsie =
    pkgs.callPackage nix/hsie {
      inherit (pkgs.haskell.packages."${compiler}") ghcWithPackages;
    };

  ### Tools

  cabalTools =
    pkgs.callPackage nix/tools/cabalTools.nix { inherit devCabalOptions postgrest; };

  # Development tools.
  devTools =
    pkgs.callPackage nix/tools/devTools.nix { inherit tests style devCabalOptions hsie withTools; };

  # Docker images and loading script.
  docker =
    pkgs.callPackage nix/tools/docker { postgrest = postgrestStatic; };

  # Load testing tools.
  loadtest =
    pkgs.callPackage nix/tools/loadtest.nix { inherit withTools; };

  # Script for running memory tests.
  memory =
    pkgs.callPackage nix/tools/memory.nix { inherit postgrestProfiled withTools; };

  # Utility for updating the pinned version of Nixpkgs.
  nixpkgsTools =
    pkgs.callPackage nix/tools/nixpkgsTools.nix { };

  # Scripts for publishing new releases.
  release =
    pkgs.callPackage nix/tools/release { };

  # Linting and styling tools.
  style =
    pkgs.callPackage nix/tools/style.nix { inherit hsie; };

  # Scripts for running tests.
  tests =
    pkgs.callPackage nix/tools/tests.nix {
      inherit postgrest devCabalOptions withTools;
      ghc = pkgs.haskell.compiler."${compiler}";
      inherit (pkgs.haskell.packages."${compiler}") hpc-codecov;
    };

  withTools =
    pkgs.callPackage nix/tools/withTools.nix { inherit devCabalOptions postgresqlVersions postgrest; };
}
