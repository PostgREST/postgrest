{ system ? builtins.currentSystem

, compiler ? "ghc948"

, # Commit of the Nixpkgs repository that we want to use.
  # It defaults to reading the inputs from flake.lock, which serves
  # as a compatibility layer for non-flake builds / default.nix / shell.nix.
  nixpkgsVersion ? let
    lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  in
  {
    inherit (lock.nodes.nixpkgs.locked) owner repo rev;
    tarballHash = lock.nodes.nixpkgs.locked.narHash;
  }

, # Nix files that describe the Nixpkgs repository. We evaluate the expression
  # using `import` below.
  nixpkgs ? let inherit (nixpkgsVersion) owner repo rev tarballHash; in
  builtins.fetchTarball {
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    sha256 = tarballHash;
  }
}:

let
  name =
    "postgrest";

  # PostgREST source files, filtered based on the rules in the .gitignore files
  # and file extensions. We want to include as little as possible, as the files
  # added here will increase the space used in the Nix store and trigger the
  # build of new Nix derivations when changed.
  src =
    pkgs.lib.sourceFilesBySuffices
      (pkgs.gitignoreSource ./.)
      [ ".cabal" ".hs" ".lhs" "LICENSE" ];

  allOverlays =
    import nix/overlays;

  overlays =
    [
      allOverlays.build-toolbox
      allOverlays.checked-shell-script
      allOverlays.gitignore
      (allOverlays.haskell-packages { inherit compiler; })
      allOverlays.slocat
    ];

  # Evaluated expression of the Nixpkgs repository.
  pkgs =
    import nixpkgs { inherit overlays system; };

  postgresqlVersions =
    [
      { name = "postgresql-17"; postgresql = pkgs.postgresql_17.withPackages (p: [ p.postgis p.pg_safeupdate ]); }
      { name = "postgresql-16"; postgresql = pkgs.postgresql_16.withPackages (p: [ p.postgis p.pg_safeupdate ]); }
      { name = "postgresql-15"; postgresql = pkgs.postgresql_15.withPackages (p: [ p.postgis p.pg_safeupdate ]); }
      { name = "postgresql-14"; postgresql = pkgs.postgresql_14.withPackages (p: [ p.postgis p.pg_safeupdate ]); }
      { name = "postgresql-13"; postgresql = pkgs.postgresql_13.withPackages (p: [ p.postgis p.pg_safeupdate ]); }
    ];

  haskellPackages = pkgs.haskell.packages."${compiler}";

  # Dynamic derivation for PostgREST
  postgrest = pkgs.lib.pipe (haskellPackages.callCabal2nix name src { }) [
    # To allow ghc-datasize to be used.
    lib.disableLibraryProfiling
    # We are never going to use dynamic haskell libraries anyway. "Dynamic" refers to how
    # non-haskell deps are linked. All haskell dependencies are always statically linked.
    lib.disableSharedLibraries
  ];

  staticHaskellPackage = import nix/static.nix { inherit compiler name pkgs src; };

  # Options passed to cabal in dev tools and tests
  devCabalOptions =
    "-f dev --test-show-detail=direct --disable-shared";

  inherit (pkgs.haskell) lib;
in
rec {
  inherit nixpkgs pkgs;

  # Derivation for the PostgREST Haskell package, including the executable,
  # libraries and documentation. We disable running the test suite on Nix
  # builds, as they require a database to be set up. We split the binary
  # into a separate output, so that the default distribution via flake.nix
  # has a much smaller closure size.
  postgrestPackage = pkgs.lib.pipe postgrest [
    lib.dontCheck
    lib.enableSeparateBinOutput
    (haskellPackages.generateOptparseApplicativeCompletions [ "postgrest" ])
  ];

  # Profiled dynamic executable.
  postgrestProfiled = pkgs.lib.pipe postgrestPackage [
    lib.enableExecutableProfiling
    lib.enableLibraryProfiling
    lib.dontHaddock
  ];

  inherit (postgrest) env;

  # Tooling for analyzing Haskell imports and exports.
  hsie =
    pkgs.callPackage nix/hsie {
      inherit (pkgs.haskell.packages."${compiler}") ghcWithPackages;
    };

  ### Tools

  cabalTools =
    pkgs.callPackage nix/tools/cabalTools.nix { inherit devCabalOptions postgrest; };

  withTools =
    pkgs.callPackage nix/tools/withTools.nix { inherit postgresqlVersions postgrest; };

  # Development tools.
  devTools =
    pkgs.callPackage nix/tools/devTools.nix { inherit tests style devCabalOptions hsie withTools; };

  # Documentation tools.
  docs =
    pkgs.callPackage nix/tools/docs.nix { };

  # Git tools.
  gitTools =
    pkgs.callPackage nix/tools/gitTools.nix { };

  # Load testing tools.
  loadtest =
    pkgs.callPackage nix/tools/loadtest.nix { inherit withTools; };

  # Utility for updating the pinned version of Nixpkgs.
  nixpkgsTools =
    pkgs.callPackage nix/tools/nixpkgsTools.nix { };

  # Scripts for publishing new releases.
  release =
    pkgs.callPackage nix/tools/release.nix { };

  # Linting and styling tools.
  style =
    pkgs.callPackage nix/tools/style.nix { inherit hsie; };

  # Scripts for running tests.
  tests =
    pkgs.callPackage nix/tools/tests.nix {
      inherit postgrest devCabalOptions withTools;
      ghc = pkgs.haskell.compiler."${compiler}";
      inherit (pkgs.haskell.packages."${compiler}") hpc-codecov;
      inherit (pkgs.haskell.packages."${compiler}") weeder;
    };
} // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux rec {
  # Static executable.
  inherit (staticHaskellPackage) postgrestStatic;
  inherit (staticHaskellPackage) packagesStatic;

  # Docker images and loading script.
  docker =
    pkgs.callPackage nix/tools/docker { postgrest = postgrestStatic; };
}
