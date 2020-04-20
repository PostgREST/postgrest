let
  name =
    "postgrest";

  # PostgREST source files, filtered based on the rules in the .gitignore files.
  src =
    pkgs.gitignoreSource ./.;

  nixpkgsVersion =
    import nix/nixpkgs-version.nix;

  pinnedPkgs =
    builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
      sha256 = nixpkgsVersion.tarballHash;
    };

  overlays =
    [
      (import nix/overlays/gitignore.nix)
    ];

  pkgs =
    import pinnedPkgs { inherit overlays; };

  drv =
    pkgs.haskellPackages.callCabal2nix name src {};
in
rec {
  inherit pkgs pinnedPkgs;

  postgrest =
    # Disable running the test suite on Nix builds, as they require a database
    # to be set up.
    pkgs.haskell.lib.dontCheck drv;

  env =
    drv.env;

  nixpkgsUpgrade =
    pkgs.callPackage nix/nixpkgs-upgrade.nix {};
}
