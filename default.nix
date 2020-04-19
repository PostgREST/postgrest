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
in
rec {
  inherit pkgs pinnedPkgs;

  postgrest =
    pkgs.haskellPackages.callCabal2nixWithOptions name src "--no-check" {};

  nixpkgsUpgrade =
    pkgs.callPackage nix/nixpkgs-upgrade.nix {};
}
