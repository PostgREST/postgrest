# The compiler version is set to be identical to the one from the stack
# resolver by default.
{ compiler ? "ghc883" }:
let
  name =
    "postgrest";

  src =
    pkgs.gitignoreSource ./.;

  nixpkgsVersion =
    import nix/nixpkgs-version.nix;

  pinnedPkgs =
    builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
      sha256 = nixpkgsVersion.tarballHash;
    };

  overlays = [
    (import nix/overlays/gitignore.nix)
    (import nix/overlays/haskell-packages { inherit compiler; })
  ];

  pkgs =
    import pinnedPkgs { inherit overlays; };

  postgresqlVersions =
    map postgresqlWithPackages
      [
        pkgs.postgresql_12
        pkgs.postgresql_11
        pkgs.postgresql_10
        pkgs.postgresql_9_6
        pkgs.postgresql_9_5
      ];

  postgresqlWithPackages =
    postgresql:
      postgresql.withPackages (p: [ p.pgjwt ]);
in
rec {
  inherit pkgs pinnedPkgs;

  postgrest =
    pkgs.haskellPackages.callCabal2nixWithOptions name src "--no-check" {};

  tests =
    pkgs.callPackage nix/tests.nix { inherit postgresqlVersions pinnedPkgs; };

  style =
    pkgs.callPackage nix/style.nix {};

  lint =
    pkgs.callPackage nix/lint.nix {};

  nixpkgsUpdate =
    pkgs.callPackage nix/nixpkgs-update.nix {};
}
