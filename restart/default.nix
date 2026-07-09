{ system ? builtins.currentSystem
, nixpkgs ? <nixpkgs>
, compiler ? null
, pkgs ? import nixpkgs { inherit system; }
, haskellPackages ? if compiler == null then pkgs.haskellPackages else pkgs.haskell.packages."${compiler}"
, lib ? pkgs.haskell.lib
}:

let
  src =
    pkgs.lib.sourceFilesBySuffices
      (pkgs.lib.cleanSource ./.)
      [ ".cabal" ".hs" "LICENSE" "README.md" ];

  restart =
    pkgs.lib.pipe (haskellPackages.callCabal2nix "restart" src { }) [
      lib.disableLibraryProfiling
      lib.disableSharedLibraries
    ];
in
{
  inherit restart src;
}
