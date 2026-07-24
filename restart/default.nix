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
      (pkgs.lib.cleanSourceWith {
        src = pkgs.lib.cleanSource ./.;
        filter = path: _: !(pkgs.lib.hasPrefix "${toString ./test}/" (toString path));
      })
      [ ".cabal" ".hs" "LICENSE" "README.md" ];

  restart =
    pkgs.lib.pipe (haskellPackages.callCabal2nix "restart" src { }) [
      lib.disableLibraryProfiling
      lib.disableSharedLibraries
    ];

  processRestartTestAppSrc =
    pkgs.lib.sourceFilesBySuffices
      ./test
      [ ".cabal" ".hs" ];

  processRestartTestApp =
    pkgs.lib.pipe (haskellPackages.callCabal2nix "process-restart-test-app" processRestartTestAppSrc { inherit restart; }) [
      lib.dontCheck
      lib.disableLibraryProfiling
      lib.disableSharedLibraries
    ];

  nixos-lib = import (pkgs.path + "/nixos/lib") { };
in
{
  inherit processRestartTestApp restart src;
} // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
  process-restart-systemd-test = (nixos-lib.runTest {
    hostPkgs = pkgs;
    defaults.nixpkgs.overlays = [ (_: _: { inherit processRestartTestApp; }) ];
    defaults.documentation.enable = pkgs.lib.mkDefault false;
    imports = [ ./test/process-restart-systemd.nix ];
  }).config.result;
}
