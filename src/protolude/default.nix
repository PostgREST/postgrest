{ pkgs ? import <nixpkgs> {}
}:
pkgs.haskellPackages.developPackage {
  root = ./.;
}
