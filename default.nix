with import (builtins.fetchGit {
  url = https://github.com/NixOS/nixpkgs-channels;
  ref = "nixos-18.09-small";
  rev = "95fed28ac372c61eb83c87ad97c24b0f957827bf";
}) {};

stdenv.mkDerivation {
  name = "postgrest-docs";
  buildInputs = [
    python36Full
    python36Packages.sphinx
    python36Packages.sphinx_rtd_theme
    python36Packages.livereload ];
  shellHook = ''
    python livereload_docs.py && exit
  '';
}
