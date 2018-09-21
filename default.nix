with import <nixpkgs> {};

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
