with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "postgrest-docs";
  buildInputs = [
    python36Full
    python36Packages.sphinx
    python36Packages.sphinx_rtd_theme
    python36Packages.livereload ];
  shellHook = ''
    sphinx-build -b html -a -n . _build
    python reload_docs.py && exit
  '';
}
