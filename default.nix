let
  # Commit of the Nixpkgs repository that we want to use.
  nixpkgsVersion = {
    date = "2020-10-27";
    rev = "cd63096d6d887d689543a0b97743d28995bc9bc3";
    tarballHash = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
  };

  # Nix files that describe the Nixpkgs repository. We evaluate the expression
  # using `import` below.
  pkgs = import
    (fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
      sha256 = nixpkgsVersion.tarballHash;
    })
    { };

  python = pkgs.python3.withPackages (ps: [ ps.sphinx ps.sphinx_rtd_theme ps.livereload ]);
in
{
  inherit pkgs;

  build =
    pkgs.writeShellScriptBin "postgrest-docs-build"
      ''
        set -euo pipefail

        # clean previous build, otherwise some errors might be supressed
        rm -rf _build

        ${python}/bin/sphinx-build -W -b html -a -n . _build
      '';

  serve =
    pkgs.writeShellScriptBin "postgrest-docs-serve"
      ''
        set -euo pipefail

        # livereload_docs.py needs to find "sphinx-build"
        PATH=${python}/bin:$PATH

        ${python}/bin/python livereload_docs.py
      '';

  spellcheck =
    pkgs.writeShellScriptBin "postgrest-docs-spellcheck"
      ''
        set -euo pipefail

        FILES=$(find . -type f -iname '*.rst' | tr '\n' ' ')

        cat $FILES \
         | grep -v '^\(\.\.\|  \)' \
         | sed 's/`.*`//g' \
         | ${pkgs.aspell}/bin/aspell -d ${pkgs.aspellDicts.en}/lib/aspell/en_US -p ./postgrest.dict list \
         | sort -f \
         | tee misspellings
        test ! -s misspellings
      '';
}
