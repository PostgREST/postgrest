let
  # Commit of the Nixpkgs repository that we want to use.
  nixpkgsVersion = {
    date = "2021-06-02";
    rev = "84aa23742f6c72501f9cc209f29c438766f5352d";
    tarballHash = "0h7xl6q0yjrbl9vm3h6lkxw692nm8bg3wy65gm95a2mivhrdjpxp";
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

  # dictcheck detects obsolete entries in postgrest.dict, that are not used anymore
  dictcheck =
    pkgs.writeShellScriptBin "postgrest-docs-dictcheck"
      ''
        set -euo pipefail

        FILES=$(find . -type f -iname '*.rst' | tr '\n' ' ')

        cat postgrest.dict \
         | tail -n+2 \
         | tr '\n' '\0' \
         | xargs -0 -n 1 -i \
           sh -c "grep \"{}\" $FILES > /dev/null || echo \"{}\""
      '';

  linkcheck =
    pkgs.writeShellScriptBin "postgrest-docs-linkcheck"
      ''
        set -euo pipefail

        ${python}/bin/sphinx-build -b linkcheck . _build
      '';
}
