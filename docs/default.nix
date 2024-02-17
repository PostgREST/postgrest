let
  # Commit of the Nixpkgs repository that we want to use.
  nixpkgsVersion = {
    date = "2023-03-25";
    rev = "dbf5322e93bcc6cfc52268367a8ad21c09d76fea";
    tarballHash = "0lwk4v9dkvd28xpqch0b0jrac4xl9lwm6snrnzx8k5lby72kmkng";
  };

  # Nix files that describe the Nixpkgs repository. We evaluate the expression
  # using `import` below.
  pkgs = import
    (fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
      sha256 = nixpkgsVersion.tarballHash;
    })
    { };

  python = pkgs.python3.withPackages (ps: [ ps.sphinx ps.sphinx_rtd_theme ps.livereload ps.sphinx-tabs ps.sphinx-copybutton ps.sphinxext-opengraph ]);
in
rec {
  inherit pkgs;

  build =
    pkgs.writeShellScriptBin "postgrest-docs-build"
      ''
        set -euo pipefail
        cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)/docs"

        # clean previous build, otherwise some errors might be supressed
        rm -rf _build

        ${python}/bin/sphinx-build --color -W -b html -a -n . _build
      '';

  serve =
    pkgs.writeShellScriptBin "postgrest-docs-serve"
      ''
        set -euo pipefail
        cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)/docs"

        # livereload_docs.py needs to find "sphinx-build"
        PATH=${python}/bin:$PATH

        ${python}/bin/python livereload_docs.py
      '';

  spellcheck =
    pkgs.writeShellScriptBin "postgrest-docs-spellcheck"
      ''
        set -euo pipefail
        cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)/docs"

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
        cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)/docs"

        FILES=$(find . -type f -iname '*.rst' | tr '\n' ' ')

        cat postgrest.dict \
         | tail -n+2 \
         | tr '\n' '\0' \
         | xargs -0 -n 1 -i \
           sh -c "grep \"{}\" $FILES > /dev/null || echo \"{}\"" \
         | tee unuseddict
        test ! -s unuseddict
      '';

  linkcheck =
    pkgs.writeShellScriptBin "postgrest-docs-linkcheck"
      ''
        set -euo pipefail
        cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)/docs"

        ${python}/bin/sphinx-build --color -b linkcheck . _build
      '';

  check =
    pkgs.writeShellScriptBin "postgrest-docs-check"
      ''
        set -euo pipefail
        ${build}/bin/postgrest-docs-build
        ${dictcheck}/bin/postgrest-docs-dictcheck
        ${linkcheck}/bin/postgrest-docs-linkcheck
        ${spellcheck}/bin/postgrest-docs-spellcheck
      '';
}
