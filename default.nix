let
  # Commit of the Nixpkgs repository that we want to use.
  nixpkgsVersion = {
    date = "2024-01-06";
    rev = "4bbf5a2eb6046c54f7a29a0964c642ebfe912cbc";
    tarballHash = "03p45qdcxqxc41mmzmmyzbkff29vv95vv643z0kd3mf1s2nnsy5b";
  };

  # Nix files that describe the Nixpkgs repository. We evaluate the expression
  # using `import` below.
  pkgs = import
    (fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
      sha256 = nixpkgsVersion.tarballHash;
    })
    { };

  python = pkgs.python3.withPackages (ps: [
    ps.sphinx
    ps.sphinx_rtd_theme
    ps.livereload
    ps.sphinx-tabs
    ps.sphinx-copybutton
    ps.sphinxext-opengraph
    # TODO: Remove override once new sphinx-intl version (> 2.1.0) is released and available in nixpkgs
    (ps.sphinx-intl.overrideAttrs (drv: { nativeBuildInputs = drv.nativeBuildInputs ++ [ ps.six ]; }))
  ]);
in
rec {
  inherit pkgs;

  build =
    pkgs.writeShellScriptBin "postgrest-docs-build"
      ''
        set -euo pipefail

        # build.sh needs to find "sphinx-build"
        PATH=${python}/bin:$PATH

        ./build.sh "$@"
      '';

  serve =
    pkgs.writeShellScriptBin "postgrest-docs-serve"
      ''
        set -euo pipefail

        # livereload_docs.py needs to find "sphinx-build"
        PATH=${python}/bin:$PATH

        ./livereload_docs.py "$@"
      '';

  spellcheck =
    pkgs.writeShellScriptBin "postgrest-docs-spellcheck"
      ''
        set -euo pipefail

        FILES=$(find docs -type f -iname '*.rst' | tr '\n' ' ')

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

        FILES=$(find docs -type f -iname '*.rst' | tr '\n' ' ')

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

        ${python}/bin/sphinx-build --color -b linkcheck docs _build
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
