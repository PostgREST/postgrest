{ aspell
, aspellDicts
, buildToolbox
, checkedShellScript
, imagemagick
, python3
, python3Packages
, texlive
, writers
, plantuml
}:
let
  selectPythonPackages = ps: [
    ps.sphinx
    ps.sphinx_rtd_theme
    ps.livereload
    ps.sphinx-tabs
    ps.sphinx-copybutton
    ps.sphinxext-opengraph
    (ps.callPackage ../docs-extensions/sphinx-rtd-dark-mode.nix { })
    # TODO: Remove override once new sphinx-intl version (> 2.1.0) is released and available in nixpkgs
    (ps.sphinx-intl.overrideAttrs (drv: { nativeBuildInputs = drv.nativeBuildInputs ++ [ ps.six ]; }))
  ];

  python = python3.withPackages selectPythonPackages;

  build =
    checkedShellScript
      {
        name = "postgrest-docs-build";
        docs = "Build the documentation.";
        args = [ "ARG_POSITIONAL_SINGLE([language], [Language to build docs for.], [\"\"])" ];
        workingDir = "/docs";
      }
      ''
        function build() {
          ${python}/bin/sphinx-build --color -W -a -n . -b "$@"
        }

        if [ "$_arg_language" == "" ]; then
          # clean previous build, otherwise some errors might be supressed
          rm -rf "../.docs-build/html/default"

          if [ -d languages ]; then
            # default to updating all existing locales
            build gettext ../.docs-build/gettext
            ${python}/bin/sphinx-intl update -p ../.docs-build/gettext
          fi

          build html "../.docs-build/html/default"
        else
          # clean previous build, otherwise some errors might be supressed
          rm -rf "../.docs-build/html/$_arg_language"

          # update and build specific locale, can be used to create new locale
          build gettext ../.docs-build/gettext
          ${python}/bin/sphinx-intl update -p ../.docs-build/gettext -l "$_arg_language"

          build html "../.docs-build/html/$_arg_language" -D "language=$_arg_language"
        fi
      '';

  render =
    let
      pdflatex = texlive.combine {
        inherit (texlive)
          amsmath
          booktabs
          cancel
          gensymb
          mathdots
          multirow
          pgf
          pgf-blur
          scheme-basic
          siunitx
          standalone
          yhmath
          ;
      };
    in
    checkedShellScript
      {
        name = "postgrest-docs-render";
        docs = "Render the diagrams.";
        workingDir = "/docs/_diagrams";
        withTmpDir = true;
      }
      ''
        ${pdflatex}/bin/pdflatex -halt-on-error -output-directory="$tmpdir" db.tex
        ${imagemagick}/bin/convert -density 300 "$tmpdir/db.pdf" ../_static/db.png

        ${plantuml}/bin/plantuml arch.uml -o ../_static
      '';

  server =
    writers.writePython3
      "postgrest-docs-server"
      { libraries = selectPythonPackages python3Packages; }
      ''
        import sys
        from livereload import Server, shell
        from subprocess import call

        build = sys.argv[1]
        locale = sys.argv[2]

        if locale == "":
            locale = "default"
        else:
            build += " " + locale

        call(build, shell=True)

        server = Server()
        server.watch("**/*.rst", shell(build))
        server.watch(f"locales/{locale}/LC_MESSAGES/*.po", shell(build))
        server.serve(root=f"../.docs-build/html/{locale}")
      '';

  serve =
    checkedShellScript
      {
        name = "postgrest-docs-serve";
        docs = "Serve the documentation locally with live reload.";
        args = [ "ARG_POSITIONAL_SINGLE([language], [Language to serve docs for.], [\"\"])" ];
        workingDir = "/docs";
      }
      ''
        ${server} ${build} "$_arg_language"
      '';

  spellcheck =
    checkedShellScript
      {
        name = "postgrest-docs-spellcheck";
        docs = "Verify spelling mistakes. Bypass if the word is present in postgrest.dict.";
        workingDir = "/docs";
      }
      ''
        FILES=$(find . -type f -iname '*.rst' | tr '\n' ' ')

        # shellcheck disable=SC2086 disable=SC2016
        cat $FILES \
         | grep -v '^\(\.\.\|  \)' \
         | sed -E 's/`+[^`]+`+//g' \
         | ${aspell}/bin/aspell -d ${aspellDicts.en}/lib/aspell/en_US -p ./postgrest.dict list \
         | sort -f \
         | tee misspellings
        test ! -s misspellings
      '';

  dictcheck =
    checkedShellScript
      {
        name = "postgrest-docs-dictcheck";
        docs = "Detect obsolete entries in postgrest.dict that are not used anymore.";
        workingDir = "/docs";
      }
      ''
        FILES=$(find . -type f -iname '*.rst' | tr '\n' ' ')

        tail -n+2 postgrest.dict \
         | tr '\n' '\0' \
         | xargs -0 -i \
           sh -c "grep \"{}\" $FILES > /dev/null || echo \"{}\"" \
         | tee unuseddict
        test ! -s unuseddict
      '';

  linkcheck =
    checkedShellScript
      {
        name = "postgrest-docs-linkcheck";
        docs = "Verify that external links are working correctly.";
        workingDir = "/docs";
      }
      ''
        ${python}/bin/sphinx-build --color -b linkcheck . ../.docs-build
      '';

  check =
    checkedShellScript
      {
        name = "postgrest-docs-check";
        docs = "Build and run all the validation scripts.";
        workingDir = "/docs";
      }
      ''
        ${build}
        ${dictcheck}
        ${spellcheck}
      '';

in
buildToolbox
{
  name = "postgrest-docs";
  tools = {
    inherit
      build
      check
      dictcheck
      linkcheck
      render
      serve
      spellcheck;
  };
}
