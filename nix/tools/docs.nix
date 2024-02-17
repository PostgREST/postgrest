{ aspell
, aspellDicts
, buildToolbox
, checkedShellScript
, python3
, python3Packages
, writers
}:
let
  selectPythonPackages = ps: [
    ps.sphinx
    ps.sphinx_rtd_theme
    ps.livereload
    ps.sphinx-tabs
    ps.sphinx-copybutton
    ps.sphinxext-opengraph
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
          rm -rf "_build/html/default"

          if [ -d languages ]; then
            # default to updating all existing locales
            build gettext _build/gettext
            ${python}/bin/sphinx-intl update -p _build/gettext
          fi

          build html "_build/html/default"
        else
          # clean previous build, otherwise some errors might be supressed
          rm -rf "_build/html/$_arg_language"

          # update and build specific locale, can be used to create new locale
          build gettext _build/gettext
          ${python}/bin/sphinx-intl update -p _build/gettext -l "$_arg_language"

          build html "_build/html/$_arg_language" -D "language=$_arg_language"
        fi
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
        server.serve(root=f"_build/html/{locale}")
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
         | sed 's/`.*`//g' \
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
        ${python}/bin/sphinx-build --color -b linkcheck . _build
      '';

  check =
    checkedShellScript
      {
        name = "postgrest-docs-check";
        docs = "Build and run all the validation scripts.";
        workingDir = "/docs";
      }
      ''
        ${build}/bin/postgrest-docs-build
        ${dictcheck}/bin/postgrest-docs-dictcheck
        ${linkcheck}/bin/postgrest-docs-linkcheck
        ${spellcheck}/bin/postgrest-docs-spellcheck
      '';

in
buildToolbox
{
  name = "postgrest-docs";
  tools =
    [
      build
      check
      dictcheck
      linkcheck
      serve
      spellcheck
    ];
}
