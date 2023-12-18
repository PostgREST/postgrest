{ aspell
, aspellDicts
, buildToolbox
, checkedShellScript
, python3
}:
let
  python = python3.withPackages (ps: [
    ps.sphinx
    ps.sphinx_rtd_theme
    ps.livereload
    ps.sphinx-tabs
    ps.sphinx-copybutton
    ps.sphinxext-opengraph
    # TODO: Remove override once new sphinx-intl version (> 2.1.0) is released and available in nixpkgs
    (ps.sphinx-intl.overrideAttrs (drv: { nativeBuildInputs = drv.nativeBuildInputs ++ [ ps.six ]; }))
  ]);

  build =
    checkedShellScript
      {
        name = "postgrest-docs-build";
        docs = "Build the documentation.";
        args = [ "ARG_POSITIONAL_SINGLE([language], [Language to build docs for.], [\"\"])" ];
        workingDir = "/docs";
      }
      ''
        # build.sh needs to find "sphinx-build"
        PATH=${python}/bin:$PATH

        ./build.sh "$_arg_language"
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
        # livereload_docs.py needs to find "sphinx-build"
        PATH=${python}/bin:$PATH

        ./livereload_docs.py "$_arg_language"
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
