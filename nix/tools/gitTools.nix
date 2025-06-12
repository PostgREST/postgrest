{ buildToolbox
, checkedShellScript
, commitlint
, writeText
}:
let
  commitlintConfig = writeText "commitlint.config.mjs" ''
    export default {

      /* Rules applied to commit messages */

      rules: {

        /* Rules format: [<severity>, <"always"/"never">, <value>] */

        "type-enum": [2, "always", [
            'add',      // Add a new feature
            'chore',    // Update sponsors, changelog, readme etc
            'ci',       // CI configuration files and scripts
            'docs',     // Documentation
            'drop',     // Drop a feature or fix
            'fix',      // Bug fix
            'nix',      // Related to Nix
            'perf',     // Performance improvements
            'refactor', // Refactoring code
            'remove',   // Remove a feature or fix
            'test',     // Adding tests
          ]],

          'subject-case':       [2, 'always', 'lower-case'],
          'subject-empty':      [2, 'never'],
          'subject-full-stop':  [2, 'never', '.'],
          'subject-max-length': [2, 'always', 60],
          'subject-min-length': [2, 'always', 2],


          'body-leading-blank': [1, 'always'],

          //'header-max-length':      [2, 'always', 72],
          //'header-min-length':      [2, 'always', 5],
          //'body-max-line-length':   [2, 'always', 100],
          //'body-case':              [1, 'always', 'sentence-case'],
          //'footer-leading-blank':   [1, 'always'],
          //'footer-max-line-length': [2, 'always', 100],
          //'scope-case':             [2, 'always', 'lower-case'],
          //'scope-max-length':       [2, 'always', 20],
          //'scope-empty':            [1, 'never'],
          //'references-empty':       [1, 'never'],
          //'signed-off-by':          [0, 'always', 'Signed-off-by:']]
      },
    };
  '';

  commitCheck =
    checkedShellScript
      {
        name = "postgrest-commitlint";
        docs = "Script to validate commit messages";
        workingDir = "/";
        args = [
          "ARG_OPTIONAL_SINGLE([from],, [commit ref start from], [origin/main])"
          "ARG_OPTIONAL_SINGLE([to],, [commit ref end at], [HEAD])"
        ];
      }
      ''
        # Run commitlint with the given configuration

        ${commitlint}/bin/commitlint --config ${commitlintConfig} --from "$_arg_from" --to "$_arg_to"
      '';
in
buildToolbox
{
  name = "postgrest-commitlint";
  tools = { inherit commitCheck; };
}
