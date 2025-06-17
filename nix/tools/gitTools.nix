{ buildToolbox
, checkedShellScript
, commitlint
, writeText
}:
let
  # Rules format: [<severity>, <"always"/"never">, <value>]
  commitlintConfig = writeText "commitlint.config.mjs" ''
    export default {
      rules: {
        "type-enum": [2, "always", [
            'add',      // Add a new feature
            'amend',    // To amend an unrealease commit
            'change',   // Breaking changes
            'chore',    // Update sponsors, changelog, readme etc
            'ci',       // CI configuration files and scripts
            'docs',     // Documentation
            'fix',      // Bug fix
            'nix',      // Related to Nix
            'perf',     // Performance improvements
            'refactor', // Refactoring code
            'remove',   // Remove a feature or fix
            'test',     // Adding tests
          ]],

          'subject-case':       [2, 'never', ['pascal-case', 'start-case']],
          'subject-empty':      [2, 'never'],
          'subject-full-stop':  [2, 'never', '.'],
          'subject-max-length': [2, 'always', 80],
          'subject-min-length': [2, 'always', 5],

          'scope-case':         [2, 'always', 'lower-case'],

          'body-leading-blank': [2, 'always'],
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
          "ARG_OPTIONAL_SINGLE([from],, [commit ref start from], [main])"
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
