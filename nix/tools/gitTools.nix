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

  changelogCheck =
    checkedShellScript
      {
        name = "postgrest-changelog-check";
        docs = "Script to validate changelog entries";
        workingDir = "/";
        args = [
          "ARG_OPTIONAL_SINGLE([base],, [base commit ref], [origin/main])"
          "ARG_OPTIONAL_SINGLE([head],, [head commit ref], [HEAD])"
        ];
      }
      ''
        # Get all commit hashes and store in ADD_OR_FIX_COMMITS array
        mapfile -t ADD_OR_FIX_COMMITS < <(git log "$_arg_base".."$_arg_head" --format=%H -E --grep="^(add|fix)")

        # Loop over each "add:" or "fix:" commit and compare the changelog
        # of those commits with the base(main) commit.
        for add_or_fix_commit in "''${ADD_OR_FIX_COMMITS[@]}"; do
          
          # Get Unreleased section of the selected 'add: ...' or 'fix: ...' commit
          commit_changelog=$(
            git show "$add_or_fix_commit":CHANGELOG.md | \
              sed -n "1,/## Unreleased/d;/## \[/q;p" || true  # true is added so this command always exits with 0 code
          )

          # Get Unreleased section of the base commit changelog
          base_changelog=$(
            git show "$_arg_base":CHANGELOG.md | \
              sed -n "1,/## Unreleased/d;/## \[/q;p" || true  # true is added so this command always exits with 0 code
          )

          # Compare the unreleased section of base commit and the selected
          # commit. If there is a diff, then we assume that a changelog entry
          # is added.

          if diff -q <(echo "$commit_changelog") <(echo "$base_changelog") >/dev/null; then
            echo "No change in CHANGELOG.md observed as compared to base commit."
            echo "A changelog entry must be added in the Unreleased section of CHANGELOG.md for 'add: ...' or 'fix: ...' commits."
            exit 1
          else
            echo "Changelog added in 'Unreleased' section, see diff:"
            diff <(echo "$commit_changelog") <(echo "$base_changelog") || true
          fi

        done
      '';
in
buildToolbox
{
  name = "postgrest-commitlint";
  tools = { inherit commitCheck changelogCheck; };
}
