{ buildToolbox
, checkedShellScript
, jq
, python3Packages
, vegeta
, withTools
, writers
}:
let
  runner =
    checkedShellScript
      {
        name = "postgrest-loadtest-runner";
        docs = "Run vegeta. Assume PostgREST to be running.";
        args = [
          "ARG_LEFTOVERS([additional vegeta arguments])"
          "ARG_USE_ENV([PGRST_SERVER_UNIX_SOCKET], [], [Unix socket to connect to running PostgREST instance])"
        ];
      }
      ''
        # ARG_USE_ENV only adds defaults or docs for environment variables
        # We manually implement a required check here
        # See also: https://github.com/matejak/argbash/issues/80
        : "''${PGRST_SERVER_UNIX_SOCKET:?PGRST_SERVER_UNIX_SOCKET is required}"

        ${vegeta}/bin/vegeta -cpus 1 attack \
                                     -unix-socket "$PGRST_SERVER_UNIX_SOCKET" \
                                     -max-workers 1 \
                                     -workers 1 \
                                     -rate 0 \
                                     -duration 60s \
                                     "''${_arg_leftovers[@]}"
      '';

  loadtest =
    checkedShellScript
      {
        name = "postgrest-loadtest";
        docs = "Run the vegeta loadtests with PostgREST.";
        args = [
          "ARG_OPTIONAL_SINGLE([output], [o], [Filename to dump json output to], [./loadtest/result.bin])"
          "ARG_OPTIONAL_SINGLE([testdir], [t], [Directory to load tests and fixtures from], [./test/load])"
          "ARG_LEFTOVERS([additional vegeta arguments])"
        ];
        inRootDir = true;
      }
      ''
        # previously required settings to make this work with older branches
        export PGRST_DB_ANON_ROLE="postgrest_test_anonymous"
        export PGRST_DB_URI="postgresql://"
        export PGRST_DB_SCHEMAS="test"

        export PGRST_DB_CONFIG="false"
        export PGRST_DB_POOL="1"
        export PGRST_DB_TX_END="rollback-allow-override"
        export PGRST_LOG_LEVEL="crit"

        mkdir -p "$(dirname "$_arg_output")"
        abs_output="$(realpath "$_arg_output")"

        # shellcheck disable=SC2145
        ${withTools.withPg} --fixtures "$_arg_testdir"/fixtures.sql \
        ${withTools.withSlowPg} \
        ${withTools.withPgrst} \
        ${withTools.withSlowPgrst} \
        sh -c "cd \"$_arg_testdir\" && ${runner} -targets targets.http -output \"$abs_output\" \"''${_arg_leftovers[@]}\""
        ${vegeta}/bin/vegeta report -type=text "$_arg_output"
      '';

  loadtestAgainst =
    let
      name = "postgrest-loadtest-against";
    in
    checkedShellScript
      {
        inherit name;
        docs =
          ''
            Run the vegeta loadtest twice:
            - once on the <target> branch
            - once in the current worktree
          '';
        args = [
          "ARG_POSITIONAL_SINGLE([target], [Commit-ish reference to compare with])"
          "ARG_LEFTOVERS([additional vegeta arguments])"
        ];
        positionalCompletion =
          ''
            if test "$prev" == "${name}"; then
              __gitcomp_nl "$(__git_refs)"
            fi
          '';
        inRootDir = true;
      }
      ''
        cat << EOF

        Running loadtest on "$_arg_target"...

        EOF

        # Runs the test files from the current working tree
        # to make sure both tests are run with the same files.
        # Save the results in the current working tree, too,
        # otherwise they'd be lost in the temporary working tree
        # created by withTools.withGit.
        ${withTools.withGit} "$_arg_target" ${loadtest} --output "$PWD/loadtest/$_arg_target.bin" --testdir "$PWD/test/load" "''${_arg_leftovers[@]}"

        cat << EOF

        Done running on "$_arg_target".

        EOF

        cat << EOF

        Running loadtest on HEAD...

        EOF

        ${loadtest} --output "$PWD/loadtest/head.bin" --testdir "$PWD/test/load" "''${_arg_leftovers[@]}"

        cat << EOF

        Done running on HEAD.

        EOF
      '';

  reporter =
    checkedShellScript
      {
        name = "postgrest-loadtest-reporter";
        docs = "Create a named json report for a single result file.";
        args = [
          "ARG_POSITIONAL_SINGLE([file], [Filename of result to create report for])"
          "ARG_LEFTOVERS([additional vegeta arguments])"
        ];
        inRootDir = true;
      }
      ''
        ${vegeta}/bin/vegeta report -type=json "$_arg_file" \
          | ${jq}/bin/jq --arg branch "$(basename "$_arg_file" .bin)" '. + {branch: $branch}'
      '';

  toMarkdown =
    writers.writePython3 "postgrest-loadtest-to-markdown"
      {
        libraries = [ python3Packages.pandas python3Packages.tabulate ];
      }
      ''
        import sys
        import pandas as pd

        pd.read_json(sys.stdin) \
          .set_index('param') \
          .drop(['branch', 'earliest', 'end', 'latest']) \
          .convert_dtypes() \
          .to_markdown(sys.stdout, floatfmt='.0f')
      '';


  report =
    checkedShellScript
      {
        name = "postgrest-loadtest-report";
        docs = "Create a report of all loadtest reports as markdown.";
        inRootDir = true;
      }
      ''
        find loadtest -type f -iname '*.bin' -exec ${reporter} {} \; \
          | ${jq}/bin/jq '[leaf_paths as $path | {param: $path | join("."), (.branch): getpath($path)}]' \
          | ${jq}/bin/jq --slurp 'flatten | group_by(.param) | map(add)' \
          | ${toMarkdown}
      '';

in
buildToolbox {
  name = "postgrest-loadtest";
  tools = [ loadtest loadtestAgainst report ];
}
