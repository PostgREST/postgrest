# Create a bash script that is checked with shellcheck. You can either use it
# directly, or use the .bin attribute to get the script in a bin/ directory,
# to be used in a path for example.
{ git
, lib
, runCommand
, runtimeShell
, shellcheck
, stdenv
, writeTextFile
}:
{ name, docs, inRootDir ? false, redirectTixFiles ? true, withTmpDir ? false }: text:
# TODO: do something sensible with docs, e.g. provide automated --help
let
  bin =
    writeTextFile {
      inherit name;
      executable = true;
      destination = "/bin/${name}";

      text =
        ''
          #!${runtimeShell}
          set -euo pipefail
        ''
        + lib.optionalString redirectTixFiles ''
          # storing tix files in a temporary throw away directory avoids mix/tix conflicts after changes
          hpctixdir=$(mktemp -d)
          export HPCTIXFILE="$hpctixdir"/postgrest.tix
          trap 'rm -rf $hpctixdir' EXIT
        ''
        + lib.optionalString inRootDir ''
          cd "$(${git}/bin/git rev-parse --show-toplevel)"

          if test ! -f postgrest.cabal; then
            >&2 echo "Couldn't find postgrest.cabal. Please make sure to" \
                     "run this command somewhere in the PostgREST repo."
            exit 1
          fi
        ''
        + lib.optionalString withTmpDir ''
          tmpdir="$(mktemp -d)"

          # we keep the tmpdir when an error occurs for debugging
          trap 'echo Temporary directory kept at: $tmpdir' ERR
          # remove the tmpdir when cancelled (postgrest-watch)
          trap 'rm -rf "$tmpdir"' SIGINT SIGTERM
        ''
        + "(${text})"
        + lib.optionalString withTmpDir ''

          rm -rf "$tmpdir"
        '';

      checkPhase =
        ''
          # check syntax
          ${stdenv.shell} -n $out/bin/${name}

          # check for shellcheck recommendations
          ${shellcheck}/bin/shellcheck $out/bin/${name}
        '';
    };

  script =
    runCommand name { inherit bin name; } "ln -s $bin/bin/$name $out";
in
script // { inherit bin; }
