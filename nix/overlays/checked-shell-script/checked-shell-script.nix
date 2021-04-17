# Create a bash script that is checked with shellcheck. You can either use it
# directly, or use the .bin attribute to get the script in a bin/ directory,
# to be used in a path for example.
{ argbash
, bash_5
, git
, lib
, runCommand
, shellcheck
, stdenv
, writeTextFile
}:
{ name
, docs
, args ? [ ]
, addCommandCompletion ? false
, inRootDir ? false
, redirectTixFiles ? true
, withEnv ? null
, withTmpDir ? false
}: text:
let
  argsTemplate =
    let
      # square brackets are a pain to escape - if even possible. just don't use them...
      escapedDocs = builtins.replaceStrings [ "\n" ] [ " \\n" ] docs;
    in
    writeTextFile {
      inherit name;
      destination = "/${name}.m4"; # destination is needed to have the proper basename for completion

      text =
        ''
          # BASH_ARGV0 sets $0 - which is used in parser.sh for usage information
          # stripping the /nix/store/... path for nicer display
          BASH_ARGV0="$(basename "$0")"

          # ARG_HELP([${name}], [${escapedDocs}])
          ${lib.strings.concatMapStrings (arg: "# " + arg) args}
          # ARG_DEFAULTS_POS()
          # ARGBASH_GO

        '';
    };

  argsParser =
    runCommand "${name}-parser" { }
      "${argbash}/bin/argbash -o $out ${argsTemplate}/${name}.m4";

  bashCompletion =
    runCommand "${name}-completion" { } (
      ''
        ${argbash}/bin/argbash --type completion --strip all ${argsTemplate}/${name}.m4 > $out
      ''

      + lib.optionalString addCommandCompletion ''
        sed 's/COMPREPLY.*compgen -o bashdefault .*$/_command/' -i $out
      ''
    );

  bin =
    writeTextFile {
      inherit name;
      executable = true;
      destination = "/bin/${name}";

      text =
        ''
          #!${bash_5}/bin/bash
          set -euo pipefail

          source ${argsParser}
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

        + lib.optionalString (withEnv != null) ''
          env="$(cat ${withEnv})"
          export PATH="$env/bin:$PATH"
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
          ${shellcheck}/bin/shellcheck -x $out/bin/${name}
        '';
    };

  script =
    runCommand name { inherit bin name; } "ln -s $bin/bin/$name $out";
in
script // { inherit bin bashCompletion; }
