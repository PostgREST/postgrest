# Create a bash script that is checked with shellcheck. You can either use it
# directly, or use the .bin attribute to get the script in a bin/ directory,
# to be used in a path for example.
{ git
, runCommand
, runtimeShell
, shellcheck
, stdenv
, writeTextFile
}:
name: text:
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

          cd "$(${git}/bin/git rev-parse --show-toplevel)"

          if test ! -f postgrest.cabal; then
            >&2 echo "Couldn't find postgrest.cabal. Please make sure to" \
                     "run this command somewhere in the PostgREST repo."
            exit 1
          fi

          ${text}
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
