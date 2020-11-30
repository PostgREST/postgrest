# Create a bash script that is checked with shellcheck. You can either use it
# directly, or use the .bin attribute to get the script in a bin/ directory,
# to be used in a path for example.
{ writeTextFile
, runtimeShell
, runCommand
, stdenv
, shellcheck
}:
name: text:
let
  writeBin =
    name: text:
    writeTextFile {
      inherit name;
      executable = true;
      destination = "/bin/${name}";

      text =
        ''
          #!${runtimeShell}
          set -euo pipefail

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

  bin =
    writeBin name text;

  script =
    runCommand name { inherit bin name; } "ln -s $bin/bin/$name $out";
in
script // { inherit bin; }
