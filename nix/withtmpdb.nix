# Wrap the `test/with_tmp_db` script with the required dependencies from Nix.
{ git
, runtimeShell
, writeShellScript
}:

postgresql:
writeShellScript "postgrest-test-${postgresql.name}"
  ''
    set -euo pipefail

    export PATH=${postgresql}/bin:${git}/bin:${runtimeShell}/bin:"$PATH"

    exec ${../test/with_tmp_db} "$@"
  ''
