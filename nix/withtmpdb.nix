# Wrap the `test/with_tmp_db` script with the required dependencies from Nix.
{ checkedShellScript }:

postgresql:
checkedShellScript
{
  name = "postgrest-test-withtmpdb-${postgresql.name}";
  docs = "Run the given command in a temporary database";
  inRootDir = true;
}
  ''
    # avoid starting multiple layers of with_tmp_db
    if test ! -v PGRST_DB_URI; then
      export PATH=${postgresql}/bin:"$PATH"

      exec test/with_tmp_db "$@"
    else
      "$@"
    fi
  ''
