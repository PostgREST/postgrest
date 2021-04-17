{ buildEnv
, checkedShellScript
, lib
, postgresqlVersions
}:
let
  # Wrap the `test/with_tmp_db` script with the required dependencies from Nix.
  withTmpDb =
    { name, postgresql }:
    checkedShellScript
      {
        name = "postgrest-with-${name}";
        docs = "Run the given command in a temporary database with ${name}";
        inRootDir = true;
        redirectTixFiles = false;
      }
      ''
        # avoid starting multiple layers of with_tmp_db
        if test ! -v PGRST_DB_URI; then
          export PATH=${postgresql}/bin:"$PATH"

          exec ${../test/with_tmp_db} "$@"
        else
          "$@"
        fi
      '';

  # Helper script for running a command against all PostgreSQL versions.
  withAll =
    let
      runners =
        builtins.map
          (pg:
            ''
              cat << EOF

              Running against ${pg.name}...

              EOF

              trap 'echo "Failed on ${pg.name}"' exit

              ${withTmpDb pg} "$@"

              trap "" exit

              cat << EOF

              Done running against ${pg.name}.

              EOF
            '')
          postgresqlVersions;
    in
    checkedShellScript
      {
        name = "postgrest-with-all";
        docs = "Run command against all supported PostgreSQL versions.";
        inRootDir = true;
      }
      (lib.concatStringsSep "\n\n" runners);

  # Create a `postgrest-with-postgresql-` for each PostgreSQL version
  withVersions = builtins.map withTmpDb postgresqlVersions;

in
buildEnv
  {
    name =
      "postgrest-with";

    paths =
      [
        withAll.bin
      ] ++ (builtins.map (v: v.bin) withVersions);
  }
  # make withTools.latest available for other nix files
  // {
  latest = withTmpDb (builtins.head postgresqlVersions);
}
