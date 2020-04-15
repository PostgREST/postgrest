{ postgresql
, postgresqlVersions
, runtimeShell
, writeShellScript
, writeShellScriptBin
, cabal-install
, buildEnv
}:
let
  withTestEnv =
    postgresql:
      writeShellScript "postgrest-test-${postgresql.name}"
        ''
          set -euo pipefail

          # All data will be stored in a temporary directory.
          tmpdir="$(mktemp -d)"

          trap "rm -rf tmpdir" exit

          export PGDATA="$tmpdir"
          export PGHOST="$tmpdir"
          export PGUSER=postgrest_test_authenticator
          export PGDATABASE=postgres
          export DB_URI="postgresql://$PGDATABASE?host=$PGDATA&user=$PGUSER"
          export POSTGREST_TEST_CONNECTION="$DB_URI"

          # Initialize a database cluster. We try to make it as independent as possible
          # from the host by specifying the timezone, locale and encoding.
          TZ=UTC ${postgresql}/bin/initdb --no-locale --encoding=UTF8 \
            --nosync -U "$PGUSER" --auth=trust > /dev/null

          # Start the database cluster. Instead of listening on a local port, we will
          # listen on a unix domain socket. We will reuse the $PGDATA directory for that
          # socket.
          ${postgresql}/bin/pg_ctl start \
            -o "-F -c listen_addresses=\"\" -k $PGDATA" > /dev/null

          # We need to wait for the cluster to be ready on older versions of
          # Postgres (< 10)
          until ${postgresql}/bin/pg_isready > /dev/null; do
            sleep 0.1
          done

          stop() {
              ${postgresql}/bin/pg_ctl stop -m i > /dev/null
              rm -rf "$tmpdir"
          }

          trap stop exit

          # Prepare the database for running the tests.
          ${postgresql}/bin/psql > /dev/null << EOF
              create extension pgcrypto;
              alter database $PGDATABASE set request.jwt.claim.id = '-1';
              alter role $PGUSER set default_text_search_config to english;
          EOF

          export PATH="${postgresql}/bin:$PATH"

          ${runtimeShell} -c "$@"
        '';

  test =
    writeShellScriptBin "postgrest-test"
      ''
        ${withTestEnv postgresql} "${cabal-install}/bin/cabal v2-test"
      '';
in
buildEnv {
  name = "postgrest-tests";
  paths = [ test ];
}
