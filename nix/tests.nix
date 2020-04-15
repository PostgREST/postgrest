{ postgresqlVersions
, runtimeShell
, writeShellScript
, writeShellScriptBin
, cabal-install
, buildEnv
, git
, ncat
, stack
, curl
, lib
, pinnedPkgs
}:
let
  withTestEnv =
    postgresql:
      writeShellScript "postgrest-test-${postgresql.name}"
        ''
          set -euo pipefail

          rootdir="$(${git}/bin/git rev-parse --show-toplevel)"

          # All data will be stored in a temporary directory.
          tmpdir="$(mktemp -d)"

          trap "rm -rf tmpdir" exit

          mkdir -p "$tmpdir"/{db,socket}

          export PGDATA="$tmpdir/db"
          export PGHOST="$tmpdir/socket"
          export PGUSER=postgrest_test_authenticator
          export PGDATABASE=postgres
          export DB_URI="postgresql://$PGDATABASE?host=$PGHOST&user=$PGUSER"
          export POSTGREST_TEST_CONNECTION="$DB_URI"

          # Initialize a database cluster. We try to make it as independent as possible
          # from the host by specifying the timezone, locale and encoding.
          TZ=UTC ${postgresql}/bin/initdb --no-locale --encoding=UTF8 \
            --nosync -U "$PGUSER" --auth=trust > "$tmpdir/initdb.log"

          # Start the database cluster. Instead of listening on a local port, we will
          # listen on a unix domain socket.
          ${postgresql}/bin/pg_ctl -l "$tmpdir/db.log" start \
            -o "-F -c listen_addresses=\"\" -k $PGHOST"

          # We need to wait for the cluster to be ready on older versions of
          # Postgres (< 10)
          until ${postgresql}/bin/pg_isready > "$tmpdir/wait.log"; do
            sleep 0.1
          done

          stop() {
              ${postgresql}/bin/pg_ctl stop -m i > "$tmpdir/stop.log"
              rm -rf "$tmpdir"
          }

          trap stop exit

          # Prepare the database for running the tests.
          ${postgresql}/bin/psql -v ON_ERROR_STOP=1 > "$tmpdir/fixtures.log" <<EOF
              create extension pgcrypto;
              alter database $PGDATABASE set request.jwt.claim.id = '-1';
              alter role $PGUSER set default_text_search_config to english;
          EOF

          loadfixture() {
            ${postgresql}/bin/psql -q -v ON_ERROR_STOP=1 \
              -f "$rootdir/test/fixtures/$1.sql" \
              > "$tmpdir/fixtures.log"
          }

          loadfixture database
          loadfixture roles
          loadfixture schema
          loadfixture jwt
          loadfixture jsonschema
          loadfixture privileges

          export PATH="${postgresql}/bin:$PATH"

          ${runtimeShell} -c "$@"
        '';

  testSpec =
    name: postgresql:
      writeShellScriptBin name
        ''
          set -euo pipefail

          ${withTestEnv postgresql} "${cabal-install}/bin/cabal v2-test"
        '';

  testIO =
    name: postgresql:
      writeShellScriptBin name
        ''
          set -euo pipefail

          rootdir="$(${git}/bin/git rev-parse --show-toplevel)"
          cd "$rootdir"

          tmpdir="$(mktemp -d)"
          trap "rm -rf $tmpdir" exit
          ${cabal-install}/bin/cabal v2-install --installdir="$tmpdir"

          echo $(find $tmpdir)

          export PATH="${curl}/bin:${ncat}/bin:$PATH"
          export POSTGREST_TEST_POSTGREST_CMD="$tmpdir/postgrest"

          ${withTestEnv postgresql} "$rootdir"/test/io-tests.sh
        '';

  testMemory =
    name: postgresql:
      writeShellScriptBin name
        ''
          set -euo pipefail

          rootdir="$(${git}/bin/git rev-parse --show-toplevel)"
          cd "$rootdir"

          echo $(find $tmpdir)

          export PATH="${curl}/bin:${stack}/bin:$PATH"

          # need to set the Nix path for stack Nix integration
          export NIX_PATH="nixpath=${pinnedPkgs}"
          export POSTGREST_TEST_STACK_CMD="stack --nix"

          ${withTestEnv postgresql} "$rootdir/test/memory-tests.sh"
        '';

  defaultPostgresql =
    builtins.head postgresqlVersions;

  testSpecVersions =
    map
      (postgresql: testSpec "postgrest-test-spec-${postgresql.name}" postgresql)
      postgresqlVersions;

  testSpecAllVersions =
    writeShellScriptBin "postgrest-test-spec-all"
      ''
        set -euo pipefail

        ${lib.concatStringsSep "\n" (
        map
          (test: "${test}/bin/${test.name}")
          testSpecVersions
      )}
      '';
in
buildEnv {
  name = "postgrest-tests";
  paths = [
    (testSpec "postgrest-test-spec" defaultPostgresql)
    testSpecAllVersions
    (testIO "postgrest-test-io" defaultPostgresql)
    (testMemory "postgrest-test-memory" defaultPostgresql)
  ] ++ testSpecVersions;
}
