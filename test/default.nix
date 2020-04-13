# Run the PostgREST test suite with several versions of Postgres.

# You'll need Nix (https://nixos.org/download.html) to use the scripts defined
# here. To run the tests for all versions:
#
#     nix-shell --run postgrest-test-all
#
# You can also install the script on your path and run it:
#
#     nix-env -iA postgrestTestAll -f default.nix
#     postgrest-test-all
#
# To run the test suite for individual Postgres versions, use the respective
# scripts:
#
#     nix-shell --run postgrest-test-postgresql-11.7
#
# You can install all of them with using the postgrestTests attribute:
#
#     nix-env -iA postgrestTests -f default.nix
#     postgrest-tests-postgresql-9.5.21
#
let
  # We use a pinned version of nixpkgs so the environment is always
  # reproducible. To update the pinned version, grab a newer git commit hash
  # from the https://github.com/NixOS/nixpkgs repository and plug it in 'rev'.
  nixpkgsVersion =
    {
      date = "2020-04-11";
      rev = "f601ab37c2fb7e5f65989a92df383bcd6942567a";
      tarballHash = "0ikhcmcc29iiaqjv5r91ncgxny2z67bjzkppd3wr1yx44sv7v69s";
    };

  # Downloading a tarball of nixpkgs is more efficient than getting the huge
  # nixpkgs git repository.
  pinnedPkgs =
    builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
      sha256 = nixpkgsVersion.tarballHash;
    };

  pkgs =
    import pinnedPkgs {};

  # We will run the PostgREST tests with all Postgres versions defined here.
  postgresqlVersions =
    [
      pkgs.postgresql_9_5
      pkgs.postgresql_9_6
      pkgs.postgresql_10
      pkgs.postgresql_11
      pkgs.postgresql_12
    ];

  # The 'postgrestTest' function takes a Postgres package as an argument. It
  # defines a script that sets up a temporary database with that version and
  # runs the PostgREST tests against that database.
  postgrestTest =
    postgresql:
      pkgs.writeShellScriptBin "postgrest-test-${postgresql.name}"
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
          until ${postgresql}/bin/pg_isready; do
            sleep 0.1
          done

          stop() {
              ${postgresql}/bin/pg_ctl stop -m i > /dev/null
              rm -rf "$tmpdir"
          }

          trap stop exit

          # Prepare the database for running the tests.
          ${postgresql}/bin/psql <<EOF
              create extension pgcrypto;
              alter database $PGDATABASE set request.jwt.claim.id = '-1';
          EOF

          export PATH="${postgresql}/bin:$PATH"

          ${pkgs.stack}/bin/stack test
        '';

    # Helper function for pulling all the scripts into 'postgrest-test-all'
    # below.
    runPostgrestTest =
      postgresql:
        let
          test =
            postgrestTest postgresql;
        in
        ''
          echo "Testing with ${postgresql.name}..."
          ${test}/bin/${test.name}
        '';
in
# The shell can be used with nix-shell for convenience. It sets up an
# environment where 'postgrest-test-all' and the test scripts for the individual
# Postgres versions are on the path.
pkgs.mkShell rec {
  name = "postgrest-tests";

  buildInputs =
    [
      postgrestTestAll
      postgrestTests
    ];

  # 'postgrest-test-all' is a script that will run the PostgREST tests for all
  # the Postgres versions defined above. The exit-code will be non-zero if
  # the tests for any of the versions fail.
  postgrestTestAll =
    pkgs.writeShellScriptBin "postgrest-test-all"
      ''
        set -euo pipefail

        ${pkgs.lib.concatStringsSep "\n" (map runPostgrestTest postgresqlVersions)}
      '';

  # This defines an environment where the tests for the individual Postgres
  # versions are on the path.
  postgrestTests =
    pkgs.buildEnv
      {
        name = "postgrest-tests";
        paths = map postgrestTest postgresqlVersions;
      };
}
