#!/usr/bin/env bash
# Generates or verifies a snapshot of the OpenAPI (Swagger 2.0) document
# emitted by PostgREST when run against the spec test fixture schema.
#
# Intended invocation (assumes nix devShell with withTools available):
#
#   # Regenerate the snapshot (write mode, default):
#   postgrest-with-pg-17 --fixtures test/spec/fixtures/load.sql \
#     postgrest-with-pgrst test/openapi-drift-check.sh
#
#   # Verify the snapshot is in sync (CI mode):
#   postgrest-with-pg-17 --fixtures test/spec/fixtures/load.sql \
#     postgrest-with-pgrst test/openapi-drift-check.sh --check
#
# The script expects PGRST_SERVER_UNIX_SOCKET to be set, which
# postgrest-with-pgrst provides.

set -euo pipefail

MODE="${1:-write}"
SNAPSHOT="test/spec/fixtures/openapi-snapshot.json"

: "${PGRST_SERVER_UNIX_SOCKET:?PGRST_SERVER_UNIX_SOCKET is required; run this script via postgrest-with-pgrst}"

TMPFILE="$(mktemp)"
trap 'rm -f "$TMPFILE"' EXIT

curl -sSf -H "Accept: application/openapi+json" \
  --unix-socket "$PGRST_SERVER_UNIX_SOCKET" \
  http://localhost/ \
  | jq -S . > "$TMPFILE"

case "$MODE" in
  --check)
    if ! diff -u "$SNAPSHOT" "$TMPFILE"; then
      cat >&2 <<EOF
::error::OpenAPI snapshot is out of date.

To regenerate it, run from inside the project nix devShell:

  postgrest-with-pg-17 --fixtures test/spec/fixtures/load.sql \\
    postgrest-with-pgrst test/openapi-drift-check.sh

Then commit the updated $SNAPSHOT.
EOF
      exit 1
    fi
    echo "OpenAPI snapshot is up to date."
    ;;
  write|"")
    mv "$TMPFILE" "$SNAPSHOT"
    trap - EXIT
    echo "Wrote $SNAPSHOT"
    ;;
  *)
    echo "Unknown mode: $MODE (expected --check or write)" >&2
    exit 2
    ;;
esac
