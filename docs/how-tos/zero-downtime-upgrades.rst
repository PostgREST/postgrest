.. _zero_downtime_upgrades:

Zero-Downtime Upgrades
======================

When :ref:`server-reuseport` is enabled on an operating system that supports
``SO_REUSEPORT``, PostgREST can start more than one process on the same
:ref:`server-host` and :ref:`server-port`. This allows a new PostgREST process
to start and become ready before the old process is stopped.

While both processes are running, the operating system distributes new
connections between them. After the old process exits, the new process receives
all new connections.

This is useful for upgrades and restarts:

1. Keep the old PostgREST process serving requests.
2. Start the new PostgREST process on the same host and port.
3. Wait for the new process to report ``/ready``.
4. Stop the old process.

Configuration
-------------

Both processes should use the same public host and port:

.. code-block:: ini

  # /etc/postgrest/postgrest.conf
  server-host = "127.0.0.1"
  server-port = 3000
  server-reuseport = true

  admin-server-host = "127.0.0.1"
  admin-server-port = 3001

The second process can use the same configuration file and override only the
admin server port:

.. code-block:: bash

  PGRST_ADMIN_SERVER_PORT=3002 postgrest /etc/postgrest/postgrest.conf

.. important::

  Use a different :ref:`admin-server-port` for each PostgREST process during
  the handover. Admin ports are not shared between processes. This keeps
  readiness checks unambiguous: ``/ready`` on the new admin port can only be
  answered by the new process.

Before using this in production, keep these details in mind:

- This works for host and port based servers. It does not apply when
  :ref:`server-unix-socket` is used.
- If :ref:`server-reuseport` is disabled, the new process will fail to start
  with an address-in-use error and the old process will keep serving requests.
- If :ref:`server-reuseport` is enabled on an operating system that does not
  support ``SO_REUSEPORT``, PostgREST will fail to start because the
  configuration is not supported on that platform.
- If the new process uses the same :ref:`admin-server-port` as the old process,
  it will fail to start because that admin port is already in use.
- Each PostgREST process has its own :ref:`db-pool`. During the handover, the
  total possible database connections can temporarily double.
- The old and new processes may both serve requests for a short time. Database
  migrations should be compatible with both versions while they overlap.

Manual Handover
---------------

Assuming the old process is already serving on ``127.0.0.1:3000`` and its PID
is stored in ``OLD_PID``:

.. code-block:: bash

  PGRST_ADMIN_SERVER_PORT=3002 postgrest /etc/postgrest/postgrest.conf &
  NEW_PID=$!

  curl --fail http://127.0.0.1:3002/ready

  kill -TERM "$OLD_PID"

The ``curl`` request checks the new process through its own admin server port.
If the new process cannot load its configuration, connect to the database, or
load the schema cache, ``/ready`` will not return a successful response and the
old process can keep serving traffic.

Example Script
--------------

The following script shows the full sequence for a setup that stores the old
process PID in a PID file. Adapt the start and stop commands to your process
manager.

.. code-block:: bash

  #!/usr/bin/env bash
  set -euo pipefail

  POSTGREST=${POSTGREST:-postgrest}
  CONFIG=${CONFIG:-/etc/postgrest/postgrest.conf}
  PID_FILE=${PID_FILE:-/run/postgrest.pid}

  ADMIN_HOST=${ADMIN_HOST:-127.0.0.1}
  NEW_ADMIN_PORT=${NEW_ADMIN_PORT:-3002}
  READY_TIMEOUT=${READY_TIMEOUT:-30}
  STOP_TIMEOUT=${STOP_TIMEOUT:-30}

  if [[ ! -s "$PID_FILE" ]]; then
    echo "PID file not found or empty: $PID_FILE" >&2
    exit 1
  fi

  OLD_PID=$(<"$PID_FILE")

  if ! kill -0 "$OLD_PID" 2>/dev/null; then
    echo "Old PostgREST process is not running: $OLD_PID" >&2
    exit 1
  fi

  PGRST_ADMIN_SERVER_HOST="$ADMIN_HOST" \
  PGRST_ADMIN_SERVER_PORT="$NEW_ADMIN_PORT" \
    "$POSTGREST" "$CONFIG" &
  NEW_PID=$!

  cleanup_new_process() {
    kill "$NEW_PID" 2>/dev/null || true
  }
  trap cleanup_new_process EXIT INT TERM

  READY_URL="http://$ADMIN_HOST:$NEW_ADMIN_PORT/ready"
  READY_DEADLINE=$((SECONDS + READY_TIMEOUT))

  until curl --fail --silent --show-error --output /dev/null "$READY_URL"; do
    if ! kill -0 "$NEW_PID" 2>/dev/null; then
      echo "New PostgREST process exited before it became ready" >&2
      exit 1
    fi

    if (( SECONDS >= READY_DEADLINE )); then
      echo "New PostgREST process did not become ready at $READY_URL" >&2
      exit 1
    fi

    sleep 1
  done

  printf '%s\n' "$NEW_PID" > "$PID_FILE"

  kill -TERM "$OLD_PID" 2>/dev/null || true

  STOP_DEADLINE=$((SECONDS + STOP_TIMEOUT))

  while kill -0 "$OLD_PID" 2>/dev/null; do
    if (( SECONDS >= STOP_DEADLINE )); then
      echo "Old PostgREST process did not stop after SIGTERM; sending SIGKILL" >&2
      kill -KILL "$OLD_PID"
      break
    fi

    sleep 1
  done

  trap - EXIT INT TERM
  echo "PostgREST handover complete: $OLD_PID -> $NEW_PID"
