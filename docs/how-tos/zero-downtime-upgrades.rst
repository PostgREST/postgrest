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

Handover Procedure
------------------

Follow this sequence when replacing a running PostgREST process:

1. Start the replacement process with the same :ref:`server-host`,
   :ref:`server-port`, and ``server-reuseport = true``.
2. Give the replacement process its own :ref:`admin-server-port`.
3. Wait for the replacement process to report ``/ready`` through its own admin
   server port.
4. Stop the old process only after the replacement process is ready.

If the new process cannot load its configuration, connect to the database, or
load the schema cache, ``/ready`` will not return a successful response and the
old process can keep serving traffic.
