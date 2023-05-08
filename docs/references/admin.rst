.. _admin:

Admin
#####

.. _pgrst_logging:

Logging
-------

PostgREST logs basic request information to ``stdout``, including the authenticated user if available, the requesting IP address and user agent, the URL requested, and HTTP response status.

.. code::

   127.0.0.1 - user [26/Jul/2021:01:56:38 -0500] "GET /clients HTTP/1.1" 200 - "" "curl/7.64.0"
   127.0.0.1 - anonymous [26/Jul/2021:01:56:48 -0500] "GET /unexistent HTTP/1.1" 404 - "" "curl/7.64.0"

For diagnostic information about the server itself, PostgREST logs to ``stderr``.

.. code::

   12/Jun/2021:17:47:39 -0500: Attempting to connect to the database...
   12/Jun/2021:17:47:39 -0500: Listening on port 3000
   12/Jun/2021:17:47:39 -0500: Connection successful
   12/Jun/2021:17:47:39 -0500: Config re-loaded
   12/Jun/2021:17:47:40 -0500: Schema cache loaded

.. note::

   When running it in an SSH session you must detach it from stdout or it will be terminated when the session closes. The easiest technique is redirecting the output to a log file or to the syslog:

   .. code-block:: bash

     ssh foo@example.com \
       'postgrest foo.conf </dev/null >/var/log/postgrest.log 2>&1 &'

     # another option is to pipe the output into "logger -t postgrest"

Currently PostgREST doesn't log the SQL commands executed against the underlying database.

Database Logs
~~~~~~~~~~~~~

To find the SQL operations, you can watch the database logs. By default PostgreSQL does not keep these logs, so you'll need to make the configuration changes below.

Find :code:`postgresql.conf` inside your PostgreSQL data directory (to find that, issue the command :code:`show data_directory;`). Either find the settings scattered throughout the file and change them to the following values, or append this block of code to the end of the configuration file.

.. code:: sql

  # send logs where the collector can access them
  log_destination = "stderr"

  # collect stderr output to log files
  logging_collector = on

  # save logs in pg_log/ under the pg data directory
  log_directory = "pg_log"

  # (optional) new log file per day
  log_filename = "postgresql-%Y-%m-%d.log"

  # log every kind of SQL statement
  log_statement = "all"

Restart the database and watch the log file in real-time to understand how HTTP requests are being translated into SQL commands.

.. note::

  On Docker you can enable the logs by using a custom ``init.sh``:

  .. code:: bash

    #!/bin/sh
    echo "log_statement = 'all'" >> /var/lib/postgresql/data/postgresql.conf

  After that you can start the container and check the logs with ``docker logs``.

  .. code:: bash

    docker run -v "$(pwd)/init.sh":"/docker-entrypoint-initdb.d/init.sh" -d postgres
    docker logs -f <container-id>

Server Version
--------------

When debugging a problem it's important to verify the PostgREST version. Look for the :code:`Server` HTTP response header, which contains the version number.

.. code::

  Server: postgrest/11.0.1

.. _health_check:

Health Check
------------

You can enable a health check to verify if PostgREST is available for client requests. Also to check the status of its internal state.

To do this, set the configuration variable :ref:`admin-server-port` to the port number of your preference. Two endpoints ``live`` and ``ready`` will then be available.

The ``live`` endpoint verifies if PostgREST is running on its configured port. A request will return ``200 OK`` if PostgREST is alive or ``503`` otherwise.

The ``ready`` endpoint also checks the state of both the Database Connection and the :ref:`schema_cache`. A request will return ``200 OK`` if it is ready or ``503`` if not.

For instance, to verify if PostgREST is running at ``localhost:3000`` while the ``admin-server-port`` is set to ``3001``:

.. tabs::

  .. code-tab:: http

    GET localhost:3001/live HTTP/1.1

  .. code-tab:: bash Curl

    curl -I "http://localhost:3001/live"

.. code-block:: http

  HTTP/1.1 200 OK

If you have a machine with multiple network interfaces and multiple PostgREST instances in the same port, you need to specify a unique :ref:`hostname <server-host>` in the configuration of each PostgREST instance for the health check to work correctly. Don't use the special values(``!4``, ``*``, etc) in this case because the health check could report a false positive.
