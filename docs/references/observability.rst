.. _observability:

Observability
#############

.. _pgrst_logging:

Logging
-------

PostgREST logs basic request information to ``stdout``, including the authenticated user if available, the requesting IP address and user agent, the URL requested, and HTTP response status.

.. code::

   127.0.0.1 - user [26/Jul/2021:01:56:38 -0500] "GET /clients HTTP/1.1" 200 - "" "curl/7.64.0"
   127.0.0.1 - anonymous [26/Jul/2021:01:56:48 -0500] "GET /unexistent HTTP/1.1" 404 - "" "curl/7.64.0"

For diagnostic information about the server itself, PostgREST logs to ``stderr``.

.. code::

   12/Jun/2021:17:47:39 -0500: Starting PostgREST 11.1.0...
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

When debugging a problem it's important to verify the running PostgREST version. There are three ways to do this:

- Look for the :code:`Server` HTTP response header that is returned on every request.

.. code::

  HEAD /users HTTP/1.1

  Server: postgrest/11.0.1

- Query ``application_name`` on `pg_stat_activity <https://www.postgresql.org/docs/current/monitoring-stats.html#MONITORING-PG-STAT-ACTIVITY-VIEW>`_.

.. code-block:: psql

  select distinct application_name
  from pg_stat_activity
  where application_name ilike '%postgrest%';

        application_name
  ------------------------------
  PostgREST 11.1.0

.. important::

  - The server sets the `fallback_application_name <https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNECT-FALLBACK-APPLICATION-NAME>`_ to the connection URI for this query to work. To override the value set ``application_name`` on the connection string.
  - The version will only be set if it's a valid URI (`RFC 3986 <https://datatracker.ietf.org/doc/html/rfc3986>`_). This means any special characters must be urlencoded.
  - The version will not be set if the connection string is in `keyword/value format <https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING-KEYWORD-VALUE>`_.

- The ``stderr`` logs also contain the version, as noted on :ref:`pgrst_logging`.

.. _trace_header:

Trace Header
------------

You can enable tracing HTTP requests by setting :ref:`server-trace-header`. Specify the set header in the request, and the server will include it in the response.

.. code:: bash

  server-trace-header = "X-Request-Id"

.. tabs::

  .. code-tab:: http

    GET /users HTTP/1.1

    X-Request-Id: 123

  .. code-tab:: bash Curl

    curl "http://localhost:3000/users" \
      -H "X-Request-Id: 123"

.. code::

  HTTP/1.1 200 OK
  X-Request-Id: 123

.. _server-timing_header:

Server-Timing Header
--------------------

You can enable the `Server-Timing <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Server-Timing>`_ header by setting :ref:`server-timing-enabled` on.
This header communicates metrics of the different phases in the request-response cycle.

.. tabs::

  .. code-tab:: http

    GET /users HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/users" -i

.. code::

  HTTP/1.1 200 OK

  Server-Timing: jwt;dur=14.9, parse;dur=71.1, plan;dur=109.0, transaction;dur=353.2, response;dur=4.4

- All the durations (``dur``) are in milliseconds.
- The ``jwt`` stage is when :ref:`jwt_impersonation` is done. This duration can be lowered with :ref:`jwt_caching`.
- On the ``parse`` stage, the :ref:`url_grammar` is parsed.
- On the ``plan`` stage, the :ref:`schema_cache` is used to generate the :ref:`main_query` of the transaction.
- The ``transaction`` stage corresponds to the database transaction. See :ref:`transactions`.
- The ``response`` stage is where the response status and headers are computed.

.. note::

  We're working on lowering the duration of the ``parse`` and ``plan`` stages on https://github.com/PostgREST/postgrest/issues/2816.

.. _explain_plan:

Execution plan
--------------

You can get the `EXPLAIN execution plan <https://www.postgresql.org/docs/current/sql-explain.html>`_ of a request by adding the ``Accept: application/vnd.pgrst.plan`` header.
This is enabled by :ref:`db-plan-enabled` (false by default).

.. tabs::

  .. code-tab:: http

    GET /users?select=name&order=id HTTP/1.1
    Accept: application/vnd.pgrst.plan

  .. code-tab:: bash Curl

    curl "http://localhost:3000/users?select=name&order=id" \
      -H "Accept: application/vnd.pgrst.plan"

.. code-block:: psql

  Aggregate  (cost=73.65..73.68 rows=1 width=112)
    ->  Index Scan using users_pkey on users  (cost=0.15..60.90 rows=850 width=36)

The output of the plan is generated in ``text`` format by default but you can change it to JSON by using the ``+json`` suffix.

.. tabs::

  .. code-tab:: http

    GET /users?select=name&order=id HTTP/1.1
    Accept: application/vnd.pgrst.plan+json

  .. code-tab:: bash Curl

    curl "http://localhost:3000/users?select=name&order=id" \
      -H "Accept: application/vnd.pgrst.plan+json"

.. code-block:: json

  [
    {
      "Plan": {
        "Node Type": "Aggregate",
        "Strategy": "Plain",
        "Partial Mode": "Simple",
        "Parallel Aware": false,
        "Async Capable": false,
        "Startup Cost": 73.65,
        "Total Cost": 73.68,
        "Plan Rows": 1,
        "Plan Width": 112,
        "Plans": [
          {
            "Node Type": "Index Scan",
            "Parent Relationship": "Outer",
            "Parallel Aware": false,
            "Async Capable": false,
            "Scan Direction": "Forward",
            "Index Name": "users_pkey",
            "Relation Name": "users",
            "Alias": "users",
            "Startup Cost": 0.15,
            "Total Cost": 60.90,
            "Plan Rows": 850,
            "Plan Width": 36
          }
        ]
      }
    }
  ]

By default the plan is assumed to generate the JSON representation of a resource(``application/json``), but you can obtain the plan for the :ref:`different representations that PostgREST supports <res_format>` by adding them to the ``for`` parameter. For instance, to obtain the plan for a ``text/xml``, you would use ``Accept: application/vnd.pgrst.plan; for="text/xml``.

The other available parameters are ``analyze``, ``verbose``, ``settings``, ``buffers`` and ``wal``, which correspond to the `EXPLAIN command options <https://www.postgresql.org/docs/current/sql-explain.html>`_. To use the ``analyze`` and ``wal`` parameters for example, you would add them like ``Accept: application/vnd.pgrst.plan; options=analyze|wal``.

Note that akin to the EXPLAIN command, the changes will be committed when using the ``analyze`` option. To avoid this, you can use the :ref:`db-tx-end` and the ``Prefer: tx=rollback`` header.

Securing the Execution Plan
~~~~~~~~~~~~~~~~~~~~~~~~~~~

It's recommended to only activate :ref:`db-plan-enabled` on testing environments since it reveals internal database details.
However, if you choose to use it in production you can add a :ref:`db-pre-request` to filter the requests that can use this feature.

For example, to only allow requests from an IP address to get the execution plans:

.. code-block:: postgresql

 -- Assuming a proxy(Nginx, Cloudflare, etc) passes an "X-Forwarded-For" header(https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Forwarded-For)
 create or replace function filter_plan_requests()
 returns void as $$
 declare
   headers   json := current_setting('request.headers', true)::json;
   client_ip text := coalesce(headers->>'x-forwarded-for', '');
   accept    text := coalesce(headers->>'accept', '');
 begin
   if accept like 'application/vnd.pgrst.plan%' and client_ip != '144.96.121.73' then
     raise insufficient_privilege using
       message = 'Not allowed to use application/vnd.pgrst.plan';
   end if;
 end; $$ language plpgsql;

 -- set this function on your postgrest.conf
 -- db-pre-request = filter_plan_requests

.. raw:: html

  <script type="text/javascript">
    let hash = window.location.hash;

    const redirects = {
      '#health_check': 'health_check.html',
    };

    let willRedirectTo = redirects[hash];

    if (willRedirectTo) {
      window.location.href = willRedirectTo;
    }
  </script>
