.. _admin:

Hardening PostgREST
===================

PostgREST is a fast way to construct a RESTful API. Its default behavior is great for scaffolding in development. When it's time to go to production it works great too, as long as you take precautions. PostgREST is a small sharp tool that focuses on performing the API-to-database mapping. We rely on a reverse proxy like Nginx for additional safeguards.

The first step is to create an Nginx configuration file that proxies requests to an underlying PostgREST server.

.. code-block:: nginx

  http {
    # ...
    # upstream configuration
    upstream postgrest {
      server localhost:3000;
    }
    # ...
    server {
      # ...
      # expose to the outside world
      location /api/ {
        default_type  application/json;
        proxy_hide_header Content-Location;
        add_header Content-Location  /api/$upstream_http_content_location;
        proxy_set_header  Connection "";
        proxy_http_version 1.1;
        proxy_pass http://postgrest/;
      }
      # ...
    }
  }

.. note::

  For ubuntu, if you already installed nginx through :code:`apt` you can add this to the config file in
  :code:`/etc/nginx/sites-enabled/default`.

.. _block_fulltable:

Block Full-Table Operations
---------------------------

Each table in the admin-selected schema gets exposed as a top level route. Client requests are executed by certain database roles depending on their authentication. All HTTP verbs are supported that correspond to actions permitted to the role. For instance if the active role can drop rows of the table then the DELETE verb is allowed for clients. Here's an API request to delete old rows from a hypothetical logs table:

.. tabs::

  .. code-tab:: http

    DELETE /logs?time=lt.1991-08-06 HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/logs?time=lt.1991-08-06" -X DELETE

However it's very easy to delete the **entire table** by omitting the query parameter!

.. tabs::

  .. code-tab:: http

    DELETE /logs HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/logs" -X DELETE

This can happen accidentally such as by switching a request from a GET to a DELETE. To protect against accidental operations use the `pg-safeupdate <https://github.com/eradman/pg-safeupdate>`_ PostgreSQL extension. It raises an error if UPDATE or DELETE are executed without specifying conditions. To install it you can use the `PGXN <https://pgxn.org/>`_ network:

.. code-block:: bash

  sudo -E pgxn install safeupdate

  # then add this to postgresql.conf:
  # shared_preload_libraries='safeupdate';

This does not protect against malicious actions, since someone can add a url parameter that does not affect the result set. To prevent this you must turn to database permissions, forbidding the wrong people from deleting rows, and using `row-level security <https://www.postgresql.org/docs/current/ddl-rowsecurity.html>`_ if finer access control is required.

Count-Header DoS
----------------

For convenience to client-side pagination controls PostgREST supports counting and reporting total table size in its response. As described in :ref:`limits`, responses ordinarily include a range but leave the total unspecified like

.. code-block:: http

  HTTP/1.1 200 OK
  Range-Unit: items
  Content-Range: 0-14/*

However including the request header :code:`Prefer: count=exact` calculates and includes the full count:

.. code-block:: http

  HTTP/1.1 206 Partial Content
  Range-Unit: items
  Content-Range: 0-14/3573458

This is fine in small tables, but count performance degrades in big tables due to the MVCC architecture of PostgreSQL. For very large tables it can take a very long time to retrieve the results which allows a denial of service attack. The solution is to strip this header from all requests:

.. code-block:: postgres

  -- Pending nginx config: Remove any prefer header which contains the word count

.. _https:

HTTPS
-----

PostgREST aims to do one thing well: add an HTTP interface to a PostgreSQL database. To keep the code small and focused we do not implement HTTPS. Use a reverse proxy such as NGINX to add this, `here's how <https://nginx.org/en/docs/http/configuring_https_servers.html>`_. Note that some Platforms as a Service like Heroku also add SSL automatically in their load balancer.

Rate Limiting
-------------

Nginx supports "leaky bucket" rate limiting (see `official docs <https://nginx.org/en/docs/http/ngx_http_limit_req_module.html>`_). Using standard Nginx configuration, routes can be grouped into *request zones* for rate limiting. For instance we can define a zone for login attempts:

.. code-block:: nginx

  limit_req_zone $binary_remote_addr zone=login:10m rate=1r/s;

This creates a shared memory zone called "login" to store a log of IP addresses that access the rate limited urls. The space reserved, 10 MB (:code:`10m`) will give us enough space to store a history of 160k requests. We have chosen to allow only allow one request per second (:code:`1r/s`).

Next we apply the zone to certain routes, like a hypothetical stored procedure called :code:`login`.

.. code-block:: nginx

  location /rpc/login/ {
    # apply rate limiting
    limit_req zone=login burst=5;
  }

The burst argument tells Nginx to start dropping requests if more than five queue up from a specific IP.

Nginx rate limiting is general and indiscriminate. To rate limit each authenticated request individually you will need to add logic in a :ref:`Custom Validation <custom_validation>` function.

.. _external_connection_poolers:

Using External Connection Poolers
---------------------------------

PostgREST manages its :ref:`own pool of connections <db-pool>` and uses prepared statements by default in order to increase performance. However, this setting is incompatible with external connection poolers such as PgBouncer working in transaction pooling mode. In this case, you need to set the :ref:`db-prepared-statements` config option to ``false``. On the other hand, session pooling is fully compatible with PostgREST, while statement pooling is not compatible at all.

.. note::

  If prepared statements are enabled, PostgREST will quit after detecting that transaction or statement pooling is being used.

You should also set the :ref:`db-channel-enabled` config option to ``false``, due to the ``LISTEN`` command not being compatible with transaction pooling, although it should not give any errors if it's left enabled by default.

Debugging
=========

Server Version
--------------

When debugging a problem it's important to verify the PostgREST version. At any time you can make a request to the running server and determine exactly which version is deployed. Look for the :code:`Server` HTTP response header, which contains the version number.

Errors
------

See the :doc:`Errors <errors>` reference page for detailed information on the errors that PostgREST returns.

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

PostgREST logging provides limited information for debugging server errors. It's helpful to get full information about both client requests and the corresponding SQL commands executed against the underlying database.

HTTP Requests
-------------

A great way to inspect incoming HTTP requests including headers and query parameters is to sniff the network traffic on the port where PostgREST is running. For instance on a development server bound to port 3000 on localhost, run this:

.. code:: bash

  # sudo access is necessary for watching the network
  sudo ngrep -d lo0 port 3000

The options to ngrep vary depending on the address and host on which you've bound the server. The binding is described in the :ref:`configuration` section. The ngrep output isn't particularly pretty, but it's legible.

.. _automatic_recovery:

Automatic Connection Recovery
-----------------------------

When PostgREST loses the connection to the database, it retries the connection using capped exponential backoff, with 32 seconds being the maximum backoff time.

This retry behavior is triggered immediately after the connection is lost if :ref:`db-channel-enabled` is set to true(the default), otherwise it will be activated once a request is made.

To notify the client when the next reconnection attempt will be, PostgREST responds with ``503 Service Unavailable`` and the ``Retry-After: x`` header, where ``x`` is the number of seconds programmed for the next retry.

Database Logs
-------------

Once you've verified that requests are as you expect, you can get more information about the server operations by watching the database logs. By default PostgreSQL does not keep these logs, so you'll need to make the configuration changes below. Find :code:`postgresql.conf` inside your PostgreSQL data directory (to find that, issue the command :code:`show data_directory;`). Either find the settings scattered throughout the file and change them to the following values, or append this block of code to the end of the configuration file.

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

Schema Reloading
----------------

Changing the schema while the server is running can lead to errors due to a stale schema cache. To learn how to refresh the cache see :ref:`schema_reloading`.

.. _health_check:

Health Check
------------

You can enable a minimal health check to verify if PostgREST is available for client requests and to check the status of its internal state.

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

Daemonizing
===========

For Linux distributions that use **systemd** (Ubuntu, Debian, Archlinux) you can create a daemon in the following way.

First, create postgrest configuration in ``/etc/postgrest/config``

.. code-block:: ini

  db-uri = "postgres://<your_user>:<your_password>@localhost:5432/<your_db>"
  db-schemas = "<your_exposed_schema>"
  db-anon-role = "<your_anon_role>"
  jwt-secret = "<your_secret>"

Then create the systemd service file in ``/etc/systemd/system/postgrest.service``

.. code-block:: ini

  [Unit]
  Description=REST API for any PostgreSQL database
  After=postgresql.service

  [Service]
  ExecStart=/bin/postgrest /etc/postgrest/config
  ExecReload=/bin/kill -SIGUSR1 $MAINPID

  [Install]
  WantedBy=multi-user.target

After that, you can enable the service at boot time and start it with:

.. code-block:: bash

  systemctl enable postgrest
  systemctl start postgrest

  ## For reloading the service
  ## systemctl restart postgrest

.. _file_descriptors:

File Descriptors
----------------

File descriptors are kernel resources that are used by HTTP connections (among others). File descriptors are limited per process. The kernel default limit is 1024, which is increased in some Linux distributions.
When under heavy traffic, PostgREST can reach this limit and start showing ``No file descriptors available`` errors. To clear these errors, you can increase the process' file descriptor limit.

.. code-block:: ini

  [Service]
  LimitNOFILE=10000

Alternate URL Structure
=======================

As discussed in :ref:`singular_plural`, there are no special URL forms for singular resources in PostgREST, only operators for filtering. Thus there are no URLs like :code:`/people/1`. It would be specified instead as

.. tabs::

  .. code-tab:: http

    GET /people?id=eq.1 HTTP/1.1
    Accept: application/vnd.pgrst.object+json

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?id=eq.1" \
      -H "Accept: application/vnd.pgrst.object+json"

This allows compound primary keys and makes the intent for singular response independent of a URL convention.

Nginx rewrite rules allow you to simulate the familiar URL convention. The following example adds a rewrite rule for all table endpoints, but you'll want to restrict it to those tables that have a numeric simple primary key named "id."

.. code-block:: nginx

  # support /endpoint/:id url style
  location ~ ^/([a-z_]+)/([0-9]+) {

    # make the response singular
    proxy_set_header Accept 'application/vnd.pgrst.object+json';

    # assuming an upstream named "postgrest"
    proxy_pass http://postgrest/$1?id=eq.$2;

  }

.. TODO
.. Administration
..   API Versioning
..   HTTP Caching
..   Upgrading
