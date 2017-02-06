Configuration
=============

The PostgREST server reads a configuration file to determine information about the database and how to serve client requests. There is no predefined location for this file, you must specify the file path as the one and only argument to the server:

.. code:: bash

  postgrest /path/to/postgrest.conf

The file must contain a set of key value pairs. At minimum you must include these keys:

.. code::

  # postgrest.conf

  # The standard connection URI format, documented at
  # https://www.postgresql.org/docs/current/static/libpq-connect.html#AEN45347
  db-uri       = "postgres://user:pass@host:5432/dbname"

  # The name of which database schema to expose to REST clients
  db-schema    = "api"

  # The database role to use when no client authentication is provided.
  # Can (and probably should) differ from user in db-uri
  db-anon-role = "anon"

The user specified in the db-uri is also known as the authenticator role. For more information about the anonymous vs authenticator roles see the :ref:`roles`.

Here is the full list of configuration parameters.

================  ======  =======  ========
Name              Type    Default  Required
================  ======  =======  ========
db-uri            String           Y
db-schema         String           Y
db-anon-role      String           Y
db-pool           Int     10
server-host       String  \*4
server-port       Int     3000
server-proxy-url  String
jwt-secret        String
secret-is-base64  Bool    False
max-rows          Int     ∞
pre-request       String
================  ======  =======  ========

db-uri
  The standard connection PostgreSQL `URI format <https://www.postgresql.org/docs/current/static/libpq-connect.html#AEN45347>`_. On older systems like Centos 6, with older versions of libpq, a different db-uri syntax has to be used. In this case the URI is a string of space separated key-value pairs (key=value), so the example above would be :code:`"host=host user=user port=5432 dbname=dbname password=pass"`. Also allows connections over Unix sockets for higher performance.
db-schema
  The database schema to expose to REST clients. Tables, views and stored procedures in this schema will get API endpoints.
db-anon-role
  The database role to use when executing commands on behalf of unauthenticated clients.
db-pool
  Number of connections to keep open in PostgREST's database pool. Having enough here for the maximum expected simultaneous client connections can improve performance. Note it's pointless to set this higher than the :code:`max_connections` GUC in your database.
server-host
  Where to bind the PostgREST web server.
server-port
  The port to bind the web server.
server-proxy-url
  Overrides the base URL used within the OpenAPI self-documentation hosted at the API root path.
jwt-secret
  The secret used to decode JWT tokens clients provide for authentication. If this parameter is not specified then PostgREST refuses authentication requests. Choosing a value for this parameter beginning with the at sign such as :code:`@filename` loads the secret out of an external file. This is useful for automating deployments. Note that any binary secrets must be base64 encoded.
secret-is-base64
  When this is set to :code:`true`, the value derived from :code:`jwt-secret` will be treated as a base64 encoded secret.
max-rows
  A hard limit to the number of rows PostgREST will fetch from a view, table, or stored procedure. Limits payload size for accidental or malicious requests.
pre-request
  A schema-qualified stored procedure name to call right after switching roles for a client request. This provides an opportunity to modify SQL variables or raise an exception to prevent the request from completing.

Running the Server
------------------

PostgREST outputs basic request logging to stdout. When running it in an SSH session you must detach it from stdout or it will be terminated when the session closes. The easiest technique is redirecting the output to a logfile or to the syslog:

.. code-block:: bash

  ssh foo@example.com \
    'postgrest foo.conf </dev/null >/var/log/postgrest.log 2>&1 &'

  # another option is to pipe the output into "logger -t postgrest"

(Avoid :code:`nohup postgrest` because the HUP signal is used for manual :ref:`schema_reloading`.)

Hardening PostgREST
===================

PostgREST is a fast way to construct a RESTful API. Its default behavior is great for scaffolding in development. When it's time to go to production it works great too, as long as you take precautions. PostgREST is a small sharp tool that focuses on performing the API-to-database mapping. We rely on a reverse proxy like Nginx for additional safeguards.

The first step is to create an Nginx configuration file that proxies requests to an underlying PostgREST server.

.. code:: nginx

  http {
    ...
    # upstream configuration
    upstream postgrest {
      server localhost:3000;
      keepalive 64;
    }
    ...
    server {
      ...
      # expose to the outside world
      location /api {
        default_type  application/json;
        proxy_hide_header Content-Location;
        add_header Content-Location  /api$upstream_http_content_location;
        proxy_set_header  Connection "";
        proxy_http_version 1.1;
        proxy_pass http://postgrest/;
      }
      ...
    }
  }

.. _block_fulltable:

Block Full-Table Operations
---------------------------

Each table in the admin-selected schema gets exposed as a top level route. Client requests are executed by certain database roles depending on their authentication. All HTTP verbs are supported that correspond to actions permitted to the role. For instance if the active role can drop rows of the table then the DELETE verb is allowed for clients. Here's an API request to delete old rows from a hypothetical logs table:

.. code:: http

  DELETE /logs?time=lt.1991-08-06 HTTP/1.1

However it's very easy to delete the **entire table** by omitting the query parameter!

.. code:: http

  DELETE /logs HTTP/1.1

This can happen accidentally such as by switching a request from a GET to a DELETE. To protect against accidental operations use the `pg-safeupdate <https://bitbucket.org/eradman/pg-safeupdate/>`_ PostgreSQL extension. It raises an error if UPDATE or DELETE are executed without specifying conditions. To install it you can use the `PGXN <http://pgxn.org/>`_ network:

.. code-block:: bash

  sudo -E pgxn install safeupdate

  # then add this to postgresql.conf:
  # shared_preload_libraries='safeupdate';

This does not protect against malicious actions, since someone can add a url parameter that does not affect the result set. To prevent this you must turn to database permissions, forbidding the wrong people from deleting rows, and using `row-level security <https://www.postgresql.org/docs/current/static/ddl-rowsecurity.html>`_ if finer access control is required.

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

.. code::

  Nginx stuff. Remove any prefer header which contains the word count

.. note::

  In future versions we will support :code:`Prefer: count=estimated` to leverage the PostgreSQL statistics tables for a fast (and fairly accurate) result.

.. _hardening_https:

HTTPS
-----

See the :ref:`ssl` section of the authentication guide.

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

Nginx rate limiting is general and indescriminate. To rate limit each authenticated request individually you will need to add logic in a :ref:`Custom Validation <custom_validation>` function.

Debugging
=========

The PostgREST server logs basic request information to stdout, including the requesting IP address and user agent, the URL requested, and HTTP response status. However this provides limited information for debugging server errors. It's helpful to get full information about both client requests and the corresponding SQL commands executed against the underlying database.

A great way to inspect incoming HTTP requests including headers and query params is to sniff the network traffic on the port where PostgREST is running. For instance on a development server bound to port 3000 on localhost, run this:

.. code:: bash

  # sudo access is necessary for watching the network
  sudo ngrep -d lo0 port 3000

The options to ngrep vary depending on the address and host on which you've bound the server. The binding is described in the `Configuration`_ section. The ngrep output isn't particularly pretty, but it's legible. Note the :code:`Server` response header as well which identifies the version of server. This is important when submitting bug reports.

Once you've verified that requests are as you expect, you can get more information about the server operations by watching the database logs. By default PostgreSQL does not keep these logs, so you'll need to make the configuration changes below. Find :code:`postgresql.conf` inside your PostgreSQL data directory (to find that, issue the command :code:`show data_directory;`). Either find the settings scattered throughout the file and change them to the following values, or append this block of code to the end of the configuration file.

.. code:: sql

  # send logs where the collector can access them
  log_destination = 'stderr'

  # collect stderr output to log files
  logging_collector = on

  # save logs in pg_log/ under the pg data directory
  log_directory = 'pg_log'

  # (optional) new log file per day
  log_filename = 'postgresql-%Y-%m-%d.log'

  # log every kind of SQL statement
  log_statement = 'all'

Restart the database and watch the log file in real-time to understand how HTTP requests are being translated into SQL commands.

.. _schema_reloading:

Schema Reloading
----------------

Users are often confused by PostgREST's database schema cache. It is present because detecting foreign key relationships between tables (including how those relationships pass through views) is necessary, but costly. API requests consult the schema cache as part of :ref:`resource_embedding`. However if the schema changes while the server is running it results in a stale cache and leads to errors claiming that no relations are detected between tables.

To refresh the cache without restarting the PostgREST server, send the server process a SIGHUP signal:

.. code:: bash

  killall -HUP postgrest

In the future we're investigating ways to keep the cache updated without manual intervention.

Alternate URL Structure
=======================

As discussed in `Singular or Plural`_, there are no special URL forms for singular resources in PostgREST, only operators for filtering. Thus there are no URLs like :code:`/people/1`. It would be specified instead as

.. code:: http

  GET /people?id=eq.1
  Accept: application/vnd.pgrst.object+json

This allows compound primary keys and makes the intent for singular response independent of a URL convention. However for any table which uses a simple primary key you can use Nginx to simulate the familiar URL convention.

.. code:: nginx

  nginx code here

.. TODO
.. Administration
..   API Versioning
..   HTTP Caching
..   Upgrading
