Configuration
=============

The PostgREST server reads a configuration file to determine information about the database and how to serve client requests. There is no predefined location for this file, you must specify it with the `-c` option when starting the server:

.. code:: bash

  postgrest -c /path/to/postgrest.conf

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
max-rows          Int     ∞
pre-request       String
================  ======  =======  ========

db-uri
  The standard connection PostgreSQL `URI format <https://www.postgresql.org/docs/current/static/libpq-connect.html#AEN45347>`_. Also allows connections over Unix sockets for higher performance.
db-schema
  The database schema to expose to REST clients. Tables, views and stored procedures in this schema will get API endpoints.
db-anon-role
  The database role to use when executing commands on behalf of unauthenticated clients.
db-pool
  Number of connections to keep open in PostgREST's database pool. Having enough here for the maximum expected simultaneous client connections can improve performance. Note it's pointless to set this higher than the `max_connections` GUC in your database.
server-host
  Where to bind the PostgREST web server.
server-port
  The port to bind the web server.
server-proxy-url
  Overrides the base URL used within the OpenAPI self-documentation hosted at the API root path.
jwt-secret
  The secret used to decode JWT tokens clients provide for authentication. If this parameter is not specified then PostgREST refuses authentication requests. Choosing a value for this parameter beginning with the at sign such as `@filename` loads the secret out of an external file which is useful for non-UTF-8 binary secrets.
max-rows
  A hard limit to the number of rows PostgREST will fetch from a view, table, or stored procedure. Limits payload size for accidental or malicious requests.
pre-request
  A schema-qualified stored procedure name to call right after switching roles for a client request. This provides an opportunity to modify SQL variables or raise an exception to prevent the request from completing.

Hardening PostgREST
===================

PostgREST is a fast way to construct a RESTful API. Its default behavior is great for scaffolding in development. When it's time to go to production it works great too, as long as you take precautions. PostgREST is a small sharp tool that focuses on performing the API-to-database mapping. We rely on a reverse proxy like Nginx for additional safeguards.

The first step is to create an Nginx configuration file that proxies requests to an underlying PostgREST server.

.. code::

  Nginx code goes here.

Block Full-Table Operations
---------------------------

Each table in the admin-selected schema gets exposed as a top level route. Client requests are executed by certain database roles depending on their authentication. All HTTP verbs are supported that correspond to actions permitted to the role. For instance if the active role can drop rows of the table then the DELETE verb is allowed for clients. Here's an API request to delete old rows from a hypothetical logs table:

.. code:: http

  DELETE /logs?time=lt.1991-08-06 HTTP/1.1

However it's very easy to delete the **entire table** by omitting the query parameter!

.. code:: http

  DELETE /logs HTTP/1.1

This can happen accidentally even just by switching a request from a GET to a DELETE. To protect against accidental operations we can add an Nginx rule to prevent DELETE or PATCH requests which lack a query parameter.

.. code::

  Nginx stuff goes here

This does not protect against malicious actions, since someone can add a url parameter that does not affect the resultset. To prevent this you must turn to database permissions, forbidding the wrong people from deleting rows, and using `row-level security <https://www.postgresql.org/docs/current/static/ddl-rowsecurity.html>`_ if finer access control is required.

Count-Header DoS
----------------

For convenience to client-side pagination controls PostgREST supports counting and reporting total table size in its response. As described in :ref:`Limits and Pagination`_, responses ordinarily include a range and unspecified total like

.. code-block:: http

  HTTP/1.1 200 OK
  Range-Unit: items
  Content-Range: 0-14/*

However including the request header `Prefer: count=exact` calculates and includes the full count:

.. code-block:: http

  HTTP/1.1 206 Partial Content
  Range-Unit: items
  Content-Range: 0-14/3573458

This is fine in small tables, but count performance degrades in big tables due to the MVCC architecture of PostgreSQL. For very large tables it can take a very long time to retrieve the results which allows a denial of service attack. The solution is to strip this header from all requests:

.. code::

  Nginx stuff. Remove any prefer header which contains the word count

.. note::

  In future versions we will support `Prefer: count=estimated` to leverage the PostgreSQL statistics tables for a fast (and fairly accurate) result.

.. _hardening_https:

HTTPS
-----

See the :ref:`ssl` section of the authentication guide.

Rate Limiting
-------------


.. Administration
..   Alternate URL structure
..   API Versioning
..   Schema Reloading
..   HTTP Caching
..   Upgrading
..   Debugging
..     (viewing db logs)
