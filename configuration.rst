.. _configuration:

Configuration
=============

PostgREST reads a configuration file to determine information about the database and how to serve client requests. There is no predefined location for this file, you must specify the file path as the one and only argument to the server:

.. code:: bash

  ./postgrest /path/to/postgrest.conf

.. note::

   Configuration can be reloaded without restarting the server. See :ref:`config_reloading`.

The configuration file must contain a set of key value pairs. At minimum you must include these keys:

.. code::

  # postgrest.conf

  # The standard connection URI format, documented at
  # https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING
  db-uri       = "postgres://user:pass@host:5432/dbname"

  # The name of which database schema to expose to REST clients
  db-schema    = "api"

  # The database role to use when no client authentication is provided.
  # Can (and should) differ from user in db-uri
  db-anon-role = "anon"

The user specified in the db-uri is also known as the authenticator role. For more information about the anonymous vs authenticator roles see the :ref:`roles`.

.. _config_full_list:

Here is the full list of configuration parameters.

======================== ======= ================= ========
Name                     Type    Default           Required
======================== ======= ================= ========
db-uri                   String                    Y
db-schema                String                    Y
db-anon-role             String                    Y
db-pool                  Int     10
db-pool-timeout          Int     10
db-extra-search-path     String  public
db-channel               String  pgrst
db-channel-enabled       Boolean True
db-prepared-statements   Boolean True
db-tx-end                String  commit
db-config                Boolean True
db-embed-default-join    String  left
db-use-legacy-gucs       Boolean True
server-host              String  !4
server-port              Int     3000
server-unix-socket       String
server-unix-socket-mode  String  660
log-level                String  error
openapi-mode             String  follow-privileges
openapi-server-proxy-uri String
jwt-secret               String
jwt-aud                  String
secret-is-base64         Boolean False
max-rows                 Int     ∞
pre-request              String
app.settings.*           String
role-claim-key           String  .role
raw-media-types          String
======================== ======= ================= ========

.. _db-uri:

db-uri
------

  The standard connection PostgreSQL `URI format <https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING>`_. Symbols and unusual characters in the password or other fields should be percent encoded to avoid a parse error. If enforcing an SSL connection to the database is required you can use `sslmode <https://www.postgresql.org/docs/current/libpq-ssl.html#LIBPQ-SSL-SSLMODE-STATEMENTS>`_ in the URI, for example ``postgres://user:pass@host:5432/dbname?sslmode=require``.

  When running PostgREST on the same machine as PostgreSQL, it is also possible to connect to the database using a `Unix socket <https://en.wikipedia.org/wiki/Unix_domain_socket>`_ and the `Peer Authentication method <https://www.postgresql.org/docs/current/auth-peer.html>`_ as an alternative to TCP/IP communication and authentication with a password, this also grants higher performance.  To do this you can omit the host and the password, e.g. ``postgres://user@/dbname``, see the `libpq connection string <https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING>`_ documentation for more details.

  On older systems like Centos 6, with older versions of libpq, a different db-uri syntax has to be used. In this case the URI is a string of space separated key-value pairs (key=value), so the example above would be :code:`"host=host user=user port=5432 dbname=dbname password=pass"`.

  Choosing a value for this parameter beginning with the at sign such as ``@filename`` (e.g. ``@./configs/my-config``) loads the secret out of an external file.


.. _db-schema:

db-schema
---------

  The database schema to expose to REST clients. Tables, views and stored procedures in this schema will get API endpoints.

  .. code:: bash

     db-schema = "api"

  This schema gets added to the `search_path <https://www.postgresql.org/docs/current/ddl-schemas.html#DDL-SCHEMAS-PATH>`_ of every request.

List of schemas
~~~~~~~~~~~~~~~

  You can also specify a list of schemas that can be used for **schema-based multitenancy** and **api versioning** by :ref:`multiple-schemas`. Example:

  .. code:: bash

     db-schema = "tenant1, tenant2"

  If you don't :ref:`Switch Schemas <multiple-schemas>`, the first schema in the list(``tenant1`` in this case) is chosen as the default schema.

  *Only the chosen schema* gets added to the `search_path <https://www.postgresql.org/docs/current/ddl-schemas.html#DDL-SCHEMAS-PATH>`_ of every request.

  .. warning::

     Never expose private schemas in this way. See :ref:`schema_isolation`.

.. _db-anon-role:

db-anon-role
------------

  The database role to use when executing commands on behalf of unauthenticated clients. For more information, see :ref:`roles`.

.. _db-pool:

db-pool
-------

  Number of connections to keep open in PostgREST's database pool. Having enough here for the maximum expected simultaneous client connections can improve performance. Note it's pointless to set this higher than the :code:`max_connections` GUC in your database.

.. _db-pool-timeout:

db-pool-timeout
---------------

   Time to live, in seconds, for an idle database pool connection. If the timeout is reached the connection will be closed.
   Once a new request arrives a new connection will be started.

.. _db-extra-search-path:

db-extra-search-path
--------------------

  Extra schemas to add to the `search_path <https://www.postgresql.org/docs/current/ddl-schemas.html#DDL-SCHEMAS-PATH>`_ of every request. These schemas tables, views and stored procedures **don't get API endpoints**, they can only be referred from the database objects inside your :ref:`db-schema`.

  This parameter was meant to make it easier to use **PostgreSQL extensions** (like PostGIS) that are outside of the :ref:`db-schema`.

  Multiple schemas can be added in a comma-separated string, e.g. ``public, extensions``.

.. _db-channel:

db-channel
----------

  The name of the notification channel that PostgREST uses for :ref:`schema_reloading` and configuration reloading.

.. _db-channel-enabled:

db-channel-enabled
------------------

  When this is set to :code:`true`, the notification channel specified in :ref:`db-channel` is enabled.

  You should set this to ``false`` when using PostgresSQL behind an external connection pooler such as PgBouncer working in transaction pooling mode. See :ref:`this section <external_connection_poolers>` for more information.

.. _db-prepared-statements:

db-prepared-statements
----------------------

  Enables or disables prepared statements.

  When disabled, the generated queries will be parameterized (invulnerable to SQL injection) but they will not be prepared (cached in the database session). Not using prepared statements will noticeably decrease performance, so it's recommended to always have this setting enabled.

  You should only set this to ``false`` when using PostgresSQL behind an external connection pooler such as PgBouncer working in transaction pooling mode. See :ref:`this section <external_connection_poolers>` for more information.

.. _db-tx-end:

db-tx-end
---------

  Specifies how to terminate the database transactions.

  .. code:: bash

    # The transaction is always committed
    db-tx-end = "commit"

    # The transaction is committed unless a "Prefer: tx=rollback" header is sent
    db-tx-end = "commit-allow-override"

    # The transaction is always rolled back
    db-tx-end = "rollback"

    # The transaction is rolled back unless a "Prefer: tx=commit" header is sent
    db-tx-end = "rollback-allow-override"

.. _db-config:

db-config
---------

   Enables the in-database configuration.

.. _db-embed-default-join:

db-embed-default-join
---------------------

  Determines the default embedding type between tables or views when none is specified in the request. For more info, see :ref:`embedding_top_level_filter`.

  .. code:: bash

    # Embeds using LEFT JOIN
    db-embed-default-join = "left"

    # Embeds using INNER JOIN
    db-embed-default-join = "inner"

.. _db-use-legacy-gucs:

db-use-legacy-gucs
------------------

  Determine if GUC request settings for headers, cookies and jwt claims use the `legacy names <https://postgrest.org/en/v8.0/api.html#accessing-request-headers-cookies-and-jwt-claims>`_ (string with dashes, invalid starting from PostgreSQL v14) with text values instead of the :ref:`new names <guc_req_headers_cookies_claims>` (string without dashes, valid on all PostgreSQL versions) with json values.

  On PostgreSQL versions 14 and above, this parameter is ignored.

.. _server-host:

server-host
-----------

  Where to bind the PostgREST web server. In addition to the usual address options, PostgREST interprets these reserved addresses with special meanings:

  * :code:`*` - any IPv4 or IPv6 hostname
  * :code:`*4` - any IPv4 or IPv6 hostname, IPv4 preferred
  * :code:`!4` - any IPv4 hostname
  * :code:`*6` - any IPv4 or IPv6 hostname, IPv6 preferred
  * :code:`!6` - any IPv6 hostname

.. _server-port:

server-port
-----------

  The TCP port to bind the web server.

.. _server-unix-socket:

server-unix-socket
------------------

  `Unix domain socket <https://en.wikipedia.org/wiki/Unix_domain_socket>`_ where to bind the PostgREST web server.
  If specified, this takes precedence over :ref:`server-port`. Example:

  .. code:: bash

    server-unix-socket = "/tmp/pgrst.sock"

.. _server-unix-socket-mode:

server-unix-socket-mode
-----------------------

  `Unix file mode <https://en.wikipedia.org/wiki/File_system_permissions>`_ to be set for the socket specified in :ref:`server-unix-socket`
  Needs to be a valid octal between 600 and 777.

  .. code:: bash

    server-unix-socket-mode = "660"

.. _log-level:

log-level
---------

  Specifies the level of information to be logged while running PostgREST.

  .. code:: bash

      # Only startup and db connection recovery messages are logged
      log-level = "crit"

      # All the "crit" level events plus server errors (status 5xx) are logged
      log-level = "error"

      # All the "error" level events plus request errors (status 4xx) are logged
      log-level = "warn"

      # All the "warn" level events plus all requests (every status code) are logged
      log-level  "info"


  Because currently there's no buffering for logging, the levels with minimal logging(``crit/error``) will increase throughput.

.. _openapi-mode:

openapi-mode
------------

  Specifies how the OpenAPI output should be displayed.

  .. code:: bash

    # Follows the privileges of the JWT role claim (or from db-anon-role if the JWT is not sent)
    # Shows information depending on the permissions that the role making the request has
    openapi-mode = "follow-privileges"

    # Ignores the privileges of the JWT role claim (or from db-anon-role if the JWT is not sent)
    # Shows all the exposed information, regardless of the permissions that the role making the request has
    openapi-mode = "ignore-privileges"

    # Disables the OpenApi output altogether.
    # Throws a `404 Not Found` error when accessing the API root path
    openapi-mode = "disabled"

.. _openapi-server-proxy-uri:

openapi-server-proxy-uri
------------------------

  Overrides the base URL used within the OpenAPI self-documentation hosted at the API root path. Use a complete URI syntax :code:`scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]`. Ex. :code:`https://postgrest.com`

  .. code:: json

    {
      "swagger": "2.0",
      "info": {
        "version": "0.4.3.0",
        "title": "PostgREST API",
        "description": "This is a dynamic API generated by PostgREST"
      },
      "host": "postgrest.com:443",
      "basePath": "/",
      "schemes": [
        "https"
      ]
    }

.. _jwt-secret:

jwt-secret
----------

  The secret or `JSON Web Key (JWK) (or set) <https://datatracker.ietf.org/doc/html/rfc7517>`_ used to decode JWT tokens clients provide for authentication. For security the key must be **at least 32 characters long**. If this parameter is not specified then PostgREST refuses authentication requests. Choosing a value for this parameter beginning with the at sign such as :code:`@filename` loads the secret out of an external file. This is useful for automating deployments. Note that any binary secrets must be base64 encoded. Both symmetric and asymmetric cryptography are supported. For more info see :ref:`asym_keys`.

.. _jwt-aud:

jwt-aud
-------

  Specifies the `JWT audience claim <https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.3>`_. If this claim is present in the client provided JWT then you must set this to the same value as in the JWT, otherwise verifying the JWT will fail.

.. _secret-is-base64:

secret-is-base64
----------------

  When this is set to :code:`true`, the value derived from :code:`jwt-secret` will be treated as a base64 encoded secret.

.. _max-rows:

max-rows
--------

  A hard limit to the number of rows PostgREST will fetch from a view, table, or stored procedure. Limits payload size for accidental or malicious requests.

.. _pre-request:

pre-request
-----------

  A schema-qualified stored procedure name to call right after switching roles for a client request. This provides an opportunity to modify SQL variables or raise an exception to prevent the request from completing.

.. _app.settings.*:

app.settings.*
--------------

  Arbitrary settings that can be used to pass in secret keys directly as strings, or via OS environment variables. For instance: :code:`app.settings.jwt_secret = "$(MYAPP_JWT_SECRET)"` will take :code:`MYAPP_JWT_SECRET` from the environment and make it available to postgresql functions as :code:`current_setting('app.settings.jwt_secret')`.

.. _role-claim-key:

role-claim-key
--------------

  A JSPath DSL that specifies the location of the :code:`role` key in the JWT claims. This can be used to consume a JWT provided by a third party service like Auth0, Okta or Keycloak. Usage examples:

  .. code:: bash

    # {"postgrest":{"roles": ["other", "author"]}}
    # the DSL accepts characters that are alphanumerical or one of "_$@" as keys
    role-claim-key = ".postgrest.roles[1]"

    # {"https://www.example.com/role": { "key": "author }}
    # non-alphanumerical characters can go inside quotes(escaped in the config value)
    role-claim-key = ".\"https://www.example.com/role\".key"

.. _raw-media-types:

raw-media-types
---------------

 This serves to extend the `Media Types <https://en.wikipedia.org/wiki/Media_type>`_ that PostgREST currently accepts through an ``Accept`` header.

 These media types can be requested by following the same rules as the ones defined in :ref:`binary_output`.

 As an example, the below config would allow you to request an **image** and a **XML** file by doing a request with ``Accept: image/png``
 or ``Accept: text/xml``, respectively.

 .. code:: bash

   raw-media-types="image/png, text/xml"

.. _env_variables_config:

Environment Variables
=====================

You can also set these :ref:`configuration parameters <config_full_list>` using environment variables. They are capitalized, have a ``PGRST_`` prefix, and use underscores. For example: ``PGRST_DB_URI`` corresponds to ``db-uri`` and ``PGRST_APP_SETTINGS_*`` to ``app.settings.*``.

.. _config_reloading:

Configuration Reloading
=======================

To reload the configuration without restarting the PostgREST server, send a SIGUSR2 signal to the server process.

.. code:: bash

  killall -SIGUSR2 postgrest

This method does not reload :ref:`env_variables_config` and it will not work for reloading a Docker container configuration. In these cases, you need to restart the PostgREST server or use the :ref:`in_db_config` as an alternative.

.. important::

  The following settings will not be reread when reloading the configuration. You will need to restart PostgREST in that case.

    * :ref:`db-uri`
    * :ref:`db-pool`
    * :ref:`db-pool-timeout`
    * :ref:`server-host`
    * :ref:`server-port`
    * :ref:`server-unix-socket`
    * :ref:`server-unix-socket-mode`

.. _in_db_config:

In-Database Configuration
=========================

By adding settings to the **authenticator** role (see :ref:`roles`), you can make the database the single source of truth for PostgREST's configuration.
This is enabled by :ref:`db-config`.

For example, you can configure :ref:`db-schema` and :ref:`jwt-secret` like this:

.. code:: postgresql

   ALTER ROLE authenticator SET pgrst.db_schema = "tenant1, tenant2, tenant3"
   ALTER ROLE authenticator SET pgrst.jwt_secret = "REALLYREALLYREALLYREALLYVERYSAFE"

Note that underscores(``_``) need to be used instead of dashes(``-``) for the in-database config options.

.. important::

   For altering a role in this way, you need a SUPERUSER. You might not be able to use this configuration mode on cloud-hosted databases.

When using both the configuration file and the in-database configuration, the latter takes precedence.

.. danger::

  If direct connections to the database are allowed, then it's not safe to use the in-db configuration for storing the :ref:`jwt-secret`.
  The settings of every role are PUBLIC - they can be viewed by any user that queries the ``pg_catalog.pg_db_role_setting`` table.
  In this case you should keep the :ref:`jwt-secret` in the configuration file or as environment variables.

.. _in_db_config_reloading:

In-database configuration reloading
-----------------------------------

To reload the in-database configuration from within the database, you can use a NOTIFY command.

.. code:: postgresql

   NOTIFY pgrst, 'reload config'

The ``"pgrst"`` notification channel is enabled by default. For configuring the channel, see :ref:`db-channel` and :ref:`db-channel-enabled`.
