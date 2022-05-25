.. _configuration:

Configuration
=============

Without configuration, PostgREST won't be able to serve requests. At the minimum it needs either :ref:`a role to serve anonymous requests with <db-anon-role>` - or :ref:`a secret to use for JWT authentication <jwt-secret>`. Config parameters can be provided via :ref:`file_config`, via :ref:`env_variables_config` or through :ref:`in_db_config`.

To connect to a database it uses a `libpq connection string <https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING>`_. The connection string can be set in the configuration file or via environment variable or can be read from an external file. See :ref:`db-uri` for details. Any parameter that is not set in the connection string is read from `libpq environment variables <https://www.postgresql.org/docs/current/libpq-envars.html>`_. The default connection string is ``postgresql://``, which reads **all** parameters from the environment.

The user with whom PostgREST connects to the database is also known as the authenticator role. For more information about the anonymous vs authenticator roles see :ref:`roles`.

Config parameters are read in the following order:

1. From the config file.
2. From environment variables, overriding values from the config file.
3. From the database, overriding values from both the config file and environment variables.

.. _file_config:

Config File
-----------

PostgREST can read a config file. There is no predefined location for this file, you must specify the file path as the one and only argument to the server:

.. code:: bash

  ./postgrest /path/to/postgrest.conf

.. note::

   Configuration can be reloaded without restarting the server. See :ref:`config_reloading`.

The configuration file must contain a set of key value pairs:

.. code::

  # postgrest.conf

  # The standard connection URI format, documented at
  # https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING
  db-uri       = "postgres://user:pass@host:5432/dbname"

  # The database role to use when no client authentication is provided.
  # Should differ from authenticator
  db-anon-role = "anon"

  # The secret to verify the JWT for authenticated requests with.
  # Needs to be 32 characters minimum.
  jwt-secret           = "reallyreallyreallyreallyverysafe"
  jwt-secret-is-base64 = False

  # Port the postgrest process is listening on for http requests
  server-port = 80

You can run ``postgrest --example`` to display all possible configuration parameters and how to use them in a configuration file.

.. _env_variables_config:

Environment Variables
---------------------

You can also set these :ref:`configuration parameters <config_full_list>` using environment variables. They are capitalized, have a ``PGRST_`` prefix, and use underscores. For example: ``PGRST_DB_URI`` corresponds to ``db-uri`` and ``PGRST_APP_SETTINGS_*`` to ``app.settings.*``.

.. _in_db_config:

In-Database Configuration
-------------------------

By adding settings to the **authenticator** role (see :ref:`roles`), you can make the database the single source of truth for PostgREST's configuration.
This is enabled by :ref:`db-config`.

For example, you can configure :ref:`db-schemas` and :ref:`jwt-secret` like this:

.. code:: postgresql

   ALTER ROLE authenticator SET pgrst.db_schemas = "tenant1, tenant2, tenant3"
   ALTER ROLE authenticator IN DATABASE <your_database_name> SET pgrst.jwt_secret = "REALLYREALLYREALLYREALLYVERYSAFE"

You can use both database-specific settings with `IN DATABASE` and cluster-wide settings without it. Database-specific settings will override cluster-wide settings if both are used for the same parameter.

Note that underscores(``_``) need to be used instead of dashes(``-``) for the in-database config parameters.

.. important::

   For altering a role in this way, you need a SUPERUSER. You might not be able to use this configuration mode on cloud-hosted databases.

When using both the configuration file and the in-database configuration, the latter takes precedence.

.. danger::

  If direct connections to the database are allowed, then it's not safe to use the in-db configuration for storing the :ref:`jwt-secret`.
  The settings of every role are PUBLIC - they can be viewed by any user that queries the ``pg_catalog.pg_db_role_setting`` table.
  In this case you should keep the :ref:`jwt-secret` in the configuration file or as environment variables.

.. _config_reloading:

Configuration Reloading
=======================

It's possible to reload PostgREST's configuration without restarting the server. You can do this :ref:`via signal <config_reloading_signal>` or :ref:`via notification <config_reloading_notify>`.

It's not possible to change :ref:`env_variables_config` for a running process and reloading a Docker container configuration will not work. In these cases, you need to restart the PostgREST server or use :ref:`in_db_config` as an alternative.

.. important::

  The following settings will not be reloaded. You will need to restart PostgREST to change those.

    * :ref:`admin-server-port`
    * :ref:`db-uri`
    * :ref:`db-pool`
    * :ref:`db-pool-timeout`
    * :ref:`server-host`
    * :ref:`server-port`
    * :ref:`server-unix-socket`
    * :ref:`server-unix-socket-mode`

.. _config_reloading_signal:

Reload with signal
------------------

To reload the configuration via signal, send a SIGUSR2 signal to the server process.

.. code:: bash

  killall -SIGUSR2 postgrest

.. _config_reloading_notify:

Reload with NOTIFY
------------------

To reload the configuration from within the database, you can use a NOTIFY command.

.. code:: postgresql

   NOTIFY pgrst, 'reload config'

The ``"pgrst"`` notification channel is enabled by default. For configuring the channel, see :ref:`db-channel` and :ref:`db-channel-enabled`.

.. _config_full_list:

List of parameters
==================

======================== ======= ================= ==========
Name                     Type    Default           Reloadable
======================== ======= ================= ==========
admin-server-port        Int
app.settings.*           String                    Y
db-anon-role             String                    Y
db-channel               String  pgrst             Y
db-channel-enabled       Boolean True              Y
db-config                Boolean True              Y
db-extra-search-path     String  public            Y
db-max-rows              Int     ∞                 Y
db-pool                  Int     10
db-pool-timeout          Int     10
db-pre-request           String                    Y
db-prepared-statements   Boolean True              Y
db-schemas               String  public            Y
db-tx-end                String  commit            
db-uri                   String  postgresql://
db-use-legacy-gucs       Boolean True              Y
jwt-aud                  String                    Y
jwt-role-claim-key       String  .role             Y
jwt-secret               String                    Y
jwt-secret-is-base64     Boolean False             Y
log-level                String  error             Y
openapi-mode             String  follow-privileges Y
openapi-server-proxy-uri String                    Y
raw-media-types          String                    Y
server-host              String  !4
server-port              Int     3000
server-unix-socket       String
server-unix-socket-mode  String  660
======================== ======= ================= ==========

.. _admin-server-port:

admin-server-port
-----------------

  =============== =======================
  **Environment** PGRST_ADMIN_SERVER_PORT
  **In-Database** `n/a`
  =============== =======================

Specifies the port for the :ref:`health_check` endpoints.

.. _app.settings.*:

app.settings.*
--------------

  =============== ====================
  **Environment** PGRST_APP_SETTINGS_*
  **In-Database** pgrst.app_settings_*
  =============== ====================

  Arbitrary settings that can be used to pass in secret keys directly as strings, or via OS environment variables. For instance: :code:`app.settings.jwt_secret = "$(MYAPP_JWT_SECRET)"` will take :code:`MYAPP_JWT_SECRET` from the environment and make it available to postgresql functions as :code:`current_setting('app.settings.jwt_secret')`.

.. _db-anon-role:

db-anon-role
------------

  =============== ==================
  **Environment** PGRST_DB_ANON_ROLE
  **In-Database** `n/a`
  =============== ==================

  The database role to use when executing commands on behalf of unauthenticated clients. For more information, see :ref:`roles`.

  When unset anonymous access will be blocked.

.. _db-channel:

db-channel
----------

  =============== ================
  **Environment** PGRST_DB_CHANNEL
  **In-Database** `n/a`
  =============== ================

  The name of the notification channel that PostgREST uses for :ref:`schema_reloading` and configuration reloading.

.. _db-channel-enabled:

db-channel-enabled
------------------

  =============== ========================
  **Environment** PGRST_DB_CHANNEL_ENABLED
  **In-Database** `n/a`
  =============== ========================

  When this is set to :code:`true`, the notification channel specified in :ref:`db-channel` is enabled.

  You should set this to ``false`` when using PostgresSQL behind an external connection pooler such as PgBouncer working in transaction pooling mode. See :ref:`this section <external_connection_poolers>` for more information.

.. _db-config:

db-config
---------

  =============== ===============
  **Environment** PGRST_DB_CONFIG
  **In-Database** `n/a`
  =============== ===============

   Enables the in-database configuration.

.. _db-extra-search-path:

db-extra-search-path
--------------------

  =============== ==========================
  **Environment** PGRST_DB_EXTRA_SEARCH_PATH
  **In-Database** pgrst.db_extra_search_path
  =============== ==========================

  Extra schemas to add to the `search_path <https://www.postgresql.org/docs/current/ddl-schemas.html#DDL-SCHEMAS-PATH>`_ of every request. These schemas tables, views and stored procedures **don't get API endpoints**, they can only be referred from the database objects inside your :ref:`db-schemas`.

  This parameter was meant to make it easier to use **PostgreSQL extensions** (like PostGIS) that are outside of the :ref:`db-schemas`.

  Multiple schemas can be added in a comma-separated string, e.g. ``public, extensions``.

.. _db-max-rows:

db-max-rows
-----------

  *For backwards compatibility, this config parameter is also available without prefix as "max-rows".*

  =============== =================
  **Environment** PGRST_DB_MAX_ROWS
  **In-Database** pgrst.db_max_rows
  =============== =================

  A hard limit to the number of rows PostgREST will fetch from a view, table, or stored procedure. Limits payload size for accidental or malicious requests.

.. _db-pool:

db-pool
-------

  =============== =================
  **Environment** PGRST_DB_POOL
  **In-Database** `n/a`
  =============== =================

  Number of connections to keep open in PostgREST's database pool. Having enough here for the maximum expected simultaneous client connections can improve performance. Note it's pointless to set this higher than the :code:`max_connections` GUC in your database.

.. _db-pool-timeout:

db-pool-timeout
---------------

  =============== =================
  **Environment** PGRST_DB_POOL_TIMEOUT
  **In-Database** `n/a`
  =============== =================

   Time to live, in seconds, for an idle database pool connection. If the timeout is reached the connection will be closed.
   Once a new request arrives a new connection will be started.

.. _db-pre-request:

db-pre-request
--------------

  *For backwards compatibility, this config parameter is also available without prefix as "pre-request".*

  =============== =================
  **Environment** PGRST_DB_PRE_REQUEST
  **In-Database** pgrst.db_pre_request
  =============== =================

  A schema-qualified stored procedure name to call right after switching roles for a client request. This provides an opportunity to modify SQL variables or raise an exception to prevent the request from completing.

.. _db-prepared-statements:

db-prepared-statements
----------------------

  =============== =================
  **Environment** PGRST_DB_PREPARED_STATEMENTS
  **In-Database** pgrst.db_prepared_statements
  =============== =================

  Enables or disables prepared statements.

  When disabled, the generated queries will be parameterized (invulnerable to SQL injection) but they will not be prepared (cached in the database session). Not using prepared statements will noticeably decrease performance, so it's recommended to always have this setting enabled.

  You should only set this to ``false`` when using PostgresSQL behind an external connection pooler such as PgBouncer working in transaction pooling mode. See :ref:`this section <external_connection_poolers>` for more information.

.. _db-schemas:

db-schemas
----------

  *For backwards compatibility, this config parameter is also available in singular as "db-schema".*

  =============== =================
  **Environment** PGRST_DB_SCHEMAS
  **In-Database** pgrst.db_schemas
  =============== =================

  The database schema to expose to REST clients. Tables, views and stored procedures in this schema will get API endpoints.

  .. code:: bash

     db-schemas = "api"

  This schema gets added to the `search_path <https://www.postgresql.org/docs/current/ddl-schemas.html#DDL-SCHEMAS-PATH>`_ of every request.

List of schemas
~~~~~~~~~~~~~~~

  You can also specify a list of schemas that can be used for **schema-based multitenancy** and **api versioning** by :ref:`multiple-schemas`. Example:

  .. code:: bash

     db-schemas = "tenant1, tenant2"

  If you don't :ref:`Switch Schemas <multiple-schemas>`, the first schema in the list(``tenant1`` in this case) is chosen as the default schema.

  *Only the chosen schema* gets added to the `search_path <https://www.postgresql.org/docs/current/ddl-schemas.html#DDL-SCHEMAS-PATH>`_ of every request.

  .. warning::

     Never expose private schemas in this way. See :ref:`schema_isolation`.

.. _db-tx-end:

db-tx-end
---------

  =============== =================
  **Environment** PGRST_DB_TX_END
  **In-Database** pgrst.db_tx_end
  =============== =================

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

.. _db-uri:

db-uri
------

  =============== =================
  **Environment** PGRST_DB_URI
  **In-Database** `n/a`
  =============== =================

  The standard connection PostgreSQL `URI format <https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING>`_. Symbols and unusual characters in the password or other fields should be percent encoded to avoid a parse error. If enforcing an SSL connection to the database is required you can use `sslmode <https://www.postgresql.org/docs/current/libpq-ssl.html#LIBPQ-SSL-SSLMODE-STATEMENTS>`_ in the URI, for example ``postgres://user:pass@host:5432/dbname?sslmode=require``.

  When running PostgREST on the same machine as PostgreSQL, it is also possible to connect to the database using a `Unix socket <https://en.wikipedia.org/wiki/Unix_domain_socket>`_ and the `Peer Authentication method <https://www.postgresql.org/docs/current/auth-peer.html>`_ as an alternative to TCP/IP communication and authentication with a password, this also grants higher performance.  To do this you can omit the host and the password, e.g. ``postgres://user@/dbname``, see the `libpq connection string <https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING>`_ documentation for more details.

  Choosing a value for this parameter beginning with the at sign such as ``@filename`` (e.g. ``@./configs/my-config``) loads the connection string out of an external file.

.. _db-use-legacy-gucs:

db-use-legacy-gucs
------------------

  =============== =================
  **Environment** PGRST_DB_USE_LEGACY_GUCS
  **In-Database** pgrst.db_use_legacy_gucs
  =============== =================

  Determine if GUC request settings for headers, cookies and jwt claims use the `legacy names <https://postgrest.org/en/v8.0/api.html#accessing-request-headers-cookies-and-jwt-claims>`_ (string with dashes, invalid starting from PostgreSQL v14) with text values instead of the :ref:`new names <guc_req_headers_cookies_claims>` (string without dashes, valid on all PostgreSQL versions) with json values.

  On PostgreSQL versions 14 and above, this parameter is ignored.

.. _jwt-aud:

jwt-aud
-------

  =============== =================
  **Environment** PGRST_JWT_AUD
  **In-Database** pgrst.jwt_aud
  =============== =================

  Specifies the `JWT audience claim <https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.3>`_. If this claim is present in the client provided JWT then you must set this to the same value as in the JWT, otherwise verifying the JWT will fail.

.. _jwt-role-claim-key:

jwt-role-claim-key
------------------

  *For backwards compatibility, this config parameter is also available without prefix as "role-claim-key".*

  =============== =================
  **Environment** PGRST_JWT_ROLE_CLAIM_KEY
  **In-Database** pgrst.jwt_role_claim_key
  =============== =================

  A JSPath DSL that specifies the location of the :code:`role` key in the JWT claims. This can be used to consume a JWT provided by a third party service like Auth0, Okta or Keycloak. Usage examples:

  .. code:: bash

    # {"postgrest":{"roles": ["other", "author"]}}
    # the DSL accepts characters that are alphanumerical or one of "_$@" as keys
    jwt-role-claim-key = ".postgrest.roles[1]"

    # {"https://www.example.com/role": { "key": "author }}
    # non-alphanumerical characters can go inside quotes(escaped in the config value)
    jwt-role-claim-key = ".\"https://www.example.com/role\".key"

.. _jwt-secret:

jwt-secret
----------

  =============== =================
  **Environment** PGRST_JWT_SECRET
  **In-Database** pgrst.jwt_secret
  =============== =================

  The secret or `JSON Web Key (JWK) (or set) <https://datatracker.ietf.org/doc/html/rfc7517>`_ used to decode JWT tokens clients provide for authentication. For security the key must be **at least 32 characters long**. If this parameter is not specified then PostgREST refuses authentication requests. Choosing a value for this parameter beginning with the at sign such as :code:`@filename` loads the secret out of an external file. This is useful for automating deployments. Note that any binary secrets must be base64 encoded. Both symmetric and asymmetric cryptography are supported. For more info see :ref:`asym_keys`.

  Choosing a value for this parameter beginning with the at sign such as ``@filename`` (e.g. ``@./configs/my-config``) loads the secret out of an external file.

  .. warning::

     Only when using the :ref:`file_config`, if the ``jwt-secret`` contains a ``$`` character by itself it will give errors. In this case, use ``$$`` and PostgREST will interpret it as a single ``$`` character.

.. _jwt-secret-is-base64:

jwt-secret-is-base64
--------------------

  =============== =================
  **Environment** PGRST_JWT_SECRET_IS_BASE64
  **In-Database** pgrst.jwt_secret_is_base64
  =============== =================

  When this is set to :code:`true`, the value derived from :code:`jwt-secret` will be treated as a base64 encoded secret.

.. _log-level:

log-level
---------

  =============== =================
  **Environment** PGRST_LOG_LEVEL
  **In-Database** `n/a`
  =============== =================

  Specifies the level of information to be logged while running PostgREST.

  .. code:: bash

      # Only startup and db connection recovery messages are logged
      log-level = "crit"

      # All the "crit" level events plus server errors (status 5xx) are logged
      log-level = "error"

      # All the "error" level events plus request errors (status 4xx) are logged
      log-level = "warn"

      # All the "warn" level events plus all requests (every status code) are logged
      log-level = "info"


  Because currently there's no buffering for logging, the levels with minimal logging(``crit/error``) will increase throughput.

.. _openapi-mode:

openapi-mode
------------

  =============== =================
  **Environment** PGRST_OPENAPI_MODE
  **In-Database** pgrst.openapi_mode
  =============== =================

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

  =============== =================
  **Environment** PGRST_OPENAPI_SERVER_PROXY_URI
  **In-Database** pgrst.openapi_server_proxy_uri
  =============== =================

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

.. _raw-media-types:

raw-media-types
---------------

  =============== =================
  **Environment** PGRST_RAW_MEDIA_TYPES
  **In-Database** pgrst.raw_media_types
  =============== =================

 This serves to extend the `Media Types <https://en.wikipedia.org/wiki/Media_type>`_ that PostgREST currently accepts through an ``Accept`` header.

 These media types can be requested by following the same rules as the ones defined in :ref:`scalar_return_formats`.

 As an example, the below config would allow you to request an **image** and a **XML** file by doing a request with ``Accept: image/png``
 or ``Accept: font/woff2``, respectively.

 .. code:: bash

   raw-media-types="image/png, font/woff2"

.. _server-host:

server-host
-----------

  =============== =================
  **Environment** PGRST_SERVER_HOST
  **In-Database** `n/a`
  =============== =================

  Where to bind the PostgREST web server. In addition to the usual address options, PostgREST interprets these reserved addresses with special meanings:

  * :code:`*` - any IPv4 or IPv6 hostname
  * :code:`*4` - any IPv4 or IPv6 hostname, IPv4 preferred
  * :code:`!4` - any IPv4 hostname
  * :code:`*6` - any IPv4 or IPv6 hostname, IPv6 preferred
  * :code:`!6` - any IPv6 hostname

.. _server-port:

server-port
-----------

  =============== =================
  **Environment** PGRST_SERVER_PORT
  **In-Database** `n/a`
  =============== =================

  The TCP port to bind the web server.

.. _server-unix-socket:

server-unix-socket
------------------

  =============== =================
  **Environment** PGRST_SERVER_UNIX_SOCKET
  **In-Database** `n/a`
  =============== =================

  `Unix domain socket <https://en.wikipedia.org/wiki/Unix_domain_socket>`_ where to bind the PostgREST web server.
  If specified, this takes precedence over :ref:`server-port`. Example:

  .. code:: bash

    server-unix-socket = "/tmp/pgrst.sock"

.. _server-unix-socket-mode:

server-unix-socket-mode
-----------------------

  =============== =================
  **Environment** PGRST_SERVER_UNIX_SOCKET_MODE
  **In-Database** `n/a`
  =============== =================

  `Unix file mode <https://en.wikipedia.org/wiki/File_system_permissions>`_ to be set for the socket specified in :ref:`server-unix-socket`
  Needs to be a valid octal between 600 and 777.

  .. code:: bash

    server-unix-socket-mode = "660"
