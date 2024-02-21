.. _configuration:

Configuration
#############

Configuration parameters can be provided via:

- :ref:`file_config`.
- :ref:`env_variables_config`, overriding values from the config file.
- :ref:`in_db_config`, overriding values from both the config file and environment variables.

Using :ref:`config_reloading` you can modify the parameters without restarting the server.


Minimum parameters
==================

The server is able to start without any config parameters, but it won't be able to serve requests unless it has :ref:`a role to serve anonymous requests with <db-anon-role>` - or :ref:`a secret to use for JWT authentication <jwt-secret>`.

.. _file_config:

Config File
===========

There is no predefined location for the config file, you must specify the file path as the one and only argument to the server:

.. code:: bash

  ./postgrest /path/to/postgrest.conf

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
  jwt-secret-is-base64 = false

  # Port the postgrest process is listening on for http requests
  server-port = 3000

You can run ``postgrest --example`` to display all possible configuration parameters and how to use them in a configuration file.

.. _env_variables_config:

Environment Variables
=====================

Environment variables are capitalized, have a ``PGRST_`` prefix, and use underscores. For example: ``PGRST_DB_URI`` corresponds to ``db-uri`` and ``PGRST_APP_SETTINGS_*`` to ``app.settings.*``.

`libpq environment variables <https://www.postgresql.org/docs/current/libpq-envars.html>`_ are also supported for constructing the connection string, see :ref:`db-uri`.

See the full list of environment variable names on :ref:`config_full_list`.

.. _in_db_config:

In-Database Configuration
=========================

You can also configure the server with database settings by using a :ref:`pre-config <db-pre-config>` function. For example, you can configure :ref:`db-schemas` and :ref:`jwt-secret` like this:

.. code-block::

  # postgrest.conf

  db-pre-config  = "postgrest.pre_config"

  # or env vars

  PGRST_DB_PRE_CONFIG = "postgrest.pre_config"

.. code-block:: postgres

  -- create a dedicated schema, hidden from the API
  create schema postgrest;
  -- grant usage on this schema to the authenticator
  grant usage on schema postgrest to authenticator;

  -- the function can configure postgREST by using set_config
  create or replace function postgrest.pre_config()
  returns void as $$
    select
        set_config('pgrst.db_schemas', 'schema1, schema2', true)
      , set_config('pgrst.jwt_secret', 'REALLYREALLYREALLYREALLYVERYSAFE', true);
  $$ language sql;

Note that underscores(``_``) need to be used instead of dashes(``-``) for the in-database config parameters. See the full list of in-database names on :ref:`config_full_list`.

You can disable the in-database configuration by setting :ref:`db-config` to ``false``.

.. note::
  For backwards compatibility, you can do in-db config by modifying the :ref:`authenticator role <roles>`. This is no longer recommended as it requires SUPERUSER.

  .. code:: postgresql

     ALTER ROLE authenticator SET pgrst.db_schemas = "tenant1, tenant2, tenant3"
     ALTER ROLE authenticator IN DATABASE <your_database_name> SET pgrst.db_schemas = "tenant4, tenant5" -- database-specific setting, overrides the previous setting

.. _config_reloading:

Configuration Reloading
=======================

It's possible to reload PostgREST's configuration without restarting the server. You can do this :ref:`via signal <config_reloading_signal>` or :ref:`via notification <config_reloading_notify>`.

- Any modification to the :ref:`file_config` will be applied during reload.
- Any modification to the :ref:`in_db_config` will be applied during reload.
- Not all settings are reloadable, see the reloadable list on :ref:`config_full_list`.
- It's not possible to change :ref:`env_variables_config` for a running process, hence reloading a Docker container configuration will not work. In these cases, you can restart the process or use :ref:`in_db_config`.

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

The ``"pgrst"`` notification channel is enabled by default. You can name the channel with :ref:`db-channel` and enable or disable it with :ref:`db-channel-enabled`.

.. _config_full_list:

List of parameters
==================

.. _admin-server-port:

admin-server-port
-----------------

  =============== =======================
  **Type**        Int
  **Default**     `n/a`
  **Reloadable**  N
  **Environment** PGRST_ADMIN_SERVER_PORT
  **In-Database** `n/a`
  =============== =======================

  Specifies the port for the :ref:`health_check` endpoints.

.. _app.settings.*:

app.settings.*
--------------

  =============== =======================
  **Type**        String
  **Default**     `n/a`
  **Reloadable**  &
  **Environment** PGRST_APP_SETTINGS_*
  **In-Database** `n/a`
  =============== =======================

  Arbitrary settings that can be used to pass in secret keys directly as strings, or via OS environment variables. For instance: :code:`app.settings.jwt_secret = "$(MYAPP_JWT_SECRET)"` will take :code:`MYAPP_JWT_SECRET` from the environment and make it available to PostgreSQL functions as :code:`current_setting('app.settings.jwt_secret')`.

.. _db-aggregates-enabled:

db-aggregates-enabled
---------------------

  =============== =======================
  **Type**        Boolean
  **Default**     False
  **Reloadable**  Y
  **Environment** PGRST_DB_AGGREGATES_ENABLED
  **In-Database** pgrst.db_aggregates_enabled
  =============== =======================


  When this is set to :code:`true`, the use of :ref:`aggregate_functions` is allowed.

  It is recommended that this be set to ``false`` unless proper safeguards are in place to prevent potential performance problems from arising. For example, it is possible that a user may request the ``max()`` of an unindexed column in a table with millions of rows. At best, this would result in a slow query, and at worst, it could be abused to prevent other users from accessing your API (i.e. a form of denial-of-service attack.)

  Proper safeguards could include:
    - Use of a statement timeout. See :ref:`impersonated_settings`.
    - Use of the `pg_plan_filter extension <https://github.com/pgexperts/pg_plan_filter>`_ to block excessively expensive queries.

.. _db-anon-role:

db-anon-role
------------

  =============== =======================
  **Type**        String
  **Default**     `n/a`
  **Reloadable**  Y
  **Environment** PGRST_DB_ANON_ROLE
  **In-Database** pgrst.db_anon_role
  =============== =======================

  The database role to use when executing commands on behalf of unauthenticated clients. For more information, see :ref:`roles`.

  When unset anonymous access will be blocked.

.. _db-channel:

db-channel
----------

  =============== =======================
  **Type**        String
  **Default**     pgrst
  **Reloadable**  Y
  **Environment** PGRST_DB_CHANNEL
  **In-Database** `n/a`
  =============== =======================

  The name of the notification channel that PostgREST uses for :ref:`schema_reloading` and configuration reloading.

.. _db-channel-enabled:

db-channel-enabled
------------------

  =============== =======================
  **Type**        Boolean
  **Default**     True
  **Reloadable**  Y
  **Environment** PGRST_DB_CHANNEL_ENABLED
  **In-Database** `n/a`
  =============== =======================

  When this is set to :code:`true`, the notification channel specified in :ref:`db-channel` is enabled.

  You should set this to ``false`` when using PostgresSQL behind an external connection pooler such as PgBouncer working in transaction pooling mode. See :ref:`this section <external_connection_poolers>` for more information.

.. _db-config:

db-config
---------

  =============== =======================
  **Type**        Boolean
  **Default**     True
  **Reloadable**  Y
  **Environment** PGRST_DB_CONFIG
  **In-Database** `n/a`
  =============== =======================

   Enables the in-database configuration.

.. _db-pre-config:

db-pre-config
-------------

  =============== =======================
  **Type**        String
  **Default**     `n/a`
  **Reloadable**  Y
  **Environment** PGRST_DB_PRE_CONFIG
  **In-Database** pgrst.db_pre_config
  =============== =======================

   Name of the function that does :ref:`in_db_config`.

.. _db-extra-search-path:

db-extra-search-path
--------------------

  =============== ==========================
  **Type**        String
  **Default**     public
  **Reloadable**  Y
  **Environment** PGRST_DB_EXTRA_SEARCH_PATH
  **In-Database** pgrst.db_extra_search_path
  =============== ==========================

  Extra schemas to add to the `search_path <https://www.postgresql.org/docs/current/ddl-schemas.html#DDL-SCHEMAS-PATH>`_ of every request. These schemas tables, views and functions **don't get API endpoints**, they can only be referred from the database objects inside your :ref:`db-schemas`.

  This parameter was meant to make it easier to use **PostgreSQL extensions** (like PostGIS) that are outside of the :ref:`db-schemas`.

  Multiple schemas can be added in a comma-separated string, e.g. ``public, extensions``.

.. _db-max-rows:

db-max-rows
-----------

  =============== ==========================
  **Type**        Int
  **Default**     ∞
  **Reloadable**  Y
  **Environment** PGRST_DB_MAX_ROWS
  **In-Database** pgrst.db_max_rows
  =============== ==========================

  *For backwards compatibility, this config parameter is also available without prefix as "max-rows".*

  A hard limit to the number of rows PostgREST will fetch from a view, table, or function. Limits payload size for accidental or malicious requests.

.. _db-plan-enabled:

db-plan-enabled
---------------

  =============== ==========================
  **Type**        Boolean
  **Default**     False
  **Reloadable**  Y
  **Environment** PGRST_DB_PLAN_ENABLED
  **In-Database** pgrst.db_plan_enabled
  =============== ==========================

  When this is set to :code:`true`, the execution plan of a request can be retrieved by using the :code:`Accept: application/vnd.pgrst.plan` header. See :ref:`explain_plan`.

.. _db-pool:

db-pool
-------

  =============== ==========================
  **Type**        Int
  **Default**     10
  **Reloadable**  N
  **Environment** PGRST_DB_POOL
  **In-Database** n/a
  =============== ==========================

  Number of maximum connections to keep open in PostgREST's database pool.

.. _db-pool-acquisition-timeout:

db-pool-acquisition-timeout
---------------------------

  =============== =================================
  **Type**        Int
  **Default**     10
  **Reloadable**  N
  **Environment** PGRST_DB_POOL_ACQUISITION_TIMEOUT
  **In-Database** `n/a`
  =============== =================================

  Specifies the maximum time in seconds that the request will wait for the pool to free up a connection slot to the database.

.. _db-pool-max-idletime:

db-pool-max-idletime
--------------------

  =============== =================================
  **Type**        Int
  **Default**     30
  **Reloadable**  N
  **Environment** PGRST_DB_POOL_MAX_IDLETIME
  **In-Database** `n/a`
  =============== =================================

   *For backwards compatibility, this config parameter is also available as “db-pool-timeout”.*

   Time in seconds to close idle pool connections.

.. _db-pool-max-lifetime:

db-pool-max-lifetime
--------------------

  =============== =================================
  **Type**        Int
  **Default**     1800
  **Reloadable**  N
  **Environment** PGRST_DB_POOL_MAX_LIFETIME
  **In-Database** `n/a`
  =============== =================================

  Specifies the maximum time in seconds of an existing connection in the pool.

.. _db-pool-automatic-recovery:

db-pool-automatic-recovery
--------------------------

  =============== =================================
  **Type**        Boolean
  **Default**     True
  **Reloadable**  Y
  **Environment** PGRST_DB_POOL_AUTOMATIC_RECOVERY
  **In-Database** `n/a`
  =============== =================================

  Enables or disables connection retrying.

  When disabled, PostgREST would terminate immediately after connection loss instead of retrying indefinitely. See :ref:`this section <automatic_recovery>` for more information.

.. _db-pre-request:

db-pre-request
--------------

  =============== =================================
  **Type**        String
  **Default**     `n/a`
  **Reloadable**  Y
  **Environment** PGRST_DB_PRE_REQUEST
  **In-Database** pgrst.db_pre_request
  =============== =================================

  *For backwards compatibility, this config parameter is also available without prefix as "pre-request".*

  A schema-qualified function name to call right after the :ref:`tx_settings` are set. See :ref:`pre-request`.

.. _db-prepared-statements:

db-prepared-statements
----------------------

  =============== =================================
  **Type**        Boolean
  **Default**     True
  **Reloadable**  Y
  **Environment** PGRST_DB_PREPARED_STATEMENTS
  **In-Database** pgrst.db_prepared_statements
  =============== =================================

  Enables or disables prepared statements.

  When disabled, the generated queries will be parameterized (invulnerable to SQL injection) but they will not be prepared (cached in the database session). Not using prepared statements will noticeably decrease performance, so it's recommended to always have this setting enabled.

  You should only set this to ``false`` when using PostgresSQL behind an external connection pooler such as PgBouncer working in transaction pooling mode. See :ref:`this section <external_connection_poolers>` for more information.

.. _db-root-spec:

db-root-spec
------------

  =============== =================================
  **Type**        String
  **Default**     `n/a`
  **Reloadable**  Y
  **Environment** PGRST_DB_ROOT_SPEC
  **In-Database** pgrst.db_root_spec
  =============== =================================

  Function to override the OpenAPI response. See :ref:`override_openapi`.

.. _db-schemas:

db-schemas
----------

  =============== =================================
  **Type**        String
  **Default**     public
  **Reloadable**  Y
  **Environment** PGRST_DB_SCHEMAS
  **In-Database** pgrst.db_schemas
  =============== =================================

  *For backwards compatibility, this config parameter is also available in singular as "db-schema".*

  The list of database schemas to expose to clients. See :ref:`schemas`.

.. _db-tx-end:

db-tx-end
---------

  =============== =================================
  **Type**        String
  **Default**     commit
  **Reloadable**  N
  **Environment** PGRST_DB_TX_END
  **In-Database** `n/a`
  =============== =================================

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

  =============== =================================
  **Type**        String
  **Default**     postgresql://
  **Reloadable**  N
  **Environment** PGRST_DB_URI
  **In-Database** `n/a`
  =============== =================================

  The standard `PostgreSQL connection string <https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING>`_, there are different ways to specify it:

URI Format
~~~~~~~~~~

  .. code::

    "postgres://authenticator:mysecretpassword@localhost:5433/postgres?parameters=val"

  - Under this format symbols and unusual characters in the password or other fields should be percent encoded to avoid a parse error.
  - If enforcing an SSL connection to the database is required you can use `sslmode <https://www.postgresql.org/docs/current/libpq-ssl.html#LIBPQ-SSL-SSLMODE-STATEMENTS>`_ in the URI, for example ``postgres://user:pass@host:5432/dbname?sslmode=require``.
  - The user with whom PostgREST connects to the database is also known as the ``authenticator`` role. For more information see :ref:`roles`.
  - When running PostgREST on the same machine as PostgreSQL, it is also possible to connect to the database using a `Unix socket <https://en.wikipedia.org/wiki/Unix_domain_socket>`_ and the `Peer Authentication method <https://www.postgresql.org/docs/current/auth-peer.html>`_ as an alternative to TCP/IP communication and authentication with a password, this also grants higher performance.  To do this you can omit the host and the password, e.g. ``postgres://user@/dbname``, see the `libpq connection string <https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING>`_ documentation for more details.

Keyword/Value Format
~~~~~~~~~~~~~~~~~~~~

  .. code::

    "host=localhost port=5433 user=authenticator password=mysecretpassword dbname=postgres"

LIBPQ Environment Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~

  .. code::

    PGHOST=localhost PGPORT=5433 PGUSER=authenticator PGDATABASE=postgres

  Any parameter that is not set in the above formats is read from `libpq environment variables <https://www.postgresql.org/docs/current/libpq-envars.html>`_. The default connection string is ``postgresql://``, which reads **all** parameters from the environment.

External config file
~~~~~~~~~~~~~~~~~~~~

  Choosing a value for this parameter beginning with the at sign such as ``@filename`` (e.g. ``@./configs/my-config``) loads the connection string out of an external file.

.. _jwt-aud:

jwt-aud
-------

  =============== =================================
  **Type**        String
  **Default**     `n/a`
  **Reloadable**  Y
  **Environment** PGRST_JWT_AUD
  **In-Database** pgrst.jwt_aud
  =============== =================================

  Specifies the `JWT audience claim <https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.3>`_. If this claim is present in the client provided JWT then you must set this to the same value as in the JWT, otherwise verifying the JWT will fail.

  .. warning::

     Using this setting will only reject tokens with a different audience claim. Tokens **without** audience claim will still be accepted.

.. _jwt-role-claim-key:

jwt-role-claim-key
------------------

  =============== =================================
  **Type**        String
  **Default**     .role
  **Reloadable**  Y
  **Environment** PGRST_JWT_ROLE_CLAIM_KEY
  **In-Database** pgrst.jwt_role_claim_key
  =============== =================================

  *For backwards compatibility, this config parameter is also available without prefix as "role-claim-key".*

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

  =============== =================================
  **Type**        String
  **Default**     `n/a`
  **Reloadable**  Y
  **Environment** PGRST_JWT_SECRET
  **In-Database** pgrst.jwt_secret
  =============== =================================

  The secret or `JSON Web Key (JWK) (or set) <https://datatracker.ietf.org/doc/html/rfc7517>`_ used to decode JWT tokens clients provide for authentication. For security the key must be **at least 32 characters long**. If this parameter is not specified then PostgREST refuses authentication requests. Choosing a value for this parameter beginning with the at sign such as :code:`@filename` loads the secret out of an external file. This is useful for automating deployments. Note that any binary secrets must be base64 encoded. Both symmetric and asymmetric cryptography are supported. For more info see :ref:`asym_keys`.

  Choosing a value for this parameter beginning with the at sign such as ``@filename`` (e.g. ``@./configs/my-config``) loads the secret out of an external file.

  .. warning::

     Only when using the :ref:`file_config`, if the ``jwt-secret`` contains a ``$`` character by itself it will give errors. In this case, use ``$$`` and PostgREST will interpret it as a single ``$`` character.

.. _jwt-secret-is-base64:

jwt-secret-is-base64
--------------------

  =============== =================================
  **Type**        Boolean
  **Default**     False
  **Reloadable**  Y
  **Environment** PGRST_JWT_SECRET_IS_BASE64
  **In-Database** pgrst.jwt_secret_is_base64
  =============== =================================

  When this is set to :code:`true`, the value derived from :code:`jwt-secret` will be treated as a base64 encoded secret.

.. _jwt-cache-max-lifetime:

jwt-cache-max-lifetime
----------------------

  =============== =================================
  **Type**        Int
  **Default**     0
  **Reloadable**  Y
  **Environment** PGRST_JWT_CACHE_MAX_LIFETIME
  **In-Database** pgrst.jwt_cache_max_lifetime
  =============== =================================

  Maximum number of seconds of lifetime for cached entries. The default :code:`0` disables caching. See :ref:`jwt_caching`.

.. _log-level:

log-level
---------

  =============== =================================
  **Type**        String
  **Default**     error
  **Reloadable**  N
  **Environment** PGRST_LOG_LEVEL
  **In-Database** `n/a`
  =============== =================================

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

  =============== =================================
  **Type**        String
  **Default**     follow-privileges
  **Reloadable**  Y
  **Environment** PGRST_OPENAPI_MODE
  **In-Database** pgrst.openapi_mode
  =============== =================================

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

.. _openapi-security-active:

openapi-security-active
-----------------------

  =============== =================================
  **Type**        Boolean
  **Default**     False
  **Reloadable**  Y
  **Environment** PGRST_OPENAPI_SECURITY_ACTIVE
  **In-Database** pgrst.openapi_security_active
  =============== =================================

When this is set to :code:`true`, security options are included in the :ref:`OpenAPI output <open-api>`.

.. _openapi-server-proxy-uri:

openapi-server-proxy-uri
------------------------

  =============== =================================
  **Type**        String
  **Default**     `n/a`
  **Reloadable**  N
  **Environment** PGRST_OPENAPI_SERVER_PROXY_URI
  **In-Database** pgrst.openapi_server_proxy_uri
  =============== =================================

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

.. _server_cors_allowed_origins:

server-cors-allowed-origins
---------------------------

  =============== ===================================
  **Type**        String
  **Default**     `n/a`
  **Reloadable**  N
  **Environment** PGRST_SERVER_CORS_ALLOWED_ORIGINS
  **In-Database** `pgrst.server_cors_allowed_origins`
  =============== ===================================

  Specifies allowed CORS origins in this config. See :ref:`cors`.

  When this is not set or set to :code:`""`, PostgREST **accepts** CORS requests from any domain.

.. _server-host:

server-host
-----------

  =============== =================================
  **Type**        String
  **Default**     !4
  **Reloadable**  N
  **Environment** PGRST_SERVER_HOST
  **In-Database** `n/a`
  =============== =================================

  Where to bind the PostgREST web server. In addition to the usual address options, PostgREST interprets these reserved addresses with special meanings:

  * :code:`*` - any IPv4 or IPv6 hostname
  * :code:`*4` - any IPv4 or IPv6 hostname, IPv4 preferred
  * :code:`!4` - any IPv4 hostname
  * :code:`*6` - any IPv4 or IPv6 hostname, IPv6 preferred
  * :code:`!6` - any IPv6 hostname

.. _server-port:

server-port
-----------

  =============== =================================
  **Type**        Int
  **Default**     3000
  **Reloadable**  N
  **Environment** PGRST_SERVER_PORT
  **In-Database** `n/a`
  =============== =================================

  The TCP port to bind the web server. Use ``0`` to automatically assign a port.

.. _server-trace-header:

server-trace-header
-------------------

  =============== =================================
  **Type**        String
  **Default**     `n/a`
  **Reloadable**  Y
  **Environment** PGRST_SERVER_TRACE_HEADER
  **In-Database** pgrst.server_trace_header
  =============== =================================

  The header name used to trace HTTP requests. See :ref:`trace_header`.

.. _server-timing-enabled:

server-timing-enabled
---------------------

  =============== =================================
  **Type**        Boolean
  **Default**     False
  **Reloadable**  Y
  **Environment** PGRST_SERVER_TIMING_ENABLED
  **In-Database** pgrst.server_timing_enabled
  =============== =================================

  Enables the `Server-Timing <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Server-Timing>`_ header.
  See :ref:`server-timing_header`.

.. _server-unix-socket:

server-unix-socket
------------------

  =============== =================================
  **Type**        String
  **Default**     `n/a`
  **Reloadable**  N
  **Environment** PGRST_SERVER_UNIX_SOCKET
  **In-Database** `n/a`
  =============== =================================

  `Unix domain socket <https://en.wikipedia.org/wiki/Unix_domain_socket>`_ where to bind the PostgREST web server.
  If specified, this takes precedence over :ref:`server-port`. Example:

  .. code:: bash

    server-unix-socket = "/tmp/pgrst.sock"

.. _server-unix-socket-mode:

server-unix-socket-mode
-----------------------

  =============== =================================
  **Type**        String
  **Default**     660
  **Reloadable**  N
  **Environment** PGRST_SERVER_UNIX_SOCKET_MODE
  **In-Database** `n/a`
  =============== =================================

  `Unix file mode <https://en.wikipedia.org/wiki/File_system_permissions>`_ to be set for the socket specified in :ref:`server-unix-socket`
  Needs to be a valid octal between 600 and 777.

  .. code:: bash

    server-unix-socket-mode = "660"
