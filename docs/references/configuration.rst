.. _configuration:

Configuration
#############

Config parameters can be provided via :ref:`file_config`, :ref:`env_variables_config` or :ref:`in_db_config`. Using :ref:`config_reloading` you can modify the parameters without restarting the server.

Without configuration, PostgREST won't be able to serve requests. At the minimum it needs either :ref:`a role to serve anonymous requests with <db-anon-role>` - or :ref:`a secret to use for JWT authentication <jwt-secret>`.

To connect to a database it uses a `libpq connection string <https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING>`_. The connection string can be set in the configuration file or via environment variable or can be read from an external file. See :ref:`db-uri` for details. Any parameter that is not set in the connection string is read from `libpq environment variables <https://www.postgresql.org/docs/current/libpq-envars.html>`_. The default connection string is ``postgresql://``, which reads **all** parameters from the environment.

Config parameters are read in the following order:

1. From the config file.
2. From environment variables, overriding values from the config file.
3. From the database, overriding values from both the config file and environment variables.

.. _file_config:

Config File
===========

PostgREST can read a config file. There is no predefined location for this file, you must specify the file path as the one and only argument to the server:

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
  jwt-secret-is-base64 = False

  # Port the postgrest process is listening on for http requests
  server-port = 80

You can run ``postgrest --example`` to display all possible configuration parameters and how to use them in a configuration file.

.. _env_variables_config:

Environment Variables
=====================

You can also set these :ref:`configuration parameters <config_full_list>` using environment variables. They are capitalized, have a ``PGRST_`` prefix, and use underscores. For example: ``PGRST_DB_URI`` corresponds to ``db-uri`` and ``PGRST_APP_SETTINGS_*`` to ``app.settings.*``.

See the full list of environment variable names on :ref:`config_full_list`.

.. _in_db_config:

In-Database Configuration
=========================

Using a :ref:`pre-config <db-pre-config>` function, you can configure the server with database settings. For example, you can configure :ref:`db-schemas` and :ref:`jwt-secret` like this:

.. code-block::

  # postgrest.conf

  db-pre-config  = "postgrest.pre_config"

  # or env vars

  PGRST_DB_PRE_CONFIG = "postgrest.pre_config"

.. code-block:: postgresql

  -- create a dedicated schema, hidden from the API
  create schema postgrest;
  -- grant usage on this schema to the authenticator
  grant usage on schema postgrest to authenticator;

  -- the function can configure postgREST by using set_config
  create or replace function postgrest.pre_config()
  returns void as $$
    select
        set_config('pgrst.db_schemas', 'schema1, schema2', true)
      , set_config('pgrst.db_jwt_secret', 'REALLYREALLYREALLYREALLYVERYSAFE', true);
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

=========================== ======= ================= ========== ================================= ==============================
Name                        Type    Default           Reloadable Environment variable              In-database name
=========================== ======= ================= ========== ================================= ==============================
admin-server-port           Int                                  PGRST_ADMIN_SERVER_PORT
app.settings.*              String                    Y          PGRST_APP_SETTINGS_*
db-anon-role                String                    Y          PGRST_DB_ANON_ROLE                pgrst.db_anon_role
db-channel                  String  pgrst             Y          PGRST_DB_CHANNEL
db-channel-enabled          Boolean True              Y          PGRST_DB_CHANNEL_ENABLED
db-config                   Boolean True              Y          PGRST_DB_CONFIG
db-pre-config               String                    Y          PGRST_DB_PRE_CONFIG               pgrst.db_pre_config
db-extra-search-path        String  public            Y          PGRST_DB_EXTRA_SEARCH_PATH        pgrst.db_extra_search_path
db-max-rows                 Int     ∞                 Y          PGRST_DB_MAX_ROWS                 pgrst.db_max_rows
db-plan-enabled             Boolean False             Y          PGRST_DB_PLAN_ENABLED             pgrst.db_plan_enabled
db-pool                     Int     10                           PGRST_DB_POOL
db-pool-acquisition-timeout Int     10                           PGRST_DB_POOL_ACQUISITION_TIMEOUT
db-pool-max-lifetime        Int     1800                         PGRST_DB_POOL_MAX_LIFETIME
db-pre-request              String                    Y          PGRST_DB_PRE_REQUEST              pgrst.db_pre_request
db-prepared-statements      Boolean True              Y          PGRST_DB_PREPARED_STATEMENTS      pgrst.db_prepared_statements
db-root-spec                String                    Y          PGRST_DB_ROOT_SPEC                pgrst.db_root_spec
db-schemas                  String  public            Y          PGRST_DB_SCHEMAS                  pgrst.db_schemas
db-tx-end                   String  commit                       PGRST_DB_TX_END
db-uri                      String  postgresql://                PGRST_DB_URI
db-use-legacy-gucs          Boolean True              Y          PGRST_DB_USE_LEGACY_GUCS          pgrst.db_use_legacy_gucs
jwt-aud                     String                    Y          PGRST_JWT_AUD                     pgrst.jwt_aud
jwt-role-claim-key          String  .role             Y          PGRST_JWT_ROLE_CLAIM_KEY          pgrst.jwt_role_claim_key
jwt-secret                  String                    Y          PGRST_JWT_SECRET                  pgrst.jwt_secret
jwt-secret-is-base64        Boolean False             Y          PGRST_JWT_SECRET_IS_BASE64        pgrst.jwt_secret_is_base64
log-level                   String  error                        PGRST_LOG_LEVEL
openapi-mode                String  follow-privileges Y          PGRST_OPENAPI_MODE                pgrst.openapi_mode
openapi-security-active     Boolean False             Y          PGRST_OPENAPI_SECURITY_ACTIVE     pgrst.openapi_security_active
openapi-server-proxy-uri    String                    Y          PGRST_OPENAPI_SERVER_PROXY_URI    pgrst.openapi_server_proxy_uri
raw-media-types             String                    Y          PGRST_RAW_MEDIA_TYPES             pgrst.raw_media_types
server-host                 String  !4                           PGRST_SERVER_HOST
server-port                 Int     3000                         PGRST_SERVER_PORT
server-trace-header         String                    Y          PGRST_SERVER_TRACE_HEADER         pgrst.server_trace_header
server-unix-socket          String                               PGRST_SERVER_UNIX_SOCKET
server-unix-socket-mode     String  660                          PGRST_SERVER_UNIX_SOCKET_MODE
=========================== ======= ================= ========== ================================= ==============================

.. _admin-server-port:

admin-server-port
-----------------

  Specifies the port for the :ref:`health_check` endpoints.

.. _app.settings.*:

app.settings.*
--------------

  Arbitrary settings that can be used to pass in secret keys directly as strings, or via OS environment variables. For instance: :code:`app.settings.jwt_secret = "$(MYAPP_JWT_SECRET)"` will take :code:`MYAPP_JWT_SECRET` from the environment and make it available to postgresql functions as :code:`current_setting('app.settings.jwt_secret')`.

.. _db-anon-role:

db-anon-role
------------

  The database role to use when executing commands on behalf of unauthenticated clients. For more information, see :ref:`roles`.

  When unset anonymous access will be blocked.

.. _db-channel:

db-channel
----------

  The name of the notification channel that PostgREST uses for :ref:`schema_reloading` and configuration reloading.

.. _db-channel-enabled:

db-channel-enabled
------------------

  When this is set to :code:`true`, the notification channel specified in :ref:`db-channel` is enabled.

  You should set this to ``false`` when using PostgresSQL behind an external connection pooler such as PgBouncer working in transaction pooling mode. See :ref:`this section <external_connection_poolers>` for more information.

.. _db-config:

db-config
---------

   Enables the in-database configuration.

.. _db-pre-config:

db-pre-config
-------------

   Name of the function that does in-database configuration.

.. _db-extra-search-path:

db-extra-search-path
--------------------

  Extra schemas to add to the `search_path <https://www.postgresql.org/docs/current/ddl-schemas.html#DDL-SCHEMAS-PATH>`_ of every request. These schemas tables, views and stored procedures **don't get API endpoints**, they can only be referred from the database objects inside your :ref:`db-schemas`.

  This parameter was meant to make it easier to use **PostgreSQL extensions** (like PostGIS) that are outside of the :ref:`db-schemas`.

  Multiple schemas can be added in a comma-separated string, e.g. ``public, extensions``.

.. _db-max-rows:

db-max-rows
-----------

  *For backwards compatibility, this config parameter is also available without prefix as "max-rows".*

  A hard limit to the number of rows PostgREST will fetch from a view, table, or stored procedure. Limits payload size for accidental or malicious requests.

.. _db-plan-enabled:

db-plan-enabled
---------------

  When this is set to :code:`true`, the execution plan of a request can be retrieved by using the :code:`Accept: application/vnd.pgrst.plan` header. See :ref:`explain_plan`.

  It's recommended to use this in testing environments only since it reveals internal database details.
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

.. _db-pool:

db-pool
-------

  Number of maximum connections to keep open in PostgREST's database pool.

.. _db-pool-acquisition-timeout:

db-pool-acquisition-timeout
---------------------------

  Specifies the maximum time in seconds that the request will wait for the pool to free up a connection slot to the database.

.. _db-pool-max-lifetime:

db-pool-max-lifetime
--------------------

  Specifies the maximum time in seconds of an existing connection in the pool.

.. _db-pre-request:

db-pre-request
--------------

  *For backwards compatibility, this config parameter is also available without prefix as "pre-request".*

  A schema-qualified stored procedure name to call right after the :ref:`tx_settings` are set. See :ref:`pre-request`.

.. _db-prepared-statements:

db-prepared-statements
----------------------

  Enables or disables prepared statements.

  When disabled, the generated queries will be parameterized (invulnerable to SQL injection) but they will not be prepared (cached in the database session). Not using prepared statements will noticeably decrease performance, so it's recommended to always have this setting enabled.

  You should only set this to ``false`` when using PostgresSQL behind an external connection pooler such as PgBouncer working in transaction pooling mode. See :ref:`this section <external_connection_poolers>` for more information.

.. _db-root-spec:

db-root-spec
------------

  Function to override the OpenAPI response. See :ref:`override_openapi`.

.. _db-schemas:

db-schemas
----------

  *For backwards compatibility, this config parameter is also available in singular as "db-schema".*

  The list of database schemas to expose to clients. See :ref:`schemas`.

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

.. _db-uri:

db-uri
------

  The standard connection PostgreSQL `URI format <https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING>`_. Symbols and unusual characters in the password or other fields should be percent encoded to avoid a parse error. If enforcing an SSL connection to the database is required you can use `sslmode <https://www.postgresql.org/docs/current/libpq-ssl.html#LIBPQ-SSL-SSLMODE-STATEMENTS>`_ in the URI, for example ``postgres://user:pass@host:5432/dbname?sslmode=require``.

  The user with whom PostgREST connects to the database is also known as the ``authenticator`` role. For more information see :ref:`roles`.

  When running PostgREST on the same machine as PostgreSQL, it is also possible to connect to the database using a `Unix socket <https://en.wikipedia.org/wiki/Unix_domain_socket>`_ and the `Peer Authentication method <https://www.postgresql.org/docs/current/auth-peer.html>`_ as an alternative to TCP/IP communication and authentication with a password, this also grants higher performance.  To do this you can omit the host and the password, e.g. ``postgres://user@/dbname``, see the `libpq connection string <https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING>`_ documentation for more details.

  Choosing a value for this parameter beginning with the at sign such as ``@filename`` (e.g. ``@./configs/my-config``) loads the connection string out of an external file.

.. _db-use-legacy-gucs:

db-use-legacy-gucs
------------------

  Determine if GUC request settings for headers, cookies and jwt claims use the `legacy names <https://postgrest.org/en/v8.0/api.html#accessing-request-headers-cookies-and-jwt-claims>`_ (string with dashes, invalid starting from PostgreSQL v14) with text values instead of the :ref:`new names <guc_req_headers_cookies_claims>` (string without dashes, valid on all PostgreSQL versions) with json values.

  On PostgreSQL versions 14 and above, this parameter is ignored.

.. _jwt-aud:

jwt-aud
-------

  Specifies the `JWT audience claim <https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.3>`_. If this claim is present in the client provided JWT then you must set this to the same value as in the JWT, otherwise verifying the JWT will fail.

.. _jwt-role-claim-key:

jwt-role-claim-key
------------------

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

  The secret or `JSON Web Key (JWK) (or set) <https://datatracker.ietf.org/doc/html/rfc7517>`_ used to decode JWT tokens clients provide for authentication. For security the key must be **at least 32 characters long**. If this parameter is not specified then PostgREST refuses authentication requests. Choosing a value for this parameter beginning with the at sign such as :code:`@filename` loads the secret out of an external file. This is useful for automating deployments. Note that any binary secrets must be base64 encoded. Both symmetric and asymmetric cryptography are supported. For more info see :ref:`asym_keys`.

  Choosing a value for this parameter beginning with the at sign such as ``@filename`` (e.g. ``@./configs/my-config``) loads the secret out of an external file.

  .. warning::

     Only when using the :ref:`file_config`, if the ``jwt-secret`` contains a ``$`` character by itself it will give errors. In this case, use ``$$`` and PostgREST will interpret it as a single ``$`` character.

.. _jwt-secret-is-base64:

jwt-secret-is-base64
--------------------

  When this is set to :code:`true`, the value derived from :code:`jwt-secret` will be treated as a base64 encoded secret.

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
      log-level = "info"


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

.. _openapi-security-active:

openapi-security-active
-----------------------

When this is set to :code:`true`, security options are included in the :ref:`OpenAPI output <open-api>`.

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

.. _raw-media-types:

raw-media-types
---------------

 This serves to extend the `Media Types <https://en.wikipedia.org/wiki/Media_type>`_ that PostgREST currently accepts through an ``Accept`` header.

 These media types can be requested by following the same rules as the ones defined in :ref:`scalar_return_formats`.

 As an example, the below config would allow you to request an **image** and a **XML** file by doing a request with ``Accept: image/png``
 or ``Accept: font/woff2``, respectively.

 .. code:: bash

   raw-media-types="image/png, font/woff2"

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

.. _server-trace-header:

server-trace-header
-------------------

  The header name used to trace HTTP requests. See :ref:`trace_header`.

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
