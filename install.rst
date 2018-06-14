Binary Release
==============

[ `Download from release page <https://github.com/begriffs/postgrest/releases/latest>`_ ]

The release page has pre-compiled binaries for Mac OS X, Windows, and several Linux distributions. Extract the tarball and run the binary inside with the :code:`--help` flag to see usage instructions:

.. code-block:: bash

  # Untar the release (available at https://github.com/begriffs/postgrest/releases/latest)

  $ tar Jxf postgrest-[version]-[platform].tar.xz

  # Try running it
  $ ./postgrest --help

  # You should see a usage help message

.. note::

  If you see a dialog box like this on Windows, it may be that the :code:`pg_config` program is not in your system path.

  .. image:: _static/win-err-dialog.png

  It usually lives in :code:`C:\Program Files\PostgreSQL\<version>\bin`. See this `article <https://www.howtogeek.com/118594/how-to-edit-your-system-path-for-easy-command-line-access/>`_ about how to modify the system path.


PostgreSQL dependency
=====================

To use PostgREST you will need an underlying database (PostgreSQL version 9.5 or greater is required). You can use something like Amazon `RDS <https://aws.amazon.com/rds/>`_ but installing your own locally is cheaper and more convenient for development.

* `Instructions for OS X <http://exponential.io/blog/2015/02/21/install-postgresql-on-mac-os-x-via-brew/>`_
* `Instructions for Ubuntu 14.04 <https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-14-04>`_
* `Installer for Windows <http://www.enterprisedb.com/products-services-training/pgdownload#windows>`_

On Windows, PostgREST will fail to run unless the PostgreSQL binaries are on the system path. To test whether this is the case, run ``pg_config`` from the command line. You should see it output a list of paths.

.. _configuration:

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
server-proxy-uri  String
jwt-secret        String
jwt-aud           String
secret-is-base64  Bool    False
max-rows          Int     ∞
pre-request       String
app.settings.*    String
role-claim-key    String  .role
================  ======  =======  ========

db-uri
  The standard connection PostgreSQL `URI format <https://www.postgresql.org/docs/current/static/libpq-connect.html#AEN45347>`_. Symbols and unusual characters in the password or other fields should be percent encoded to avoid a parse error. If enforcing an SSL connection to the database is required you can use `sslmode <https://www.postgresql.org/docs/9.1/static/libpq-ssl.html#LIBPQ-SSL-SSLMODE-STATEMENTS>`_ in the URI, for example ``postgres://user:pass@host:5432/dbname?sslmode=require``.

  When running PostgREST on the same machine as PostgreSQL, it is also possible to connect to the database using a `Unix socket <https://en.wikipedia.org/wiki/Unix_domain_socket>`_ and the `Peer Authentication method <http://www.postgresql.org/docs/current/static/auth-methods.html#AUTH-PEER>`_ as an alternative to TCP/IP communication and authentication with a password, this also grants higher performance.  To do this you can omit the host and the password, e.g. ``postgres://user@/dbname``, see the `libpq connection string <https://www.postgresql.org/docs/10/static/libpq-connect.html#LIBPQ-CONNSTRING>`_ documentation for more details.

  On older systems like Centos 6, with older versions of libpq, a different db-uri syntax has to be used. In this case the URI is a string of space separated key-value pairs (key=value), so the example above would be :code:`"host=host user=user port=5432 dbname=dbname password=pass"`.
db-schema
  The database schema to expose to REST clients. Tables, views and stored procedures in this schema will get API endpoints.
db-anon-role
  The database role to use when executing commands on behalf of unauthenticated clients.
db-pool
  Number of connections to keep open in PostgREST's database pool. Having enough here for the maximum expected simultaneous client connections can improve performance. Note it's pointless to set this higher than the :code:`max_connections` GUC in your database.
server-host
  Where to bind the PostgREST web server. In addition to the usual address options, PostgREST interprets these reserved addresses with special meanings:

  * :code:`*` - any IPv4 or IPv6 hostname
  * :code:`*4` - any IPv4 or IPv6 hostname, IPv4 preferred
  * :code:`!4` - any IPv4 hostname
  * :code:`*6` - any IPv4 or IPv6 hostname, IPv6 preferred
  * :code:`!6` - any IPv6 hostname

server-port
  The port to bind the web server.
server-proxy-uri
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

jwt-secret
  The secret or `JSON Web Key (JWK) <https://tools.ietf.org/html/rfc7517>`_ used to decode JWT tokens clients provide for authentication. For security the key must be at least thirty-two characters long. If this parameter is not specified then PostgREST refuses authentication requests. Choosing a value for this parameter beginning with the at sign such as :code:`@filename` loads the secret out of an external file. This is useful for automating deployments. Note that any binary secrets must be base64 encoded. Both symmetric and asymmetric cryptography are supported. For more info see :ref:`asym_keys`.
jwt-aud
  Specifies the `JWT audience claim <https://tools.ietf.org/html/rfc7519#section-4.1.3>`_. If this claim is present in the client provided JWT then you must set this to the same value as in the JWT, otherwise verifying the JWT will fail.
secret-is-base64
  When this is set to :code:`true`, the value derived from :code:`jwt-secret` will be treated as a base64 encoded secret.
max-rows
  A hard limit to the number of rows PostgREST will fetch from a view, table, or stored procedure. Limits payload size for accidental or malicious requests.
pre-request
  A schema-qualified stored procedure name to call right after switching roles for a client request. This provides an opportunity to modify SQL variables or raise an exception to prevent the request from completing.
app.settings.*
  Arbitrary settings that will become database session settings. This can be used to pass in secret keys directly as strings, or via OS environment variables. For instance: :code:`app.settings.jwt_secret = "$(MYAPP_JWT_SECRET)"` will take :code:`MYAPP_JWT_SECRET` from the environment and make it available to postgresql functions as :code:`current_setting('app.settings.jwt_secret')`.
role-claim-key
  A JSPath DSL that specifies the location of the :code:`role` key in the JWT claims. This can be used to consume a JWT provided by a third party service like Auth0, Okta or Keycloak. Usage examples:

.. code:: bash

  # {"postgrest":{"roles": ["other", "author"]}}
  # the DSL accepts characters that are alphanumerical or one of "_$@" as keys
  role-claim-key = ".postgrest.roles[1]"

  # {"https://www.example.com/role": { "key": "author }}
  # non-alphanumerical characters can go inside quotes(escaped in the config value)
  role-claim-key = ".\"https://www.example.com/role\".key"

Running the Server
------------------

PostgREST outputs basic request logging to stdout. When running it in an SSH session you must detach it from stdout or it will be terminated when the session closes. The easiest technique is redirecting the output to a log file or to the syslog:

.. code-block:: bash

  ssh foo@example.com \
    'postgrest foo.conf </dev/null >/var/log/postgrest.log 2>&1 &'

  # another option is to pipe the output into "logger -t postgrest"

(Avoid :code:`nohup postgrest` because the HUP signal is used for manual :ref:`schema_reloading`.)


Docker
======

The official PostgREST Docker image consults an internal :code:`/etc/postgrest.conf` file. To customize this file you can either mount a replacement configuration file into the container, or use environment variables. The environment variables will be interpolated into the default config file.

These variables match the options shown in our :ref:`configuration` section, except they are capitalized, have a prefix, and use underscores. To get a list of the available environment variables, run this:

.. code-block:: bash

  docker inspect -f "{{.Config.Env}}" postgrest/postgrest

There are two ways to run the PostgREST container: with an existing external database, or through docker-compose.

Containerized PostgREST with native PostgreSQL
----------------------------------------------

The first way to run PostgREST in Docker is to connect it to an existing native database on the host.

.. code-block:: bash

  # Pull the official image
  docker pull postgrest/postgrest

  # Run the server
  docker run --rm --net=host -p 3000:3000 \
    -e PGRST_DB_URI="postgres://postgres@localhost/postgres" \
    -e PGRST_DB_ANON_ROLE="postgres" \
    postgrest/postgrest

The database connection string above is just an example. Adjust the role and password as necessary. You may need to edit PostgreSQL's :code:`pg_hba.conf` to grant the user local login access.

.. note::

  Docker on Mac does not support the :code:`--net=host` flag. Instead you'll need to create an IP address alias to the host. Requests for the IP address from inside the container are unable to resolve and fall back to resolution by the host.

  .. code-block:: bash

    sudo ifconfig lo0 10.0.0.10 alias

  You should then use 10.0.0.10 as the host in your database connection string. Also remember to include the IP address in the :code:`listen_address` within postgresql.conf. For instance:

  .. code-block:: bash

    listen_addresses = 'localhost,10.0.0.10'

Containerized PostgREST *and* db with docker-compose
----------------------------------------------------

To avoid having to install the database at all, you can run both it and the server in containers and link them together with docker-compose. Use this configuration:

.. code-block:: yaml

  # docker-compose.yml

  version: '3'
  services:
    server:
      image: postgrest/postgrest
      ports:
        - "3000:3000"
      links:
        - db:db
      environment:
        PGRST_DB_URI: postgres://app_user:password@db:5432/app_db
        PGRST_DB_SCHEMA: public
        PGRST_DB_ANON_ROLE: app_user #In production this role should not be the same as the one used for the connection
      depends_on:
        - db
    db:
      image: postgres
      ports:
        - "5432:5432"
      environment:
        POSTGRES_DB: app_db
        POSTGRES_USER: app_user
        POSTGRES_PASSWORD: password
    # Uncomment this if you want to persist the data.
    # volumes:
    #   - "./pgdata:/var/lib/postgresql/data"

Go into the directory where you saved this file and run :code:`docker-compose up`. You will see the logs of both the database and PostgREST, and be able to access the latter on port 3000.

If you want to have a visual overview of your API in your browser you can add swagger-ui to your :code:`docker-compose.yml`:

.. code-block:: yaml

  swagger:
    image: swaggerapi/swagger-ui
    ports:
      - "8080:8080"
    expose:
      - "8080"
    environment:
      API_URL: http://localhost:3000/

With this you can see the swagger-ui in your browser on port 8080.

.. _build_source:

Build from Source
=================

.. note::

  We discourage building and using PostgREST on **Alpine Linux** because of a reported GHC memory leak on that platform.

When a pre-built binary does not exist for your system you can build the project from source. You'll also need to do this if you want to help with development. `Stack <https://github.com/commercialhaskell/stack>`_ makes it easy. It will install any necessary Haskell dependencies on your system.

* `Install Stack <http://docs.haskellstack.org/en/stable/README.html#how-to-install>`_ for your platform
* Install Library Dependencies

  =====================  =======================================
  Operating System       Dependencies
  =====================  =======================================
  Ubuntu/Debian          libpq-dev, libgmp-dev
  CentOS/Fedora/Red Hat  postgresql-devel, zlib-devel, gmp-devel
  BSD                    postgresql95-client
  OS X                   libpq, gmp
  =====================  =======================================

* Build and install binary

  .. code-block:: bash

    git clone https://github.com/begriffs/postgrest.git
    cd postgrest

    # adjust local-bin-path to taste
    stack build --install-ghc --copy-bins --local-bin-path /usr/local/bin

.. note::

   If building fails and your system has less than 1GB of memory, try adding a swap file.

* Check that the server is installed: :code:`postgrest --help`.

PostgREST Test Suite
--------------------

Creating the Test Database
~~~~~~~~~~~~~~~~~~~~~~~~~~

To properly run postgrest tests one needs to create a database. To do so, use the test creation script :code:`create_test_database` in the :code:`test/` folder.

The script expects the following parameters:

.. code:: bash

  test/create_test_db connection_uri database_name [test_db_user] [test_db_user_password]

Use the `connection URI <https://www.postgresql.org/docs/current/static/libpq-connect.html#AEN45347>`_ to specify the user, password, host, and port. Do not provide the database in the connection URI. The PostgreSQL role you are using to connect must be capable of creating new databases.

The :code:`database_name` is the name of the database that :code:`stack test` will connect to. If the database of the same name already exists on the server, the script will first drop it and then re-create it.

Optionally, specify the database user :code:`stack test` will use. The user will be given necessary permissions to reset the database after every test run.

If the user is not specified, the script will generate the role name :code:`postgrest_test_` suffixed by the chosen database name, and will generate a random password for it.

Optionally, if specifying an existing user to be used for the test connection, one can specify the password the user has.

The script will return the db uri to use in the tests--this uri corresponds to the :code:`db-uri` parameter in the configuration file that one would use in production.

Generating the user and the password allows one to create the database and run the tests against any PostgreSQL server without any modifications to the server. (Such as allowing accounts without a password or setting up trust authentication, or requiring the server to be on the same localhost the tests are run from).

Running the Tests
~~~~~~~~~~~~~~~~~

To run the tests, one must supply the database uri in the environment variable :code:`POSTGREST_TEST_CONNECTION`.

Typically, one would create the database and run the test in the same command line, using the `postgres` superuser:

.. code:: bash

  POSTGREST_TEST_CONNECTION=$(test/create_test_db "postgres://postgres:pwd@database-host" test_db) stack test

For repeated runs on the same database, one should export the connection variable:

.. code:: bash

  export POSTGREST_TEST_CONNECTION=$(test/create_test_db "postgres://postgres:pwd@database-host" test_db)
  stack test
  stack test
  ...

If the environment variable is empty or not specified, then the test runner will default to connection uri

.. code:: bash

  postgres://postgrest_test@localhost/postgrest_test

This connection assumes the test server on the :code:`localhost:code:` with the user `postgrest_test` without the password and the database of the same name.

Destroying the Database
~~~~~~~~~~~~~~~~~~~~~~~

The test database will remain after the test, together with four new roles created on the PostgreSQL server. To permanently erase the created database and the roles, run the script :code:`test/delete_test_database`, using the same superuser role used for creating the database:

.. code:: bash

  test/destroy_test_db connection_uri database_name

Testing with Docker
~~~~~~~~~~~~~~~~~~~

The ability to connect to non-local PostgreSQL simplifies the test setup. One elegant way of testing is to use a disposable PostgreSQL in docker.

For example, if local development is on a mac with Docker for Mac installed:

.. code:: bash

  $ docker run --name db-scripting-test -e POSTGRES_PASSWORD=pwd -p 5434:5432 -d postgres
  $ POSTGREST_TEST_CONNECTION=$(test/create_test_db "postgres://postgres:pwd@localhost:5434" test_db) stack test

Additionally, if one creates a docker container to run stack test (this is necessary on Mac OS Sierra with GHC below 8.0.1, where :code:`stack test` fails), one can run PostgreSQL in a separate linked container, or use the locally installed PostgreSQL app.

Build the test container with :code:`test/Dockerfile.test`:

.. code:: bash

  $ docker build -t pgst-test - < test/Dockerfile.test
  $ mkdir .stack-work-docker ~/.stack-linux

The first run of the test container will take a long time while the dependencies get cached. Creating the :code:`~/.stack-linux` folder and mapping it as a volume into the container ensures that we can run the container in disposable mode and not worry about subsequent runs being slow. :code:`.stack-work-docker` is also mapped into the container and must be specified when using stack from Linux, not to interfere with the :code:`.stack-work` for local development. (On Sierra, :code:`stack build` works, while :code:`stack test` fails with GHC 8.0.1).

Linked containers:

.. code:: bash

  $ docker run --name pg -e POSTGRES_PASSWORD=pwd  -d postgres
  $ docker run --rm -it -v `pwd`:`pwd` -v ~/.stack-linux:/root/.stack --link pg:pg -w="`pwd`" -v `pwd`/.stack-work-docker:`pwd`/.stack-work pgst-test bash -c "POSTGREST_TEST_CONNECTION=$(test/create_test_db "postgres://postgres:pwd@pg" test_db) stack test"

Stack test in Docker for Mac, PostgreSQL app on mac:

.. code:: bash

  $ host_ip=$(ifconfig en0 | grep 'inet ' | cut -f 2 -d' ')
  $ export POSTGREST_TEST_CONNECTION=$(test/create_test_db "postgres://postgres@$HOST" test_db)
  $ docker run --rm -it -v `pwd`:`pwd` -v ~/.stack-linux:/root/.stack -v `pwd`/.stack-work-docker:`pwd`/.stack-work -e "HOST=$host_ip" -e "POSTGREST_TEST_CONNECTION=$POSTGREST_TEST_CONNECTION" -w="`pwd`" pgst-test bash -c "stack test"
  $ test/destroy_test_db "postgres://postgres@localhost" test_db
