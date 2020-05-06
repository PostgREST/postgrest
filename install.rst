Binary Release
==============

[ `Download from release page <https://github.com/PostgREST/postgrest/releases/latest>`_ ]

The release page has pre-compiled binaries for Mac OS X, Windows, and several Linux distributions. Extract the tarball and run the binary inside with the :code:`--help` flag to see usage instructions:

.. code-block:: bash

  # Untar the release (available at https://github.com/PostgREST/postgrest/releases/latest)

  $ tar Jxf postgrest-[version]-[platform].tar.xz

  # Try running it
  $ ./postgrest --help

  # You should see a usage help message

.. note::

  If you see a dialog box like this on Windows, it may be that the :code:`pg_config` program is not in your system path.

  .. image:: _static/win-err-dialog.png

  It usually lives in :code:`C:\Program Files\PostgreSQL\<version>\bin`. See this `article <https://www.howtogeek.com/118594/how-to-edit-your-system-path-for-easy-command-line-access/>`_ about how to modify the system path.

.. _pg-dependency:

PostgreSQL dependency
=====================

To use PostgREST you will need an underlying database. We require PostgreSQL 9.4 or greater, but recommend at least 9.5 for row-level security features.
You can use something like Amazon `RDS <https://aws.amazon.com/rds/>`_ but installing your own locally is cheaper and more convenient for development.

* `Instructions for OS X <http://exponential.io/blog/2015/02/21/install-postgresql-on-mac-os-x-via-brew/>`_
* `Instructions for Ubuntu 14.04 <https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-14-04>`_
* `Installer for Windows <http://www.enterprisedb.com/products-services-training/pgdownload#windows>`_

On Windows, PostgREST will fail to run unless the PostgreSQL binaries are on the system path. To test whether this is the case, run ``pg_config`` from the command line. You should see it output a list of paths.

Configuration
=============

The PostgREST server reads a configuration file as its only argument:

.. code:: bash

  ./postgrest /path/to/postgrest.conf

For a complete reference of the configuration file, see :ref:`configuration`.

Running the Server
==================

PostgREST outputs basic request logging to stdout. When running it in an SSH session you must detach it from stdout or it will be terminated when the session closes. The easiest technique is redirecting the output to a log file or to the syslog:

.. code-block:: bash

  ssh foo@example.com \
    'postgrest foo.conf </dev/null >/var/log/postgrest.log 2>&1 &'

  # another option is to pipe the output into "logger -t postgrest"

Docker
======

You can get the `official PostgREST Docker image <https://hub.docker.com/r/postgrest/postgrest>`_ with:

.. code-block:: bash

  docker pull postgrest/postgrest

The image consults an internal ``/etc/postgrest.conf`` file. To customize this file you can either mount a replacement configuration file into the container, or use environment variables. The environment variables will be interpolated into the default config file.

These variables match the options shown in our :ref:`configuration` section, except they are capitalized, have a ``PGRST_`` prefix, and use underscores. To get a list of the available environment variables, run this:

.. code-block:: bash

  docker inspect -f "{{.Config.Env}}" postgrest/postgrest

You can also specify a config file by mounting the file to the container:

.. code-block:: bash

  docker run -v /absolute/path/to/config:/etc/postgrest.conf postgrest/postgrest

There are two ways to run the PostgREST container: with an existing external database, or through docker-compose.

Containerized PostgREST with native PostgreSQL
----------------------------------------------

The first way to run PostgREST in Docker is to connect it to an existing native database on the host.

.. code-block:: bash

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

  You might also need to add a new IPv4 local connection within pg_hba.conf. For instance:

  .. code-block:: bash

    host    all             all             10.0.0.10/32            trust

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
        PGRST_SERVER_PROXY_URI: "http://127.0.0.1:3000"
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

Deploying to Heroku
===================
Assuming your making modifications locally and then pushing to GitHub, it's easy to deploy to Heroku.

1. Create a new app on Heroku
2. In Settings add the following buildpack :code:`https://github.com/PostgREST/postgrest-heroku`
3. Add the require Config Vars in Heroku (see https://github.com/PostgREST/postgrest/blob/master/app.json#L7-L57 for more details)
4. Modify your postgrest.conf file as required to match your Config Vars in Heroku
5. Create your :code:`Procfile` and add :code:`./env-to-config ./postgrest postgrest.conf`
6. Push your changes to GitHub
7. Set Heroku to automatically deploy from Master and then manually deploy the branch for the first build


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

    git clone https://github.com/PostgREST/postgrest.git
    cd postgrest

    # adjust local-bin-path to taste
    stack build --install-ghc --copy-bins --local-bin-path /usr/local/bin

.. note::

   If building fails and your system has less than 1GB of memory, try adding a swap file.

* Check that the server is installed: :code:`postgrest --help`.

PostgREST Test Suite
--------------------

To properly run the test suite, you need a Postgres database that the tests can run against. There are several ways to set up this database. 

Testing with a temporary database
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have Postgres installed locally (:code:`initdb`, :code:`pg_ctl` and :code:`psql` should be on your PATH, no server needs to be running), you can run the test suite against a temporary database:

.. code:: bash

   test/with_tmp_db stack test
   
The :code:`with_tmp_db` script will set up a new Postgres cluster in a temporary directory, set the required environment variables and run the command that you passed it as an argument, :code:`stack test` in the example above. When the command is done, the temporary database is torn down and deleted again.

Manually creating the Test Database
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To manually create a database for testing, use the test creation script :code:`create_test_database` in the :code:`test/` folder.

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

Running the Tests with the manually created database
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
