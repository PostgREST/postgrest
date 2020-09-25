.. _build_source:

Build from Source
=================

.. note::

  We discourage building and using PostgREST on **Alpine Linux** because of a reported GHC memory leak on that platform.

To help with development, you'll need to build from source. `Stack <https://github.com/commercialhaskell/stack>`_ makes it easy. It will install any necessary Haskell dependencies on your system.

* `Install Stack <http://docs.haskellstack.org/en/stable/README.html#how-to-install>`_ for your platform
* Install Library Dependencies

  =====================  =======================================
  Operating System       Dependencies
  =====================  =======================================
  Ubuntu/Debian          libpq-dev, libgmp-dev, zlib1g-dev
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

   - If building fails and your system has less than 1GB of memory, try adding a swap file.
   - `--install-ghc` flag is only needed for the first build and can be omitted in the subsequent builds.

* Check that the server is installed: :code:`postgrest --help`.

Running the Test Suite
======================

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
