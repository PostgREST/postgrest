Binary Release
==============

The `release page <https://github.com/begriffs/postgrest/releases/latest>`_ has precompiled binaries for Mac OS X, Windows, and several Linux distros. Extract the tarball and run the binary inside with the :code:`--help` flag to see usage instructions:

.. code-block:: bash

  # Untar the release (available at https://github.com/begriffs/postgrest/releases/latest)

  $ tar zxf postgrest-[version]-[platform].tar.xz

  # Try running it
  $ ./postgrest --help

  # You should see a usage help message

Homebrew
========

You can use the Homebrew package manager to install PostgREST on Mac

.. code-block:: bash

  # Ensure brew is up to date
  brew update

  # Check for any problems with brew's setup
  brew doctor

  # Install the postgrest package
  brew install postgrest

This will automatically install PostgreSQL as a dependency. The process tends to take up to 15 minutes to install the package and its dependencies.

After installation completes, the tool is added to your $PATH and can be used from anywhere with:

.. code-block:: bash

  postgrest --help

PostgreSQL dependency
=====================

To use PostgREST you will need an underlying database (PostgreSQL version 9.3 or greater is required). You can use something like Amazon `RDS <https://aws.amazon.com/rds/>`_ but installing your own locally is cheaper and more convenient for development.

* `Instructions for OS X <http://exponential.io/blog/2015/02/21/install-postgresql-on-mac-os-x-via-brew/>`_
* `Instructions for Ubuntu 14.04 <https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-14-04>`_
* `Installer for Windows <http://www.enterprisedb.com/products-services-training/pgdownload#windows>`_

Build from Source
=================

.. note::

  We discourage building and using PostgREST on **Alpine Linux** because of a reported GHC memory leak on that platform.

When a pre-built binary does not exist for your system you can build the project from source. You'll also need to do this if you want to help with development. `Stack <https://github.com/commercialhaskell/stack>`_ makes it easy. It will install any necessary Haskell dependencies on your system.

* `Install Stack <http://docs.haskellstack.org/en/stable/README.html#how-to-install>`_ for your platform
* Install Library Dependencies

  =====================  ============================
  Operating System       Dependencies
  =====================  ============================
  Ubuntu/Debian          libpq-dev
  CentOS/Fedora/Red Hat  postgresql-devel, zlib-devel
  BSD                    postgresql95-server
  =====================  ============================

* Build and install binary

  .. code-block:: bash

    git clone https://github.com/begriffs/postgrest.git
    cd postgrest
    stack build --install-ghc
    sudo stack install --allow-different-user --local-bin-path /usr/local/bin
    
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

Use the `connection URI <https://www.postgresql.org/docs/current/static/libpq-connect.html#AEN45347>`_ to specify the user, password, host, and port. Do not provide the database in the connection URI. The Postgres role you are using to connect must be capable of creating new databases.

The :code:`database_name` is the name of the database that :code:`stack test` will connect to. If the database of the same name already exists on the server, the script will first drop it and then re-create it.

Optionally, specify the database user :code:`stack test` will use. The user will be given necessary permissions to reset the database after every test run.

If the user is not specified, the script will generate the role name :code:`postgrest_test_` suffixed by the chosen database name, and will generate a random password for it.

Optionally, if specifying an existing user to be used for the test connection, one can specify the password the user has.

The script will return the db uri to use in the tests--this uri corresponds to the :code:`db-uri` parameter in the configuration file that one would use in production.

Generating the user and the password allows one to create the database and run the tests against any postgres server without any modifications to the server. (Such as allowing accounts without a passoword or setting up trust authentication, or requiring the server to be on the same localhost the tests are run from).

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

The test database will remain after the test, together with four new roles created on the postgres server. To permanently erase the created database and the roles, run the script :code:`test/delete_test_database`, using the same superuser role used for creating the database:

.. code:: bash

  test/destroy_test_db connection_uri database_name

Testing with Docker
~~~~~~~~~~~~~~~~~~~

The ability to connect to non-local PostgreSQL simplifies the test setup. One elegant way of testing is to use a disposable PostgreSQL in docker.

For example, if local development is on a mac with Docker for Mac installed:

.. code:: bash

  $ docker run --name db-scripting-test -e POSTGRES_PASSWORD=pwd -p 5434:5432 -d postgres
  $ POSTGREST_TEST_CONNECTION=$(test/create_test_db "postgres://postgres:pwd@localhost:5434" test_db) stack test

Additionally, if one creates a docker container to run stack test (this is necessary on MacOS Sierra with GHC below 8.0.1, where :code:`stack test` fails), one can run PostgreSQL in a separate linked container, or use the locally installed Postgres.app.

Build the test container with :code:`test/Dockerfile.test`:

.. code:: bash

  $ docker build -t pgst-test - < test/Dockerfile.test
  $ mkdir .stack-work-docker ~/.stack-linux

The first run of the test container will take a long time while the dependencies get cached. Creating the :code:`~/.stack-linux` folder and mapping it as a volume into the container ensures that we can run the container in disposable mode and not worry about subsequent runs being slow. :code:`.stack-work-docker` is also mapped into the container and must be specified when using stack from Linux, not to interfere with the :code:`.stack-work` for local development. (On Sierra, :code:`stack build` works, while :code:`stack test` fails with GHC 8.0.1).

Linked containers:

.. code:: bash

  $ docker run --name pg -e POSTGRES_PASSWORD=pwd  -d postgres
  $ docker run --rm -it -v `pwd`:`pwd` -v ~/.stack-linux:/root/.stack --link pg:pg -w="`pwd`" -v `pwd`/.stack-work-docker:`pwd`/.stack-work pgst-test bash -c "POSTGREST_TEST_CONNECTION=$(test/create_test_db "postgres://postgres:pwd@pg" test_db) stack test"

Stack test in Docker for Mac, Postgres.app on mac:

.. code:: bash

  $ host_ip=$(ifconfig en0 | grep 'inet ' | cut -f 2 -d' ')
  $ export POSTGREST_TEST_CONNECTION=$(test/create_test_db "postgres://postgres@$HOST" test_db)
  $ docker run --rm -it -v `pwd`:`pwd` -v ~/.stack-linux:/root/.stack -v `pwd`/.stack-work-docker:`pwd`/.stack-work -e "HOST=$host_ip" -e "POSTGREST_TEST_CONNECTION=$POSTGREST_TEST_CONNECTION" -w="`pwd`" pgst-test bash -c "stack test"
  $ test/destroy_test_db "postgres://postgres@localhost" test_db
