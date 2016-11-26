## Creating the Test Database

To properly run postgrest tests one needs to create a database. To do so, use the test creation 
script `create_test_database` in the `test/` folder.

The script expects the following parameters:

    test/create_test_db connection_uri database_name [test_db_user] [test_db_user_password]

Use the [connection URI]() to specify the user, password, host, and port. Do not provide the database
in the connection URI. The Postgres role you are using to connect must be capable of creating new databases.

The `database_name` is the name of the database that `stack test` will connect to. If the database of the
same name already exists on the server, the script will first drop it and then re-create it.

Optionally, specify the database user `stack test` will use. The user will be given necessary permissions 
to reset the database after every test run.

If the user is not specified, the script will generate the role name `postgrest_test_` suffixed by the 
chosen database name, and will generate a random password for it.

Optionally, if specifying an existing user to be used for the test connection, one can specify the password
the user has.

The script will return the db uri to use in the tests--this uri corresponds to the `db-uri` parameter in the
configuration file that one would use in production.

Generating the user and the password allows one to create the database and run the tests against any postgres
server without any modifications to the server. (Such as allowing accounts without a passoword or setting up
trust authentication, or requiring the server to be on the same localhost the tests are run from).

## Running the Tests

To run the tests, one must supply the database uri in the environment variable `POSTGREST_TEST_CONNECTION`.

Typically, one would create the database and run the test in the same command line, using the `postgres` superuser:

    POSTGREST_TEST_CONNECTION=$(test/create_test_db "postgres://postgres:pwd@database-host" test_db) stack test

For repeated runs on the same database, one should export the connection variable:

    export POSTGREST_TEST_CONNECTION=$(test/create_test_db "postgres://postgres:pwd@database-host" test_db)
    stack test
    stack test
    ...


If the environment variable is empty or not specified, then the test runner will default to connection uri 

    postgres://postgrest_test@localhost/postgrest_test

This connection assumes the test server on the `localhost` with the user `postgrest_test` without the password
and the database of the same name.

## Destroying the Database

The test database will remain after the test, together with four new roles created on the postgres server. To 
permanently erase the created database and the roles, run the script `test/delete_test_database`, using the same
superuser role used for creating the database:

    test/destroy_test_db connection_uri database_name

## Testing with Docker

The ability to connect to non-locel postgres simplifies the test setup. One elegant way of testing is to use
a disposable Postgres in docker.

For example, if local development is on a mac with Docker for Mac installed:

    $ docker run --name db-scripting-test -e POSTGRES_PASSWORD=pwd -p 5434:5432 -d postgres
    $ POSTGREST_TEST_CONNECTION=$(test/create_test_db "postgres://postgres:pwd@localhost:5434" test_db) stack test

Addtionally, if one creates a docker container to run stack test (this is necessary on macOS Sierra with ghc below
8.0.1, where `stack test` fails), one can run postgres in a separate linked container, or use the locally installed
Postgres.app.

Build the test container with `test/Dockerfile.test`:

    $ docker build -t pgst-test - < text/Dockerfile.test
    $ mkdir .stack-work-docker ~/.stack-linux

The first run of the test container will take a long time while the dependencies get cached. Creating the `~/.stack-linux`
folder and mapping it as a volume into the container ensures that we can run the container in disposable mode and
not worry about subsequent runs being slow. `.stack-work-docker` is also mapped into the container and must be specified
when using stack from linux, not to interfere with the `.stack-work` for local development. (On Sierra, `stack build`
works, while `stack test` fails with ghc 8.0.1).

Linked containers:

    $ docker run --name pg -e POSTGRES_PASSWORD=pwd  -d postgres
    $ docker run --rm -it -v `pwd`:`pwd` -v ~/.stack-linux:/root/.stack --link pg:pg -w="`pwd`" -v `pwd`/.stack-work-docker:`pwd`/.stack-work pgst-test bash -c "POSTGREST_TEST_CONNECTION=$(test/create_test_db "postgres://postgres:pwd@pg" test_db) stack test"

Stack test in Docker for Mac, Postgres.app on mac:

    $ host_ip=$(ifconfig en0 | grep 'inet ' | cut -f 2 -d' ')
    $ export POSTGREST_TEST_CONNECTION=$(test/create_test_db "postgres://postgres@$HOST" test_db)
    $ docker run --rm -it -v `pwd`:`pwd` -v ~/.stack-linux:/root/.stack -v `pwd`/.stack-work-docker:`pwd`/.stack-work -e "HOST=$host_ip" -e "POSTGREST_TEST_CONNECTION=$POSTGREST_TEST_CONNECTION" -w="`pwd`" pgst-test bash -c "stack test"
    $ test/destroy_test_db "postgres://postgres@localhost" test_db
