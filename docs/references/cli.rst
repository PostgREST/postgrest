.. _cli:

CLI
===

PostgREST provides a CLI with the options listed below:

.. code:: text

  Usage: postgrest [-v|--version] [-e|--example]
                   [--dump-config | --dump-schema | --query-to-sql QUERY | --ready]
                   [FILENAME]

    PostgREST / create a REST API to an existing Postgres
    database

  Available options:
    -h,--help                Show this help text
    -v,--version             Show the version information
    -e,--example             Show an example configuration file
    --dump-config            Dump loaded configuration and exit
    --dump-schema            Dump loaded schema as JSON and exit (for debugging,
                             output structure is unstable)
    --query-to-sql QUERY     Translate a PostgREST URL query to SQL and exit
                             (e.g., "/items?select=id&id=gt.5")
    --ready                  Checks the health of PostgREST by doing a request on
                             the admin server /ready endpoint
    FILENAME                 Path to configuration file

FILENAME
--------

Runs PostgREST with the given :ref:`file_config`.

Help
----

.. code:: bash

  $ postgrest --help

Shows all the options available.

Version
-------

.. code:: bash

  $ postgrest --version

Prints the PostgREST version.

Example
-------

.. code:: bash

  $ postgrest --example

Shows example configuration settings.

Dump Config
-----------

.. code:: bash

  $ postgrest --dump-config

Dumps the loaded :ref:`configuration` values, considering the configuration file, environment variables and :ref:`in_db_config`.

Dump Schema
-----------

.. code:: bash

  $ postgrest --dump-schema

Dumps the schema cache in JSON format.

Query to SQL
------------

.. code:: bash

  $ postgrest --query-to-sql "/items?select=id&id=gt.5"

Translates a PostgREST URL query to SQL and prints the resulting query. This is useful for debugging and understanding how PostgREST translates API requests to SQL.

The command connects to the database to load the schema cache, then translates the provided query without executing it.

.. code-block:: bash

  $ postgrest --query-to-sql "/items?select=id,name&id=gt.5"
  WITH pgrst_source AS ( SELECT "public"."items"."id", "public"."items"."name" FROM "public"."items" WHERE "public"."items"."id" > $1 ) ...

Ready Flag
----------

Makes a request to the ``/ready`` endpoint of the :ref:`admin_server`. It exits with a return code of ``0`` on success and ``1`` on failure.

.. code-block:: bash

  $ postgrest --ready
  OK: http://localhost:3001/ready

.. note::

  The ``--ready`` flag cannot be used when :ref:`server-host` is configured with special hostnames. We suggest to change it to ``localhost``.
