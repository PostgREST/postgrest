.. _cli:

CLI
===

PostgREST provides a CLI with the options listed below:

.. code:: text

  Usage: postgrest [-v|--version] [-e|--example] [--dump-config | --dump-schema] 
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
