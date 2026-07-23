.. _cli:

CLI
===

PostgREST provides a CLI with the options listed below:

.. code:: text

  Usage: postgrest [-v|--version] [-e|--example] [--dump-config | --dump-schema | --ready]
                   [--schema-cache-uri URI] [FILENAME]

    PostgREST / create a REST API to an existing Postgres
    database

  Available options:
    -h,--help                Show this help text
    -v,--version             Show the version information
    -e,--example             Show an example configuration file
    --dump-config            Dump loaded configuration and exit
    --dump-schema            Dump loaded schema as JSON and exit (for debugging,
                             output structure is unstable)
    --ready                  Checks the health of PostgREST by doing a request on
                             the admin server /ready endpoint
    --schema-cache-uri URI   Try pre-loading schema cache from provided URI
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

Schema Cache URI
----------------

.. code:: bash

  $ postgrest --schema-cache-uri http://localhost/schema-cache.json

Tries to load the initial :ref:`schema_cache` from the given URI when starting PostgREST to speed up its startup.
The load is asynchronous. Database-backed schema cache load is started in parallel and will overwrite values loaded from the URI once finished.

The supported URI schemes are:

``file:``
  Loads a schema cache dump from a local file. The URI must point to a local path, for
  example ``file:///var/lib/postgrest/schema-cache.json``. Remote file authorities are
  not supported.

``http:`` and ``https:``
  Loads a schema cache dump over HTTP(S). The response body must be a schema cache JSON
  dump, such as the output of ``postgrest --dump-schema`` or the runtime cache exposed
  by another PostgREST instance's :ref:`admin_server`.

Other schemes are rejected as unsupported.

Ready Flag
----------

Makes a request to the ``/ready`` endpoint of the :ref:`admin_server`. It exits with a return code of ``0`` on success and ``1`` on failure.

.. code-block:: bash

  $ postgrest --ready
  OK: http://localhost:3001/ready

.. note::

  The ``--ready`` flag cannot be used when :ref:`server-host` is configured with special hostnames. We suggest to change it to ``localhost``.
