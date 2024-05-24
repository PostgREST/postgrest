.. _cli:

CLI
===

PostgREST provides a CLI with the commands listed below:

Help
----

.. code:: bash

  $ postgrest [-h|--help]

Shows all the commands available.

Version
-------

.. code:: bash

  $ postgrest [-v|--version]

Prints the PostgREST version.

Example
-------

.. code:: bash

  $ postgrest [-e|--example]

Shows example configuration options.

Dump Config
-----------

.. code:: bash

  $ postgrest [--dump-config]

Dumps the loaded :ref:`configuration` values, considering the configuration file, environment variables and :ref:`in_db_config`.

Dump Schema
-----------

.. code:: bash

  $ postgrest [--dump-schema]

Dumps the schema cache in JSON format.
