.. _cli:

CLI
===

PostgREST provides a CLI to start and run your postgrest service. The CLI provides the commands listed below:

.. _cli_commands:

CLI Commands
------------

Help and Version
~~~~~~~~~~~~~~~~

.. code:: bash

  $ postgrest [-h|--help]
  $ postgrest [-v|--version]


Example Config
~~~~~~~~~~~~~~

.. code:: bash

  $ postgrest [--example-file]
  $ postgrest [--example-db]
  $ postgrest [--example-env]

These commands show the example configuration file of the selected type.


Config or Schema Cache
~~~~~~~~~~~~~~~~~~~~~~

.. code:: bash
  
  $ postgrest [--dump-config|--dump-schema-cache] [FILENAME]

Here ``FILENAME`` is the path to configuration file.
