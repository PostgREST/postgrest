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

  $ postgrest [-e|--example]

These commands show the example configuration file.

Config
~~~~~~

.. code:: bash

  $ postgrest [--dump-config] [FILENAME]

Here ``FILENAME`` is the path to configuration file.

Schema Cache
~~~~~~~~~~~~

.. code:: bash

  $ postgrest [--dump-schema] [FILENAME]

Here ``FILENAME`` is the path to configuration file.
