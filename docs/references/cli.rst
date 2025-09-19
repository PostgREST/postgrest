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

Ready Flag
----------

.. code-block:: bash

  $ postgrest [--ready]

Makes a request to the ``/ready`` endpoint of the :ref:`admin_server`. It exits with a return code of ``0`` on success and ``1`` on failure.

.. code-block:: bash

  $ postgrest --ready
  OK: http://localhost:3001/ready

.. note::

  The ``--ready`` flag cannot be used when :ref:`server-host` is configured with special hostnames. We suggest to change it to ``localhost``.
