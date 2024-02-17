.. _health_check:

Health Check
############

You can enable a health check to verify if PostgREST is available for client requests. Also to check the status of its internal state.

To do this, set the configuration variable :ref:`admin-server-port` to the port number of your preference. Two endpoints ``live`` and ``ready`` will then be available.

The ``live`` endpoint verifies if PostgREST is running on its configured port. A request will return ``200 OK`` if PostgREST is alive or ``503`` otherwise.

The ``ready`` endpoint also checks the state of both the Database Connection and the :ref:`schema_cache`. A request will return ``200 OK`` if it is ready or ``503`` if not.

For instance, to verify if PostgREST is running at ``localhost:3000`` while the ``admin-server-port`` is set to ``3001``:

.. code-block:: bash

  curl -I "http://localhost:3001/live"

.. code-block:: http

  HTTP/1.1 200 OK

If you have a machine with multiple network interfaces and multiple PostgREST instances in the same port, you need to specify a unique :ref:`hostname <server-host>` in the configuration of each PostgREST instance for the health check to work correctly. Don't use the special values(``!4``, ``*``, etc) in this case because the health check could report a false positive.
