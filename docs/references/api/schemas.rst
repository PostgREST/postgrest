.. _schemas:

Schemas
=======

PostgREST can expose a single or multiple schema's tables, views and functions. The :ref:`active database role <roles>` must have the usage privilege on the schemas to access them.

Single schema
-------------

To expose a single schema, specify a single value in :ref:`db-schemas`.

.. code:: bash

   db-schemas = "api"

This schema is added to the `search_path <https://www.postgresql.org/docs/current/ddl-schemas.html#DDL-SCHEMAS-PATH>`_ of every request using :ref:`tx_settings`.

.. _multiple-schemas:

Multiple schemas
----------------

To expose multiple schemas, specify a comma-separated list on :ref:`db-schemas`:

.. code:: bash

   db-schemas = "tenant1, tenant2"

To switch schemas, use the ``Accept-Profile`` and ``Content-Profile`` headers.

If you don't specify a Profile header, the first schema in the list(``tenant1`` here) is selected as the default schema.

Only the selected schema gets added to the `search_path <https://www.postgresql.org/docs/current/ddl-schemas.html#DDL-SCHEMAS-PATH>`_ of every request.

.. note::

   These headers are based on the "Content Negotiation by Profile" spec: https://www.w3.org/TR/dx-prof-conneg

GET/HEAD
~~~~~~~~

For GET or HEAD, select the schema with ``Accept-Profile``.

.. tabs::

  .. code-tab:: http

     GET /items HTTP/1.1
     Accept-Profile: tenant2

  .. code-tab:: bash Curl

     curl "http://localhost:3000/items" \
       -H "Accept-Profile: tenant2"

Other methods
~~~~~~~~~~~~~

For POST, PATCH, PUT and DELETE, select the schema with ``Content-Profile``.

.. tabs::

  .. code-tab:: http

     POST /items HTTP/1.1
     Content-Profile: tenant2

     {...}

  .. code-tab:: bash Curl

     curl "http://localhost:3000/items" \
       -X POST -H "Content-Type: application/json" \
       -H "Content-Profile: tenant2" \
       -d '{...}'

You can also select the schema for :ref:`s_procs` and :ref:`open-api`.

Restricted schemas
~~~~~~~~~~~~~~~~~~

You can only switch to a schema included in :ref:`db-schemas`. Using another schema will result in an error:

.. tabs::

  .. code-tab:: http

     GET /items HTTP/1.1
     Accept-Profile: tenant3

     {...}

  .. code-tab:: bash Curl

     curl "http://localhost:3000/items" \
       -H "Accept-Profile: tenant3"

.. code-block::

  {
    "code":"PGRST106",
    "details":null,
    "hint":null,
    "message":"The schema must be one of the following: tenant1, tenant2"
  }

