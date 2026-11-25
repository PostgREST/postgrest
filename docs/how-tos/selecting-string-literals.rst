.. _selecting_string_literals:

Selecting String Literals
=========================

PostgREST query grammar does not have an equivalent of the below PostgreSQL query that selects a string literal:

.. code-block:: postgres

  SELECT username, 'Active User' as status
  FROM users
  WHERE is_active;

In PostgREST, the database schemas define the overall shape of the API and the query grammar is used to select and filter the result. Adding a string literal at runtime **contradicts** with PostgREST's philosophy, so the above cannot be supported.

The above can be implemented using `Generated Columns <https://www.postgresql.org/docs/current/ddl-generated-columns.html>`_, :ref:`computed_cols` or Database Views.

An example of how to implement above using *Generated Columns*:

.. code-block:: postgres

  CREATE TABLE users (
    username text,
    is_active boolean,
    status text GENERATED ALWAYS AS (CASE WHEN is_active Then 'Active User' ELSE 'Inactive User' END) VIRTUAL
  );

Now, query it in PostgREST with:

.. code-block:: bash

  curl "http://localhost:3000/users?select=username,status&is_active=is.true"

.. note::

  Virtual generated columns are not supported in PostgreSQL versions ``<= 17``. It is recommended to implement above using :ref:`computed_cols` for such versions.
