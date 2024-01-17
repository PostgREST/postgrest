.. _s_procs:

Stored Procedures
=================

*"A single resource can be the equivalent of a database stored procedure, with the power to abstract state changes over any number of storage items"* -- `Roy T. Fielding <http://roy.gbiv.com/untangled/2008/rest-apis-must-be-hypertext-driven#comment-743>`_

Procedures can perform any operations allowed by PostgreSQL (read data, modify data, :ref:`raise errors <raise_error>`, and even DDL operations). Every stored procedure in the :ref:`exposed schema <schemas>` and accessible by the :ref:`active database role <roles>` is executable under the :code:`/rpc` prefix.

If they return table types, Stored Procedures can:

- Use all the same :ref:`read filters as Tables and Views <read>` (horizontal/vertical filtering, counts, limits, etc.).
- Use :ref:`Resource Embedding <s_proc_embed>`, if the returned table type has relationships to other tables.

.. note::

  Why the ``/rpc`` prefix? PostgreSQL allows a table or view to have the same name as a function. The prefix allows us to avoid routes collisions.

Calling with POST
-----------------

To supply arguments in an API call, include a JSON object in the request payload. Each key/value of the object will become an argument.

For instance, assume we have created this function in the database.

.. code-block:: plpgsql

  CREATE FUNCTION add_them(a integer, b integer)
  RETURNS integer AS $$
   SELECT a + b;
  $$ LANGUAGE SQL IMMUTABLE;

.. important::

  Whenever you create or change a function you must refresh PostgREST's schema cache. See the section :ref:`schema_reloading`.

The client can call it by posting an object like

.. code-block:: bash

  curl "http://localhost:3000/rpc/add_them" \
    -X POST -H "Content-Type: application/json" \
    -d '{ "a": 1, "b": 2 }'

.. code-block:: json

  3

.. note::

  PostgreSQL converts identifier names to lowercase unless you quote them like:

  .. code-block:: postgres

    CREATE FUNCTION "someFunc"("someParam" text) ...

Calling with GET
----------------

If the function doesn't modify the database, it will also run under the GET method (see :ref:`access_mode`).

.. code-block:: bash

  curl "http://localhost:3000/rpc/add_them?a=1&b=2"

The function parameter names match the JSON object keys in the POST case, for the GET case they match the query parameters ``?a=1&b=2``.

.. _s_proc_single_json:

Functions with a single unnamed JSON parameter
----------------------------------------------

If you want the JSON request body to be sent as a single argument, you can create a function with a single unnamed ``json`` or ``jsonb`` parameter.
For this the ``Content-Type: application/json`` header must be included in the request.

.. code-block:: plpgsql

  CREATE FUNCTION mult_them(json) RETURNS int AS $$
    SELECT ($1->>'x')::int * ($1->>'y')::int
  $$ LANGUAGE SQL;

.. code-block:: bash

  curl "http://localhost:3000/rpc/mult_them" \
    -X POST -H "Content-Type: application/json" \
    -d '{ "x": 4, "y": 2 }'

.. code-block:: json

  8

.. note::

 If an overloaded function has a single ``json`` or ``jsonb`` unnamed parameter, PostgREST will call this function as a fallback provided that no other overloaded function is found with the parameters sent in the POST request.

.. warning::

 Sending the JSON request body as a single argument is also possible with :ref:`Prefer: params=single-object <prefer_params>` but this method is **deprecated**.

.. _s_proc_single_unnamed:

Functions with a single unnamed parameter
-----------------------------------------

You can make a POST request to a function with a single unnamed parameter to send raw ``bytea``, ``text`` or ``xml`` data.

To send raw XML, the parameter type must be ``xml`` and the header ``Content-Type: text/xml`` must be included in the request.

To send raw binary, the parameter type must be ``bytea`` and the header ``Content-Type: application/octet-stream`` must be included in the request.

.. code-block:: plpgsql

  CREATE TABLE files(blob bytea);

  CREATE FUNCTION upload_binary(bytea) RETURNS void AS $$
    INSERT INTO files(blob) VALUES ($1);
  $$ LANGUAGE SQL;

.. code-block:: bash

  curl "http://localhost:3000/rpc/upload_binary" \
    -X POST -H "Content-Type: application/octet-stream" \
    --data-binary "@file_name.ext"

.. code-block:: http

  HTTP/1.1 200 OK

  [ ... ]

To send raw text, the parameter type must be ``text`` and the header ``Content-Type: text/plain`` must be included in the request.

.. _s_procs_array:

Functions with array parameters
-------------------------------

You can call a function that takes an array parameter:

.. code-block:: postgres

   create function plus_one(arr int[]) returns int[] as $$
      SELECT array_agg(n + 1) FROM unnest($1) AS n;
   $$ language sql;

.. code-block:: bash

   curl "http://localhost:3000/rpc/plus_one" \
     -X POST -H "Content-Type: application/json" \
     -d '{"arr": [1,2,3,4]}'

.. code-block:: json

   [2,3,4,5]

For calling the function with GET, you can pass the array as an `array literal <https://www.postgresql.org/docs/current/arrays.html#ARRAYS-INPUT>`_,
as in ``{1,2,3,4}``. Note that the curly brackets have to be urlencoded(``{`` is ``%7B`` and ``}`` is ``%7D``).

.. code-block:: bash

  curl "http://localhost:3000/rpc/plus_one?arr=%7B1,2,3,4%7D'"

.. note::

   For versions prior to PostgreSQL 10, to pass a PostgreSQL native array on a POST payload, you need to quote it and use an array literal:

   .. code-block:: bash

     curl "http://localhost:3000/rpc/plus_one" \
       -X POST -H "Content-Type: application/json" \
       -d '{ "arr": "{1,2,3,4}" }'

   In these versions we recommend using function parameters of type JSON to accept arrays from the client.

.. _s_procs_variadic:

Variadic functions
------------------

You can call a variadic function by passing a JSON array in a POST request:

.. code-block:: postgres

   create function plus_one(variadic v int[]) returns int[] as $$
      SELECT array_agg(n + 1) FROM unnest($1) AS n;
   $$ language sql;

.. code-block:: bash

  curl "http://localhost:3000/rpc/plus_one" \
    -X POST -H "Content-Type: application/json" \
    -d '{"v": [1,2,3,4]}'

.. code-block:: json

   [2,3,4,5]

In a GET request, you can repeat the same parameter name:

.. code-block:: bash

  curl "http://localhost:3000/rpc/plus_one?v=1&v=2&v=3&v=4"

Repeating also works in POST requests with ``Content-Type: application/x-www-form-urlencoded``:

.. code-block:: bash

  curl "http://localhost:3000/rpc/plus_one" \
    -X POST -H "Content-Type: application/x-www-form-urlencoded" \
    -d 'v=1&v=2&v=3&v=4'

.. _table_functions:

Table-Valued Functions
----------------------

A function that returns a table type can be filtered using the same filters as :ref:`tables and views <tables_views>`. They can also use :ref:`Resource Embedding <s_proc_embed>`.

.. code-block:: postgres

  CREATE FUNCTION best_films_2017() RETURNS SETOF films ..

.. code-block:: bash

  curl "http://localhost:3000/rpc/best_films_2017?select=title,director:directors(*)"

.. code-block:: bash

  curl "http://localhost:3000/rpc/best_films_2017?rating=gt.8&order=title.desc"

.. _function_inlining:

Function Inlining
~~~~~~~~~~~~~~~~~

A function that follows the `rules for inlining <https://wiki.postgresql.org/wiki/Inlining_of_SQL_functions#Inlining_conditions_for_table_functions>`_ will also inline :ref:`filters <h_filter>`, :ref:`order <ordering>` and :ref:`limits <limits>`.

For example, for the following function:

.. code-block:: postgres

  create function getallprojects() returns setof projects
  language sql stable
  as $$
    select * from projects;
  $$;

Let's get its :ref:`explain_plan` when calling it with filters applied:

.. code-block:: bash

  curl "http://localhost:3000/rpc/getallprojects?id=eq.1" \
    -H "Accept: application/vnd.pgrst.plan"

.. code-block:: psql

  Aggregate  (cost=8.18..8.20 rows=1 width=112)
    ->  Index Scan using projects_pkey on projects  (cost=0.15..8.17 rows=1 width=40)
          Index Cond: (id = 1)

Notice there's no "Function Scan" node in the plan, which tells us it has been inlined.

.. _scalar_functions:

Scalar functions
----------------

PostgREST will detect if the function is scalar or table-valued and will shape the response format accordingly:

.. code-block:: bash

  curl "http://localhost:3000/rpc/add_them?a=1&b=2"

.. code-block:: json

  3

.. code-block:: bash

  curl "http://localhost:3000/rpc/best_films_2017"

.. code-block:: json

  [
    { "title": "Okja", "rating": 7.4},
    { "title": "Call me by your name", "rating": 8},
    { "title": "Blade Runner 2049", "rating": 8.1}
  ]

To manually choose a return format such as binary, see :ref:`custom_media`.

.. _untyped_functions:

Untyped functions
-----------------

Functions that return ``record`` or ``SETOF record`` are supported:

.. code-block:: postgres

  create function projects_setof_record() returns setof record as $$
    select * from projects;
  $$ language sql;

.. code-block:: bash

  curl "http://localhost:3000/rpc/projects_setof_record"

.. code-block:: json

  [{"id":1,"name":"Windows 7","client_id":1},
   {"id":2,"name":"Windows 10","client_id":1},
   {"id":3,"name":"IOS","client_id":2}]

However note that they will fail when trying to use :ref:`v_filter` and :ref:`h_filter` on them.

So while they can be used for quick tests, it's recommended to always choose a strict return type for the function.

Overloaded functions
--------------------

You can call overloaded functions with different number of arguments.

.. code-block:: postgres

  CREATE FUNCTION rental_duration(customer_id integer) ..

  CREATE FUNCTION rental_duration(customer_id integer, from_date date) ..

.. code-block:: bash

  curl "http://localhost:3000/rpc/rental_duration?customer_id=232"

.. code-block:: bash

  curl "http://localhost:3000/rpc/rental_duration?customer_id=232&from_date=2018-07-01"

.. important::

  Overloaded functions with the same argument names but different types are not supported.
