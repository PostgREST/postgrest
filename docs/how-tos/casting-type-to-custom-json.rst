Casting a type to a custom JSON object
======================================

:author: `steve-chavez <https://github.com/steve-chavez>`_

While using PostgREST you might have noticed that certain PostgreSQL types translate to JSON strings when you would
have expected a JSON object or array. For example, let's see the case of `range types <https://www.postgresql.org/docs/current/rangetypes.html>`_.

.. code-block:: postgres

   -- example taken from https://www.postgresql.org/docs/current/rangetypes.html#RANGETYPES-EXAMPLES
   create table reservations (
     room   int
   , during tsrange
   );

   insert into
      reservations
   values
       (1108, tsrange('2010-01-01 14:30', '2010-01-01 15:30'));

Here we have a column named **during** as a ``tsrange`` type, we would like to get it as JSON through PostgREST.

.. code-block:: bash

   curl "http://localhost:3000/reservations"

Result:

.. code-block:: json

   [
     {
      "room":1108,
      "during":"[\"2010-01-01 14:30:00\",\"2010-01-01 15:30:00\")"
     }
   ]

The **during** value is probably not the in the format you want. We get a JSON string because by default PostgreSQL casts
the type to JSON by using its ``text`` representation. We can change this representation to a custom JSON object by `creating a CAST <https://www.postgresql.org/docs/current/sql-createcast.html>`_ .

To do this, first we'll define the function that will do the conversion from ``tsrange`` to ``json``.

.. code-block:: postgres

   create or replace function tsrange_to_json(tsrange) returns json as $$
     select json_build_object(
       'lower', lower($1)
     , 'upper', upper($1)
     , 'lower_inc', lower_inc($1)
     , 'upper_inc', upper_inc($1)
     );
   $$ language sql;

Using this function we'll create the CAST.

.. code-block:: postgres

   create cast (tsrange as json) with function tsrange_to_json(tsrange) as assignment;

And we'll do the request and :ref:`cast the column <casting_columns>`.

.. code-block:: bash

   curl "http://localhost:3000/reservations?select=room,during::json"

The result now is:

.. code-block:: json

   [
     {
      "room":1108,
      "during":{
         "lower" : "2010-01-01T14:30:00",
         "upper" : "2010-01-01T15:30:00",
         "lower_inc" : true,
         "upper_inc" : false
      }
     }
   ]

You can use the same idea for creating custom casts for different types.

.. note::

   If you don't want to modify casts for built-in types, an option would be to `create a custom type <https://www.postgresql.org/docs/current/sql-createtype.html>`_
   for your own ``tsrange`` and add its own cast.

   .. code-block:: postgres

      create type mytsrange as range (subtype = timestamp, subtype_diff = tsrange_subdiff);

      -- define column types and casting function analoguously to the above example
      -- ...

      create cast (mytsrange as json) with function mytsrange_to_json(mytsrange) as assignment;
