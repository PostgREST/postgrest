.. _working_with_types:

Working with PostgreSQL data types
==================================

PostgREST makes use of PostgreSQL string representations to work with data types. Thanks to this, you can use special values, such as ``now`` for timestamps, ``yes`` for booleans or time values including the time zones. This page describes how you can take advantage of these string representations to perform operations on different PostgreSQL data types.

Timestamps
----------

You can use the **time zone** to filter or send data if needed. Let's use this table as an example:

.. code-block:: postgres

  create table reports (
    id int primary key
    , due_date timestamptz
  );

Suppose you are located in Sydney and want create a report with the date in the local time zone. Your request should look like this:

.. tabs::

  .. code-tab:: http

    POST /reports HTTP/1.1
    Content-Type: application/json

    [{ "id": 1, "due_date": "2022-02-24 11:10:15 Australia/Sydney" },
     { "id": 2, "due_date": "2022-02-27 22:00:00 Australia/Sydney" }]

  .. code-tab:: bash Curl

    curl "http://localhost:3000/reports" \
      -X POST -H "Content-Type: application/json" \
      -d '[{ "id": 1, "due_date": "2022-02-24 11:10:15 Australia/Sydney" },{ "id": 2, "due_date": "2022-02-27 22:00:00 Australia/Sydney" }]'

Someone located in Cairo can retrieve the data using their local time, too:

.. tabs::

  .. code-tab:: http

    GET /reports?due_date=eq.2022-02-24+02:10:15+Africa/Cairo HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/reports?due_date=eq.2022-02-24+02:10:15+Africa/Cairo"

.. code-block:: json

  [
    {
      "id": 1,
      "due_date": "2022-02-23T19:10:15-05:00"
    }
  ]

The response has the date in the time zone configured by the server: ``UTC -05:00``.

You can use other comparative filters and also all the `PostgreSQL special date/time input values <https://www.postgresql.org/docs/current/datatype-datetime.html#DATATYPE-DATETIME-SPECIAL-TABLE>`_ as illustrated in this example:

.. tabs::

  .. code-tab:: http

    GET /reports?or=(and(due_date.gte.today,due_date.lte.tomorrow),and(due_date.gt.-infinity,due_date.lte.epoch)) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/reports?or=(and(due_date.gte.today,due_date.lte.tomorrow),and(due_date.gt.-infinity,due_date.lte.epoch))"

.. code-block:: json

  [
    {
      "id": 2,
      "due_date": "2022-02-27T06:00:00-05:00"
    }
  ]

JSON
----

To work with a ``json`` type column, you can handle the value as a JSON object. For instance, let's use this table:

.. code-block:: postgres

  create table products (
    id int primary key,
    name text unique,
    extra_info json
  );

Now, you can insert a new product using a JSON object for the ``extra_info`` column:

.. tabs::

  .. code-tab:: http

    POST /products HTTP/1.1
    Content-Type: application/json

    {
      "id": 1,
      "name": "Canned fish",
      "extra_info": {
        "expiry_date": "2025-12-31",
        "exportable": true
      }
    }

  .. code-tab:: bash Curl

    curl "http://localhost:3000/products" \
      -X POST -H "Content-Type: application/json" \
      -d @- << EOF
      {
        "id": 1,
        "name": "Canned fish",
        "extra_info": {
          "expiry_date": "2025-12-31",
          "exportable": true
        }
      }
    EOF

To query and filter the data see :ref:`json_columns` for a complete reference.

Arrays
------

To handle `array types <https://www.postgresql.org/docs/current/arrays.html>`_ you can use string representation or JSON array format. For instance, let's create the following table:

.. code-block:: postgres

  create table movies (
    id int primary key,
    title text not null,
    tags text[],
    performance_times time[]
  );

To insert a new value you can use string representation.

.. tabs::

  .. code-tab:: http

    POST /movies HTTP/1.1
    Content-Type: application/json

    {
      "id": 1,
      "title": "Paddington",
      "tags": "{family,comedy,not streamable}",
      "performance_times": "{12:40,15:00,20:00}"
    }

  .. code-tab:: bash Curl

    curl "http://localhost:3000/movies" \
      -X POST -H "Content-Type: application/json" \
      -d @- << EOF
      {
        "id": 1,
        "title": "Paddington",
        "tags": "{family,comedy,not streamable}",
        "performance_times": "{12:40,15:00,20:00}"
      }
    EOF

Or you could send the data using a JSON array format. The following request sends the same data as the example above:

.. tabs::

  .. code-tab:: http

    POST /movies HTTP/1.1
    Content-Type: application/json

    {
      "id": 1,
      "title": "Paddington",
      "tags": ["family", "comedy", "not streamable"],
      "performance_times": ["12:40", "15:00", "20:00"]
    }

  .. code-tab:: bash Curl

    curl "http://localhost:3000/movies" \
      -X POST -H "Content-Type: application/json" \
      -d @- << EOF
      {
        "id": 1,
        "title": "Paddington",
        "tags": ["family", "comedy", "not streamable"],
        "performance_times": ["12:40", "15:00", "20:00"]
      }
    EOF

To query the data you can use the arrow operators. See :ref:`composite_array_columns`.

Multidimensional Arrays
~~~~~~~~~~~~~~~~~~~~~~~

Handling multidimensional arrays is no different than handling one-dimensional ones: both the string representation and the JSON array format are allowed. For example, let's add a new column to the table:

.. code-block:: postgres

  -- The column stores the cinema, floor and auditorium numbers in that order
  alter table movies
  add column cinema_floor_auditorium int[][][];

Now, let's update the row we inserted before using JSON array format:

.. tabs::

  .. code-tab:: http

    PATCH /movies?id=eq.1 HTTP/1.1
    Content-Type: application/json

    {
      "cinema_floor_auditorium": [ [ [1,2], [6,7] ], [ [3,5], [8,9] ] ]
    }

  .. code-tab:: bash Curl

    curl "http://localhost:3000/movies?id=eq.1" \
      -X PATCH -H "Content-Type: application/json" \
      -d @- << EOF
      {
        "cinema_floor_auditorium": [ [ [1,2], [6,7] ], [ [3,5], [8,9] ] ]
      }
    EOF

Now, for example, to query the auditoriums that are located in the first cinema (position 0 in the array) and on the second floor (position 1 in the next inner array), we can use the arrow operators this way:

.. tabs::

  .. code-tab:: http

    GET /movies?select=title,auditorium:cinema_floor_auditorium->0->1&id=eq.1 HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/movies?select=title,auditorium:cinema_floor_auditorium->0->1&id=eq.1"

.. code-block:: json

  [
    {
      "title": "Paddington",
      "auditorium": [6,7]
    }
  ]

Composite Types
---------------

With PostgREST, you have two options to handle `composite type columns <https://www.postgresql.org/docs/current/rowtypes.html>`_. On one hand you can use string representation and on the other you can handle it as you would a JSON column. Let's create a type and a table for this example:

.. code-block:: postgres

  create type dimension as (
    length decimal(6,2),
    width decimal (6,2),
    height decimal (6,2),
    unit text
  );

  create table products (
    id int primary key,
    size dimension
  );

  insert into products (id, size)
  values (1, '(5.0,5.0,10.0,"cm")');

Now, you could insert values using string representation as seen in the example above.

.. tabs::

  .. code-tab:: http

    POST /products HTTP/1.1
    Content-Type: application/json

    { "id": 2, "size": "(0.7,0.5,1.8,\"m\")" }

  .. code-tab:: bash Curl

    curl "http://localhost:3000/products" \
      -X POST -H "Content-Type: application/json" \
      -d @- << EOF
      { "id": 2, "size": "(0.7,0.5,1.8,\"m\")" }
    EOF

Or, you could insert the data in JSON format. The following request is equivalent to the previous one:

.. tabs::

  .. code-tab:: http

    POST /products HTTP/1.1
    Content-Type: application/json

      {
        "id": 2,
        "size": {
          "length": 0.7,
          "width": 0.5,
          "height": 1.8,
          "unit": "m"
        }
      }

  .. code-tab:: bash Curl

    curl "http://localhost:3000/products" \
      -X POST -H "Content-Type: application/json" \
      -d @- << EOF
      {
        "id": 2,
        "size": {
          "length": 0.7,
          "width": 0.5,
          "height": 1.8,
          "unit": "m"
        }
      }
    EOF

You can also query data using the arrow operators. See :ref:`composite_array_columns`.

Ranges
------

To illustrate how to work with `ranges <https://www.postgresql.org/docs/current/rangetypes.html>`_, let's use the following table as an example:

.. code-block:: postgres

   create table events (
     id int primary key,
     name text unique,
     duration tsrange
   );

Now, to insert a new event, specify the ``duration`` value as a string representation of the ``tsrange`` type, for example:

.. tabs::

  .. code-tab:: http

    POST /events HTTP/1.1
    Content-Type: application/json

    {
      "id": 1,
      "name": "New Year's Party",
      "duration": "['2022-12-31 11:00','2023-01-01 06:00']"
    }

  .. code-tab:: bash Curl

    curl "http://localhost:3000/events" \
      -X POST -H "Content-Type: application/json" \
      -d @- << EOF
      {
        "id": 1,
        "name": "New Year's Party",
        "duration": "['2022-12-31 11:00','2023-01-01 06:00']"
      }
    EOF

You can use range :ref:`operators <operators>` to filter the data. But what if you need get the events for the New Year 2023? Doing this filter ``events?duration=cs.2023-01-01`` will return an error because PostgreSQL needs an explicit cast to timestamp of the string value. A workaround would be to use a range starting and ending in the same date, like this:

.. tabs::

  .. code-tab:: http

    GET /events?duration=cs.[2023-01-01,2023-01-01] HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/events?duration=cs.\[2023-01-01,2023-01-01\]"

.. code-block:: json

  [
    {
      "id": 1,
      "name": "New Year's Party",
      "duration": "[\"2022-12-31 11:00:00\",\"2023-01-01 06:00:00\"]"
    }
  ]

.. _casting_range_to_json:

Casting a Range to a JSON Object
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As you may have noticed, the ``tsrange`` value is returned as a string literal. To return it as a JSON value, first you need to create a function that will do the conversion from a ``tsrange`` type:

.. code-block:: postgres

   create or replace function tsrange_to_json(tsrange) returns json as $$
     select json_build_object(
       'lower', lower($1)
     , 'upper', upper($1)
     , 'lower_inc', lower_inc($1)
     , 'upper_inc', upper_inc($1)
     );
   $$ language sql;

Then, create the cast using this function:

.. code-block:: postgres

   create cast (tsrange as json) with function tsrange_to_json(tsrange) as assignment;

Finally, do the request :ref:`casting the range column <casting_columns>`:

.. tabs::

  .. code-tab:: http

    GET /events?select=id,name,duration::json HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/events?select=id,name,duration::json"

.. code-block:: json

  [
    {
      "id": 1,
      "name": "New Year's Party",
      "duration": {
        "lower": "2022-12-31T11:00:00",
        "upper": "2023-01-01T06:00:00",
        "lower_inc": true,
        "upper_inc": true
      }
    }
  ]

.. note::

   If you don't want to modify casts for built-in types, an option would be to `create a custom type <https://www.postgresql.org/docs/current/sql-createtype.html>`_
   for your own ``tsrange`` and add its own cast.

   .. code-block:: postgres

      create type mytsrange as range (subtype = timestamp, subtype_diff = tsrange_subdiff);

      -- define column types and casting function analogously to the above example
      -- ...

      create cast (mytsrange as json) with function mytsrange_to_json(mytsrange) as assignment;

hstore
------

You can work with data types belonging to additional supplied modules such as `hstore <https://www.postgresql.org/docs/current/hstore.html>`_. Let's use the following table:

.. code-block:: postgres

  -- Activate the hstore module in the current database
  create extension if not exists hstore;

  create table countries (
    id int primary key,
    name hstore unique
  );

The ``name`` column will have the name of the country in different formats. You can insert values using the string representation for that data type, for instance:

.. tabs::

  .. code-tab:: http

    POST /countries HTTP/1.1
    Content-Type: application/json

    [
      { "id": 1, "name": "common => Egypt, official => \"Arab Republic of Egypt\", native => مصر" },
      { "id": 2, "name": "common => Germany, official => \"Federal Republic of Germany\", native => Deutschland" }
    ]

  .. code-tab:: bash Curl

    curl "http://localhost:3000/countries" \
      -X POST -H "Content-Type: application/json" \
      -d @- << EOF
      [
        { "id": 1, "name": "common => Egypt, official => \"Arab Republic of Egypt\", native => مصر" },
        { "id": 2, "name": "common => Germany, official => \"Federal Republic of Germany\", native => Deutschland" }
      ]
    EOF

Notice that the use of ``"`` in the value of the ``name`` column needs to be escaped using a backslash ``\``.

You can also query and filter the value of a ``hstore`` column using the arrow operators, as you would do for a :ref:`JSON column<json_columns>`. For example, if you want to get the native name of Egypt, the query would be:

.. tabs::

  .. code-tab:: http

    GET /countries?select=name->>native&name->>common=like.Egypt HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/countries?select=name->>native&name->>common=like.Egypt"

.. code-block:: json

  [{ "native": "مصر" }]

PostGIS
-------

You can use the string representation for `PostGIS <https://postgis.net/>`_ data types such as ``geometry`` or ``geography``. As an example, let's create a table using the ``geometry`` type (you need to `install PostGIS <https://postgis.net/install/>`_ first).

.. code-block:: postgres

  -- Activate the postgis module in the current database
  create extension if not exists postgis;

  create table coverage (
    id int primary key,
    name text unique,
    area geometry
  );

Say you want to add areas in polygon format. The request using string representation would look like:

.. tabs::

  .. code-tab:: http

    POST /coverage HTTP/1.1
    Content-Type: application/json

    [
      { "id": 1, "name": "small", "area": "SRID=4326;POLYGON((0 0, 1 0, 1 1, 0 1, 0 0))" },
      { "id": 2, "name": "big", "area": "SRID=4326;POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))" }
    ]

  .. code-tab:: bash Curl

    curl "http://localhost:3000/coverage" \
      -X POST -H "Content-Type: application/json" \
      -d @- << EOF
      [
        { "id": 1, "name": "small", "area": "SRID=4326;POLYGON((0 0, 1 0, 1 1, 0 1, 0 0))" },
        { "id": 2, "name": "big", "area": "SRID=4326;POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))" }
      ]
    EOF

Now, when you request the information, PostgREST will automatically cast the ``area`` column to ``JSON`` format. Although this output is useful, you will want to use the PostGIS functions to have more control on filters or casts. For these cases, creating a ``view`` is your best option. For example, let's use some of the functions to get the data in `GeoJSON format <https://geojson.org/>`_ and to calculate the area in square units:

.. code-block:: postgres

  create or replace view coverage_geo as
  select name,
         -- Get the Geometry Object
         st_AsGeoJSON(c.area)::json as geo_geometry,
         -- Get the Feature Object
         st_AsGeoJSON(c.*)::json as geo_feature,
         -- Calculate the area in square units
         st_area(c.area) as square_units
  from coverage c;

  -- Create another view for the FeatureCollection Object
  -- for the sake of making the examples clearer
  create or replace view coverage_geo_collection as
    select
      json_build_object(
          'type', 'FeatureCollection',
          'features', json_agg(st_AsGeoJSON(c.*)::json)
        )
        as geo_feature_collection
    from coverage c;

Now the query will return the information as you expected:

.. tabs::

  .. code-tab:: http

    GET /coverage_geo?name=eq.big HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/coverage_geo?name=eq.big"

.. code-block:: json

  [
    {
      "name": "big",
      "geo_geometry": {
        "type": "Polygon",
        "coordinates": [
          [[0,0],[10,0],[10,10],[0,10],[0,0]]
        ]
      },
      "geo_feature": {
        "type": "Feature",
        "geometry": {
          "type": "Polygon",
          "coordinates": [
            [[0,0],[10,0],[10,10],[0,10],[0,0]]
          ]
        },
        "properties": {
          "id": 2,
          "name": "big"
        }
      },
      "square_units": 100
    }
  ]

And for the Feature Collection format:

.. tabs::

  .. code-tab:: http

    GET /coverage_geo_collection HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/coverage_geo_collection"

.. code-block:: json

  [
    {
      "geo_feature_collection": {
        "type": "FeatureCollection",
        "features": [
          {
            "type": "Feature",
            "geometry": {
                "type": "Polygon",
                "coordinates": [
                  [[0,0],[1,0],[1,1],[0,1],[0,0]]
                ]
            },
            "properties": {
              "id": 1,
              "name": "small"
            }
          },
          {
            "type": "Feature",
            "geometry": {
                "type": "Polygon",
                "coordinates": [
                  [[0,0],[10,0],[10,10],[0,10],[0,0]]
                ]
            },
            "properties": {
              "id": 2,
              "name": "big"
            }
          }
        ]
      }
    }
  ]
