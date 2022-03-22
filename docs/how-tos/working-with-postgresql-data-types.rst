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

You can use other comparative filters and also `PostgreSQL special date/time input values <https://www.postgresql.org/docs/current/datatype-datetime.html#DATATYPE-DATETIME-SPECIAL-TABLE>`_. For instance, to get the reports that are due after today you would do:

.. tabs::

  .. code-tab:: http

    GET /reports?due_date=gt.today HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/reports?due_date=gt.today"

.. code-block:: json

  [
    {
      "id": 2,
      "due_date": "2022-02-27T06:00:00-05:00"
    }
  ]

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
------------------

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
