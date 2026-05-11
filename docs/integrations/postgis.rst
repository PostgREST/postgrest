.. _ww_postgis:

PostGIS
=======

You can use the string representation for `PostGIS <https://postgis.net/>`_ data types such as ``geometry`` or ``geography`` (you need to `install PostGIS <https://postgis.net/documentation/getting_started/>`_ first).

.. code-block:: postgres

  -- Activate the postgis module in the current database
  create extension if not exists postgis;

  create table coverage (
    id int primary key,
    name text unique,
    area geometry
  );

To add areas in polygon format, you can use string representation:

.. code-block:: bash

  curl "http://localhost:3000/coverage" \
    -X POST -H "Content-Type: application/json" \
    -d @- << EOF
    [
      { "id": 1, "name": "small", "area": "SRID=4326;POLYGON((0 0, 1 0, 1 1, 0 1, 0 0))" },
      { "id": 2, "name": "big", "area": "SRID=4326;POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))" }
    ]
  EOF

Now, when you request the information, PostgREST will automatically cast the ``area`` column into a ``Polygon`` geometry type. Although this is useful, you may need the whole output to be in `GeoJSON <https://geojson.org/>`_ format out of the box, which can be done by including the ``Accept: application/geo+json`` in the request. This will work for PostGIS versions 3.0.0 and up and will return the output as a `FeatureCollection Object <https://www.rfc-editor.org/rfc/rfc7946#section-3.3>`_:

.. code-block:: bash

  curl "http://localhost:3000/coverage" \
    -H "Accept: application/geo+json"

.. code-block:: json

  {
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

If you need to add an extra property, like the area in square units by using ``st_area(area)``, you could add a generated column to the table and it will appear in the ``properties`` key of each ``Feature``.

.. code-block:: postgres

  alter table coverage
    add square_units double precision generated always as ( st_area(area) ) stored;

In the case that you are using older PostGIS versions, then creating a function is your best option:

.. code-block:: postgres

  create or replace function coverage_geo_collection() returns json as $$
    select
      json_build_object(
        'type', 'FeatureCollection',
        'features', json_agg(
          json_build_object(
            'type', 'Feature',
            'geometry', st_AsGeoJSON(c.area)::json,
            'properties', json_build_object('id', c.id, 'name', c.name)
          )
        )
      )
    from coverage c;
  $$ language sql;

Now this query will return the same results:

.. code-block:: bash

  curl "http://localhost:3000/rpc/coverage_geo_collection"

.. code-block:: json

  {
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
