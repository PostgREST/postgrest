.. _preferences:

Prefer Header
#############

PostgREST honors the Prefer HTTP header specified on `RFC 7240 <https://www.rfc-editor.org/rfc/rfc7240.html>`_. It allows clients to specify required and optional behaviors for their requests.

The following preferences are supported.

- ``Prefer: handling``. See :ref:`prefer_handling`.
- ``Prefer: timezone``. See :ref:`prefer_timezone`.
- ``Prefer: return``. See :ref:`prefer_return`.
- ``Prefer: count``. See :ref:`prefer_count`.
- ``Prefer: resolution``. See :ref:`prefer_resolution`.
- ``Prefer: missing``. See :ref:`bulk_insert_default`.

.. _prefer_handling:

Strict or Lenient Handling
==========================

The server ignores unrecognized or unfulfillable preferences by default. You can control this behavior with the ``handling`` preference. It can take two values: ``lenient`` (the default) or ``strict``.

``handling=strict`` will throw an error if you specify invalid preferences. For instance:

.. code-block:: http

  GET /projects HTTP/1.1
  Prefer: handling=strict, foo, bar

.. code-block:: http

  HTTP/1.1 400 Bad Request
  Content-Type: application/json; charset=utf-8

.. code-block:: json

  {
      "code": "PGRST122",
      "message": "Invalid preferences given with handling=strict",
      "details": "Invalid preferences: foo, bar",
      "hint": null
  }


``handling=lenient`` ignores invalid preferences.

.. code-block:: http

  GET /projects HTTP/1.1
  Prefer: handling=lenient, foo, bar

.. code-block:: http

  HTTP/1.1 200 OK
  Content-Type: application/json; charset=utf-8

.. _prefer_timezone:

Timezone
========

The ``timezone`` preference allows you to change the `PostgreSQL timezone <https://www.postgresql.org/docs/current/runtime-config-client.html#GUC-TIMEZONE>`_. It accepts all timezones in `pg_timezone_names <https://www.postgresql.org/docs/current/view-pg-timezone-names.html>`_.


.. code-block:: http

  GET /timestamps HTTP/1.1
  Prefer: timezone=America/Los_Angeles

.. code-block:: http

  HTTP/1.1 200 OK
  Content-Type: application/json; charset=utf-8
  Preference-Applied: timezone=America/Los_Angeles

.. code-block:: json

  [
    {"t":"2023-10-18T05:37:59.611-07:00"},
    {"t":"2023-10-18T07:37:59.611-07:00"},
    {"t":"2023-10-18T09:37:59.611-07:00"}
  ]

For an invalid timezone, PostgREST returns values with the default timezone (configured on ``postgresql.conf`` or as a setting on the :ref:`authenticator <roles>`).

.. code-block:: http

  GET /timestamps HTTP/1.1
  Prefer: timezone=Jupiter/Red_Spot

.. code-block:: http

  HTTP/1.1 200 OK
  Content-Type: application/json; charset=utf-8

.. code-block:: json

  [
    {"t":"2023-10-18T12:37:59.611+00:00"},
    {"t":"2023-10-18T14:37:59.611+00:00"},
    {"t":"2023-10-18T16:37:59.611+00:00"}
  ]

Note that there's no ``Preference-Applied`` in the response.

However, with ``handling=strict``, an invalid timezone preference will throw an :ref:`error <pgrst122>`.

.. code-block:: http

  GET /timestamps HTTP/1.1
  Prefer: handling=strict, timezone=Jupiter/Red_Spot

.. code-block:: http

  HTTP/1.1 400 Bad Request

.. _prefer_return:

Return Representation
=====================

The ``return`` preference can be used to obtain information about affected resource when it's :ref:`inserted <insert>`, :ref:`updated <update>` or :ref:`deleted <delete>`.
This helps avoid a subsequent GET request.

Minimal
-------

With ``Prefer: return=minimal``, no response body will be returned. This is the default mode for all write requests.

Headers Only
------------

If the table has a primary key, the response can contain a :code:`Location` header describing where to find the new object by including the header :code:`Prefer: return=headers-only` in the request. Make sure that the table is not write-only, otherwise constructing the :code:`Location` header will cause a permissions error.

.. tabs::

  .. code-tab:: http

    POST /projects HTTP/1.1
    Prefer: return=headers-only

    {"id":33, "name": "x"}

  .. code-tab:: bash Curl

    curl "http://localhost:3000/projects" \
      -X POST -H "Content-Type: application/json" -H "Prefer: return=headers-only" \
      -d '{"id":33, "name": "x"}'

.. code-block:: http

  HTTP/1.1 201 Created
  Location: /projects?id=eq.34
  Preference-Applied: return=headers-only

Full
----

On the other end of the spectrum you can get the full created object back in the response to your request by including the header :code:`Prefer: return=representation`. That way you won't have to make another HTTP call to discover properties that may have been filled in on the server side. You can also apply the standard :ref:`v_filter` to these results.

.. tabs::

  .. code-tab:: http

    POST /projects HTTP/1.1
    Content-Type: application/json; charset=utf-8
    Prefer: return=representation

    {"id":33, "name": "x"}

  .. code-tab:: bash Curl

    curl "http://localhost:3000/projects" \
      -X POST -H "Content-Type: application/json" -H "Prefer: return=representation" \
      -d '{"id":33, "name": "x"}'

.. code::

  HTTP/1.1 201 Created
  Preference-Applied: return=representation

  [
      {
          "id": 33,
          "name": "x"
      }
  ]
