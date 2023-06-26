Resource Representation
#######################

PostgREST uses proper HTTP content negotiation (`RFC7231 <https://datatracker.ietf.org/doc/html/rfc7231#section-5.3>`_) to deliver a resource representation.
That is to say the same API endpoint can respond in different formats like JSON or CSV depending on the request.

.. _res_format:

Response Format
===============

Use the Accept request header to specify the acceptable format (or formats) for the response:

.. tabs::

  .. code-tab:: http

    GET /people HTTP/1.1
    Accept: application/json

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people" \
      -H "Accept: application/json"

For tables and views the current possibilities are:

* ``*/*``
* ``text/csv``
* ``application/json``
* ``application/openapi+json``
* ``application/geo+json``

The server will default to JSON for API endpoints and OpenAPI on the root.

.. _singular_plural:

Singular or Plural
------------------

By default PostgREST returns all JSON results in an array, even when there is only one item. For example, requesting :code:`/items?id=eq.1` returns

.. code:: json

  [
    { "id": 1 }
  ]

This can be inconvenient for client code. To return the first result as an object unenclosed by an array, specify :code:`vnd.pgrst.object` as part of the :code:`Accept` header

.. tabs::

  .. code-tab:: http

    GET /items?id=eq.1 HTTP/1.1
    Accept: application/vnd.pgrst.object+json

  .. code-tab:: bash Curl

    curl "http://localhost:3000/items?id=eq.1" \
      -H "Accept: application/vnd.pgrst.object+json"

This returns

.. code:: json

  { "id": 1 }

with a :code:`Content-Type: application/vnd.pgrst.object+json`.

When a singular response is requested but no entries are found, the server responds with an error message and 406 Not Acceptable status code rather than the usual empty array and 200 status:

.. code-block:: json

  {
    "message": "JSON object requested, multiple (or no) rows returned",
    "details": "Results contain 0 rows, application/vnd.pgrst.object+json requires 1 row",
    "hint": null,
    "code": "PGRST505"
  }

.. note::

  Many APIs distinguish plural and singular resources using a special nested URL convention e.g. `/stories` vs `/stories/1`. Why do we use `/stories?id=eq.1`? The answer is because a singular resource is (for us) a row determined by a primary key, and primary keys can be compound (meaning defined across more than one column). The more familiar nested urls consider only a degenerate case of simple and overwhelmingly numeric primary keys. These so-called artificial keys are often introduced automatically by Object Relational Mapping libraries.

  Admittedly PostgREST could detect when there is an equality condition holding on all columns constituting the primary key and automatically convert to singular. However this could lead to a surprising change of format that breaks unwary client code just by filtering on an extra column. Instead we allow manually specifying singular vs plural to decouple that choice from the URL format.

.. _scalar_return_formats:

Scalar Function Response Format
-------------------------------

In the special case of a :ref:`scalar_functions` there are three additional formats:

* ``application/octet-stream``
* ``text/plain``
* ``text/xml``

Example 1: If you want to return raw binary data from a :code:`bytea` column, you must specify :code:`application/octet-stream` as part of the :code:`Accept` header
and select a single column :code:`?select=bin_data`.

.. tabs::

  .. code-tab:: http

    GET /items?select=bin_data&id=eq.1 HTTP/1.1
    Accept: application/octet-stream

  .. code-tab:: bash Curl

    curl "http://localhost:3000/items?select=bin_data&id=eq.1" \
      -H "Accept: application/octet-stream"

Example 2: You can request XML output when having a scalar function that returns a type of ``text/xml``. You are not forced to use select for this case.

.. code-block:: postgres

  CREATE FUNCTION generate_xml_content(..) RETURNS xml ..

.. tabs::

  .. code-tab:: http

    POST /rpc/generate_xml_content HTTP/1.1
    Accept: text/xml

  .. code-tab:: bash Curl

    curl "http://localhost:3000/rpc/generate_xml_content" \
      -X POST -H "Accept: text/xml"

Example 3: If the stored procedure returns non-scalar values, you need to do a :code:`select` in the same way as for GET binary output.

.. code-block:: sql

  CREATE FUNCTION get_descriptions(..) RETURNS SETOF TABLE(id int, description text) ..

.. tabs::

  .. code-tab:: http

    POST /rpc/get_descriptions?select=description HTTP/1.1
    Accept: text/plain

  .. code-tab:: bash Curl

    curl "http://localhost:3000/rpc/get_descriptions?select=description" \
      -X POST -H "Accept: text/plain"

.. note::

  If more than one row would be returned the binary/plain-text/xml results will be concatenated with no delimiter.

.. _req_body:

Request Body
============

The server handles the following request body media types:

* ``application/json``
* ``application/x-www-form-urlencoded``
* ``text/csv``

For :ref:`tables_views` this works on ``POST``, ``PATCH`` and ``PUT`` methods. For :ref:`s_procs`, it works on ``POST`` methods.

For stored procedures there are three additional types:

* ``application/octet-stream``
* ``text/plain``
* ``text/xml``

See :ref:`s_proc_single_unnamed`.
