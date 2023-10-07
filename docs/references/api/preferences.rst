.. _preferences:

Preferences
###########

PostgREST honors the Prefer HTTP header specified on `RFC 7240 <https://www.rfc-editor.org/rfc/rfc7240.html>`_. It allows clients to specify required and optional behaviors for their requests.

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
