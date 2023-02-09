.. _error_source:

Error Source
============

For the most part, error messages will come directly from the database with the same `structure that PostgreSQL uses <https://www.postgresql.org/docs/current/error-style-guide.html>`_. PostgREST will convert the ``MESSAGE``, ``DETAIL``, ``HINT`` and ``ERRCODE`` from the PostgreSQL error to JSON format and add an HTTP status code to the response (see :ref:`status_codes`). For instance, this is the error you will get when querying a nonexistent table:

.. code-block:: http

  GET /nonexistent_table?id=eq.1 HTTP/1.1

.. code-block:: http

  HTTP/1.1 404 Not Found
  Content-Type: application/json; charset=utf-8

.. code-block:: json

  {
    "hint": null,
    "details": null,
    "code": "42P01",
    "message": "relation \"api.nonexistent_table\" does not exist"
  }

However, some errors do come from PostgREST itself (such as those related to the :ref:`schema_cache`). These have the same structure as the PostgreSQL errors but are differentiated by the ``PGRST`` prefix in the ``code`` field (see :ref:`pgrst_errors`). For instance, when querying a function that does not exist, the error will be:

.. code-block:: http

  POST /rpc/nonexistent_function HTTP/1.1

.. code-block:: http

  HTTP/1.1 404 Not Found
  Content-Type: application/json; charset=utf-8

.. code-block:: json

  {
    "hint": "If a new function was created in the database with this name and parameters, try reloading the schema cache.",
    "details": null
    "code": "PGRST202",
    "message": "Could not find the api.nonexistent_function() function in the schema cache"
  }

.. _status_codes:

HTTP Status Codes
=================

PostgREST translates `PostgreSQL error codes <https://www.postgresql.org/docs/current/errcodes-appendix.html>`_ into HTTP status as follows:

+--------------------------+-------------------------+---------------------------------+
| PostgreSQL error code(s) | HTTP status             | Error description               |
+==========================+=========================+=================================+
| 08*                      | 503                     | pg connection err               |
+--------------------------+-------------------------+---------------------------------+
| 09*                      | 500                     | triggered action exception      |
+--------------------------+-------------------------+---------------------------------+
| 0L*                      | 403                     | invalid grantor                 |
+--------------------------+-------------------------+---------------------------------+
| 0P*                      | 403                     | invalid role specification      |
+--------------------------+-------------------------+---------------------------------+
| 23503                    | 409                     | foreign key violation           |
+--------------------------+-------------------------+---------------------------------+
| 23505                    | 409                     | uniqueness violation            |
+--------------------------+-------------------------+---------------------------------+
| 25006                    | 405                     | read only sql transaction       |
+--------------------------+-------------------------+---------------------------------+
| 25*                      | 500                     | invalid transaction state       |
+--------------------------+-------------------------+---------------------------------+
| 28*                      | 403                     | invalid auth specification      |
+--------------------------+-------------------------+---------------------------------+
| 2D*                      | 500                     | invalid transaction termination |
+--------------------------+-------------------------+---------------------------------+
| 38*                      | 500                     | external routine exception      |
+--------------------------+-------------------------+---------------------------------+
| 39*                      | 500                     | external routine invocation     |
+--------------------------+-------------------------+---------------------------------+
| 3B*                      | 500                     | savepoint exception             |
+--------------------------+-------------------------+---------------------------------+
| 40*                      | 500                     | transaction rollback            |
+--------------------------+-------------------------+---------------------------------+
| 53*                      | 503                     | insufficient resources          |
+--------------------------+-------------------------+---------------------------------+
| 54*                      | 413                     | too complex                     |
+--------------------------+-------------------------+---------------------------------+
| 55*                      | 500                     | obj not in prerequisite state   |
+--------------------------+-------------------------+---------------------------------+
| 57*                      | 500                     | operator intervention           |
+--------------------------+-------------------------+---------------------------------+
| 58*                      | 500                     | system error                    |
+--------------------------+-------------------------+---------------------------------+
| F0*                      | 500                     | config file error               |
+--------------------------+-------------------------+---------------------------------+
| HV*                      | 500                     | foreign data wrapper error      |
+--------------------------+-------------------------+---------------------------------+
| P0001                    | 400                     | default code for "raise"        |
+--------------------------+-------------------------+---------------------------------+
| P0*                      | 500                     | PL/pgSQL error                  |
+--------------------------+-------------------------+---------------------------------+
| XX*                      | 500                     | internal error                  |
+--------------------------+-------------------------+---------------------------------+
| 42883                    | 404                     | undefined function              |
+--------------------------+-------------------------+---------------------------------+
| 42P01                    | 404                     | undefined table                 |
+--------------------------+-------------------------+---------------------------------+
| 42501                    | | if authenticated 403, | insufficient privileges         |
|                          | | else 401              |                                 |
+--------------------------+-------------------------+---------------------------------+
| other                    | 400                     |                                 |
+--------------------------+-------------------------+---------------------------------+

.. _pgrst_errors:

PostgREST Error Codes
=====================

PostgREST error codes have the form ``PGRSTgxx``, where ``PGRST`` is the prefix that differentiates the error from a PostgreSQL error, ``g`` is the group where the error belongs and ``xx`` is the number that identifies the error in the group.

.. _pgrst0**:

Group 0 - Connection
--------------------

Related to the connection with the database.

+---------------+-------------+-------------------------------------------------------------+
| Code          | HTTP status | Description                                                 |
+===============+=============+=============================================================+
| .. _pgrst000: | 503         | Could not connect with the database due to an incorrect     |
|               |             | :ref:`db-uri` or due to the PostgreSQL service not running. |
| PGRST000      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst001: | 503         | Could not connect with the database due to an internal      |
|               |             | error.                                                      |
| PGRST001      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst002: | 503         | Could not connect with the database when building the       |
|               |             | :ref:`schema_cache` due to the PostgreSQL service not       |
| PGRST002      |             | running.                                                    |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst003: | 503         | The request time exceeded the timeout specified in          |
|               |             | :ref:`db-pool-acquisition-timeout`.                         |
| PGRST003      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+

.. _pgrst1**:

Group 1 - Api Request
---------------------

Related to the HTTP request elements.

+---------------+-------------+-------------------------------------------------------------+
| Code          | HTTP status | Description                                                 |
+===============+=============+=============================================================+
| .. _pgrst100: | 400         | Parsing error in the query string parameter.                |
|               |             | See :ref:`h_filter`, :ref:`operators` and :ref:`ordering`.  |
| PGRST100      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst101: | 405         | For :ref:`functions <s_procs>`, only ``GET`` and ``POST``   |
|               |             | verbs are allowed. Any other verb will throw this error.    |
| PGRST101      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst102: | 400         | An invalid request body was sent(e.g. an empty body or      |
|               |             | malformed JSON).                                            |
| PGRST102      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst103: | 416         | An invalid range was specified for :ref:`limits`.           |
|               |             |                                                             |
| PGRST103      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst105: | 405         | An invalid :ref:`PUT <upsert_put>` request was done         |
|               |             |                                                             |
| PGRST105      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst106: | 406         | The schema specified when                                   |
|               |             | :ref:`switching schemas <multiple-schemas>` is not present  |
| PGRST106      |             | in the :ref:`db-schemas` configuration variable.            |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst107: | 415         | The ``Content-Type`` sent in the request is invalid.        |
|               |             |                                                             |
| PGRST107      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst108: | 400         | The filter is applied to a embedded resource that is not    |
|               |             | specified in the ``select`` part of the query string.       |
| PGRST108      |             | See :ref:`embed_filters`.                                   |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst109: | 400         | Restricting a Deletion or an Update using limits must       |
|               |             | include the ordering of a unique column.                    |
| PGRST109      |             | See :ref:`limited_update_delete`.                           |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst110: | 400         | When restricting a Deletion or an Update using limits       |
|               |             | modifies more rows than the maximum specified in the limit. |
| PGRST110      |             | See :ref:`limited_update_delete`.                           |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst111: | 500         | An invalid ``response.headers`` was set.                    |
|               |             | See :ref:`guc_resp_hdrs`.                                   |
| PGRST111      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst112: | 500         | The status code must be a positive integer.                 |
|               |             | See :ref:`guc_resp_status`.                                 |
| PGRST112      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst113: | 406         | More than one column was returned for a scalar result.      |
|               |             | See :ref:`scalar_return_formats`.                           |
| PGRST113      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst114: | 400         | For an :ref:`UPSERT using PUT <upsert_put>`, when           |
|               |             | :ref:`limits and offsets <limits>` are used.                |
| PGRST114      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst115: | 400         | For an :ref:`UPSERT using PUT <upsert_put>`, when the       |
|               |             | primary key in the query string and the body are different. |
| PGRST115      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst116: | 406         | More than 1 or no items where returned when requesting      |
|               |             | a singular response. See :ref:`singular_plural`.            |
| PGRST116      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst117: | 405         | The HTTP verb used in the request in not supported.         |
|               |             |                                                             |
| PGRST117      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+

.. _pgrst2**:

Group 2 - Schema Cache
----------------------

Related to a :ref:`stale schema cache <stale_schema>`. Most of the time, these errors are solved by :ref:`reloading the schema cache <schema_reloading>`.

+---------------+-------------+-------------------------------------------------------------+
| Code          | HTTP status | Description                                                 |
+===============+=============+=============================================================+
| .. _pgrst200: | 400         | Caused by :ref:`stale_fk_relationships`, otherwise any of   |
|               |             | the embedding resources or the relationship itself may not  |
| PGRST200      |             | exist in the database.                                      |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst201: | 300         | An ambiguous embedding request was made.                    |
|               |             | See :ref:`embed_disamb`.                                    |
| PGRST201      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst202: | 404         | Caused by a :ref:`stale_function_signature`, otherwise      |
|               |             | the function may not exist in the database.                 |
| PGRST202      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst203: | 300         | Caused by requesting overloaded functions with the same     |
|               |             | argument names but different types, or by using a ``POST``  |
| PGRST203      |             | verb to request overloaded functions with a ``JSON`` or     |
|               |             | ``JSONB`` type unnamed parameter. The solution is to rename |
|               |             | the function or add/modify the names of the arguments.      |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst204: | 400         | Caused when the :ref:`column specified <specify_columns>`   |
|               |             | in the ``columns`` query parameter is not found.            |
| PGRST204      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+

.. _pgrst3**:

Group 3 - JWT
-------------

Related to the authentication process using JWT. You can follow the :ref:`tut1` for an example on how to implement authentication and the :doc:`Authentication page <auth>` for more information on this process.

+---------------+-------------+-------------------------------------------------------------+
| Code          | HTTP status | Description                                                 |
+===============+=============+=============================================================+
| .. _pgrst300: | 500         | A :ref:`JWT secret <jwt-secret>` is missing from the        |
|               |             | configuration.                                              |
| PGRST300      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst301: | 401         | Any error related to the verification of the JWT,           |
|               |             | which means that the JWT provided is invalid in some way.   |
| PGRST301      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
| .. _pgrst302: | 401         | Attempted to do a request without                           |
|               |             | :ref:`authentication <client_auth>` when the anonymous role |
| PGRST302      |             | is disabled by not setting it in :ref:`db-anon-role`.       |
+---------------+-------------+-------------------------------------------------------------+

.. The Internal Errors Group X** is always at the end

.. _pgrst_X**:

Group X - Internal
------------------

Internal errors. If you encounter any of these, you may have stumbled on a PostgREST bug, please `open an issue <https://github.com/PostgREST/postgrest/issues>`_ and we'll be glad to fix it.

+---------------+-------------+-------------------------------------------------------------+
| Code          | HTTP status | Description                                                 |
+===============+=============+=============================================================+
| .. _pgrstX00: | 500         | Internal errors related to the library used for connecting  |
|               |             | to the database.                                            |
| PGRSTX00      |             |                                                             |
+---------------+-------------+-------------------------------------------------------------+
