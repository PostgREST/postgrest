Transactions
============

After :ref:`user_impersonation`, every request to an :doc:`API resource <api>` runs inside a transaction. The sequence of the transaction is as follows:

.. code-block:: postgresql

  BEGIN; -- <Access Mode> <Isolation Level>
  -- <Transaction-scoped settings>
  -- <Main Query>;
  END;

.. _access_mode:

Access Mode
-----------

The access mode on :ref:`tables_views` is determined by the HTTP method.

.. list-table::
   :header-rows: 1

   * - HTTP Method
     - Access Method
   * - GET, HEAD
     - READ ONLY
   * - POST, PATCH, PUT, DELETE
     - READ WRITE

:ref:`s_procs` additionally depend on the function `volatility <https://www.postgresql.org/docs/current/xfunc-volatility.html>`_.

.. list-table::
   :header-rows: 2

   * -
     - Access Method
     -
     -
   * - HTTP Method
     - VOLATILE
     - STABLE
     - IMMUTABLE
   * - GET, HEAD
     - READ ONLY
     - READ ONLY
     - READ ONLY
   * - POST
     - READ WRITE
     - READ ONLY
     - READ ONLY

Modifying the database inside READ ONLY transactions is not possible. PostgREST uses this fact to enforce HTTP semantics in GET and HEAD requests.

.. note::

  The volatility marker is a promise about the behavior of the function.  PostgreSQL will let you mark a function that modifies the database as ``IMMUTABLE`` or ``STABLE`` without failure.  But, because of the READ ONLY transaction the function will fail under PostgREST.

The :ref:`options_requests` method doesn't start a transaction, so it's not relevant here.

Isolation Level
---------------

Every transaction uses the PostgreSQL default isolation level: READ COMMITTED.

.. _tx_settings:

Transaction-Scoped Settings
---------------------------

PostgREST uses settings tied to the transaction lifetime. These can be used to get data about the HTTP request. Or to modify the HTTP response.

You can get these with ``current_setting``

.. code-block:: postgresql

  -- request settings use the ``request.`` prefix.
  SELECT
    current_setting('request.<setting>', true);

And you can set them with ``set_config``

.. code-block:: postgresql

  -- response settings use the ``response.`` prefix.
  SELECT
    set_config('response.<setting>', 'value1' ,true);

Request Role and Search Path
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Because of :ref:`user_impersonation`, PostgREST sets the standard ``role``. You can get this in different ways:

.. code-block:: postgresql

  SELECT current_role;

  SELECT current_user;

  SELECT current_setting('role', true);

Additionally it also sets the ``search_path`` based on :ref:`db-schemas` and :ref:`db-extra-search-path`.


.. _guc_req_headers_cookies_claims:

Request Headers, Cookies and JWT claims
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PostgREST stores the headers, cookies and headers as JSON. To get them:

.. code-block:: postgresql

  -- To get all the headers sent in the request
  SELECT current_setting('request.headers', true)::json;

  -- To get a single header, you can use JSON arrow operators
  SELECT current_setting('request.headers', true)::json->>'user-agent';

  -- value of sessionId in a cookie
  SELECT current_setting('request.cookies', true)::json->>'sessionId';

  -- value of the email claim in a jwt
  SELECT current_setting('request.jwt.claims', true)::json->>'email';

.. note::

  The ``role`` in ``request.jwt.claims`` defaults to the value of :ref:`db-anon-role`.

.. _guc_req_path_method:

Request Path and Method
~~~~~~~~~~~~~~~~~~~~~~~

The path and method are stored as ``text``.

.. code-block:: postgresql

  SELECT current_setting('request.path', true);

  SELECT current_setting('request.method', true);

.. _guc_resp_hdrs:

Response Headers
~~~~~~~~~~~~~~~~

You can set ``response.headers`` to add headers to the HTTP response. For instance, this statement would add caching headers to the response:

.. code-block:: sql

  -- tell client to cache response for two days

  SELECT set_config('response.headers',
    '[{"Cache-Control": "public"}, {"Cache-Control": "max-age=259200"}]', true);

.. code-block:: http

  HTTP/1.1 200 OK
  Content-Type: application/json; charset=utf-8
  Cache-Control: no-cache, no-store, must-revalidate

Notice that the ``response.headers`` should be set to an *array* of single-key objects rather than a single multiple-key object. This is because headers such as ``Cache-Control`` or ``Set-Cookie`` need repeating when setting many values. An object would not allow the repeated key.

.. note::

  PostgREST provided headers such as ``Content-Type``, ``Location``, etc. can be overriden this way. Note that irrespective of overridden ``Content-Type`` response header, the content will still be converted to JSON, unless you also set :ref:`raw-media-types` to something like ``text/html``.

.. _guc_resp_status:

Response Status Code
~~~~~~~~~~~~~~~~~~~~

You can set the ``response.status`` to override the default status code PostgREST provides. For instance, the following function would replace the default ``200`` status code.

.. code-block:: postgres

   create or replace function teapot() returns json as $$
   begin
     perform set_config('response.status', '418', true);
     return json_build_object('message', 'The requested entity body is short and stout.',
                              'hint', 'Tip it over and pour it out.');
   end;
   $$ language plpgsql;

.. tabs::

  .. code-tab:: http

    GET /rpc/teapot HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/rpc/teapot" -i

.. code-block:: http

  HTTP/1.1 418 I'm a teapot

  {
    "message" : "The requested entity body is short and stout.",
    "hint" : "Tip it over and pour it out."
  }

If the status code is standard, PostgREST will complete the status message(**I'm a teapot** in this example).

.. _main_query:

Main query
----------

The main query is produced by requesting the :doc:`API resources <api>`.

Transaction End
---------------

If the transaction doesn't fail, it will always end in a COMMIT. Unless :ref:`db-tx-end` is configured to ROLLBACK in any case or conditionally with ``Prefer: tx=rollback``. This can be used for testing purposes.

Aborting transactions
---------------------

Any database failure(like a failed constraint) will result in a rollback of the transaction. You can also do a RAISE inside a function to cause a rollback.

.. _raise_error:

Raise errors with HTTP Status Codes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can return non-200 HTTP status codes by raising SQL exceptions. For instance, here's a saucy function that always responds with an error:

.. code-block:: postgresql

  CREATE OR REPLACE FUNCTION just_fail() RETURNS void
    LANGUAGE plpgsql
    AS $$
  BEGIN
    RAISE EXCEPTION 'I refuse!'
      USING DETAIL = 'Pretty simple',
            HINT = 'There is nothing you can do.';
  END
  $$;

Calling the function returns HTTP 400 with the body

.. code-block:: json

  {
    "message":"I refuse!",
    "details":"Pretty simple",
    "hint":"There is nothing you can do.",
    "code":"P0001"
  }

One way to customize the HTTP status code is by raising particular exceptions according to the PostgREST :ref:`error to status code mapping <status_codes>`. For example, :code:`RAISE insufficient_privilege` will respond with HTTP 401/403 as appropriate.

For even greater control of the HTTP status code, raise an exception of the ``PTxyz`` type. For instance to respond with HTTP 402, raise 'PT402':

.. code-block:: sql

  RAISE sqlstate 'PT402' using
    message = 'Payment Required',
    detail = 'Quota exceeded',
    hint = 'Upgrade your plan';

Returns:

.. code-block:: http

  HTTP/1.1 402 Payment Required
  Content-Type: application/json; charset=utf-8

  {
    "message": "Payment Required",
    "details": "Quota exceeded",
    "hint": "Upgrade your plan",
    "code": "PT402"
  }

.. _pre-request:

Pre-Request
-----------

The pre-request is a function that can run after the :ref:`tx_settings` are set and before the :ref:`main_query`. It's enabled with :ref:`db-pre-request`.

This provides an opportunity to modify settings or raise an exception to prevent the request from completing.

.. _pre_req_headers:

Setting headers via pre-request
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As an example, let's add some cache headers for all requests that come from an Internet Explorer(6 or 7) browser.

.. code-block:: postgresql

   create or replace function custom_headers()
   returns void as $$
   declare
     user_agent text := current_setting('request.headers', true)::json->>'user-agent';
   begin
     if user_agent similar to '%MSIE (6.0|7.0)%' then
       perform set_config('response.headers',
         '[{"Cache-Control": "no-cache, no-store, must-revalidate"}]', false);
     end if;
   end; $$ language plpgsql;

   -- set this function on postgrest.conf
   -- db-pre-request = custom_headers

Now when you make a GET request to a table or view, you'll get the cache headers.

.. tabs::

  .. code-tab:: http

    GET /people HTTP/1.1
    User-Agent: Mozilla/4.01 (compatible; MSIE 6.0; Windows NT 5.1)

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people" -i \
     -H "User-Agent: Mozilla/4.01 (compatible; MSIE 6.0; Windows NT 5.1)"
