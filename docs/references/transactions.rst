.. _transactions:

Transactions
============

After :ref:`user_impersonation`, every request to an :doc:`API resource <api>` runs inside a transaction. The sequence of the transaction is as follows:

.. code-block:: postgresql

  BEGIN; -- <Access Mode> <Isolation Level>
  -- <Transaction-scoped settings>
  -- <Main Query>
  END; -- <Transaction End>

.. _access_mode:

Access Mode
-----------

The access mode determines whether the transaction can modify the database or not. There are 2 possible values: READ ONLY and READ WRITE.

Modifying the database inside READ ONLY transactions is not possible. PostgREST uses this fact to enforce HTTP semantics in GET and HEAD requests. Consider the following:

.. code-block:: postgresql

  CREATE SEQUENCE callcounter_count START 1;

  CREATE VIEW callcounter AS
  SELECT nextval('callcounter_count');

Since the ``callcounter`` view modifies the sequence, calling it with GET or HEAD will result in an error:

.. tabs::

  .. code-tab:: http

    GET /callounter HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/callcounter"

.. code-block:: http

  HTTP/1.1 405 Method Not Allowed

  {"code":"25006","details":null,"hint":null,"message":"cannot execute nextval() in a read-only transaction"}

Access Mode on Tables and Views
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The access mode on :ref:`tables_views` is determined by the HTTP method.

.. list-table::
   :header-rows: 1

   * - HTTP Method
     - Access Mode
   * - GET, HEAD
     - READ ONLY
   * - POST, PATCH, PUT, DELETE
     - READ WRITE

Access Mode on Functions
~~~~~~~~~~~~~~~~~~~~~~~~

:ref:`s_procs` additionally depend on the function `volatility <https://www.postgresql.org/docs/current/xfunc-volatility.html>`_.

.. list-table::
   :header-rows: 2

   * -
     - Access Mode
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

.. note::

  - The volatility marker is a promise about the behavior of the function.  PostgreSQL will let you mark a function that modifies the database as ``IMMUTABLE`` or ``STABLE`` without failure.  But, because of the READ ONLY transaction the function will fail under PostgREST.
  - The :ref:`options_requests` method doesn't start a transaction, so it's not relevant here.

.. _isolation_lvl:

Isolation Level
---------------

Every transaction uses the PostgreSQL default isolation level: READ COMMITTED. Unless you modify `default_transaction_isolation <https://www.postgresql.org/docs/15/runtime-config-client.html#GUC-DEFAULT-TRANSACTION-ISOLATION>`_  for an impersonated role or function.

Using :ref:`impersonated_settings`, change the isolation level for all the role's requests with:

.. code-block:: postgresql

  ALTER ROLE webuser SET default_transaction_isolation TO 'repeatable read';

Or to change the isolation level per function call.

.. code-block:: postgresql

  CREATE OR REPLACE FUNCTION myfunc()
  RETURNS text as $$
    SELECT 'hello';
  $$
  LANGUAGE SQL
  SET default_transaction_isolation TO 'serializable';

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

.. important::

  - The headers names are lowercased. e.g. If the request sends ``User-Agent: x`` this will be obtainable as ``current_setting('request.headers', true)::json->>'user-agent'``.
  - The ``role`` in ``request.jwt.claims`` defaults to the value of :ref:`db-anon-role`.
  - Settings don't become NULL after the transaction is committed, instead they're set to a an empty string ``''``.

    + This is considered expected behavior by PostgreSQL. For more details, see `this discussion <https://www.postgresql.org/message-id/flat/CAB_pDVVa84w7hXhzvyuMTb8f5kKV3bee_p9QTZZ58Rg7zYM7sw%40mail.gmail.com>`_.
    + To avoid this inconsistency, you can create a wrapper function like:

    .. code-block:: postgresql

      CREATE FUNCTION my_current_setting(text) RETURNS text
      LANGUAGE SQL AS $$
        SELECT nullif(current_setting($1, true), '');
      $$;

.. _guc_legacy_names:

Legacy settings
^^^^^^^^^^^^^^^

For PostgreSQL versions below 14, PostgREST will take into consideration the :ref:`db-use-legacy-gucs` config, which is set to true by default. This means that the interface for accessing these GUCs is `the same as in older versions <https://postgrest.org/en/v8.0/api.html#accessing-request-headers-cookies-and-jwt-claims>`_. You can opt in to use the JSON GUCs mentioned above by setting the ``db-use-legacy-gucs`` to false.

.. _guc_req_path_method:

Request Path and Method
~~~~~~~~~~~~~~~~~~~~~~~

The path and method are stored as ``text``.

.. code-block:: postgresql

  SELECT current_setting('request.path', true);

  SELECT current_setting('request.method', true);

Request Role and Search Path
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Because of :ref:`user_impersonation`, PostgREST sets the standard ``role``. You can get this in different ways:

.. code-block:: postgresql

  SELECT current_role;

  SELECT current_user;

  SELECT current_setting('role', true);

Additionally it also sets the ``search_path`` based on :ref:`db-schemas` and :ref:`db-extra-search-path`.

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

The main query is generated by requesting :ref:`tables_views` or :ref:`s_procs`. All generated queries use prepared statements (:ref:`db-prepared-statements`).

Transaction End
---------------

If the transaction doesn't fail, it will always end in a COMMIT. Unless :ref:`db-tx-end` is configured to ROLLBACK in any case or conditionally with ``Prefer: tx=rollback``. This can be used for testing purposes.

Aborting transactions
---------------------

Any database failure(like a failed constraint) will result in a rollback of the transaction. You can also :ref:`RAISE an error inside a function <raise_error>` to cause a rollback.

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
