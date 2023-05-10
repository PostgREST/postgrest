.. _connection_pool:

Connection Pool
===============

Every request to an :doc:`API resource <api>` borrows a connection from the connection pool to start a :doc:`transaction <transactions>`.

A connection pool is a cache of reusable database connections. It allows serving many HTTP requests using few database connections.

Minimizing connections it’s paramount to performance. Each PostgreSQL connection creates a process, having too many can exhaust available resources.

.. _pool_growth_limit:

Growth Limit
------------

If all the connections are being used, a new connection is added to the pool. The pool can grow until it reaches the :ref:`db-pool` size.

Note it’s pointless to set this higher than the ``max_connections`` setting in your database.

Connection lifetime
-------------------

After a period of time, connections from the pool will be released and news ones will be created. This time is specified by :ref:`db-pool-max-lifetime`.

The lifetime doesn't affect running requests. Only unused connections will be released.

For knowing why a connection lifetime is necessary, see the following discussion:
https://www.postgresql.org/message-id/flat/CA%2Bmi_8bnvpxHZtb6EgHSHY-xn29W8VJMzjPU3fiCOv1bfjrNuA%40mail.gmail.com.

Acquisition Timeout
-------------------

If all the available connections in the pool are busy, an HTTP request will wait until reaching a timeout. You can configure this timeout with :ref:`db-pool-acquisition-timeout`.

If the request reaches the timeout, it will be aborted with the following response:

.. code-block:: http

  HTTP/1.1 504 Gateway Timeout

  {"code":"PGRST003",
   "details":null,
   "hint":null,
   "message":"Timed out acquiring connection from connection pool."}

Getting this error message is an indicator of a performance issue. To solve it, you can:

- Reduce your queries execution time.

  - Check the request :ref:`explain_plan` to tune your query, this usually means adding indexes.

- Reduce the amount of requests.

  - Reduce write requests. Do :ref:`bulk_insert` (or :ref:`upsert`) instead of inserting rows one by one.
  - Reduce read requests. Use :ref:`resource_embedding`. Combine unrelated data into a single request using custom database views or functions.
  - Use :ref:`s_procs` for combining read and write logic into a single request.

- Increase the :ref:`pool growth limit <pool_growth_limit>`.

  - Not a panacea since connections can't grow infinitely. Try the previous recommendations before this.

.. _automatic_recovery:

Automatic Recovery
------------------

If the pool loses the connection to the database, it will retry reconnecting using exponential backoff. With 32 seconds being the maximum backoff time between retries.

The retries happen immediately after a connection loss, if :ref:`db-channel-enabled` is set to true(the default). Otherwise they'll happen once a request arrives.

The server reloads the :ref:`schema_cache` when recovering.

To notify the client of the next retry, the server sends a ``503 Service Unavailable`` status with the ``Retry-After: x`` header. Where ``x`` is the number of seconds programmed for the next retry.

.. _external_connection_poolers:

Using External Connection Poolers
---------------------------------

It's possible to use external connection poolers, such as PgBouncer. Session pooling is compatible, while transaction pooling requires :ref:`db-prepared-statements` set to ``false``. Statement pooling is not compatible with PostgREST.

Also set :ref:`db-channel-enabled` to ``false`` since ``LISTEN`` is not compatible with transaction pooling. Although it should not give any errors if left enabled.

.. note::

  It’s not recommended to use an external connection pooler. `Our benchmarks <https://github.com/PostgREST/postgrest/issues/2294#issuecomment-1139148672>`_ indicate it provides much lower performance than PostgREST built-in pool.
