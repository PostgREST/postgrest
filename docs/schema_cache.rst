.. _schema_cache:

Schema Cache
============

Certain PostgREST features require metadata from the database schema. Getting this metadata requires executing expensive queries, so
in order to avoid repeating this work, PostgREST uses a schema cache.

+--------------------------------------------+-------------------------------------------------------------------------------+
| Feature                                    | Required Metadata                                                             |
+============================================+===============================================================================+
| :ref:`resource_embedding`                  | Foreign key constraints                                                       |
+--------------------------------------------+-------------------------------------------------------------------------------+
| :ref:`Stored Functions <s_procs>`          | Function signature (parameters, return type, volatility and                   |
|                                            | `overloading <https://www.postgresql.org/docs/current/xfunc-overload.html>`_) |
+--------------------------------------------+-------------------------------------------------------------------------------+
| :ref:`Upserts <upsert>`                    | Primary keys                                                                  |
+--------------------------------------------+-------------------------------------------------------------------------------+
| :ref:`Insertions <insert>`                 | Primary keys (optional: only if the Location header is requested)             |
+--------------------------------------------+-------------------------------------------------------------------------------+
| :ref:`OPTIONS requests <options_requests>` | View INSTEAD OF TRIGGERS and primary keys                                     |
+--------------------------------------------+-------------------------------------------------------------------------------+
| :ref:`open-api`                            | Table columns, primary keys and foreign keys                                  |
+                                            +-------------------------------------------------------------------------------+
|                                            | View columns and INSTEAD OF TRIGGERS                                          |
+                                            +-------------------------------------------------------------------------------+
|                                            | Function signature                                                            |
+--------------------------------------------+-------------------------------------------------------------------------------+

.. _stale_schema:

The Stale Schema Cache
----------------------

When you make changes on the metadata mentioned above, the schema cache will turn stale on a running PostgREST. Future requests that use the above features will need the :ref:`schema cache to be reloaded <schema_reloading>`; otherwise, you'll get an error instead of the expected result.

For instance, let's see what would happen if you have a stale schema cache for foreign key relationships and function signatures.

.. _stale_fk_relationships:

Stale Foreign Key Relationships
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Suppose you add a ``cities`` table to your database and define a foreign key that references an existing ``countries`` table. Then, you make a request to get the ``cities`` and their belonging ``countries``.

.. tabs::

  .. code-tab:: http

    GET /cities?select=name,country:countries(id,name) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/cities?select=name,country:countries(id,name)"

The result will be an error:

.. code-block:: json

  {
    "hint": "Verify that 'cities' and 'countries' exist in the schema 'api' and that there is a foreign key relationship between them. If a new relationship was created, try reloading the schema cache.",
    "details": null,
    "code": "PGRST200",
    "message": "Could not find a relationship between 'cities' and 'countries' in the schema cache"
  }

As you can see, PostgREST couldn't find the newly created foreign key in the schema cache. See :ref:`schema_reloading` and :ref:`auto_schema_reloading` to solve this issue.

.. _stale_function_signature:

Stale Function Signature
~~~~~~~~~~~~~~~~~~~~~~~~

The same issue will occur on newly created functions on a running PostgREST.

.. code-block:: plpgsql

  CREATE FUNCTION plus_one(num integer)
  RETURNS integer AS $$
   SELECT num + 1;
  $$ LANGUAGE SQL IMMUTABLE;

.. tabs::

  .. code-tab:: http

    GET /rpc/plus_one?num=1 HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/rpc/plus_one?num=1"

.. code-block:: json

  {
    "hint": "If a new function was created in the database with this name and parameters, try reloading the schema cache.",
    "details": null,
    "code": "PGRST202",
    "message": "Could not find the api.plus_one(num) function in the schema cache"
  }

Here, PostgREST tries to find the function on the stale schema to no avail. See :ref:`schema_reloading` and :ref:`auto_schema_reloading` to solve this issue.

.. _schema_reloading:

Schema Cache Reloading
----------------------

To reload the cache without restarting the PostgREST server, send a SIGUSR1 signal to the server process.

.. code:: bash

  killall -SIGUSR1 postgrest


For docker you can do:

.. code:: bash

  docker kill -s SIGUSR1 <container>

  # or in docker-compose
  docker-compose kill -s SIGUSR1 <service>

There's no downtime when reloading the schema cache. The reloading will happen on a background thread while requests keep being served.

.. _schema_reloading_notify:

Reloading with NOTIFY
~~~~~~~~~~~~~~~~~~~~~

There are environments where you can't send the SIGUSR1 Unix Signal (like on managed containers in cloud services or on Windows systems). For this reason, PostgREST also allows you to reload its schema cache through PostgreSQL `NOTIFY <https://www.postgresql.org/docs/current/sql-notify.html>`_ as follows:

.. code-block:: postgresql

  NOTIFY pgrst, 'reload schema'

The ``"pgrst"`` notification channel is enabled by default. For configuring the channel, see :ref:`db-channel` and :ref:`db-channel-enabled`.

.. _auto_schema_reloading:

Automatic Schema Cache Reloading
--------------------------------

You can do automatic schema cache reloading in a pure SQL way and forget about stale schema cache errors with an `event trigger <https://www.postgresql.org/docs/current/event-trigger-definition.html>`_ and ``NOTIFY``.

.. code-block:: postgresql

  -- Create an event trigger function
  CREATE OR REPLACE FUNCTION public.pgrst_watch() RETURNS event_trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    NOTIFY pgrst, 'reload schema';
  END;
  $$;

  -- This event trigger will fire after every ddl_command_end event
  CREATE EVENT TRIGGER pgrst_watch
    ON ddl_command_end
    EXECUTE PROCEDURE public.pgrst_watch();

Now, whenever the ``pgrst_watch`` trigger is fired in the database, PostgREST will automatically reload the schema cache.

To disable auto reloading, drop the trigger:

.. code-block:: postgresql

  DROP EVENT TRIGGER pgrst_watch

Finer-Grained Event Trigger
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can refine the previous event trigger and only react to the events relevant to the schema cache. This also prevents unnecessary
reloading when creating temporary tables(``CREATE TEMP TABLE``) inside functions.

.. code-block:: postgresql

  -- watch create and alter
  CREATE OR REPLACE FUNCTION pgrst_ddl_watch() RETURNS event_trigger AS $$
  DECLARE
    cmd record;
  BEGIN
    FOR cmd IN SELECT * FROM pg_event_trigger_ddl_commands()
    LOOP
      IF cmd.command_tag IN (
        'CREATE SCHEMA', 'ALTER SCHEMA'
      , 'CREATE TABLE', 'CREATE TABLE AS', 'SELECT INTO', 'ALTER TABLE'
      , 'CREATE FOREIGN TABLE', 'ALTER FOREIGN TABLE'
      , 'CREATE VIEW', 'ALTER VIEW'
      , 'CREATE MATERIALIZED VIEW', 'ALTER MATERIALIZED VIEW'
      , 'CREATE FUNCTION', 'ALTER FUNCTION'
      , 'CREATE TRIGGER'
      , 'CREATE TYPE', 'ALTER TYPE'
      , 'CREATE RULE'
      , 'COMMENT'
      )
      -- don't notify in case of CREATE TEMP table or other objects created on pg_temp
      AND cmd.schema_name is distinct from 'pg_temp'
      THEN
        NOTIFY pgrst, 'reload schema';
      END IF;
    END LOOP;
  END; $$ LANGUAGE plpgsql;

  -- watch drop
  CREATE OR REPLACE FUNCTION pgrst_drop_watch() RETURNS event_trigger AS $$
  DECLARE
    obj record;
  BEGIN
    FOR obj IN SELECT * FROM pg_event_trigger_dropped_objects()
    LOOP
      IF obj.object_type IN (
        'schema'
      , 'table'
      , 'foreign table'
      , 'view'
      , 'materialized view'
      , 'function'
      , 'trigger'
      , 'type'
      , 'rule'
      )
      AND obj.is_temporary IS false -- no pg_temp objects
      THEN
        NOTIFY pgrst, 'reload schema';
      END IF;
    END LOOP;
  END; $$ LANGUAGE plpgsql;

  CREATE EVENT TRIGGER pgrst_ddl_watch
    ON ddl_command_end
    EXECUTE PROCEDURE pgrst_ddl_watch();

  CREATE EVENT TRIGGER pgrst_drop_watch
    ON sql_drop
    EXECUTE PROCEDURE pgrst_drop_watch();
