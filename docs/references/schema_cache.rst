.. _schema_cache:

Schema Cache
============

Some PostgREST features need metadata from the database schema. Getting this metadata requires expensive queries. To avoid repeating this work, PostgREST uses a schema cache.

+--------------------------------------------+-------------------------------------------------------------------------------+
| Feature                                    | Required Metadata                                                             |
+============================================+===============================================================================+
| :ref:`resource_embedding`                  | Foreign key constraints                                                       |
+--------------------------------------------+-------------------------------------------------------------------------------+
| :ref:`Functions <functions>`               | Function signature (parameters, return type, volatility and                   |
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

Stale Schema Cache
------------------

One operational problem that comes with a cache is that it can go stale. This can happen for PostgREST when you make changes to the metadata before mentioned. Requests that depend on the metadata will fail.

You can solve this by reloading the cache manually or automatically.

.. note::
  If you are using :ref:`in_db_config`, a schema reload will always :ref:`reload the configuration<config_reloading>` as well.

.. _schema_reloading:

Schema Cache Reloading
----------------------

To manually reload the cache without restarting the PostgREST server, send a SIGUSR1 signal to the server process.

.. code:: bash

  killall -SIGUSR1 postgrest


For docker you can do:

.. code:: bash

  docker kill -s SIGUSR1 <container>

  # or in docker-compose
  docker-compose kill -s SIGUSR1 <service>

There’s no downtime when reloading the schema cache. The reloading will happen on a background thread while serving requests.

.. _schema_reloading_notify:

Reloading with NOTIFY
~~~~~~~~~~~~~~~~~~~~~

PostgREST also allows you to reload its schema cache through PostgreSQL `NOTIFY <https://www.postgresql.org/docs/current/sql-notify.html>`_.

.. code-block:: postgres

  NOTIFY pgrst, 'reload schema'

This is useful in environments where you can’t send the SIGUSR1 Unix Signal. Like on cloud managed containers or on Windows systems.

The ``pgrst`` notification channel is enabled by default. For configuring the channel, see :ref:`db-channel` and :ref:`db-channel-enabled`.

.. _auto_schema_reloading:

Automatic Schema Cache Reloading
--------------------------------

You can do automatic schema cache reloading in a pure SQL way and forget about stale schema cache errors. For this use an `event trigger <https://www.postgresql.org/docs/current/event-trigger-definition.html>`_ and ``NOTIFY``.

.. code-block:: postgres

  -- Create an event trigger function
  CREATE OR REPLACE FUNCTION pgrst_watch() RETURNS event_trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    NOTIFY pgrst, 'reload schema';
  END;
  $$;

  -- This event trigger will fire after every ddl_command_end event
  CREATE EVENT TRIGGER pgrst_watch
    ON ddl_command_end
    EXECUTE PROCEDURE pgrst_watch();

Now, whenever the ``pgrst_watch`` trigger fires, PostgREST will auto-reload the schema cache.

To disable auto reloading, drop the trigger.

.. code-block:: postgres

  DROP EVENT TRIGGER pgrst_watch

Finer-Grained Event Trigger
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can refine the previous event trigger to only react to the events relevant to the schema cache. This also prevents unnecessary
reloading when creating temporary tables inside functions.

.. code-block:: postgres

  -- watch CREATE and ALTER
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

  -- watch DROP
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
