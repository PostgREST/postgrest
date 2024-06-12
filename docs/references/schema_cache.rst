.. _schema_cache:

Schema Cache
============

PostgREST requires metadata from the database schema to provide a REST API that abstracts SQL details. One example of this is the interface for :ref:`resource_embedding`.

Getting this metadata requires expensive queries. To avoid repeating this work, PostgREST uses a schema cache.

.. _schema_reloading:

Schema Cache Reloading
----------------------

To not let the schema cache go stale (happens when you make changes to the database), you need to reload it.

You can do this with UNIX signals or with PostgreSQL notifications. It's also possible to do this automatically using `event triggers <https://www.postgresql.org/docs/current/event-trigger-definition.html>`_.

.. note::

  - Requests will wait until the schema cache reload is done. This to prevent client errors due to an stale schema cache.
  - If you are using the :ref:`in_db_config`, a schema cache reload will :ref:`reload the configuration<config_reloading>` as well.

.. _schema_reloading_signals:

Schema Cache Reloading with Unix Signals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To manually reload the cache without restarting the PostgREST server, send a SIGUSR1 signal to the server process.

.. code:: bash

  killall -SIGUSR1 postgrest


For docker you can do:

.. code:: bash

  docker kill -s SIGUSR1 <container>

  # or in docker-compose
  docker-compose kill -s SIGUSR1 <service>

.. _schema_reloading_notify:

Schema Cache Reloading with NOTIFY
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To reload the schema cache from within the database, you can use the ``NOTIFY`` command. See :ref:`listener`.

.. code-block:: postgres

  NOTIFY pgrst, 'reload schema'

.. _auto_schema_reloading:

Automatic Schema Cache Reloading
--------------------------------

You can do automatic reloading and forget there is a schema cache. For this use an `event trigger <https://www.postgresql.org/docs/current/event-trigger-definition.html>`_ and ``NOTIFY``.

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
