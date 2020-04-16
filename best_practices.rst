.. _func_privs:

Function privileges
-------------------

By default, when a function is created, the privilege to execute it is not restricted by role. The function access is PUBLIC—executable by all roles(more details at `PostgreSQL Privileges page <https://www.postgresql.org/docs/12/ddl-priv.html>`_). This is not ideal for an API schema. To disable this behavior, you can run the following SQL statement:

.. code-block:: postgres

  -- Assuming your schema is named "api"
  ALTER DEFAULT PRIVILEGES IN SCHEMA api REVOKE EXECUTE ON FUNCTIONS FROM PUBLIC;

  -- Or to stop functions from being executable in the whole database(note the removal of the IN SCHEMA part).
  ALTER DEFAULT PRIVILEGES REVOKE EXECUTE ON FUNCTIONS FROM PUBLIC;

See `PostgreSQL alter default privileges <https://www.postgresql.org/docs/current/static/sql-alterdefaultprivileges.html>`_ for more details.

After that, you'll need to grant EXECUTE privileges on functions explicitly:

.. code-block:: postgres

   GRANT EXECUTE ON FUNCTION login TO anonymous;
   GRANT EXECUTE ON FUNCTION reset_password TO web_user;

   -- you can also GRANT EXECUTE on all functions to a privileged role
   GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA api TO admin;

Security definer
----------------

By default, a function is executed with the privileges of the user who calls it. This means that the user has to have all permissions to do the operations the procedure performs.

Another option is to define the function with the :code:`SECURITY DEFINER` option. Then only one permission check will take place, the permission to call the function, and the operations in the function will have the authority of the user who owns the function itself. See `PostgreSQL documentation <https://www.postgresql.org/docs/current/static/sql-createfunction.html#SQL-CREATEFUNCTION-SECURITY>`_ for more details.

Views with RLS
--------------

Views are invoked with the privileges of the view owner, much like stored procedures with the ``SECURITY DEFINER`` option. When created by a SUPERUSER role, all `row-level security <https://www.postgresql.org/docs/current/static/ddl-rowsecurity.html>`_ will be bypassed unless a different, non-SUPERUSER owner is specified.

.. code-block:: postgres

 -- Workaround:
 -- non-SUPERUSER role to be used as the owner of the views
 CREATE ROLE api_views_owner;
 -- alter the view owner so RLS can work normally
 ALTER VIEW sample_view OWNER TO api_views_owner;

Views with Rules
----------------

Insertion on VIEWs with complex `RULEs <https://www.postgresql.org/docs/11/sql-createrule.html>`_ might not work out of the box with PostgREST.
It's recommended that you `use triggers instead of RULEs <https://wiki.postgresql.org/wiki/Don%27t_Do_This#Don.27t_use_rules>`_.
If you want to keep using RULEs, a workaround is to wrap the VIEW insertion in a stored procedure and call it through the :ref:`s_procs` interface.
