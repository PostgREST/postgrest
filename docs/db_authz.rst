.. raw:: html

  <h1>Database Authorization</h1>

Database authorization is the process of granting and verifying database access permissions. PostgreSQL manages permissions using the concept of roles. A role can be thought of as either a database user, or a group of database users, depending on how the role is set up.

Roles for Each Web User
-----------------------

PostgREST can accommodate either viewpoint. If you treat a role as a single user then the :ref:`jwt_impersonation` does most of what you need. When an authenticated user makes a request PostgREST will switch into the database role for that user, which in addition to restricting queries, is available to SQL through the :code:`current_user` variable.

You can use row-level security to flexibly restrict visibility and access for the current user. Here is an `example <https://www.2ndquadrant.com/en/blog/application-users-vs-row-level-security/>`_ from Tomas Vondra, a chat table storing messages sent between users. Users can insert rows into it to send messages to other users, and query it to see messages sent to them by other users.

.. code-block:: postgres

  CREATE TABLE chat (
    message_uuid    UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    message_time    TIMESTAMP NOT NULL DEFAULT now(),
    message_from    NAME      NOT NULL DEFAULT current_user,
    message_to      NAME      NOT NULL,
    message_subject VARCHAR(64) NOT NULL,
    message_body    TEXT
  );

  ALTER TABLE chat ENABLE ROW LEVEL SECURITY;

We want to enforce a policy that ensures a user can see only those messages sent by them or intended for them. Also we want to prevent a user from forging the ``message_from`` column with another person's name.

PostgreSQL allows us to set this policy with row-level security:

.. code-block:: postgres

  CREATE POLICY chat_policy ON chat
    USING ((message_to = current_user) OR (message_from = current_user))
    WITH CHECK (message_from = current_user)

Anyone accessing the generated API endpoint for the chat table will see exactly the rows they should, without our needing custom imperative server-side coding.

.. warning::

   Roles are namespaced per-cluster rather than per-database so they may be prone to collision.

Web Users Sharing Role
----------------------

Alternately database roles can represent groups instead of (or in addition to) individual users. You may choose that all signed-in users for a web app share the role ``webuser``. You can distinguish individual users by including extra claims in the JWT such as email.

.. code:: json

  {
    "role": "webuser",
    "email": "john@doe.com"
  }

SQL code can access claims through GUC variables set by PostgREST per request. For instance to get the email claim, call this function:

For PostgreSQL server version >= 14

.. code:: sql

  current_setting('request.jwt.claims', true)::json->>'email';


For PostgreSQL server version < 14

.. code:: sql

  current_setting('request.jwt.claim.email', true);

This allows JWT generation services to include extra information and your database code to react to it. For instance the RLS example could be modified to use this ``current_setting`` rather than ``current_user``. The second ``'true'`` argument tells ``current_setting`` to return NULL if the setting is missing from the current configuration.

Hybrid User-Group Roles
-----------------------

You can mix the group and individual role policies. For instance we could still have a webuser role and individual users which inherit from it:

.. code-block:: postgres

  CREATE ROLE webuser NOLOGIN;
  -- grant this role access to certain tables etc

  CREATE ROLE user000 NOLOGIN;
  GRANT webuser TO user000;
  -- now user000 can do whatever webuser can

  GRANT user000 TO authenticator;
  -- allow authenticator to switch into user000 role
  -- (the role itself has nologin)

