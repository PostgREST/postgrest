.. _roles:

Overview of Role System
=======================

PostgREST is designed to keep the database at the center of API security. All authorization happens through database roles and permissions. It is PostgREST's job to **authenticate** requests -- i.e. verify that a client is who they say they are -- and then let the database **authorize** client actions.

Authentication Sequence
-----------------------

There are three types of roles used by PostgREST, the **authenticator**, **anonymous** and **user** roles. The database administrator creates these roles and configures PostgREST to use them.

.. image:: _static/security-roles.png

The authenticator should be created :code:`NOINHERIT` and configured in the database to have very limited access. It is a chameleon whose job is to "become" other users to service authenticated HTTP requests. The picture below shows how the server handles authentication. If auth succeeds, it switches into the user role specified by the request, otherwise it switches into the anonymous role.

.. image:: _static/security-anon-choice.png

Here are the technical details. We use `JSON Web Tokens <https://jwt.io/>`_ to authenticate API requests. As you'll recall a JWT contains a list of cryptographically signed claims. All claims are allowed but PostgREST cares specifically about a claim called role.

.. code:: json

  {
    "role": "user123"
  }

When a request contains a valid JWT with a role claim PostgREST will switch to the database role with that name for the duration of the HTTP request.

.. code:: sql

  SET LOCAL ROLE user123;

Note that the database administrator must allow the authenticator role to switch into this user by previously executing

.. code:: sql

  GRANT user123 TO authenticator;

If the client included no JWT (or one without a role claim) then PostgREST switches into the anonymous role whose actual database-specific name, like that of with the authenticator role, is specified in the PostgREST server configuration file. The database administrator must set anonymous role permissions correctly to prevent anonymous users from seeing or changing things they shouldn't.

Users and Groups
----------------

PostgreSQL manages database access permissions using the concept of roles. A role can be thought of as either a database user, or a group of database users, depending on how the role is set up.

Roles for Each Web User
~~~~~~~~~~~~~~~~~~~~~~~

PostgREST can accommodate either viewpoint. If you treat a role as a single user then the JWT-based role switching described above does most of what you need. When an authenticated user makes a request PostgREST will switch into the role for that user, which in addition to restricting queries, is available to SQL through the :code:`current_user` variable.

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

We want to enforce a policy that ensures a user can see only those messages sent by him or intended for him. Also we want to prevent a user from forging the message_from column with another person's name.

PostgreSQL (9.5 and later) allows us to set this policy with row-level security:

.. code-block:: postgres

  CREATE POLICY chat_policy ON chat
    USING ((message_to = current_user) OR (message_from = current_user))
    WITH CHECK (message_from = current_user)

Anyone accessing the generated API endpoint for the chat table will see exactly the rows they should, without our needing custom imperative server-side coding.

.. warning::

   Roles are namespaced per-cluster rather than per-database so they may be prone to collision.

Web Users Sharing Role
~~~~~~~~~~~~~~~~~~~~~~

Alternately database roles can represent groups instead of (or in addition to) individual users. You may choose that all signed-in users for a web app share the role webuser. You can distinguish individual users by including extra claims in the JWT such as email.

.. code:: json

  {
    "role": "webuser",
    "email": "john@doe.com"
  }

SQL code can access claims through GUC variables set by PostgREST per request. For instance to get the email claim, call this function:

.. code:: sql

  current_setting('request.jwt.claim.email', true)

This allows JWT generation services to include extra information and your database code to react to it. For instance the RLS example could be modified to use this current_setting rather than current_user. The second 'true' argument tells current_setting to return NULL if the setting is missing from the current configuration.

Hybrid User-Group Roles
~~~~~~~~~~~~~~~~~~~~~~~

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

.. _custom_validation:

Custom Validation
-----------------

PostgREST honors the :code:`exp` claim for token expiration, rejecting expired tokens. However it does not enforce any extra constraints. An example of an extra constraint would be to immediately revoke access for a certain user. The configuration file parameter :code:`pre-request` specifies a stored procedure to call immediately after the authenticator switches into a new role and before the main query itself runs.

Here's an example. In the config file specify a stored procedure:

.. code:: ini

  pre-request = "public.check_user"

In the function you can run arbitrary code to check the request and raise an exception to block it if desired.

.. code-block:: postgres

  CREATE OR REPLACE FUNCTION check_user() RETURNS void AS $$
  BEGIN
    IF current_user = 'evil_user' THEN
      RAISE EXCEPTION 'No, you are evil'
        USING HINT = 'Stop being so evil and maybe you can log in';
    END IF;
  END
  $$ LANGUAGE plpgsql;

Client Auth
===========

To make an authenticated request the client must include an :code:`Authorization` HTTP header with the value :code:`Bearer <jwt>`. For instance:

.. code:: http

  GET /foo HTTP/1.1
  Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiamRvZSIsImV4cCI6MTQ3NTUxNjI1MH0.GYDZV3yM0gqvuEtJmfpplLBXSGYnke_Pvnl0tbKAjB4

JWT Generation
--------------

You can create a valid JWT either from inside your database or via an external service. Each token is cryptographically signed with a secret key. In the case of symmetric cryptography the signer and verifier share the same secret passphrase. In asymmetric cryptography the signer uses the private key and the verifier the public key. PostgREST supports both symmetric and asymmetric cryptography.

JWT from SQL
~~~~~~~~~~~~

You can create JWT tokens in SQL using the `pgjwt extension <https://github.com/michelp/pgjwt>`_. It's simple and requires only pgcrypto. If you're on an environment like Amazon RDS which doesn't support installing new extensions, you can still manually run the `SQL inside pgjwt <https://github.com/michelp/pgjwt/blob/master/pgjwt--0.1.0.sql>`_ (you'll need to replace ``@extschema@`` with another schema or just delete it) which creates the functions you will need.

Next write a stored procedure that returns the token. The one below returns a token with a hard-coded role, which expires five minutes after it was issued. Note this function has a hard-coded secret as well.

.. code-block:: postgres

  CREATE TYPE jwt_token AS (
    token text
  );

  CREATE FUNCTION jwt_test() RETURNS public.jwt_token AS $$
    SELECT public.sign(
      row_to_json(r), 'reallyreallyreallyreallyverysafe'
    ) AS token
    FROM (
      SELECT
        'my_role'::text as role,
        extract(epoch from now())::integer + 300 AS exp
    ) r;
  $$ LANGUAGE sql;

PostgREST exposes this function to clients via a POST request to ``/rpc/jwt_test``.

.. note::

  To avoid hard-coding the secret in stored procedures, save it as a property of the database.

  .. code-block:: postgres

    -- run this once
    ALTER DATABASE mydb SET "app.jwt_secret" TO 'reallyreallyreallyreallyverysafe';

    -- then all functions can refer to app.jwt_secret
    SELECT sign(
      row_to_json(r), current_setting('app.jwt_secret')
    ) AS token
    FROM ...

JWT from Auth0
~~~~~~~~~~~~~~

An external service like `Auth0 <https://auth0.com/>`_ can do the hard work transforming OAuth from Github, Twitter, Google etc into a JWT suitable for PostgREST. Auth0 can also handle email signup and password reset flows.

To use Auth0, create `an application <https://auth0.com/docs/applications>`_ for your app and `an API <https://auth0.com/docs/authorization/apis>`_ for your PostgREST server. Auth0 supports both HS256 and RS256 scheme for the issued tokens for APIs. For simplicity, you may first try HS256 scheme while creating your API on Auth0. Your application should use your PostgREST API's `API identifier <https://auth0.com/docs/get-started/dashboard/api-settings>`_ by setting it with the `audience parameter <https://auth0.com/docs/tokens/access-tokens/get-access-tokens#control-access-token-audience>`_  during the authorization request. This will ensure that Auth0 will issue an access token for your PostgREST API. For PostgREST to verify the access token, you will need to set ``jwt-secret`` on PostgREST config file with your API's signing secret.

.. note::

  Our code requires a database role in the JWT. To add it you need to save the database role in Auth0 `app metadata <https://auth0.com/docs/users/metadata/manage-metadata-rules>`_. Then, you will need to write `a rule <https://auth0.com/docs/rules>`_ that will extract the role from the user's app_metadata and set it as a `custom claim <https://auth0.com/docs/scopes/sample-use-cases-scopes-and-claims#add-custom-claims-to-a-token>`_ in the access token. Note that, you may use Auth0's `core authorization feature <https://auth0.com/docs/authorization/rbac>`_ for more complex use cases. Metadata solution is mentioned here for simplicity.

  .. code:: javascript

    function (user, context, callback) {

      // Follow the documentations at
      // https://postgrest.org/en/latest/configuration.html#role-claim-key
      // to set a custom role claim on PostgREST
      // and use it as custom claim attribute in this rule
      const myRoleClaim = 'https://myapp.com/role';

      user.app_metadata = user.app_metadata || {};
      context.accessToken[myRoleClaim] = user.app_metadata.role;
      callback(null, user, context);
    }

.. _asym_keys:

Asymmetric Keys
~~~~~~~~~~~~~~~

As described in the :ref:`configuration` section, PostgREST accepts a ``jwt-secret`` config file parameter. If it is set to a simple string value like "reallyreallyreallyreallyverysafe" then PostgREST interprets it as an HMAC-SHA256 passphrase. However you can also specify a literal JSON Web Key (JWK) or set. For example, you can use an RSA-256 public key encoded as a JWK:

.. code-block:: json

  {
    "alg":"RS256",
    "e":"AQAB",
    "key_ops":["verify"],
    "kty":"RSA",
    "n":"9zKNYTaYGfGm1tBMpRT6FxOYrM720GhXdettc02uyakYSEHU2IJz90G_MLlEl4-WWWYoS_QKFupw3s7aPYlaAjamG22rAnvWu-rRkP5sSSkKvud_IgKL4iE6Y2WJx2Bkl1XUFkdZ8wlEUR6O1ft3TS4uA-qKifSZ43CahzAJyUezOH9shI--tirC028lNg767ldEki3WnVr3zokSujC9YJ_9XXjw2hFBfmJUrNb0-wldvxQbFU8RPXip-GQ_JPTrCTZhrzGFeWPvhA6Rqmc3b1PhM9jY7Dur1sjYWYVyXlFNCK3c-6feo5WlRfe1aCWmwZQh6O18eTmLeT4nWYkDzQ"
  }

.. note::

  This could also be a JSON Web Key Set (JWKS) if it was contained within an array assigned to a `keys` member, e.g. ``{ keys: [jwk1, jwk2] }``.

Just pass it in as a single line string, escaping the quotes:

.. code-block:: ini

  jwt-secret = "{ \"alg\":\"RS256\", … }"

To generate such a public/private key pair use a utility like `latchset/jose <https://github.com/latchset/jose>`_.

.. code-block:: bash

  jose jwk gen -i '{"alg": "RS256"}' -o rsa.jwk
  jose jwk pub -i rsa.jwk -o rsa.jwk.pub

  # now rsa.jwk.pub contains the desired JSON object

You can specify the literal value as we saw earlier, or reference a filename to load the JWK from a file:

.. code-block:: ini

  jwt-secret = "@rsa.jwk.pub"

JWT security
~~~~~~~~~~~~

There are at least three types of common critiques against using JWT: 1) against the standard itself, 2) against using libraries with known security vulnerabilities, and 3) against using JWT for web sessions. We'll briefly explain each critique, how PostgREST deals with it, and give recommendations for appropriate user action.

The critique against the `JWT standard <https://datatracker.ietf.org/doc/html/rfc7519>`_ is voiced in detail `elsewhere on the web <https://paragonie.com/blog/2017/03/jwt-json-web-tokens-is-bad-standard-that-everyone-should-avoid>`_. The most relevant part for PostgREST is the so-called :code:`alg=none` issue. Some servers implementing JWT allow clients to choose the algorithm used to sign the JWT. In this case, an attacker could set the algorithm to :code:`none`, remove the need for any signature at all and gain unauthorized access. The current implementation of PostgREST, however, does not allow clients to set the signature algorithm in the HTTP request, making this attack irrelevant. The critique against the standard is that it requires the implementation of the :code:`alg=none` at all.

Critiques against JWT libraries are only relevant to PostgREST via the library it uses. As mentioned above, not allowing clients to choose the signature algorithm in HTTP requests removes the greatest risk. Another more subtle attack is possible where servers use asymmetric algorithms like RSA for signatures. Once again this is not relevant to PostgREST since it is not supported. Curious readers can find more information in `this article <https://auth0.com/blog/critical-vulnerabilities-in-json-web-token-libraries/>`_. Recommendations about high quality libraries for usage in API clients can be found on `jwt.io <https://jwt.io/>`_.

The last type of critique focuses on the misuse of JWT for maintaining web sessions. The basic recommendation is to `stop using JWT for sessions <http://cryto.net/~joepie91/blog/2016/06/13/stop-using-jwt-for-sessions/>`_ because most, if not all, solutions to the problems that arise when you do, `do not work <http://cryto.net/~joepie91/blog/2016/06/19/stop-using-jwt-for-sessions-part-2-why-your-solution-doesnt-work/>`_. The linked articles discuss the problems in depth but the essence of the problem is that JWT is not designed to be secure and stateful units for client-side storage and therefore not suited to session management.

PostgREST uses JWT mainly for authentication and authorization purposes and encourages users to do the same. For web sessions, using cookies over HTTPS is good enough and well catered for by standard web frameworks.

Schema Isolation
================

You can isolate your api schema from internal implementation details, as explained in :ref:`schema_isolation`. For an example of wrapping a private table with a public view see the :ref:`public_ui` section below.

SQL User Management
===================

Storing Users and Passwords
---------------------------

As mentioned, an external service can provide user management and coordinate with the PostgREST server using JWT. It's also possible to support logins entirely through SQL. It's a fair bit of work, so get ready.

The following table, functions, and triggers will live in a :code:`basic_auth` schema that you shouldn't expose publicly in the API. The public views and functions will live in a different schema which internally references this internal information.

First we'll need a table to keep track of our users:

.. code:: sql

  -- We put things inside the basic_auth schema to hide
  -- them from public view. Certain public procs/views will
  -- refer to helpers and tables inside.
  create schema if not exists basic_auth;

  create table if not exists
  basic_auth.users (
    email    text primary key check ( email ~* '^.+@.+\..+$' ),
    pass     text not null check (length(pass) < 512),
    role     name not null check (length(role) < 512)
  );

We would like the role to be a foreign key to actual database roles, however PostgreSQL does not support these constraints against the :code:`pg_roles` table. We'll use a trigger to manually enforce it.

.. code-block:: plpgsql

  create or replace function
  basic_auth.check_role_exists() returns trigger as $$
  begin
    if not exists (select 1 from pg_roles as r where r.rolname = new.role) then
      raise foreign_key_violation using message =
        'unknown database role: ' || new.role;
      return null;
    end if;
    return new;
  end
  $$ language plpgsql;

  drop trigger if exists ensure_user_role_exists on basic_auth.users;
  create constraint trigger ensure_user_role_exists
    after insert or update on basic_auth.users
    for each row
    execute procedure basic_auth.check_role_exists();

Next we'll use the pgcrypto extension and a trigger to keep passwords safe in the :code:`users` table.

.. code-block:: plpgsql

  create extension if not exists pgcrypto;

  create or replace function
  basic_auth.encrypt_pass() returns trigger as $$
  begin
    if tg_op = 'INSERT' or new.pass <> old.pass then
      new.pass = crypt(new.pass, gen_salt('bf'));
    end if;
    return new;
  end
  $$ language plpgsql;

  drop trigger if exists encrypt_pass on basic_auth.users;
  create trigger encrypt_pass
    before insert or update on basic_auth.users
    for each row
    execute procedure basic_auth.encrypt_pass();

With the table in place we can make a helper to check a password against the encrypted column. It returns the database role for a user if the email and password are correct.

.. code-block:: plpgsql

  create or replace function
  basic_auth.user_role(email text, pass text) returns name
    language plpgsql
    as $$
  begin
    return (
    select role from basic_auth.users
     where users.email = user_role.email
       and users.pass = crypt(user_role.pass, users.pass)
    );
  end;
  $$;

.. _public_ui:

Public User Interface
---------------------

In the previous section we created an internal table to store user information. Here we create a login function which takes an email address and password and returns JWT if the credentials match a user in the internal table.

Logins
~~~~~~

As described in `JWT from SQL`_, we'll create a JWT inside our login function. Note that you'll need to adjust the secret key which is hard-coded in this example to a secure (at least thirty-two character) secret of your choosing.

.. code-block:: postgres

  -- add type
  CREATE TYPE basic_auth.jwt_token AS (
    token text
  );

  -- login should be on your exposed schema
  create or replace function
  login(email text, pass text) returns basic_auth.jwt_token as $$
  declare
    _role name;
    result basic_auth.jwt_token;
  begin
    -- check email and password
    select basic_auth.user_role(email, pass) into _role;
    if _role is null then
      raise invalid_password using message = 'invalid user or password';
    end if;

    select sign(
        row_to_json(r), 'reallyreallyreallyreallyverysafe'
      ) as token
      from (
        select _role as role, login.email as email,
           extract(epoch from now())::integer + 60*60 as exp
      ) r
      into result;
    return result;
  end;
  $$ language plpgsql security definer;

An API request to call this function would look like:

.. code:: http

  POST /rpc/login HTTP/1.1

  { "email": "foo@bar.com", "pass": "foobar" }

The response would look like the snippet below. Try decoding the token at `jwt.io <https://jwt.io/>`_. (It was encoded with a secret of :code:`reallyreallyreallyreallyverysafe` as specified in the SQL code above. You'll want to change this secret in your app!)

.. code:: json

  {
    "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImZvb0BiYXIuY29tIiwicGFzcyI6ImZvb2JhciJ9.37066TTRlh-1hXhnA9oO9Pj6lgL6zFuJU0iCHhuCFno"
  }

Permissions
~~~~~~~~~~~

Your database roles need access to the schema, tables, views and functions in order to service HTTP requests.
Recall from the `Overview of Role System`_ that PostgREST uses special roles to process requests, namely the authenticator and
anonymous roles. Below is an example of permissions that allow anonymous users to create accounts and attempt to log in.

.. code-block:: postgres

  -- the names "anon" and "authenticator" are configurable and not
  -- sacred, we simply choose them for clarity
  create role anon noinherit;
  create role authenticator noinherit;
  grant anon to authenticator;

  grant execute on function login(text,text) to anon;

Since the above :code:`login` function is defined as `security definer <https://www.postgresql.org/docs/current/sql-createfunction.html#id-1.9.3.67.10.2>`_,
the anonymous user :code:`anon` doesn't need permission to read the :code:`basic_auth.users` table. It doesn't even need permission to access the :code:`basic_auth` schema.
:code:`grant execute on function` is included for clarity but it might not be needed, see :ref:`func_privs` for more details.
