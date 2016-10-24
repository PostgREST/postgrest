Overview of Role System
=======================

PostgREST is designed to keep the database at the center of API security. All authorization happens through database roles and permissions. It is PostgREST's job to authenticate requests -- i.e. verify that a client is who they say they are -- and then let the database authorize client actions.

There are three types of roles used by PostgREST, the **authenticator**, **anonymous** and **user** roles. The database administrator creates these roles and configures PostgREST to use them.

.. image:: _static/security-roles.png

The authenticator should be created `NOINHERIT` and configured in the database to have very limited access. It is a chameleon whose job is to "become" other users to service authenticated HTTP requests. The picture below shows how the server handles authentication. If auth succeeds, it switches into the user role specified by the request, otherwise it switches into the anonymous role.

.. image:: _static/security-anon-choice.png

Here are the technical details. We use `JSON Web Tokens <http://jwt.io/>`_ to authenticate API requests. As you'll recall a JWT contains a list of cryptographically signed claims. All claims are allowed but PostgREST cares specifically about a claim called role.

.. code:: json

  {
    "role": "user123"
  }

When a request contains a valid JWT with a role claim PostgREST will switch to the database role with that name for the duration of the HTTP request.

.. code:: postgres

  SET LOCAL ROLE user123;

Note that the database administrator must allow the authenticator role to switch into this user by previously executing

.. code:: postgres

  GRANT user123 TO authenticator;

If the client included no JWT (or one without a role claim) then PostgREST switches into the anonymous role whose actual database-specific name, like that of with the authenticator role, is specified in the PostgREST server configuration file. The database administrator must set anonymous role permissions correctly to prevent anonymous users from seeing or changing things they shouldn't.

Custom Validation
-----------------

PostgREST honors the `exp` claim for token expiration, rejecting expired tokens. However it does not enforce any extra constraints. An example of an extra constraint would be to immediately revoke access for a certain user. The configuration file paramter `pre-request` specifies a stored procedure to call immediately after the authenticator switches into a new role and before the main query itself runs.

Here's an example. In the config file specify a stored procedure:

.. code:: ini

  pre-request = "public.check_user"

In the function you can run arbitrary code to check the request and raise an exception to block it if desired.

.. code:: postgres

  CREATE OR REPLACE FUNCTION check_user() RETURNS void
    LANGUAGE plpgsql
    AS $$
  BEGIN
    IF current_role = 'evil_user' THEN
      RAISE EXCEPTION 'No, you are evil'
        USING HINT = 'Stop being so evil and maybe you can log in';
    END IF;
  END
  $$;

Client Auth
===========

To make an authenticated request the client must include an `Authorization` HTTP header with the value `Bearer <jwt>`. For instance:

.. code:: http

  GET /foo
  Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiamRvZSIsImV4cCI6MTQ3NTUxNjI1MH0.GYDZV3yM0gqvuEtJmfpplLBXSGYnke_Pvnl0tbKAjB4

JWT Generation
--------------

You can create a valid JWT either from inside your database or via an external service. Each token is cryptographically signed with a secret passphrase -- the signer and verifier share the secret. Hence any service that shares a passphrase with a PostgREST server can create valid JWT. (PostgREST currently supports only the HMAC-SHA256 signing algorithm.)

JWT from SQL
~~~~~~~~~~~~

You can create JWT tokens in SQL using the `pgjwt extension <https://github.com/michelp/pgjwt>`_. It's simple and requires only pgcrypto. If you're on an environment like Amazon RDS which doesn't support installing new extensions, you can still manually run the SQL inside pgjwt which creates the functions you will need.

Next write a stored procedure that returns the token. The one below returns a token with a hard-coded role, which expires five minutes after it was issued. Note this function has a hard-coded secret as well.

.. code:: postgres

  CREATE TYPE jwt_token AS (
    token text
  );

  CREATE FUNCTION jwt_test() RETURNS public.jwt_token
      LANGUAGE sql
      AS $$
    SELECT jwt.sign(
      row_to_json(r), 'mysecret'
    ) AS token
    FROM (
      SELECT
        'my_role'::text as role,
        extract(epoch from now())::integer + 300 AS exp
    ) r;
  $$;

PostgREST exposes this function to clients via a POST request to `/rpc/jwt_token`.

JWT from Auth0
~~~~~~~~~~~~~~

An external service like `Auth0 <https://auth0.com/>`_ can do the hard work transforming OAuth from Github, Twitter, Google etc into a JWT suitable for PostgREST. Auth0 can also handle email signup and password reset flows.

To adapt Auth0 to our uses we need to save the database role in `user metadata <https://auth0.com/docs/rules/metadata-in-rules>`_ and include the metadata in `private claims <https://auth0.com/docs/jwt#payload>`_ of the generated JWT.

**TODO: add details**

SSL
---

PostgREST aims to do one thing well: add an HTTP interface to a PostgreSQL database. To keep the code small and focused we do not implement SSL. Use a reverse proxy such as NGINX to add this, `here's how <https://nginx.org/en/docs/http/configuring_https_servers.html>`_. Note that some Platforms as a Service like Heroku also add SSL automatically in their load balancer.

Schema Isolation
================

A PostgREST instance is configured to expose all the tables, views, and stored procedures of a single schema specified in a server configuration file. This means private data or implementation details can go inside a private schema and be invisible to HTTP clients. You can then expose views and stored procedures which insulate the internal details from the outside world. It keeps you code easier to refactor, and provides a natural way to do API `versioning`_. For an example of wrapping a private table with a public view see the `Editing User Info`_ section below.

SQL User Management
===================

Storing Users and Passwords
---------------------------

As mentioned, an external service can provide user management and coordinate with the PostgREST server using JWT. It's also possible to support logins entirely through SQL. It's a fair bit of work, so get ready.

The following table, functions, and triggers will live in a `basic_auth` schema that you shouldn't expose publicly in the API. The public views and functions will live in a different schema which internally references this internal information.

First we'll need a table to keep track of our users:

.. code:: postgres

  -- We put things inside the basic_auth schema to hide
  -- them from public view. Certain public procs/views will
  -- refer to helpers and tables inside.
  create schema if not exists basic_auth;

  create table if not exists
  basic_auth.users (
    email    text primary key check ( email ~* '^.+@.+\..+$' ),
    pass     text not null check (length(pass) < 512),
    role     name not null check (length(role) < 512),
    verified boolean not null default false
    -- If you like add more columns, or a json column
  );

We would like the role to be a foreign key to actual database roles, however PostgreSQL does not support these constraints against the `pg_roles` table. We'll use a trigger to manually enforce it.

.. code:: postgres

  create or replace function
  basic_auth.check_role_exists() returns trigger
    language plpgsql
    as $$
  begin
    if not exists (select 1 from pg_roles as r where r.rolname = new.role) then
      raise foreign_key_violation using message =
        'unknown database role: ' || new.role;
      return null;
    end if;
    return new;
  end
  $$;

  drop trigger if exists ensure_user_role_exists on basic_auth.users;
  create constraint trigger ensure_user_role_exists
    after insert or update on basic_auth.users
    for each row
    execute procedure basic_auth.check_role_exists();

Next we'll use the pgcrypto extension and a trigger to keep passwords safe in the `users` table.

.. code:: postgres

  create extension if not exists pgcrypto;

  create or replace function
  basic_auth.encrypt_pass() returns trigger
    language plpgsql
    as $$
  begin
    if tg_op = 'INSERT' or new.pass <> old.pass then
      new.pass = crypt(new.pass, gen_salt('bf'));
    end if;
    return new;
  end
  $$;

  drop trigger if exists encrypt_pass on basic_auth.users;
  create trigger encrypt_pass
    before insert or update on basic_auth.users
    for each row
    execute procedure basic_auth.encrypt_pass();

With the table in place we can make a helper to check a password against the encrypted column. It returns the database role for a user if the email and password are correct.

.. code:: postgres

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

Finally we want a helper function to check whether the database user for the current API request has access to see or change a given role. This will become useful in the next section.

.. code:: postgres

  create or replace function
  basic_auth.clearance_for_role(u name) returns void as
  $$
  declare
    ok boolean;
  begin
    select exists (
      select rolname
        from pg_authid
       where pg_has_role(current_user, oid, 'member')
         and rolname = u
    ) into ok;
    if not ok then
      raise invalid_password using message =
        'current user not member of role ' || u;
    end if;
  end
  $$ LANGUAGE plpgsql;

Public User Interface
---------------------

In the previous section we created an internal place to store user information. Here we create views and functions in a public schema that clients will access through the HTTP API. These public relations allow users view or edit their own information, log in, sign up, etc.

Logins and Signup
~~~~~~~~~~~~~~~~~

As described in `JWT from SQL`_, we'll create a JWT inside our login function. Note that you'll need to adjust the secret key which is hardcoded in this example to a secure secret of your choosing.

.. code:: postgres

  create or replace function
  login(email text, pass text) returns basic_auth.jwt_token
    language plpgsql
    as $$
  declare
    _role name;
    _verified boolean;
    _email text;
    result basic_auth.jwt_claims;
  begin
    -- check email and password
    select basic_auth.user_role(email, pass) into _role;
    if _role is null then
      raise invalid_password using message = 'invalid user or password';
    end if;
    -- check verified flag whether users
    -- have validated their emails
    _email := email;
    select verified from basic_auth.users as u where u.email=_email limit 1 into _verified;
    if not _verified then
      raise invalid_authorization_specification using message = 'user is not verified';
    end if;

    select jwt.sign(
        row_to_json(r), 'mysecret'
      ) as token
      from (
        select _role as role, login.email as email,
           extract(epoch from now())::integer + 60*60 as exp
      ) r
      into result;
    return result;
  end;
  $$;

An API request to call this function would look like:

.. code:: http

  POST /rpc/login

  { "email": "foo@bar.com", "pass": "foobar" }

The response would look like the snippet below. Try decoding the token at `jwt.io <https://jwt.io/>`_. (It was encoded with a secret of `mysecret` as specified in the SQL code above. You'll want to change this secret in your app!)

.. code:: json

  {
    "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImZvb0BiYXIuY29tIiwicm9sZSI6ImF1dGhvciJ9.fpf3_ERi5qbWOE5NPzvauJgvulm0zkIG9xSm2w5zmdw"
  }

Editing User Info
~~~~~~~~~~~~~~~~~

By creating a public wrapper around the internal users table we can allow people to safely edit it through the same auto-generated API that apply to other tables and views. The following view redacts sensitive information. It hides passwords and shows only those users whose roles the currently logged in user has database permission to access.

.. code:: postgres

  create or replace view users as
  select actual.role as role,
         '***'::text as pass,
         actual.email as email,
         actual.verified as verified
  from basic_auth.users as actual,
       (select rolname
          from pg_authid
         where pg_has_role(current_user, oid, 'member')
       ) as member_of
  where actual.role = member_of.rolname;
    -- can also add restriction that current_setting('request.jwt.claim.email')
    -- is equal to email so that user can only see themselves

Using this view a client can see their role and any other users to whose roles the client belongs. This view does not yet support inserts or updates because not all the columns refer directly to underlying columns. Nor do we want it to be auto-updatable because it would allow an escalation of privileges. Someone could update their own row and change their role to become more powerful. We'll handle updates with a trigger:

.. code:: postgres

  create or replace function
  update_users() returns trigger
  language plpgsql
  AS $$
  begin
    if tg_op = 'INSERT' then
      perform basic_auth.clearance_for_role(new.role);

      insert into basic_auth.users
        (role, pass, email, verified)
      values (
        new.role, new.pass, new.email,
        coalesce(new.verified, false));
      return new;
    elsif tg_op = 'UPDATE' then
      -- no need to check clearance for old.role because
      -- an ineligible row would not have been available to update (http 404)
      perform basic_auth.clearance_for_role(new.role);

      update basic_auth.users set
        email  = new.email,
        role   = new.role,
        pass   = new.pass,
        verified = coalesce(new.verified, old.verified, false)
        where email = old.email;
      return new;
    elsif tg_op = 'DELETE' then
      -- no need to check clearance for old.role (see previous case)

      delete from basic_auth.users
       where basic_auth.email = old.email;
      return null;
    end if;
  end
  $$;

  drop trigger if exists update_users on users;
  create trigger update_users
    instead of insert or update or delete on
      users for each row execute procedure update_users();

Permissions
~~~~~~~~~~~

Your database roles need access to the schema, tables, views and functions in order to service HTTP requests. Recall from the `Overview of Role System`_ that PostgREST uses special roles to process requests, namely the authenticator and anonymous roles. Below is an example of permissions that allow anonymous users to create accounts and attempt to log in.

.. code:: postgres

  -- the names "anon" and "authenticator" are configurable and not
  -- sacred, we simply choose them for clarity
  create role anon;
  create role authenticator noinherit;
  grant anon to authenticator;

  grant usage on schema public, basic_auth to anon;

  -- anon can create new logins
  grant insert on table basic_auth.users, basic_auth.tokens to anon;
  grant select on table pg_authid, basic_auth.users to anon;
  grant execute on function
    login(text,text),
    signup(text, text)
    to anon;

You may be worried from the above that anonymous users can read everything from the `basic_auth.users` table. However this table is not available for direct queries because it lives in a separate schema. The anonymous role needs access because the public `users` view reads the underlying table with the permissions of the calling user. But we have made sure the view properly restricts access to sensitive information.

Interacting with Email
----------------------

External actions like sending an email or calling 3rd-party services are possible in PostgREST but must be handled with care. Even if there are PostgreSQL extensions to make network requests it is bad practice to do this in SQL. Blocking on the outside world is unhealthy in a database and holds open long-running transactions. The proper approach is for the database to signal an external program to perform the required action and then not block on the result.

One way to do this is using a table to implement a job queue for external programs. However this approach is `dangerous <https://brandur.org/postgres-queues>`_ because of its potential interactions with unrelated long-running queries. However things are improving with PostgreSQL 9.5 which introduces SKIP LOCKED to build reliable work queues, see `this article <http://blog.2ndquadrant.com/what-is-select-skip-locked-for-in-postgresql-9-5/>`_.

Another way to queue tasks for external processing is by bridging PostgreSQL's `LISTEN <https://www.postgresql.org/docs/9.6/static/sql-listen.html>`_/`NOTIFY <https://www.postgresql.org/docs/9.6/static/sql-notify.html>`_ pubsub with a dedicated external queue system. Two programs to listen for database events and queue them are

* `aweber/pgsql-listen-exchange <https://github.com/aweber/pgsql-listen-exchange>`_ for RabbitMQ
* `SpiderOak/skeeter <https://github.com/SpiderOak/skeeter>`_ for ZeroMQ

For experimentation purposes you can also have external programs LISTEN directly for PostgreSQL events. It's less robust than a queuing system but an example Node program might look like this:

.. code:: js

  var PS = require('pg-pubsub');

  if(process.argv.length !== 3) {
    console.log("USAGE: DB_URL");
    process.exit(2);
  }
  var url  = process.argv[2],
      ps   = new PS(url);

  // password reset request events
  ps.addChannel('reset', console.log);
  // email validation required event
  ps.addChannel('validate', console.log);

  // modify me to send emails

To use this LISTEN/NOTIFY approach (with or without a real queue hooked up) we can make our SQL functions issue a NOTIFY to perform external actions. Two such such functions are those to confirm an email address or send a password reset token. Both will use nonces and need a place to store them, so we'll start there.

.. code:: postgres

  create type token_type_enum as enum ('validation', 'reset');

  create table if not exists
  basic_auth.tokens (
    token       uuid primary key,
    token_type  token_type_enum not null,
    email       text not null references basic_auth.users (email)
                  on delete cascade on update cascade,
    created_at  timestamptz not null default current_date
  );

Here is a password reset function to make public for API requests. The function takes a user email address.

.. code:: postgres

  create or replace function
  request_password_reset(email text) returns void
    language plpgsql
    as $$
  declare
    tok uuid;
  begin
    delete from basic_auth.tokens
     where token_type = 'reset'
       and tokens.email = request_password_reset.email;

    select gen_random_uuid() into tok;
    insert into basic_auth.tokens (token, token_type, email)
           values (tok, 'reset', request_password_reset.email);
    perform pg_notify('reset',
      json_build_object(
        'email', request_password_reset.email,
        'token', tok,
        'token_type', 'reset'
      )::text
    );
  end;
  $$;

Notice the use of `pg_notify` above. It notifies a channel called `reset` with a JSON object containing details of the email address and token. A worker process would directly LISTEN for this event or would pull it off a queue and do the work to send an email with a friendly human readable message.

Similar to the password reset request, an email validation function creates a token and then defers to external processing. This one won't be publicly accessible, but rather can be triggered on user account creation.

.. code:: postgres

  create or replace function
  basic_auth.send_validation() returns trigger
    language plpgsql
    as $$
  declare
    tok uuid;
  begin
    select gen_random_uuid() into tok;
    insert into basic_auth.tokens (token, token_type, email)
           values (tok, 'validation', new.email);
    perform pg_notify('validate',
      json_build_object(
        'email', new.email,
        'token', tok,
        'token_type', 'validation'
      )::text
    );
    return new;
  end
  $$;

  drop trigger if exists send_validation on basic_auth.users;
  create trigger send_validation
    after insert on basic_auth.users
    for each row
    execute procedure basic_auth.send_validation();
