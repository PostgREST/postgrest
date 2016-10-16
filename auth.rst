Overview of Role System
=======================

PostgREST is designed to keep the database at the center of API security. All authorization happens through database roles and permissions. It is PostgREST's job to authenticate requests -- i.e. verify that a client is who they say they are -- and then let the database authorize client actions.

There are three types of roles used by PostgREST, the **authenticator**, **anonymous** and **user** roles. The database administrator creates these roles and configures PostgREST to use them.

.. image:: _static/security-roles.png

The authenticator should be configured in the database to have very limited access. It is a chameleon whose job is to "become" other users to service authenticated HTTP requests. The picture below shows how the server handles authentication. If auth succeeds, it switches into the user role specified by the request, otherwise it switches into the anonymous role.

.. image:: _static/security-anon-choice.png

Here are the technical details. We use `JSON Web Tokens <http://jwt.io/>`_ to authenticate API requests. As you'll recall a JWT contains a list of cryptographically signed claims. All claims are allowed but PostgREST cares specifically about a claim called role.

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

If the client included no JWT (or one without a role claim) then PostgREST switches into the anonymous role whos actual database-specific name, like that of with the authenticator role, is specified in the PostgREST server configuration file. The database administrator must set anonymous role permissions correctly to prevent anonymous users from seeing or changing things they shouldn't.

Custom Authentication
---------------------

PostgREST honors the `exp` claim for token expiration, rejecting expired tokens. However it does not enforce any extra constraints. An example of an extra constraint would be to immediately revoke access for a certain user. The configuration file paramter `pre-request` specifies a stored procedure to call immediately after the authenticator switches into a new role and before the main query itself runs.

Here's an example. In the config file specify a stored procedure:

.. code::

  pre-request = "public.check_user"

In the function you can run arbitrary code to check the request and raise an exception to block it if desired.

.. code:: sql

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
==============

You can create a valid JWT either from inside your database or via an external service. Each token is cryptographically signed with a secret passphrase -- the signer and verifier share the secret. Hence any service that shares a passphrase with a PostgREST server can create valid JWT. (PostgREST currently supports only the HMAC-SHA256 signing algorithm.)

From SQL
--------

You can create JWT tokens in SQL using the `pgjwt extension <https://github.com/michelp/pgjwt>`_. It's simple and requires only pgcrypto. If you're on an environment like Amazon RDS which doesn't support installing new extensions, you can still manually run the SQL inside pgjwt which creates the functions you will need.

Next write a stored procedure that returns the token. The one below returns a token with a hard-coded role, which expires five minutes after it was issued. Note this function has a hard-coded secret as well.

.. code:: sql

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

Using Auth0
-----------

An external service like `Auth0 <https://auth0.com/>`_ can do the hard work transforming Github, Twitter, Google etc OAuth into a JWT suitable for PostgREST. Auth0 can also handle email signup and password reset flows.

To adapt Auth0 to our uses we need to save the database role in `user metadata <https://auth0.com/docs/rules/metadata-in-rules>`_ and include the metadata in `private claims <https://auth0.com/docs/jwt#payload>`_ of the generated JWT.

**TODO: add details**

SSL
===

PostgREST aims to do one thing well: add an HTTP interface to a PostgreSQL database. To keep the code small and focused we do not implement SSL. Use a reverse proxy such as NGINX to add this, `here's how <https://nginx.org/en/docs/http/configuring_https_servers.html>`_. Note that some Platforms as a Service like Heroku also add SSL automatically in their load balancer.

Schema Isolation
================

User Management
===============

Logins
------

Password Reset
--------------
