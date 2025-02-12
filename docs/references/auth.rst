.. _authn:

Authentication
==============

PostgREST is designed to keep the database at the center of API security. All :ref:`authorization happens in the database <db_authz>` . It is PostgREST's job to **authenticate** requests -- i.e. verify that a client is who they say they are -- and then let the database **authorize** client actions.

.. _roles:

Overview of role system
-----------------------

There are three types of roles used by PostgREST, the **authenticator**, **anonymous** and **user** roles. The database administrator creates these roles and configures PostgREST to use them.

.. image:: ../_static/security-roles.png

The authenticator role is used for connecting to the database and should be configured to have very limited access. It is a chameleon whose job is to "become" other users to service authenticated HTTP requests.


.. code:: sql


  CREATE ROLE authenticator LOGIN NOINHERIT NOCREATEDB NOCREATEROLE NOSUPERUSER;
  CREATE ROLE anonymous NOLOGIN;
  CREATE ROLE webuser NOLOGIN;

.. note::

  The names "authenticator" and "anon" names are configurable and not sacred, we simply choose them for clarity. See :ref:`db-uri` and :ref:`db-anon-role`.

.. _user_impersonation:

User Impersonation
------------------

The picture below shows how the server handles authentication. If auth succeeds, it switches into the user role specified by the request, otherwise it switches into the anonymous role (if it's set in :ref:`db-anon-role`).

.. image:: ../_static/security-anon-choice.png

This role switching mechanism is called **user impersonation**. In PostgreSQL it's done with the ``SET ROLE`` statement.

.. note::

  The impersonated roles will have their settings applied. See :ref:`impersonated_settings`.

.. _jwt_impersonation:

JWT-Based User Impersonation
----------------------------

We use `JSON Web Tokens <https://jwt.io/>`_ to authenticate API requests, this allows us to be stateless and not require database lookups for verification. As you'll recall a JWT contains a list of cryptographically signed claims. All claims are allowed but PostgREST cares specifically about a claim called role.

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
  -- similarly for the anonymous role
  -- GRANT anonymous TO authenticator;

If the client included no JWT (or one without a role claim) then PostgREST switches into the anonymous role. The database administrator must set the anonymous role permissions correctly to prevent anonymous users from seeing or changing things they shouldn't.

.. _jwt_generation:

JWT Generation
~~~~~~~~~~~~~~

You can create a valid JWT either from inside your database (see :ref:`sql_user_management`) or via an external service (see :ref:`external_jwt`).

.. _client_auth:

Client Auth
~~~~~~~~~~~

To make an authenticated request the client must include an :code:`Authorization` HTTP header with the value :code:`Bearer <jwt>`. For instance:

.. code-block:: bash

  curl "http://localhost:3000/foo" \
    -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiamRvZSIsImV4cCI6MTQ3NTUxNjI1MH0.GYDZV3yM0gqvuEtJmfpplLBXSGYnke_Pvnl0tbKAjB4"

The ``Bearer`` header value can be used with or without capitalization(``bearer``).

.. _jwt_caching:

JWT Caching
-----------

PostgREST validates ``JWTs`` on every request. We can cache ``JWTs`` to avoid this performance overhead.

To enable JWT caching, the config :code:`jwt-cache-max-lifetime` is to be set. It is the maximum number of seconds for which the cache stores the JWT validation results. The cache uses the :code:`exp` claim to set the cache entry lifetime. If the JWT does not have an :code:`exp` claim, it uses the config value. See :ref:`jwt-cache-max-lifetime` for more details.

.. note::

  You can use the :ref:`server-timing_header` to see the effect of JWT caching.

Symmetric Keys
~~~~~~~~~~~~~~

Each token is cryptographically signed with a secret key. In the case of symmetric cryptography the signer and verifier share the same secret passphrase, which can be configured with :ref:`jwt-secret`.
If it is set to a simple string value like “reallyreallyreallyreallyverysafe” then PostgREST interprets it as an HMAC-SHA256 passphrase.

.. _asym_keys:

Asymmetric Keys
~~~~~~~~~~~~~~~

In asymmetric cryptography the signer uses the private key and the verifier the public key.

As described in the :ref:`configuration` section, PostgREST accepts a ``jwt-secret`` config file parameter. However you can also specify a literal JSON Web Key (JWK) or set. For example, you can use an RSA-256 public key encoded as a JWK:

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

JWT Claims Validation
~~~~~~~~~~~~~~~~~~~~~

PostgREST honors the following `JWT claims <https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.4>`_:

- ``exp`` Expiration Time
- ``iat`` Issued At
- ``nbf`` Not Before
- ``aud`` Audience, see :ref:`jwt-aud`

.. note::
  PostgREST allows for a 30-second clock skew when validating the ``exp`` and ``iat`` claims. In other words, it gives an extra 30 seconds before the token is rejected if there is a slight discrepancy in the timestamps.

.. _jwt_role_claim_key_extract:

JWT Role Claim Key Extraction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A JSPath DSL that specifies the location of the :code:`role` key in the JWT claims. This can be used to consume a JWT provided by a third party service like Auth0, Okta, Microsoft Entra or Keycloak.

The DSL follows the `JSONPath <https://goessner.net/articles/JsonPath/>`_ expression grammar with extended string comparison operators. Supported operators are:

- ``==`` selects the first array element that exactly matches the right operand
- ``!=`` selects the first array element that does not match the right operand
- ``^==`` selects the first array element that starts with the right operand
- ``==^`` selects the first array element that ends with the right operand
- ``*==`` selects the first array element that contains the right operand

Usage examples:

  .. code:: bash

    # {"postgrest":{"roles": ["other", "author"]}}
    # the DSL accepts characters that are alphanumerical or one of "_$@" as keys
    jwt-role-claim-key = ".postgrest.roles[1]"

    # {"https://www.example.com/role": { "key": "author" }}
    # non-alphanumerical characters can go inside quotes(escaped in the config value)
    jwt-role-claim-key = ".\"https://www.example.com/role\".key"

    # {"postgrest":{"roles": ["other", "author"]}}
    # `@` represents the current element in the array
    # all the these match the string "author"
    jwt-role-claim-key = ".postgrest.roles[?(@ == \"author\")]"
    jwt-role-claim-key = ".postgrest.roles[?(@ != \"other\")]"
    jwt-role-claim-key = ".postgrest.roles[?(@ ^== \"aut\")]"
    jwt-role-claim-key = ".postgrest.roles[?(@ ==^ \"hor\")]"
    jwt-role-claim-key = ".postgrest.roles[?(@ *== \"utho\")]"


JWT Security
~~~~~~~~~~~~

There are at least three types of common critiques against using JWT: 1) against the standard itself, 2) against using libraries with known security vulnerabilities, and 3) against using JWT for web sessions. We'll briefly explain each critique, how PostgREST deals with it, and give recommendations for appropriate user action.

The critique against the `JWT standard <https://datatracker.ietf.org/doc/html/rfc7519>`_ is voiced in detail `elsewhere on the web <https://web.archive.org/web/20230123041631/https://paragonie.com/blog/2017/03/jwt-json-web-tokens-is-bad-standard-that-everyone-should-avoid>`_. The most relevant part for PostgREST is the so-called :code:`alg=none` issue. Some servers implementing JWT allow clients to choose the algorithm used to sign the JWT. In this case, an attacker could set the algorithm to :code:`none`, remove the need for any signature at all and gain unauthorized access. The current implementation of PostgREST, however, does not allow clients to set the signature algorithm in the HTTP request, making this attack irrelevant. The critique against the standard is that it requires the implementation of the :code:`alg=none` at all.

Another type of critique focuses on the misuse of JWT for maintaining web sessions. The basic recommendation is to `stop using JWT for sessions <http://cryto.net/~joepie91/blog/2016/06/13/stop-using-jwt-for-sessions/>`_ because most, if not all, solutions to the problems that arise when you do, `do not work <http://cryto.net/~joepie91/blog/2016/06/19/stop-using-jwt-for-sessions-part-2-why-your-solution-doesnt-work/>`_. The linked articles discuss the problems in depth but the essence of the problem is that JWT is not designed to be secure and stateful units for client-side storage and therefore not suited to session management.

PostgREST uses JWT mainly for authentication and authorization purposes and encourages users to do the same. For web sessions, using cookies over HTTPS is good enough and well catered for by standard web frameworks.

.. _custom_validation:

Custom Validation
-----------------

PostgREST does not enforce any extra constraints besides JWT validation. An example of an extra constraint would be to immediately revoke access for a certain user. Using :ref:`db-pre-request` you can specify a function to call immediately after :ref:`user_impersonation` and before the main query itself runs.

.. code:: ini

  db-pre-request = "public.check_user"

In the function you can run arbitrary code to check the request and raise an exception(see :ref:`raise_error`) to block it if desired. Here you can take advantage of :ref:`guc_req_headers_cookies_claims` for
doing custom logic based on the web user info.

.. code-block:: postgres

  CREATE OR REPLACE FUNCTION check_user() RETURNS void AS $$
  DECLARE
    email text := current_setting('request.jwt.claims', true)::json->>'email';
  BEGIN
    IF email = 'evil.user@malicious.com' THEN
      RAISE EXCEPTION 'No, you are evil'
        USING HINT = 'Stop being so evil and maybe you can log in';
    END IF;
  END
  $$ LANGUAGE plpgsql;
