Overview of Role System
=======================

PostgREST is designed to keep the database at the center of API security. All authorization happens through database roles and permissions. It is PostgREST's job to authenticate requests -- i.e. verify that a client is who they say they are -- and then let the database authorize client actions.

There are three *types* of roles used by PostgREST, the **authenticator**, **anonymous** and **user** roles. The database administrator creates these roles and configures PostgREST to use them.

.. image:: _static/security-roles.png

The authenticator should be configured in the db to have very limited access. It is a chameleon whose job is to "become" other users to service authenticated HTTP requests. The picture below shows how the server handles authentication. If auth succeeds, it switches into the user role specified by the request, otherwise it switches into the anonymous role.

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

If the client included no JWT (or one without a role claim) then PostgREST switches into the anonymous role whos actual database-specific name, like that of with the authenticator role, is specified in the PostgREST server configuration file.

JSON Web Tokens
===============

Internal Generation
-------------------

External Generation
-------------------

SSL
===

Custom Validation
=================

Schema Isolation
================

User Management
===============

Logins
------

Password Reset
--------------
