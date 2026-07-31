.. _migrate_to_v16:

Migrate to PostgREST v16
========================

To migrate to PostgREST ``v16`` from ``v14``, following changes are required:

.. _changed_syntax_for_jwt_role_extract:

Changed Syntax for JWT Role Extraction
--------------------------------------

The :ref:`jwt-role-claim-key` config should be updated according to the following rules:

* All config values must start with ``$`` character.

  * Example: ``.roles.read`` -> ``$.roles.read``

* Keys with special characters, with the exception of ``_`` char must be quoted.

  * Example: ``.roles.write-role`` -> ``$.roles["write-role"]``

* String comparison operators (``^==``, ``==^`` and ``*==``) are replaced with regular expression search.

  * Example: ``.roles[?(@ ^== "postgrest_test_")]`` -> ``$.roles[?search(@, "^postgrest_test_")]``

* Detailed reference for syntax: `RFC 9535 <https://www.rfc-editor.org/rfc/rfc9535.html#name-jsonpath-syntax-and-semanti>`_.
