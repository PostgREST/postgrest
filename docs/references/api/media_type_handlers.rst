Media Type Handlers
###################

PostgREST offers builtin handlers for common media types such as ``application/json`` and ``text/csv``. You can add handlers for other media types or override the builtin handlers.

Standard Media Types Handlers
=============================

Builtins handlers are provided for the following standard media types:

* ``application/json``
* ``text/csv``
* ``application/geo+json``
* ``application/x-www-form-urlencoded``
* ``*/*``, uses the same handler as ``application/json``.

Vendor Media Types Handlers
===========================

PostgREST also includes its own vendored media types, handlers for these are provided but cannot be overridden.

* ``application/vnd.pgrst.object``
* ``application/vnd.pgrst.array``
* ``application/vnd.pgrst.plan``

Custom Media Type Handlers
==========================

TODO
