.. note::

  This page is a work in progress.

.. _schema_isolation:

Schema Isolation
================

A PostgREST instance exposes all the tables, views, and functions of a single `PostgreSQL schema <https://www.postgresql.org/docs/current/ddl-schemas.html>`_ (a namespace of database objects). This means private data or implementation details can go inside different private schemas and be invisible to HTTP clients.

It is recommended that you don't expose tables on your API schema. Instead expose views and functions which insulate the internal details from the outside world.
This allows you to change the internals of your schema and maintain backwards compatibility. It also keeps your code easier to refactor, and provides a natural way to do API versioning.

.. image:: ../_static/db.png
