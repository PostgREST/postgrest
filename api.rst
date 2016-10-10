Tables and Views
================

All views and tables in the active schema and accessible by the active database role for a request are available for querying. They are exposed in one-level deep routes. For instance the full contents of a table `people` is returned at

.. code-block:: HTTP

  GET /people

There are no deeply/nested/routes. Each route provides OPTIONS, GET, POST, PATCH, and DELETE verbs depending entirely on database permissions.

.. note::

  Why not provide nested routes? Many APIs allow nesting to retrieve related information, such as :code:`/films/1/director`. We offer a more flexible mechanism (inspired by GraphQL) to embed related information. It can handle one-to-many and many-to-many relationships. This is covered in the section about Embedding.

Filtering
---------

You can filter result rows by adding conditions on columns, each condition a query string parameter. For instance, to return people aged under 13 years old:

.. code-block:: http

  GET /people?age=lt.13

Adding multiple parameters conjoins the conditions:

.. code-block:: http

  GET /people?age=gte.18&student=is.true

These operators are available:

============  =============================================
abbreviation  meaning
============  =============================================
eq            equals
gte           greater than or equal
gt            greater than
lte           less than or equal
lt            less than
neq           not equal
like          LIKE operator (use * in place of %)
ilike         ILIKE operator (use * in place of %)
in            one of a list of values e.g. :code:`?a=in.1,2,3`
is            checking for exact equality (null,true,false)
@@            full-text search using to_tsquery
@>            contains e.g. :code:`?tags=@>.{example, new}`
<@            contained in e.g. :code:`?values=<@{1,2,3}`
not           negates another operator, see below
============  =============================================


To negate any operator, prefix it with :code:`not` like :code:`?a=not.eq.2`.

For more complicated filters (such as those involving condition 1 OR condition 2) you will have to create a new view in the database.

.. _computed_cols:

Computed Columns
~~~~~~~~~~~~~~~~

Filters may be applied to computed columns as well as actual table/view columns, even though the computed columns will not appear in the output. For example, to search first and last names at once we can create a computed column that will not appear in the output but can be used in a filter:

.. code-block:: sql

  CREATE TABLE people (
    fname text,
    lname text
  );

  CREATE FUNCTION full_name(people) RETURNS text AS $$
    SELECT $1.fname || ' ' || $1.lname;
  $$ LANGUAGE SQL;

  # (optional) add an index to speed up anticipated query
  CREATE INDEX people_full_name_idx ON people
    USING GIN (to_tsvector('english', fname || ' ' || lname));

A full-text search on the computed column:

.. code-block:: http

  GET /people?full_name=@@.Beckett

Ordering
--------

The reserved word :code:`order` reorders the response rows. It uses a comma-separated list of columns and directions:

.. code-block:: http

  GET /people?order=age.desc,height.asc

If no direction is specified it defaults to ascending order:

.. code-block:: http

  GET /people?order=age

If you care where nulls are sorted, add nullsfirst or nullslast:

.. code-block:: http

  GET /people?order=age.nullsfirst
  GET /people?order=age.desc.nullslast

To order the embedded items, you need to specify the tree path for the order param like so.

.. code-block:: http

  GET /projects?select=id,name,tasks{id,name}&order=id.asc&tasks.order=name.asc

You can also use :ref:`computed_cols` to order the results, even though the computed columns will not appear in the output.

Limits and Pagination
---------------------

Counting
--------

Response Format
---------------

Singular or Plural
------------------

OpenAPI Support
===============

Resource Embedding
==================

Query Limitations
=================

Stored Procedures
=================

Insertions / Updates
====================

Getting Results
---------------

Bulk Insert
-----------

Deletions
=========
