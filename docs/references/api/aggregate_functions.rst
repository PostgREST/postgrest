.. _aggregate_functions:

Aggregate Functions
###################

PostgREST supports the following aggregate functions: ``avg()``, ``count()``, ``max()``, ``min()``, and ``sum()``. Please refer to the `section on aggregate functions in the PostgreSQL documentation <https://www.postgresql.org/docs/current/functions-aggregate.html>`_ for a detailed explanation of these functions.

.. note::
 Aggregate functions are *disabled* by default in PostgREST, as without appropriate safeguards, aggregate functions can create performance problems. See :ref:`db-aggregates-enabled` for further details.

To use an aggregate function, you append the function to a value in the ``select`` parameter, like so:

.. code-block:: bash

  curl "http://localhost:3000/orders?select=amount.sum()"

With the above query, PostgREST will return a single row with a single column named ``sum`` that contains the sum of all the values in the ``amount`` column:

.. code-block:: json

  [
    {
      "sum": 1234.56
    }
  ]

You can use multiple aggregate functions by just adding more columns with aggregate functions to the ``select`` parameter.

To group by other columns, you add those columns to the ``select`` parameter. For instance:

.. code-block:: bash

  curl "http://localhost:3000/orders?select=amount.sum(),amount.avg(),order_date"

This will return a row for each unique value in the ``order_date`` column, with the sum and average of the ``amount`` column for all rows that share the same ``order_date``:

.. code-block:: json

  [
    {
      "sum": 1234.56,
      "avg": 123.45,
      "order_date": "2023-01-01"
    },
    {
      "sum": 2345.67,
      "avg": 234.56,
      "order_date": "2023-01-02"
    }
  ]

.. note::
  Aggregate functions work alongside other PostgREST features, like :ref:`h_filter`, :ref:`json_columns`, and :ref:`ordering`. Please note at this time aggregate functions are not compatible with :ref:`domain_reps`. Additionally, PostgreSQL's ``HAVING`` clause and ordering by aggregated columns are not yet supported.

The Case of ``count()``
===========================

.. note::
  Before the addition of aggregate functions, it was possible to count by adding ``count`` (without parentheses) to the ``select`` parameter. While this is still supported, it may be deprecated in the future, and thus use of this legacy feature is **not recommended.** Please use ``count()`` (with parentheses) instead.


``count()`` is treated specially, as it can be used without an associated column. Take for example the following query:

.. code-block:: bash

  curl "http://localhost:3000/orders?select=count(),order_date"

This would return a row for each unique value in the ``order_date`` column, with the count of all rows that share the same ``order_date``:

.. code-block:: json

  [
    {
      "count": 4,
      "order_date": "2023-01-01"
    },
    {
      "count": 2,
      "order_date": "2023-01-02"
    }
  ]

When ``count()`` is used with an associated column, its behavior is slightly different: It will return the count of all values that are not ``NULL``. This is due to how PostgreSQL itself implements the ``count()`` function.

Renaming and Casting
====================

Renaming Aggregates
-------------------

Just like with other columns, you can rename aggregated columns too. See :ref:`renaming_columns` for details.

Renaming columns is especially helpful in the context of aggregate functions, as by default a column with an aggregate function applied will take on the name of the applied aggregate function. You may want to provide a more semantically meaningful name or prevent collisions when using multiple aggregate functions of the same type.

Casting Aggregates
------------------

When applying an aggregate function to a column, you are able to cast both the value of the input to the aggregate function *and* the value of the output from the aggregate function. In both cases, the syntax works as described in :ref:`casting_columns`, with the only difference being the placement of the cast.

Casting the Value of the Input
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For instance, imagine that the ``orders`` table has a JSON column, ``order_details``, and this column contains a JSON object that has a key, ``tax_amount``. Let's say you want to get the sum of the tax amount for every order. You can use the ``->`` or ``->>`` operators to extract the value with this key (see :ref:`json_columns`), but these operators will return values of the types JSON and ``text`` respectively, and neither of these types can be used with ``sum()``.

Therefore, you will need to first cast the input value to a type that is compatible with ``sum()`` (e.g. ``numeric``). Casting the input value is done in exactly the same way as casting any other value:

.. code-block:: bash

  curl "http://localhost:3000/orders?select=order_details->tax_amount::numeric.sum()"

With this, you will receive the sum of the casted ``tax_amount`` value:

.. code-block:: json

  [
    {
      "sum": 1234.56
    }
  ]

Casting the Value of the Output
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now let's return to an example involving the ``amount`` column of the ``orders`` table. Imagine that we want to get the rounded average of the ``amount`` column. One way to do this is to use the ``avg()`` aggregate function and then to cast the output value of the function to ``int``. To cast the value of the output of the function, we simply place the cast *after* the aggregate function:

.. code-block:: bash

  curl "http://localhost:3000/orders?select=amount.avg()::int"

You will then receive the rounded average as the result:

.. code-block:: json

  [
    {
      "avg": 201
    }
  ]

Of course, you can use both input and output casts at the same time, if you so desire.


Using Aggregate Functions with Resource Embedding
=================================================

Aggregate functions can be used in conjunction with :ref:`resource_embedding`. You can use embedded resources as grouping columns, use aggregate functions within the context of an embedded resource, or use columns from a spreaded resource as grouping columns or as inputs to aggregate functions.

Using Embedded Resources as Grouping Columns
--------------------------------------------

Using an embedded resource as a grouping column allows you to use data from an association to group the results of an aggregation.

For example, imagine that the ``orders`` table from the examples above is related to a ``customers`` table. If you want to get the sum of the ``amount`` column grouped by the ``name`` column from the ``customers`` table, you can include the customer name, using the standard :ref:`resource_embedding` syntax, and perform a sum on the ``amount`` column.

.. code-block:: bash

  curl "http://localhost:3000/orders?select=amount.sum(),customers(name)"

You will then get the summed amount, along with the embedded customer resource:

.. code-block:: json

  [
    {
      "sum": 100,
      "customers": {
        "name": "Customer A"
      }
    },
    {
      "sum": 200,
      "customers": {
        "name": "Customer B"
      }
    }
  ]

.. note::
 The previous example uses a has-one association to demonstrate this functionality, but you may also use has-many associations as grouping columns, although there are few obvious use cases for this.

Using Aggregate Functions Within the Context of an Embedded Resource
--------------------------------------------------------------------

When embedding a resource, you can apply aggregate functions to columns from the associated resource to perform aggregations within the context of an embedded resource.

Continuing with the example relationship between ``orders`` and ``customers`` from the previous section, imagine that you want to fetch the ``name``, ``city``, and ``state`` for each customer, along with the sum of amount of the customer's orders, grouped by the order date. This can be done in the following way:

.. code-block:: bash

  curl "http://localhost:3000/customers?select=name,city,state,orders(amount.sum(),order_date)"

.. code-block:: json

  [
    {
      "name": "Customer A",
      "city": "New York",
      "state": "NY",
      "orders": [
        {
          "sum": 215.22,
          "order_date": "2023-09-01"
        },
        {
          "sum": 905.73,
          "order_date": "2023-09-02"
        }
      ]
    },
    {
      "name": "Customer B",
      "city": "Los Angeles",
      "state": "CA",
      "orders": [
        {
          "sum": 329.71,
          "order_date": "2023-09-01"
        },
        {
          "sum": 425.87,
          "order_date": "2023-09-03"
        }
      ]
    }
  ]

In this example, the ``amount`` column is summed and grouped by the ``order_date`` *within* the context of the embedded resource. That is, the ``name``, ``city``, and ``state`` from the ``customers`` table have no bearing on the aggregation performed in the context of the ``orders`` association; instead, each aggregation can be seen as being performed independently on just the orders belonging to a particular customer, using only the data from the embedded resource for both grouping and aggregation.

Using Columns from a Spreaded Resource
--------------------------------------

When you :ref:`spread an embedded resource <spread_embed>`, the columns from the spreaded resource are treated as if they were columns of the top-level resource, both when using them as grouping columns and when applying aggregate functions to them.

Grouping with Columns from a Spreaded Resource
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For instance, assume you want to sum the ``amount`` column from the ``orders`` table, using the ``city`` and ``state`` columns from the ``customers`` table as grouping columns. To achieve this, you may select these two columns from the ``customers`` table and spread them; they will then be used as grouping columns:

.. code-block:: bash

  curl "http://localhost:3000/orders?select=amount.sum(),...customers(city,state)

The result will be the same as if ``city`` and ``state`` were columns from the ``orders`` table:

.. code-block:: json

  [
    {
      "sum": 2000.29,
      "city": "New York",
      "state": "NY"
    },
    {
      "sum": 9241.21,
      "city": "Los Angeles",
      "state": "CA"
    }
  ]

Aggregate Functions with Columns from a Spreaded Resource
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now imagine that the ``customers`` table has a ``joined_date`` column that represents the date that the customer joined. You want to get both the most recent and the oldest ``joined_date`` for customers that placed an order on every distinct order date. This can be expressed as follows:

.. code-block:: bash

  curl "http://localhost:3000/orders?select=order_date,...customers(joined_date.max(),joined_date.min())

As columns from a spreaded resource are treated as if they were columns from the top-level resource, the ``max()`` and ``min()`` are applied *within* the context of the top-level, rather than within the context of the embedded resource, as in the previous section.

The result will be the same as if the aggregations were applied to columns from the top-level:

.. code-block:: json

  [
    {
      "order_date": "2023-11-01",
      "max": "2023-10-15",
      "min": "2013-10-01"
    },
    {
      "order_date": "2023-11-02",
      "max": "2023-10-30",
      "min": "2016-02-11"
    }
  ]
