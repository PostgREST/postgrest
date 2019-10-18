Embedding a table from another schema
=====================================

:author: `steve-chavez <https://github.com/steve-chavez>`_

Suppose you have a **people** table in the ``public`` schema and this schema is exposed through PostgREST's :ref:`db-schema`.

.. code-block:: postgres

   create table public.people(
     id        int  primary key
   , full_name text
   );

And you want to :ref:`embed <resource_embedding>` the **people** table with a **details** table that's in another schema named ``private``.

.. code-block:: postgres

   create schema if not exists private;

   -- For simplicity's sake the table is devoid of constraints/domains on email, phone, etc.
   create table private.details(
     id         int  primary key references public.people
   , email      text
   , phone      text
   , birthday   date
   , occupation text
   , company    text
   );

   -- other database objects in this schema
   -- ...
   -- ...

To solve this, you can create a view of **details** in the ``public`` schema. We'll call it **public_details**.

.. code-block:: postgres

   create view public.public_details as
   select
       id
     , occupation
     , company
   from
      private.details;

Since PostgREST supports :ref:`embedding_views`, you can embed **people** with **public_details**.

Let's insert some data to test this:

.. code-block:: postgres

   insert into
      public.people
   values
      (1, 'John Doe'), (2, 'Jane Doe');

   insert into
      private.details
   values
      (1, 'jhon@fake.com', '772-323-5433', '1990-02-01', 'Transportation attendant', 'Body Fate'),
      (2, 'jane@fake.com', '480-474-6571', '1980-04-21', 'Geotechnical engineer', 'Earthworks Garden Kare');

.. important::

  Make sure PostgREST's schema cache is up-to-date. See :ref:`schema_reloading`.

Now, make the following request:

.. code-block:: bash

   curl "http://localhost:3000/people?select=full_name,public_details(occupation,company)"

The result should be:

.. code-block:: json

   [
      {"full_name":"John Doe","public_details":[{"occupation":"Transportation attendant","company":"Body Fate"}]},
      {"full_name":"Jane Doe","public_details":[{"occupation":"Geotechnical engineer","company":"Earthworks Garden Kare"}]}
   ]
