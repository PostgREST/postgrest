.. _resource_embedding:

Resource Embedding
==================

PostgREST allows including related resources in a single API call. This reduces the need for many API requests.

**Foreign Keys** determine which tables and views can be returned together. For example, consider a database of films and their awards:

.. image:: ../../_static/film.png

.. important::

  Whenever foreign keys change you must do :ref:`schema_reloading` for this feature to work.

.. _many-to-one:

Many-to-one relationships
-------------------------

Since ``films`` has a **foreign key** to ``directors``, this establishes a many-to-one relationship. Thus, we're able to request all the films and the director for each film.

.. tabs::

  .. code-tab:: http

    GET /films?select=title,directors(id,last_name) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=title,directors(id,last_name)"

.. code-block:: json

  [
    { "title": "Workers Leaving The Lumière Factory In Lyon",
      "directors": {
        "id": 2,
        "last_name": "Lumière"
      }
    },
    { "title": "The Dickson Experimental Sound Film",
      "directors": {
        "id": 1,
        "last_name": "Dickson"
      }
    },
    { "title": "The Haunted Castle",
      "directors": {
        "id": 3,
        "last_name": "Méliès"
      }
    }
  ]

Note that the embedded ``directors`` is returned as a JSON object because of the "to-one" end.

Since the table name is plural, we can be more accurate by making it singular with an alias.

.. tabs::

  .. code-tab:: http

    GET /films?select=title,director:directors(id,last_name) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=title,director:directors(id,last_name)"

.. code-block:: json

  [
    { "title": "Workers Leaving The Lumière Factory In Lyon",
      "director": {
        "id": 2,
        "last_name": "Lumière"
      }
    },
    ".."
  ]

.. _one-to-many:

One-to-many relationships
-------------------------

The **foreign key reference** establishes the inverse one-to-many relationship. In this case, ``films`` returns as a JSON array because of the “to-many” end.

.. tabs::

  .. code-tab:: http

    GET /directors?select=last_name,films(title) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/directors?select=last_name,films(title)"

.. code-block:: json

  [
    { "last_name": "Lumière",
      "films": [
        {"title": "Workers Leaving The Lumière Factory In Lyon"}
      ]
    },
    { "last_name": "Dickson",
      "films": [
        {"title": "The Dickson Experimental Sound Film"}
      ]
    },
    { "last_name": "Méliès",
      "films": [
        {"title": "The Haunted Castle"}
      ]
    }
  ]

.. _many-to-many:

Many-to-many relationships
--------------------------

The join table determines many-to-many relationships. It must contain foreign keys to other two tables and they must be part of its composite key.

For the many-to-many relationship between ``films`` and ``actors``, the join table ``roles`` is:

.. code-block:: postgresql

  create table roles(
    film_id int references films(id)
  , actor_id int references actors(id)
  , primary key(film_id, actor_id)
  );

  -- the join table can also be detected if the composite key has additional columns

  create table roles(
    id int generated always as identity,
  , film_id int references films(id)
  , actor_id int references actors(id)
  , primary key(id, film_id, actor_id)
  );

.. tabs::

  .. code-tab:: http

    GET /actors?select=first_name,last_name,films(title) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/actors?select=first_name,last_name,films(title)"

.. code-block:: json

  [
    { "first_name": "Willem",
      "last_name": "Dafoe",
      "films": [
        {"title": "The Lighthouse"}
      ]
    },
    ".."
  ]

.. _one-to-one:

One-to-one relationships
------------------------

One-to-one relationships are detected when:

- The foreign key has a unique constraint.

.. code-block:: postgresql

  CREATE TABLE technical_specs(
    film_id INT REFERENCES films UNIQUE,
    runtime TIME,
    camera TEXT,
    sound TEXT
  );

- The foreign key is a primary key.

.. code-block:: postgresql

  -- references Films using the primary key as a foreign key
  CREATE TABLE technical_specs(
    film_id INT PRIMARY KEY REFERENCES films,
    runtime TIME,
    camera TEXT,
    sound TEXT
  );

.. tabs::

  .. code-tab:: http

    GET /films?select=title,technical_specs(runtime) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=title,technical_specs(runtime)"

.. code-block:: json

  [
    {
      "title": "Pulp Fiction",
      "technical_specs": {"camera": "Arriflex 35-III"}
    },
    ".."
  ]

.. _computed_relationships:

Computed relationships
----------------------

You can manually define relationships between resources. This is useful for database objects that can't define foreign keys, like `Foreign Data Wrappers <https://wiki.postgresql.org/wiki/Foreign_data_wrappers>`_.

To do this, you can create functions similar to :ref:`computed_cols`.

Assuming there's a foreign table ``premieres`` that we want to relate to ``films``.

.. code-block:: postgres

  create foreign table premieres (
    id integer,
    location text,
    "date" date,
    film_id integer
  ) server import_csv options ( filename '/tmp/directors.csv', format 'csv');

  create function film(premieres) returns setof films rows 1 as $$
    select * from films where id = $1.film_id
  $$ stable language sql;

The above function defines a relationship between ``premieres`` (the parameter) and ``films`` (the return type). Since there's a ``rows 1``, this defines a many-to-one relationship.
The name of the function ``film`` is arbitrary and can be used to do the embedding:

.. tabs::

  .. code-tab:: http

    GET /premieres?select=location,film(name) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/premieres?select=location,film(name)"

.. code-block:: json

  [
    {
      "location": "Cannes Film Festival",
      "film": {"name": "Pulp Fiction"}
    },
    ".."
  ]

Now let's define the opposite one-to-many relationship.

.. code-block:: postgres

  create function premieres(films) returns setof premieres as $$
    select * from premieres where film_id = $1.id
  $$ stable language sql;

In this case there's an implicit ``ROWS 1000`` defined by PostgreSQL(`search "result_rows" on this PostgreSQL doc <https://www.postgresql.org/docs/current/sql-createfunction.html>`_).
We consider any value greater than 1 as "many" so this defines a one-to-many relationship.

.. tabs::

  .. code-tab:: http

    GET /films?select=name,premieres(name) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=name,premieres(name)"

.. code-block:: json

  [
    {
      "name": "Pulp Ficiton",
      "premieres": [{"location": "Cannes Festival"}]
    },
    ".."
  ]

Computed relationships also allow you to override the ones that PostgREST auto-detects.

For example, to override the :ref:`many-to-one relationship <many-to-one>` between ``films`` and ``directors``.

.. code-block:: postgres

  create function directors(films) returns setof directors rows 1 as $$
    select * from directors where id = $1.director_id
  $$ stable language sql;

Thanks to overloaded functions, you can use the same function name for different parameters. Thus define relationships from other tables/views to directors.

.. code-block:: postgres

  create function directors(film_schools) returns setof directors as $$
    select * from directors where film_school_id = $1.id
  $$ stable language sql;

Computed relationships have good performance as their intended design enable `inlining <https://wiki.postgresql.org/wiki/Inlining_of_SQL_functions#Inlining_conditions_for_table_functions>`_.

.. warning::

  - Always use ``SETOF`` when creating computed relationships. Functions can return a table without using ``SETOF``, but bear in mind that they will not be inlined.

  - Make sure to correctly label the ``to-one`` part of the relationship. When using the ``ROWS 1`` estimation, PostgREST will expect a single row to be returned. If that is not the case, it will unnest the embedding and return repeated values for the top level resource.

.. _nested_embedding:

Nested Embedding
----------------

If you want to embed through join tables but need more control on the intermediate resources, you can do nested embedding. For instance, you can request the Actors, their Roles and the Films for those Roles:

.. tabs::

  .. code-tab:: http

    GET /actors?select=roles(character,films(title,year)) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/actors?select=roles(character,films(title,year))"

.. _embed_filters:

Embedded Filters
----------------

Embedded resources can be shaped similarly to their top-level counterparts. To do so, prefix the query parameters with the name of the embedded resource. For instance, to order the actors in each film:

.. tabs::

  .. code-tab:: http

    GET /films?select=*,actors(*)&actors.order=last_name,first_name HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=*,actors(*)&actors.order=last_name,first_name"

This sorts the list of actors in each film but does *not* change the order of the films themselves. To filter the roles returned with each film:

.. tabs::

  .. code-tab:: http

    GET /films?select=*,roles(*)&roles.character=in.(Chico,Harpo,Groucho) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=*,roles(*)&roles.character=in.(Chico,Harpo,Groucho)"

Once again, this restricts the roles included to certain characters but does not filter the films in any way. Films without any of those characters would be included along with empty character lists.

An ``or`` filter  can be used for a similar operation:

.. tabs::

  .. code-tab:: http

    GET /films?select=*,roles(*)&roles.or=(character.eq.Gummo,character.eq.Zeppo) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=*,roles(*)&roles.or=(character.eq.Gummo,character.eq.Zeppo)"

Limit and offset operations are possible:

.. tabs::

  .. code-tab:: http

    GET /films?select=*,actors(*)&actors.limit=10&actors.offset=2 HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=*,actors(*)&actors.limit=10&actors.offset=2"

Embedded resources can be aliased and filters can be applied on these aliases:

.. tabs::

  .. code-tab:: http

    GET /films?select=*,90_comps:competitions(name),91_comps:competitions(name)&90_comps.year=eq.1990&91_comps.year=eq.1991 HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=*,90_comps:competitions(name),91_comps:competitions(name)&90_comps.year=eq.1990&91_comps.year=eq.1991"

Filters can also be applied on nested embedded resources:

.. tabs::

  .. code-tab:: http

    GET /films?select=*,roles(*,actors(*))&roles.actors.order=last_name&roles.actors.first_name=like.*Tom* HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=*,roles(*,actors(*))&roles.actors.order=last_name&roles.actors.first_name=like.*Tom*"

The result will show the nested actors named Tom and order them by last name. Aliases can also be used instead of the resource names to filter the nested tables.

.. _embedding_top_level_filter:

Top-level Filtering
-------------------

By default, :ref:`embed_filters` don't change the top-level resource(``films``) rows at all:

.. tabs::

  .. code-tab:: http

    GET /films?select=title,actors(first_name,last_name)&actors.first_name=eq.Jehanne HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=title,actors(first_name,last_name)&actors.first_name=eq.Jehanne

.. code-block:: json

  [
    {
      "title": "Workers Leaving The Lumière Factory In Lyon",
      "actors": []
    },
    {
      "title": "The Dickson Experimental Sound Film",
      "actors": []
    },
    {
      "title": "The Haunted Castle",
      "actors": [
        {
          "first_name": "Jehanne",
          "last_name": "d'Alcy"
        }
      ]
    }
  ]

In order to filter the top level rows you need to add ``!inner`` to the embedded resource. For instance, to get **only** the films that have an actor named ``Jehanne``:

.. tabs::

  .. code-tab:: http

    GET /films?select=title,actors!inner(first_name,last_name)&actors.first_name=eq.Jehanne HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=title,actors!inner(first_name,last_name)&actors.first_name=eq.Jehanne"

.. code-block:: json

  [
    {
      "title": "The Haunted Castle",
      "actors": [
        {
          "first_name": "Jehanne",
          "last_name": "d'Alcy"
        }
      ]
    }
  ]

.. _null_embed:

Null filtering on Embedded Resources
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Null filtering on the embedded resources can behave the same as ``!inner``. While providing more flexibility.

For example, doing ``actors=not.is.null`` returns the same result as ``actors!inner(*)``:

.. tabs::

  .. code-tab:: http

    GET /films?select=title,actors(*)&actors=not.is.null HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=title,actors(*)&actors=not.is.null"

The ``is.null`` filter can be used in embedded resources to perform an anti-join. To get all the films that do not have any nominations:

.. tabs::

  .. code-tab:: http

    GET /films?select=title,nominations()&nominations=is.null HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=title,nominations()&nominations=is.null"


Both ``is.null`` and ``not.is.null`` can be included inside the `or` operator. For instance, to get the films that have no actors **or** directors registered yet:

.. tabs::

  .. code-tab:: http

    GET /films?select=title,actors(*),directors(*)&or=(actors.is.null,directors.is.null) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=title,actors(*),directors(*)&or=(actors.is.null,directors.is.null)"

.. _empty_embed:

Empty Embed
~~~~~~~~~~~

You can leave an embedded resource empty, this helps with filtering in some cases.

To filter the films by actors but not include them:

.. tabs::

  .. code-tab:: http

    GET /films?select=title,actors()&actors.first_name=eq.Jehanne&actors=not.is.null HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=title,actors()&actors.first_name=eq.Jehanne&actors=not.is.null"

.. code-block:: json

  [
    {
      "title": "The Haunted Castle",
    }
  ]

.. _top_level_order:

Top-level Ordering
------------------

On :ref:`Many-to-One <many-to-one>` and :ref:`One-to-One <one-to-one>` relationships, you can use a column of the "to-one" end to sort the top-level.

For example, to arrange the films in descending order using the director's last name.

.. tabs::

  .. code-tab:: http

    GET /films?select=title,directors(last_name)&order=directors(last_name).desc HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=title,directors(last_name)&order=directors(last_name).desc"

.. _spread_embed:

Spread embedded resource
------------------------

On many-to-one and one-to-one relationships, you can "spread" the embedded resource. That is, remove the surrounding JSON object for the embedded resource columns.

.. tabs::

  .. code-tab:: http

     GET /films?select=title,...directors(director_last_name:last_name)&title=like.*Workers* HTTP/1.1

  .. code-tab:: bash Curl

     curl "http://localhost:3000/films?select=title,...directors(director_last_name:last_name)&title=like.*Workers*"

.. code-block:: json

   [
     {
       "title": "Workers Leaving The Lumière Factory In Lyon",
       "director_last_name": "Lumière"
     }
   ]

Note that there is no ``"directors"`` object. Also the embed columns can be aliased normally.

You can use this to get the columns of a join table in a many-to-many relationship. For instance, to get films and its actors, but including the ``character`` column from the roles table:

.. tabs::

  .. code-tab:: http

     GET /films?select=title,actors:roles(character,...actors(first_name,last_name))&title=like.*Lighthouse* HTTP/1.1

  .. code-tab:: bash Curl

     curl "http://localhost:3000/films?select=title,actors:roles(character,...actors(first_name,last_name))&title=like.*Lighthouse*"

.. code-block:: json

   [
     {
       "title": "The Lighthouse",
       "actors": [
          {
            "character": "Thomas Wake",
            "first_name": "Willem",
            "last_name": "Dafoe"
          }
       ]
     }
   ]

.. note::

  The spread operator ``...`` is borrowed from the Javascript `spread syntax <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax>`_.

.. _embedding_partitioned_tables:

Embedding Partitioned Tables
----------------------------

Embedding can also be done between `partitioned tables <https://www.postgresql.org/docs/current/ddl-partitioning.html>`_ and other tables.

For example, let's create the ``box_office`` partitioned table that has the gross daily revenue of a film:

.. code-block:: postgres

  CREATE TABLE box_office (
    bo_date DATE NOT NULL,
    film_id INT REFERENCES test.films NOT NULL,
    gross_revenue DECIMAL(12,2) NOT NULL,
    PRIMARY KEY (bo_date, film_id)
  ) PARTITION BY RANGE (bo_date);

  -- Let's also create partitions for each month of 2021

  CREATE TABLE box_office_2021_01 PARTITION OF test.box_office
  FOR VALUES FROM ('2021-01-01') TO ('2021-01-31');

  CREATE TABLE box_office_2021_02 PARTITION OF test.box_office
  FOR VALUES FROM ('2021-02-01') TO ('2021-02-28');

  -- and so until december 2021

Since it contains the ``films_id`` foreign key, it is possible to embed ``box_office`` and ``films``:

.. tabs::

  .. code-tab:: http

    GET /box_office?select=bo_date,gross_revenue,films(title)&gross_revenue=gte.1000000 HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/box_office?select=bo_date,gross_revenue,films(title)&gross_revenue=gte.1000000"

.. note::

  * Embedding on partitions is not allowed because it leads to ambiguity errors (see :ref:`embed_disamb`) between them and their parent partitioned table. More details at `#1783(comment) <https://github.com/PostgREST/postgrest/issues/1783#issuecomment-959823827>`_). :ref:`custom_queries` can be used if this is needed.

  * Partitioned tables can reference other tables since PostgreSQL 11 but can only be referenced from any other table since PostgreSQL 12.

.. _embedding_views:

Embedding Views
---------------

PostgREST will infer the relationships of a view based on its source tables. Source tables are the ones referenced in the ``FROM`` and ``JOIN`` clauses of the view definition. The foreign keys of the relationships must be present in the top ``SELECT`` clause of the view for this to work.

For instance, the following view has ``nominations``, ``films`` and ``competitions`` as source tables:

.. code-block:: postgres

  CREATE VIEW nominations_view AS
    SELECT
       films.title as film_title
     , competitions.name as competition_name
     , nominations.rank
     , nominations.film_id as nominations_film_id
     , films.id as film_id
    FROM nominations
    JOIN films ON films.id = nominations.film_id
    JOIN competitions ON competitions.id = nominations.competition_id;

Since this view contains ``nominations.film_id``, which has a **foreign key** relationship to ``films``, then we can embed the ``films`` table. Similarly, because the view contains ``films.id``, then we can also embed the ``roles`` and the ``actors`` tables (the last one in a many-to-many relationship):

.. tabs::

  .. code-tab:: http

    GET /nominations_view?select=film_title,films(language),roles(character),actors(last_name,first_name)&rank=eq.5 HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/nominations_view?select=film_title,films(language),roles(character),actors(last_name,first_name)&rank=eq.5"

It's also possible to embed `Materialized Views <https://www.postgresql.org/docs/current/rules-materializedviews.html>`_.

.. important::

  - It's not guaranteed that all kinds of views will be embeddable. In particular, views that contain UNIONs will not be made embeddable.

    + Why? PostgREST detects source table foreign keys in the view by querying and parsing `pg_rewrite <https://www.postgresql.org/docs/current/catalog-pg-rewrite.html>`_.
      This may fail depending on the complexity of the view.
    + As a workaround, you can use :ref:`computed_relationships` to define manual relationships for views.

  - If view definitions change you must refresh PostgREST's schema cache for this to work properly. See the section :ref:`schema_reloading`.

.. _embedding_view_chains:

Embedding Chains of Views
-------------------------

Views can also depend on other views, which in turn depend on the actual source table. For PostgREST to pick up those chains recursively to any depth, all the views must be in the search path, so either in the exposed schema (:ref:`db-schemas`) or in one of the schemas set in :ref:`db-extra-search-path`. This does not apply to the source table, which could be in a private schema as well. See :ref:`schema_isolation` for more details.

.. _s_proc_embed:

Embedding on Stored Procedures
------------------------------

If you have a :ref:`Stored Procedure <s_procs>` that returns a table type, you can embed its related resources.

Here's a sample function (notice the ``RETURNS SETOF films``).

.. code-block:: plpgsql

  CREATE FUNCTION getallfilms() RETURNS SETOF films AS $$
    SELECT * FROM films;
  $$ LANGUAGE SQL IMMUTABLE;

A request with ``directors`` embedded:

.. tabs::

  .. code-tab:: http

     GET /rpc/getallfilms?select=title,directors(id,last_name)&title=like.*Workers* HTTP/1.1

  .. code-tab:: bash Curl

     curl "http://localhost:3000/rpc/getallfilms?select=title,directors(id,last_name)&title=like.*Workers*"

.. code-block:: json

   [
     { "title": "Workers Leaving The Lumière Factory In Lyon",
       "directors": {
         "id": 2,
         "last_name": "Lumière"
       }
     }
   ]

.. _mutation_embed:

Embedding after Insertions/Updates/Deletions
--------------------------------------------

You can embed related resources after doing :ref:`insert`, :ref:`update` or :ref:`delete`.

Say you want to insert a **film** and then get some of its attributes plus embed its **director**.

.. tabs::

  .. code-tab:: http

     POST /films?select=title,year,director:directors(first_name,last_name) HTTP/1.1
     Prefer: return=representation

     {
      "id": 100,
      "director_id": 40,
      "title": "127 hours",
      "year": 2010,
      "rating": 7.6,
      "language": "english"
     }

  .. code-tab:: bash Curl

    curl "http://localhost:3000/films?select=title,year,director:directors(first_name,last_name)" \
      -H "Prefer: return=representation" \
      -d @- << EOF
      {
        "id": 100,
        "director_id": 40,
        "title": "127 hours",
        "year": 2010,
        "rating": 7.6,
        "language": "english"
      }
    EOF

Response:

.. code-block:: json

   {
    "title": "127 hours",
    "year": 2010,
    "director": {
      "first_name": "Danny",
      "last_name": "Boyle"
    }
   }

.. _embed_disamb:

Embedding Disambiguation
------------------------

For doing resource embedding, PostgREST infers the relationship between two tables based on a foreign key between them.
However, in cases where there's more than one foreign key between two tables, it's not possible to infer the relationship unambiguously
by just specifying the tables names.

.. _target_disamb:

Target Disambiguation
~~~~~~~~~~~~~~~~~~~~~

For example, suppose you have the following ``orders`` and ``addresses`` tables:

.. image:: ../../_static/orders.png

And you try to embed ``orders`` with ``addresses`` (this is the **target**):

.. tabs::

  .. code-tab:: http

    GET /orders?select=*,addresses(*) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/orders?select=*,addresses(*)" -i

Since the ``orders`` table has two foreign keys to the ``addresses`` table — an order has a billing address and a shipping address —
the request is ambiguous and PostgREST will respond with an error:

.. code-block:: http

   HTTP/1.1 300 Multiple Choices

   {..}

If this happens, you need to disambiguate the request by adding precision to the **target**.
Instead of the **table name**, you can specify the **foreign key constraint name** or the **column name** that is part of the foreign key.

Let's try first with the **foreign key constraint name**. To make it clearer we can name it:

.. code-block:: postgresql

   ALTER TABLE orders
      ADD CONSTRAINT billing_address  foreign key (billing_address_id) references addresses(id),
      ADD CONSTRAINT shipping_address foreign key (shipping_address_id) references addresses(id);

   -- Or if the constraints names were already generated by PostgreSQL we can rename them
   -- ALTER TABLE orders
   --   RENAME CONSTRAINT orders_billing_address_id_fkey  TO billing_address,
   --   RENAME CONSTRAINT orders_shipping_address_id_fkey TO shipping_address;

Now we can unambiguously embed the billing address by specifying the ``billing_address`` foreign key constraint as the **target**.

.. tabs::

  .. code-tab:: http

    GET /orders?select=name,billing_address(name) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/orders?select=name,billing_address(name)"

.. code-block:: json

   [
    {
     "name": "Personal Water Filter",
     "billing_address": {
       "name": "32 Glenlake Dr.Dearborn, MI 48124"
     }
    }
   ]

Alternatively, you can specify the **column name** of the foreign key constraint as the **target**. This can be aliased to make
the result more clear.

.. tabs::

  .. code-tab:: http

    GET /orders?select=name,billing_address:billing_address_id(name) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/orders?select=name,billing_address:billing_address_id(name)"

.. code-block:: json

   [
    {
     "name": "Personal Water Filter",
     "billing_address": {
      "name": "32 Glenlake Dr.Dearborn, MI 48124"
     }
    }
   ]

.. _hint_disamb:

Hint Disambiguation
~~~~~~~~~~~~~~~~~~~

If specifying the **target** is not enough for unambiguous embedding, you can add a **hint**. For example, let's assume we create
two views of ``addresses``: ``central_addresses`` and ``eastern_addresses``.

PostgREST cannot detect a view as an embedded resource by using a column name or foreign key name as targets, that is why we need to use the view name ``central_addresses`` instead. But, still, this is not enough for an unambiguous embed.

.. tabs::

  .. code-tab:: http

    GET /orders?select=*,central_addresses(*) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/orders?select=*,central_addresses(*)" -i

.. code-block:: http

  HTTP/1.1 300 Multiple Choices

For solving this case, in addition to the **target**, we can add a **hint**.
Here, we still specify ``central_addresses`` as the **target** and use the ``billing_address`` foreign key as the **hint**:

.. tabs::

  .. code-tab:: http

    GET /orders?select=*,central_addresses!billing_address(*) HTTP/1.1

  .. code-tab:: bash Curl

    curl 'http://localhost:3000/orders?select=*,central_addresses!billing_address(*)' -i

.. code-block:: http

  HTTP/1.1 200 OK

  [ ... ]

Similarly to the **target**, the **hint** can be a **table name**, **foreign key constraint name** or **column name**.

Hints also work alongside ``!inner`` if a top level filtering is needed. From the above example:

.. tabs::

  .. code-tab:: http

    GET /orders?select=*,central_addresses!billing_address!inner(*)&central_addresses.code=AB1000 HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/orders?select=*,central_addresses!billing_address!inner(*)&central_addresses.code=AB1000"

.. note::

  If the relationship is so complex that hint disambiguation does not solve it, you can use :ref:`computed_relationships`.

