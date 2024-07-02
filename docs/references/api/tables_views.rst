.. _tables_views:

Tables and Views
################

All views and tables of the :ref:`exposed schema <schemas>` and accessible by the :ref:`active database role <roles>` are available for querying. They are exposed in one-level deep routes.

.. _read:

Read
====

For instance the full contents of a table `people` is returned at

.. tabs::

  .. code-tab:: http

    GET /people HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people"

There are no deeply/nested/routes. Each route provides OPTIONS, GET, HEAD, POST, PATCH, and DELETE verbs depending entirely on database permissions.

.. note::

  Why not provide nested routes? Many APIs allow nesting to retrieve related information, such as :code:`/films/1/director`. We offer a more flexible mechanism (inspired by GraphQL) to embed related information. It can handle one-to-many and many-to-many relationships. This is covered in the section about :ref:`resource_embedding`.


.. _h_filter:

Horizontal Filtering
--------------------

You can filter result rows by adding conditions on columns. For instance, to return people aged under 13 years old:

.. tabs::

  .. code-tab:: http

    GET /people?age=lt.13 HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?age=lt.13"

You can evaluate multiple conditions on columns by adding more query string parameters. For instance, to return people who are 18 or older **and** are students:

.. tabs::

  .. code-tab:: http

    GET /people?age=gte.18&student=is.true HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?age=gte.18&student=is.true"

.. _operators:

Operators
~~~~~~~~~

These operators are available:

============  ========================  ==================================================================================
Abbreviation  In PostgreSQL             Meaning
============  ========================  ==================================================================================
eq            :code:`=`                 equals
gt            :code:`>`                 greater than
gte           :code:`>=`                greater than or equal
lt            :code:`<`                 less than
lte           :code:`<=`                less than or equal
neq           :code:`<>` or :code:`!=`  not equal
like          :code:`LIKE`              LIKE operator (to avoid `URL encoding <https://en.wikipedia.org/wiki/Percent-encoding>`_ you can use ``*`` as an alias of the percent sign ``%`` for the pattern)
ilike         :code:`ILIKE`             ILIKE operator (to avoid `URL encoding <https://en.wikipedia.org/wiki/Percent-encoding>`_ you can use ``*`` as an alias of the percent sign ``%`` for the pattern)
match         :code:`~`                 ~ operator, see :ref:`pattern_matching`
imatch        :code:`~*`                ~* operator, see :ref:`pattern_matching`
in            :code:`IN`                one of a list of values, e.g. :code:`?a=in.(1,2,3)`
                                        – also supports commas in quoted strings like
                                        :code:`?a=in.("hi,there","yes,you")`
is            :code:`IS`                checking for exact equality (null,true,false,unknown)
isdistinct    :code:`IS DISTINCT FROM`  not equal, treating :code:`NULL` as a comparable value
fts           :code:`@@`                :ref:`fts` using to_tsquery
plfts         :code:`@@`                :ref:`fts` using plainto_tsquery
phfts         :code:`@@`                :ref:`fts` using phraseto_tsquery
wfts          :code:`@@`                :ref:`fts` using websearch_to_tsquery
cs            :code:`@>`                contains e.g. :code:`?tags=cs.{example, new}`
cd            :code:`<@`                contained in e.g. :code:`?values=cd.{1,2,3}`
ov            :code:`&&`                overlap (have points in common), e.g. :code:`?period=ov.[2017-01-01,2017-06-30]` –
                                        also supports array types, use curly braces instead of square brackets e.g.
                                        :code:`?arr=ov.{1,3}`
sl            :code:`<<`                strictly left of, e.g. :code:`?range=sl.(1,10)`
sr            :code:`>>`                strictly right of
nxr           :code:`&<`                does not extend to the right of, e.g. :code:`?range=nxr.(1,10)`
nxl           :code:`&>`                does not extend to the left of
adj           :code:`-|-`               is adjacent to, e.g. :code:`?range=adj.(1,10)`
not           :code:`NOT`               negates another operator, see :ref:`logical_operators`
or            :code:`OR`                logical :code:`OR`, see :ref:`logical_operators`
and           :code:`AND`               logical :code:`AND`, see :ref:`logical_operators`
all           :code:`ALL`               comparison matches all the values in the list, see :ref:`logical_operators`
any           :code:`ANY`               comparison matches any value in the list, see :ref:`logical_operators`
============  ========================  ==================================================================================

For more complicated filters you will have to create a new view in the database, or use a stored procedure. For instance, here's a view to show "today's stories" including possibly older pinned stories:

.. code-block:: postgresql

  CREATE VIEW fresh_stories AS
  SELECT *
    FROM stories
   WHERE pinned = true
      OR published > now() - interval '1 day'
  ORDER BY pinned DESC, published DESC;

The view will provide a new endpoint:

.. tabs::

  .. code-tab:: http

    GET /fresh_stories HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/fresh_stories"

.. _logical_operators:

Logical operators
~~~~~~~~~~~~~~~~~

Multiple conditions on columns are evaluated using ``AND`` by default, but you can combine them using ``OR`` with the ``or`` operator. For example, to return people under 18 **or** over 21:

.. tabs::

  .. code-tab:: http

    GET /people?or=(age.lt.18,age.gt.21) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?or=(age.lt.18,age.gt.21)"

To **negate** any operator, you can prefix it with :code:`not` like :code:`?a=not.eq.2` or :code:`?not.and=(a.gte.0,a.lte.100)` .

You can also apply complex logic to the conditions:

.. tabs::

  .. code-tab:: http

    GET /people?grade=gte.90&student=is.true&or=(age.eq.14,not.and(age.gte.11,age.lte.17)) HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?grade=gte.90&student=is.true&or=(age.eq.14,not.and(age.gte.11,age.lte.17))"

.. _modifiers:

Operator Modifiers
~~~~~~~~~~~~~~~~~~

You may further simplify the logic using the ``any/all`` modifiers of ``eq,like,ilike,gt,gte,lt,lte,match,imatch``.

For instance, to avoid repeating the same column for ``or``, use ``any`` to get people with last names that start with O or P:

.. tabs::

  .. code-tab:: http

    GET /people?last_name=like(any).{O*,P*} HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?last_name=like(any).{O*,P*}"

In a similar way, you can use ``all`` to avoid repeating the same column for ``and``. To get the people with last names that start with O and end with n:

.. tabs::

  .. code-tab:: http

    GET /people?last_name=like(all).{O*,*n} HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?last_name=like(all).{O*,*n}"

.. _pattern_matching:

Pattern Matching
~~~~~~~~~~~~~~~~

The pattern-matching operators (:code:`like`, :code:`ilike`, :code:`match`, :code:`imatch`) exist to support filtering data using patterns instead of concrete strings, as described in the `PostgreSQL docs <https://www.postgresql.org/docs/current/functions-matching.html>`__.

To ensure best performance on larger data sets, an `appropriate index <https://www.postgresql.org/docs/current/pgtrgm.html#id-1.11.7.44.8>`__ should be used and even then, it depends on the pattern value and actual data statistics whether an existing index will be used by the query planner or not.

.. _fts:

Full-Text Search
~~~~~~~~~~~~~~~~

The :code:`fts` filter mentioned above has a number of options to support flexible textual queries, namely the choice of plain vs phrase search and the language used for stemming. Suppose that :code:`tsearch` is a table with column :code:`my_tsv`, of type `tsvector <https://www.postgresql.org/docs/current/datatype-textsearch.html>`_. The following examples illustrate the possibilities.

.. tabs::

  .. code-tab:: http

    GET /tsearch?my_tsv=fts(french).amusant HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/tsearch?my_tsv=fts(french).amusant"

.. tabs::

  .. code-tab:: http

    GET /tsearch?my_tsv=plfts.The%20Fat%20Cats HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/tsearch?my_tsv=plfts.The%20Fat%20Cats"

.. tabs::

  .. code-tab:: http

    GET /tsearch?my_tsv=not.phfts(english).The%20Fat%20Cats HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/tsearch?my_tsv=not.phfts(english).The%20Fat%20Cats"

.. tabs::

  .. code-tab:: http

    GET /tsearch?my_tsv=not.wfts(french).amusant HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/tsearch?my_tsv=not.wfts(french).amusant"

Using `websearch_to_tsquery` requires PostgreSQL of version at least 11.0 and will raise an error in earlier versions of the database.

.. _v_filter:

Vertical Filtering
------------------

When certain columns are wide (such as those holding binary data), it is more efficient for the server to withhold them in a response. The client can specify which columns are required using the :code:`select` parameter.

.. tabs::

  .. code-tab:: http

    GET /people?select=first_name,age HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?select=first_name,age"

.. code-block:: json

  [
    {"first_name": "John", "age": 30},
    {"first_name": "Jane", "age": 20}
  ]

The default is ``*``, meaning all columns. This value will become more important below in :ref:`resource_embedding`.

Renaming Columns
~~~~~~~~~~~~~~~~

You can rename the columns by prefixing them with an alias followed by the colon ``:`` operator.

.. tabs::

  .. code-tab:: http

    GET /people?select=fullName:full_name,birthDate:birth_date HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?select=fullName:full_name,birthDate:birth_date"

.. code-block:: json

  [
    {"fullName": "John Doe", "birthDate": "04/25/1988"},
    {"fullName": "Jane Doe", "birthDate": "01/12/1998"}
  ]

.. _casting_columns:

Casting Columns
~~~~~~~~~~~~~~~

Casting the columns is possible by suffixing them with the double colon ``::`` plus the desired type.

.. tabs::

  .. code-tab:: http

    GET /people?select=full_name,salary::text HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?select=full_name,salary::text"

.. code-block:: json

  [
    {"full_name": "John Doe", "salary": "90000.00"},
    {"full_name": "Jane Doe", "salary": "120000.00"}
  ]

.. _json_columns:

JSON Columns
------------

You can specify a path for a ``json`` or ``jsonb`` column using the arrow operators(``->`` or ``->>``) as per the `PostgreSQL docs <https://www.postgresql.org/docs/current/functions-json.html>`__.

.. code-block:: postgres

  CREATE TABLE people (
    id int,
    json_data json
  );

.. tabs::

  .. code-tab:: http

    GET /people?select=id,json_data->>blood_type,json_data->phones HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?select=id,json_data->>blood_type,json_data->phones"

.. code-block:: json

  [
    { "id": 1, "blood_type": "A-", "phones": [{"country_code": "61", "number": "917-929-5745"}] },
    { "id": 2, "blood_type": "O+", "phones": [{"country_code": "43", "number": "512-446-4988"}, {"country_code": "43", "number": "213-891-5979"}] }
  ]

.. tabs::

  .. code-tab:: http

    GET /people?select=id,json_data->phones->0->>number HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?select=id,json_data->phones->0->>number"

.. code-block:: json

  [
    { "id": 1, "number": "917-929-5745"},
    { "id": 2, "number": "512-446-4988"}
  ]

This also works with filters:

.. tabs::

  .. code-tab:: http

    GET /people?select=id,json_data->blood_type&json_data->>blood_type=eq.A- HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?select=id,json_data->blood_type&json_data->>blood_type=eq.A-"

.. code-block:: json

  [
    { "id": 1, "blood_type": "A-" },
    { "id": 3, "blood_type": "A-" },
    { "id": 7, "blood_type": "A-" }
  ]

Note that ``->>`` is used to compare ``blood_type`` as ``text``. To compare with an integer value use ``->``:

.. tabs::

  .. code-tab:: http

    GET /people?select=id,json_data->age&json_data->age=gt.20 HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?select=id,json_data->age&json_data->age=gt.20"

.. code-block:: json

  [
    { "id": 11, "age": 25 },
    { "id": 12, "age": 30 },
    { "id": 15, "age": 35 }
  ]
.. _composite_array_columns:

Composite / Array Columns
-------------------------

The arrow operators(``->``, ``->>``) can also be used for accessing composite fields and array elements.

.. code-block:: postgres

  CREATE TYPE coordinates (
    lat decimal(8,6),
    long decimal(9,6)
  );

  CREATE TABLE countries (
    id int,
    location coordinates,
    languages text[]
  );

.. tabs::

  .. code-tab:: http

    GET /countries?select=id,location->>lat,location->>long,primary_language:languages->0&location->lat=gte.19 HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/countries?select=id,location->>lat,location->>long,primary_language:languages->0&location->lat=gte.19"

.. code-block:: json

  [
    {
      "id": 5,
      "lat": "19.741755",
      "long": "-155.844437",
      "primary_language": "en"
    }
  ]

.. important::

  When using the ``->`` and ``->>`` operators on composite and array columns, PostgREST uses a query like ``to_jsonb(<col>)->'field'``. To make filtering and ordering on those nested fields use an index, the index needs to be created on the same expression, including the ``to_jsonb(...)`` call:

  .. code-block:: postgres

    CREATE INDEX ON mytable ((to_jsonb(data) -> 'identification' ->> 'registration_number'));

.. _ordering:

Ordering
--------

The reserved word ``order`` reorders the response rows. It uses a comma-separated list of columns and directions:

.. tabs::

  .. code-tab:: http

    GET /people?order=age.desc,height.asc HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?order=age.desc,height.asc"

If no direction is specified it defaults to ascending order:

.. tabs::

  .. code-tab:: http

    GET /people?order=age HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?order=age"

If you care where nulls are sorted, add ``nullsfirst`` or ``nullslast``:

.. tabs::

  .. code-tab:: http

    GET /people?order=age.nullsfirst HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?order=age.nullsfirst"

.. tabs::

  .. code-tab:: http

    GET /people?order=age.desc.nullslast HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?order=age.desc.nullslast"

You can also sort on fields of :ref:`composite_array_columns` or :ref:`json_columns`.

.. tabs::

  .. code-tab:: http

    GET /countries?order=location->>lat HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/countries?order=location->>lat"

.. _limits:

Limits and Pagination
---------------------

PostgREST uses HTTP range headers to describe the size of results. Every response contains the current range and, if requested, the total number of results:

.. code-block:: http

  HTTP/1.1 200 OK
  Range-Unit: items
  Content-Range: 0-14/*

Here items zero through fourteen are returned. This information is available in every response and can help you render pagination controls on the client. This is an RFC7233-compliant solution that keeps the response JSON cleaner.

There are two ways to apply a limit and offset rows: through request headers or query parameters. When using headers you specify the range of rows desired. This request gets the first twenty people.

.. tabs::

  .. code-tab:: http

    GET /people HTTP/1.1
    Range-Unit: items
    Range: 0-19

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people" -i \
      -H "Range-Unit: items" \
      -H "Range: 0-19"

Note that the server may respond with fewer if unable to meet your request:

.. code-block:: http

  HTTP/1.1 200 OK
  Range-Unit: items
  Content-Range: 0-17/*

You may also request open-ended ranges for an offset with no limit, e.g. :code:`Range: 10-`.

The other way to request a limit or offset is with query parameters. For example

.. tabs::

  .. code-tab:: http

    GET /people?limit=15&offset=30 HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?limit=15&offset=30"

This method is also useful for embedded resources, which we will cover in another section. The server always responds with range headers even if you use query parameters to limit the query.

.. _exact_count:

Exact Count
-----------

In order to obtain the total size of the table or view (such as when rendering the last page link in a pagination control), specify ``Prefer: count=exact`` as a request header:

.. tabs::

  .. code-tab:: http

    HEAD /bigtable HTTP/1.1
    Range-Unit: items
    Range: 0-24
    Prefer: count=exact

  .. code-tab:: bash Curl

    curl "http://localhost:3000/bigtable" -I \
      -H "Range-Unit: items" \
      -H "Range: 0-24" \
      -H "Prefer: count=exact"

Note that the larger the table the slower this query runs in the database. The server will respond with the selected range and total

.. code-block:: http

  HTTP/1.1 206 Partial Content
  Range-Unit: items
  Content-Range: 0-24/3573458

.. _planned_count:

Planned Count
-------------

To avoid the shortcomings of :ref:`exact count <exact_count>`, PostgREST can leverage PostgreSQL statistics and get a fairly accurate and fast count.
To do this, specify the ``Prefer: count=planned`` header.

.. tabs::

  .. code-tab:: http

    HEAD /bigtable?limit=25 HTTP/1.1
    Prefer: count=planned

  .. code-tab:: bash Curl

    curl "http://localhost:3000/bigtable?limit=25" -I \
      -H "Prefer: count=planned"

.. code-block:: http

  HTTP/1.1 206 Partial Content
  Content-Range: 0-24/3572000

Note that the accuracy of this count depends on how up-to-date are the PostgreSQL statistics tables.
For example in this case, to increase the accuracy of the count you can do ``ANALYZE bigtable``.
See `ANALYZE <https://www.postgresql.org/docs/current/sql-analyze.html>`_ for more details.

.. _estimated_count:

Estimated Count
---------------

When you are interested in the count, the relative error is important. If you have a :ref:`planned count <planned_count>` of 1000000 and the exact count is
1001000, the error is small enough to be ignored. But with a planned count of 7, an exact count of 28 would be a huge misprediction.

In general, when having smaller row-counts, the estimated count should be as close to the exact count as possible.

To help with these cases, PostgREST can get the exact count up until a threshold and get the planned count when
that threshold is surpassed. To use this behavior, you can specify the ``Prefer: count=estimated`` header. The **threshold** is
defined by :ref:`db-max-rows`.

Here's an example. Suppose we set ``db-max-rows=1000`` and ``smalltable`` has 321 rows, then we'll get the exact count:

.. tabs::

  .. code-tab:: http

    HEAD /smalltable?limit=25 HTTP/1.1
    Prefer: count=estimated

  .. code-tab:: bash Curl

    curl "http://localhost:3000/smalltable?limit=25" -I \
      -H "Prefer: count=estimated"

.. code-block:: http

  HTTP/1.1 206 Partial Content
  Content-Range: 0-24/321

If we make a similar request on ``bigtable``, which has 3573458 rows, we would get the planned count:

.. tabs::

  .. code-tab:: http

    HEAD /bigtable?limit=25 HTTP/1.1
    Prefer: count=estimated

  .. code-tab:: bash Curl

    curl "http://localhost:3000/bigtable?limit=25" -I \
      -H "Prefer: count=estimated"

.. code-block:: http

  HTTP/1.1 206 Partial Content
  Content-Range: 0-24/3572000

.. _head_req:

HEAD
----

A HEAD method will behave identically to GET except that no body will be returned (`RFC 2616 <https://datatracker.ietf.org/doc/html/rfc2616#section-9.4>`_) .
As an optimization, the generated query won't execute an aggregate (to avoid unnecessary data transfer).

.. _insert:

Insert
======

All tables and `auto-updatable views <https://www.postgresql.org/docs/current/sql-createview.html#SQL-CREATEVIEW-UPDATABLE-VIEWS>`_ can be modified through the API, subject to permissions of the requester's database role.

To create a row in a database table post a JSON object whose keys are the names of the columns you would like to create. Missing properties will be set to default values when applicable.

.. tabs::

  .. code-tab:: http

    POST /table_name HTTP/1.1

    { "col1": "value1", "col2": "value2" }

  .. code-tab:: bash Curl

    curl "http://localhost:3000/table_name" \
      -X POST -H "Content-Type: application/json" \
      -d '{ "col1": "value1", "col2": "value2" }'

.. code::

  HTTP/1.1 201 Created

No request body will be returned by default.

.. note::

   You can use the ``Prefer: return=minimal`` header to get the same behavior. This is only provided for completeness because it's basically a no-op.

Prefer: return=headers-only
---------------------------

If the table has a primary key, the response can contain a :code:`Location` header describing where to find the new object by including the header :code:`Prefer: return=headers-only` in the request. Make sure that the table is not write-only, otherwise constructing the :code:`Location` header will cause a permissions error.

.. tabs::

  .. code-tab:: http

    POST /projects HTTP/1.1
    Prefer: return=headers-only

    {"id":33, "name": "x"}

  .. code-tab:: bash Curl

    curl "http://localhost:3000/projects" \
      -X POST -H "Content-Type: application/json" -H "Prefer: return=headers-only" \
      -d '{"id":33, "name": "x"}'

.. code-block:: http

  HTTP/1.1 201 Created
  Location: /projects?id=eq.34
  Preference-Applied: return=headers-only

Prefer: return=representation
-----------------------------

On the other end of the spectrum you can get the full created object back in the response to your request by including the header :code:`Prefer: return=representation`. That way you won't have to make another HTTP call to discover properties that may have been filled in on the server side. You can also apply the standard :ref:`v_filter` to these results.

.. tabs::

  .. code-tab:: http

    POST /projects HTTP/1.1
    Content-Type: application/json; charset=utf-8
    Prefer: return=representation

    {"id":33, "name": "x"}

  .. code-tab:: bash Curl

    curl "http://localhost:3000/projects" \
      -X POST -H "Content-Type: application/json" -H "Prefer: return=representation" \
      -d '{"id":33, "name": "x"}'

.. code::

  HTTP/1.1 201 Created
  Preference-Applied: return=representation

  [
      {
          "id": 33,
          "name": "x"
      }
  ]

x-www-form-urlencoded
---------------------

URL encoded payloads can be posted with ``Content-Type: application/x-www-form-urlencoded``.

.. tabs::

  .. code-tab:: http

    POST /people HTTP/1.1
    Content-Type: application/x-www-form-urlencoded

    name=John+Doe&age=50&weight=80

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people" \
      -X POST -H "Content-Type: application/x-www-form-urlencoded" \
      -d "name=John+Doe&age=50&weight=80"

.. note::

  When inserting a row you must post a JSON object, not quoted JSON.

  .. code::

    Yes
    { "a": 1, "b": 2 }

    No
    "{ \"a\": 1, \"b\": 2 }"

  Some JavaScript libraries will post the data incorrectly if you're not careful. For best results try one of the :ref:`clientside_libraries` built for PostgREST.

.. important::

  It's recommended that you `use triggers instead of rules <https://wiki.postgresql.org/wiki/Don%27t_Do_This#Don.27t_use_rules>`_.
  Insertion on views with complex `rules <https://www.postgresql.org/docs/current/sql-createrule.html>`_ might not work out of the box with PostgREST due to its usage of CTEs.
  If you want to keep using rules, a workaround is to wrap the view insertion in a stored procedure and call it through the :ref:`s_procs` interface.
  For more details, see this `github issue <https://github.com/PostgREST/postgrest/issues/1283>`_.

.. _bulk_insert:

Bulk Insert
-----------

Bulk insert works exactly like single row insert except that you provide either a JSON array of objects having uniform keys, or lines in CSV format. This not only minimizes the HTTP requests required but uses a single INSERT statement on the back-end for efficiency.

To bulk insert CSV simply post to a table route with :code:`Content-Type: text/csv` and include the names of the columns as the first row. For instance

.. tabs::

  .. code-tab:: http

    POST /people HTTP/1.1
    Content-Type: text/csv

    name,age,height
    J Doe,62,70
    Jonas,10,55

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people" \
      -X POST -H "Content-Type: text/csv" \
      --data-binary @- << EOF
    name,age,height
    J Doe,62,70
    Jonas,10,55
    EOF

An empty field (:code:`,,`) is coerced to an empty string and the reserved word :code:`NULL` is mapped to the SQL null value. Note that there should be no spaces between the column names and commas.

To bulk insert JSON post an array of objects having all-matching keys

.. tabs::

  .. code-tab:: http

    POST /people HTTP/1.1
    Content-Type: application/json

    [
      { "name": "J Doe", "age": 62, "height": 70 },
      { "name": "Janus", "age": 10, "height": 55 }
    ]

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people" \
      -X POST -H "Content-Type: application/json" \
      -d @- << EOF
      [
        { "name": "J Doe", "age": 62, "height": 70 },
        { "name": "Janus", "age": 10, "height": 55 }
      ]
    EOF

.. _bulk_insert_default:

Bulk Insert with Default Values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Any missing columns in the payload will be inserted as ``null`` values. To use the ``DEFAULT`` column value instead, use the ``Prefer: missing=default`` header.

Having:

.. code-block:: postgres

  create table foo (
    id bigint generated by default as identity primary key
  , bar text
  , baz int default 100
  );

A request:

.. tabs::

  .. code-tab:: http

    POST /foo?columns=id,bar,baz HTTP/1.1
    Content-Type: application/json
    Prefer: missing=default, return=representation

    [
      { "bar": "val1"
      }
    , { "bar": "val2"
      , "baz": 15
      }
    ]

  .. code-tab:: bash Curl

    curl "http://localhost:3000/foo?columns=id,bar,baz" \
      -H "Content-Type: application/json" \
      -H "Prefer: missing=default, return=representation" \
      -d @- << EOF
        [
          { "bar": "val1"
          }
        , { "bar": "val2"
          , "baz": 15
          }
        ]
     EOF

Will result in:

.. code-block:: json

  [
    { "id":  1
    , "bar": "val1"
    , "baz": 100
    }
  , { "id":  2
    , "bar": "val2"
    , "baz": 15
    }
  ]

.. _specify_columns:

Specifying Columns
------------------

By using the :code:`columns` query parameter it's possible to specify the payload keys that will be inserted and ignore the rest of the payload.

.. tabs::

  .. code-tab:: http

     POST /datasets?columns=source,publication_date,figure HTTP/1.1
     Content-Type: application/json

     {
       "source": "Natural Disaster Prevention and Control",
       "publication_date": "2015-09-11",
       "figure": 1100,
       "location": "...",
       "comment": "...",
       "extra": "...",
       "stuff": "..."
     }

  .. code-tab:: bash Curl

     curl "http://localhost:3000/datasets?columns=source,publication_date,figure" \
       -X POST -H "Content-Type: application/json" \
       -d @- << EOF
       {
         "source": "Natural Disaster Prevention and Control",
         "publication_date": "2015-09-11",
         "figure": 1100,
         "location": "...",
         "comment": "...",
         "extra": "...",
         "stuff": "..."
       }
     EOF

In this case, only **source**, **publication_date** and **figure** will be inserted. The rest of the JSON keys will be ignored.

Using this also has the side-effect of being more efficient for :ref:`bulk_insert` since PostgREST will not process the JSON and
it'll send it directly to PostgreSQL.

.. _update:

Update
======

To update a row or rows in a table, use the PATCH verb. Use :ref:`h_filter` to specify which record(s) to update. Here is an example query setting the :code:`category` column to child for all people below a certain age.

.. tabs::

  .. code-tab:: http

    PATCH /people?age=lt.13 HTTP/1.1

    { "category": "child" }

  .. code-tab:: bash Curl

    curl "http://localhost:3000/people?age=lt.13" \
      -X PATCH -H "Content-Type: application/json" \
      -d '{ "category": "child" }'

Updates also support :code:`Prefer: return=representation` plus :ref:`v_filter`.

.. warning::

  Beware of accidentally updating every row in a table. To learn to prevent that see :ref:`block_fulltable`.

.. _upsert:

Upsert
======

You can make an upsert with :code:`POST` and the :code:`Prefer: resolution=merge-duplicates` header:

.. tabs::

  .. code-tab:: http

    POST /employees HTTP/1.1
    Prefer: resolution=merge-duplicates

    [
      { "id": 1, "name": "Old employee 1", "salary": 30000 },
      { "id": 2, "name": "Old employee 2", "salary": 42000 },
      { "id": 3, "name": "New employee 3", "salary": 50000 }
    ]

  .. code-tab:: bash Curl

    curl "http://localhost:3000/employees" \
      -X POST -H "Content-Type: application/json" \
      -H "Prefer: resolution=merge-duplicates" \
      -d @- << EOF
      [
        { "id": 1, "name": "Old employee 1", "salary": 30000 },
        { "id": 2, "name": "Old employee 2", "salary": 42000 },
        { "id": 3, "name": "New employee 3", "salary": 50000 }
      ]
    EOF

By default, upsert operates based on the primary key columns, you must specify all of them. You can also choose to ignore the duplicates with :code:`Prefer: resolution=ignore-duplicates`. This works best when the primary key is natural, but it's also possible to use it if the primary key is surrogate (example: "id serial primary key"). For more details read `this issue <https://github.com/PostgREST/postgrest/issues/1118>`_.

.. important::
  After creating a table or changing its primary key, you must refresh PostgREST schema cache for upsert to work properly. To learn how to refresh the cache see :ref:`schema_reloading`.

.. _on_conflict:

On Conflict
-----------

By specifying the ``on_conflict`` query parameter, you can make upsert work on a column(s) that has a UNIQUE constraint.

.. tabs::

  .. code-tab:: http

    POST /employees?on_conflict=name HTTP/1.1
    Prefer: resolution=merge-duplicates

    [
      { "name": "Old employee 1", "salary": 40000 },
      { "name": "Old employee 2", "salary": 52000 },
      { "name": "New employee 3", "salary": 60000 }
    ]

  .. code-tab:: bash Curl

    curl "http://localhost:3000/employees?on_conflict=name" \
      -X POST -H "Content-Type: application/json" \
      -H "Prefer: resolution=merge-duplicates" \
      -d @- << EOF
      [
        { "name": "Old employee 1", "salary": 40000 },
        { "name": "Old employee 2", "salary": 52000 },
        { "name": "New employee 3", "salary": 60000 }
      ]
    EOF

.. _upsert_put:

PUT
---

A single row upsert can be done by using :code:`PUT` and filtering the primary key columns with :code:`eq`:

.. tabs::

  .. code-tab:: http

    PUT /employees?id=eq.4 HTTP/1.1

    { "id": 4, "name": "Sara B.", "salary": 60000 }

  .. code-tab:: bash Curl

    curl "http://localhost/employees?id=eq.4" \
      -X PUT -H "Content-Type: application/json" \
      -d '{ "id": 4, "name": "Sara B.", "salary": 60000 }'

All the columns must be specified in the request body, including the primary key columns.

.. _delete:

Delete
======

To delete rows in a table, use the DELETE verb plus :ref:`h_filter`. For instance deleting inactive users:

.. tabs::

  .. code-tab:: http

    DELETE /user?active=is.false HTTP/1.1

  .. code-tab:: bash Curl

    curl "http://localhost:3000/user?active=is.false" -X DELETE

Deletions also support :code:`Prefer: return=representation` plus :ref:`v_filter`.

.. tabs::

  .. code-tab:: http

    DELETE /user?id=eq.1 HTTP/1.1
    Prefer: return=representation

  .. code-tab:: bash Curl

    curl "http://localhost:3000/user?id=eq.1" -X DELETE \
      -H "Prefer: return=representation"

.. code-block:: json

  {"id": 1, "email": "johndoe@email.com"}

.. warning::

  Beware of accidentally deleting all rows in a table. To learn to prevent that see :ref:`block_fulltable`.

.. _limited_update_delete:

Limited Update/Delete
=====================

You can limit the amount of affected rows by :ref:`update` or :ref:`delete` with the ``limit`` query parameter. For this, you must add an explicit ``order`` on a unique column(s).

.. tabs::

  .. code-tab:: http

    PATCH /users?limit=10&order=id&last_login=lt.2017-01-01 HTTP/1.1

    { "status": "inactive" }

  .. code-tab:: bash Curl

    curl -X PATCH "/users?limit=10&order=id&last_login=lt.2020-01-01" \
      -H "Content-Type: application/json" \
      -d '{ "status": "inactive" }'

.. tabs::

  .. code-tab:: http

    DELETE /users?limit=10&order=id&status=eq.inactive HTTP/1.1

  .. code-tab:: bash Curl

    curl -X DELETE "http://localhost:3000/users?limit=10&order=id&status=eq.inactive"

If your table has no unique columns, you can use the `ctid <https://www.postgresql.org/docs/current/ddl-system-columns.html>`_ system column.

Using ``offset`` to target a different subset of rows is also possible.

.. note::

  There is no native ``UPDATE...LIMIT`` or ``DELETE...LIMIT`` support in PostgreSQL; the generated query simulates that behavior and is based on `this Crunchy Data blog post <https://www.crunchydata.com/blog/simulating-update-or-delete-with-limit-in-postgres-ctes-to-the-rescue>`_.

.. raw:: html

  <script type="text/javascript">
    let hash = window.location.hash;

    const redirects = {
      // Tables and Views
      '#computed-virtual-columns': 'computed_fields.html#computed-fields',
    };

    let willRedirectTo = redirects[hash];

    if (willRedirectTo) {
      window.location.href = willRedirectTo;
    }
  </script>
