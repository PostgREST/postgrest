## Requesting Information

### Tables and Views

* ✅ Cacheable, prefetchable
* ✅ Idempotent

The list of accessible tables and views is provided at

```HTTP
GET /
```

Every view and table accessible by the active db role is exposed
in a one-level deep route. For instance the full contents of a table
`people` is returned at

```HTTP
GET /people
```

There are no `deeply/nested/routes`. Each route provides `OPTIONS`,
`GET`, `POST`, `PATCH`, and `DELETE` verbs depending entirely
on database permissions.

<div class="admonition note">
  <p class="admonition-title">Design Consideration</p>

  <p>Why not provide nested routes? Many APIs allow nesting to
  retrieve related information, such as <code>/films/1/director</code>.
  We offer a more flexible mechanism (inspired by GraphQL) to embed
  related information. It can handle one-to-many and many-to-many
  relationships. This is covered in the section about Embedding.</p>
</div>

### Stored Procedures

* ❌ Cannot necessarily be cached or prefetched
* ❌ Not necessarily idempotent

Every stored procedure is accessible under the `/rpc` prefix. The
API endpoint supports only POST which executes the function.

```HTTP
POST /rpc/proc_name
```

PostgREST supports calling procedures with [named
arguments](http://www.postgresql.org/docs/9.4/static/sql-syntax-calling-funcs.html#SQL-SYNTAX-CALLING-FUNCS-NAMED).
Include a JSON object in the request payload and each
key/value of the object will become an argument.

<div class="admonition note">
  <p class="admonition-title">Design Consideration</p>

  <p>Why the /rpc prefix? One reason is to avoid name collisions
  between views and procedures. It also helps emphasize to API
  consumers that these functions are not normal restful things.
  The functions can have arbitrary and surprising behavior, not
  the standard "post creates a resource" thing that users expect
  from the other routes.</p>

  <p>We considered allowing GET requests for functions that are
  marked non-volatile but could not reconcile how to pass in
  parameters. Query string arguments are reserved for shaping/filtering
  the output, not providing input.</p>
</div>



### Filtering

#### Filtering Rows

You can filter result rows by adding conditions on columns, each
condition a query string parameter.  For instance, to return people
aged under 13 years old:

```HTTP
GET /people?age=lt.13
```

Adding multiple parameters conjoins the conditions:

```HTTP
GET /people?age=gte.18&student=is.true
```

These operators are available:

abbreviation | meaning
------------ | -------
eq           | equals
gt           | greater than
lt           | less than
gte          | greater than or equal
lte          | less than or equal
like         | LIKE operator (use * in place of %)
ilike        | ILIKE operator (use * in place of %)
@@           | full-text search using to_tsquery
is           | checking for exact equality (null,true,false)
in           | one of a list of values e.g. `?a=in.1,2,3`
not          | negates another operator, see below

To negate any operator, prefix it with `not` like `?a=not.eq.2`.

For more complicated filters (such as those involving condition 1
*OR* condition 2) you will have to create a new view in the database.

Filters may be applied to [computed
columns](http://www.postgresql.org/docs/current/interactive/xfunc-sql.html#XFUNC-SQL-COMPOSITE-FUNCTIONS)
as well as actual table/view columns, even though the computed
columns will not appear in the output.

#### Filtering Columns

You can customize which columns are returned by using the `select`
parameter:

```HTTP
GET /people?select=age,height,weight
```

To cast the column types, add a double colon

```HTTP
GET /people?select=age::text,height,weight
```

Not all type coercions are possible, and you will get an error
describing any problems from selection or type casting.

The `select` keyword is reserved. You thus cannot filter rows based
on a column named select. Then again it is a reserved SQL keyword
too, hence an unlikely column name.

#### Inside JSONB

PostgreSQL >=9.4.2 supports native JSON columns and can even index
them by internal keys using the `jsonb` column type. PostgREST
allows you to filter results by internal JSON object values. Use
the single- and double-arrows to path into and obtain values, e.g.

```HTTP
GET /stuff?json_col->a->>b=eq.2
```

This query finds rows in `stuff` where `json_col->'a'->>'b'` is
equal to 2 (or "2" -- it coerces as needed). The final arrow must
be the double kind, `->>`, or else PostgREST will not attempt to
look inside the JSON.

### Ordering

The reserved word `order` reorders the response rows.  It uses a
comma-separated list of columns and directions:

```HTTP
GET /people?order=age.desc,height.asc
```

If no direction is specified it defaults to ascending order:

```HTTP
GET /people?order=age
```

If you care where nulls are sorted, add `nullsfirst` or `nullslast`:

```HTTP
GET /people?order=age.nullsfirst
GET /people?order=age.desc.nullslast
```

To filter the embedded items, you need to specify the tree path for the order param like so. 
```HTTP
GET /projects?select=id,name,tasks{id,name}&order=id.asc&tasks.order=name.ask
```


You can also use [computed
columns](http://www.postgresql.org/docs/current/interactive/xfunc-sql.html#XFUNC-SQL-COMPOSITE-FUNCTIONS)
to order the results, even though the computed
columns will not appear in the output.

### Limiting and Pagination

#### Pagination by Limit-Offset

PostgREST uses HTTP range headers for limiting and describing the
size of results. Every response contains the current range and total
results:

```
Range-Unit: items
Content-Range → 0-14/15
```

This means items zero through fourteen are returned out of a total
of fifteen -- i.e. all of them. This information is available in
every response and can help you render pagination controls on the
client. This is a RFC7233-compliant solution that keeps the response
JSON cleaner.

The client can set the limit and offset of a request by setting the
`Range` header. Translate the limit and offset into a range. To
request the first five elements, include these request headers:

```
Range-Unit: items
Range: 0-4
```

You can also use open-ended ranges for an offset with no limit:
`Range: 10-`.

#### Suppressing Counts

Sometimes knowing the total row count of a query is unnecessary and
only adds extra cost to the database query. So you can skip the
count total using a ```Prefer``` header as:

```
Prefer: count=none
```

With count suppressed the PostgREST response will look like:

```
Range-Unit: items
Content-Range → 0-14/*
```

### Embedding Foreign Entities

To help you make fewer requests, PostgREST allows the embedding of
traditional SQL relationships into a response. Suppose you have a
`projects` table which references `clients` through a foreign key
called `client_id`. When listing projects through the API you can
have it embed the client within each project response. For example,

```HTTP
GET /projects?id=eq.1&select=id, name, clients{*}
```

Notice this is the same `select` keyword which is used to choose
which columns to include. When a column name is followed by parentheses
that means to fetch the entire record and nest it. You include a
list of columns inside the parens, or asterisk to request all
columns.

The embedding works for 1-N, N-1, and N-N relationships. That means
you could also ask for a client and all their projects:

```HTTP
GET /clients?id=eq.42&select=id, name, projects{*}
```

In the examples above we asked for all columns in the embedded resource
but the the select query is recursive. You could for instance specify


```HTTP
GET /foo?select=x, y, bar{z, w, baz{*}}
```

You can select not only using table names, but also foreign key column names!
This is especially needed when you have a table with two foreign keys pointing to the same table, for example billing_address_id and shipping_address_id.
To embed the same foreign key row from our client example earlier
you could do the following:

```HTTP
GET /projects?id=eq.1&select=id, name, client_id{*}
```

In the response there will be a `client_id` object containing all
the data for that row.

However, a `client_id` object doesn't make a lot of sense, so you
could do one of two things. Tell PostgREST that you want the key renamed by using the `alias` feature like so `client:client_id{*}`, or just try `client{*}`
in the select parameter! PostgREST supports smart ducktype checking
for common foreign key names, so if your column name ends with
`_id`, `_fk`, or any variation of the two (including camelcase)
you can embed a row with just the name's beginning.

So for a complete example:

```HTTP
GET /projects?id=eq.1&select=id, name, client{*}
```

Would embed in the `client` key the row referenced with `client_id`.

The `alias` feature works for embedded entities and also for regular columns. This is useful in situations where for example you use different naming conventions in the database and frontend.

The following request will produce the output below:
```HTTP
GET /orders?id=eq.1&select=orderId:id, customer:customer_id{customerId:id, customerName:name}
```

```json
[
  {
    "orderId": 1,
    "customer": {
      "customerId": 1,
      "customerName": "John Smith"
    }
  }
]
```

<div class="admonition note">
    <p class="admonition-title">Design Consideration</p>
    <p>In order for this feature to work as expected after a schema change, PostgREST currently requires to be restarted.</p>
</div>

### Response Format

Query responses default to JSON but you can get them in CSV as well. Just make your request with the header

```HTTP
Accept: text/csv
```

### Singular vs Plural

Many APIs distinguish plural and singular resources, e.g.`/stories`
vs `/stories/1`. Why do we use `/stories?id=eq.1`? It is because a
single resource is for us a row determined by a primary key, and
primary keys can be *compound* (meaning defined across more than
one column). The common urls come from a degenerate case of simple
(and overwhelmingly numeric) primary keys often introduced automatically
be Object Relational Mapping.

For consistency's sake all these endpoints return a JSON array,
`/stories`, `/stories?genre=eq.mystery`, `/stories?id=eq.1`. They
are all filtering a bigger array. However you might want the
last one to return a single JSON object, not an array with one
element. To request a singular response send the header
`Prefer: plurality=singular`.

### Data Schema

As well as issuing a `GET /` to obtain a list of the tables, views,
and stored procedures available, you can get more information about
any particular endpoint.

```HTTP
OPTIONS /my_view
```

This will include the row names, their types, primary key
information, and foreign keys for the given table or view.

<div class="admonition warning">
    <p class="admonition-title">Schema Changes</p>

    <p>Note that when the schema of your database changes PostgREST will not reflect
    the change. You have to either restart PostgREST or send its running process
    a HUP signal:

    <pre><code>killall -HUP postgrest</code></pre>
</div>

### CORS

PostgREST sets highly permissive cross origin resource sharing.  It
accepts Ajax requests from any domain.
