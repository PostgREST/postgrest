## Updating Data

### Record Creation

* ❌ Cannot be cached or prefetched
* ❌ Not idempotent

To create a row in a database table post a JSON object whose keys
are the names of the columns you would like to create. Missing keys
will be set to default values when applicable.

```HTTP
POST /table_name
{ "col1": "value1", "col2": "value2" }
```

The response will include a `Location` header describing where to
find the new object. If you would like to get the full object back
in the response to your request, include the header `Prefer:
return=representation`. That way you won't have to make another
HTTP call to discover properties that may have been filled in on
the server side.

### Bulk Insertion

* ❌ Cannot be cached or prefetched
* ❌ Not idempotent

While regular insertion uses JSON to encode the value, bulk insertion
uses CSV. Simply post to a table route with `Content-Type: text/csv`
and include the names of the columns as the first row. For instance

```HTTP
POST /people
name,age,height
J Doe,62,70
Jonas,10,55
```

An empty field (`,,`) is coerced to an empty string and the reserved
word `NULL` is mapped to the SQL null value. Note that there should
be no spaces between the column names and commas.

The server sends a multipart response for bulk insertions. Each part
contains a Location header with URL of each created resource.

```HTTP
Content-Type: application/json
Location: /festival?name=eq.Venice%20Film%20Festival


--postgrest_boundary
Content-Type: application/json
Location: /festival?name=eq.Cannes%20Film%20Festival
```

### Upsertion

* ❌ Cannot be cached or prefetched
* ✅ Idempotent

To insert or update a single row use the `PUT` verb on a properly
filtered table url:

```HTTP
PUT /table_name?primary_key=eq.foo
{ "col1": "value1", "col2": "value2" }
```

The request must satisfy two things. First all columns must be
specified (because a default value might be a changing value which
would violate idempotence). Second the URL must match the URL you
would use to get the value of the resource. This means that all
primary key columns must be included in the filter (there are more
than one when the primary key is compound).

If you would like to get the full object back in the response to
your request, include the header `Prefer: return=representation`.
It will of match exactly the object you sent though.

### Bulk Updates

* ❌ Cannot be cached or prefetched
* ❌ Not idempotent

To change parts of a resource or resources use the `PATCH` verb.
For instance, here is how to mark all young people as children.

```HTTP
PATCH /people?age=lt.13
{
  "person_type": "child"
}
```

This affects any rows matched by the url param filters, overwrites
any fields specified in in the payload JSON and leaves the other
fields unaffected. Note that although the payload is not in the
JSON patch format specified by
[RFC6902](https://tools.ietf.org/html/rfc6902), HTTP does not specify
which patch format to use. Our format is more pleasant, meant for
basic field replacements, and not at all "incorrect."

### Deletion

* ❌ Cannot be cached or prefetched
* ✅ Idempotent

Simply use the `DELETE` verb. All recors that match your filter
will be removed. For instance deleting inactive users:

```HTTP
DELETE /user?active=eq.false
```

### Protecting Dangerous Actions

Notice that it is very easy to delete or update many records at
once. In fact forgetting a filter will affect an entire table!


<div class="admonition warning">
    <p class="admonition-title">Invitation to Contribute</p>

    <p>We would like to investigate nginx rules to guard dangerous
    actions, perhaps requiring a confirmation header or query param
    to perform the action.</p>

    <p>You're invited to research this option and contribute to
    this documentation.</p>
</div>
