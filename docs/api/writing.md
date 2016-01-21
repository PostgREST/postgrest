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

You can POST a JSON array or CSV to insert multiple rows in a single
HTTP request. Note that using CSV requires less parsing on the server
and is **much faster**.

Example of CSV bulk insert. Simply post to a table route with
`Content-Type: text/csv` and include the names of the columns as
the first row. For instance

```HTTP
POST /people
name,age,height
J Doe,62,70
Jonas,10,55
```

An empty field (`,,`) is coerced to an empty string and the reserved
word `NULL` is mapped to the SQL null value. Note that there should
be no spaces between the column names and commas.

Example of JSON bulk insert. Send an array:

```HTTP
POST /people
[
  { "name": "J Doe", "age": 62, "height": 70 },
  { "name": "Janus", "age": 10, "height": 55 }
]
```

If you would like to get the full object back in the response to
your request, include the header `Prefer: return=representation`.
Chances are you only want certain information back, though, like
created ids. You can pass a `select` parameter to affect the shape
of the response (further documented in the [reading](/api/reading/)
page). For instance

```HTTP
POST /people?select=id
[...]
```
returns something like
```json
[ { "id": 1 }, { "id": 2 } ]
```

### Multiple Tables Insertion or Update

The cleanest way to insert or update data into multiple tables using only one POST/PATCH request
is to create a view that will join all target tables and present a single endpoint.
In our example let's assume one users table and one companies table.
In this case, we want a signup endpoint to create the first user within a company.
And for this endpoint we want to insert with one request both user and company.

```SQL
CREATE TABLE companies (
    id serial primary key,
    name text unique
);

CREATE TABLE users (
    id serial primary key,
    name text not null,
    pass text,
    company_id integer not null references companies
);
```

Having both tables created we create a view that joins them to be used
as a ```/signup``` endpoint.

```SQL
CREATE VIEW signup AS
    SELECT
        c.name AS company_name,
        u.name AS user_name,
        u.pass
    FROM
        public.users u
        JOIN public.companies c ON c.id = u.company_id;

```

After the signup view creation, we can issue ```GET``` requests to read data
from users and companies, but any atempt to ```POST``` or ```PATCH``` data will fail.
PostgreSQL won't allow any data change on views that have a ```JOIN``` 
clause in their ```FROM``` without a proper ```INSTEAD OF``` trigger.
So in the example bellow we create a trigger to allow insertion of data in the signup view.
The trigger is a simple PL/pgSQL function that first inserts into the companies table and
uses the newly create company_id to create its first user.


```SQL
CREATE FUNCTION signup()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  vcompany_id int;
BEGIN
  INSERT INTO companies (name) VALUES (new.company_name) RETURNING id INTO vcompany_id;
  INSERT INTO users (name, pass, company_id) VALUES (new.user_name, new.pass, vcompany_id);
RETURN new;
END;
$$;

CREATE TRIGGER signup
INSTEAD OF INSERT ON signup
FOR EACH ROW
EXECUTE PROCEDURE signup();
```

After the trigger creation we can issue a normal ```POST``` request to our signup endpoint:

```HTTP
POST /signup
{ "company_name": "foo", "user_name": "bar" }
```

For an endpoint such as signup its usually not desirable to have a ```PATCH``` route for updates,
and we will skip this example for the sake of brevity. But it would be implemented in a very
similar way to our ```POST``` example.

<div class="admonition note">
    <p class="admonition-title">Design Consideration</p>

    <p>It's advisable to create a separate trigger for <code>UPDATE</code> and <code>INSERT</code>
    avoiding conditionals that decide which is the trigger current operation.
    This makes it easier to change code for (or even disable) one operation without intefering with others while
    improving readability.
    </p>
</div>

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
DELETE /user?active=is.false
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
