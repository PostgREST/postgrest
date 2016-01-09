## Security

PostgREST is designed to keep the database at the center of API
security. All authorization happens through database roles and
permissions. It is PostgREST's job to *authenticate* requests --
i.e. verify that a client is who they say they are -- and then let
the database *authorize* client actions.

We use [JSON Web Tokens](http://jwt.io/) to authenticate API requests.
As you'll recall a JWT contains a list of cryptographically signed
claims. PostgREST cares specifically about a claim called `role`.
When request contains a valid JWT with a role claim PostgREST will
switch to the database role with that name for the duration of the
HTTP request.  If the client included no (or an invalid) JWT then
PostgREST selects the "anonymous role" which is specified by a
command line arguments to the server on startup.

```js
{
  "role": "jdoe123"
}

// Encoded as JWT with a secret of "secret" this becomes
// eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiamRvZTEyMyJ9.X_ZeWSS9qsKDCDczv8C-GE2fccrPQjOh_ALMZJa5jsU
```

Using JWT allows us to authenticate with external services. A login
service needs merely to share a JWT encryption secret with the
PostgREST server. The secret is also a server command line option.

It is even possible to generate JWT from inside a stored procedure
in your database. Any SQL stored procedure that returns a type whose
name ends in `jwt_claims` will have its return value encoded into
JWT.  See the [User Management](http://postgrest.com/examples/users/)
example for details.

### Database Roles

Suppose you start the server like this:

```bash
postgrest postgres://foo@localhost:5432/mydb --anonymous anon
```

This means that `foo` is the so-called *authenticator role* and
`anon` is the anonymous role. When a new HTTP request arrives at the
server the latter is connected to the database as user `foo`. If
no JWT is present, or if it is invalid, or if it does not contain
the role claim then the server changes to the anonymous role with
the query

```sql
SET LOCAL ROLE anon;
```

Otherwise it sets the role to that specified by JWT. For security
your authenticator role should have access to nothing except the
ability to become other users. Supposing you have three roles, one
for anonymous users, one for authors, and another for the authenticator,
you would set it up like this

```sql
CREATE ROLE authenticator NOINHERIT LOGIN;
CREATE ROLE anon;
CREATE ROLE author;

GRANT anon, author TO authenticator;
```

### Row-Level Security

#### Simulated - PostgreSQL <9.5

#### Real - PostgreSQL >=9.5

### Building Auth on top of JWT

#### Basic Auth

#### Github Sign-in

### SSL
