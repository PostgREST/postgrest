![Logo](static/logo.png "Logo")

[![Build Status](https://circleci.com/gh/begriffs/postgrest.png?style=shield&circle-token=f723c01686abf0364de1e2eaae5aff1f68bd3ff2)](https://circleci.com/gh/begriffs/postgrest/tree/master)
<a href="https://heroku.com/deploy?template=https://github.com/begriffs/postgrest">
  <img src="https://img.shields.io/badge/%E2%86%91_Deploy_to-Heroku-7056bf.svg" alt="Deploy">
</a>
[![Join the chat at https://gitter.im/begriffs/postgrest](https://img.shields.io/badge/gitter-join%20chat%20%E2%86%92-brightgreen.svg)](https://gitter.im/begriffs/postgrest)

PostgREST serves a fully RESTful API from any existing PostgreSQL
database. It provides a cleaner, more standards-compliant, faster
API than you are likely to write from scratch.

### Demo [postgrest.herokuapp.com](https://postgrest.herokuapp.com) | Read [Docs](http://postgrest.com/) | Watch [Video](http://begriffs.com/posts/2014-12-30-intro-to-postgrest.html)


Try making requests to the live demo server with an HTTP client
such as [postman](http://www.getpostman.com/). The structure of the
demo database is defined by
[begriffs/postgrest-example](https://github.com/begriffs/postgrest-example).
You can use it as inspiration for test-driven server migrations in
your own projects.

Also try other tools in the PostgREST
[ecosystem](http://postgrest.com/install/ecosystem/) like the
[ng-admin demo](http://marmelab.com/ng-admin-postgrest).

### Usage

1. Download the binary ([latest release](https://github.com/begriffs/postgrest/releases/latest))
   for your platform.
2. Invoke like so:

    ```bash
    postgrest postgres://postgres:foobar@localhost:5432/my_db \
              --port 3000 \
              --schema public \
              --anonymous postgres \
              --pool 200
    ```

For more information on valid connection strings see the
[PostgreSQL docs](http://www.postgresql.org/docs/9.4/static/libpq-connect.html#LIBPQ-CONNSTRING).

### Performance

TLDR; subsecond response times for up to 2000 requests/sec on Heroku
free tier. ([see the load
test](http://postgrest.com/admin/performance/#benchmarks))

If you're used to servers written in interpreted languages (or named
after precious gems), prepare to be pleasantly surprised by PostgREST
performance.

Three factors contribute to the speed. First the server is written
in [Haskell](https://www.haskell.org/) using the
[Warp](http://www.yesodweb.com/blog/2011/03/preliminary-warp-cross-language-benchmarks)
HTTP server (aka a compiled language with lightweight threads).
Next it delegates as much calculation as possible to the database
including

* Serializing JSON responses directly in SQL
* Data validation
* Authorization
* Combined row counting and retrieval
* Data post in single command (`returning *`)

Finally it uses the database efficiently with the
[Hasql](https://nikita-volkov.github.io/hasql-benchmarks/) library
by

* Reusing prepared statements
* Keeping a pool of db connections
* Using the PostgreSQL binary protocol
* Being stateless to allow horizontal scaling

Ultimately the server (when load balanced) is constrained by database
performance. This may make it inappropriate for very large traffic
load. To learn more about scaling with Heroku and Amazon RDS see
the [performance guide](http://postgrest.com/admin/performance/).
Alternatively [CitusDB](https://www.citusdata.com/products/what-is-citusdb)
supports Postgres clustering for higher performance.

Other optimizations are possible, and some are outlined in the
[Future Features](#future-features).

### Security

PostgREST handles authentication (via [JSON Web
Tokens](http://postgrest.com/admin/security/#json-web-tokens))
and delegates authorization to the role information defined in the
database. This ensures there is a single declarative source of truth
for security.  When dealing with the database the server assumes
the identity of the currently authenticated user, and for the
duration of the connection cannot do anything the user themselves
couldn't. Other forms of authentication can be built on top
of the JWT primitive. See the docs for more information.

PostgreSQL 9.5 supports true [row-level
security](http://www.postgresql.org/docs/9.5/static/ddl-rowsecurity.html).
In previous versions it can be simulated with triggers and
security-barrier views. Because the possible queries to the database
are limited to certain templates using
[leakproof](http://blog.2ndquadrant.com/how-do-postgresql-security_barrier-views-work/)
functions, the trigger workaround does not compromise row-level
security.

For example security patterns see the [security
guide](http://postgrest.com/admin/security/).

### Versioning

A robust long-lived API needs the freedom to exist in multiple
versions. PostgREST does versioning through database schemas. This
allows you to expose tables and views without making the app brittle.
Underlying tables can be superseded and hidden behind public facing
views. You run an instance of PostgREST per schema and route requests
among them with a reverse proxy such as [nginx](http://nginx.org).
Learn more [here](http://postgrest.com/admin/versioning/).

### Self-documentation

Rather than writing and maintaining separate docs yourself let the
API explain its own affordances using HTTP. All PostgREST endpoints
respond to the OPTIONS verb and explain what they support as well
as the data format of their JSON payload. RAML support is an upcoming
feature.

The project uses HTTP itself to communicate other metadata. For
instance the number of rows returned by an endpoint is reported by -
and limited with - range headers. More about
[that](http://begriffs.com/posts/2014-03-06-beyond-http-header-links.html).

There are more opportunities for self-documentation listed in [Future
Features](#future-features).

### Data Integrity

Rather than relying on an Object Relational Mapper and custom
imperative coding, this system requires you put declarative constraints
directly into your database. Hence no application can corrupt your
data (including your API server).

The PostgREST exposes HTTP interface with safeguards to prevent
surprises, such as enforcing idempotent PUT requests, and

See examples of [PostgreSQL
constraints](http://www.tutorialspoint.com/postgresql/postgresql_constraints.htm)
and the [guide to routing](http://postgrest.com/api/reading/).

### Future Features

* Watching endpoint changes with sockets and Postgres pubsub
* Specifying per-view HTTP caching
* Inferring good default caching policies from the Postgres stats collector
* Generating mock data for test clients
* Maintaining separate connection pools per role to avoid "set/reset
  role" performance penalty
* Describe more relationships with Link headers
* Depending on accept headers, render OPTIONS as [RAML](http://raml.org/) or a
  relational diagram
* ... the other [issues](https://github.com/begriffs/postgrest/issues)

### Thanks

I'm grateful to the generous project
[contributors](https://github.com/begriffs/postgrest/graphs/contributors)
who have improved PostgREST immensely with their code and good
judgement.  See more details in the
[changelog](https://github.com/begriffs/postgrest/blob/master/CHANGELOG.md).

The cool logo came from [Mikey Casalaina](https://github.com/casalaina).
