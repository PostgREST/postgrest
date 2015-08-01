[![Stories in Ready](https://badge.waffle.io/begriffs/postgrest.png?label=ready&title=Ready)](https://waffle.io/begriffs/postgrest)
![Logo](static/logo.png "Logo")

[![Build Status](https://circleci.com/gh/begriffs/postgrest.png?circle-token=f723c01686abf0364de1e2eaae5aff1f68bd3ff2)](https://circleci.com/gh/begriffs/postgrest/tree/master)
<a href="https://heroku.com/deploy?template=https://github.com/begriffs/postgrest">
  <img src="static/heroku.png" alt="Deploy">
</a>

PostgREST serves a fully RESTful API from any existing PostgreSQL
database. It provides a cleaner, more standards-compliant, faster
API than you are likely to write from scratch.

### Demo [postgrest.herokuapp.com](https://postgrest.herokuapp.com) | Watch [Video](http://begriffs.com/posts/2014-12-30-intro-to-postgrest.html) | [GUI Demo](http://marmelab.com/ng-admin-postgrest)

Try making requests to the live demo server with an HTTP client
such as [postman](http://www.getpostman.com/). The structure of the
demo database is defined by
[begriffs/postgrest-example](https://github.com/begriffs/postgrest-example).
You can use it as inspiration for test-driven server migrations in
your own projects.

### Usage

Download the binary ([latest release](https://github.com/begriffs/postgrest/releases/latest)) and invoke like so:

```bash
postgrest  --db-host localhost  --db-port 5432     \
           --db-name my_db      --db-user postgres \
           --db-pass foobar     --db-pool 200      \
           --anonymous postgres --port 3000        \
           --v1schema public
```

In production include the `--secure` option which redirects all
requests to HTTPS. Note that PostgREST does not handle the SSL
internally and must be put behind another server that does (such
as nginx or the Heroku load balancer).

### Performance

TLDR; subsecond response times for up to 2000 requests/sec on Heroku free tier. ([see the load test](https://github.com/begriffs/postgrest/wiki/Performance-and-Scaling))

If you're used to servers written in interpreted languages (or named
after precious gems), prepare to be pleasantly surprised by PostgREST
performance.

Three factors contribute to the speed. First the server is written
in [Haskell](https://new-www.haskell.org/) using the
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
* Using the Postgres binary protocol
* Being stateless to allow horizontal scaling

Ultimately the server (when load balanced) is constrained by database
performance. This may make it inappropriate for very large traffic
load. To learn more about scaling with Heroku and Amazon RDS see
the [performance guide](https://github.com/begriffs/postgrest/wiki/Performance-and-Scaling).

Other optimizations are possible, and some are outlined in the
[Future Features](#future-features).

### Security

PostgREST handles authentication (HTTP Basic over SSL or [JSON Web
Tokens](https://github.com/begriffs/postgrest/wiki/Security-and-Permissions#json-web-tokens))
and delegates authorization to the role information defined in the
database. This ensures there is a single declarative source of truth
for security.  When dealing with the database the server assumes
the identity of the currently authenticated user, and for the
duration of the connection cannot do anything the user themselves
couldn't.

Postgres 9.5 will soon support true [row-level
security](http://michael.otacoo.com/postgresql-2/postgres-9-5-feature-highlight-row-level-security/).
In the meantime what isn't yet implemented can be simulated with
triggers and security-barrier views. Because the possible queries
to the database are limited to certain templates using
[leakproof](http://blog.2ndquadrant.com/how-do-postgresql-security_barrier-views-work/)
functions, the trigger workaround does not compromise row-level
security.

For example security patterns see the [security
guide](https://github.com/begriffs/postgrest/wiki/Security-and-Permissions).

### Versioning

A robust long-lived API needs the freedom to exist in multiple
versions. PostgREST supports versioning through HTTP content
negotiation. Requests for a certain version translate into switching
which database schema to search for tables. PostgreSQL schema search
paths allow tables from earlier versions to be reused verbatim in
later versions.

To learn more, see the [guide to versioning](https://github.com/begriffs/postgrest/wiki/API-Versioning).

### Self-documention

Rather than writing and maintaining separate docs yourself let the
API explain its own affordances using HTTP. All PostgREST endpoints
respond to the OPTIONS verb and explain what they support as well
as the data format of their JSON payload.

The number of rows returned by an endpoint is reported by - and
limited with - range headers. More about
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

See examples of [Postgres
constraints](http://www.tutorialspoint.com/postgresql/postgresql_constraints.htm)
and the [guide to routing](https://github.com/begriffs/postgrest/wiki/Routing).

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
* Add two-legged auth with OAuth 1.0a(?)
* ... the other [issues](https://github.com/begriffs/postgrest/issues)

### Guides

* [Routing](https://github.com/begriffs/postgrest/wiki/Routing)
* [Versioning](https://github.com/begriffs/postgrest/wiki/API-Versioning)
* [Performance](https://github.com/begriffs/postgrest/wiki/Performance-and-Scaling)
* [Security](https://github.com/begriffs/postgrest/wiki/Security-and-Permissions)
* [Tutorial](http://blog.jonharrington.org/postgrest-introduction/) (external)
* [Heroku](https://github.com/begriffs/postgrest/wiki/Heroku)

### Thanks

* [Adam Baker](https://github.com/adambaker) for code
  contributions and many fundamental design discussions
* [Nikita Volkov](https://github.com/nikita-volkov) for writing the
  wonderful [Hasql](https://github.com/nikita-volkov/hasql) library
  and helping me use it
* [Mikey Casalaina](https://github.com/casalaina) for the cool logo
* [Jonathan Harrington](https://github.com/prio) for writing a [nice tutorial](http://blog.jonharrington.org/postgrest-introduction/)
* [Federico Rampazzo](https://github.com/framp) for suggesting and implementing [JWT](http://jwt.io/) support
