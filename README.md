![Logo](static/logo.png "Logo")

[![Donate](https://img.shields.io/badge/Donate-Patreon-orange.svg?colorB=F96854)](https://www.patreon.com/postgrest)
[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.me/postgrest)
<a href="https://heroku.com/deploy?template=https://github.com/PostgREST/postgrest">
  <img src="https://img.shields.io/badge/%E2%86%91_Deploy_to-Heroku-7056bf.svg" alt="Deploy">
</a>
[![Join the chat at https://gitter.im/begriffs/postgrest](https://img.shields.io/badge/gitter-join%20chat%20%E2%86%92-brightgreen.svg)](https://gitter.im/begriffs/postgrest)
[![Docs](https://img.shields.io/badge/docs-latest-brightgreen.svg?style=flat)](http://postgrest.org)
[![Docker Stars](https://img.shields.io/docker/pulls/postgrest/postgrest.svg)](https://hub.docker.com/r/postgrest/postgrest/)
[![Build Status](https://circleci.com/gh/PostgREST/postgrest/tree/master.svg?style=shield)](https://circleci.com/gh/PostgREST/postgrest/tree/master)
[![Hackage docs](https://img.shields.io/hackage/v/postgrest.svg?label=hackage)](http://hackage.haskell.org/package/postgrest)

PostgREST serves a fully RESTful API from any existing PostgreSQL
database. It provides a cleaner, more standards-compliant, faster
API than you are likely to write from scratch.

## Usage

1. Download the binary ([latest release](https://github.com/PostgREST/postgrest/releases/latest))
   for your platform.
2. Invoke for help:

    ```bash
    postgrest --help
    ```

## Performance

TLDR; subsecond response times for up to 2000 requests/sec on Heroku
free tier. If you're used to servers written in interpreted languages
(or named after precious gems), prepare to be pleasantly surprised by
PostgREST performance.

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

* Keeping a pool of db connections
* Using the PostgreSQL binary protocol
* Being stateless to allow horizontal scaling

## Security

PostgREST [handles
authentication](http://postgrest.org/en/stable/auth.html) (via JSON Web
Tokens) and delegates authorization to the role information defined in
the database. This ensures there is a single declarative source of truth
for security.  When dealing with the database the server assumes the
identity of the currently authenticated user, and for the duration of
the connection cannot do anything the user themselves couldn't. Other
forms of authentication can be built on top of the JWT primitive. See
the docs for more information.

PostgreSQL 9.5 supports true [row-level
security](http://www.postgresql.org/docs/9.5/static/ddl-rowsecurity.html).
In previous versions it can be simulated with triggers and
security-barrier views. Because the possible queries to the database
are limited to certain templates using
[leakproof](http://blog.2ndquadrant.com/how-do-postgresql-security_barrier-views-work/)
functions, the trigger workaround does not compromise row-level
security.

## Versioning

A robust long-lived API needs the freedom to exist in multiple
versions. PostgREST does versioning through database schemas. This
allows you to expose tables and views without making the app brittle.
Underlying tables can be superseded and hidden behind public facing
views.

## Self-documentation

PostgREST uses the [OpenAPI](https://openapis.org/) standard to
generate up-to-date documentation for APIs. You can use a tool like
[Swagger-UI](https://github.com/swagger-api/swagger-ui) to render
interactive documentation for demo requests against the live API server.

This project uses HTTP to communicate other metadata as well.  For
instance the number of rows returned by an endpoint is reported by -
and limited with - range headers. More about
[that](http://begriffs.com/posts/2014-03-06-beyond-http-header-links.html).

## Data Integrity

Rather than relying on an Object Relational Mapper and custom
imperative coding, this system requires you put declarative constraints
directly into your database. Hence no application can corrupt your
data (including your API server).

The PostgREST exposes HTTP interface with safeguards to prevent
surprises, such as enforcing idempotent PUT requests.

See examples of [PostgreSQL
constraints](http://www.tutorialspoint.com/postgresql/postgresql_constraints.htm)
and the [API guide](http://postgrest.org/en/stable/api.html).

## Supporting development

You can help PostgREST ongoing maintenance and development by:

- Making a regular donation through Patreon https://www.patreon.com/postgrest

- Alternatively, you can make a one-time donation via Paypal https://www.paypal.me/postgrest

Every donation will be spent on making PostgREST better for the whole community.

## Thanks

The PostgREST organization is grateful to:

- The project [sponsors and backers](https://github.com/PostgREST/postgrest/blob/master/BACKERS.md) who support PostgREST's development.
- The project [contributors](https://github.com/PostgREST/postgrest/graphs/contributors) who have improved PostgREST immensely with their code
  and good judgement. See more details in the [changelog](https://github.com/PostgREST/postgrest/blob/master/CHANGELOG.md).

The cool logo came from [Mikey Casalaina](https://github.com/casalaina).
