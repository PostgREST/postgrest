# Architecture

This document describes the high-level architecture of PostgREST.

## Bird's Eye View

```haskell
postgrest :: Request -> Either Error SQLStatement -> Response
```

On the highest level, PostgREST processes an HTTP request, if it's accepted it builds a SQL statement for it, executes it, and produces a response.

## Code Map

This section talks briefly about various important modules.

The starting point of the program is `main/Main.hs`, which calls `src/PostgREST/CLI.hs` which then calls `src/PostgREST/App.hs`.

`App.hs` is then in charge of composing the different modules.

### ApiRequest.hs

PostgREST operates over two types of resources: database relations(tables or views) and database functions; providing different representations(depending on the media type)
for them.

This module is in charge of representing the operation over an `ApiRequest` type. It parses the URL querystring following PostgREST syntax, the request headers, and the request body
(if possible it avoids parsing the body and sends it directly to the db).

A request might be rejected at this level if it's invalid, e.g. providing an unknown media type to PostgREST or using an unknown HTTP method.

### Plan.hs

Using the Schema Cache, this module enables more complex functionality(like resource embedding) by enriching the ApiRequest. It generates Plan types(`ReadPlan`, `MutatePlan`)
that then will be used to generate a SQL statement.

A request might be rejected at this level if it's invalid, e.g. by doing resource embedding on a nonexistent resource.

An OPTIONS request doesn't require a plan to be generated.

### Query.hs

This module constructs single SQL statements that can be parametrized and prepared. Only at this stage a PostgreSQL connection from the pool is used.

A query might fail(and be rollbacked) at this level if it doesn't comply to certain conditions, e.g. by not returning a single row when a ``Accept: application/vnd.pgrst.object`` header is specified.

An OPTIONS request doesn't require a query to be executed.

### Response.hs

This module constructs the HTTP response body with the right headers.

It builds the OpenAPI response using the schema cache.

### Auth.hs

This module provides functions to deal with JWT authorization.

### Workers.hs

This spawns threads which are used to execute concurrent jobs.

Jobs include connection recovery, a listener for the PostgreSQL LISTEN command, and an admin server.

### SchemaCache.hs

This queries the PostgreSQL system catalogs and caches the metadata into a SchemaCache type,

### AppState.hs

The state of the App which is kept across requests.
