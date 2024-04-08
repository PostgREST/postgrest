# Architecture

This document describes the high-level architecture of PostgREST.

## Bird's Eye View

![PostgREST Architecture](docs/_static/arch.png)

On the highest level, PostgREST processes the user HTTP request, if it's accepted it generates a SQL query, executes it, and produces a response.

## Code Map

This section talks briefly about various important modules.

The starting points of the program are `main/Main.hs` -> `src/PostgREST/CLI.hs` -> `src/PostgREST/App.hs`.

`App.hs` is then in charge of composing the different modules.

### Auth

It authenticates the request using JWT.

### ApiRequest

PostgREST operates over two types of resources: database relations (tables or views) and database functions; providing different representations (depending on the media type)
for them.

This module is in charge of representing the operation over an `ApiRequest` type. It parses the URL querystring following PostgREST syntax, the request headers, and the request body
(if possible it avoids parsing the body and sends it directly to the db).

A request might be rejected at this level if it's invalid. For example when providing an unknown media type to PostgREST or using an unknown HTTP method.

### Plan

Using the Schema Cache, this module fills in SQL details that the user might have not specified (like an `ON CONFLICT (pk)` clause ).

A request might be rejected at this level if it's invalid. For example when doing resource embedding on a nonexistent resource.

### Query

This module generates SQL queries that are parametrized and prepared. Only at this stage a connection from the pool might be used.

A query might fail (and be rollbacked) at this level if it doesn't comply to certain conditions, e.g. by not returning a single row when a ``Accept: application/vnd.pgrst.object``
header is specified.

### SchemaCache

This queries the PostgreSQL system catalogs and caches the results into a SchemaCache type, which then is used by the other modules.

### Admin

Admin server exposed in another port, it provides health checks.
