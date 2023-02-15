# Change Log

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## Unreleased

### Added

 - #1414, Add related orders - @steve-chavez
   + On a many-to-one or one-to-one relationship, you can order a parent by a child column `/projects?select=*,clients(*)&order=clients(name).desc.nullsfirst`
 - #1233, #1907, #2566, Allow spreading embedded resources - @steve-chavez
   + On a many-to-one or one-to-one relationship, you can unnest a json object with `/projects?select=*,...clients(client_name:name)`
   + Allows including the join table columns when resource embedding
   + Allows disambiguating a recursive m2m embed
   + Allows disambiguating an embed that has a many-to-many relationship using two foreign keys on a junction
 - #2340, Allow embedding without selecting any column - @steve-chavez
 - #2563, Allow `is.null` or `not.is.null` on an embedded resource - @steve-chavez
   + Offers a more flexible replacement for `!inner`, e.g. `/projects?select=*,clients(*)&clients=not.is.null`
   + Allows doing an anti join, e.g. `/projects?select=*,clients(*)&clients=is.null`
   + Allows using or across related tables conditions
 - #1100, Customizable OpenAPI title - @AnthonyFisi

### Fixed

 - #2651, Add the missing `get` path item for RPCs to the OpenAPI output - @laurenceisla
 - #2648, Fix inaccurate error codes with new ones - @laurenceisla
   + `PGRST204`: Column is not found
   + `PGRST003`: Timed out when acquiring connection to db
 - #2667, Fix `db-pool-acquisition-timeout` not logging to stderr when the timeout is reached - @steve-chavez

## [10.1.2] - 2023-02-01

### Fixed

 - #2565, Fix bad M2M embedding on RPC - @steve-chavez
 - #2575, Replace misleading error message when no function is found with a hint containing functions/parameters names suggestions - @laurenceisla
 - #2582, Move explanation about "single parameters" from the `message` to the `details` in the error output - @laurenceisla
 - #2569, Replace misleading error message when no relationship is found with a hint containing parent/child names suggestions - @laurenceisla
 - #1405, Add the required OpenAPI items object when the parameter is an array - @laurenceisla
 - #2592, Add upsert headers for POST requests to the OpenAPI output - @laurenceisla
 - #2623, Fix FK pointing to VIEW instead of TABLE in OpenAPI output - @laurenceisla
 - #2622, Consider any PostgreSQL authentication failure as fatal and exit immediately - @michivi
 - #2620, Fix `NOTIFY pgrst` not reloading the db connections catalog cache - @steve-chavez

## [10.1.1] - 2022-11-08

### Fixed

 - #2548, Fix regression when embedding views with partial references to multi column FKs - @wolfgangwalther
 - #2558, Fix regression when requesting limit=0 and `db-max-row` is set - @laurenceisla
 - #2542, Return a clear error without hitting the database when trying to update or insert an unknown column with `?columns` - @aljungberg

## [10.1.0] - 2022-10-28

### Added

 - #2348, Add `db-pool-acquisition-timeout` configuration option, time in seconds to wait to acquire a connection. - @robx

### Fixed

 - #2261, #2349, #2467, Reduce allocations communication with PostgreSQL, particularly for request bodies. - @robx
 - #2401, #2444, Fix SIGUSR1 to fully flush connections pool. - @robx
 - #2428, Fix opening an empty transaction on failed resource embedding - @steve-chavez
 - #2455, Fix embedding the same table multiple times - @steve-chavez
 - #2518, Fix a regression when embedding views where base tables have a different column order for FK columns - @wolfgangwalther
 - #2458, Fix a regression with the location header when inserting into views with PKs from multiple tables - @wolfgangwalther
 - #2356, Fix a regression in openapi output with mode follow-privileges - @wolfgangwalther
 - #2283, Fix infinite recursion when loading schema cache with self-referencing view - @wolfgangwalther
 - #2343, Return status code 200 for PATCH requests which don't affect any rows - @wolfgangwalther
 - #2481, Treat computed relationships not marked SETOF as M2O/O2O relationship - @wolfgangwalther
 - #2534, Fix embedding a computed relationship with a normal relationship - @steve-chavez
 - #2362, Fix error message when [] is used inside select - @wolfgangwalther
 - #2475, Disallow !inner on computed columns - @wolfgangwalther
 - #2285, Ignore leading and trailing spaces in column names when parsing the query string - @wolfgangwalther
 - #2545, Fix UPSERT with PostgreSQL 15 - @wolfgangwalther
 - #2459, Fix embedding views with multiple references to the same base column - @wolfgangwalther

### Changed

 - #2444, Removed `db-pool-timeout` option, because this was removed upstream in hasql-pool. - @robx
 - #2343, PATCH requests that don't affect any rows no longer return 404 - @wolfgangwalther
 - #2537, Stricter parsing of query string. Instead of silently ignoring, the parser now throws on invalid syntax like json paths for embeddings, hints for regular columns, empty casts or fts languages, etc. - @wolfgangwalther

### Deprecated

 - #1385, Deprecate bulk-calls when including the `Prefer: params=multiple-objects` in the request. A function with a JSON array or object parameter should be used instead for a better performance.

## [10.0.0] - 2022-08-18

### Added

 - #1933, #2109, Add a minimal health check endpoint - @steve-chavez
   + For enabling this, the `admin-server-port` config must be set explictly
   + A `<host>:<admin_server_port>/live` endpoint is available for checking if postgrest is running on its port/socket. 200 OK = alive, 503 = dead.
   + A `<host>:<admin_server_port>/ready` endpoint is available for checking a correct internal state(the database connection plus the schema cache). 200 OK = ready, 503 = not ready.
 - #1988, Add the current user to the request log on stdout - @DavidLindbom, @wolfgangwalther
 - #1823, Add the ability to run postgrest without any configuration. - @wolfgangwalther
   + #1991, Add the ability to run without `db-uri` using libpq's PG environment variables to connect. - @wolfgangwalther
   + #1769, Add the ability to run without `db-schemas`, defaulting to `db-schemas=public`. - @wolfgangwalther
   + #1689, Add the ability to run without `db-anon-role` disabling anonymous access. - @wolfgangwalther
 - #1543, Allow access to fields of composite types in select=, order= and filters through JSON operators -> and ->>. - @wolfgangwalther
 - #2075, Allow access to array items in ?select=, ?order= and filters through JSON operators -> and ->>. - @wolfgangwalther
 - #2156, #2211, Allow applying `limit/offset` to UPDATE/DELETE to only affect a subset of rows - @steve-chavez
   + It requires an explicit `order` on a unique column(s)
 - #1917, Add error codes with the `"PGRST"` prefix to the error response body to differentiate PostgREST errors from PostgreSQL errors - @laurenceisla
 - #1917, Normalize the error response body by always having the `detail` and `hint` error fields with a `null` value if they are empty - @laurenceisla
 - #2176, Errors raised with `SQLSTATE` now include the message and the code in the response body - @laurenceisla
 - #2236, Support POSIX regular expression operators for row filtering - @enote-kane
 - #2202, Allow returning XML from RPCs - @fjf2002
 - #2268, Allow returning XML from single-column queries - @fjf2002
 - #2300, RPC POST for function w/single unnamed XML param #2300 - @fjf2002
 - #1564, Allow geojson output by specifying the `Accept: application/geo+json` media type - @steve-chavez
   + Requires postgis >= 3.0
   + Works for GET, RPC, POST/PATCH/DELETE with `Prefer: return=representation`.
   + Resource embedding works and the embedded rows will go into the `properties` key
   + In case of multiple geometries in the same table, you can choose which one will go into the `geometry` key with the usual `?select` query parameter.
 - #1082, Add security definitions to the OpenAPI output - @laurenceisla
 - #2378, Support http OPTIONS method on RPC and root path - @steve-chavez
 - #2354, Allow getting the EXPLAIN plan of a request by using the `Accept: application/vnd.pgrst.plan` header - @steve-chavez
   + Only allowed if the `db-plan-enabled` config is set to true
   + Can generate the plan for different media types using the `for` parameter: `Accept: application/vnd.pgrst.plan; for="application/vnd.pgrst.object"`
   + Different options for the plan can be used with the `options` parameter: `Accept: application/vnd.pgrst.plan; options=analyze|verbose|settings|buffers|wal`
   + The plan can be obtained in text or json by using different media type suffixes: `Accept: application/vnd.pgrst.plan+text` and `Accept: application/vnd.pgrst.plan+json`.
 - #2144, Support computed relationships which allow extending and overriding relationships for resource embedding - @steve-chavez, @wolfgangwalther
 - #1984, Detect one-to-one relationships for resource embedding - @steve-chavez
   + Detected when there's a foreign key with a unique constraint or when a foreign key is also a primary key

### Fixed

 - #2058, Return 204 No Content without Content-Type for PUT - @wolfgangwalther
 - #2107, Clarify error for failed schema cache load. - @steve-chavez
   + From `Database connection lost. Retrying the connection` to `Could not query the database for the schema cache. Retrying.`
 - #1771, Fix silently ignoring filter on a non-existent embedded resource - @steve-chavez
 - #2152, Remove functions, which are uncallable because of unnamend arguments from schema cache and OpenAPI output. - @wolfgangwalther
 - #2145, Fix accessing json array fields with -> and ->> in ?select= and ?order=. - @wolfgangwalther
 - #2155, Ignore `max-rows` on POST, PATCH, PUT and DELETE - @steve-chavez
 - #2254, Fix inferring a foreign key column as a primary key column on views - @steve-chavez
 - #2070, Restrict generated many-to-many relationships - @steve-chavez
   + Only adds many-to-many relationships when: a table has FKs to two other tables and these FK columns are part of the table's PK columns.
 - #2278, Allow casting to types with underscores and numbers(e.g. `select=oid_array::_int4`) - @steve-chavez
 - #2277, #2238, #1643, Prevent views from breaking one-to-many/many-to-one embeds when using column or FK as target - @steve-chavez
    + When using a column or FK as target for embedding(`/tbl?select=*,col-or-fk(*)`), only tables are now detected and views are not.
    + You can still use a column or an inferred FK on a view to embed a table(`/view?select=*,col-or-fk(*)`)
 - #2317, Increase the `db-pool-timeout` to 1 hour to prevent frequent high connection latency - @steve-chavez
 - #2341, The search path now correctly identifies schemas with uppercase and special characters in their names (regression) - @laurenceisla
 - #2364, "404 Not Found" on nested routes and "405 Method Not Allowed" errors no longer start an empty database transaction - @steve-chavez
 - #2342, Fix inaccurate result count when an inner embed was selected after a normal embed in the query string - @laurenceisla
 - #2376, OPTIONS requests no longer start an empty database transaction - @steve-chavez
 - #2395, Allow using columns with dollar sign($) without double quoting in filters and `select` - @steve-chavez
 - #2410, Fix loop crash error on startup in Postgres 15 beta 3. Log: "UNION types \"char\" and text cannot be matched". - @yevon
 - #2397, Fix race conditions managing database connection helper - @robx
 - #2269, Allow `limit=0` in the request query to return an empty array - @gautam1168, @laurenceisla
 - #2401, Ensure database connections can't outlive SIGUSR1 - @robx

### Changed

 - #2001, Return 204 No Content without Content-Type for RPCs returning VOID - @wolfgangwalther
   + Previously, those RPCs would return "null" as a body with Content-Type: application/json.
 - #2156, `limit/offset` now limits the affected rows on UPDATE/DELETE  - @steve-chavez
   + Previously, `limit/offset` only limited the returned rows but not the actual updated rows
 - #2155, `max-rows` is no longer applied on POST/PATCH/PUT/DELETE returned rows - @steve-chavez
   + This was misleading because the affected rows were not really affected by `max-rows`, only the returned rows were limited
 - #2070, Restrict generated many-to-many relationships - @steve-chavez
   + A primary key that contains the foreign key columns is now needed for generating many-to-many relationships.
 - #2277, Views now are not detected when embedding using the column or FK as target (`/view?select=*,column(*)`) - @steve-chavez
   + This embedding form was easily made ambiguous whenever a new view was added.
   + You can use computed relationships to keep this embedding form working
 - #2312, Using `Prefer: return=representation` no longer returns a `Location` header - @laurenceisla
 - #1984, For the cases where one to one relationships are detected, json objects will be returned instead of json arrays of length 1
   + If you wish to override this behavior, you can use computed relationships to return arrays again

## [9.0.1] - 2022-06-03

### Fixed

- #2165, Fix json/jsonb columns should not have type in OpenAPI spec - @clrnd
- #2020, Execute deferred constraint triggers when using `Prefer: tx=rollback` - @wolfgangwalther
- #2077, Fix `is` not working with upper or mixed case values like `NULL, TrUe, FaLsE` - @steve-chavez
- #2024, Fix schema cache loading when views with XMLTABLE and DEFAULT are present - @wolfgangwalther
- #1724, Fix wrong CORS header Authentication -> Authorization - @wolfgangwalther
- #2120, Fix reading database configuration properly when `=` is present in value - @wolfgangwalther
- #2135, Remove trigger functions from schema cache and OpenAPI output, because they can't be called directly anyway. - @wolfgangwalther
- #2101, Remove aggregates, procedures and window functions from the schema cache and OpenAPI output. - @wolfgangwalther
- #2153, Fix --dump-schema running with a wrong PG version. - @wolfgangwalther
- #2042, Keep working when EMFILE(Too many open files) is reached. - @steve-chavez
- #2147, Ignore `Content-Type` headers for `GET` requests when calling RPCs. - @laurenceisla
    + Previously, `GET` without parameters, but with `Content-Type: text/plain` or `Content-Type: application/octet-stream` would fail with `404 Not Found`, even if a function without arguments was available.
- #2239, Fix misleading disambiguation error where the content of the `relationship` key looks like valid syntax - @laurenceisla
- #2294, Disable parallel GC for better performance on higher core CPUs - @steve-chavez
- #1076, Fix using CPU while idle - @steve-chavez

## [9.0.0] - 2021-11-25

### Added

 - #1783, Include partitioned tables into the schema cache. Allows embedding, UPSERT, INSERT with Location response, OPTIONS request and OpenAPI support for partitioned tables - @laurenceisla
 - #1878, Add Retry-After hint header when in recovery mode - @gautam1168
 - #1735, Allow calling function with single unnamed param through RPC POST. - @steve-chavez
   + Enables calling a function with a single json parameter without using `Prefer: params=single-object`
   + Enables uploading bytea to a function with `Content-Type: application/octet-stream`
   + Enables uploading raw text to a function with `Content-Type: text/plain`
 - #1938, Allow escaping inside double quotes with a backslash, e.g. `?col=in.("Double\"Quote")`, `?col=in.("Back\\slash")` - @steve-chavez
 - #1075, Allow filtering top-level resource based on embedded resources filters. This is enabled by adding `!inner` to the embedded resource, e.g. `/projects?select=*,clients!inner(*)&clients.id=eq.12`- @steve-chavez, @Iced-Sun
 - #1857, Make GUC names for headers, cookies and jwt claims compatible with PostgreSQL v14 - @laurenceisla, @robertsosinski
   + Getting the value for a header GUC on PostgreSQL 14 is done using `current_setting('request.headers')::json->>'name-of-header'` and in a similar way for `request.cookies` and `request.jwt.claims`
   + PostgreSQL versions below 14 can opt in to the new JSON GUCs by setting the `db-use-legacy-gucs` config option to false (true by default)
 - #1988, Allow specifying `unknown` for the `is` operator - @steve-chavez
 - #2031, Improve error message for ambiguous embedding and add a relevant hint that includes unambiguous embedding suggestions - @laurenceisla

### Fixed

 - #1871, Fix OpenAPI missing default values for String types and identify Array types as "array" instead of "string" - @laurenceisla
 - #1930, Fix RPC return type handling for `RETURNS TABLE` with a single column. Regression of #1615. - @wolfgangwalther
 - #1938, Fix using single double quotes(`"`) and backslashes(`/`) as values on the "in" operator - @steve-chavez
 - #1992, Fix schema cache query failing with standard_conforming_strings = off - @wolfgangwalther

### Changed

 - #1949, Drop support for embedding hints used with '.'(`select=projects.client_id(*)`), '!' should be used instead(`select=projects!client_id(*)`) - @steve-chavez
 - #1783, Partitions (created using `PARTITION OF`) are no longer included in the schema cache. - @laurenceisla
 - #2038, Dropped support for PostgreSQL 9.5 - @wolfgangwalther

## [8.0.0] - 2021-07-25

### Added

 - #1525, Allow http status override through response.status guc - @steve-chavez
 - #1512, Allow schema cache reloading with NOTIFY - @steve-chavez
 - #1119, Allow config file reloading with SIGUSR2 - @steve-chavez
 - #1558, Allow 'Bearer' with and without capitalization as authentication schema - @wolfgangwalther
 - #1470, Allow calling RPC with variadic argument by passing repeated params - @wolfgangwalther
 - #1559, No downtime when reloading the schema cache with SIGUSR1 - @steve-chavez
 - #504, Add `log-level` config option. The admitted levels are: crit, error, warn and info - @steve-chavez
 - #1607, Enable embedding through multiple views recursively - @wolfgangwalther
 - #1598, Allow rollback of the transaction with Prefer tx=rollback - @wolfgangwalther
 - #1633, Enable prepared statements for GET filters. When behind a connection pooler, you can disable preparing with `db-prepared-statements=false`
   + This increases throughput by around 30% for simple GET queries(no embedding, with filters applied).
 - #1729, #1760, Get configuration parameters from the db and allow reloading config with NOTIFY  - @steve-chavez
 - #1824, Allow OPTIONS to generate certain HTTP methods for a DB view - @laurenceisla
 - #1872, Show timestamps in startup/worker logs - @steve-chavez
 - #1881, Add `openapi-mode` config option that allows ignoring roles privileges when showing the OpenAPI output - @steve-chavez
 - CLI options(for debugging):
   + #1678, Add --dump-config CLI option that prints loaded config and exits - @wolfgangwalther
   + #1691, Add --example CLI option to show example config file - @wolfgangwalther
   + #1697, #1723, Add --dump-schema CLI option for debugging purposes - @monacoremo, @wolfgangwalther
 - #1794, (Experimental) Add `request.spec` GUC for db-root-spec - @steve-chavez

### Fixed

 - #1592, Removed single column restriction to allow composite foreign keys in join tables - @goteguru
 - #1530, Fix how the PostgREST version is shown in the help text when the `.git` directory is not available - @monacoremo
 - #1094, Fix expired JWTs starting an empty transaction on the db - @steve-chavez
 - #1162, Fix location header for POST request with select= without PK - @wolfgangwalther
 - #1585, Fix error messages on connection failure for localized postgres on Windows - @wolfgangwalther
 - #1636, Fix `application/octet-stream` appending `charset=utf-8` - @steve-chavez
 - #1469, #1638 Fix overloading of functions with unnamed arguments - @wolfgangwalther
 - #1560, Return 405 Method not Allowed for GET of volatile RPC instead of 500 - @wolfgangwalther
 - #1584, Fix RPC return type handling and embedding for domains with composite base type (#1615) - @wolfgangwalther
 - #1608, #1635, Fix embedding through views that have COALESCE with subselect - @wolfgangwalther
 - #1572, Fix parsing of boolean config values for Docker environment variables, now it accepts double quoted truth values ("true", "false") and numbers("1", "0") - @wolfgangwalther
 - #1624, Fix using `app.settings.xxx` config options in Docker, now they can be used as `PGRST_APP_SETTINGS_xxx` - @wolfgangwalther
 - #1814, Fix panic when attempting to run with unix socket on non-unix host and properly close unix domain socket on exit - @monacoremo
 - #1825, Disregard internal junction(in non-exposed schema) when embedding - @steve-chavez
 - #1846, Fix requests for overloaded functions from html forms to no longer hang (#1848) - @laurenceisla
 - #1858, Add a hint and clarification to the no relationship found error - @laurenceisla
 - #1841, Show comprehensive error when an RPC is not found in a stale schema cache - @laurenceisla
 - #1875, Fix Location headers in headers only representation for null PK inserts on views - @laurenceisla

### Changed

 - #1522, #1528, #1535, Docker images are now built from scratch based on a the static PostgREST executable (#1494) and with Nix instead of a `Dockerfile`. This reduces the compressed image size from over 30mb to about 4mb - @monacoremo
 - #1461, Location header for POST request is only included when PK is available on the table - @wolfgangwalther
 - #1560, Volatile RPC called with GET now returns 405 Method not Allowed instead of 500 - @wolfgangwalther
 - #1584, #1849 Functions that declare `returns composite_type` no longer return a single object array by default, only functions with `returns setof composite_type` return an array of objects - @wolfgangwalther
 - #1604, Change the default logging level to `log-level=error`. Only requests with a status greater or equal than 500 will be logged. If you wish to go back to the previous behaviour and log all the requests, use `log-level=info` - @steve-chavez
   + Because currently there's no buffering for logging, defaulting to the `error` level(minimum logging) increases throughput by around 15% for simple GET queries(no embedding, with filters applied).
 - #1617, Dropped support for PostgreSQL 9.4 - @wolfgangwalther
 - #1679, Renamed config settings with fallback aliases. e.g. `max-rows` is now `db-max-rows`, but `max-rows` is still accepted - @wolfgangwalther
 - #1656, Allow `Prefer=headers-only` on POST requests and change default to `minimal` (#1813) - @laurenceisla
 - #1854, Dropped undocumented support for gzip compression (which was surprisingly slow and easily enabled by mistake). In some use-cases this makes Postgres up to 3x faster - @aljungberg
 - #1872, Send startup/worker logs to stderr to differentiate from access logs on stdout - @steve-chavez

## [7.0.1] - 2020-05-18

### Fixed

- #1473, Fix overloaded computed columns on RPC - @wolfgangwalther
- #1471, Fix POST, PATCH, DELETE with ?select= and return=minimal and PATCH with empty body - @wolfgangwalther
- #1500, Fix missing `openapi-server-proxy-uri` config option - @steve-chavez
- #1508, Fix `Content-Profile` not working for POST RPC - @steve-chavez
- #1452, Fix PUT restriction for all columns - @steve-chavez

### Changed

- From this version onwards, the release page will only include a single Linux static executable that can be run on any Linux distribution.

## [7.0.0] - 2020-04-03

### Added

- #1417, `Accept: application/vnd.pgrst.object+json` behavior is now enforced for POST/PATCH/DELETE regardless of `Prefer: return=representation/minimal` - @dwagin
- #1415, Add support for user defined socket permission via `server-unix-socket-mode` config option - @Dansvidania
- #1383, Add support for HEAD request - @steve-chavez
- #1378, Add support for `Prefer: count=planned` and `Prefer: count=estimated` on GET /table - @steve-chavez, @LorenzHenk
- #1327, Add support for optional query parameter `on_conflict` to upsert with specified keys for POST - @ykst
- #1430, Allow specifying the foreign key constraint name(`/source?select=fk_constraint(*)`) to disambiguate an embedding - @steve-chavez
- #1168, Allow access to the `Authorization` header through the `request.header.authorization` GUC - @steve-chavez
- #1435, Add `request.method` and `request.path` GUCs - @steve-chavez
- #1088, Allow adding headers to GET/POST/PATCH/PUT/DELETE responses through the `response.headers` GUC - @steve-chavez
- #1427, Allow overriding provided headers(Location, Content-Type, etc) through the `response.headers` GUC - @steve-chavez
- #1450, Allow multiple schemas to be exposed in one instance. The schema to use can be selected through the headers `Accept-Profile` for GET/HEAD and `Content-Profile` for POST/PATCH/PUT/DELETE - @steve-chavez, @mahmoudkassem

### Fixed

- #1301, Fix self join resource embedding on PATCH - @herulume, @steve-chavez
- #1389, Fix many to many resource embedding on RPC/PATCH - @steve-chavez
- #1355, Allow PATCH/DELETE without `return=minimal` on tables with no select privileges - @steve-chavez
- #1361, Fix embedding a VIEW when its source foreign key is UNIQUE - @bwbroersma

### Changed

- #1385, bulk RPC call now should be done by specifying a `Prefer: params=multiple-objects` header - @steve-chavez
- #1401, resource embedding now outputs an error when multiple relationships between two tables are found - @steve-chavez
- #1423, default Unix Socket file mode from 755 to 660 - @dwagin
- #1430, Remove embedding with duck typed column names `GET /projects?select=client(*)`- @steve-chavez
  + You can rename the foreign key to `client` to make this request work in the new version: `alter table projects rename constraint projects_client_id_fkey to client`
- #1413, Change `server-proxy-uri` config option to `openapi-server-proxy-uri` - @steve-chavez

## [6.0.2] - 2019-08-22

### Fixed

- #1369, Change `raw-media-types` to accept a string of comma separated MIME types - @Dansvidania
- #1368, Fix long column descriptions being truncated at 63 characters in PostgreSQL 12 - @amedeedaboville
- #1348, Go back to converting plus "+" to space " " in querystrings by default - @steve-chavez

### Deprecated

- #1348, Deprecate `.` symbol for disambiguating resource embedding(added in #918). The url-safe '!' should be used instead. We refrained from using `+` as part of our syntax because it conflicts with some http clients and proxies.

## [6.0.1] - 2019-07-30

### Added

- #1349, Add user defined raw output media types via `raw-media-types` config option - @Dansvidania
- #1243, Add websearch_to_tsquery support - @herulume

### Fixed

- #1336, Error when testing on Chrome/Firefox: text/html requested but a single column was not selected - @Dansvidania
- #1334, Unable to compile v6.0.0 on windows - @steve-chavez

## [6.0.0] - 2019-06-21

### Added

- #1186, Add support for user defined unix socket via `server-unix-socket` config option - @Dansvidania
- #690, Add `?columns` query parameter for faster bulk inserts, also ignores unspecified json keys in a payload - @steve-chavez
- #1239, Add support for resource embedding on materialized views - @vitorbaptista
- #1264, Add support for bulk RPC call - @steve-chavez
- #1278, Add db-pool-timeout config option - @qu4tro
- #1285, Abort on wrong database password - @qu4tro
- #790, Allow override of OpenAPI spec through `root-spec` config option - @steve-chavez
- #1308, Accept `text/plain` and `text/html` for raw output - @steve-chavez


### Fixed

- #1223, Fix incorrect OpenAPI externalDocs url - @steve-chavez
- #1221, Fix embedding other resources when having a self join - @steve-chavez
- #1242, Fix embedding a view having a select in a where - @steve-chavez
- #1238, Fix PostgreSQL to OpenAPI type mappings for numeric and character types - @fpusch
- #1265, Fix query generated on bulk upsert with an empty array - @qu4tro
- #1273, Fix RPC ignoring unknown arguments by default - @steve-chavez
- #1257, Fix incorrect status when a PATCH request doesn't find rows to change - @qu4tro

### Changed

- #1288, Change server-host default of 127.0.0.1 to !4

### Deprecated

- #1288, Deprecate `.` symbol for disambiguating resource embedding(added in #918). '+' should be used instead. Though '+' is url safe, certain clients might need to encode it to '%2B'.

### Removed

- #1288, Removed support for schema reloading with SIGHUP, SIGUSR1 should be used instead - @steve-chavez

## [5.2.0] - 2018-12-12

### Added

- #1205, Add support for parsing JSON Web Key Sets - @russelldavies
- #1203, Add support for reading db-uri from a separate file - @zhoufeng1989
- #1200, Add db-extra-search-path config for adding schemas to the search_path, solves issues related to extensions created on the public schema - @steve-chavez
- #1219, Add ability to quote column names on filters - @steve-chavez

### Fixed

- #1182, Fix embedding on views with composite pks - @steve-chavez
- #1180, Fix embedding on views with subselects in pg10 - @steve-chavez
- #1197, Allow CORS for PUT - @bkylerussell
- #1181, Correctly qualify function argument of custom type in public schema - @steve-chavez
- #1008, Allow columns that contain spaces in filters - @steve-chavez

## [5.1.0] - 2018-08-31

### Added

- #1099, Add support for getting json/jsonb by array index - @steve-chavez
- #1145, Add materialized view columns to OpenAPI output - @steve-chavez
- #709, Allow embedding on views with subselects/CTE - @steve-chavez
- #1148, OpenAPI: add `required` section for the non-nullable columns - @laughedelic
- #1158, Add summary to OpenAPI doc for RPC functions - @mdr1384

### Fixed

- #1113, Fix UPSERT failing when having a camel case PK column - @steve-chavez
- #945, Fix slow start-up time on big schemas - @steve-chavez
- #1129, Fix view embedding when table is capitalized - @steve-chavez
- #1149, OpenAPI: Change `GET` response type to array - @laughedelic
- #1152, Fix RPC failing when having arguments with reserved or uppercase keywords - @mdr1384
- #905, Fix intermittent empty replies - @steve-chavez
- #1139, Fix JWTIssuedAtFuture failure for valid iat claim - @steve-chavez
- #1141, Fix app.settings resetting on pool timeout - @steve-chavez

### Changed

- #1099, Numbers in json path `?select=data->1->>key` now get treated as json array indexes instead of keys - @steve-chavez
- #1128, Allow finishing a json path with a single arrow `->`. Now a json can be obtained without resorting to casting, Previously: `/json_arr?select=data->>2::json`, now: `/json_arr?select=data->2` - @steve-chavez
- #724, Change server-host default of *4 to 127.0.0.1

### Deprecated

- #724, SIGHUP deprecated, SIGUSR1 should be used instead

## [0.5.0.0] - 2018-05-14

### Added

- The configuration (e.g. `postgrest.conf`) now accepts arbitrary settings that will be passed through as session-local database settings. This can be used to pass in secret keys directly as strings, or via OS environment variables. For instance: `app.settings.jwt_secret = "$(MYAPP_JWT_SECRET)"` will take `MYAPP_JWT_SECRET` from the environment and make it available to postgresql functions as `current_setting('app.settings.jwt_secret')`. Only `app.settings.*` values in the configuration file are treated in this way. - @canadaduane
- #256, Add support for bulk UPSERT with POST and single UPSERT with PUT - @steve-chavez
- #1078, Add ability to specify source column in embed - @steve-chavez
- #821, Allow embeds alias to be used in filters - @steve-chavez
- #906, Add jspath configurable `role-claim-key` - @steve-chavez
- #1061, Add foreign tables to OpenAPI output - @rhyamada

### Fixed

- #828, Fix computed column only working in public schema - @steve-chavez
- #925, Fix RPC high memory usage by using parametrized query and avoiding json encoding - @steve-chavez
- #987, Fix embedding with self-reference foreign key - @steve-chavez
- #1044, Fix view parent embedding when having many views - @steve-chavez
- #781, Fix accepting misspelled desc/asc ordering modificators - @onporat, @steve-chavez

### Changed

- #828, A `SET SCHEMA <db-schema>` is done on each request, this has the following implications:
  - Computed columns now only work if they belong to the db-schema
  - Stored procedures might require a `search_path` to work properly, for further details see https://postgrest.org/en/v5.0/api.html#explicit-qualification
- To use RPC now the `json_to_record/json_to_recordset` functions are needed, these are available starting from PostgreSQL 9.4 - @steve-chavez
- Overloaded functions now depend on the `dbStructure`, restart/sighup may be needed for their correct functioning - @steve-chavez
- #1098, Removed support for:
  + curly braces `{}` in embeds, i.e. `/clients?select=*,projects{*}` can no longer be used, from now on parens `()` should be used `/clients?select=*,projects(*)` - @steve-chavez
  + "in" operator without parens, i.e. `/clients?id=in.1,2,3` no longer supported, `/clients?id=in.(1,2,3)` should be used - @steve-chavez
  + "@@", "@>" and "<@" operators, from now on their mnemonic equivalents should be used "fts", "cs" and "cd" respectively - @steve-chavez

## [0.4.4.0] - 2018-01-08

### Added

- #887, #601, #1007, Allow specifying dictionary and plain/phrase tsquery in full text search - @steve-chavez
- #328, Allow doing GET on rpc - @steve-chavez
- #917, Add ability to map RAISE errorcode/message to http status - @steve-chavez
- #940, Add ability to map GUC to http response headers - @steve-chavez
- #1022, Include git sha in version report - @begriffs
- Faster queries using json_agg - @ruslantalpa

### Fixed

- #876, Read secret files as binary, discard final LF if any - @eric-brechemier
- #968, Treat blank proxy uri as missing - @begriffs
- #933, OpenAPI externals docs url to current version - @steve-chavez
- #962, OpenAPI don't err on nonexistent schema - @steve-chavez
- #954, make OpenAPI rpc output dependent on user privileges - @steve-chavez
- #955, Support configurable aud claim - @statik
- #996, Fix embedded column conflicts table name - @grotsev
- #974, Fix RPC error when function has single OUT param - @steve-chavez
- #1021, Reduce join size in allColumns for faster program start - @nextstopsun
- #411, Remove the need for pk in &select for parent embed - @steve-chavez
- #1016, Fix anonymous requests when configured with jwt-aud - @ruslantalpa

## [0.4.3.0] - 2017-09-06

### Added

- #567, Support more JWT signing algorithms, including RSA - @begriffs
- #889, Allow more than two conditions in a single and/or - @steve-chavez
- #883, Binary output support for RPC - @steve-chavez
- #885, Postgres COMMENTs on SCHEMA/TABLE/COLUMN are used for OpenAPI - @ldesgoui
- #907, Ability to embed using a specific relation when there are multiple between tables - @ruslantalpa
- #930, Split table comment on newline to get OpenAPI operation summary and description - @daurnimator
- #938, Support for range operators - @russelldavies

### Fixed

- #877, Base64 secret read from a file ending with a newline - @eric-brechemier
- #896, Boolean env var interpolation in config file - @begriffs
- #885, OpenAPI repetition reduced by using more definitions- @ldesgoui
- #924, Improve relations initialization time - @9too
- #927, Treat blank pre-request as missing - @begriffs

### Changed

- #938, Deprecate symbol operators with mnemonic names. - @russelldavies

## [0.4.2.0] - 2017-06-11

### Added

- #742, Add connection retrying on startup and SIGHUP - @steve-chavez
- #652, Add and/or params for complex boolean logic - @steve-chavez
- #808, Env var interpolation in config file (helps Docker) - @begriffs
- #878 - CSV output support for RPC - @begriffs

### Fixed

- #822, Treat blank string JWT secret as no secret - @begriffs

## [0.4.1.0] - 2017-04-25

### Added
- Allow requesting binary output on GET - @steve-chavez
- Accept clients requesting `Content-Type: application/json` from / - @feynmanliang
- #493, Updating with empty JSON object makes zero updates @koulakis
- Make HTTP headers and cookies available as GUCs #800 - @ruslantalpa
- #701, Ability to quote values on IN filters - @steve-chavez
- #641, Allow IN filter to have no values - @steve-chavez

### Fixed
- #827, Avoid Warp reaper, extend socket timeout to 1 hour - @majorcode
- #791, malformed nested JSON error - @diogob
- Resource embedding in views referencing tables in public schema - @fab1an
- #777, Empty body is allowed when calling a non-parameterized RPC - @koulakis
- #831, Fix proc resource embedding issue with search_path - @steve-chavez
- #547, Use read-only transaction for stable/immutable RPC - @begriffs

## [0.4.0.0] - 2017-01-19

### Added
- Allow test database to be on another host - @dsimunic
- `Prefer: params=single-object` to treat payload as single json argument in RPC - @dsimunic
- Ability to generate an OpenAPI spec - @mainx07, @hudayou, @ruslantalpa, @begriffs
- Ability to generate an OpenAPI spec behind a proxy - @hudayou
- Ability to set addresses to listen on - @hudayou
- Filtering, shaping and embedding with &select for the /rpc path - @ruslantalpa
- Output names of used-defined types (instead of 'USER-DEFINED') - @martingms
- Implement support for singular representation responses for POST/PATCH requests - @ehamberg
- Include RPC endpoints in OpenAPI output - @begriffs, @LogvinovLeon
- Custom request validation with `--pre-request` argument - @begriffs
- Ability to order by jsonb keys - @steve-chavez
- Ability to specify offset for a deeper level - @ruslantalpa
- Ability to use binary base64 encoded secrets - @TrevorBasinger

### Fixed
- Do not apply limit to parent items - @ruslantalpa
- Fix bug in relation detection when selecting parents two levels up by using the name of the FK - @ruslantalpa
- Customize content negotiation per route - @begriffs
- Allow using nulls order without explicit order direction - @steve-chavez
- Fatal error on postgres unsupported version, format supported version in error message - @steve-chavez
- Prevent database memory cosumption by prepared statements caches - @ruslantalpa
- Use specific columns in the RETURNING section - @ruslantalpa
- Fix columns alias for RETURNING - @steve-chavez

### Changed
- Replace `Prefer: plurality=singular` with `Accept: application/vnd.pgrst.object` - @begriffs
- Standardize arrays in responses for `Prefer: return=representation` - @begriffs
- Calling unknown RPC gives 404, not 400 - @begriffs
- Use HTTP 400 for raise\_exception - @begriffs
- Remove non-OpenAPI schema description - @begriffs
- Use comma rather than semicolon to separate Prefer header values - @begriffs
- Omit total query count by default - @begriffs
- No more reserved `jwt_claims` return type - @begriffs
- HTTP 401 rather than 400 for expired JWT - @begriffs
- Remove default JWT secret - @begriffs
- Use GUC request.jwt.claim.foo rather than postgrest.claims.foo - @begriffs
- Use config file rather than command line arguments - @begriffs

## [0.3.2.0] - 2016-06-10

### Added
- Reload database schema on SIGHUP - @begriffs
- Support "-" in column names - @ruslantalpa
- Support column/node renaming `alias:column` - @ruslantalpa
- Accept posts from HTML forms - @begriffs
- Ability to order embedded entities - @ruslantalpa
- Ability to paginate using &limit and &offset parameters - @ruslantalpa
- Ability to apply limits to embedded entities and enforce --max-rows on all levels - @ruslantalpa, @begriffs
- Add allow response header in OPTIONS - @begriffs

### Fixed
- Return 401 or 403 for access denied rather than 404 - @begriffs
- Omit Content-Type header for empty body - @begriffs
- Prevent role from being changed twice - @begriffs
- Use read-only transaction for read requests - @ruslantalpa
- Include entities from the same parent table using two different foreign keys - @ruslantalpa
- Ensure that Location header in 201 response is URL-encoded - @league
- Fix garbage collector CPU leak - @ruslantalpa et al.
- Return deleted items when return=representation header is sent - @ruslantalpa
- Use table default values for empty object inserts - @begriffs

## [0.3.1.1] - 2016-03-28

### Fixed
- Preserve unicode values in insert,update,rpc (regression) - @begriffs
- Prevent duplicate call to stored procs (regression) - @begriffs
- Allow SQL functions to generate registered JWT claims - @begriffs
- Terminate gracefully on SIGTERM (for use in Docker) - @recmo
- Relation detection fix for views that depend on multiple tables - @ruslantalpa
- Avoid count on plurality=singular and allow multiple Prefer values - @ruslantalpa

## [0.3.1.0] - 2016-02-28

### Fixed
- Prevent query error from infecting later connection - @begriffs, @ruslantalpa, @nikita-volkov, @jwiegley

### Added
- Applies range headers to RPC calls - @diogob

## [0.3.0.4] - 2016-02-12

### Fixed
- Improved usage screen - @begriffs
- Reject non-POSTs to rpc endpoints - @begriffs
- Throw an error for OPTIONS on nonexistent tables - @calebmer
- Remove deadlock on simultaneous contentious updates - @ruslantalpa, @begriffs

## [0.3.0.3] - 2016-01-08

### Fixed
- Fix bug in many-many relation detection - @ruslantalpa
- Inconsistent escaping of table names in read queries - @calebmer

## [0.3.0.2] - 2015-12-16

### Fixed
- Miscalculation of time used for expiring tokens - @calebmer
- Remove bcrypt dependency to fix Windows build - @begriffs
- Detect relations event when authenticator does not have rights to intermediate tables - @ruslantalpa
- Ensure db connections released on sigint - @begriffs
- Fix #396 include records with missing parents - @ruslantalpa
- `pgFmtIdent` always quotes #388 - @calebmer
- Default schema, changed from `"1"` to `public` - @calebmer
- #414 revert to separate count query - @ruslantalpa
- Fix #399, allow inserting in tables with no select privileges using "Prefer: representation=minimal" - @ruslantalpa

### Added
- Allow order by computed columns - @diogob
- Set max rows in response with --max-rows - @begriffs
- Selection by column name (can detect if `_id` is not included) - @calebmer

## [0.3.0.1] - 2015-11-27

### Fixed
- Filter columns on embedded parent items - @ruslantalpa

## [0.3.0.0] - 2015-11-24

### Fixed
- Use reasonable amount of memory during bulk inserts - @begriffs

### Added
- Ensure JWT expires - @calebmer
- Postgres connection string argument - @calebmer
- Encode JWT for procs that return type `jwt_claims` - @diogob
- Full text operators `@>`,`<@` - @ruslantalpa
- Shaping of the response body (filter columns, embed relations) with &select parameter for POST/PATCH - @ruslantalpa
- Detect relationships between public views and private tables - @calebmer
- `Prefer: plurality=singular` for selecting single objects - @calebmer

### Removed
- API versioning feature - @calebmer
- `--db-x` command line arguments - @calebmer
- Secure flag - @calebmer
- PUT request handling - @ruslantalpa

### Changed
- Embed foreign keys with {} rather than () - @begriffs
- Remove version number from binary filename in release - @begriffs

## [0.2.12.1] - 2015-11-12

### Fixed
- Correct order for -> and ->> in a json path - @ruslantalpa
- Return empty array instead of 500 when a set returning function returns an empty result set - @diogob

## [0.2.12.0] - 2015-10-25

### Added
- Embed associations, e.g. `/film?select=*,director(*)` - @ruslantalpa
- Filter columns, e.g. `?select=col1,col2` - @ruslantalpa
- Does not execute the count total if header "Prefer: count=none" - @diogob

### Fixed
- Tolerate a missing role in user creation - @calebmer
- Avoid unnecessary text re-encoding - @ruslantalpa

## [0.2.11.1] - 2015-09-01

### Fixed
- Accepts `*/*` in Accept header - @diogob

## [0.2.11.0] - 2015-08-28
### Added
- Negate any filter in a uniform way, e.g. `?col=not.eq=foo` - @diogob
- Call stored procedures
- Filter NOT IN values, e.g. `?col=notin.1,2,3` - @rall
- CSV responses to GET requests with `Accept: text/csv` - @diogob
- Debian init scripts - @mkhon
- Allow filters by computed columns - @diogob

### Fixed
- Reset user role on error
- Compatible with Stack
- Add materialized views to results in GET / - @diogob
- Indicate insertable=true for views that are insertable through triggers - @diogob
- Builds under GHC 7.10
- Allow the use of columns named "count" in relations queried - @diogob

## [0.2.10.0] - 2015-06-03
### Added
- Full text search, eg `/foo?text_vector=@@.bar`
- Include auth id as well as db role to views (for row-level security)

## [0.2.9.1] - 2015-05-20
### Fixed
- Put -Werror behind a cabal flag (for CI) so Hackage accepts package

## [0.2.9.0] - 2015-05-20
### Added
- Return range headers in PATCH
- Return PATCHed resources if header "Prefer: return=representation"
- Allow nested objects and arrays in JSON post for jsonb columns
- JSON Web Tokens - [Federico Rampazzo](https://github.com/framp)
- Expose PostgREST as a Haskell package

### Fixed
- Return 404 if no records updated by PATCH

## [0.2.8.0] - 2015-04-17
### Added
- Option to specify nulls first or last, eg `/people?order=age.desc.nullsfirst`
- Filter nulls, `?col=is.null` and `?col=isnot.null`
- Filter within jsonb, `?col->a->>b=eq.c`
- Accept CSV in post body for bulk inserts

### Fixed
- Allow NULL values in posts
- Show full command line usage on param errors

## [0.2.7.0] - 2015-03-03
### Added
- Server response logging
- Filter IN values, e.g. `?col=in.1,2,3`
- Return POSTed resource if header "Prefer: return=representation"
- Allow override of default (v1) schema

## [0.2.6.0] - 2015-02-18
### Added
- A changelog
- Filter by substring match, e.g. `?col=like.*hello*` (or ilike for
  case insensitivity).
- Access-Control-Expose-Headers for CORS

### Fixed
- Make filter position match docs, e.g. `?order=col.asc` rather
  than `?order=asc.col`.
