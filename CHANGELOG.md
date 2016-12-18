# Change Log

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## Unreleased

### Added
- Allow test database to be on another host - @dsimunic
- New `Prefer` header value: `params=single-object` to pass all form values as a single json object to a stored procedure - @dsimunic
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

### Changed
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
