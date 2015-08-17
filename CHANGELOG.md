# Change Log

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## Unreleased
### Added
- Filter NOT IN values, e.g. `?col=notin.1,2,3` - @rall
- CSV responses to GET requests with `Accept: text/csv` - @diogob
- Debian init scripts - @mkhon
- Allow filters by computed columns - @diogob

### Fixed
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
