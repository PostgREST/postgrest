# Change Log

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## unreleased

- Return 404 if no records updated by PATCH
- Return range headers in PATCH
- Return PATCHed resources if header "Prefer: return=representation"
- Allow nested objects and arrays in JSON post for jsonb columns

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
