## Serve a RESTful API from any Postgres database

[![Build Status](https://travis-ci.org/begriffs/dbapi.svg)](https://travis-ci.org/begriffs/dbapi)

### Installation

```sh
brew install postgres
createuser -d postgres
cabal install -j --enable-tests --reorder-goals
```

Example usage:

```sh
dbapi -p 3000 -d postgres://postgres:@localhost:5432/dbapi_test
```

### Running tests

```sh
cabal test
```
