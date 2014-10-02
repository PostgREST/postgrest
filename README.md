## Serve a RESTful API from any Postgres database

[![Build Status](https://travis-ci.org/begriffs/dbapi.svg?branch=master)](https://travis-ci.org/begriffs/dbapi)

### Installation

```sh
brew install postgres
cabal install -j --enable-tests
```

Example usage:

```sh
dbapi -p 3000 -d postgres://[user]:@localhost:5432/[database]
```

### Running tests

```sh
createuser --superuser --no-password dbapi_test
createdb -O dbapi_test -U postgres dbapi_test

cabal test --show-details=always --test-options="--color"
```
