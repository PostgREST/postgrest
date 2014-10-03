## Serve a RESTful API from any Postgres database

[![Build Status](https://travis-ci.org/begriffs/dbapi.svg?branch=master)](https://travis-ci.org/begriffs/dbapi)

### Installation

```sh
brew install postgres
cabal install -j --enable-tests
```

Example usage:

```sh
cabal run -- -p 3000 -d postgres://[user]:@localhost:5432/[database] -a [role]
```

### Running tests

```sh
createuser --superuser --no-password dbapi_test
createdb -O dbapi_test -U postgres dbapi_test

cabal test --show-details=always --test-options="--color"
```

# TEST SSL CERT
The test certificate lives in this project. One solution for testing is to grant access at the OS level:
http://www.robpeck.com/2010/10/google-chrome-mac-os-x-and-self-signed-ssl-certificates/

In chrome, I also had to click "proceed unsafe" as well once visiting the url
