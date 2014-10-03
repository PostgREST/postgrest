## Serve a RESTful API from any Postgres database

[![Build Status](https://travis-ci.org/begriffs/dbapi.svg?branch=master)](https://travis-ci.org/begriffs/dbapi)

### Installation

```sh
brew install postgres
cabal install -j --enable-tests
```

Example usage:

```sh
cabal run -p 3000 -d postgres://[auth-role]:@localhost:5432/[database] -a [anonymous-role]
```

You will need to provide two database roles (which are allowed to
be the same). One is called the authenticator role (`auth-role`
above) which should have enough privileges to read the `auth` table
in the `dbapi` schema if you intend to support multi-user applications.

The other role is for anonymous access (`anonymous-role` above).
Immediately upon acceping any unauthenticated HTTP connection dbapi
assumes this role in its queries to postgres. Give this role as
much or little permissions as you would like.

### Running tests

```sh
createuser --superuser --no-password dbapi_test
createdb -O dbapi_test -U postgres dbapi_test

cabal test --show-details=always --test-options="--color"
```

### Accessing server on localhost

Dbapi permits only secure https access. By default it uses a
self-signed key which will cause Chrome and other rest clients to
complain. You will need to configure your system to trust this key.
On mac follow [these
instructions](http://www.robpeck.com/2010/10/google-chrome-mac-os-x-and-self-signed-ssl-certificates/).
