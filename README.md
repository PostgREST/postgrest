## Serve a RESTful API from any Postgres database

![Build Status](https://circleci.com/gh/begriffs/postgrest.png?circle-token=f723c01686abf0364de1e2eaae5aff1f68bd3ff2)

### Installation

```sh
brew install postgres
cabal install -j --enable-tests
```

Example usage:

```sh
cabal run -d [database] -U [auth-role] -a [anonymous-role]
```

This will connect to a postgres DB at the url
`postgres://[auth-role]:@localhost:5432/[database]`.

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

### Distributing Heroku build

```sh
heroku create --stack=cedar --buildpack https://github.com/begriffs/heroku-buildpack-ghc.git
git push heroku master

heroku config:set S3_ACCESS_KEY=abc
heroku config:set S3_SECRET_KEY=123
heroku config:set S3_BUCKET=s3://foo/bar

heroku run scripts/release_s3.sh
```

### Acknowledgements

Thanks to [Adam Baker](https://github.com/adambaker) for code contributions and many fundamental design discussions. Also thanks to [Loop/Recur](https://looprecur.com) for open-source Fridays to advance the code, and for their courage to use this thing in real projects.
