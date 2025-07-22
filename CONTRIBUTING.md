# Contributing to PostgREST

**First:** if you're unsure or afraid of _anything_, just ask or
submit the issue or pull request anyways. You won't be yelled at
for giving your best effort. The worst that can happen is that
you'll be politely asked to change something. We appreciate any
sort of contributions, and don't want a wall of rules to get in the
way of that.

However, for those individuals who want a bit more guidance on the
best way to contribute to the project, read on. This document will
cover what we're looking for. By addressing all the points we're
looking for, it raises the chances we can quickly merge or address
your contributions.

## Issues

For questions on how to use PostgREST, please use
[GitHub discussions](https://github.com/PostgREST/postgrest/discussions).

### Reporting an Issue

* Make sure you test against the latest [stable release](https://github.com/PostgREST/postgrest/releases/latest)
  and also against the latest [devel release](https://github.com/PostgREST/postgrest/releases/tag/devel).
  It is possible we already fixed the bug you're experiencing.

* Provide steps to reproduce the issue, including your OS version and
  the specific database schema that you are using.

* Please include SQL logs for issues involving runtime problems. To obtain logs first
  [enable logging all statements](http://www.microhowto.info/howto/log_all_queries_to_a_postgresql_server.html),
  then [find your logs](http://blog.endpoint.com/2014/11/dear-postgresql-where-are-my-logs.html).

* If your database schema has changed while the PostgREST server is running,
  [send the server a `SIGUSR1` signal](http://postgrest.org/en/latest/admin.html#schema-reloading) or restart it to ensure the schema cache
  is not stale. This sometimes fixes apparent bugs.

## Code

We have a fully nix-based development environment with many tools for a smooth development workflow available.
Check the [development docs](https://github.com/PostgREST/postgrest/blob/main/nix/README.md) on how to set it up and use it.

### Haskell Conventions

* All contributions must pass the tests before being merged. When
  you create a pull request your code will automatically be tested.

* All code must also pass [hlint](http://community.haskell.org/~ndm/hlint/) and [stylish-haskell](https://github.com/jaspervdj/stylish-haskell)
  with no warnings. This helps enforce a uniform style for all committers. Continuous integration will check this as well on every
  pull request. There are useful tools in the nix-shell that help with checking this locally. You can run `postgrest-check` to do this manually but
  we recommend adding it to `.git/hooks/pre-commit` as `nix-shell --run postgrest-check` to automatically check this before doing a commit.

### Running Tests

For instructions on running tests, see the [development docs](https://github.com/PostgREST/postgrest/blob/main/nix/README.md#testing).
