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

### Reporting an Issue

* Make sure you test against the latest released version. It is possible
  we already fixed the bug you're experiencing.

* Also check the `CHANGELOG.md` to see if any unreleased changes affect
  the issue. The very newest changes can take a little while to be released
  as a new official version.

* Provide steps to reproduce the issue, including your OS version and
  the specific database schema that you are using.

* Please include SQL logs for issues involving runtime problems. To obtain logs first
  [enable logging all statements](http://www.microhowto.info/howto/log_all_queries_to_a_postgresql_server.html),
  then [find your logs](http://blog.endpoint.com/2014/11/dear-postgresql-where-are-my-logs.html).

* If your database schema has changed while the PostgREST server is running,
  send the server a `SIGHUP` signal or restart it to ensure the schema cache
  is not stale. This sometimes fixes apparent bugs.

## Code

### Haskell Conventions

* All contributions must pass the tests before being merged. When
  you create a pull request your code will automatically be tested.

* All code must also pass [hlint](http://community.haskell.org/~ndm/hlint/)
  with no warnings. This helps enforce a uniform style for all
  committers. Continuous integration will check this as well on every
  pull request.

* For help building the Haskell code on your computer check out the [building from
source](https://postgrest.com/en/stable/install.html#build-from-source)
wiki page.

### Running Tests

For instructions on running tests, see the official docs hosted here:

https://postgrest.com/en/stable/install.html#postgrest-test-suite
