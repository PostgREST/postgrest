## pgbench tests

Can be used as:

```
postgrest-with-postgresql-15 -f test/pgbench/fixtures.sql pgbench -n -T 10 -f test/pgbench/2677/old.sql
postgrest-with-postgresql-15 -f test/pgbench/fixtures.sql pgbench -n -T 10 -f test/pgbench/2677/new.sql
```
