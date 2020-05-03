
To validate and benchmark the query on each change:

```
ag -l | entr test/with_tmp_db dbstructure/benchmark
```

