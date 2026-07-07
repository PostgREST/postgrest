# 1.2

- Removed the `unpreparedTransaction` session because the same effects can now be achieved via the connection settings in Hasql

# 1.1

- Add automatic retry on deadlock errors (code 40P01)
