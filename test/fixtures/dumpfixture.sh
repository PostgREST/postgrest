pg_dump --host localhost --port 5432 --username "postgres" --no-password  --format plain --no-owner --column-inserts --verbose --file "./test/fixtures/schema.sql" "dbapi_test"
