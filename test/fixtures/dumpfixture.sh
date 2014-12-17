pg_dump --host localhost --port 5432 --username "postgres" --no-password  --format plain --column-inserts --verbose --file "./test/fixtures/schema.sql" "postgrest_test"
