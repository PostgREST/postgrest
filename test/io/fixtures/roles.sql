DROP ROLE IF EXISTS
  postgrest_test_anonymous, postgrest_test_author,
  postgrest_test_serializable, postgrest_test_repeatable_read,
  postgrest_test_w_superuser_settings;

CREATE ROLE postgrest_test_anonymous;
CREATE ROLE postgrest_test_author;
CREATE ROLE postgrest_test_serializable;
CREATE ROLE postgrest_test_repeatable_read;
CREATE ROLE postgrest_test_w_superuser_settings;

GRANT
  postgrest_test_anonymous, postgrest_test_author,
  postgrest_test_serializable, postgrest_test_repeatable_read,
  postgrest_test_w_superuser_settings TO :PGUSER;

ALTER ROLE :PGUSER SET pgrst.db_anon_role = 'postgrest_test_anonymous';
ALTER ROLE postgrest_test_serializable SET default_transaction_isolation = 'serializable';
ALTER ROLE postgrest_test_repeatable_read SET default_transaction_isolation = 'REPEATABLE READ';

ALTER ROLE postgrest_test_w_superuser_settings SET log_min_duration_statement = 1;
ALTER ROLE postgrest_test_w_superuser_settings SET log_min_messages = 'fatal';

ALTER ROLE postgrest_test_anonymous SET statement_timeout TO '2s';
ALTER ROLE postgrest_test_author SET statement_timeout TO '10s';
