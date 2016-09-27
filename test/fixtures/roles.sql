DROP ROLE IF EXISTS postgrest_test_authenticator, postgrest_test_anonymous, postgrest_test_default_role, postgrest_test_author, bad_role;
CREATE ROLE postgrest_test_authenticator WITH login noinherit;
CREATE ROLE postgrest_test_anonymous;
CREATE ROLE postgrest_test_default_role;
CREATE ROLE postgrest_test_author;
CREATE ROLE bad_role;

GRANT postgrest_test_anonymous, postgrest_test_default_role, postgrest_test_author, bad_role TO postgrest_test_authenticator;
