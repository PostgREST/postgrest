\set AUTHENTICATOR current_user
DROP ROLE IF EXISTS postgrest_test_anonymous, postgrest_test_default_role, postgrest_test_author;
CREATE ROLE postgrest_test_anonymous;
CREATE ROLE postgrest_test_default_role;
CREATE ROLE postgrest_test_author;

GRANT postgrest_test_anonymous, postgrest_test_default_role, postgrest_test_author TO :USER;
