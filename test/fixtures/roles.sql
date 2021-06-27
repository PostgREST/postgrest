\set AUTHENTICATOR current_user
DROP ROLE IF EXISTS postgrest_test_anonymous, postgrest_test_default_role, postgrest_test_author;
CREATE ROLE postgrest_test_anonymous;
CREATE ROLE postgrest_test_default_role;
CREATE ROLE postgrest_test_author;

GRANT postgrest_test_anonymous, postgrest_test_default_role, postgrest_test_author TO :USER;

-- reloadable config options for io tests
ALTER ROLE postgrest_test_authenticator SET pgrst.jwt_aud = 'https://example.org';
ALTER ROLE postgrest_test_authenticator SET pgrst.openapi_server_proxy_uri = 'https://example.org/api';
ALTER ROLE postgrest_test_authenticator SET pgrst.raw_media_types = 'application/vnd.pgrst.db-config';
ALTER ROLE postgrest_test_authenticator SET pgrst.jwt_secret = 'REALLYREALLYREALLYREALLYVERYSAFE';
ALTER ROLE postgrest_test_authenticator SET pgrst.jwt_secret_is_base64 = 'true';
ALTER ROLE postgrest_test_authenticator SET pgrst.jwt_role_claim_key = '."a"."role"';
ALTER ROLE postgrest_test_authenticator SET pgrst.db_tx_end = 'commit-allow-override';
ALTER ROLE postgrest_test_authenticator SET pgrst.db_schemas = 'test, tenant1, tenant2';
ALTER ROLE postgrest_test_authenticator SET pgrst.db_root_spec = 'root';
ALTER ROLE postgrest_test_authenticator SET pgrst.db_prepared_statements = 'false';
ALTER ROLE postgrest_test_authenticator SET pgrst.db_pre_request = 'test.custom_headers';
ALTER ROLE postgrest_test_authenticator SET pgrst.db_max_rows = '1000';
ALTER ROLE postgrest_test_authenticator SET pgrst.db_extra_search_path = 'public, extensions';

-- override with database specific setting
ALTER ROLE postgrest_test_authenticator IN DATABASE :DBNAME SET pgrst.jwt_secret = 'OVERRIDEREALLYREALLYREALLYREALLYVERYSAFE';
ALTER ROLE postgrest_test_authenticator IN DATABASE :DBNAME SET pgrst.db_extra_search_path = 'public, extensions, private';

-- other database settings that should be ignored
DROP DATABASE IF EXISTS other;
CREATE DATABASE other;
ALTER ROLE postgrest_test_authenticator IN DATABASE other SET pgrst.db_max_rows = '1111';

-- non-reloadable configs for io tests
ALTER ROLE postgrest_test_authenticator SET pgrst.server_host = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst.server_port = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst.server_unix_socket = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst.server_unix_socket_mode = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst.log_level = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst.db_anon_role = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst.db_uri = 'postgresql://ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst.db_channel_enabled = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst.db_channel = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst.db_pool = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst.db_pool_timeout = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst.db_config = 'ignored';

-- other authenticator reloadable config options for io tests
CREATE ROLE other_authenticator LOGIN NOINHERIT;
ALTER ROLE other_authenticator SET pgrst.jwt_aud = 'https://otherexample.org';
ALTER ROLE other_authenticator SET pgrst.openapi_server_proxy_uri = 'https://otherexample.org/api';
ALTER ROLE other_authenticator SET pgrst.raw_media_types = 'application/vnd.pgrst.other-db-config';
ALTER ROLE other_authenticator SET pgrst.jwt_secret = 'ODERREALLYREALLYREALLYREALLYVERYSAFE';
ALTER ROLE other_authenticator SET pgrst.jwt_secret_is_base64 = 'true';
ALTER ROLE other_authenticator SET pgrst.jwt_role_claim_key = '."other"."role"';
ALTER ROLE other_authenticator SET pgrst.db_tx_end = 'rollback-allow-override';
ALTER ROLE other_authenticator SET pgrst.db_schemas = 'test, other_tenant1, other_tenant2';
ALTER ROLE other_authenticator SET pgrst.db_root_spec = 'other_root';
ALTER ROLE other_authenticator SET pgrst.db_prepared_statements = 'false';
ALTER ROLE other_authenticator SET pgrst.db_pre_request = 'test.other_custom_headers';
ALTER ROLE other_authenticator SET pgrst.db_max_rows = '100';
ALTER ROLE other_authenticator SET pgrst.db_extra_search_path = 'public, extensions, other';
ALTER ROLE other_authenticator SET pgrst.openapi_mode = 'disabled';
