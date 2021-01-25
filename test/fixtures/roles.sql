\set AUTHENTICATOR current_user
DROP ROLE IF EXISTS postgrest_test_anonymous, postgrest_test_default_role, postgrest_test_author;
CREATE ROLE postgrest_test_anonymous;
CREATE ROLE postgrest_test_default_role;
CREATE ROLE postgrest_test_author;

GRANT postgrest_test_anonymous, postgrest_test_default_role, postgrest_test_author TO :USER;

-- reloadable config options for io tests
ALTER ROLE postgrest_test_authenticator SET pgrst."jwt-aud" = 'https://example.org';
ALTER ROLE postgrest_test_authenticator SET pgrst."openapi-server-proxy-uri" = 'https://example.org/api';
ALTER ROLE postgrest_test_authenticator SET pgrst."raw-media-types" = 'application/vnd.pgrst.db-config';
ALTER ROLE postgrest_test_authenticator SET pgrst."jwt-secret" = 'REALLYREALLYREALLYREALLYVERYSAFE';
ALTER ROLE postgrest_test_authenticator SET pgrst."jwt-secret-is-base64" = 'true';
ALTER ROLE postgrest_test_authenticator SET pgrst."jwt-role-claim-key" = '."a"."role"';
ALTER ROLE postgrest_test_authenticator SET pgrst."db-tx-end" = 'commit-allow-override';
ALTER ROLE postgrest_test_authenticator SET pgrst."db-schemas" = 'test, tenant1, tenant2';
ALTER ROLE postgrest_test_authenticator SET pgrst."db-root-spec" = 'root';
ALTER ROLE postgrest_test_authenticator SET pgrst."db-prepared-statements" = 'false';
ALTER ROLE postgrest_test_authenticator SET pgrst."db-pre-request" = 'test.custom_headers';
ALTER ROLE postgrest_test_authenticator SET pgrst."db-max-rows" = '1000';
ALTER ROLE postgrest_test_authenticator SET pgrst."db-extra-search-path" = 'public, extensions';

-- non-reloadable configs for io tests
ALTER ROLE postgrest_test_authenticator SET pgrst."server-host" = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst."server-port" = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst."server-unix-socket" = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst."server-unix-socket-mode" = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst."log-level" = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst."db-anon-role" = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst."db-uri" = 'postgresql://ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst."db-channel-enabled" = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst."db-channel" = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst."db-pool" = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst."db-pool-timeout" = 'ignored';
ALTER ROLE postgrest_test_authenticator SET pgrst."db-load-guc-config" = 'ignored';

-- other authenticator reloadable config options for io tests
CREATE ROLE other_authenticator LOGIN NOINHERIT;
ALTER ROLE other_authenticator SET pgrst."jwt-aud" = 'https://otherexample.org';
ALTER ROLE other_authenticator SET pgrst."openapi-server-proxy-uri" = 'https://otherexample.org/api';
ALTER ROLE other_authenticator SET pgrst."raw-media-types" = 'application/vnd.pgrst.other-db-config';
ALTER ROLE other_authenticator SET pgrst."jwt-secret" = 'ODERREALLYREALLYREALLYREALLYVERYSAFE';
ALTER ROLE other_authenticator SET pgrst."jwt-secret-is-base64" = 'true';
ALTER ROLE other_authenticator SET pgrst."jwt-role-claim-key" = '."other"."role"';
ALTER ROLE other_authenticator SET pgrst."db-tx-end" = 'rollback-allow-override';
ALTER ROLE other_authenticator SET pgrst."db-schemas" = 'test, other_tenant1, other_tenant2';
ALTER ROLE other_authenticator SET pgrst."db-root-spec" = 'other_root';
ALTER ROLE other_authenticator SET pgrst."db-prepared-statements" = 'false';
ALTER ROLE other_authenticator SET pgrst."db-pre-request" = 'test.other_custom_headers';
ALTER ROLE other_authenticator SET pgrst."db-max-rows" = '100';
ALTER ROLE other_authenticator SET pgrst."db-extra-search-path" = 'public, extensions, other';
