CREATE ROLE db_config_authenticator LOGIN NOINHERIT;

-- reloadable config options
ALTER ROLE db_config_authenticator SET pgrst.jwt_aud = 'https://example.org';
ALTER ROLE db_config_authenticator SET pgrst.openapi_server_proxy_uri = 'https://example.org/api';
ALTER ROLE db_config_authenticator SET pgrst.raw_media_types = 'application/vnd.pgrst.db-config';
ALTER ROLE db_config_authenticator SET pgrst.jwt_secret = 'REALLY=REALLY=REALLY=REALLY=VERY=SAFE';
ALTER ROLE db_config_authenticator SET pgrst.jwt_secret_is_base64 = 'false';
ALTER ROLE db_config_authenticator SET pgrst.jwt_role_claim_key = '."a"."role"';
ALTER ROLE db_config_authenticator SET pgrst.db_anon_role = 'anonymous';
ALTER ROLE db_config_authenticator SET pgrst.db_tx_end = 'commit-allow-override';
ALTER ROLE db_config_authenticator SET pgrst.db_pre_config = 'postgrest.preconf';
ALTER ROLE db_config_authenticator SET pgrst.db_schemas = 'test, tenant1, tenant2';
ALTER ROLE db_config_authenticator SET pgrst.db_root_spec = 'root';
ALTER ROLE db_config_authenticator SET pgrst.db_plan_enabled = 'true';
ALTER ROLE db_config_authenticator SET pgrst.db_prepared_statements = 'false';
ALTER ROLE db_config_authenticator SET pgrst.db_pre_request = 'test.custom_headers';
ALTER ROLE db_config_authenticator SET pgrst.db_max_rows = '1000';
ALTER ROLE db_config_authenticator SET pgrst.db_extra_search_path = 'public, extensions';
ALTER ROLE db_config_authenticator SET pgrst.not_existing = 'should be ignored';
ALTER ROLE db_config_authenticator SET pgrst.server_trace_header = 'CF-Ray';

-- override with database specific setting
ALTER ROLE db_config_authenticator IN DATABASE :DBNAME SET pgrst.jwt_secret = 'OVERRIDE=REALLY=REALLY=REALLY=REALLY=VERY=SAFE';
ALTER ROLE db_config_authenticator IN DATABASE :DBNAME SET pgrst.db_extra_search_path = 'public, extensions, private';
ALTER ROLE db_config_authenticator IN DATABASE :DBNAME SET pgrst.not_existing = 'should be ignored';

-- other database settings that should be ignored
CREATE DATABASE other;
ALTER ROLE db_config_authenticator IN DATABASE other SET pgrst.db_max_rows = '1111';

-- non-reloadable configs
ALTER ROLE db_config_authenticator SET pgrst.server_host = 'ignored';
ALTER ROLE db_config_authenticator SET pgrst.server_port = 'ignored';
ALTER ROLE db_config_authenticator SET pgrst.server_unix_socket = 'ignored';
ALTER ROLE db_config_authenticator SET pgrst.server_unix_socket_mode = 'ignored';
ALTER ROLE db_config_authenticator SET pgrst.admin_server_port = 'ignored';
ALTER ROLE db_config_authenticator SET pgrst.log_level = 'ignored';
ALTER ROLE db_config_authenticator SET pgrst.db_uri = 'postgresql://ignored';
ALTER ROLE db_config_authenticator SET pgrst.db_channel_enabled = 'ignored';
ALTER ROLE db_config_authenticator SET pgrst.db_channel = 'ignored';
ALTER ROLE db_config_authenticator SET pgrst.db_pool = 'ignored';
ALTER ROLE db_config_authenticator SET pgrst.db_pool_timeout = 'ignored';
ALTER ROLE db_config_authenticator SET pgrst.db_pool_acquisition_timeout = 'ignored';
ALTER ROLE db_config_authenticator SET pgrst.db_pool_max_lifetime = 'ignored';
ALTER ROLE db_config_authenticator SET pgrst.db_pool_max_idletime = 'ignored';
ALTER ROLE db_config_authenticator SET pgrst.db_config = 'true';

-- other authenticator reloadable config options
CREATE ROLE other_authenticator LOGIN NOINHERIT;
ALTER ROLE other_authenticator SET pgrst.jwt_aud = 'https://otherexample.org';
ALTER ROLE other_authenticator SET pgrst.openapi_server_proxy_uri = 'https://otherexample.org/api';
ALTER ROLE other_authenticator SET pgrst.raw_media_types = 'application/vnd.pgrst.other-db-config';
ALTER ROLE other_authenticator SET pgrst.jwt_secret = 'ODERREALLYREALLYREALLYREALLYVERYSAFE';
ALTER ROLE other_authenticator SET pgrst.jwt_secret_is_base64 = 'true';
ALTER ROLE other_authenticator SET pgrst.db_schemas = 'test, other_tenant1, other_tenant2';
ALTER ROLE other_authenticator SET pgrst.db_root_spec = 'other_root';
ALTER ROLE other_authenticator SET pgrst.db_plan_enabled = 'true';
ALTER ROLE other_authenticator SET pgrst.db_prepared_statements = 'false';
ALTER ROLE other_authenticator SET pgrst.db_pre_request = 'test.other_custom_headers';
ALTER ROLE other_authenticator SET pgrst.db_max_rows = '100';
ALTER ROLE other_authenticator SET pgrst.db_extra_search_path = 'public, extensions, other';
ALTER ROLE other_authenticator SET pgrst.openapi_mode = 'disabled';
ALTER ROLE other_authenticator SET pgrst.openapi_security_active = 'false';
ALTER ROLE other_authenticator SET pgrst.server_trace_header = 'traceparent';
ALTER ROLE other_authenticator SET pgrst.db_pre_config = 'postgrest.pre_config';

create schema postgrest;
grant usage on schema postgrest to db_config_authenticator;
grant usage on schema postgrest to other_authenticator;

-- pre-config hook
create or replace function postgrest.pre_config()
returns void as $$
begin
  if current_user = 'other_authenticator' then
    perform
      set_config('pgrst.jwt_role_claim_key', '."other"."pre_config_role"', true)
    , set_config('pgrst.db_anon_role', 'pre_config_role', true)
    , set_config('pgrst.db_schemas', 'will be overriden with the above ALTER ROLE.. db_schemas', true)
    , set_config('pgrst.db_tx_end', 'rollback-allow-override', true);
  else
    null;
  end if;
end $$ language plpgsql;

create or replace function postgrest.preconf()
returns void as $$
begin
  null;
end $$ language plpgsql;

-- authenticator used for tests that manipulate statement timeout
CREATE ROLE timeout_authenticator LOGIN NOINHERIT;

create function set_statement_timeout(role text, milliseconds int) returns void as $_$
begin
  execute format($$
    alter role %I set statement_timeout to %L;
  $$, role, milliseconds);
end $_$ volatile security definer language plpgsql;

-- authenticator used for test-independent database manipulation
CREATE ROLE meta_authenticator LOGIN NOINHERIT;
