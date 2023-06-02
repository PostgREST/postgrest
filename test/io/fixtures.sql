-- \ir big_schema.sql big schema test currently skipped, see test_io.py
\ir db_config.sql

set search_path to public;

CREATE ROLE postgrest_test_anonymous;
ALTER ROLE :PGUSER SET pgrst.db_anon_role = 'postgrest_test_anonymous';

CREATE ROLE postgrest_test_author;

CREATE ROLE postgrest_test_serializable;
alter role postgrest_test_serializable set default_transaction_isolation = 'serializable';

CREATE ROLE postgrest_test_repeatable_read;
alter role postgrest_test_repeatable_read set default_transaction_isolation = 'REPEATABLE READ';

GRANT postgrest_test_anonymous, postgrest_test_author, postgrest_test_serializable, postgrest_test_repeatable_read TO :PGUSER;

CREATE SCHEMA v1;
GRANT USAGE ON SCHEMA v1 TO postgrest_test_anonymous;

CREATE TABLE authors_only ();
GRANT SELECT ON authors_only TO postgrest_test_author;

CREATE TABLE projects AS SELECT FROM generate_series(1,5);
GRANT SELECT ON projects TO postgrest_test_anonymous;

create function get_guc_value(name text) returns text as $$
  select nullif(current_setting(name), '')::text;
$$ language sql;

create function v1.get_guc_value(name text) returns text as $$
  select nullif(current_setting(name), '')::text;
$$ language sql;

create function uses_prepared_statements() returns bool as $$
  select count(name) > 0 from pg_catalog.pg_prepared_statements
$$ language sql;

create function change_max_rows_config(val int, notify bool default false) returns void as $_$
begin
  execute format($$
    alter role postgrest_test_authenticator set pgrst.db_max_rows = %L;
  $$, val);
  if notify then
    perform pg_notify('pgrst', 'reload config');
  end if;
end $_$ volatile security definer language plpgsql ;

create function reset_max_rows_config() returns void as $_$
begin
  alter role postgrest_test_authenticator reset pgrst.db_max_rows;
end $_$ volatile security definer language plpgsql ;

create function change_db_schema_and_full_reload(schemas text) returns void as $_$
begin
  execute format($$
    alter role postgrest_test_authenticator set pgrst.db_schemas = %L;
  $$, schemas);
  perform pg_notify('pgrst', 'reload config');
  perform pg_notify('pgrst', 'reload schema');
end $_$ volatile security definer language plpgsql ;

create function v1.reset_db_schema_config() returns void as $_$
begin
  alter role postgrest_test_authenticator reset pgrst.db_schemas;
  perform pg_notify('pgrst', 'reload config');
  perform pg_notify('pgrst', 'reload schema');
end $_$ volatile security definer language plpgsql ;

create function invalid_role_claim_key_reload() returns void as $_$
begin
  alter role postgrest_test_authenticator set pgrst.jwt_role_claim_key = 'test';
  perform pg_notify('pgrst', 'reload config');
end $_$ volatile security definer language plpgsql ;

create function reset_invalid_role_claim_key() returns void as $_$
begin
  alter role postgrest_test_authenticator reset pgrst.jwt_role_claim_key;
  perform pg_notify('pgrst', 'reload config');
end $_$ volatile security definer language plpgsql ;

create function reload_pgrst_config() returns void as $_$
begin
  perform pg_notify('pgrst', 'reload config');
end $_$ language plpgsql ;

create or replace function sleep(seconds double precision) returns void as $$
  select pg_sleep(seconds);
$$ language sql;

create or replace function hello() returns text as $$
  select 'hello'::text;
$$ language sql;

create table cats(id uuid primary key, name text);
grant all on cats to postgrest_test_anonymous;

create function drop_change_cats() returns void
language sql security definer
as $$
  drop table cats;
  create table cats(id bigint primary key, name text);
  grant all on table cats to postgrest_test_anonymous;
  notify pgrst, 'reload schema';
$$;

alter role postgrest_test_anonymous set statement_timeout to '2s';
alter role postgrest_test_author set statement_timeout to '10s';

create function change_role_statement_timeout(timeout text) returns void as $_$
begin
  execute format($$
    alter role current_user set statement_timeout = %L;
  $$, timeout);
end $_$ volatile language plpgsql ;

create table items as select x as id from generate_series(1,5) x;

create view items_w_isolation_level as
select
  id,
  current_setting('transaction_isolation', true) as isolation_level
from items;

grant all on items_w_isolation_level to postgrest_test_anonymous, postgrest_test_repeatable_read, postgrest_test_serializable;

create function default_isolation_level()
returns text as $$
  select current_setting('transaction_isolation', true);
$$
language sql;

create function serializable_isolation_level()
returns text as $$
  select current_setting('transaction_isolation', true);
$$
language sql set default_transaction_isolation = 'serializable';

create function repeatable_read_isolation_level()
returns text as $$
  select current_setting('transaction_isolation', true);
$$
language sql set default_transaction_isolation = 'REPEATABLE READ';

create or replace function create_function() returns void as $_$
  drop function if exists mult_them(int, int);
  create or replace function mult_them(a int, b int) returns int as $$
    select a*b;
  $$ language sql;
  notify pgrst, 'reload schema';
$_$ language sql security definer;

create or replace function migrate_function() returns void as $_$
  drop function if exists mult_them(int, int);
  create or replace function mult_them(c int, d int) returns int as $$
    select c*d;
  $$ language sql;
  notify pgrst, 'reload schema';
$_$ language sql security definer;
