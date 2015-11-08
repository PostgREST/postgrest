-------------------------------------------------------------------------------
-- Adapted from https://github.com/robconery/pg-auth

begin;

create extension if not exists pgcrypto;
create extension if not exists "uuid-ossp";
create schema if not exists basic_auth;

-------------------------------------------------------------------------------
-- Utility functions

create or replace function
basic_auth.clearance_for_role(u name) returns void as
$$
declare
  ok boolean;
begin
  select exists (
    select rolname
      from pg_authid
     where pg_has_role(current_user, oid, 'member')
       and rolname = u
  ) into ok;
  if not ok then
    raise invalid_password using message =
      'current user not member of role ' || u;
  end if;
end
$$ LANGUAGE plpgsql;

-------------------------------------------------------------------------------
-- Login storage and constraints

create table if not exists
basic_auth.logins (
  username character varying not null,
  pass   character(60) not null,
  role   name not null,
  email  character varying not null unique,
  active boolean not null default false,
  more   JSON,
  constraint l_pkey primary key (username)
);

create or replace function
basic_auth.check_role_exists() returns trigger
  language plpgsql
  as $$
begin
  if not exists (select 1 from pg_roles as r where r.rolname = new.role) then
    raise foreign_key_violation using message =
      'unknown database role: ' || new.role;
    return null;
  end if;
  return new;
end
$$;

drop trigger if exists ensure_login_role_exists on basic_auth.logins;
create constraint trigger ensure_login_role_exists
  after insert or update on basic_auth.logins
  for each row
  execute procedure basic_auth.check_role_exists();

create or replace function
basic_auth.encrypt_pass() returns trigger
  language plpgsql
  as $$
begin
  if tg_op = 'INSERT' or new.pass <> old.pass then
    new.pass = crypt(new.pass, gen_salt('bf'));
  end if;
  return new;
end
$$;

drop trigger if exists protect_passwords on basic_auth.logins;
create trigger protect_passwords
  before insert or update on basic_auth.logins
  for each row
  execute procedure basic_auth.encrypt_pass();

create or replace function
basic_auth.send_validation() returns trigger
  language plpgsql
  as $$
declare
  tok character varying;
begin
  select uuid_generate_v4() into tok;
  insert into basic_auth.tokens (token, token_type, username)
         values (tok, 'validation', new.username);
  perform pg_notify('validate',
    json_build_object(
      'email', new.email,
      'username', new.username,
      'token', tok,
      'token_type', 'validation'
    )::text
  );
  return new;
end
$$;

drop trigger if exists send_validation_t on basic_auth.logins;
create trigger send_validation_t
  after insert on basic_auth.logins
  for each row
  execute procedure basic_auth.send_validation();

-------------------------------------------------------------------------------
-- Email Validation and Password Reset

create table if not exists
basic_auth.tokens (
  token       character varying unique,
  token_type  varchar(64) not null,
  username    character varying not null,
  created_at  timestamptz not null default current_date,
  constraint  t_pk primary key (token),
  constraint  t_login_fk foreign key (username) references basic_auth.logins
              on delete cascade on update cascade
);

-------------------------------------------------------------------------------
-- Login helper

create or replace function
basic_auth.login_role(username text, pass text) returns text
  language plpgsql
  as $$
begin
  return (
  select role from basic_auth.logins
   where logins.username = login_role.username
     and logins.pass = crypt(login_role.pass, logins.pass)
  );
end;
$$;

-------------------------------------------------------------------------------
-- Public functions (in current schema, not basic_auth)

create or replace function
request_password_reset(username text) returns void
  language plpgsql
  as $$
declare
  tok character varying;
begin
  delete from basic_auth.tokens
   where token_type = 'reset'
     and tokens.username = request_password_reset.username;

  select uuid_generate_v4() into tok;
  insert into basic_auth.tokens (token, token_type, username)
         values (tok, 'reset', request_password_reset.username);
  perform pg_notify('reset',
    json_build_object(
      'email', (select email
                  from basic_auth.logins
                 where logins.username = request_password_reset.username),
      'username', request_password_reset.username,
      'token', tok,
      'token_type', 'reset'
    )::text
  );
end;
$$;

create or replace function
reset_password(username text, token text, pass text)
  returns void
  language plpgsql
  as $$
declare
  tok character varying;
begin
  if exists(select 1 from basic_auth.tokens
             where tokens.username = reset_password.username
               and tokens.token = reset_password.token
               and token_type = 'reset') then
    update basic_auth.logins set pass=reset_password.pass
     where logins.username = reset_password.username;

    delete from basic_auth.tokens
     where tokens.username = reset_password.username
       and tokens.token = reset_password.token
       and token_type = 'reset';
  else
    raise invalid_password using message =
      'invalid user or token';
  end if;
  delete from basic_auth.tokens
   where token_type = 'reset'
     and tokens.username = reset_password.username;

  select uuid_generate_v4() into tok;
  insert into basic_auth.tokens (token, token_type, username)
         values (tok, 'reset', reset_password.username);
  perform pg_notify('reset',
    json_build_object(
      'email', (select email
                  from basic_auth.logins
                 where logins.username = reset_password.username),
      'username', reset_password.username,
      'token', tok
    )::text
  );
end;
$$;

drop type if exists basic_auth.jwt_claims cascade;
create type
basic_auth.jwt_claims AS (role text, username text);

create or replace function
create_auth_token(username text, pass text) returns basic_auth.jwt_claims
  language plpgsql
  as $$
declare
  _role character varying;
  result basic_auth.jwt_claims;
begin
  select basic_auth.login_role(username, pass) into _role;
  if _role is null then
    raise invalid_password using message = 'invalid user or password';
  end if;
  select _role as role, create_auth_token.username as username into result;
  return result;
end;
$$;

-------------------------------------------------------------------------------
-- User management

create or replace view logins as
select actual.username as username,
       actual.role as role,
       '***'::text as pass,
       actual.email as email,
       actual.active as active,
       actual.more as more
from basic_auth.logins as actual,
     (select rolname
        from pg_authid
       where pg_has_role(current_user, oid, 'member')
     ) as member_of
where actual.role = member_of.rolname;

create or replace function
update_logins() returns trigger
language plpgsql
AS $$
begin
  if tg_op = 'INSERT' then
    perform basic_auth.clearance_for_role(new.role);

    insert into basic_auth.logins
      (username, role, pass, email, active, more) values
      (new.username, new.role, new.pass, new.email,
       new.active, new.more);
    return new;
  elsif tg_op = 'UPDATE' then
    -- no need to check clearance for old.role because
    -- an ineligible row would not even available to update (http 404)
    perform basic_auth.clearance_for_role(new.role);

    update basic_auth.logins set
      username = new.username, role  = new.role,
      pass     = new.pass,     email = new.email,
      active   = new.active,   more  = new.more
      where username = old.username;
    return new;
  elsif tg_op = 'DELETE' then
    -- no need to check clearance for old.role (see previous case)

    delete from basic_auth.logins
     where basic_auth.username = old.username;
    return null;
  end if;
end
$$;

drop trigger if exists update_logins_t on logins;
create trigger update_logins_t
  instead of insert or update or delete on
    logins for each row execute procedure update_logins();

commit;
