-------------------------------------------------------------------------------
-- Adapted from https://github.com/robconery/pg-auth

begin;

drop schema if exists basic_auth cascade;
create schema if not exists basic_auth;
set search_path to basic_auth;

-------------------------------------------------------------------------------
-- Utility functions

create function random_value(len int, out result varchar(32)) as
$$
BEGIN
SELECT substring(md5(random()::text),0, len) into result;
END
$$ LANGUAGE plpgsql;

-------------------------------------------------------------------------------
-- Login storage and constraints

create table logins (
  username character varying not null,
  pass   character(60) not null,
  role   name not null,
  email  character varying not null unique,
  active boolean not null default false,
  more   JSON,
  constraint l_pkey primary key (username)
);

create function check_role_exists() returns trigger
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

create constraint trigger ensure_login_role_exists
  after insert or update on logins
  for each row
  execute procedure check_role_exists();

create function encrypt_pass() returns trigger
  language plpgsql
  as $$
begin
  if tg_op = 'INSERT' or new.pass <> old.pass then
    new.pass = crypt(new.pass, gen_salt('bf'));
  end if;
  return new;
end
$$;

create trigger protect_passwords
  before insert or update on logins
  for each row
  execute procedure encrypt_pass();

create function send_validation() returns trigger
  language plpgsql
  as $$
declare
  tok character varying;
begin
  select basic_auth.random_value(64) into tok;
  insert into basic_auth.tokens (token, token_type, username)
         values (tok, 'validation', new.username);
  perform pg_notify('validate',
    json_build_object(
      'email', new.email,
      'username', new.username,
      'token', tok
    )::text
  );
  return new;
end
$$;

create trigger send_validation_t
  after insert on logins
  for each row
  execute procedure send_validation();

-------------------------------------------------------------------------------
-- Email Validation and Password Reset

create table tokens (
  token       character varying unique,
  token_type  varchar(64) not null,
  username    character varying not null,
  created_at  timestamptz not null default current_date,
  constraint  t_pk primary key (token),
  constraint  t_login_fk foreign key (username) references logins
              on delete cascade on update cascade
);

-------------------------------------------------------------------------------
-- Passwording

create function
login_role(username text, pass text) returns text
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

create function request_password_reset(username text) returns void
  language plpgsql
  as $$
declare
  tok character varying;
begin
  delete from basic_auth.tokens
   where token_type = 'reset'
     and tokens.username = request_password_reset.username;

  select basic_auth.random_value(64) into tok;
  insert into basic_auth.tokens (token, token_type, username)
         values (tok, 'reset', request_password_reset.username);
  perform pg_notify('reset',
    json_build_object(
      'email', (select email
                  from basic_auth.logins
                 where logins.username = request_password_reset.username),
      'username', request_password_reset.username,
      'token', tok
    )::text
  );
end;
$$;

create or replace function basic_auth.reset_password(username text, token text, pass text)
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

  select basic_auth.random_value(64) into tok;
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

create type jwt_claims AS (role text, username text);

create function
obtain_auth_token(username text, pass text) returns jwt_claims
  language plpgsql
  as $$
declare
  _role character varying;
  result jwt_claims;
begin
  select basic_auth.login_role(username, pass) into _role;
  if _role is null then
    raise invalid_password using message = 'invalid user or password';
  end if;
  select _role as role, obtain_auth_token.username as username into result;
  return result;
end;
$$;

commit;
