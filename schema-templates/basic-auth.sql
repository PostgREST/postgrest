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
login_role(_user text, _pass text, out _matched_role text) returns text
  language plpgsql
  as $$
begin
  select role into _matched_role from login
   where username = _user
     and pass = crypt(_pass, pass);
end;
$$;

create function request_password_reset(_user text) returns text
  language plpgsql
  as $$
declare
  tok character varying;
begin
  delete from basic_auth.tokens
   where token_type = 'reset'
     and username = _user;

  select basic_auth.random_value(64) into tok;
  insert into basic_auth.tokens (token, token_type, username)
         values (tok, 'reset', new.username);
  select pg_notify('reset',
    json_build_object(
      'email', new.email,
      'username', new.username,
      'token', tok
    )::text
  );
end;
$$;

create function reset_password(_user text, _token text, _pass text)
  returns text
  language plpgsql
  as $$
declare
  tok character varying;
begin
  if exists(select 1 from basic_auth.tokens
             where username = _user
               and token = _token
               and token_type = 'reset') then
    update login set pass=_pass
     where username = _user;

    delete from basic_auth.tokens
     where username = _user
       and token = _token
       and token_type = 'reset';
  else
    raise invalid_password using message =
      'invalid user or token';
  end if;
  delete from basic_auth.tokens
   where type = 'reset'
     and username = _user;

  select basic_auth.random_value(64) into tok;
  insert into basic_auth.tokens (token, token_type, username)
         values (tok, 'reset', _user);
  select pg_notify('reset',
    json_build_object(
      'email', (select email from basic_auth.tokens where username = _user),
      'username', _user,
      'token', tok
    )::text
  );
end;
$$;

commit;
