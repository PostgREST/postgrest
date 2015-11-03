-------------------------------------------------------------------------------
-- Adapted from https://github.com/robconery/pg-auth

begin;

create schema if not exists basic_auth;

-------------------------------------------------------------------------------
-- Login storage and constraints

create table basic_auth.login (
  user      character varying not null,
  pass      character(60) not null,
  role      name not null,
  email     varchar(64) not null unique,
  is_active boolean not null default false,
  more      JSON,
  constraint l_pkey primary key (user)
);

create function basic_auth.check_role_exists() returns trigger
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
  after insert or update on basic_auth.login
  for each row
  execute procedure check_role_exists();

create function basic_auth.encrypt_pass() returns trigger
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
  before insert or update on basic_auth.login
  for each row
  execute procedure encrypt_pass(); |]

-------------------------------------------------------------------------------
-- Password checking

create function if not exists
basic_auth.login_role(_login text, _pass text, out _matched_role text) returns text
  language plpgsql
  as $$
begin
  select role into _matched_role from basic_auth.login
   where login = _login
     and pass  = crypt(_pass, pass);
end;
$$;

-------------------------------------------------------------------------------
-- Password changing

create table if not exists basic_auth.token (
  token       uuid unique,
  token_type  varchar(64) not null,
  user        character varying not null,
  created_at  timestamptz not null default current_date,
  valid_until timestamptz not null,
  constraint  t_pk primary key (token),
  constraint  t_login_fk foreign key (user) references login
              on delete cascade on update cascade
);

commit;
