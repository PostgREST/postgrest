-------------------------------------------------------------------------------
-- Adapted from https://github.com/robconery/pg-auth

begin;

create extension if not exists pgcrypto;
create extension if not exists "uuid-ossp";

-- We put things inside the basic_auth schema to hide
-- them from public view. Certain public procs/views will
-- refer to helpers and tables inside.
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
-- Users storage and constraints

create table if not exists
basic_auth.users (
  username text not null,
  pass   text not null,
  role   name not null,
  email  text not null unique,
  active boolean not null default false,
  more   JSON,
  constraint user_pkey primary key (username),
  constraint user_field_length_limits check (
    length(username::text) < 512 AND length(pass) < 512 AND
    length(email::text) < 512    AND length(more::text) < 1024)
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

drop trigger if exists ensure_user_role_exists on basic_auth.users;
create constraint trigger ensure_user_role_exists
  after insert or update on basic_auth.users
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

drop trigger if exists protect_passwords on basic_auth.users;
create trigger protect_passwords
  before insert or update on basic_auth.users
  for each row
  execute procedure basic_auth.encrypt_pass();

create or replace function
basic_auth.send_validation() returns trigger
  language plpgsql
  as $$
declare
  tok text;
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

drop trigger if exists send_validation_t on basic_auth.users;
create trigger send_validation_t
  after insert on basic_auth.users
  for each row
  execute procedure basic_auth.send_validation();

-------------------------------------------------------------------------------
-- Email Validation and Password Reset

create table if not exists
basic_auth.tokens (
  token       text unique,
  token_type  text not null,
  username    text not null,
  created_at  timestamptz not null default current_date,
  constraint  t_pk primary key (token),
  constraint  t_user_fk foreign key (username) references basic_auth.users
              on delete cascade on update cascade
);

-------------------------------------------------------------------------------
-- Login helper

create or replace function
basic_auth.user_role(username text, pass text) returns text
  language plpgsql
  as $$
begin
  return (
  select role from basic_auth.users
   where users.username = user_role.username
     and users.pass = crypt(user_role.pass, users.pass)
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
  tok text;
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
                  from basic_auth.users
                 where users.username = request_password_reset.username),
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
  tok text;
begin
  if exists(select 1 from basic_auth.tokens
             where tokens.username = reset_password.username
               and tokens.token = reset_password.token
               and token_type = 'reset') then
    update basic_auth.users set pass=reset_password.pass
     where users.username = reset_password.username;

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
                  from basic_auth.users
                 where users.username = reset_password.username),
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
login(username text, pass text) returns basic_auth.jwt_claims
  language plpgsql
  as $$
declare
  _role text;
  result basic_auth.jwt_claims;
begin
  select basic_auth.user_role(username, pass) into _role;
  if _role is null then
    raise invalid_password using message = 'invalid user or password';
  end if;
  select _role as role, login.username as username into result;
  return result;
end;
$$;

create or replace function
signup(username text, email text, pass text) returns void
  language plpgsql
  as $$
begin
  insert into basic_auth.users (username, email, pass, role) values
    (signup.username, signup.email, signup.pass, 'author');
end;
$$;

-------------------------------------------------------------------------------
-- User management

create or replace view users as
select actual.username as username,
       actual.role as role,
       '***'::text as pass,
       actual.email as email,
       actual.active as active,
       actual.more as more
from basic_auth.users as actual,
     (select rolname
        from pg_authid
       where pg_has_role(current_user, oid, 'member')
     ) as member_of
where actual.role = member_of.rolname;

create or replace function
update_users() returns trigger
language plpgsql
AS $$
begin
  if tg_op = 'INSERT' then
    perform basic_auth.clearance_for_role(new.role);

    insert into basic_auth.users
      (username, role, pass, email, active, more) values
      (new.username, coalesce(new.role, 'author'), new.pass,
        new.email, coalesce(new.active, false), new.more);
    return new;
  elsif tg_op = 'UPDATE' then
    -- no need to check clearance for old.role because
    -- an ineligible row would not even available to update (http 404)
    perform basic_auth.clearance_for_role(new.role);

    update basic_auth.users set
      username = new.username, role  = new.role,
      pass     = new.pass,     email = new.email,
      active   = coalesce(new.active, old.active, false),
      more  = new.more
      where username = old.username;
    return new;
  elsif tg_op = 'DELETE' then
    -- no need to check clearance for old.role (see previous case)

    delete from basic_auth.users
     where basic_auth.username = old.username;
    return null;
  end if;
end
$$;

drop trigger if exists update_users_t on users;
create trigger update_users_t
  instead of insert or update or delete on
    users for each row execute procedure update_users();

-------------------------------------------------------------------------------
-- Blogging stuff!

create table if not exists
posts (
  id         bigserial not null,
  title      text not null,
  body       text not null,
  author     text not null,
  created_at timestamptz not null default current_date,
  constraint post_pk primary key (id),
  constraint post_user_fk foreign key (author)
             references basic_auth.users
             on delete restrict on update cascade
);

create table if not exists
comments (
  id         bigserial not null,
  body       text not null,
  author     text not null,
  post       bigint not null,
  created_at timestamptz not null default current_date,
  constraint comment_pk primary key (id),
  constraint comment_user_fk foreign key (author)
             references basic_auth.users
             on delete restrict on update cascade,
  constraint comment_post_fk foreign key (post) references posts
             on delete cascade on update cascade
);

-------------------------------------------------------------------------------
-- Permissions

--create role anon noinherit;
grant insert on table basic_auth.users, basic_auth.tokens to anon;
grant select on table pg_authid, basic_auth.users, posts, comments to anon;
grant execute on function
  login(text,text),
  request_password_reset(text),
  reset_password(text,text,text),
  signup(text, text, text)
  to anon;

--create role author;
grant author to anon;
grant select, insert, update, delete
  on table basic_auth.users, users, posts, comments to author;
grant usage, select on sequence posts_id_seq, comments_id_seq to author;

grant usage on schema public, basic_auth to anon, author;

ALTER TABLE posts ENABLE ROW LEVEL SECURITY;
drop policy if exists authors_eigenedit on posts;
create policy authors_eigenedit on posts
  for all
  using (true)
  with check (
    author = current_setting('postgrest.claims.username')
  );

ALTER TABLE comments ENABLE ROW LEVEL SECURITY;
drop policy if exists authors_eigenedit on comments;
create policy authors_eigenedit on comments
  for all
  using (true)
  with check (
    author = current_setting('postgrest.claims.username')
  );

commit;
