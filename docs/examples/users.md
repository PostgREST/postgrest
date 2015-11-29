## User Management

API clients authenticate with [JSON Web Tokens](http://jwt.io).
PostgREST does not support any other authentication mechanism
directly, but they can be built on top. In this demo we will build
a username and password system on top of JWT using only plpgsql.

Future examples such as the multi-tenant blogging platform will use
the results from this example for their auth. We will build a system
for users to sign up, log in, manage their accounts, and for admins
to manange other people's accounts. We will also see how to trigger
outside events like sending signup validation emails.

Before jumping into the code, a little more about how the tokens
work. Every JWT contains cryptographically signed *claims*. PostgREST
cares specificaly about a claim called `role`. When a client includes
a `role` claim PG executes their request using that database role.

How would a client include that claim? Without knowing the server
JWT secret a client cannot create a claim. The only place to get a
JWT is from the PG server or from another service sharing the secret
and acting on its behalf.  We'll use a stored procedure returning
type `jwt_claims` which is a special type and causes the server to
encrypt and sign the return value.

### Storing Users and Passwords

We create a database schema especially for auth information. We'll
also need the postgres extensions
[pgcrypto](http://www.postgresql.org/docs/current/static/pgcrypto.html) and
[uuid-ossp](http://www.postgresql.org/docs/current/static/uuid-ossp.html).

```sql
create extension if not exists pgcrypto;
create extension if not exists "uuid-ossp";

-- We put things inside the basic_auth schema to hide
-- them from public view. Certain public procs/views will
-- refer to helpers and tables inside.
create schema if not exists basic_auth;
```

Next a table to store the mapping from usernames and passwords to
database roles. Includes triggers and functions to encrypt the
password and ensure the role exists.

```sql
create table if not exists
basic_auth.users (
  email    text primary key check ( email ~* '^.+@.+\..+$' ),
  pass     text not null check (length(pass) < 512),
  role     name not null check (length(role) < 512),
  verified boolean not null default false
  -- If you like add more columns, or a json column
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

drop trigger if exists encrypt_pass on basic_auth.users;
create trigger encrypt_pass
  before insert or update on basic_auth.users
  for each row
  execute procedure basic_auth.encrypt_pass();
```

With the table in place we can make a helper to check passwords.
It returns the database role for a user if the email and password
are correct.

```sql
create or replace function
basic_auth.user_role(email text, pass text) returns name
  language plpgsql
  as $$
begin
  return (
  select role from basic_auth.users
   where users.email = user_role.email
     and users.pass = crypt(user_role.pass, users.pass)
  );
end;
$$;
```

### Password Reset

When a user requests a password reset or signs up we create a token
they will use later to prove their identity. The tokens go in this
table.

```sql
drop type if exists token_type_enum cascade;
create type token_type_enum as enum ('validation', 'reset');

create table if not exists
basic_auth.tokens (
  token       uuid primary key,
  token_type  token_type_enum not null,
  email       text not null references basic_auth.users (email)
                on delete cascade on update cascade,
  created_at  timestamptz not null default current_date
);
```

In the main schema (not `basic_auth`) we expose a password reset
request function for the front end to call. It takes the email
address of the user.

```sql
create or replace function
request_password_reset(email text) returns void
  language plpgsql
  as $$
declare
  tok uuid;
begin
  delete from basic_auth.tokens
   where token_type = 'reset'
     and tokens.email = request_password_reset.email;

  select uuid_generate_v4() into tok;
  insert into basic_auth.tokens (token, token_type, email)
         values (tok, 'reset', request_password_reset.email);
  perform pg_notify('reset',
    json_build_object(
      'email', request_password_reset.email,
      'token', tok,
      'token_type', 'reset'
    )::text
  );
end;
$$;
```

This function does not send any emails. It sends a postgres
[NOTIFY](http://www.postgresql.org/docs/current/static/sql-notify.html)
command to trigger external actions. The most robust way to process
these signals is by pushing them onto work queues. Here are two
programs to do that:

1. [aweber/pgsql-listen-exchange](https://github.com/aweber/pgsql-listen-exchange) for RabbitMQ
2. [SpiderOak/skeeter](https://github.com/SpiderOak/skeeter) for ZeroMQ

For experimentation you don't need that though. Here's a sample
Node program that listens for the events and logs them to stdout.

```js
var PS = require('pg-pubsub');

if(process.argv.length !== 3) {
  console.log("USAGE: DB_URL");
  process.exit(2);
}
var url  = process.argv[2],
    ps   = new PS(url);

// password reset request events
ps.addChannel('reset', console.log);
// email validation required event
ps.addChannel('validate', console.log);

// modify me to send emails
```

Once the user has a reset token they can call this function
through the PostgREST RPC interface.

```sql
create or replace function
reset_password(email text, token uuid, pass text)
  returns void
  language plpgsql
  as $$
declare
  tok uuid;
begin
  if exists(select 1 from basic_auth.tokens
             where tokens.email = reset_password.email
               and tokens.token = reset_password.token
               and token_type = 'reset') then
    update basic_auth.users set pass=reset_password.pass
     where users.email = reset_password.email;

    delete from basic_auth.tokens
     where tokens.email = reset_password.email
       and tokens.token = reset_password.token
       and token_type = 'reset';
  else
    raise invalid_password using message =
      'invalid user or token';
  end if;
  delete from basic_auth.tokens
   where token_type = 'reset'
     and tokens.email = reset_password.email;

  select uuid_generate_v4() into tok;
  insert into basic_auth.tokens (token, token_type, email)
         values (tok, 'reset', reset_password.email);
  perform pg_notify('reset',
    json_build_object(
      'email', reset_password.email,
      'token', tok
    )::text
  );
end;
$$;
```

### Email Validation

This is similar to password resets. Once again we generate a token.
It differs in that there is a trigger to send validations when a
new login is added to the user table.

```sql

```
