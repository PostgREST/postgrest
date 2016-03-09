## User Management

API clients authenticate with [JSON Web Tokens](http://jwt.io).
PostgREST does not support any other authentication mechanism
directly, but they can be built on top. In this demo we will build
a username and password system on top of JWT using only plpgsql.

Future examples such as the multi-tenant blogging platform will use
the results from this example for their auth. We will build a system
for users to sign up, log in, manage their accounts, and for admins
to manage other people's accounts. We will also see how to trigger
outside events like sending password reset emails.

Before jumping into the code, a little more about how the tokens
work. Every JWT contains cryptographically signed *claims*. PostgREST
cares specifically about a claim called `role`. When a client includes
a `role` claim PostgREST executes their request using that database
role.

How would a client include a role claim, or claims in general?
Without knowing the server JWT secret a client cannot create a
claim. The only place to get a JWT is from the PostgREST server or
from another service sharing the secret and acting on its behalf.
We'll use a stored procedure returning type `jwt_claims` which is
a special type causing the server to encrypt and sign the return
value.

### Storing Users and Passwords

We create a database schema especially for auth information. We'll
also need the postgres extension
[pgcrypto](http://www.postgresql.org/docs/current/static/pgcrypto.html).

```sql
create extension if not exists pgcrypto;

-- We put things inside the basic_auth schema to hide
-- them from public view. Certain public procs/views will
-- refer to helpers and tables inside.
create schema if not exists basic_auth;
```

Next a table to store the mapping from usernames and passwords to
database roles. The code below includes triggers and functions to
encrypt the password and ensure the role exists.

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

In the main schema (as opposed to the `basic_auth` schema) we expose
a password reset request function. HTTP clients will call it. The
function takes the email address of the user.

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

  select gen_random_uuid() into tok;
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
command. External programs such as a mailer listen for this event
and do the work. The most robust way to process these signals is
by pushing them onto work queues. Here are two programs to do that:

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

Once the user has a reset token they can use it as an argument to
the password reset function, calling it through the PostgREST RPC
interface.

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
new login is added to the users table.

```sql
create or replace function
basic_auth.send_validation() returns trigger
  language plpgsql
  as $$
declare
  tok uuid;
begin
  select gen_random_uuid() into tok;
  insert into basic_auth.tokens (token, token_type, email)
         values (tok, 'validation', new.email);
  perform pg_notify('validate',
    json_build_object(
      'email', new.email,
      'token', tok,
      'token_type', 'validation'
    )::text
  );
  return new;
end
$$;

drop trigger if exists send_validation on basic_auth.users;
create trigger send_validation
  after insert on basic_auth.users
  for each row
  execute procedure basic_auth.send_validation();
```

### Editing Own User

We'll construct a redacted view for users. It hides passwords and
shows only those users whose roles the currently logged in user has
db permission to access.

```sql
create or replace view users as
select actual.role as role,
       '***'::text as pass,
       actual.email as email,
       actual.verified as verified
from basic_auth.users as actual,
     (select rolname
        from pg_authid
       where pg_has_role(current_user, oid, 'member')
     ) as member_of
where actual.role = member_of.rolname;
  -- can also add restriction that current_setting('postgrest.claims.email')
  -- is equal to email so that user can only see themselves
```

Using this view clients can see themselves and any other users with
the right db roles. This view does not yet support inserts or updates
because not all the columns refer directly to underlying columns.
Nor do we want it to be auto-updatable because it would allow an escalation
of privileges. Someone could update their own row and change their
role to become more powerful.

We'll handle updates with a trigger, but we'll need a helper function
to prevent an escalation of privileges.

```sql
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
```

With the above function we can now make a safe trigger to allow
user updates.

```sql
create or replace function
update_users() returns trigger
language plpgsql
AS $$
begin
  if tg_op = 'INSERT' then
    perform basic_auth.clearance_for_role(new.role);

    insert into basic_auth.users
      (role, pass, email, verified)
    values
      (new.role, new.pass, new.email,
      coalesce(new.verified, false));
    return new;
  elsif tg_op = 'UPDATE' then
    -- no need to check clearance for old.role because
    -- an ineligible row would not have been available to update (http 404)
    perform basic_auth.clearance_for_role(new.role);

    update basic_auth.users set
      email  = new.email,
      role   = new.role,
      pass   = new.pass,
      verified = coalesce(new.verified, old.verified, false)
      where email = old.email;
    return new;
  elsif tg_op = 'DELETE' then
    -- no need to check clearance for old.role (see previous case)

    delete from basic_auth.users
     where basic_auth.email = old.email;
    return null;
  end if;
end
$$;

drop trigger if exists update_users on users;
create trigger update_users
  instead of insert or update or delete on
    users for each row execute procedure update_users();
```

Finally add a public function people can use to sign up. You can
hard code a default db role in it. It alters the underlying
`basic_auth.users` so you can set whatever role you want without
restriction.

```sql
create or replace function
signup(email text, pass text) returns void
as $$
  insert into basic_auth.users (email, pass, role) values
    (signup.email, signup.pass, 'hardcoded-role-here');
$$ language sql;
```

### Generating JWT

As mentioned at the start, clients authenticate with JWT. PostgREST
has a special convention to allow your sql functions to return JWT.
Any function that returns a type whose name ends in `jwt_claims` will
have its return value encoded. For instance, let's make a login function
which consults our users table.

First create a return type:

```sql
drop type if exists basic_auth.jwt_claims cascade;
create type basic_auth.jwt_claims AS (role text, email text);
```

And now the function:

```sql
create or replace function
login(email text, pass text) returns basic_auth.jwt_claims
  language plpgsql
  as $$
declare
  _role name;
  result basic_auth.jwt_claims;
begin
  select basic_auth.user_role(email, pass) into _role;
  if _role is null then
    raise invalid_password using message = 'invalid user or password';
  end if;
  -- TODO; check verified flag if you care whether users
  -- have validated their emails
  select _role as role, login.email as email into result;
  return result;
end;
$$;
```

An API request to login would look like this.

```HTTP
POST /rpc/login

{ "email": "foo@bar.com", "pass": "foobar" }
```

Response
```json
{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImZvb0BiYXIuY29tIiwicm9sZSI6ImF1dGhvciJ9.KHwYdK9dAMAg-MGCQXuDiFuvbmW-y8FjfYIcMrETnto"
}
```

Try decoding the token at [jwt.io](http://jwt.io/). (It was encoded
with a secret of `secret` which is the default.) To use this token
in a future API request include it in an `Authorization` request
header.

```HTTP
Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6ImZvb0BiYXIuY29tIiwicm9sZSI6ImF1dGhvciJ9.KHwYdK9dAMAg-MGCQXuDiFuvbmW-y8FjfYIcMrETnto
```

### Same-Role Users

You may not want a separate db role for every user. You can distinguish
one user from another in SQL by examining the JWT claims which
PostgREST makes available in the SQL variable `postgrest.claims`.
Here's a function to get the email of the currently authenticated
user.

```sql
create or replace function
basic_auth.current_email() returns text
  language plpgsql
  as $$
begin
  return current_setting('postgrest.claims.email');
exception
  -- handle unrecognized configuration parameter error
  when undefined_object then return '';
end;
$$;
```

Remember that the `login` function set the claims `email` and `role`.
You can modify `login` to set other claims as well if they are
useful for your other SQL functions to reference later.

### Permissions

Basic table-level permissions. We'll add an the `authenticator`
role which can't do anything itself other than switch into other
roles as directed by JWT.

```sql
create role anon;
create role authenticator noinherit;
grant anon to authenticator;

grant usage on schema public, basic_auth to anon;

-- anon can create new logins
grant insert on table basic_auth.users, basic_auth.tokens to anon;
grant select on table pg_authid, basic_auth.users to anon;
grant execute on function
  login(text,text),
  request_password_reset(text),
  reset_password(text,uuid,text),
  signup(text, text)
  to anon;
```

### Conclusion

This section explained the implementation details for building a
password based authentication system in pure sql. The next example
will put it to work in a multi-tenant blogging API.
