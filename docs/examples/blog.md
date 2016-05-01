## Multi-Tenant Blog

In our blog app there will be anonymous users and authors. Each
author can create and edit their own posts, and read (but not edit)
the posts of other authors. Anonymous users cannot edit anything
but can sign up for author accounts. Authors can also post comments
on articles.

This example builds off the previous previous [User Management](users/)
one. We had previously created a signup and login system on top of
JWT. We'll use this auth system for the blog. **Run the SQL in the
previous example** first, before continuing with this example.

For your convenience, the complete sql for the blog demo is
[here](https://github.com/begriffs/postgrest/blob/master/schema-templates/blog.sql).
You can try it out in this [vagrant
image](https://github.com/ruslantalpa/blogdemo) as well.

### Adding Blog-Specific Tables

Storing the posts and comments is this simple. The comments do not
form a tree, they are linear under a post.

```sql
create table if not exists
posts (
  id         bigserial primary key,
  title      text not null,
  body       text not null,
  author     text not null references basic_auth.users (email)
               on delete restrict on update cascade
               default basic_auth.current_email(),
  created_at timestamptz not null default current_date
);

create table if not exists
comments (
  id         bigserial primary key,
  body       text not null,
  author     text not null references basic_auth.users (email)
               on delete restrict on update cascade
               default basic_auth.current_email(),
  post       bigint not null references posts (id)
               on delete cascade on update cascade,
  created_at timestamptz not null default current_date
);
```

### Permissions

On top of the `authenticator` and `anon` access granted in the
previous example, blogs have an `author` role with extra permissions.

```sql
create role author;
grant author to authenticator;

grant usage on schema public, basic_auth to author;

-- authors can edit comments/posts
grant select, insert, update, delete
  on basic_auth.tokens, basic_auth.users to author;
grant select, insert, update, delete
  on table users, posts, comments to author;
grant usage, select on sequence posts_id_seq, comments_id_seq to author;
```

To ensure that authors cannot edit each others' posts and comments
we'll use [row-level
security](http://www.postgresql.org/docs/9.5/static/ddl-rowsecurity.html).
Note that it requires PostgreSQL 9.5 or later.

```sql
ALTER TABLE posts ENABLE ROW LEVEL SECURITY;
drop policy if exists authors_eigenedit on posts;
create policy authors_eigenedit on posts
  using (author = basic_auth.current_email())
  with check (
    author = basic_auth.current_email()
  );

ALTER TABLE comments ENABLE ROW LEVEL SECURITY;
drop policy if exists authors_eigenedit on comments;
create policy authors_eigenedit on comments
  using (author = basic_auth.current_email())
  with check (
    author = basic_auth.current_email()
  );
```

Finally we need to modify the `users` view from the previous example.
This is because all authors share a single db role. We could have
chosen to assign a new role for every author (all inheriting from
`author`) but we choose to tell them apart by their email addresses.
The addition below prevents authors from seeing each others' info
in the `users` view.


```diff
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
  where actual.role = member_of.rolname
+   and (
+     actual.role <> 'author'
+     or email = basic_auth.current_email()
+   );
```

### Example client queries

* Top ten most recent posts

```HTTP
  GET /posts?order=created_at.desc
  Range: 0-9
```

* Single post (randomly chose id=1) with its comments

```HTTP
  GET /posts?id=eq.1&select=*,comments{*}
```

* Add a new post

```HTTP
  POST /posts
  Authorization: Bearer [JWT TOKEN]

  {
    "title": "My first post",
    "body": "Meh, forgot what I wanted to say."
  }
```

### Conclusion

Voilà, a blog API. Most of the code ended up being for defining
security. Once you have set up an authentication system, the code
to do application specific things like blog posts and comments is
short.  All the front-end routes and verbs are created automatically
for you.
