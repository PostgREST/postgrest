--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5beta1
-- Dumped by pg_dump version 9.5beta1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

CREATE SCHEMA public;
CREATE SCHEMA postgrest;
CREATE SCHEMA private;
CREATE SCHEMA test;
CREATE SCHEMA تست;


--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;

SET search_path = public, pg_catalog;

CREATE EXTENSION IF NOT EXISTS pgcrypto;

--
-- Name: jwt_token; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.jwt_token AS (
	token text
);


SET search_path = test, pg_catalog;

--
-- Name: enum_menagerie_type; Type: TYPE; Schema: test; Owner: -
--

CREATE TYPE enum_menagerie_type AS ENUM (
    'foo',
    'bar'
);


SET search_path = postgrest, pg_catalog;

--
-- Name: check_role_exists(); Type: FUNCTION; Schema: postgrest; Owner: -
--

CREATE FUNCTION check_role_exists() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
if not exists (select 1 from pg_roles as r where r.rolname = new.rolname) then
   raise foreign_key_violation using message = 'Cannot create user with unknown role: ' || new.rolname;
   return null;
 end if;
 return new;
end
$$;


--
-- Name: set_authors_only_owner(); Type: FUNCTION; Schema: postgrest; Owner: -
--

CREATE FUNCTION set_authors_only_owner() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  NEW.owner = current_setting('request.jwt.claim.id');
  RETURN NEW;
end
$$;


--
-- Name: update_owner(); Type: FUNCTION; Schema: postgrest; Owner: -
--

CREATE FUNCTION update_owner() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.owner = current_user;
   RETURN NEW;
END;
$$;


SET search_path = test, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: items; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE items (
    id bigint NOT NULL
);


CREATE FUNCTION always_true(test.items) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$ SELECT true $$;

CREATE FUNCTION anti_id(test.items) RETURNS bigint
    LANGUAGE sql STABLE
    AS $_$ SELECT $1.id * -1 $_$;

SET search_path = public, pg_catalog;

CREATE FUNCTION always_false(test.items) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$ SELECT false $$;

create table public_consumers (
    id                  serial             not null unique,
    name                text               not null check (name <> ''),
    primary key (id)
);

create table public_orders (
    id                  serial             not null unique,
    consumer            integer            not null references public_consumers(id),
    number              integer            not null,
    primary key (id)
);

SET search_path = تست, pg_catalog;

CREATE TABLE موارد (
    هویت bigint NOT NULL
);


SET search_path = test, pg_catalog;


create view orders_view as
  select * from public.public_orders;

create view consumers_view as
  select * from public.public_consumers;


--
-- Name: getitemrange(bigint, bigint); Type: FUNCTION; Schema: test; Owner: -
--

CREATE FUNCTION getitemrange(min bigint, max bigint) RETURNS SETOF items
    LANGUAGE sql
    AS $_$
    SELECT * FROM test.items WHERE id > $1 AND id <= $2;
$_$;

--
-- Name: version(); Type: FUNCTION; Schema: test; Owner: -
--

CREATE FUNCTION noparamsproc() RETURNS text
	LANGUAGE sql
	AS $$
		SELECT a FROM (VALUES ('Return value of no parameters procedure.')) s(a);
	$$;

--
-- Name: insert_insertable_view_with_join(); Type: FUNCTION; Schema: test; Owner: -
--

CREATE FUNCTION insert_insertable_view_with_join() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  INSERT INTO test.auto_incrementing_pk (nullable_string, non_nullable_string) VALUES (NEW.nullable_string, NEW.non_nullable_string);
  RETURN NEW;
end;
$$;


--
-- Name: login(text, text); Type: FUNCTION; Schema: test; Owner: -
--

CREATE FUNCTION login(id text, pass text) RETURNS public.jwt_token
    LANGUAGE sql SECURITY DEFINER
    AS $$
SELECT jwt.sign(
    row_to_json(r), 'reallyreallyreallyreallyverysafe'
  ) as token
  FROM (
    SELECT rolname::text, id::text
      FROM postgrest.auth
     WHERE id = id AND pass = pass
  ) r;
$$;


CREATE FUNCTION varied_arguments(
  double double precision,
  "varchar" character varying,
  "boolean" boolean,
  date date,
  money money,
  enum enum_menagerie_type,
  "integer" integer default 42
) RETURNS text
    LANGUAGE sql
AS $_$
  SELECT 'Hi'::text;
$_$;

COMMENT ON FUNCTION varied_arguments(double precision, character varying, boolean, date, money, enum_menagerie_type, integer) IS
$_$An RPC function

Just a test for RPC function arguments$_$;

--
-- Name: jwt_test(); Type: FUNCTION; Schema: test; Owner: -
--

CREATE FUNCTION jwt_test() RETURNS public.jwt_token
    LANGUAGE sql SECURITY DEFINER
    AS $$
SELECT jwt.sign(
    row_to_json(r), 'reallyreallyreallyreallyverysafe'
  ) as token
  FROM (
    SELECT 'joe'::text as iss, 'fun'::text as sub, 'everyone'::text as aud,
       1300819380 as exp, 1300819380 as nbf, 1300819380 as iat,
       'foo'::text as jti, 'postgrest_test'::text as role,
       true as "http://postgrest.com/foo"
  ) r;
$$;


CREATE OR REPLACE FUNCTION switch_role() RETURNS void
  LANGUAGE plpgsql
  AS $$
declare
  user_id text;
Begin
  user_id = current_setting('request.jwt.claim.id')::text;
  if user_id = '1'::text then
    execute 'set local role postgrest_test_author';
  elseif user_id = '2'::text then
    execute 'set local role postgrest_test_default_role';
  elseif user_id = '3'::text then
    RAISE EXCEPTION 'Disabled ID --> %', user_id USING HINT = 'Please contact administrator';
  /* else */
  /*   execute 'set local role postgrest_test_anonymous'; */
  end if;
end
$$;

CREATE FUNCTION get_current_user() RETURNS text
  LANGUAGE sql
  AS $$
SELECT current_user::text;
$$;

--
-- Name: reveal_big_jwt(); Type: FUNCTION; Schema: test; Owner: -
--

CREATE FUNCTION reveal_big_jwt() RETURNS TABLE (
      iss text, sub text, exp bigint,
      nbf bigint, iat bigint, jti text, "http://postgrest.com/foo" boolean
    )
    LANGUAGE sql SECURITY DEFINER
    AS $$
SELECT current_setting('request.jwt.claim.iss') as iss,
       current_setting('request.jwt.claim.sub') as sub,
       current_setting('request.jwt.claim.exp')::bigint as exp,
       current_setting('request.jwt.claim.nbf')::bigint as nbf,
       current_setting('request.jwt.claim.iat')::bigint as iat,
       current_setting('request.jwt.claim.jti') as jti,
       -- role is not included in the claims list
       current_setting('request.jwt.claim.http://postgrest.com/foo')::boolean
         as "http://postgrest.com/foo";
$$;


--
-- Name: problem(); Type: FUNCTION; Schema: test; Owner: -
--

CREATE FUNCTION problem() RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
      RAISE 'bad thing';
END;
$$;


--
-- Name: sayhello(text); Type: FUNCTION; Schema: test; Owner: -
--

CREATE FUNCTION sayhello(name text) RETURNS text
    LANGUAGE sql
    IMMUTABLE
    AS $_$
    SELECT 'Hello, ' || $1;
$_$;


--
-- Name: callcounter(); Type: FUNCTION; Schema: test; Owner: -
--

CREATE SEQUENCE callcounter_count START 1;

CREATE FUNCTION callcounter() RETURNS bigint
    LANGUAGE sql
    AS $_$
    SELECT nextval('test.callcounter_count');
$_$;

--
-- Name: singlejsonparam(json); Type: FUNCTION; Schema: test; Owner: -
--

CREATE FUNCTION singlejsonparam(single_param json) RETURNS json
    LANGUAGE sql
    AS $_$
    SELECT single_param;
$_$;

--
-- Name: test_empty_rowset(); Type: FUNCTION; Schema: test; Owner: -
--

CREATE FUNCTION test_empty_rowset() RETURNS SETOF integer
    LANGUAGE sql
    AS $$
    SELECT null::int FROM (SELECT 1) a WHERE false;
$$;


SET search_path = postgrest, pg_catalog;

--
-- Name: auth; Type: TABLE; Schema: postgrest; Owner: -
--

CREATE TABLE auth (
    id character varying NOT NULL,
    rolname name DEFAULT 'postgrest_test_author'::name NOT NULL,
    pass character(60) NOT NULL
);


SET search_path = private, pg_catalog;

--
-- Name: article_stars; Type: TABLE; Schema: private; Owner: -
--

CREATE TABLE article_stars (
    article_id integer NOT NULL,
    user_id integer NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


--
-- Name: articles; Type: TABLE; Schema: private; Owner: -
--

CREATE TABLE articles (
    id integer NOT NULL,
    body text,
    owner name NOT NULL
);


SET search_path = test, pg_catalog;

CREATE VIEW limited_article_stars AS
  SELECT article_id, user_id, created_at FROM private.article_stars;


--
-- Name: articleStars; Type: VIEW; Schema: test; Owner: -
--

CREATE VIEW "articleStars" AS
 SELECT article_stars.article_id AS "articleId",
    article_stars.user_id AS "userId",
    article_stars.created_at AS "createdAt"
   FROM private.article_stars;


--
-- Name: articles; Type: VIEW; Schema: test; Owner: -
--

CREATE VIEW articles AS
 SELECT articles.id,
    articles.body,
    articles.owner
   FROM private.articles;


--
-- Name: authors_only; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE authors_only (
    owner character varying NOT NULL,
    secret character varying NOT NULL
);


--
-- Name: auto_incrementing_pk; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE auto_incrementing_pk (
    id integer NOT NULL,
    nullable_string character varying,
    non_nullable_string character varying NOT NULL,
    inserted_at timestamp with time zone DEFAULT now()
);


--
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE; Schema: test; Owner: -
--

CREATE SEQUENCE auto_incrementing_pk_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE OWNED BY; Schema: test; Owner: -
--

ALTER SEQUENCE auto_incrementing_pk_id_seq OWNED BY auto_incrementing_pk.id;


--
-- Name: clients; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE clients (
    id integer NOT NULL,
    name text NOT NULL
);


--
-- Name: comments; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE comments (
    id integer NOT NULL,
    commenter_id integer NOT NULL,
    user_id integer NOT NULL,
    task_id integer NOT NULL,
    content text NOT NULL
);


--
-- Name: complex_items; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE complex_items (
    id bigint NOT NULL,
    name text,
    settings pg_catalog.json,
    arr_data integer[],
		"field-with_sep" integer default 1 not null
);


--
-- Name: compound_pk; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE compound_pk (
    k1 integer NOT NULL,
    k2 text NOT NULL,
    extra integer
);


--
-- Name: empty_table; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE empty_table (
    k character varying NOT NULL,
    extra character varying NOT NULL
);


--
-- Name: private_table; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE private_table ();


--
-- Name: has_count_column; Type: VIEW; Schema: test; Owner: -
--

CREATE VIEW has_count_column AS
 SELECT 1 AS count;


--
-- Name: has_fk; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE has_fk (
    id bigint NOT NULL,
    auto_inc_fk integer,
    simple_fk character varying(255)
);


--
-- Name: has_fk_id_seq; Type: SEQUENCE; Schema: test; Owner: -
--

CREATE SEQUENCE has_fk_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: has_fk_id_seq; Type: SEQUENCE OWNED BY; Schema: test; Owner: -
--

ALTER SEQUENCE has_fk_id_seq OWNED BY has_fk.id;


--
-- Name: insertable_view_with_join; Type: VIEW; Schema: test; Owner: -
--

CREATE VIEW insertable_view_with_join AS
 SELECT has_fk.id,
    has_fk.auto_inc_fk,
    has_fk.simple_fk,
    auto_incrementing_pk.nullable_string,
    auto_incrementing_pk.non_nullable_string,
    auto_incrementing_pk.inserted_at
   FROM (has_fk
     JOIN auto_incrementing_pk USING (id));


--
-- Name: items_id_seq; Type: SEQUENCE; Schema: test; Owner: -
--

CREATE SEQUENCE items_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: items_id_seq; Type: SEQUENCE OWNED BY; Schema: test; Owner: -
--

ALTER SEQUENCE items_id_seq OWNED BY items.id;


--
-- Name: json; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE json (
    data pg_catalog.json
);


--
-- Name: materialized_view; Type: MATERIALIZED VIEW; Schema: test; Owner: -
--

CREATE MATERIALIZED VIEW materialized_view AS
 SELECT version() AS version
  WITH NO DATA;


--
-- Name: menagerie; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE menagerie (
    "integer" integer NOT NULL,
    double double precision NOT NULL,
    "varchar" character varying NOT NULL,
    "boolean" boolean NOT NULL,
    date date NOT NULL,
    money money NOT NULL,
    enum enum_menagerie_type NOT NULL
);


--
-- Name: no_pk; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE no_pk (
    a character varying,
    b character varying
);

CREATE TABLE only_pk (
    id integer primary key
);

--
-- Name: nullable_integer; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE nullable_integer (
    a integer
);


--
-- Name: insertonly; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE insertonly (
    v text NOT NULL
);


--
-- Name: projects; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE projects (
    id integer NOT NULL,
    name text NOT NULL,
    client_id integer
);


--
-- Name: projects_view; Type: VIEW; Schema: test; Owner: -
--

CREATE VIEW projects_view AS
 SELECT projects.id,
    projects.name,
    projects.client_id
   FROM projects;


CREATE VIEW projects_view_alt AS
 SELECT projects.id as t_id,
    projects.name,
    projects.client_id as t_client_id
   FROM projects;

--
-- Name: simple_pk; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE simple_pk (
    k character varying NOT NULL,
    extra character varying NOT NULL
);

--
-- Name: users_projects; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE users_projects (
    user_id integer NOT NULL,
    project_id integer NOT NULL
);


--
-- Name: tasks; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE tasks (
    id integer NOT NULL,
    name text NOT NULL,
    project_id integer
);

CREATE OR REPLACE VIEW filtered_tasks AS
SELECT id AS "myId", name, project_id AS "projectID"
FROM tasks
WHERE project_id IN (
	SELECT id FROM projects WHERE id = 1
) AND
project_id IN (
	SELECT project_id FROM users_projects WHERE user_id = 1
);


--
-- Name: tsearch; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE tsearch (
    text_search_vector tsvector
);


--
-- Name: users; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE users (
    id integer NOT NULL,
    name text NOT NULL
);



--
-- Name: users_tasks; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE users_tasks (
    user_id integer NOT NULL,
    task_id integer NOT NULL
);


CREATE TABLE "Escap3e;" (
		"so6meIdColumn" integer primary key
);

CREATE TABLE "ghostBusters" (
		"escapeId" integer not null references "Escap3e;"("so6meIdColumn")
);

CREATE TABLE "withUnique" (
    uni text UNIQUE,
    extra text
);

CREATE TABLE clashing_column (
    t text
);


--
-- Name: id; Type: DEFAULT; Schema: test; Owner: -
--

ALTER TABLE ONLY auto_incrementing_pk ALTER COLUMN id SET DEFAULT nextval('auto_incrementing_pk_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: test; Owner: -
--

ALTER TABLE ONLY has_fk ALTER COLUMN id SET DEFAULT nextval('has_fk_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: test; Owner: -
--

ALTER TABLE ONLY items ALTER COLUMN id SET DEFAULT nextval('items_id_seq'::regclass);


SET search_path = postgrest, pg_catalog;

--
-- Name: auth_pkey; Type: CONSTRAINT; Schema: postgrest; Owner: -
--

ALTER TABLE ONLY auth
    ADD CONSTRAINT auth_pkey PRIMARY KEY (id);


SET search_path = private, pg_catalog;

--
-- Name: articles_pkey; Type: CONSTRAINT; Schema: private; Owner: -
--

ALTER TABLE ONLY articles
    ADD CONSTRAINT articles_pkey PRIMARY KEY (id);


--
-- Name: user_article; Type: CONSTRAINT; Schema: private; Owner: -
--

ALTER TABLE ONLY article_stars
    ADD CONSTRAINT user_article PRIMARY KEY (article_id, user_id);


SET search_path = test, pg_catalog;

--
-- Name: authors_only_pkey; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY authors_only
    ADD CONSTRAINT authors_only_pkey PRIMARY KEY (secret);


--
-- Name: auto_incrementing_pk_pkey; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY auto_incrementing_pk
    ADD CONSTRAINT auto_incrementing_pk_pkey PRIMARY KEY (id);


--
-- Name: clients_pkey; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY clients
    ADD CONSTRAINT clients_pkey PRIMARY KEY (id);


--
-- Name: comments_pkey; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY comments
    ADD CONSTRAINT comments_pkey PRIMARY KEY (id);


--
-- Name: complex_items_pkey; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY complex_items
    ADD CONSTRAINT complex_items_pkey PRIMARY KEY (id);


--
-- Name: compound_pk_pkey; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY compound_pk
    ADD CONSTRAINT compound_pk_pkey PRIMARY KEY (k1, k2);


--
-- Name: contacts_pkey; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY simple_pk
    ADD CONSTRAINT contacts_pkey PRIMARY KEY (k);


--
-- Name: has_fk_pkey; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY has_fk
    ADD CONSTRAINT has_fk_pkey PRIMARY KEY (id);


--
-- Name: items_pkey; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY items
    ADD CONSTRAINT items_pkey PRIMARY KEY (id);


--
-- Name: menagerie_pkey; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY menagerie
    ADD CONSTRAINT menagerie_pkey PRIMARY KEY ("integer");


--
-- Name: project_user; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY users_projects
    ADD CONSTRAINT project_user PRIMARY KEY (project_id, user_id);


--
-- Name: projects_pkey; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY projects
    ADD CONSTRAINT projects_pkey PRIMARY KEY (id);


--
-- Name: task_user; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY users_tasks
    ADD CONSTRAINT task_user PRIMARY KEY (task_id, user_id);


--
-- Name: tasks_pkey; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY tasks
    ADD CONSTRAINT tasks_pkey PRIMARY KEY (id);


--
-- Name: users_pkey; Type: CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


SET search_path = postgrest, pg_catalog;

--
-- Name: ensure_auth_role_exists; Type: TRIGGER; Schema: postgrest; Owner: -
--

CREATE CONSTRAINT TRIGGER ensure_auth_role_exists AFTER INSERT OR UPDATE ON auth NOT DEFERRABLE INITIALLY IMMEDIATE FOR EACH ROW EXECUTE PROCEDURE check_role_exists();


SET search_path = private, pg_catalog;

--
-- Name: articles_owner_track; Type: TRIGGER; Schema: private; Owner: -
--

CREATE TRIGGER articles_owner_track BEFORE INSERT OR UPDATE ON articles FOR EACH ROW EXECUTE PROCEDURE postgrest.update_owner();


SET search_path = test, pg_catalog;

--
-- Name: insert_insertable_view_with_join; Type: TRIGGER; Schema: test; Owner: -
--

CREATE TRIGGER insert_insertable_view_with_join INSTEAD OF INSERT ON insertable_view_with_join FOR EACH ROW EXECUTE PROCEDURE insert_insertable_view_with_join();


--
-- Name: secrets_owner_track; Type: TRIGGER; Schema: test; Owner: -
--

CREATE TRIGGER secrets_owner_track BEFORE INSERT OR UPDATE ON authors_only FOR EACH ROW EXECUTE PROCEDURE postgrest.set_authors_only_owner();


SET search_path = private, pg_catalog;

--
-- Name: article_stars_article_id_fkey; Type: FK CONSTRAINT; Schema: private; Owner: -
--

ALTER TABLE ONLY article_stars
    ADD CONSTRAINT article_stars_article_id_fkey FOREIGN KEY (article_id) REFERENCES articles(id);


--
-- Name: article_stars_user_id_fkey; Type: FK CONSTRAINT; Schema: private; Owner: -
--

ALTER TABLE ONLY article_stars
    ADD CONSTRAINT article_stars_user_id_fkey FOREIGN KEY (user_id) REFERENCES test.users(id);


SET search_path = test, pg_catalog;

--
-- Name: comments_commenter_id_fkey; Type: FK CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY comments
    ADD CONSTRAINT comments_commenter_id_fkey FOREIGN KEY (commenter_id) REFERENCES users(id);


--
-- Name: comments_task_id_fkey; Type: FK CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY comments
    ADD CONSTRAINT comments_task_id_fkey FOREIGN KEY (task_id, user_id) REFERENCES users_tasks(task_id, user_id);


--
-- Name: has_fk_fk_fkey; Type: FK CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY has_fk
    ADD CONSTRAINT has_fk_fk_fkey FOREIGN KEY (auto_inc_fk) REFERENCES auto_incrementing_pk(id);


--
-- Name: has_fk_simple_fk_fkey; Type: FK CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY has_fk
    ADD CONSTRAINT has_fk_simple_fk_fkey FOREIGN KEY (simple_fk) REFERENCES simple_pk(k);


--
-- Name: projects_client_id_fkey; Type: FK CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY projects
    ADD CONSTRAINT projects_client_id_fkey FOREIGN KEY (client_id) REFERENCES clients(id);


--
-- Name: tasks_project_id_fkey; Type: FK CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY tasks
    ADD CONSTRAINT tasks_project_id_fkey FOREIGN KEY (project_id) REFERENCES projects(id);


--
-- Name: users_projects_project_id_fkey; Type: FK CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY users_projects
    ADD CONSTRAINT users_projects_project_id_fkey FOREIGN KEY (project_id) REFERENCES projects(id);


--
-- Name: users_projects_user_id_fkey; Type: FK CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY users_projects
    ADD CONSTRAINT users_projects_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: users_tasks_task_id_fkey; Type: FK CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY users_tasks
    ADD CONSTRAINT users_tasks_task_id_fkey FOREIGN KEY (task_id) REFERENCES tasks(id);


--
-- Name: users_tasks_user_id_fkey; Type: FK CONSTRAINT; Schema: test; Owner: -
--

ALTER TABLE ONLY users_tasks
    ADD CONSTRAINT users_tasks_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


create table addresses (
	id                   int not null unique,
	address              text not null
);

create table orders (
	id                   int not null unique,
	name                 text not null,
	billing_address_id   int references addresses(id),
	shipping_address_id  int references addresses(id)
);

CREATE FUNCTION getproject(id int) RETURNS SETOF projects
    LANGUAGE sql
    AS $_$
    SELECT * FROM test.projects WHERE id = $1;
$_$;

CREATE FUNCTION get_projects_below(id int) RETURNS SETOF projects
    LANGUAGE sql
    AS $_$
    SELECT * FROM test.projects WHERE id < $1;
$_$;

CREATE FUNCTION getallprojects() RETURNS SETOF projects
    LANGUAGE sql
    AS $_$
    SELECT * FROM test.projects;
$_$;

CREATE FUNCTION setprojects(id_l int, id_h int, name text) RETURNS SETOF projects
    LANGUAGE sql
    AS $_$
    update test.projects set name = $3 WHERE id >= $1 AND id <= $2 returning *;
$_$;

create table images (
	name               text not null,
	img                bytea not null
);

create view images_base64 as (
  -- encoding in base64 puts a '\n' after every 76 character due to legacy reasons, this is isn't necessary here so it's removed
  select name, replace(encode(img, 'base64'), E'\n', '') as img from images
);

create function test.ret_enum(val text) returns test.enum_menagerie_type as $$
  select val::test.enum_menagerie_type;
$$ language sql;

create domain one_nine as integer check (value >= 1 and value <= 9);

create function test.ret_array() returns integer[] as $$
  select '{1,2,3}'::integer[];
$$ language sql;

create function test.ret_domain(val integer) returns test.one_nine as $$
  select val::test.one_nine;
$$ language sql;

create function test.ret_range(low integer, up integer) returns int4range as $$
  select int4range(low, up);
$$ language sql;

create function test.ret_setof_integers() returns setof integer as $$
  values (1), (2), (3);
$$ language sql;

create function test.ret_scalars() returns table(
  a text, b test.enum_menagerie_type, c test.one_nine, d int4range
) as $$
  select row('scalars'::text, enum_first(null::test.enum_menagerie_type),
              1::test.one_nine, int4range(10, 20));
$$ language sql;

create type test.point_2d as (x integer, y integer);

create function test.ret_point_2d() returns test.point_2d as $$
  select row(10, 5)::test.point_2d;
$$ language sql;

create type private.point_3d as (x integer, y integer, z integer);

create function test.ret_point_3d() returns private.point_3d as $$
  select row(7, -3, 4)::private.point_3d;
$$ language sql;

create function test.ret_void() returns void as '' language sql;

create function test.ret_base64_bin() returns text as $$
  select i.img from test.images_base64 i where i.name = 'A.png';
$$ language sql;

create function test.ret_rows_with_base64_bin() returns setof test.images_base64 as $$
  select i.name, i.img from test.images_base64 i;
$$ language sql;

create function test.single_article(id integer) returns test.articles as $$
  select a.* from test.articles a where a.id = $1;
$$ language sql;

create function test.get_guc_value(name text) returns text as $$
  select nullif(current_setting(name), '')::text;
$$ language sql;

create table w_or_wo_comma_names ( name text );

create table items_with_different_col_types (
  int_data integer,
  text_data text,
  bool_data bool,
  bin_data bytea,
  char_data character varying,
  date_data date,
  real_data real,
  time_data time
);

-- Tables used for testing complex boolean logic with and/or query params

create table entities (
  id integer primary key,
  name text,
  arr integer[],
  text_search_vector tsvector
);

create table child_entities (
  id integer primary key,
  name text,
  parent_id integer references entities(id)
);

create table grandchild_entities (
  id integer primary key,
  name text,
  parent_id integer references child_entities(id),
  or_starting_col text,
  and_starting_col text,
  jsonb_col jsonb
);

-- Table used for testing range operators

create table ranges (
    id integer primary key,
    range numrange
);


-- OpenAPI description tests

comment on table child_entities is 'child_entities comment';
comment on column child_entities.id is 'child_entities id comment';
comment on column child_entities.name is 'child_entities name comment';

comment on table grandchild_entities is
$$grandchild_entities summary

grandchild_entities description
that spans
multiple lines$$;

-- Used for testing that having the same return column name as the proc name
-- doesn't conflict with the required output, details in #901
create function test.test() returns table(test text, value int) as $$
  values ('hello', 1);
$$ language sql;

create function test.privileged_hello(name text) returns text as $$
  select 'Privileged hello to ' || $1;
$$ language sql;

create function test.get_tsearch() returns setof test.tsearch AS $$
  SELECT * FROM test.tsearch;
$$ language sql;

create table test.being (
  being int primary key not null
);

create table test.descendant (
  descendant int primary key not null,
  being int references test.being(being)
);

create table test.part (
  part int primary key not null
);

create table test.being_part (
  being int not null references test.being(being),
  part int not null references test.part(part)
);

create function test.single_out_param(num int, OUT num_plus_one int) AS $$
  select num + 1;
$$ language sql;

create function test.single_json_out_param(a int, b text, OUT my_json pg_catalog.json) AS $$
  select json_build_object('a', a, 'b', b);
$$ language sql;

create function test.many_out_params(OUT my_json pg_catalog.json, OUT num int, OUT str text) AS $$
  select '{"a": 1, "b": "two"}'::json, 3, 'four'::text;
$$ language sql;

create function test.single_inout_param(INOUT num int) AS $$
  select num + 1;
$$ language sql;

create function test.many_inout_params(INOUT num int, INOUT str text, INOUT b bool DEFAULT true) AS $$
  select num, str, b;
$$ language sql;

create or replace function test.raise_pt402() returns void as $$
begin
  raise sqlstate 'PT402' using message = 'Payment Required',
                               detail = 'Quota exceeded',
                               hint = 'Upgrade your plan';
end;
$$ language plpgsql;

create or replace function test.raise_bad_pt() returns void as $$
begin
  raise sqlstate 'PT40A' using message = 'Wrong';
end;
$$ language plpgsql;

create or replace function test.get_projects_and_guc_headers() returns setof test.projects as $$
  set local "response.headers" = '[{"X-Test": "key1=val1; someValue; key2=val2"}, {"X-Test-2": "key1=val1"}]';
  select * from test.projects;
$$ language sql;

create or replace function test.get_int_and_guc_headers(num int) returns integer as $$
  set local "response.headers" = '[{"X-Test":"key1=val1; someValue; key2=val2"},{"X-Test-2":"key1=val1"}]';
  select num;
$$ language sql;

create or replace function test.bad_guc_headers_1() returns void as $$
  set local "response.headers" = '{"X-Test": "invalid structure for headers"}';
$$ language sql;

create or replace function test.bad_guc_headers_2() returns void as $$
  set local "response.headers" = '["invalid", "structure", "for", "headers"]';
$$ language sql;

create or replace function test.bad_guc_headers_3() returns void as $$
  set local "response.headers" = '{"X-Test": "invalid", "X-Test-2": "structure", "X-Test-3": "for headers"}';
$$ language sql;

create or replace function test.set_cookie_twice() returns void as $$
  set local "response.headers" = '[{"Set-Cookie": "sessionid=38afes7a8; HttpOnly; Path=/"}, {"Set-Cookie": "id=a3fWa; Expires=Wed, 21 Oct 2015 07:28:00 GMT; Secure; HttpOnly"}]';
$$ language sql;

create or replace function test.three_defaults(a int default 1, b int default 2, c int default 3) returns int as $$
select a + b + c
$$ language sql;

create or replace function test.overloaded() returns setof int as $$
  values (1), (2), (3);
$$ language sql;

create or replace function test.overloaded(pg_catalog.json) returns table(x int, y text) as $$
  select * from json_to_recordset($1) as r(x int, y text);
$$ language sql;

create or replace function test.overloaded(a int, b int) returns int as $$
  select a + b
$$ language sql;

create or replace function test.overloaded(a text, b text, c text) returns text as $$
  select a || b || c
$$ language sql;

create table test.leak(
  id serial primary key,
  blob bytea
);

create function test.leak(blob bytea) returns void as $$ begin end; $$ language plpgsql;

create table test.perf_articles(
  id integer not null,
  body text not null
);

create table test.employees(
  first_name text,
  last_name text,
  salary money,
  company text,
  occupation text,
  primary key(first_name, last_name)
);

create table test.tiobe_pls(
  name text primary key,
  rank smallint
);

create table test.family_tree (
  id text not null primary key,
  name text not null,
  parent text
);
alter table only test.family_tree add constraint pptr foreign key (parent) references test.family_tree(id);

create table test.organizations (
  id integer primary key,
  name text,
  referee integer,
  auditor integer
);
alter table only test.organizations add constraint pptr1 foreign key (referee) references test.organizations(id);
alter table only test.organizations add constraint pptr2 foreign key (auditor) references test.organizations(id);

create table private.authors(
  id integer primary key,
  name text
);

create table private.publishers(
  id integer primary key,
  name text
);

create table private.books(
  id integer primary key,
  title text,
  publication_year smallint,
  author_id integer references private.authors(id),
  first_publisher_id integer references private.publishers(id)
);

create view test.authors as select id, name from private.authors;

create view test.books as select id, title, publication_year, author_id from private.books;
create view test.forties_books as select id, title, publication_year, author_id from private.books where publication_year >= 1940 and publication_year < 1950;
create view test.fifties_books as select id, title, publication_year, author_id from private.books where publication_year >= 1950 and publication_year < 1960;
create view test.sixties_books as select id, title, publication_year, author_id from private.books where publication_year >= 1960 and publication_year < 1970;

create table person (
  id integer primary key,
  name character varying not null);

create table message (
  id integer primary key,
  body text not null default '',
  sender bigint not null references person(id),
  recipient bigint not null references person(id));

create view person_detail as
  select p.id, p.name, s.count as sent, r.count as received
  from person p
  join lateral (select message.sender, count(message.id) as count from message group by message.sender) s on s.sender = p.id
  join lateral (select message.recipient, count(message.id) as count from message group by message.recipient) r on r.recipient = p.id;

create table space(
  id integer primary key,
  name text);

create table zone(
  id integer primary key,
  name text,
  zone_type_id integer,
  space_id integer references space(id));

-- foreign table tests
create extension file_fdw;

create server import_csv foreign data wrapper file_fdw;

create foreign table projects_dump (
  id integer,
  name text,
  client_id integer
) server import_csv options ( filename '/tmp/projects_dump.csv', format 'csv');

comment on foreign table projects_dump is
$$A temporary projects dump

Just a test for foreign tables$$;

create table "UnitTest"(
  "idUnitTest" integer primary key,
  "nameUnitTest" text
);

create table json_arr(
  id integer primary key,
  data pg_catalog.json
);

create table jsonb_test(
  id integer primary key,
  data jsonb
);

create view test.authors_books_number as
select
  id,
  name,
  (
    select
      count(*)
    from forties_books where author_id = authors.id
  ) as num_in_forties,
  (
    select
      count(*)
    from fifties_books where author_id = authors.id
  ) as num_in_fifties,
  (
    select
      count(*)
    from sixties_books where author_id = authors.id
  ) as num_in_sixties,
  (
    select
      count(*)
    from (
      select id
      from forties_books where author_id = authors.id
      union
      select id
      from fifties_books where author_id = authors.id
      union
      select id
      from sixties_books where author_id = authors.id
    ) _
  ) as num_in_all_decades
from private.authors;

create view test.authors_have_book_in_decade as
select
  id,
  name,
  case
    when (x.id in (select author_id from test.forties_books))
    then true
    else false
  end as has_book_in_forties,
  case
    when (x.id in (select author_id from test.fifties_books))
    then true
    else false
  end as has_book_in_fifties,
  case
    when (x.id in (select author_id from test.sixties_books))
    then true
    else false
  end as has_book_in_sixties
from private.authors x;

create view test.forties_and_fifties_books as
select x.id, x.title, x.publication_year, y.name as first_publisher, x.author_id
from (
  select id, title, publication_year, author_id, first_publisher_id from private.books
  where publication_year >= 1940 and publication_year < 1960) x
join private.publishers y on y.id = x.first_publisher_id;

create view test.odd_years_publications as
with
odd_years_books as(
  select id, title, publication_year, author_id, first_publisher_id
  from private.books
  where publication_year % 2 <> 0
)
select
  x.id, x.title, x.publication_year,
  y.name as first_publisher, x.author_id
from odd_years_books x
join private.publishers y on y.id = x.first_publisher_id;

CREATE TABLE test."Foo"(
  id int primary key,
  name text
);

CREATE TABLE test.bar(
  id int primary key,
  name text,
  "fooId" int references "Foo"(id)
);

CREATE VIEW test.foos as select id,name from "Foo";
CREATE VIEW test.bars as select id, "fooId", name from bar;

create materialized view materialized_projects as
select id, name, client_id from projects;

comment on materialized view materialized_projects is
$$A materialized view for projects

Just a test for materialized views$$;

create or replace function test."quotedFunction"("user" text, "fullName" text, "SSN" text)
returns jsonb AS $$
  select format('{"user": "%s", "fullName": "%s", "SSN": "%s"}', "user", "fullName", "SSN")::jsonb;
$$ language sql;

create table private.player (
  id integer not null,
  first_name text not null,
  last_name text not null,
  birth_date date,
  primary key (last_name, id, first_name, birth_date) -- just for testing a long compound pk
);

create table test.contract (
  tournament text not null,
  time tsrange not null,
  purchase_price int not null,
  id integer not null,
  first_name text not null,
  last_name text not null,
  birth_date date,
  foreign key (last_name, id, first_name, birth_date) references private.player
);

create view test.player_view as select * from private.player;

create view test.contract_view as select * from test.contract;

create type public.my_type AS enum ('something');

CREATE FUNCTION test.test_arg(my_arg public.my_type) RETURNS text AS $$
  SELECT 'foobar'::text;
$$ LANGUAGE sql;
