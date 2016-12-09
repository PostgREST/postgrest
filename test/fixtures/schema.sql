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

--
-- Name: postgrest; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA postgrest;


--
-- Name: private; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA private;


--
-- Name: test; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA test;


--
-- Name: تست; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA تست;


--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;

SET search_path = public, pg_catalog;

--
-- Name: jwt_token; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE jwt_token AS (
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


SET search_path = public, pg_catalog;

--
-- Name: always_true(test.items); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION always_true(test.items) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$ SELECT true $$;


--
-- Name: anti_id(test.items); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION anti_id(test.items) RETURNS bigint
    LANGUAGE sql STABLE
    AS $_$ SELECT $1.id * -1 $_$;



SET search_path = تست, pg_catalog;

CREATE TABLE موارد (
    هویت bigint NOT NULL
);


SET search_path = test, pg_catalog;

--
-- Name: getitemrange(bigint, bigint); Type: FUNCTION; Schema: test; Owner: -
--

CREATE FUNCTION getitemrange(min bigint, max bigint) RETURNS SETOF items
    LANGUAGE sql
    AS $_$
    SELECT * FROM test.items WHERE id > $1 AND id <= $2;
$_$;


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
    row_to_json(r), 'safe'
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


--
-- Name: jwt_test(); Type: FUNCTION; Schema: test; Owner: -
--

CREATE FUNCTION jwt_test() RETURNS public.jwt_token
    LANGUAGE sql SECURITY DEFINER
    AS $$
SELECT jwt.sign(
    row_to_json(r), 'safe'
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
      iss text, sub text, aud text, exp bigint,
      nbf bigint, iat bigint, jti text, "http://postgrest.com/foo" boolean
    )
    LANGUAGE sql SECURITY DEFINER
    AS $$
SELECT current_setting('request.jwt.claim.iss') as iss,
       current_setting('request.jwt.claim.sub') as sub,
       current_setting('request.jwt.claim.aud') as aud,
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

CREATE FUNCTION getallprojects() RETURNS SETOF projects
    LANGUAGE sql
    AS $_$
    SELECT * FROM test.projects;
$_$;

--
-- PostgreSQL database dump complete
--
