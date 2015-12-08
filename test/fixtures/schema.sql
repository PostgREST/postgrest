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
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

SET search_path = public, pg_catalog;

--
-- Name: jwt_claims; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE jwt_claims AS (
	role text,
	id text
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
  NEW.owner = current_setting('postgrest.claims.id');
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

CREATE FUNCTION login(id text, pass text) RETURNS public.jwt_claims
    LANGUAGE sql SECURITY DEFINER
    AS $$
SELECT rolname::text, id::text FROM postgrest.auth WHERE id = id AND pass = pass;
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
    arr_data integer[]
);


--
-- Name: compound_pk; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE compound_pk (
    k1 integer NOT NULL,
    k2 integer NOT NULL,
    extra integer
);


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


--
-- Name: simple_pk; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE simple_pk (
    k character varying NOT NULL,
    extra character varying NOT NULL
);

CREATE TABLE empty_table (
    k character varying NOT NULL,
    extra character varying NOT NULL
);

--
-- Name: tasks; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE tasks (
    id integer NOT NULL,
    name text NOT NULL,
    project_id integer
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
-- Name: users_projects; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE users_projects (
    user_id integer NOT NULL,
    project_id integer NOT NULL
);


--
-- Name: users_tasks; Type: TABLE; Schema: test; Owner: -
--

CREATE TABLE users_tasks (
    user_id integer NOT NULL,
    task_id integer NOT NULL
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


--
-- Name: postgrest; Type: ACL; Schema: -; Owner: -
--

REVOKE ALL ON SCHEMA postgrest FROM PUBLIC;
REVOKE ALL ON SCHEMA postgrest FROM postgrest_test;
GRANT ALL ON SCHEMA postgrest TO postgrest_test;
GRANT USAGE ON SCHEMA postgrest TO postgrest_anonymous;

--
-- Name: test; Type: ACL; Schema: -; Owner: -
--

GRANT ALL ON SCHEMA test TO postgrest_test;
GRANT USAGE ON SCHEMA test TO postgrest_anonymous;
GRANT USAGE ON SCHEMA test TO postgrest_test_author;


--
-- Name: items; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE items FROM PUBLIC;
REVOKE ALL ON TABLE items FROM postgrest_test;
GRANT ALL ON TABLE items TO postgrest_test;
GRANT ALL ON TABLE items TO postgrest_anonymous;


SET search_path = public, pg_catalog;

--
-- Name: always_true(test.items); Type: ACL; Schema: public; Owner: -
--

REVOKE ALL ON FUNCTION always_true(test.items) FROM PUBLIC;
REVOKE ALL ON FUNCTION always_true(test.items) FROM postgrest_test;
GRANT ALL ON FUNCTION always_true(test.items) TO postgrest_test;
GRANT ALL ON FUNCTION always_true(test.items) TO postgrest_anonymous;


SET search_path = test, pg_catalog;

--
-- Name: getitemrange(bigint, bigint); Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON FUNCTION getitemrange(min bigint, max bigint) FROM PUBLIC;
GRANT ALL ON FUNCTION getitemrange(min bigint, max bigint) TO postgrest_test;
GRANT ALL ON FUNCTION getitemrange(min bigint, max bigint) TO postgrest_anonymous;


--
-- Name: login(text, text); Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON FUNCTION login(id text, pass text) FROM PUBLIC;
GRANT ALL ON FUNCTION login(id text, pass text) TO postgrest_test;
GRANT ALL ON FUNCTION login(id text, pass text) TO postgrest_anonymous;


--
-- Name: problem(); Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON FUNCTION problem() FROM PUBLIC;
GRANT ALL ON FUNCTION problem() TO postgrest_test_author;


--
-- Name: sayhello(text); Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON FUNCTION sayhello(name text) FROM PUBLIC;
GRANT ALL ON FUNCTION sayhello(name text) TO postgrest_test;
GRANT ALL ON FUNCTION sayhello(name text) TO postgrest_anonymous;


--
-- Name: test_empty_rowset(); Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON FUNCTION test_empty_rowset() FROM PUBLIC;
GRANT ALL ON FUNCTION test_empty_rowset() TO postgrest_test;
GRANT ALL ON FUNCTION test_empty_rowset() TO postgrest_anonymous;


SET search_path = postgrest, pg_catalog;

--
-- Name: auth; Type: ACL; Schema: postgrest; Owner: -
--

REVOKE ALL ON TABLE auth FROM PUBLIC;
REVOKE ALL ON TABLE auth FROM postgrest_test;
GRANT ALL ON TABLE auth TO postgrest_test;
GRANT INSERT ON TABLE auth TO postgrest_anonymous;


SET search_path = private, pg_catalog;

--
-- Name: articles; Type: ACL; Schema: private; Owner: -
--

REVOKE ALL ON TABLE articles FROM PUBLIC;
REVOKE ALL ON TABLE articles FROM postgrest_test;
GRANT ALL ON TABLE articles TO postgrest_test;


SET search_path = test, pg_catalog;

--
-- Name: articleStars; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE "articleStars" FROM PUBLIC;
REVOKE ALL ON TABLE "articleStars" FROM postgrest_test;
GRANT ALL ON TABLE "articleStars" TO postgrest_test;
GRANT ALL ON TABLE "articleStars" TO postgrest_anonymous;


--
-- Name: articles; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE articles FROM PUBLIC;
REVOKE ALL ON TABLE articles FROM postgrest_test;
GRANT ALL ON TABLE articles TO postgrest_test;
GRANT ALL ON TABLE articles TO postgrest_anonymous;


--
-- Name: authors_only; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE authors_only FROM PUBLIC;
REVOKE ALL ON TABLE authors_only FROM postgrest_test_author;
GRANT ALL ON TABLE authors_only TO postgrest_test_author;


--
-- Name: auto_incrementing_pk; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE auto_incrementing_pk FROM PUBLIC;
REVOKE ALL ON TABLE auto_incrementing_pk FROM postgrest_test;
GRANT ALL ON TABLE auto_incrementing_pk TO postgrest_test;
GRANT ALL ON TABLE auto_incrementing_pk TO postgrest_anonymous;


--
-- Name: auto_incrementing_pk_id_seq; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON SEQUENCE auto_incrementing_pk_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE auto_incrementing_pk_id_seq FROM postgrest_test;
GRANT ALL ON SEQUENCE auto_incrementing_pk_id_seq TO postgrest_test;
GRANT USAGE ON SEQUENCE auto_incrementing_pk_id_seq TO postgrest_anonymous;


--
-- Name: clients; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE clients FROM PUBLIC;
REVOKE ALL ON TABLE clients FROM postgrest_test;
GRANT ALL ON TABLE clients TO postgrest_test;
GRANT ALL ON TABLE clients TO postgrest_anonymous;


--
-- Name: comments; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE comments FROM PUBLIC;
REVOKE ALL ON TABLE comments FROM postgrest_test;
GRANT ALL ON TABLE comments TO postgrest_test;
GRANT ALL ON TABLE comments TO postgrest_anonymous;


--
-- Name: complex_items; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE complex_items FROM PUBLIC;
REVOKE ALL ON TABLE complex_items FROM postgrest_test;
GRANT ALL ON TABLE complex_items TO postgrest_test;
GRANT ALL ON TABLE complex_items TO postgrest_anonymous;


--
-- Name: compound_pk; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE compound_pk FROM PUBLIC;
REVOKE ALL ON TABLE compound_pk FROM postgrest_test;
GRANT ALL ON TABLE compound_pk TO postgrest_test;
GRANT ALL ON TABLE compound_pk TO postgrest_anonymous;


--
-- Name: has_count_column; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE has_count_column FROM PUBLIC;
GRANT ALL ON TABLE has_count_column TO postgrest_test;
GRANT ALL ON TABLE has_count_column TO postgrest_anonymous;


--
-- Name: has_fk; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE has_fk FROM PUBLIC;
REVOKE ALL ON TABLE has_fk FROM postgrest_test;
GRANT ALL ON TABLE has_fk TO postgrest_test;
GRANT ALL ON TABLE has_fk TO postgrest_anonymous;


--
-- Name: insertable_view_with_join; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE insertable_view_with_join FROM PUBLIC;
REVOKE ALL ON TABLE insertable_view_with_join FROM postgrest_test;
GRANT ALL ON TABLE insertable_view_with_join TO postgrest_test;
GRANT ALL ON TABLE insertable_view_with_join TO postgrest_anonymous;


--
-- Name: items_id_seq; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON SEQUENCE items_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE items_id_seq FROM postgrest_test;
GRANT ALL ON SEQUENCE items_id_seq TO postgrest_test;
GRANT USAGE ON SEQUENCE items_id_seq TO postgrest_anonymous;


--
-- Name: json; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE json FROM PUBLIC;
REVOKE ALL ON TABLE json FROM postgrest_test;
GRANT ALL ON TABLE json TO postgrest_test;
GRANT ALL ON TABLE json TO postgrest_anonymous;


--
-- Name: materialized_view; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE materialized_view FROM PUBLIC;
REVOKE ALL ON TABLE materialized_view FROM postgrest_test;
GRANT ALL ON TABLE materialized_view TO postgrest_test;
GRANT ALL ON TABLE materialized_view TO postgrest_anonymous;


--
-- Name: menagerie; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE menagerie FROM PUBLIC;
REVOKE ALL ON TABLE menagerie FROM postgrest_test;
GRANT ALL ON TABLE menagerie TO postgrest_test;
GRANT ALL ON TABLE menagerie TO postgrest_anonymous;


--
-- Name: no_pk; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE no_pk FROM PUBLIC;
REVOKE ALL ON TABLE no_pk FROM postgrest_test;
GRANT ALL ON TABLE no_pk TO postgrest_test;
GRANT ALL ON TABLE no_pk TO postgrest_anonymous;


--
-- Name: nullable_integer; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE nullable_integer FROM PUBLIC;
REVOKE ALL ON TABLE nullable_integer FROM postgrest_test;
GRANT ALL ON TABLE nullable_integer TO postgrest_test;
GRANT ALL ON TABLE nullable_integer TO postgrest_anonymous;


--
-- Name: projects; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE projects FROM PUBLIC;
REVOKE ALL ON TABLE projects FROM postgrest_test;
GRANT ALL ON TABLE projects TO postgrest_test;
GRANT ALL ON TABLE projects TO postgrest_anonymous;


--
-- Name: projects_view; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE projects_view FROM PUBLIC;
REVOKE ALL ON TABLE projects_view FROM postgrest_test;
GRANT ALL ON TABLE projects_view TO postgrest_test;
GRANT ALL ON TABLE projects_view TO postgrest_anonymous;


--
-- Name: simple_pk; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE simple_pk FROM PUBLIC;
REVOKE ALL ON TABLE simple_pk FROM postgrest_test;
GRANT ALL ON TABLE simple_pk TO postgrest_test;
GRANT ALL ON TABLE simple_pk TO postgrest_anonymous;


--
-- Name: tasks; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE tasks FROM PUBLIC;
REVOKE ALL ON TABLE tasks FROM postgrest_test;
GRANT ALL ON TABLE tasks TO postgrest_test;
GRANT ALL ON TABLE tasks TO postgrest_anonymous;


--
-- Name: tsearch; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE tsearch FROM PUBLIC;
REVOKE ALL ON TABLE tsearch FROM postgrest_test;
GRANT ALL ON TABLE tsearch TO postgrest_test;
GRANT ALL ON TABLE tsearch TO postgrest_anonymous;


--
-- Name: users; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE users FROM PUBLIC;
REVOKE ALL ON TABLE users FROM postgrest_test;
GRANT ALL ON TABLE users TO postgrest_test;
GRANT ALL ON TABLE users TO postgrest_anonymous;


--
-- Name: users_projects; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE users_projects FROM PUBLIC;
REVOKE ALL ON TABLE users_projects FROM postgrest_test;
GRANT ALL ON TABLE users_projects TO postgrest_test;
GRANT ALL ON TABLE users_projects TO postgrest_anonymous;


--
-- Name: users_tasks; Type: ACL; Schema: test; Owner: -
--

REVOKE ALL ON TABLE users_tasks FROM PUBLIC;
REVOKE ALL ON TABLE users_tasks FROM postgrest_test;
GRANT ALL ON TABLE users_tasks TO postgrest_test;
GRANT ALL ON TABLE users_tasks TO postgrest_anonymous;


--
-- PostgreSQL database dump complete
--
