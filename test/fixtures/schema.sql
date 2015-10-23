SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;


CREATE SCHEMA test;


ALTER SCHEMA test OWNER TO postgrest_test;


CREATE SCHEMA postgrest;


ALTER SCHEMA postgrest OWNER TO postgrest_test;


CREATE SCHEMA private;


ALTER SCHEMA private OWNER TO postgrest_test;


CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;



COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = test, pg_catalog;


CREATE TYPE enum_menagerie_type AS ENUM (
    'foo',
    'bar'
);


ALTER TYPE test.enum_menagerie_type OWNER TO postgrest_test;

SET search_path = postgrest, pg_catalog;


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


ALTER FUNCTION postgrest.check_role_exists() OWNER TO postgrest_test;


CREATE FUNCTION update_owner() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.owner = current_user;
   RETURN NEW;
END;
$$;


ALTER FUNCTION postgrest.update_owner() OWNER TO postgrest_test;

CREATE FUNCTION set_authors_only_owner() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  NEW.owner = current_setting('postgrest.claims.id');
  RETURN NEW;
end
$$;

ALTER FUNCTION postgrest.set_authors_only_owner() OWNER TO postgrest_test;

CREATE FUNCTION test.insert_insertable_view_with_join() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  INSERT INTO test.auto_incrementing_pk (nullable_string, non_nullable_string) VALUES (NEW.nullable_string, NEW.non_nullable_string);
  RETURN NEW;
end;
$$;

ALTER FUNCTION test.insert_insertable_view_with_join() OWNER TO postgrest_test;

SET search_path = test, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;


CREATE TABLE authors_only (
    owner character varying NOT NULL,
    secret character varying NOT NULL
);


ALTER TABLE test.authors_only OWNER TO postgrest_test_author;


CREATE TABLE auto_incrementing_pk (
    id integer NOT NULL,
    nullable_string character varying,
    non_nullable_string character varying NOT NULL,
    inserted_at timestamp with time zone DEFAULT now()
);


ALTER TABLE test.auto_incrementing_pk OWNER TO postgrest_test;


CREATE SEQUENCE auto_incrementing_pk_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE test.auto_incrementing_pk_id_seq OWNER TO postgrest_test;


ALTER SEQUENCE auto_incrementing_pk_id_seq OWNED BY auto_incrementing_pk.id;



CREATE TABLE compound_pk (
    k1 integer NOT NULL,
    k2 integer NOT NULL,
    extra integer
);


ALTER TABLE test.compound_pk OWNER TO postgrest_test;


CREATE TABLE has_fk (
    id bigint NOT NULL,
    auto_inc_fk integer,
    simple_fk character varying(255)
);


ALTER TABLE test.has_fk OWNER TO postgrest_test;


CREATE SEQUENCE has_fk_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE test.has_fk_id_seq OWNER TO postgrest_test;


ALTER SEQUENCE has_fk_id_seq OWNED BY has_fk.id;

CREATE MATERIALIZED VIEW test.materialized_view AS
 SELECT
    version();

ALTER TABLE test.materialized_view OWNER TO postgrest_test;

CREATE VIEW test.insertable_view_with_join AS
 SELECT has_fk.id,
    has_fk.auto_inc_fk,
    has_fk.simple_fk,
    auto_incrementing_pk.nullable_string,
    auto_incrementing_pk.non_nullable_string,
    auto_incrementing_pk.inserted_at
   FROM (has_fk
     JOIN auto_incrementing_pk USING (id));


ALTER TABLE test.insertable_view_with_join OWNER TO postgrest_test;

CREATE VIEW test.has_count_column AS
 SELECT 1 AS count;

ALTER TABLE test.insertable_view_with_join OWNER TO postgrest_test;


CREATE TABLE items (
    id bigint NOT NULL
);


ALTER TABLE test.items OWNER TO postgrest_test;

CREATE TABLE complex_items (
    id bigint NOT NULL,
    name text,
    settings json
);


ALTER TABLE test.complex_items OWNER TO postgrest_test;

--- Structure for testing table relations
CREATE TABLE clients(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL
);
ALTER TABLE test.clients OWNER TO postgrest_test;

CREATE TABLE projects(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    client_id      INT     REFERENCES clients(id)
);
ALTER TABLE test.projects OWNER TO postgrest_test;

CREATE TABLE tasks(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    project_id      INT     REFERENCES projects(id)
);
ALTER TABLE test.tasks OWNER TO postgrest_test;

CREATE TABLE users(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL
);
ALTER TABLE test.users OWNER TO postgrest_test;

CREATE TABLE users_tasks(
    user_id      INT     REFERENCES users(id),
    task_id      INT     REFERENCES tasks(id),
    CONSTRAINT task_user PRIMARY KEY (task_id,user_id)
);
ALTER TABLE test.users_tasks OWNER TO postgrest_test;

CREATE TABLE comments(
id INT PRIMARY KEY   NOT NULL,
commenter_id INT     NOT NULL REFERENCES users(id),
user_id      INT     NOT NULL,
task_id      INT     NOT NULL,
content      TEXT    NOT NULL,
FOREIGN KEY (task_id,user_id) REFERENCES users_tasks (task_id,user_id)
);
ALTER TABLE test.comments OWNER TO postgrest_test;

CREATE TABLE users_projects(
    user_id         INT     REFERENCES users(id),
    project_id      INT     REFERENCES projects(id),
    CONSTRAINT project_user PRIMARY KEY (project_id, user_id)
);
ALTER TABLE test.users_projects OWNER TO postgrest_test;

CREATE VIEW test.projects_view AS
  SELECT
      projects.id,
      projects.name,
      projects.client_id
  FROM projects;
ALTER TABLE test.projects_view OWNER TO postgrest_test;

CREATE SEQUENCE items_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE test.items_id_seq OWNER TO postgrest_test;


ALTER SEQUENCE items_id_seq OWNED BY items.id;



CREATE FUNCTION test.getitemrange(min bigint, max bigint) RETURNS SETOF test.items AS $$
    SELECT * FROM test.items WHERE id > $1 AND id <= $2;
$$ LANGUAGE SQL;


CREATE TYPE public.jwt_claims AS (role text, id text);
CREATE FUNCTION test.login(id text, pass text)
RETURNS public.jwt_claims
SECURITY DEFINER
AS $$
SELECT rolname::text, id::text FROM postgrest.auth WHERE id = id AND pass = pass;
$$ LANGUAGE SQL;

CREATE FUNCTION test.sayhello(name text) RETURNS text AS $$
    SELECT 'Hello, ' || $1;
$$ LANGUAGE SQL;


CREATE FUNCTION test.problem() RETURNS void LANGUAGE plpgsql AS
$$
BEGIN
      RAISE 'bad thing';
END;
$$;


CREATE TABLE menagerie (
    "integer" integer NOT NULL,
    double double precision NOT NULL,
    "varchar" character varying NOT NULL,
    "boolean" boolean NOT NULL,
    date date NOT NULL,
    money money NOT NULL,
    enum enum_menagerie_type NOT NULL
);


ALTER TABLE test.menagerie OWNER TO postgrest_test;


CREATE TABLE no_pk (
    a character varying,
    b character varying
);


ALTER TABLE test.no_pk OWNER TO postgrest_test;


CREATE TABLE nullable_integer (
    a integer
);


ALTER TABLE test.nullable_integer OWNER TO postgrest_test;


CREATE TABLE simple_pk (
    k character varying NOT NULL,
    extra character varying NOT NULL
);


ALTER TABLE test.simple_pk OWNER TO postgrest_test;


CREATE TABLE json
(
  data json
);


ALTER TABLE test.json OWNER TO postgrest_test;


CREATE TABLE tsearch (
    text_search_vector tsvector
);

ALTER TABLE test.tsearch OWNER TO postgrest_test;

SET search_path = postgrest, pg_catalog;


CREATE TABLE auth (
    id character varying NOT NULL,
    rolname name NOT NULL DEFAULT 'postgrest_test_author',
    pass character(60) NOT NULL
);


ALTER TABLE postgrest.auth OWNER TO postgrest_test;

SET search_path = private, pg_catalog;


CREATE TABLE articles (
    body text,
    id integer NOT NULL,
    owner name NOT NULL
);


ALTER TABLE private.articles OWNER TO postgrest_test;


CREATE SEQUENCE articles_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE private.articles_id_seq OWNER TO postgrest_test;


ALTER SEQUENCE articles_id_seq OWNED BY articles.id;


SET search_path = test, pg_catalog;


ALTER TABLE ONLY auto_incrementing_pk ALTER COLUMN id SET DEFAULT nextval('auto_incrementing_pk_id_seq'::regclass);



ALTER TABLE ONLY has_fk ALTER COLUMN id SET DEFAULT nextval('has_fk_id_seq'::regclass);



ALTER TABLE ONLY items ALTER COLUMN id SET DEFAULT nextval('items_id_seq'::regclass);


SET search_path = private, pg_catalog;


ALTER TABLE ONLY articles ALTER COLUMN id SET DEFAULT nextval('articles_id_seq'::regclass);


SET search_path = test, pg_catalog;








SELECT pg_catalog.setval('auto_incrementing_pk_id_seq', 1, true);









SELECT pg_catalog.setval('has_fk_id_seq', 1, false);



INSERT INTO items (id) VALUES (1);

SELECT pg_catalog.setval('items_id_seq', 1, true);


INSERT INTO tsearch (text_search_vector) VALUES ('''bar'':2 ''foo'':1');
INSERT INTO tsearch (text_search_vector) VALUES ('''baz'':1 ''qux'':2');

SET search_path = postgrest, pg_catalog;




SET search_path = private, pg_catalog;





SELECT pg_catalog.setval('articles_id_seq', 1, false);


SET search_path = test, pg_catalog;

CREATE FUNCTION public.always_true(test.items) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$ SELECT true $$;

ALTER FUNCTION public.always_true(test.items) OWNER TO postgrest_test;



ALTER TABLE ONLY authors_only
    ADD CONSTRAINT authors_only_pkey PRIMARY KEY (secret);

CREATE TRIGGER insert_insertable_view_with_join INSTEAD OF INSERT ON test.insertable_view_with_join FOR EACH ROW EXECUTE PROCEDURE test.insert_insertable_view_with_join();


CREATE TRIGGER secrets_owner_track BEFORE INSERT OR UPDATE ON authors_only FOR EACH ROW EXECUTE PROCEDURE postgrest.set_authors_only_owner();


ALTER TABLE ONLY auto_incrementing_pk
    ADD CONSTRAINT auto_incrementing_pk_pkey PRIMARY KEY (id);



ALTER TABLE ONLY compound_pk
    ADD CONSTRAINT compound_pk_pkey PRIMARY KEY (k1, k2);



ALTER TABLE ONLY simple_pk
    ADD CONSTRAINT contacts_pkey PRIMARY KEY (k);



ALTER TABLE ONLY has_fk
    ADD CONSTRAINT has_fk_pkey PRIMARY KEY (id);



ALTER TABLE ONLY items
    ADD CONSTRAINT items_pkey PRIMARY KEY (id);

ALTER TABLE ONLY complex_items
    ADD CONSTRAINT complex_items_pkey PRIMARY KEY (id);


ALTER TABLE ONLY menagerie
    ADD CONSTRAINT menagerie_pkey PRIMARY KEY ("integer");


SET search_path = postgrest, pg_catalog;


ALTER TABLE ONLY auth
    ADD CONSTRAINT auth_pkey PRIMARY KEY (id);


SET search_path = private, pg_catalog;


ALTER TABLE ONLY articles
    ADD CONSTRAINT articles_pkey PRIMARY KEY (id);


SET search_path = postgrest, pg_catalog;


CREATE CONSTRAINT TRIGGER ensure_auth_role_exists AFTER INSERT OR UPDATE ON auth NOT DEFERRABLE INITIALLY IMMEDIATE FOR EACH ROW EXECUTE PROCEDURE check_role_exists();


SET search_path = private, pg_catalog;


CREATE TRIGGER articles_owner_track BEFORE INSERT OR UPDATE ON articles FOR EACH ROW EXECUTE PROCEDURE postgrest.update_owner();


SET search_path = test, pg_catalog;


ALTER TABLE ONLY has_fk
    ADD CONSTRAINT has_fk_fk_fkey FOREIGN KEY (auto_inc_fk) REFERENCES auto_incrementing_pk(id);



ALTER TABLE ONLY has_fk
    ADD CONSTRAINT has_fk_simple_fk_fkey FOREIGN KEY (simple_fk) REFERENCES simple_pk(k);



REVOKE ALL ON SCHEMA test FROM PUBLIC;
REVOKE ALL ON SCHEMA test FROM postgrest_test;
GRANT ALL ON SCHEMA test TO postgrest_test;
GRANT USAGE ON SCHEMA test TO postgrest_anonymous;
GRANT USAGE ON SCHEMA test TO postgrest_test_author;



REVOKE ALL ON SCHEMA postgrest FROM PUBLIC;
REVOKE ALL ON SCHEMA postgrest FROM postgrest_test;
GRANT ALL ON SCHEMA postgrest TO postgrest_test;
GRANT USAGE ON SCHEMA postgrest TO postgrest_anonymous;



REVOKE ALL ON SCHEMA public FROM PUBLIC;
GRANT ALL ON SCHEMA public TO PUBLIC;



REVOKE ALL ON TABLE authors_only FROM PUBLIC;
REVOKE ALL ON TABLE authors_only FROM postgrest_test_author;
GRANT ALL ON TABLE authors_only TO postgrest_test_author;



REVOKE ALL ON TABLE auto_incrementing_pk FROM PUBLIC;
REVOKE ALL ON TABLE auto_incrementing_pk FROM postgrest_test;
GRANT ALL ON TABLE auto_incrementing_pk TO postgrest_test;
GRANT ALL ON TABLE auto_incrementing_pk TO postgrest_anonymous;



REVOKE ALL ON SEQUENCE auto_incrementing_pk_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE auto_incrementing_pk_id_seq FROM postgrest_test;
GRANT ALL ON SEQUENCE auto_incrementing_pk_id_seq TO postgrest_test;
GRANT USAGE ON SEQUENCE auto_incrementing_pk_id_seq TO postgrest_anonymous;



REVOKE ALL ON TABLE compound_pk FROM PUBLIC;
REVOKE ALL ON TABLE compound_pk FROM postgrest_test;
GRANT ALL ON TABLE compound_pk TO postgrest_test;
GRANT ALL ON TABLE compound_pk TO postgrest_anonymous;


REVOKE ALL ON TABLE has_fk FROM PUBLIC;
REVOKE ALL ON TABLE has_fk FROM postgrest_test;
GRANT ALL ON TABLE has_fk TO postgrest_test;
GRANT ALL ON TABLE has_fk TO postgrest_anonymous;


REVOKE ALL ON TABLE items FROM PUBLIC;
REVOKE ALL ON TABLE items FROM postgrest_test;
GRANT ALL ON TABLE items TO postgrest_test;
GRANT ALL ON TABLE items TO postgrest_anonymous;

REVOKE ALL ON TABLE complex_items FROM PUBLIC;
REVOKE ALL ON TABLE complex_items FROM postgrest_test;
GRANT ALL ON TABLE complex_items TO postgrest_test;
GRANT ALL ON TABLE complex_items TO postgrest_anonymous;

---------
REVOKE ALL ON TABLE clients FROM PUBLIC;
REVOKE ALL ON TABLE clients FROM postgrest_test;
GRANT ALL ON TABLE clients TO postgrest_test;
GRANT ALL ON TABLE clients TO postgrest_anonymous;
REVOKE ALL ON TABLE projects FROM PUBLIC;
REVOKE ALL ON TABLE projects FROM postgrest_test;
GRANT ALL ON TABLE projects TO postgrest_test;
GRANT ALL ON TABLE projects TO postgrest_anonymous;
REVOKE ALL ON TABLE tasks FROM PUBLIC;
REVOKE ALL ON TABLE tasks FROM postgrest_test;
GRANT ALL ON TABLE tasks TO postgrest_test;
GRANT ALL ON TABLE tasks TO postgrest_anonymous;
REVOKE ALL ON TABLE users FROM PUBLIC;
REVOKE ALL ON TABLE users FROM postgrest_test;
GRANT ALL ON TABLE users TO postgrest_test;
GRANT ALL ON TABLE users TO postgrest_anonymous;
REVOKE ALL ON TABLE users_tasks FROM PUBLIC;
REVOKE ALL ON TABLE users_tasks FROM postgrest_test;
GRANT ALL ON TABLE users_tasks TO postgrest_test;
GRANT ALL ON TABLE users_tasks TO postgrest_anonymous;
REVOKE ALL ON TABLE comments FROM PUBLIC;
REVOKE ALL ON TABLE comments FROM postgrest_test;
GRANT ALL ON TABLE  comments TO postgrest_test;
GRANT ALL ON TABLE  comments TO postgrest_anonymous;
REVOKE ALL ON TABLE users_projects FROM PUBLIC;
REVOKE ALL ON TABLE users_projects FROM postgrest_test;
GRANT ALL ON TABLE users_projects TO postgrest_test;
GRANT ALL ON TABLE users_projects TO postgrest_anonymous;
REVOKE ALL ON TABLE projects_view FROM PUBLIC;
REVOKE ALL ON TABLE projects_view FROM postgrest_test;
GRANT ALL ON TABLE projects_view TO postgrest_test;
GRANT ALL ON TABLE projects_view TO postgrest_anonymous;
---------


REVOKE ALL ON FUNCTION getitemrange(bigint, bigint) FROM PUBLIC;
REVOKE ALL ON FUNCTION getitemrange(bigint, bigint) FROM postgrest_test;
GRANT EXECUTE ON FUNCTION getitemrange(bigint, bigint) TO postgrest_test;
GRANT EXECUTE ON FUNCTION getitemrange(bigint, bigint) TO postgrest_anonymous;

REVOKE ALL ON FUNCTION login(text, text) FROM PUBLIC;
REVOKE ALL ON FUNCTION login(text, text) FROM postgrest_test;
GRANT EXECUTE ON FUNCTION login(text, text) TO postgrest_test;
GRANT EXECUTE ON FUNCTION login(text, text) TO postgrest_anonymous;

REVOKE ALL ON FUNCTION sayhello(text) FROM PUBLIC;
REVOKE ALL ON FUNCTION sayhello(text) FROM postgrest_test;
GRANT EXECUTE ON FUNCTION sayhello(text) TO postgrest_test;
GRANT EXECUTE ON FUNCTION sayhello(text) TO postgrest_anonymous;


REVOKE ALL ON FUNCTION problem() FROM PUBLIC;
REVOKE ALL ON FUNCTION problem() FROM postgrest_test_author;
GRANT EXECUTE ON FUNCTION problem() TO postgrest_test_author;


REVOKE ALL ON SEQUENCE items_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE items_id_seq FROM postgrest_test;
GRANT ALL ON SEQUENCE items_id_seq TO postgrest_test;
GRANT USAGE ON SEQUENCE items_id_seq TO postgrest_anonymous;



REVOKE ALL ON TABLE menagerie FROM PUBLIC;
REVOKE ALL ON TABLE menagerie FROM postgrest_test;
GRANT ALL ON TABLE menagerie TO postgrest_test;
GRANT ALL ON TABLE menagerie TO postgrest_anonymous;



REVOKE ALL ON TABLE no_pk FROM PUBLIC;
REVOKE ALL ON TABLE no_pk FROM postgrest_test;
GRANT ALL ON TABLE no_pk TO postgrest_test;
GRANT ALL ON TABLE no_pk TO postgrest_anonymous;



REVOKE ALL ON TABLE nullable_integer FROM PUBLIC;
REVOKE ALL ON TABLE nullable_integer FROM postgrest_test;
GRANT ALL ON TABLE nullable_integer TO postgrest_test;
GRANT ALL ON TABLE nullable_integer TO postgrest_anonymous;



REVOKE ALL ON TABLE simple_pk FROM PUBLIC;
REVOKE ALL ON TABLE simple_pk FROM postgrest_test;
GRANT ALL ON TABLE simple_pk TO postgrest_test;
GRANT ALL ON TABLE simple_pk TO postgrest_anonymous;



REVOKE ALL ON TABLE json FROM PUBLIC;
REVOKE ALL ON TABLE json FROM postgrest_test;
GRANT ALL ON TABLE json TO postgrest_test;
GRANT ALL ON TABLE json TO postgrest_anonymous;



REVOKE ALL ON TABLE tsearch FROM PUBLIC;
REVOKE ALL ON TABLE tsearch FROM postgrest_test;
GRANT ALL ON TABLE tsearch TO postgrest_test;
GRANT ALL ON TABLE tsearch TO postgrest_anonymous;

REVOKE ALL ON TABLE materialized_view FROM PUBLIC;
REVOKE ALL ON TABLE materialized_view FROM postgrest_test;
GRANT ALL ON TABLE materialized_view TO postgrest_test;
GRANT ALL ON TABLE materialized_view TO postgrest_anonymous;

REVOKE ALL ON TABLE insertable_view_with_join FROM PUBLIC;
REVOKE ALL ON TABLE insertable_view_with_join FROM postgrest_test;
GRANT ALL ON TABLE insertable_view_with_join TO postgrest_test;
GRANT ALL ON TABLE insertable_view_with_join TO postgrest_anonymous;

REVOKE ALL ON TABLE has_count_column FROM PUBLIC;
REVOKE ALL ON TABLE has_count_column FROM postgrest_test;
GRANT ALL ON TABLE has_count_column TO postgrest_test;
GRANT ALL ON TABLE has_count_column TO postgrest_anonymous;

REVOKE ALL ON FUNCTION public.always_true(test.items) FROM PUBLIC;
REVOKE ALL ON FUNCTION public.always_true(test.items) FROM postgrest_test;
GRANT ALL ON FUNCTION public.always_true(test.items) TO postgrest_test;
GRANT ALL ON FUNCTION public.always_true(test.items) TO postgrest_anonymous;


SET search_path = postgrest, pg_catalog;


REVOKE ALL ON TABLE auth FROM PUBLIC;
REVOKE ALL ON TABLE auth FROM postgrest_test;
GRANT ALL ON TABLE auth TO postgrest_test;
GRANT INSERT ON TABLE auth TO postgrest_anonymous;


SET search_path = private, pg_catalog;


REVOKE ALL ON TABLE articles FROM PUBLIC;
REVOKE ALL ON TABLE articles FROM postgrest_test;
GRANT ALL ON TABLE articles TO postgrest_test;

SET search_path = test, private, postgrest, public, pg_catalog;

------- SAMPLE DATA -----
INSERT INTO clients VALUES (1, 'Microsoft'),(2, 'Apple');
INSERT INTO projects VALUES (1,'Windows 7', 1),(2,'Windows 10', 1),(3,'IOS', 2),(4,'OSX', 2);
INSERT INTO tasks VALUES (1,'Design w7',1),(2,'Code w7',1),(3,'Design w10',2),(4,'Code w10',2),(5,'Design IOS',3),(6,'Code IOS',3),(7,'Design OSX',4),(8,'Code OSX',4);
INSERT INTO users VALUES (1, 'Angela Martin'),(2, 'Michael Scott'),(3, 'Dwight Schrute');
INSERT INTO users_projects VALUES(1,1),(1,2),(2,3),(2,4),(3,1),(3,3);
INSERT INTO users_tasks VALUES(1,1),(1,2),(1,3),(1,4),(2,5),(2,6),(2,7),(3,1),(3,5);
INSERT INTO comments VALUES (1, 1, 2, 6, 'Needs to be delivered ASAP');
INSERT INTO postgrest.auth (id, pass, rolname) VALUES ('jdoe', '1234', 'postgrest_test_author');
----------------
