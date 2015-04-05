SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;


CREATE SCHEMA "1";


ALTER SCHEMA "1" OWNER TO postgrest_test;


CREATE SCHEMA postgrest;


ALTER SCHEMA postgrest OWNER TO postgrest_test;


CREATE SCHEMA private;


ALTER SCHEMA private OWNER TO postgrest_test;


CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;



COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = "1", pg_catalog;


CREATE TYPE enum_menagerie_type AS ENUM (
    'foo',
    'bar'
);


ALTER TYPE "1".enum_menagerie_type OWNER TO postgrest_test;

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

SET search_path = "1", pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;


CREATE TABLE authors_only (
    secret character varying NOT NULL
);


ALTER TABLE "1".authors_only OWNER TO postgrest_test_author;


CREATE TABLE auto_incrementing_pk (
    id integer NOT NULL,
    nullable_string character varying,
    non_nullable_string character varying NOT NULL,
    inserted_at timestamp with time zone DEFAULT now()
);


ALTER TABLE "1".auto_incrementing_pk OWNER TO postgrest_test;


CREATE SEQUENCE auto_incrementing_pk_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE "1".auto_incrementing_pk_id_seq OWNER TO postgrest_test;


ALTER SEQUENCE auto_incrementing_pk_id_seq OWNED BY auto_incrementing_pk.id;



CREATE TABLE compound_pk (
    k1 integer NOT NULL,
    k2 integer NOT NULL,
    extra integer
);


ALTER TABLE "1".compound_pk OWNER TO postgrest_test;


CREATE TABLE has_fk (
    id bigint NOT NULL,
    auto_inc_fk integer,
    simple_fk character varying(255)
);


ALTER TABLE "1".has_fk OWNER TO postgrest_test;


CREATE SEQUENCE has_fk_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE "1".has_fk_id_seq OWNER TO postgrest_test;


ALTER SEQUENCE has_fk_id_seq OWNED BY has_fk.id;



CREATE TABLE items (
    id bigint NOT NULL
);


ALTER TABLE "1".items OWNER TO postgrest_test;


CREATE SEQUENCE items_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE "1".items_id_seq OWNER TO postgrest_test;


ALTER SEQUENCE items_id_seq OWNED BY items.id;



CREATE TABLE menagerie (
    "integer" integer NOT NULL,
    double double precision NOT NULL,
    "varchar" character varying NOT NULL,
    "boolean" boolean NOT NULL,
    date date NOT NULL,
    money money NOT NULL,
    enum enum_menagerie_type NOT NULL
);


ALTER TABLE "1".menagerie OWNER TO postgrest_test;


CREATE TABLE no_pk (
    a character varying,
    b character varying
);


ALTER TABLE "1".no_pk OWNER TO postgrest_test;


CREATE TABLE simple_pk (
    k character varying NOT NULL,
    extra character varying NOT NULL
);


ALTER TABLE "1".simple_pk OWNER TO postgrest_test;


CREATE TABLE json
(
  data json
);


ALTER TABLE "1".json OWNER TO postgrest_test;


SET search_path = postgrest, pg_catalog;


CREATE TABLE auth (
    id character varying NOT NULL,
    rolname name NOT NULL,
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


SET search_path = "1", pg_catalog;


ALTER TABLE ONLY auto_incrementing_pk ALTER COLUMN id SET DEFAULT nextval('auto_incrementing_pk_id_seq'::regclass);



ALTER TABLE ONLY has_fk ALTER COLUMN id SET DEFAULT nextval('has_fk_id_seq'::regclass);



ALTER TABLE ONLY items ALTER COLUMN id SET DEFAULT nextval('items_id_seq'::regclass);


SET search_path = private, pg_catalog;


ALTER TABLE ONLY articles ALTER COLUMN id SET DEFAULT nextval('articles_id_seq'::regclass);


SET search_path = "1", pg_catalog;








SELECT pg_catalog.setval('auto_incrementing_pk_id_seq', 1, true);









SELECT pg_catalog.setval('has_fk_id_seq', 1, false);



INSERT INTO items (id) VALUES (1);

SELECT pg_catalog.setval('items_id_seq', 1, true);











SET search_path = postgrest, pg_catalog;




SET search_path = private, pg_catalog;





SELECT pg_catalog.setval('articles_id_seq', 1, false);


SET search_path = "1", pg_catalog;


ALTER TABLE ONLY authors_only
    ADD CONSTRAINT authors_only_pkey PRIMARY KEY (secret);



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


SET search_path = "1", pg_catalog;


ALTER TABLE ONLY has_fk
    ADD CONSTRAINT has_fk_fk_fkey FOREIGN KEY (auto_inc_fk) REFERENCES auto_incrementing_pk(id);



ALTER TABLE ONLY has_fk
    ADD CONSTRAINT has_fk_simple_fk_fkey FOREIGN KEY (simple_fk) REFERENCES simple_pk(k);



REVOKE ALL ON SCHEMA "1" FROM PUBLIC;
REVOKE ALL ON SCHEMA "1" FROM postgrest_test;
GRANT ALL ON SCHEMA "1" TO postgrest_test;
GRANT USAGE ON SCHEMA "1" TO postgrest_anonymous;
GRANT USAGE ON SCHEMA "1" TO postgrest_test_author;



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



REVOKE ALL ON TABLE simple_pk FROM PUBLIC;
REVOKE ALL ON TABLE simple_pk FROM postgrest_test;
GRANT ALL ON TABLE simple_pk TO postgrest_test;
GRANT ALL ON TABLE simple_pk TO postgrest_anonymous;



REVOKE ALL ON TABLE json FROM PUBLIC;
REVOKE ALL ON TABLE json FROM postgrest_test;
GRANT ALL ON TABLE json TO postgrest_test;
GRANT ALL ON TABLE json TO postgrest_anonymous;


SET search_path = postgrest, pg_catalog;


REVOKE ALL ON TABLE auth FROM PUBLIC;
REVOKE ALL ON TABLE auth FROM postgrest_test;
GRANT ALL ON TABLE auth TO postgrest_test;
GRANT INSERT ON TABLE auth TO postgrest_anonymous;


SET search_path = private, pg_catalog;


REVOKE ALL ON TABLE articles FROM PUBLIC;
REVOKE ALL ON TABLE articles FROM postgrest_test;
GRANT ALL ON TABLE articles TO postgrest_test;
