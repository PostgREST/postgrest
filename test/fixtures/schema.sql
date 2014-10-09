--
-- PostgreSQL database dump
--

-- Dumped from database version 9.3.4
-- Dumped by pg_dump version 9.3.4
-- Started on 2014-10-01 13:41:39 PDT
-- Started on 2014-10-07 16:46:34 PDT

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- TOC entry 11 (class 2615 OID 280932)
-- Name: 1; Type: SCHEMA; Schema: -; Owner: dbapi_test
--

CREATE SCHEMA "1";


ALTER SCHEMA "1" OWNER TO dbapi_test;

--
-- TOC entry 8 (class 2615 OID 50928)
-- Name: dbapi; Type: SCHEMA; Schema: -; Owner: dbapi_test
--

CREATE SCHEMA dbapi;


ALTER SCHEMA dbapi OWNER TO dbapi_test;

--
-- TOC entry 9 (class 2615 OID 50929)
-- Name: private; Type: SCHEMA; Schema: -; Owner: dbapi_test
--

CREATE SCHEMA private;


ALTER SCHEMA private OWNER TO dbapi_test;

--
-- TOC entry 187 (class 3079 OID 12018)
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';

SET search_path = "1", pg_catalog;

--
-- TOC entry 571 (class 1247 OID 309112)
-- Name: enum_menagerie_type; Type: TYPE; Schema: 1; Owner: postgres
--

CREATE TYPE "1".enum_menagerie_type AS ENUM (
  'foo',
  'bar'
);


--
-- Name: check_role_exists(); Type: FUNCTION; Schema: dbapi; Owner: dbapi_test
--

CREATE FUNCTION dbapi.check_role_exists() RETURNS trigger
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


ALTER FUNCTION dbapi.check_role_exists() OWNER TO dbapi_test;


CREATE FUNCTION dbapi.update_owner() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
   NEW.owner = current_user; 
   RETURN NEW;
END;
$$;


ALTER FUNCTION dbapi.update_owner() OWNER TO dbapi_test;

SET search_path = "1", pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--

CREATE TABLE authors_only (
    secret character varying NOT NULL
);


ALTER TABLE "1".authors_only OWNER TO dbapi_test_author;

--
-- TOC entry 175 (class 1259 OID 280937)
--

CREATE TABLE auto_incrementing_pk (
    id integer NOT NULL,
    nullable_string character varying,
    non_nullable_string character varying NOT NULL,
    inserted_at timestamp with time zone DEFAULT now()
);


ALTER TABLE "1".auto_incrementing_pk OWNER TO dbapi_test;

--
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE; Schema: 1; Owner: dbapi_test
--

CREATE SEQUENCE auto_incrementing_pk_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE "1".auto_incrementing_pk_id_seq OWNER TO dbapi_test;

--
--

ALTER SEQUENCE auto_incrementing_pk_id_seq OWNED BY auto_incrementing_pk.id;


--
-- Name: compound_pk; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE compound_pk (
    k1 integer NOT NULL,
    k2 integer NOT NULL,
    extra integer
);


ALTER TABLE "1".compound_pk OWNER TO dbapi_test;

--
-- Name: has_fk; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE has_fk (
    id bigint NOT NULL,
    auto_inc_fk integer,
    simple_fk character varying(255)
);


ALTER TABLE "1".has_fk OWNER TO dbapi_test;

--
-- TOC entry 185 (class 1259 OID 50947)
-- Name: has_fk_id_seq; Type: SEQUENCE; Schema: 1; Owner: dbapi_test
--

CREATE SEQUENCE has_fk_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE "1".has_fk_id_seq OWNER TO dbapi_test;

--
-- TOC entry 2042 (class 0 OID 0)
-- Dependencies: 185
-- Name: has_fk_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: dbapi_test
--

ALTER SEQUENCE has_fk_id_seq OWNED BY has_fk.id;


--
-- TOC entry 186 (class 1259 OID 50949)
-- Name: items; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE items (
    id bigint NOT NULL
);


ALTER TABLE "1".items OWNER TO dbapi_test;

--
-- Name: items_id_seq; Type: SEQUENCE; Schema: 1; Owner: dbapi_test
--

CREATE SEQUENCE items_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE "1".items_id_seq OWNER TO dbapi_test;

--
-- Name: items_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: dbapi_test
--

ALTER SEQUENCE items_id_seq OWNED BY items.id;


--
-- Name: menagerie; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE menagerie (
    "integer" integer NOT NULL,
    double double precision NOT NULL,
    "varchar" character varying NOT NULL,
    "boolean" boolean NOT NULL,
    date date NOT NULL,
    money money NOT NULL,
    enum "1".enum_menagerie_type not null
);


ALTER TABLE "1".menagerie OWNER TO dbapi_test;

--
-- Name: no_pk; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE no_pk (
    a character varying,
    b character varying
);


ALTER TABLE "1".no_pk OWNER TO dbapi_test;

--
-- Name: simple_pk; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE simple_pk (
    k character varying NOT NULL,
    extra character varying NOT NULL
);


ALTER TABLE "1".simple_pk OWNER TO dbapi_test;

SET search_path = dbapi, pg_catalog;

--
-- Name: auth; Type: TABLE; Schema: dbapi; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE auth (
    id character varying NOT NULL,
    rolname name NOT NULL,
    pass character(60) NOT NULL
);


ALTER TABLE dbapi.auth OWNER TO dbapi_test;

SET search_path = private, pg_catalog;

--
-- Name: articles; Type: TABLE; Schema: private; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE articles (
    body text,
    id integer NOT NULL,
    owner name NOT NULL
);


ALTER TABLE private.articles OWNER TO dbapi_test;

--
-- Name: articles_id_seq; Type: SEQUENCE; Schema: private; Owner: dbapi_test
--

CREATE SEQUENCE articles_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE private.articles_id_seq OWNER TO dbapi_test;

--
-- Name: articles_id_seq; Type: SEQUENCE OWNED BY; Schema: private; Owner: dbapi_test
--

ALTER SEQUENCE articles_id_seq OWNED BY articles.id;


SET search_path = "1", pg_catalog;

--
-- Name: id; Type: DEFAULT; Schema: 1; Owner: dbapi_test
--

ALTER TABLE ONLY auto_incrementing_pk ALTER COLUMN id SET DEFAULT nextval('auto_incrementing_pk_id_seq'::regclass);


--
-- TOC entry 1886 (class 2604 OID 50987)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: dbapi_test
--

ALTER TABLE ONLY has_fk ALTER COLUMN id SET DEFAULT nextval('has_fk_id_seq'::regclass);


--
-- TOC entry 1887 (class 2604 OID 50988)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: dbapi_test
--

ALTER TABLE ONLY items ALTER COLUMN id SET DEFAULT nextval('items_id_seq'::regclass);


SET search_path = private, pg_catalog;

--
-- Name: id; Type: DEFAULT; Schema: private; Owner: dbapi_test
--

ALTER TABLE ONLY articles ALTER COLUMN id SET DEFAULT nextval('articles_id_seq'::regclass);


SET search_path = "1", pg_catalog;

--
-- TOC entry 2279 (class 0 OID 281006)
-- Dependencies: 186
-- Data for Name: authors_only; Type: TABLE DATA; Schema: 1; Owner: dbapi_test_author
--



--
-- TOC entry 2268 (class 0 OID 280937)
-- Dependencies: 175
-- TOC entry 2016 (class 0 OID 50932)
-- Dependencies: 181
-- Data for Name: auto_incrementing_pk; Type: TABLE DATA; Schema: 1; Owner: dbapi_test
--


-- TOC entry 2270 (class 0 OID 280946)
-- Dependencies: 177
-- TOC entry 2051 (class 0 OID 0)
-- Dependencies: 182
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: dbapi_test
--

SELECT pg_catalog.setval('auto_incrementing_pk_id_seq', 50, true);


--
-- TOC entry 2018 (class 0 OID 50941)
-- Dependencies: 183
-- Data for Name: compound_pk; Type: TABLE DATA; Schema: 1; Owner: dbapi_test
--



--
-- TOC entry 2271 (class 0 OID 280949)
-- Dependencies: 178



--
-- TOC entry 2052 (class 0 OID 0)
-- Dependencies: 185
-- Name: has_fk_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: dbapi_test
--

SELECT pg_catalog.setval('has_fk_id_seq', 1, false);


--
-- TOC entry 2021 (class 0 OID 50949)
-- Dependencies: 186
-- Data for Name: items; Type: TABLE DATA; Schema: 1; Owner: dbapi_test
--

INSERT INTO items (id) VALUES (1);
INSERT INTO items (id) VALUES (2);
INSERT INTO items (id) VALUES (3);
INSERT INTO items (id) VALUES (4);
INSERT INTO items (id) VALUES (5);
INSERT INTO items (id) VALUES (6);
INSERT INTO items (id) VALUES (7);
INSERT INTO items (id) VALUES (8);
INSERT INTO items (id) VALUES (9);
INSERT INTO items (id) VALUES (10);
INSERT INTO items (id) VALUES (11);
INSERT INTO items (id) VALUES (12);
INSERT INTO items (id) VALUES (13);
INSERT INTO items (id) VALUES (14);
INSERT INTO items (id) VALUES (15);


--
-- Name: items_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: dbapi_test
--

SELECT pg_catalog.setval('items_id_seq', 15, true);


--
-- Data for Name: menagerie; Type: TABLE DATA; Schema: 1; Owner: dbapi_test
--



--
-- Data for Name: no_pk; Type: TABLE DATA; Schema: 1; Owner: dbapi_test
--



--



SET search_path = dbapi, pg_catalog;

--
-- Data for Name: auth; Type: TABLE DATA; Schema: dbapi; Owner: dbapi_test
--



SET search_path = private, pg_catalog;

--
--



--
-- Name: articles_id_seq; Type: SEQUENCE SET; Schema: private; Owner: dbapi_test
--

SELECT pg_catalog.setval('articles_id_seq', 1, false);


SET search_path = "1", pg_catalog;

--
-- Name: authors_only_pkey; Type: CONSTRAINT; Schema: 1; Owner: dbapi_test_author; Tablespace: 
--

ALTER TABLE ONLY authors_only
    ADD CONSTRAINT authors_only_pkey PRIMARY KEY (secret);


--
-- TOC entry 2144 (class 2606 OID 280990)
--

ALTER TABLE ONLY auto_incrementing_pk
    ADD CONSTRAINT auto_incrementing_pk_pkey PRIMARY KEY (id);


--
-- Name: compound_pk_pkey; Type: CONSTRAINT; Schema: 1; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY compound_pk
    ADD CONSTRAINT compound_pk_pkey PRIMARY KEY (k1, k2);


--
-- TOC entry 1900 (class 2606 OID 50995)
--

ALTER TABLE ONLY simple_pk
    ADD CONSTRAINT contacts_pkey PRIMARY KEY (k);


--
--

ALTER TABLE ONLY has_fk
    ADD CONSTRAINT has_fk_pkey PRIMARY KEY (id);


--
-- TOC entry 1896 (class 2606 OID 50999)
--

ALTER TABLE ONLY items
    ADD CONSTRAINT items_pkey PRIMARY KEY (id);


--
--

ALTER TABLE ONLY menagerie
    ADD CONSTRAINT menagerie_pkey PRIMARY KEY ("integer");


SET search_path = dbapi, pg_catalog;

--
-- Name: auth_pkey; Type: CONSTRAINT; Schema: dbapi; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY auth
    ADD CONSTRAINT auth_pkey PRIMARY KEY (id);


SET search_path = private, pg_catalog;

--
--

ALTER TABLE ONLY articles
    ADD CONSTRAINT articles_pkey PRIMARY KEY (id);


SET search_path = dbapi, pg_catalog;

--
-- Name: ensure_auth_role_exists; Type: TRIGGER; Schema: dbapi; Owner: dbapi_test
--

CREATE CONSTRAINT TRIGGER ensure_auth_role_exists AFTER INSERT OR UPDATE ON auth NOT DEFERRABLE INITIALLY IMMEDIATE FOR EACH ROW EXECUTE PROCEDURE check_role_exists();


SET search_path = private, pg_catalog;

--
-- Name: articles_owner_track; Type: TRIGGER; Schema: private; Owner: dbapi_test
--

CREATE TRIGGER articles_owner_track BEFORE INSERT OR UPDATE ON articles FOR EACH ROW EXECUTE PROCEDURE dbapi.update_owner();


SET search_path = "1", pg_catalog;

--
-- TOC entry 1905 (class 2606 OID 51009)
-- Name: has_fk_fk_fkey; Type: FK CONSTRAINT; Schema: 1; Owner: dbapi_test
--

ALTER TABLE ONLY has_fk
    ADD CONSTRAINT has_fk_fk_fkey FOREIGN KEY (auto_inc_fk) REFERENCES auto_incrementing_pk(id);


--
-- TOC entry 1906 (class 2606 OID 51015)
-- Name: has_fk_simple_fk_fkey; Type: FK CONSTRAINT; Schema: 1; Owner: dbapi_test
--

ALTER TABLE ONLY has_fk
    ADD CONSTRAINT has_fk_simple_fk_fkey FOREIGN KEY (simple_fk) REFERENCES simple_pk(k);



REVOKE ALL ON SCHEMA "1" FROM dbapi_test;
GRANT ALL ON SCHEMA "1" TO dbapi_test;
GRANT USAGE ON SCHEMA "1" TO dbapi_anonymous;


--
-- TOC entry 2036 (class 0 OID 0)
-- Dependencies: 15
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--

REVOKE ALL ON TABLE authors_only FROM dbapi_test_author;
GRANT ALL ON TABLE authors_only TO dbapi_test_author;


--
-- TOC entry 2290 (class 0 OID 0)
-- Dependencies: 175
-- Name: auto_incrementing_pk; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON TABLE auto_incrementing_pk FROM dbapi_test;
GRANT ALL ON TABLE auto_incrementing_pk TO dbapi_test;
GRANT ALL ON TABLE auto_incrementing_pk TO dbapi_anonymous;



REVOKE ALL ON SEQUENCE auto_incrementing_pk_id_seq FROM dbapi_test;
GRANT ALL ON SEQUENCE auto_incrementing_pk_id_seq TO dbapi_test;
GRANT USAGE ON SEQUENCE auto_incrementing_pk_id_seq TO dbapi_anonymous;



REVOKE ALL ON TABLE compound_pk FROM dbapi_test;
GRANT ALL ON TABLE compound_pk TO dbapi_test;
GRANT ALL ON TABLE compound_pk TO dbapi_anonymous;

--

REVOKE ALL ON TABLE items FROM dbapi_test;
GRANT ALL ON TABLE items TO dbapi_test;
GRANT ALL ON TABLE items TO dbapi_anonymous;



REVOKE ALL ON SEQUENCE items_id_seq FROM dbapi_test;
GRANT ALL ON SEQUENCE items_id_seq TO dbapi_test;
GRANT USAGE ON SEQUENCE items_id_seq TO dbapi_anonymous;

--

REVOKE ALL ON TABLE menagerie FROM dbapi_test;
GRANT ALL ON TABLE menagerie TO dbapi_test;
GRANT ALL ON TABLE menagerie TO dbapi_anonymous;

--

REVOKE ALL ON TABLE no_pk FROM dbapi_test;
GRANT ALL ON TABLE no_pk TO dbapi_test;
GRANT ALL ON TABLE no_pk TO dbapi_anonymous;



REVOKE ALL ON TABLE simple_pk FROM dbapi_test;
GRANT ALL ON TABLE simple_pk TO dbapi_test;
GRANT ALL ON TABLE simple_pk TO dbapi_anonymous;


SET search_path = private, pg_catalog;


REVOKE ALL ON TABLE articles FROM dbapi_test;
GRANT ALL ON TABLE articles TO dbapi_test;
--

