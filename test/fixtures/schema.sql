--
-- PostgreSQL database dump
--

-- Dumped from database version 9.3.5
-- Dumped by pg_dump version 9.3.5
-- Started on 2014-10-07 15:55:44 PDT

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- TOC entry 10 (class 2615 OID 309036)
-- Name: 1; Type: SCHEMA; Schema: -; Owner: dbapi_test
--

CREATE SCHEMA "1";


ALTER SCHEMA "1" OWNER TO dbapi_test;

--
-- TOC entry 15 (class 2615 OID 309037)
-- Name: dbapi; Type: SCHEMA; Schema: -; Owner: dbapi_test
--

CREATE SCHEMA dbapi;


ALTER SCHEMA dbapi OWNER TO dbapi_test;

--
-- TOC entry 9 (class 2615 OID 309038)
-- Name: private; Type: SCHEMA; Schema: -; Owner: dbapi_test
--

CREATE SCHEMA private;


ALTER SCHEMA private OWNER TO dbapi_test;

--
-- TOC entry 190 (class 3079 OID 12018)
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- TOC entry 2287 (class 0 OID 0)
-- Dependencies: 190
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


SET search_path = dbapi, pg_catalog;

--
-- TOC entry 203 (class 1255 OID 309039)
-- Name: check_role_exists(); Type: FUNCTION; Schema: dbapi; Owner: dbapi_test
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


ALTER FUNCTION dbapi.check_role_exists() OWNER TO dbapi_test;

--
-- TOC entry 204 (class 1255 OID 309040)
-- Name: update_owner(); Type: FUNCTION; Schema: dbapi; Owner: dbapi_test
--

CREATE FUNCTION update_owner() RETURNS trigger
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
-- TOC entry 179 (class 1259 OID 309041)
-- Name: auto_incrementing_pk; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE auto_incrementing_pk (
    id integer NOT NULL,
    nullable_string character varying,
    non_nullable_string character varying NOT NULL,
    inserted_at timestamp with time zone DEFAULT now()
);


ALTER TABLE "1".auto_incrementing_pk OWNER TO dbapi_test;

--
-- TOC entry 180 (class 1259 OID 309048)
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
-- TOC entry 2289 (class 0 OID 0)
-- Dependencies: 180
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: dbapi_test
--

ALTER SEQUENCE auto_incrementing_pk_id_seq OWNED BY auto_incrementing_pk.id;


--
-- TOC entry 181 (class 1259 OID 309050)
-- Name: compound_pk; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE compound_pk (
    k1 integer NOT NULL,
    k2 integer NOT NULL,
    extra integer
);


ALTER TABLE "1".compound_pk OWNER TO dbapi_test;

--
-- TOC entry 182 (class 1259 OID 309053)
-- Name: items; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE items (
    id bigint NOT NULL
);


ALTER TABLE "1".items OWNER TO dbapi_test;

--
-- TOC entry 183 (class 1259 OID 309056)
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
-- TOC entry 2293 (class 0 OID 0)
-- Dependencies: 183
-- Name: items_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: dbapi_test
--

ALTER SEQUENCE items_id_seq OWNED BY items.id;


--
-- TOC entry 184 (class 1259 OID 309058)
-- Name: menagerie; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE menagerie (
    "integer" integer NOT NULL,
    double double precision NOT NULL,
    "varchar" character varying NOT NULL,
    "boolean" boolean NOT NULL,
    date date NOT NULL,
    money money NOT NULL,
    enum "1".enum_menagerie_type NOT NULL
);


ALTER TABLE "1".menagerie OWNER TO dbapi_test;

--
-- TOC entry 185 (class 1259 OID 309064)
-- Name: no_pk; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE no_pk (
    a character varying,
    b character varying
);


ALTER TABLE "1".no_pk OWNER TO dbapi_test;

--
-- TOC entry 186 (class 1259 OID 309070)
-- Name: simple_pk; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE simple_pk (
    k character varying NOT NULL,
    extra character varying NOT NULL
);


ALTER TABLE "1".simple_pk OWNER TO dbapi_test;

SET search_path = dbapi, pg_catalog;

--
-- TOC entry 187 (class 1259 OID 309076)
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
-- TOC entry 188 (class 1259 OID 309082)
-- Name: articles; Type: TABLE; Schema: private; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE articles (
    body text,
    id integer NOT NULL,
    owner name NOT NULL
);


ALTER TABLE private.articles OWNER TO dbapi_test;

--
-- TOC entry 189 (class 1259 OID 309088)
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
-- TOC entry 2299 (class 0 OID 0)
-- Dependencies: 189
-- Name: articles_id_seq; Type: SEQUENCE OWNED BY; Schema: private; Owner: dbapi_test
--

ALTER SEQUENCE articles_id_seq OWNED BY articles.id;


SET search_path = "1", pg_catalog;

--
-- TOC entry 2142 (class 2604 OID 309090)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: dbapi_test
--

ALTER TABLE ONLY auto_incrementing_pk ALTER COLUMN id SET DEFAULT nextval('auto_incrementing_pk_id_seq'::regclass);


--
-- TOC entry 2143 (class 2604 OID 309091)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: dbapi_test
--

ALTER TABLE ONLY items ALTER COLUMN id SET DEFAULT nextval('items_id_seq'::regclass);


SET search_path = private, pg_catalog;

--
-- TOC entry 2144 (class 2604 OID 309092)
-- Name: id; Type: DEFAULT; Schema: private; Owner: dbapi_test
--

ALTER TABLE ONLY articles ALTER COLUMN id SET DEFAULT nextval('articles_id_seq'::regclass);


SET search_path = "1", pg_catalog;

--
-- TOC entry 2268 (class 0 OID 309041)
-- Dependencies: 179
-- Data for Name: auto_incrementing_pk; Type: TABLE DATA; Schema: 1; Owner: dbapi_test
--



--
-- TOC entry 2300 (class 0 OID 0)
-- Dependencies: 180
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: dbapi_test
--

SELECT pg_catalog.setval('auto_incrementing_pk_id_seq', 46, true);


--
-- TOC entry 2270 (class 0 OID 309050)
-- Dependencies: 181
-- Data for Name: compound_pk; Type: TABLE DATA; Schema: 1; Owner: dbapi_test
--



--
-- TOC entry 2271 (class 0 OID 309053)
-- Dependencies: 182
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
-- TOC entry 2301 (class 0 OID 0)
-- Dependencies: 183
-- Name: items_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: dbapi_test
--

SELECT pg_catalog.setval('items_id_seq', 15, true);


--
-- TOC entry 2273 (class 0 OID 309058)
-- Dependencies: 184
-- Data for Name: menagerie; Type: TABLE DATA; Schema: 1; Owner: dbapi_test
--



--
-- TOC entry 2274 (class 0 OID 309064)
-- Dependencies: 185
-- Data for Name: no_pk; Type: TABLE DATA; Schema: 1; Owner: dbapi_test
--



--
-- TOC entry 2275 (class 0 OID 309070)
-- Dependencies: 186
-- Data for Name: simple_pk; Type: TABLE DATA; Schema: 1; Owner: dbapi_test
--



SET search_path = dbapi, pg_catalog;

--
-- TOC entry 2276 (class 0 OID 309076)
-- Dependencies: 187
-- Data for Name: auth; Type: TABLE DATA; Schema: dbapi; Owner: dbapi_test
--



SET search_path = private, pg_catalog;

--
-- TOC entry 2277 (class 0 OID 309082)
-- Dependencies: 188
-- Data for Name: articles; Type: TABLE DATA; Schema: private; Owner: dbapi_test
--



--
-- TOC entry 2302 (class 0 OID 0)
-- Dependencies: 189
-- Name: articles_id_seq; Type: SEQUENCE SET; Schema: private; Owner: dbapi_test
--

SELECT pg_catalog.setval('articles_id_seq', 1, false);


SET search_path = "1", pg_catalog;

--
-- TOC entry 2146 (class 2606 OID 309094)
-- Name: auto_incrementing_pk_pkey; Type: CONSTRAINT; Schema: 1; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY auto_incrementing_pk
    ADD CONSTRAINT auto_incrementing_pk_pkey PRIMARY KEY (id);


--
-- TOC entry 2148 (class 2606 OID 309096)
-- Name: compound_pk_pkey; Type: CONSTRAINT; Schema: 1; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY compound_pk
    ADD CONSTRAINT compound_pk_pkey PRIMARY KEY (k1, k2);


--
-- TOC entry 2154 (class 2606 OID 309098)
-- Name: contacts_pkey; Type: CONSTRAINT; Schema: 1; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY simple_pk
    ADD CONSTRAINT contacts_pkey PRIMARY KEY (k);


--
-- TOC entry 2150 (class 2606 OID 309100)
-- Name: items_pkey; Type: CONSTRAINT; Schema: 1; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY items
    ADD CONSTRAINT items_pkey PRIMARY KEY (id);


--
-- TOC entry 2152 (class 2606 OID 309102)
-- Name: menagerie_pkey; Type: CONSTRAINT; Schema: 1; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY menagerie
    ADD CONSTRAINT menagerie_pkey PRIMARY KEY ("integer");


SET search_path = dbapi, pg_catalog;

--
-- TOC entry 2156 (class 2606 OID 309104)
-- Name: auth_pkey; Type: CONSTRAINT; Schema: dbapi; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY auth
    ADD CONSTRAINT auth_pkey PRIMARY KEY (id);


SET search_path = private, pg_catalog;

--
-- TOC entry 2158 (class 2606 OID 309106)
-- Name: articles_pkey; Type: CONSTRAINT; Schema: private; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY articles
    ADD CONSTRAINT articles_pkey PRIMARY KEY (id);


SET search_path = dbapi, pg_catalog;

--
-- TOC entry 2159 (class 2620 OID 309108)
-- Name: ensure_auth_role_exists; Type: TRIGGER; Schema: dbapi; Owner: dbapi_test
--

CREATE CONSTRAINT TRIGGER ensure_auth_role_exists AFTER INSERT OR UPDATE ON auth NOT DEFERRABLE INITIALLY IMMEDIATE FOR EACH ROW EXECUTE PROCEDURE check_role_exists();


SET search_path = private, pg_catalog;

--
-- TOC entry 2160 (class 2620 OID 309109)
-- Name: articles_owner_track; Type: TRIGGER; Schema: private; Owner: dbapi_test
--

CREATE TRIGGER articles_owner_track BEFORE INSERT OR UPDATE ON articles FOR EACH ROW EXECUTE PROCEDURE dbapi.update_owner();


--
-- TOC entry 2284 (class 0 OID 0)
-- Dependencies: 10
-- Name: 1; Type: ACL; Schema: -; Owner: dbapi_test
--

REVOKE ALL ON SCHEMA "1" FROM dbapi_test;
GRANT ALL ON SCHEMA "1" TO dbapi_test;
GRANT USAGE ON SCHEMA "1" TO dbapi_anonymous;


SET search_path = "1", pg_catalog;

--
-- TOC entry 2288 (class 0 OID 0)
-- Dependencies: 179
-- Name: auto_incrementing_pk; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON TABLE auto_incrementing_pk FROM PUBLIC;
REVOKE ALL ON TABLE auto_incrementing_pk FROM dbapi_test;
GRANT ALL ON TABLE auto_incrementing_pk TO dbapi_test;
GRANT ALL ON TABLE auto_incrementing_pk TO dbapi_anonymous;


--
-- TOC entry 2290 (class 0 OID 0)
-- Dependencies: 180
-- Name: auto_incrementing_pk_id_seq; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON SEQUENCE auto_incrementing_pk_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE auto_incrementing_pk_id_seq FROM dbapi_test;
GRANT ALL ON SEQUENCE auto_incrementing_pk_id_seq TO dbapi_test;
GRANT USAGE ON SEQUENCE auto_incrementing_pk_id_seq TO dbapi_anonymous;


--
-- TOC entry 2291 (class 0 OID 0)
-- Dependencies: 181
-- Name: compound_pk; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON TABLE compound_pk FROM PUBLIC;
REVOKE ALL ON TABLE compound_pk FROM dbapi_test;
GRANT ALL ON TABLE compound_pk TO dbapi_test;
GRANT ALL ON TABLE compound_pk TO dbapi_anonymous;


--
-- TOC entry 2292 (class 0 OID 0)
-- Dependencies: 182
-- Name: items; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON TABLE items FROM PUBLIC;
REVOKE ALL ON TABLE items FROM dbapi_test;
GRANT ALL ON TABLE items TO dbapi_test;
GRANT ALL ON TABLE items TO dbapi_anonymous;


--
-- TOC entry 2294 (class 0 OID 0)
-- Dependencies: 183
-- Name: items_id_seq; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON SEQUENCE items_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE items_id_seq FROM dbapi_test;
GRANT ALL ON SEQUENCE items_id_seq TO dbapi_test;
GRANT USAGE ON SEQUENCE items_id_seq TO dbapi_anonymous;


--
-- TOC entry 2295 (class 0 OID 0)
-- Dependencies: 184
-- Name: menagerie; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON TABLE menagerie FROM PUBLIC;
REVOKE ALL ON TABLE menagerie FROM dbapi_test;
GRANT ALL ON TABLE menagerie TO dbapi_test;
GRANT ALL ON TABLE menagerie TO dbapi_anonymous;


--
-- TOC entry 2296 (class 0 OID 0)
-- Dependencies: 185
-- Name: no_pk; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON TABLE no_pk FROM PUBLIC;
REVOKE ALL ON TABLE no_pk FROM dbapi_test;
GRANT ALL ON TABLE no_pk TO dbapi_test;
GRANT ALL ON TABLE no_pk TO dbapi_anonymous;


--
-- TOC entry 2297 (class 0 OID 0)
-- Dependencies: 186
-- Name: simple_pk; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON TABLE simple_pk FROM PUBLIC;
REVOKE ALL ON TABLE simple_pk FROM dbapi_test;
GRANT ALL ON TABLE simple_pk TO dbapi_test;
GRANT ALL ON TABLE simple_pk TO dbapi_anonymous;


SET search_path = private, pg_catalog;

--
-- TOC entry 2298 (class 0 OID 0)
-- Dependencies: 188
-- Name: articles; Type: ACL; Schema: private; Owner: dbapi_test
--

REVOKE ALL ON TABLE articles FROM PUBLIC;
REVOKE ALL ON TABLE articles FROM dbapi_test;
GRANT ALL ON TABLE articles TO dbapi_test;


-- Completed on 2014-10-07 15:55:44 PDT

--
-- PostgreSQL database dump complete
--

