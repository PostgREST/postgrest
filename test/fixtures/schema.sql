--
-- PostgreSQL database dump
--

-- Dumped from database version 9.3.4
-- Dumped by pg_dump version 9.3.4
-- Started on 2014-09-30 15:17:14 PDT

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- TOC entry 8 (class 2615 OID 280279)
-- Name: 1; Type: SCHEMA; Schema: -; Owner: dbapi_test
--

CREATE SCHEMA "1";


ALTER SCHEMA "1" OWNER TO dbapi_test;

--
-- TOC entry 7 (class 2615 OID 280280)
-- Name: dbapi; Type: SCHEMA; Schema: -; Owner: dbapi_test
--

CREATE SCHEMA dbapi;


ALTER SCHEMA dbapi OWNER TO dbapi_test;

--
-- TOC entry 5 (class 2615 OID 280341)
-- Name: private; Type: SCHEMA; Schema: -; Owner: dbapi_test
--

CREATE SCHEMA private;


ALTER SCHEMA private OWNER TO dbapi_test;

--
-- TOC entry 183 (class 3079 OID 12018)
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- TOC entry 2276 (class 0 OID 0)
-- Dependencies: 183
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = dbapi, pg_catalog;

--
-- TOC entry 196 (class 1255 OID 280281)
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

--
-- TOC entry 198 (class 1255 OID 280353)
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
-- TOC entry 172 (class 1259 OID 280283)
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
-- TOC entry 173 (class 1259 OID 280290)
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
-- TOC entry 2278 (class 0 OID 0)
-- Dependencies: 173
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: dbapi_test
--

ALTER SEQUENCE auto_incrementing_pk_id_seq OWNED BY auto_incrementing_pk.id;


--
-- TOC entry 174 (class 1259 OID 280292)
-- Name: compound_pk; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE compound_pk (
    k1 integer NOT NULL,
    k2 integer NOT NULL,
    extra integer
);


ALTER TABLE "1".compound_pk OWNER TO dbapi_test;

--
-- TOC entry 175 (class 1259 OID 280295)
-- Name: items; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE items (
    id bigint NOT NULL
);


ALTER TABLE "1".items OWNER TO dbapi_test;

--
-- TOC entry 176 (class 1259 OID 280298)
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
-- TOC entry 2282 (class 0 OID 0)
-- Dependencies: 176
-- Name: items_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: dbapi_test
--

ALTER SEQUENCE items_id_seq OWNED BY items.id;


--
-- TOC entry 177 (class 1259 OID 280300)
-- Name: menagerie; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE menagerie (
    "integer" integer NOT NULL,
    double double precision NOT NULL,
    "varchar" character varying NOT NULL,
    "boolean" boolean NOT NULL,
    date date NOT NULL,
    money money NOT NULL
);


ALTER TABLE "1".menagerie OWNER TO dbapi_test;

--
-- TOC entry 178 (class 1259 OID 280306)
-- Name: no_pk; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE no_pk (
    a character varying,
    b character varying
);


ALTER TABLE "1".no_pk OWNER TO dbapi_test;

--
-- TOC entry 179 (class 1259 OID 280312)
-- Name: simple_pk; Type: TABLE; Schema: 1; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE simple_pk (
    k character varying NOT NULL,
    extra character varying NOT NULL
);


ALTER TABLE "1".simple_pk OWNER TO dbapi_test;

SET search_path = dbapi, pg_catalog;

--
-- TOC entry 180 (class 1259 OID 280318)
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
-- TOC entry 182 (class 1259 OID 280356)
-- Name: articles; Type: TABLE; Schema: private; Owner: dbapi_test; Tablespace: 
--

CREATE TABLE articles (
    body text,
    id integer NOT NULL,
    owner name NOT NULL
);


ALTER TABLE private.articles OWNER TO dbapi_test;

--
-- TOC entry 181 (class 1259 OID 280354)
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
-- TOC entry 2288 (class 0 OID 0)
-- Dependencies: 181
-- Name: articles_id_seq; Type: SEQUENCE OWNED BY; Schema: private; Owner: dbapi_test
--

ALTER SEQUENCE articles_id_seq OWNED BY articles.id;


SET search_path = "1", pg_catalog;

--
-- TOC entry 2133 (class 2604 OID 280324)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: dbapi_test
--

ALTER TABLE ONLY auto_incrementing_pk ALTER COLUMN id SET DEFAULT nextval('auto_incrementing_pk_id_seq'::regclass);


--
-- TOC entry 2134 (class 2604 OID 280325)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: dbapi_test
--

ALTER TABLE ONLY items ALTER COLUMN id SET DEFAULT nextval('items_id_seq'::regclass);


SET search_path = private, pg_catalog;

--
-- TOC entry 2135 (class 2604 OID 280359)
-- Name: id; Type: DEFAULT; Schema: private; Owner: dbapi_test
--

ALTER TABLE ONLY articles ALTER COLUMN id SET DEFAULT nextval('articles_id_seq'::regclass);


SET search_path = "1", pg_catalog;

--
-- TOC entry 2259 (class 0 OID 280283)
-- Dependencies: 172
-- Data for Name: auto_incrementing_pk; Type: TABLE DATA; Schema: 1; Owner: dbapi_test
--



--
-- TOC entry 2289 (class 0 OID 0)
-- Dependencies: 173
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: dbapi_test
--

SELECT pg_catalog.setval('auto_incrementing_pk_id_seq', 42, true);


--
-- TOC entry 2261 (class 0 OID 280292)
-- Dependencies: 174
-- Data for Name: compound_pk; Type: TABLE DATA; Schema: 1; Owner: dbapi_test
--



--
-- TOC entry 2262 (class 0 OID 280295)
-- Dependencies: 175
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
-- TOC entry 2290 (class 0 OID 0)
-- Dependencies: 176
-- Name: items_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: dbapi_test
--

SELECT pg_catalog.setval('items_id_seq', 15, true);


--
-- TOC entry 2264 (class 0 OID 280300)
-- Dependencies: 177
-- Data for Name: menagerie; Type: TABLE DATA; Schema: 1; Owner: dbapi_test
--



--
-- TOC entry 2265 (class 0 OID 280306)
-- Dependencies: 178
-- Data for Name: no_pk; Type: TABLE DATA; Schema: 1; Owner: dbapi_test
--



--
-- TOC entry 2266 (class 0 OID 280312)
-- Dependencies: 179
-- Data for Name: simple_pk; Type: TABLE DATA; Schema: 1; Owner: dbapi_test
--



SET search_path = dbapi, pg_catalog;

--
-- TOC entry 2267 (class 0 OID 280318)
-- Dependencies: 180
-- Data for Name: auth; Type: TABLE DATA; Schema: dbapi; Owner: dbapi_test
--



SET search_path = private, pg_catalog;

--
-- TOC entry 2269 (class 0 OID 280356)
-- Dependencies: 182
-- Data for Name: articles; Type: TABLE DATA; Schema: private; Owner: dbapi_test
--



--
-- TOC entry 2291 (class 0 OID 0)
-- Dependencies: 181
-- Name: articles_id_seq; Type: SEQUENCE SET; Schema: private; Owner: dbapi_test
--

SELECT pg_catalog.setval('articles_id_seq', 1, false);


SET search_path = "1", pg_catalog;

--
-- TOC entry 2137 (class 2606 OID 280327)
-- Name: auto_incrementing_pk_pkey; Type: CONSTRAINT; Schema: 1; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY auto_incrementing_pk
    ADD CONSTRAINT auto_incrementing_pk_pkey PRIMARY KEY (id);


--
-- TOC entry 2139 (class 2606 OID 280329)
-- Name: compound_pk_pkey; Type: CONSTRAINT; Schema: 1; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY compound_pk
    ADD CONSTRAINT compound_pk_pkey PRIMARY KEY (k1, k2);


--
-- TOC entry 2145 (class 2606 OID 280331)
-- Name: contacts_pkey; Type: CONSTRAINT; Schema: 1; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY simple_pk
    ADD CONSTRAINT contacts_pkey PRIMARY KEY (k);


--
-- TOC entry 2141 (class 2606 OID 280333)
-- Name: items_pkey; Type: CONSTRAINT; Schema: 1; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY items
    ADD CONSTRAINT items_pkey PRIMARY KEY (id);


--
-- TOC entry 2143 (class 2606 OID 280335)
-- Name: menagerie_pkey; Type: CONSTRAINT; Schema: 1; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY menagerie
    ADD CONSTRAINT menagerie_pkey PRIMARY KEY ("integer");


SET search_path = dbapi, pg_catalog;

--
-- TOC entry 2147 (class 2606 OID 280337)
-- Name: auth_pkey; Type: CONSTRAINT; Schema: dbapi; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY auth
    ADD CONSTRAINT auth_pkey PRIMARY KEY (id);


SET search_path = private, pg_catalog;

--
-- TOC entry 2149 (class 2606 OID 280364)
-- Name: articles_pkey; Type: CONSTRAINT; Schema: private; Owner: dbapi_test; Tablespace: 
--

ALTER TABLE ONLY articles
    ADD CONSTRAINT articles_pkey PRIMARY KEY (id);


SET search_path = dbapi, pg_catalog;

--
-- TOC entry 2150 (class 2620 OID 280339)
-- Name: ensure_auth_role_exists; Type: TRIGGER; Schema: dbapi; Owner: dbapi_test
--

CREATE CONSTRAINT TRIGGER ensure_auth_role_exists AFTER INSERT OR UPDATE ON auth NOT DEFERRABLE INITIALLY IMMEDIATE FOR EACH ROW EXECUTE PROCEDURE check_role_exists();


SET search_path = private, pg_catalog;

--
-- TOC entry 2151 (class 2620 OID 280365)
-- Name: articles_owner_track; Type: TRIGGER; Schema: private; Owner: dbapi_test
--

CREATE TRIGGER articles_owner_track BEFORE INSERT OR UPDATE ON articles FOR EACH ROW EXECUTE PROCEDURE dbapi.update_owner();


--
-- TOC entry 2275 (class 0 OID 0)
-- Dependencies: 8
-- Name: 1; Type: ACL; Schema: -; Owner: dbapi_test
--

REVOKE ALL ON SCHEMA "1" FROM PUBLIC;
REVOKE ALL ON SCHEMA "1" FROM dbapi_test;
GRANT ALL ON SCHEMA "1" TO dbapi_test;
GRANT USAGE ON SCHEMA "1" TO dbapi_anonymous;


SET search_path = "1", pg_catalog;

--
-- TOC entry 2277 (class 0 OID 0)
-- Dependencies: 172
-- Name: auto_incrementing_pk; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON TABLE auto_incrementing_pk FROM PUBLIC;
REVOKE ALL ON TABLE auto_incrementing_pk FROM dbapi_test;
GRANT ALL ON TABLE auto_incrementing_pk TO dbapi_test;
GRANT ALL ON TABLE auto_incrementing_pk TO dbapi_anonymous;


--
-- TOC entry 2279 (class 0 OID 0)
-- Dependencies: 173
-- Name: auto_incrementing_pk_id_seq; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON SEQUENCE auto_incrementing_pk_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE auto_incrementing_pk_id_seq FROM dbapi_test;
GRANT ALL ON SEQUENCE auto_incrementing_pk_id_seq TO dbapi_test;
GRANT USAGE ON SEQUENCE auto_incrementing_pk_id_seq TO dbapi_anonymous;


--
-- TOC entry 2280 (class 0 OID 0)
-- Dependencies: 174
-- Name: compound_pk; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON TABLE compound_pk FROM PUBLIC;
REVOKE ALL ON TABLE compound_pk FROM dbapi_test;
GRANT ALL ON TABLE compound_pk TO dbapi_test;
GRANT ALL ON TABLE compound_pk TO dbapi_anonymous;


--
-- TOC entry 2281 (class 0 OID 0)
-- Dependencies: 175
-- Name: items; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON TABLE items FROM PUBLIC;
REVOKE ALL ON TABLE items FROM dbapi_test;
GRANT ALL ON TABLE items TO dbapi_test;
GRANT ALL ON TABLE items TO dbapi_anonymous;


--
-- TOC entry 2283 (class 0 OID 0)
-- Dependencies: 176
-- Name: items_id_seq; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON SEQUENCE items_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE items_id_seq FROM dbapi_test;
GRANT ALL ON SEQUENCE items_id_seq TO dbapi_test;
GRANT USAGE ON SEQUENCE items_id_seq TO dbapi_anonymous;


--
-- TOC entry 2284 (class 0 OID 0)
-- Dependencies: 177
-- Name: menagerie; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON TABLE menagerie FROM PUBLIC;
REVOKE ALL ON TABLE menagerie FROM dbapi_test;
GRANT ALL ON TABLE menagerie TO dbapi_test;
GRANT ALL ON TABLE menagerie TO dbapi_anonymous;


--
-- TOC entry 2285 (class 0 OID 0)
-- Dependencies: 178
-- Name: no_pk; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON TABLE no_pk FROM PUBLIC;
REVOKE ALL ON TABLE no_pk FROM dbapi_test;
GRANT ALL ON TABLE no_pk TO dbapi_test;
GRANT ALL ON TABLE no_pk TO dbapi_anonymous;


--
-- TOC entry 2286 (class 0 OID 0)
-- Dependencies: 179
-- Name: simple_pk; Type: ACL; Schema: 1; Owner: dbapi_test
--

REVOKE ALL ON TABLE simple_pk FROM PUBLIC;
REVOKE ALL ON TABLE simple_pk FROM dbapi_test;
GRANT ALL ON TABLE simple_pk TO dbapi_test;
GRANT ALL ON TABLE simple_pk TO dbapi_anonymous;


SET search_path = private, pg_catalog;

--
-- TOC entry 2287 (class 0 OID 0)
-- Dependencies: 182
-- Name: articles; Type: ACL; Schema: private; Owner: dbapi_test
--

REVOKE ALL ON TABLE articles FROM PUBLIC;
REVOKE ALL ON TABLE articles FROM dbapi_test;
GRANT ALL ON TABLE articles TO dbapi_test;


-- Completed on 2014-09-30 15:17:14 PDT

--
-- PostgreSQL database dump complete
--

