--
-- PostgreSQL database dump
--

-- Dumped from database version 9.3.4
-- Dumped by pg_dump version 9.3.4
-- Started on 2014-09-25 16:21:25 PDT

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- TOC entry 6 (class 2615 OID 46226)
-- Name: 1; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA "1";


--
-- TOC entry 7 (class 2615 OID 46227)
-- Name: dbapi; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA dbapi;


--
-- TOC entry 181 (class 3079 OID 11756)
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- TOC entry 2000 (class 0 OID 0)
-- Dependencies: 181
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = dbapi, pg_catalog;

--
-- TOC entry 194 (class 1255 OID 46295)
-- Name: check_role_exists(); Type: FUNCTION; Schema: dbapi; Owner: -
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


SET search_path = "1", pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 172 (class 1259 OID 46228)
-- Name: auto_incrementing_pk; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE auto_incrementing_pk (
    id integer NOT NULL,
    nullable_string character varying,
    non_nullable_string character varying NOT NULL,
    inserted_at timestamp with time zone DEFAULT now()
);


--
-- TOC entry 173 (class 1259 OID 46235)
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE; Schema: 1; Owner: -
--

CREATE SEQUENCE auto_incrementing_pk_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 2001 (class 0 OID 0)
-- Dependencies: 173
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: -
--

ALTER SEQUENCE auto_incrementing_pk_id_seq OWNED BY auto_incrementing_pk.id;


--
-- TOC entry 174 (class 1259 OID 46237)
-- Name: compound_pk; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE compound_pk (
    k1 integer NOT NULL,
    k2 integer NOT NULL,
    extra integer
);


--
-- TOC entry 175 (class 1259 OID 46240)
-- Name: items; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE items (
    id bigint NOT NULL
);


--
-- TOC entry 176 (class 1259 OID 46243)
-- Name: items_id_seq; Type: SEQUENCE; Schema: 1; Owner: -
--

CREATE SEQUENCE items_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 2002 (class 0 OID 0)
-- Dependencies: 176
-- Name: items_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: -
--

ALTER SEQUENCE items_id_seq OWNED BY items.id;


--
-- TOC entry 177 (class 1259 OID 46245)
-- Name: menagerie; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE menagerie (
    "integer" integer NOT NULL,
    double double precision NOT NULL,
    "varchar" character varying NOT NULL,
    "boolean" boolean NOT NULL,
    date date NOT NULL,
    money money NOT NULL
);


--
-- TOC entry 178 (class 1259 OID 46251)
-- Name: no_pk; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE no_pk (
    a character varying,
    b character varying
);


--
-- TOC entry 179 (class 1259 OID 46257)
-- Name: simple_pk; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE simple_pk (
    k character varying NOT NULL,
    extra character varying NOT NULL
);


SET search_path = dbapi, pg_catalog;

--
-- TOC entry 180 (class 1259 OID 46284)
-- Name: auth; Type: TABLE; Schema: dbapi; Owner: -; Tablespace: 
--

CREATE TABLE auth (
    id character varying NOT NULL,
    rolname name NOT NULL
);


SET search_path = "1", pg_catalog;

--
-- TOC entry 1862 (class 2604 OID 46269)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: -
--

ALTER TABLE ONLY auto_incrementing_pk ALTER COLUMN id SET DEFAULT nextval('auto_incrementing_pk_id_seq'::regclass);


--
-- TOC entry 1863 (class 2604 OID 46270)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: -
--

ALTER TABLE ONLY items ALTER COLUMN id SET DEFAULT nextval('items_id_seq'::regclass);


--
-- TOC entry 1984 (class 0 OID 46228)
-- Dependencies: 172
-- Data for Name: auto_incrementing_pk; Type: TABLE DATA; Schema: 1; Owner: -
--



--
-- TOC entry 2003 (class 0 OID 0)
-- Dependencies: 173
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: -
--

SELECT pg_catalog.setval('auto_incrementing_pk_id_seq', 9, true);


--
-- TOC entry 1986 (class 0 OID 46237)
-- Dependencies: 174
-- Data for Name: compound_pk; Type: TABLE DATA; Schema: 1; Owner: -
--



--
-- TOC entry 1987 (class 0 OID 46240)
-- Dependencies: 175
-- Data for Name: items; Type: TABLE DATA; Schema: 1; Owner: -
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
-- TOC entry 2004 (class 0 OID 0)
-- Dependencies: 176
-- Name: items_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: -
--

SELECT pg_catalog.setval('items_id_seq', 15, true);


--
-- TOC entry 1989 (class 0 OID 46245)
-- Dependencies: 177
-- Data for Name: menagerie; Type: TABLE DATA; Schema: 1; Owner: -
--



--
-- TOC entry 1990 (class 0 OID 46251)
-- Dependencies: 178
-- Data for Name: no_pk; Type: TABLE DATA; Schema: 1; Owner: -
--



--
-- TOC entry 1991 (class 0 OID 46257)
-- Dependencies: 179
-- Data for Name: simple_pk; Type: TABLE DATA; Schema: 1; Owner: -
--



SET search_path = dbapi, pg_catalog;

--
-- TOC entry 1992 (class 0 OID 46284)
-- Dependencies: 180
-- Data for Name: auth; Type: TABLE DATA; Schema: dbapi; Owner: -
--

INSERT INTO auth (id, rolname) VALUES ('me@me.com', 'adam');


SET search_path = "1", pg_catalog;

--
-- TOC entry 1865 (class 2606 OID 46272)
-- Name: auto_incrementing_pk_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY auto_incrementing_pk
    ADD CONSTRAINT auto_incrementing_pk_pkey PRIMARY KEY (id);


--
-- TOC entry 1867 (class 2606 OID 46274)
-- Name: compound_pk_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY compound_pk
    ADD CONSTRAINT compound_pk_pkey PRIMARY KEY (k1, k2);


--
-- TOC entry 1873 (class 2606 OID 46276)
-- Name: contacts_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY simple_pk
    ADD CONSTRAINT contacts_pkey PRIMARY KEY (k);


--
-- TOC entry 1869 (class 2606 OID 46278)
-- Name: items_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY items
    ADD CONSTRAINT items_pkey PRIMARY KEY (id);


--
-- TOC entry 1871 (class 2606 OID 46280)
-- Name: menagerie_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY menagerie
    ADD CONSTRAINT menagerie_pkey PRIMARY KEY ("integer");


SET search_path = dbapi, pg_catalog;

--
-- TOC entry 1875 (class 2606 OID 46291)
-- Name: auth_pkey; Type: CONSTRAINT; Schema: dbapi; Owner: -; Tablespace: 
--

ALTER TABLE ONLY auth
    ADD CONSTRAINT auth_pkey PRIMARY KEY (id);


--
-- TOC entry 1876 (class 2620 OID 46297)
-- Name: ensure_auth_role_exists; Type: TRIGGER; Schema: dbapi; Owner: -
--

CREATE CONSTRAINT TRIGGER ensure_auth_role_exists AFTER INSERT OR UPDATE ON auth NOT DEFERRABLE INITIALLY IMMEDIATE FOR EACH ROW EXECUTE PROCEDURE check_role_exists();


--
-- TOC entry 1999 (class 0 OID 0)
-- Dependencies: 8
-- Name: public; Type: ACL; Schema: -; Owner: -
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


-- Completed on 2014-09-25 16:21:25 PDT

--
-- PostgreSQL database dump complete
--

