--
-- PostgreSQL database dump
--

-- Dumped from database version 9.3.4
-- Dumped by pg_dump version 9.3.4
-- Started on 2014-09-29 17:06:36 PDT

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- TOC entry 6 (class 2615 OID 278520)
-- Name: 1; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA "1";


--
-- TOC entry 7 (class 2615 OID 278521)
-- Name: dbapi; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA dbapi;


--
-- TOC entry 180 (class 3079 OID 12018)
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- TOC entry 2260 (class 0 OID 0)
-- Dependencies: 180
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = dbapi, pg_catalog;

--
-- TOC entry 193 (class 1255 OID 278522)
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


--
-- TOC entry 194 (class 1255 OID 278523)
-- Name: create_role_if_not_exists(name); Type: FUNCTION; Schema: dbapi; Owner: -
--

CREATE FUNCTION create_role_if_not_exists(rolename name) RETURNS text
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NOT EXISTS (SELECT * FROM pg_roles WHERE rolname = rolename) THEN
        EXECUTE format('CREATE ROLE %I', rolename);
        RETURN 'CREATE ROLE';
    ELSE
        RETURN format('ROLE ''%I'' ALREADY EXISTS', rolename);
    END IF;
END;
$$;


SET search_path = "1", pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 171 (class 1259 OID 278524)
-- Name: auto_incrementing_pk; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE auto_incrementing_pk (
    id integer NOT NULL,
    nullable_string character varying,
    non_nullable_string character varying NOT NULL,
    inserted_at timestamp with time zone DEFAULT now()
);


--
-- TOC entry 172 (class 1259 OID 278531)
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE; Schema: 1; Owner: -
--

CREATE SEQUENCE auto_incrementing_pk_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 2261 (class 0 OID 0)
-- Dependencies: 172
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: -
--

ALTER SEQUENCE auto_incrementing_pk_id_seq OWNED BY auto_incrementing_pk.id;


--
-- TOC entry 173 (class 1259 OID 278533)
-- Name: compound_pk; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE compound_pk (
    k1 integer NOT NULL,
    k2 integer NOT NULL,
    extra integer
);


--
-- TOC entry 174 (class 1259 OID 278536)
-- Name: items; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE items (
    id bigint NOT NULL
);


--
-- TOC entry 175 (class 1259 OID 278539)
-- Name: items_id_seq; Type: SEQUENCE; Schema: 1; Owner: -
--

CREATE SEQUENCE items_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 2262 (class 0 OID 0)
-- Dependencies: 175
-- Name: items_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: -
--

ALTER SEQUENCE items_id_seq OWNED BY items.id;


--
-- TOC entry 176 (class 1259 OID 278541)
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
-- TOC entry 177 (class 1259 OID 278547)
-- Name: no_pk; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE no_pk (
    a character varying,
    b character varying
);


--
-- TOC entry 178 (class 1259 OID 278553)
-- Name: simple_pk; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE simple_pk (
    k character varying NOT NULL,
    extra character varying NOT NULL
);


SET search_path = dbapi, pg_catalog;

--
-- TOC entry 179 (class 1259 OID 278559)
-- Name: auth; Type: TABLE; Schema: dbapi; Owner: -; Tablespace: 
--

CREATE TABLE auth (
    id character varying NOT NULL,
    rolname name NOT NULL,
    pass character(60) NOT NULL
);


SET search_path = "1", pg_catalog;

--
-- TOC entry 2124 (class 2604 OID 278565)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: -
--

ALTER TABLE ONLY auto_incrementing_pk ALTER COLUMN id SET DEFAULT nextval('auto_incrementing_pk_id_seq'::regclass);


--
-- TOC entry 2125 (class 2604 OID 278566)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: -
--

ALTER TABLE ONLY items ALTER COLUMN id SET DEFAULT nextval('items_id_seq'::regclass);


--
-- TOC entry 2246 (class 0 OID 278524)
-- Dependencies: 171
-- Data for Name: auto_incrementing_pk; Type: TABLE DATA; Schema: 1; Owner: -
--



--
-- TOC entry 2263 (class 0 OID 0)
-- Dependencies: 172
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: -
--

SELECT pg_catalog.setval('auto_incrementing_pk_id_seq', 29, true);


--
-- TOC entry 2248 (class 0 OID 278533)
-- Dependencies: 173
-- Data for Name: compound_pk; Type: TABLE DATA; Schema: 1; Owner: -
--



--
-- TOC entry 2249 (class 0 OID 278536)
-- Dependencies: 174
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
-- TOC entry 2264 (class 0 OID 0)
-- Dependencies: 175
-- Name: items_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: -
--

SELECT pg_catalog.setval('items_id_seq', 15, true);


--
-- TOC entry 2251 (class 0 OID 278541)
-- Dependencies: 176
-- Data for Name: menagerie; Type: TABLE DATA; Schema: 1; Owner: -
--



--
-- TOC entry 2252 (class 0 OID 278547)
-- Dependencies: 177
-- Data for Name: no_pk; Type: TABLE DATA; Schema: 1; Owner: -
--



--
-- TOC entry 2253 (class 0 OID 278553)
-- Dependencies: 178
-- Data for Name: simple_pk; Type: TABLE DATA; Schema: 1; Owner: -
--



SET search_path = dbapi, pg_catalog;

--
-- TOC entry 2254 (class 0 OID 278559)
-- Dependencies: 179
-- Data for Name: auth; Type: TABLE DATA; Schema: dbapi; Owner: -
--



SET search_path = "1", pg_catalog;

--
-- TOC entry 2127 (class 2606 OID 278568)
-- Name: auto_incrementing_pk_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY auto_incrementing_pk
    ADD CONSTRAINT auto_incrementing_pk_pkey PRIMARY KEY (id);


--
-- TOC entry 2129 (class 2606 OID 278570)
-- Name: compound_pk_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY compound_pk
    ADD CONSTRAINT compound_pk_pkey PRIMARY KEY (k1, k2);


--
-- TOC entry 2135 (class 2606 OID 278572)
-- Name: contacts_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY simple_pk
    ADD CONSTRAINT contacts_pkey PRIMARY KEY (k);


--
-- TOC entry 2131 (class 2606 OID 278574)
-- Name: items_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY items
    ADD CONSTRAINT items_pkey PRIMARY KEY (id);


--
-- TOC entry 2133 (class 2606 OID 278576)
-- Name: menagerie_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY menagerie
    ADD CONSTRAINT menagerie_pkey PRIMARY KEY ("integer");


SET search_path = dbapi, pg_catalog;

--
-- TOC entry 2137 (class 2606 OID 278578)
-- Name: auth_pkey; Type: CONSTRAINT; Schema: dbapi; Owner: -; Tablespace: 
--

ALTER TABLE ONLY auth
    ADD CONSTRAINT auth_pkey PRIMARY KEY (id);


--
-- TOC entry 2138 (class 2620 OID 278580)
-- Name: ensure_auth_role_exists; Type: TRIGGER; Schema: dbapi; Owner: -
--

CREATE CONSTRAINT TRIGGER ensure_auth_role_exists AFTER INSERT OR UPDATE ON auth NOT DEFERRABLE INITIALLY IMMEDIATE FOR EACH ROW EXECUTE PROCEDURE check_role_exists();


-- Completed on 2014-09-29 17:06:36 PDT

--
-- PostgreSQL database dump complete
--

