--
-- PostgreSQL database dump
--

-- Dumped from database version 9.3.4
-- Dumped by pg_dump version 9.3.1
-- Started on 2014-08-31 10:00:41 PDT

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- TOC entry 6 (class 2615 OID 231254)
-- Name: 1; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA "1";


--
-- TOC entry 178 (class 3079 OID 12018)
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- TOC entry 2248 (class 0 OID 0)
-- Dependencies: 178
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = "1", pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 170 (class 1259 OID 231255)
-- Name: auto_incrementing_pk; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE auto_incrementing_pk (
    id integer NOT NULL,
    nullable_string character varying,
    non_nullable_string character varying NOT NULL,
    inserted_at timestamp with time zone DEFAULT now()
);


--
-- TOC entry 171 (class 1259 OID 231262)
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE; Schema: 1; Owner: -
--

CREATE SEQUENCE auto_incrementing_pk_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 2249 (class 0 OID 0)
-- Dependencies: 171
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: -
--

ALTER SEQUENCE auto_incrementing_pk_id_seq OWNED BY auto_incrementing_pk.id;


--
-- TOC entry 172 (class 1259 OID 231264)
-- Name: compound_pk; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE compound_pk (
    k1 integer NOT NULL,
    k2 integer NOT NULL,
    extra integer
);


--
-- TOC entry 173 (class 1259 OID 231267)
-- Name: items; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE items (
    id bigint NOT NULL
);


--
-- TOC entry 174 (class 1259 OID 231270)
-- Name: items_id_seq; Type: SEQUENCE; Schema: 1; Owner: -
--

CREATE SEQUENCE items_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 2250 (class 0 OID 0)
-- Dependencies: 174
-- Name: items_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: -
--

ALTER SEQUENCE items_id_seq OWNED BY items.id;


--
-- TOC entry 175 (class 1259 OID 231272)
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
-- TOC entry 176 (class 1259 OID 231278)
-- Name: no_pk; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE no_pk (
    a character varying,
    b character varying
);


--
-- TOC entry 177 (class 1259 OID 231284)
-- Name: simple_pk; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE simple_pk (
    k character varying NOT NULL,
    extra character varying NOT NULL
);


--
-- TOC entry 2116 (class 2604 OID 231290)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: -
--

ALTER TABLE ONLY auto_incrementing_pk ALTER COLUMN id SET DEFAULT nextval('auto_incrementing_pk_id_seq'::regclass);


--
-- TOC entry 2117 (class 2604 OID 231291)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: -
--

ALTER TABLE ONLY items ALTER COLUMN id SET DEFAULT nextval('items_id_seq'::regclass);


--
-- TOC entry 2235 (class 0 OID 231255)
-- Dependencies: 170
-- Data for Name: auto_incrementing_pk; Type: TABLE DATA; Schema: 1; Owner: -
--



--
-- TOC entry 2251 (class 0 OID 0)
-- Dependencies: 171
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: -
--

SELECT pg_catalog.setval('auto_incrementing_pk_id_seq', 1, true);


--
-- TOC entry 2237 (class 0 OID 231264)
-- Dependencies: 172
-- Data for Name: compound_pk; Type: TABLE DATA; Schema: 1; Owner: -
--



--
-- TOC entry 2238 (class 0 OID 231267)
-- Dependencies: 173
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
-- TOC entry 2252 (class 0 OID 0)
-- Dependencies: 174
-- Name: items_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: -
--

SELECT pg_catalog.setval('items_id_seq', 15, true);


--
-- TOC entry 2240 (class 0 OID 231272)
-- Dependencies: 175
-- Data for Name: menagerie; Type: TABLE DATA; Schema: 1; Owner: -
--



--
-- TOC entry 2241 (class 0 OID 231278)
-- Dependencies: 176
-- Data for Name: no_pk; Type: TABLE DATA; Schema: 1; Owner: -
--



--
-- TOC entry 2242 (class 0 OID 231284)
-- Dependencies: 177
-- Data for Name: simple_pk; Type: TABLE DATA; Schema: 1; Owner: -
--



--
-- TOC entry 2119 (class 2606 OID 231293)
-- Name: auto_incrementing_pk_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY auto_incrementing_pk
    ADD CONSTRAINT auto_incrementing_pk_pkey PRIMARY KEY (id);


--
-- TOC entry 2121 (class 2606 OID 231295)
-- Name: compound_pk_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY compound_pk
    ADD CONSTRAINT compound_pk_pkey PRIMARY KEY (k1, k2);


--
-- TOC entry 2127 (class 2606 OID 231297)
-- Name: contacts_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY simple_pk
    ADD CONSTRAINT contacts_pkey PRIMARY KEY (k);


--
-- TOC entry 2123 (class 2606 OID 231299)
-- Name: items_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY items
    ADD CONSTRAINT items_pkey PRIMARY KEY (id);


--
-- TOC entry 2125 (class 2606 OID 231301)
-- Name: menagerie_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY menagerie
    ADD CONSTRAINT menagerie_pkey PRIMARY KEY ("integer");


-- Completed on 2014-08-31 10:00:41 PDT

--
-- PostgreSQL database dump complete
--

