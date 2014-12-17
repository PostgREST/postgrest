--
-- PostgreSQL database dump
--

-- Dumped from database version 9.3.5
-- Dumped by pg_dump version 9.3.5
-- Started on 2014-10-21 15:12:44 PDT

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- TOC entry 20 (class 2615 OID 337536)
-- Name: 1; Type: SCHEMA; Schema: -; Owner: postgrest_test
--

CREATE SCHEMA "1";


ALTER SCHEMA "1" OWNER TO postgrest_test;

--
-- TOC entry 19 (class 2615 OID 337537)
-- Name: postgrest; Type: SCHEMA; Schema: -; Owner: postgrest_test
--

CREATE SCHEMA postgrest;


ALTER SCHEMA postgrest OWNER TO postgrest_test;

--
-- TOC entry 16 (class 2615 OID 337538)
-- Name: private; Type: SCHEMA; Schema: -; Owner: postgrest_test
--

CREATE SCHEMA private;


ALTER SCHEMA private OWNER TO postgrest_test;

--
-- TOC entry 205 (class 3079 OID 12018)
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- TOC entry 2321 (class 0 OID 0)
-- Dependencies: 205
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = "1", pg_catalog;

--
-- TOC entry 553 (class 1247 OID 337540)
-- Name: enum_menagerie_type; Type: TYPE; Schema: 1; Owner: postgrest_test
--

CREATE TYPE enum_menagerie_type AS ENUM (
    'foo',
    'bar'
);


ALTER TYPE "1".enum_menagerie_type OWNER TO postgrest_test;

SET search_path = postgrest, pg_catalog;

--
-- TOC entry 218 (class 1255 OID 337545)
-- Name: check_role_exists(); Type: FUNCTION; Schema: postgrest; Owner: postgrest_test
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


ALTER FUNCTION postgrest.check_role_exists() OWNER TO postgrest_test;

--
-- TOC entry 219 (class 1255 OID 337546)
-- Name: update_owner(); Type: FUNCTION; Schema: postgrest; Owner: postgrest_test
--

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

--
-- TOC entry 191 (class 1259 OID 337547)
-- Name: authors_only; Type: TABLE; Schema: 1; Owner: postgrest_test_author; Tablespace: 
--

CREATE TABLE authors_only (
    secret character varying NOT NULL
);


ALTER TABLE "1".authors_only OWNER TO postgrest_test_author;

--
-- TOC entry 192 (class 1259 OID 337553)
-- Name: auto_incrementing_pk; Type: TABLE; Schema: 1; Owner: postgrest_test; Tablespace: 
--

CREATE TABLE auto_incrementing_pk (
    id integer NOT NULL,
    nullable_string character varying,
    non_nullable_string character varying NOT NULL,
    inserted_at timestamp with time zone DEFAULT now()
);


ALTER TABLE "1".auto_incrementing_pk OWNER TO postgrest_test;

--
-- TOC entry 193 (class 1259 OID 337560)
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE; Schema: 1; Owner: postgrest_test
--

CREATE SEQUENCE auto_incrementing_pk_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE "1".auto_incrementing_pk_id_seq OWNER TO postgrest_test;

--
-- TOC entry 2324 (class 0 OID 0)
-- Dependencies: 193
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: postgrest_test
--

ALTER SEQUENCE auto_incrementing_pk_id_seq OWNED BY auto_incrementing_pk.id;


--
-- TOC entry 194 (class 1259 OID 337562)
-- Name: compound_pk; Type: TABLE; Schema: 1; Owner: postgrest_test; Tablespace: 
--

CREATE TABLE compound_pk (
    k1 integer NOT NULL,
    k2 integer NOT NULL,
    extra integer
);


ALTER TABLE "1".compound_pk OWNER TO postgrest_test;

--
-- TOC entry 195 (class 1259 OID 337565)
-- Name: has_fk; Type: TABLE; Schema: 1; Owner: postgrest_test; Tablespace: 
--

CREATE TABLE has_fk (
    id bigint NOT NULL,
    auto_inc_fk integer,
    simple_fk character varying(255)
);


ALTER TABLE "1".has_fk OWNER TO postgrest_test;

--
-- TOC entry 196 (class 1259 OID 337568)
-- Name: has_fk_id_seq; Type: SEQUENCE; Schema: 1; Owner: postgrest_test
--

CREATE SEQUENCE has_fk_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE "1".has_fk_id_seq OWNER TO postgrest_test;

--
-- TOC entry 2327 (class 0 OID 0)
-- Dependencies: 196
-- Name: has_fk_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: postgrest_test
--

ALTER SEQUENCE has_fk_id_seq OWNED BY has_fk.id;


--
-- TOC entry 197 (class 1259 OID 337570)
-- Name: items; Type: TABLE; Schema: 1; Owner: postgrest_test; Tablespace: 
--

CREATE TABLE items (
    id bigint NOT NULL
);


ALTER TABLE "1".items OWNER TO postgrest_test;

--
-- TOC entry 198 (class 1259 OID 337573)
-- Name: items_id_seq; Type: SEQUENCE; Schema: 1; Owner: postgrest_test
--

CREATE SEQUENCE items_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE "1".items_id_seq OWNER TO postgrest_test;

--
-- TOC entry 2329 (class 0 OID 0)
-- Dependencies: 198
-- Name: items_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: postgrest_test
--

ALTER SEQUENCE items_id_seq OWNED BY items.id;


--
-- TOC entry 199 (class 1259 OID 337575)
-- Name: menagerie; Type: TABLE; Schema: 1; Owner: postgrest_test; Tablespace: 
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


ALTER TABLE "1".menagerie OWNER TO postgrest_test;

--
-- TOC entry 200 (class 1259 OID 337581)
-- Name: no_pk; Type: TABLE; Schema: 1; Owner: postgrest_test; Tablespace: 
--

CREATE TABLE no_pk (
    a character varying,
    b character varying
);


ALTER TABLE "1".no_pk OWNER TO postgrest_test;

--
-- TOC entry 201 (class 1259 OID 337587)
-- Name: simple_pk; Type: TABLE; Schema: 1; Owner: postgrest_test; Tablespace: 
--

CREATE TABLE simple_pk (
    k character varying NOT NULL,
    extra character varying NOT NULL
);


ALTER TABLE "1".simple_pk OWNER TO postgrest_test;

SET search_path = postgrest, pg_catalog;

--
-- TOC entry 202 (class 1259 OID 337593)
-- Name: auth; Type: TABLE; Schema: postgrest; Owner: postgrest_test; Tablespace: 
--

CREATE TABLE auth (
    id character varying NOT NULL,
    rolname name NOT NULL,
    pass character(60) NOT NULL
);


ALTER TABLE postgrest.auth OWNER TO postgrest_test;

SET search_path = private, pg_catalog;

--
-- TOC entry 203 (class 1259 OID 337599)
-- Name: articles; Type: TABLE; Schema: private; Owner: postgrest_test; Tablespace: 
--

CREATE TABLE articles (
    body text,
    id integer NOT NULL,
    owner name NOT NULL
);


ALTER TABLE private.articles OWNER TO postgrest_test;

--
-- TOC entry 204 (class 1259 OID 337605)
-- Name: articles_id_seq; Type: SEQUENCE; Schema: private; Owner: postgrest_test
--

CREATE SEQUENCE articles_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE private.articles_id_seq OWNER TO postgrest_test;

--
-- TOC entry 2336 (class 0 OID 0)
-- Dependencies: 204
-- Name: articles_id_seq; Type: SEQUENCE OWNED BY; Schema: private; Owner: postgrest_test
--

ALTER SEQUENCE articles_id_seq OWNED BY articles.id;


SET search_path = "1", pg_catalog;

--
-- TOC entry 2165 (class 2604 OID 337607)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: postgrest_test
--

ALTER TABLE ONLY auto_incrementing_pk ALTER COLUMN id SET DEFAULT nextval('auto_incrementing_pk_id_seq'::regclass);


--
-- TOC entry 2166 (class 2604 OID 337608)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: postgrest_test
--

ALTER TABLE ONLY has_fk ALTER COLUMN id SET DEFAULT nextval('has_fk_id_seq'::regclass);


--
-- TOC entry 2167 (class 2604 OID 337609)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: postgrest_test
--

ALTER TABLE ONLY items ALTER COLUMN id SET DEFAULT nextval('items_id_seq'::regclass);


SET search_path = private, pg_catalog;

--
-- TOC entry 2168 (class 2604 OID 337610)
-- Name: id; Type: DEFAULT; Schema: private; Owner: postgrest_test
--

ALTER TABLE ONLY articles ALTER COLUMN id SET DEFAULT nextval('articles_id_seq'::regclass);


SET search_path = "1", pg_catalog;

--
-- TOC entry 2298 (class 0 OID 337547)
-- Dependencies: 191
-- Data for Name: authors_only; Type: TABLE DATA; Schema: 1; Owner: postgrest_test_author
--



--
-- TOC entry 2299 (class 0 OID 337553)
-- Dependencies: 192
-- Data for Name: auto_incrementing_pk; Type: TABLE DATA; Schema: 1; Owner: postgrest_test
--



--
-- TOC entry 2337 (class 0 OID 0)
-- Dependencies: 193
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: postgrest_test
--

SELECT pg_catalog.setval('auto_incrementing_pk_id_seq', 54, true);


--
-- TOC entry 2301 (class 0 OID 337562)
-- Dependencies: 194
-- Data for Name: compound_pk; Type: TABLE DATA; Schema: 1; Owner: postgrest_test
--



--
-- TOC entry 2302 (class 0 OID 337565)
-- Dependencies: 195
-- Data for Name: has_fk; Type: TABLE DATA; Schema: 1; Owner: postgrest_test
--



--
-- TOC entry 2338 (class 0 OID 0)
-- Dependencies: 196
-- Name: has_fk_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: postgrest_test
--

SELECT pg_catalog.setval('has_fk_id_seq', 1, false);


--
-- TOC entry 2304 (class 0 OID 337570)
-- Dependencies: 197
-- Data for Name: items; Type: TABLE DATA; Schema: 1; Owner: postgrest_test
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
-- TOC entry 2339 (class 0 OID 0)
-- Dependencies: 198
-- Name: items_id_seq; Type: SEQUENCE SET; Schema: 1; Owner: postgrest_test
--

SELECT pg_catalog.setval('items_id_seq', 19, true);


--
-- TOC entry 2306 (class 0 OID 337575)
-- Dependencies: 199
-- Data for Name: menagerie; Type: TABLE DATA; Schema: 1; Owner: postgrest_test
--



--
-- TOC entry 2307 (class 0 OID 337581)
-- Dependencies: 200
-- Data for Name: no_pk; Type: TABLE DATA; Schema: 1; Owner: postgrest_test
--



--
-- TOC entry 2308 (class 0 OID 337587)
-- Dependencies: 201
-- Data for Name: simple_pk; Type: TABLE DATA; Schema: 1; Owner: postgrest_test
--



SET search_path = postgrest, pg_catalog;

--
-- TOC entry 2309 (class 0 OID 337593)
-- Dependencies: 202
-- Data for Name: auth; Type: TABLE DATA; Schema: postgrest; Owner: postgrest_test
--



SET search_path = private, pg_catalog;

--
-- TOC entry 2310 (class 0 OID 337599)
-- Dependencies: 203
-- Data for Name: articles; Type: TABLE DATA; Schema: private; Owner: postgrest_test
--



--
-- TOC entry 2340 (class 0 OID 0)
-- Dependencies: 204
-- Name: articles_id_seq; Type: SEQUENCE SET; Schema: private; Owner: postgrest_test
--

SELECT pg_catalog.setval('articles_id_seq', 1, false);


SET search_path = "1", pg_catalog;

--
-- TOC entry 2170 (class 2606 OID 337612)
-- Name: authors_only_pkey; Type: CONSTRAINT; Schema: 1; Owner: postgrest_test_author; Tablespace: 
--

ALTER TABLE ONLY authors_only
    ADD CONSTRAINT authors_only_pkey PRIMARY KEY (secret);


--
-- TOC entry 2172 (class 2606 OID 337614)
-- Name: auto_incrementing_pk_pkey; Type: CONSTRAINT; Schema: 1; Owner: postgrest_test; Tablespace: 
--

ALTER TABLE ONLY auto_incrementing_pk
    ADD CONSTRAINT auto_incrementing_pk_pkey PRIMARY KEY (id);


--
-- TOC entry 2174 (class 2606 OID 337616)
-- Name: compound_pk_pkey; Type: CONSTRAINT; Schema: 1; Owner: postgrest_test; Tablespace: 
--

ALTER TABLE ONLY compound_pk
    ADD CONSTRAINT compound_pk_pkey PRIMARY KEY (k1, k2);


--
-- TOC entry 2182 (class 2606 OID 337618)
-- Name: contacts_pkey; Type: CONSTRAINT; Schema: 1; Owner: postgrest_test; Tablespace: 
--

ALTER TABLE ONLY simple_pk
    ADD CONSTRAINT contacts_pkey PRIMARY KEY (k);


--
-- TOC entry 2176 (class 2606 OID 337620)
-- Name: has_fk_pkey; Type: CONSTRAINT; Schema: 1; Owner: postgrest_test; Tablespace: 
--

ALTER TABLE ONLY has_fk
    ADD CONSTRAINT has_fk_pkey PRIMARY KEY (id);


--
-- TOC entry 2178 (class 2606 OID 337622)
-- Name: items_pkey; Type: CONSTRAINT; Schema: 1; Owner: postgrest_test; Tablespace: 
--

ALTER TABLE ONLY items
    ADD CONSTRAINT items_pkey PRIMARY KEY (id);


--
-- TOC entry 2180 (class 2606 OID 337624)
-- Name: menagerie_pkey; Type: CONSTRAINT; Schema: 1; Owner: postgrest_test; Tablespace: 
--

ALTER TABLE ONLY menagerie
    ADD CONSTRAINT menagerie_pkey PRIMARY KEY ("integer");


SET search_path = postgrest, pg_catalog;

--
-- TOC entry 2184 (class 2606 OID 337626)
-- Name: auth_pkey; Type: CONSTRAINT; Schema: postgrest; Owner: postgrest_test; Tablespace: 
--

ALTER TABLE ONLY auth
    ADD CONSTRAINT auth_pkey PRIMARY KEY (id);


SET search_path = private, pg_catalog;

--
-- TOC entry 2186 (class 2606 OID 337628)
-- Name: articles_pkey; Type: CONSTRAINT; Schema: private; Owner: postgrest_test; Tablespace: 
--

ALTER TABLE ONLY articles
    ADD CONSTRAINT articles_pkey PRIMARY KEY (id);


SET search_path = postgrest, pg_catalog;

--
-- TOC entry 2189 (class 2620 OID 337630)
-- Name: ensure_auth_role_exists; Type: TRIGGER; Schema: postgrest; Owner: postgrest_test
--

CREATE CONSTRAINT TRIGGER ensure_auth_role_exists AFTER INSERT OR UPDATE ON auth NOT DEFERRABLE INITIALLY IMMEDIATE FOR EACH ROW EXECUTE PROCEDURE check_role_exists();


SET search_path = private, pg_catalog;

--
-- TOC entry 2190 (class 2620 OID 337631)
-- Name: articles_owner_track; Type: TRIGGER; Schema: private; Owner: postgrest_test
--

CREATE TRIGGER articles_owner_track BEFORE INSERT OR UPDATE ON articles FOR EACH ROW EXECUTE PROCEDURE postgrest.update_owner();


SET search_path = "1", pg_catalog;

--
-- TOC entry 2187 (class 2606 OID 337632)
-- Name: has_fk_fk_fkey; Type: FK CONSTRAINT; Schema: 1; Owner: postgrest_test
--

ALTER TABLE ONLY has_fk
    ADD CONSTRAINT has_fk_fk_fkey FOREIGN KEY (auto_inc_fk) REFERENCES auto_incrementing_pk(id);


--
-- TOC entry 2188 (class 2606 OID 337637)
-- Name: has_fk_simple_fk_fkey; Type: FK CONSTRAINT; Schema: 1; Owner: postgrest_test
--

ALTER TABLE ONLY has_fk
    ADD CONSTRAINT has_fk_simple_fk_fkey FOREIGN KEY (simple_fk) REFERENCES simple_pk(k);


--
-- TOC entry 2317 (class 0 OID 0)
-- Dependencies: 20
-- Name: 1; Type: ACL; Schema: -; Owner: postgrest_test
--

REVOKE ALL ON SCHEMA "1" FROM PUBLIC;
REVOKE ALL ON SCHEMA "1" FROM postgrest_test;
GRANT ALL ON SCHEMA "1" TO postgrest_test;
GRANT USAGE ON SCHEMA "1" TO postgrest_anonymous;
GRANT USAGE ON SCHEMA "1" TO postgrest_test_author;


--
-- TOC entry 2318 (class 0 OID 0)
-- Dependencies: 19
-- Name: postgrest; Type: ACL; Schema: -; Owner: postgrest_test
--

REVOKE ALL ON SCHEMA postgrest FROM PUBLIC;
REVOKE ALL ON SCHEMA postgrest FROM postgrest_test;
GRANT ALL ON SCHEMA postgrest TO postgrest_test;
GRANT USAGE ON SCHEMA postgrest TO postgrest_anonymous;


--
-- TOC entry 2320 (class 0 OID 0)
-- Dependencies: 21
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- TOC entry 2322 (class 0 OID 0)
-- Dependencies: 191
-- Name: authors_only; Type: ACL; Schema: 1; Owner: postgrest_test_author
--

REVOKE ALL ON TABLE authors_only FROM PUBLIC;
REVOKE ALL ON TABLE authors_only FROM postgrest_test_author;
GRANT ALL ON TABLE authors_only TO postgrest_test_author;


--
-- TOC entry 2323 (class 0 OID 0)
-- Dependencies: 192
-- Name: auto_incrementing_pk; Type: ACL; Schema: 1; Owner: postgrest_test
--

REVOKE ALL ON TABLE auto_incrementing_pk FROM PUBLIC;
REVOKE ALL ON TABLE auto_incrementing_pk FROM postgrest_test;
GRANT ALL ON TABLE auto_incrementing_pk TO postgrest_test;
GRANT ALL ON TABLE auto_incrementing_pk TO postgrest_anonymous;


--
-- TOC entry 2325 (class 0 OID 0)
-- Dependencies: 193
-- Name: auto_incrementing_pk_id_seq; Type: ACL; Schema: 1; Owner: postgrest_test
--

REVOKE ALL ON SEQUENCE auto_incrementing_pk_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE auto_incrementing_pk_id_seq FROM postgrest_test;
GRANT ALL ON SEQUENCE auto_incrementing_pk_id_seq TO postgrest_test;
GRANT USAGE ON SEQUENCE auto_incrementing_pk_id_seq TO postgrest_anonymous;


--
-- TOC entry 2326 (class 0 OID 0)
-- Dependencies: 194
-- Name: compound_pk; Type: ACL; Schema: 1; Owner: postgrest_test
--

REVOKE ALL ON TABLE compound_pk FROM PUBLIC;
REVOKE ALL ON TABLE compound_pk FROM postgrest_test;
GRANT ALL ON TABLE compound_pk TO postgrest_test;
GRANT ALL ON TABLE compound_pk TO postgrest_anonymous;


REVOKE ALL ON TABLE has_fk FROM PUBLIC;
REVOKE ALL ON TABLE has_fk FROM postgrest_test;
GRANT ALL ON TABLE has_fk TO postgrest_test;
GRANT ALL ON TABLE has_fk TO postgrest_anonymous;

--
-- TOC entry 2328 (class 0 OID 0)
-- Dependencies: 197
-- Name: items; Type: ACL; Schema: 1; Owner: postgrest_test
--

REVOKE ALL ON TABLE items FROM PUBLIC;
REVOKE ALL ON TABLE items FROM postgrest_test;
GRANT ALL ON TABLE items TO postgrest_test;
GRANT ALL ON TABLE items TO postgrest_anonymous;


--
-- TOC entry 2330 (class 0 OID 0)
-- Dependencies: 198
-- Name: items_id_seq; Type: ACL; Schema: 1; Owner: postgrest_test
--

REVOKE ALL ON SEQUENCE items_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE items_id_seq FROM postgrest_test;
GRANT ALL ON SEQUENCE items_id_seq TO postgrest_test;
GRANT USAGE ON SEQUENCE items_id_seq TO postgrest_anonymous;


--
-- TOC entry 2331 (class 0 OID 0)
-- Dependencies: 199
-- Name: menagerie; Type: ACL; Schema: 1; Owner: postgrest_test
--

REVOKE ALL ON TABLE menagerie FROM PUBLIC;
REVOKE ALL ON TABLE menagerie FROM postgrest_test;
GRANT ALL ON TABLE menagerie TO postgrest_test;
GRANT ALL ON TABLE menagerie TO postgrest_anonymous;


--
-- TOC entry 2332 (class 0 OID 0)
-- Dependencies: 200
-- Name: no_pk; Type: ACL; Schema: 1; Owner: postgrest_test
--

REVOKE ALL ON TABLE no_pk FROM PUBLIC;
REVOKE ALL ON TABLE no_pk FROM postgrest_test;
GRANT ALL ON TABLE no_pk TO postgrest_test;
GRANT ALL ON TABLE no_pk TO postgrest_anonymous;


--
-- TOC entry 2333 (class 0 OID 0)
-- Dependencies: 201
-- Name: simple_pk; Type: ACL; Schema: 1; Owner: postgrest_test
--

REVOKE ALL ON TABLE simple_pk FROM PUBLIC;
REVOKE ALL ON TABLE simple_pk FROM postgrest_test;
GRANT ALL ON TABLE simple_pk TO postgrest_test;
GRANT ALL ON TABLE simple_pk TO postgrest_anonymous;


SET search_path = postgrest, pg_catalog;

--
-- TOC entry 2334 (class 0 OID 0)
-- Dependencies: 202
-- Name: auth; Type: ACL; Schema: postgrest; Owner: postgrest_test
--

REVOKE ALL ON TABLE auth FROM PUBLIC;
REVOKE ALL ON TABLE auth FROM postgrest_test;
GRANT ALL ON TABLE auth TO postgrest_test;
GRANT INSERT ON TABLE auth TO postgrest_anonymous;


SET search_path = private, pg_catalog;

--
-- TOC entry 2335 (class 0 OID 0)
-- Dependencies: 203
-- Name: articles; Type: ACL; Schema: private; Owner: postgrest_test
--

REVOKE ALL ON TABLE articles FROM PUBLIC;
REVOKE ALL ON TABLE articles FROM postgrest_test;
GRANT ALL ON TABLE articles TO postgrest_test;


-- Completed on 2014-10-21 15:12:44 PDT

--
-- PostgreSQL database dump complete
--

