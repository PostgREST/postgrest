--
-- PostgreSQL database dump
--

-- Dumped from database version 9.3.4
-- Dumped by pg_dump version 9.3.1
-- Started on 2014-08-14 13:19:39 PDT

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- TOC entry 6 (class 2615 OID 226754)
-- Name: 1; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA "1";


--
-- TOC entry 172 (class 3079 OID 12018)
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- TOC entry 2206 (class 0 OID 0)
-- Dependencies: 172
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = "1", pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 170 (class 1259 OID 226755)
-- Name: auto_incrementing_pk; Type: TABLE; Schema: 1; Owner: -; Tablespace: 
--

CREATE TABLE auto_incrementing_pk (
    id integer NOT NULL,
    nullable_string character varying,
    non_nullable_string character varying NOT NULL,
    inserted_at timestamp with time zone DEFAULT now()
);


--
-- TOC entry 171 (class 1259 OID 226762)
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE; Schema: 1; Owner: -
--

CREATE SEQUENCE auto_incrementing_pk_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 2207 (class 0 OID 0)
-- Dependencies: 171
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE OWNED BY; Schema: 1; Owner: -
--

ALTER SEQUENCE auto_incrementing_pk_id_seq OWNED BY auto_incrementing_pk.id;


--
-- TOC entry 2091 (class 2604 OID 226764)
-- Name: id; Type: DEFAULT; Schema: 1; Owner: -
--

ALTER TABLE ONLY auto_incrementing_pk ALTER COLUMN id SET DEFAULT nextval('auto_incrementing_pk_id_seq'::regclass);


--
-- TOC entry 2093 (class 2606 OID 226766)
-- Name: auto_incrementing_pk_pkey; Type: CONSTRAINT; Schema: 1; Owner: -; Tablespace: 
--

ALTER TABLE ONLY auto_incrementing_pk
    ADD CONSTRAINT auto_incrementing_pk_pkey PRIMARY KEY (id);


-- Completed on 2014-08-14 13:19:39 PDT

--
-- PostgreSQL database dump complete
--

