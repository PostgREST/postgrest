--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5beta1
-- Dumped by pg_dump version 9.5beta1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: postgrest; Type: ACL; Schema: -; Owner: postgrest_test
--

REVOKE ALL ON SCHEMA postgrest FROM PUBLIC;
REVOKE ALL ON SCHEMA postgrest FROM postgrest_test;
GRANT ALL ON SCHEMA postgrest TO postgrest_test;
GRANT USAGE ON SCHEMA postgrest TO postgrest_anonymous;


--
-- Name: public; Type: ACL; Schema: -; Owner: diogo
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM diogo;
GRANT ALL ON SCHEMA public TO diogo;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- Name: test; Type: ACL; Schema: -; Owner: postgrest_test
--

REVOKE ALL ON SCHEMA test FROM PUBLIC;
REVOKE ALL ON SCHEMA test FROM postgrest_test;
GRANT ALL ON SCHEMA test TO postgrest_test;
GRANT USAGE ON SCHEMA test TO postgrest_anonymous;
GRANT USAGE ON SCHEMA test TO postgrest_test_author;


SET search_path = test, pg_catalog;

--
-- Name: items; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE items FROM PUBLIC;
REVOKE ALL ON TABLE items FROM postgrest_test;
GRANT ALL ON TABLE items TO postgrest_test;
GRANT ALL ON TABLE items TO postgrest_anonymous;


SET search_path = public, pg_catalog;

--
-- Name: always_true(test.items); Type: ACL; Schema: public; Owner: postgrest_test
--

REVOKE ALL ON FUNCTION always_true(test.items) FROM PUBLIC;
REVOKE ALL ON FUNCTION always_true(test.items) FROM postgrest_test;
GRANT ALL ON FUNCTION always_true(test.items) TO postgrest_test;
GRANT ALL ON FUNCTION always_true(test.items) TO postgrest_anonymous;


SET search_path = test, pg_catalog;

--
-- Name: getitemrange(bigint, bigint); Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON FUNCTION getitemrange(min bigint, max bigint) FROM PUBLIC;
REVOKE ALL ON FUNCTION getitemrange(min bigint, max bigint) FROM postgrest_test;
GRANT ALL ON FUNCTION getitemrange(min bigint, max bigint) TO postgrest_test;
GRANT ALL ON FUNCTION getitemrange(min bigint, max bigint) TO postgrest_anonymous;


--
-- Name: login(text, text); Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON FUNCTION login(id text, pass text) FROM PUBLIC;
REVOKE ALL ON FUNCTION login(id text, pass text) FROM postgrest_test;
GRANT ALL ON FUNCTION login(id text, pass text) TO postgrest_test;
GRANT ALL ON FUNCTION login(id text, pass text) TO postgrest_anonymous;


--
-- Name: problem(); Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON FUNCTION problem() FROM PUBLIC;
REVOKE ALL ON FUNCTION problem() FROM postgrest_test;
GRANT ALL ON FUNCTION problem() TO postgrest_test;
GRANT ALL ON FUNCTION problem() TO postgrest_test_author;


--
-- Name: sayhello(text); Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON FUNCTION sayhello(name text) FROM PUBLIC;
REVOKE ALL ON FUNCTION sayhello(name text) FROM postgrest_test;
GRANT ALL ON FUNCTION sayhello(name text) TO postgrest_test;
GRANT ALL ON FUNCTION sayhello(name text) TO postgrest_anonymous;


--
-- Name: test_empty_rowset(); Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON FUNCTION test_empty_rowset() FROM PUBLIC;
REVOKE ALL ON FUNCTION test_empty_rowset() FROM postgrest_test;
GRANT ALL ON FUNCTION test_empty_rowset() TO postgrest_test;
GRANT ALL ON FUNCTION test_empty_rowset() TO postgrest_anonymous;


SET search_path = postgrest, pg_catalog;

--
-- Name: auth; Type: ACL; Schema: postgrest; Owner: postgrest_test
--

REVOKE ALL ON TABLE auth FROM PUBLIC;
REVOKE ALL ON TABLE auth FROM postgrest_test;
GRANT ALL ON TABLE auth TO postgrest_test;
GRANT INSERT ON TABLE auth TO postgrest_anonymous;


SET search_path = private, pg_catalog;

--
-- Name: articles; Type: ACL; Schema: private; Owner: postgrest_test
--

REVOKE ALL ON TABLE articles FROM PUBLIC;
REVOKE ALL ON TABLE articles FROM postgrest_test;
GRANT ALL ON TABLE articles TO postgrest_test;


SET search_path = test, pg_catalog;

--
-- Name: articleStars; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE "articleStars" FROM PUBLIC;
REVOKE ALL ON TABLE "articleStars" FROM postgrest_test;
GRANT ALL ON TABLE "articleStars" TO postgrest_test;
GRANT ALL ON TABLE "articleStars" TO postgrest_anonymous;


--
-- Name: articles; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE articles FROM PUBLIC;
REVOKE ALL ON TABLE articles FROM postgrest_test;
GRANT ALL ON TABLE articles TO postgrest_test;
GRANT ALL ON TABLE articles TO postgrest_anonymous;


--
-- Name: authors_only; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE authors_only FROM PUBLIC;
REVOKE ALL ON TABLE authors_only FROM postgrest_test;
GRANT ALL ON TABLE authors_only TO postgrest_test;
GRANT ALL ON TABLE authors_only TO postgrest_test_author;


--
-- Name: auto_incrementing_pk; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE auto_incrementing_pk FROM PUBLIC;
REVOKE ALL ON TABLE auto_incrementing_pk FROM postgrest_test;
GRANT ALL ON TABLE auto_incrementing_pk TO postgrest_test;
GRANT ALL ON TABLE auto_incrementing_pk TO postgrest_anonymous;


--
-- Name: auto_incrementing_pk_id_seq; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON SEQUENCE auto_incrementing_pk_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE auto_incrementing_pk_id_seq FROM postgrest_test;
GRANT ALL ON SEQUENCE auto_incrementing_pk_id_seq TO postgrest_test;
GRANT USAGE ON SEQUENCE auto_incrementing_pk_id_seq TO postgrest_anonymous;


--
-- Name: clients; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE clients FROM PUBLIC;
REVOKE ALL ON TABLE clients FROM postgrest_test;
GRANT ALL ON TABLE clients TO postgrest_test;
GRANT ALL ON TABLE clients TO postgrest_anonymous;


--
-- Name: comments; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE comments FROM PUBLIC;
REVOKE ALL ON TABLE comments FROM postgrest_test;
GRANT ALL ON TABLE comments TO postgrest_test;
GRANT ALL ON TABLE comments TO postgrest_anonymous;


--
-- Name: complex_items; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE complex_items FROM PUBLIC;
REVOKE ALL ON TABLE complex_items FROM postgrest_test;
GRANT ALL ON TABLE complex_items TO postgrest_test;
GRANT ALL ON TABLE complex_items TO postgrest_anonymous;


--
-- Name: compound_pk; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE compound_pk FROM PUBLIC;
REVOKE ALL ON TABLE compound_pk FROM postgrest_test;
GRANT ALL ON TABLE compound_pk TO postgrest_test;
GRANT ALL ON TABLE compound_pk TO postgrest_anonymous;


--
-- Name: has_count_column; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE has_count_column FROM PUBLIC;
REVOKE ALL ON TABLE has_count_column FROM postgrest_test;
GRANT ALL ON TABLE has_count_column TO postgrest_test;
GRANT ALL ON TABLE has_count_column TO postgrest_anonymous;


--
-- Name: has_fk; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE has_fk FROM PUBLIC;
REVOKE ALL ON TABLE has_fk FROM postgrest_test;
GRANT ALL ON TABLE has_fk TO postgrest_test;
GRANT ALL ON TABLE has_fk TO postgrest_anonymous;


--
-- Name: insertable_view_with_join; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE insertable_view_with_join FROM PUBLIC;
REVOKE ALL ON TABLE insertable_view_with_join FROM postgrest_test;
GRANT ALL ON TABLE insertable_view_with_join TO postgrest_test;
GRANT ALL ON TABLE insertable_view_with_join TO postgrest_anonymous;


--
-- Name: items_id_seq; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON SEQUENCE items_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE items_id_seq FROM postgrest_test;
GRANT ALL ON SEQUENCE items_id_seq TO postgrest_test;
GRANT USAGE ON SEQUENCE items_id_seq TO postgrest_anonymous;


--
-- Name: json; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE json FROM PUBLIC;
REVOKE ALL ON TABLE json FROM postgrest_test;
GRANT ALL ON TABLE json TO postgrest_test;
GRANT ALL ON TABLE json TO postgrest_anonymous;


--
-- Name: materialized_view; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE materialized_view FROM PUBLIC;
REVOKE ALL ON TABLE materialized_view FROM postgrest_test;
GRANT ALL ON TABLE materialized_view TO postgrest_test;
GRANT ALL ON TABLE materialized_view TO postgrest_anonymous;


--
-- Name: menagerie; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE menagerie FROM PUBLIC;
REVOKE ALL ON TABLE menagerie FROM postgrest_test;
GRANT ALL ON TABLE menagerie TO postgrest_test;
GRANT ALL ON TABLE menagerie TO postgrest_anonymous;


--
-- Name: no_pk; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE no_pk FROM PUBLIC;
REVOKE ALL ON TABLE no_pk FROM postgrest_test;
GRANT ALL ON TABLE no_pk TO postgrest_test;
GRANT ALL ON TABLE no_pk TO postgrest_anonymous;


--
-- Name: nullable_integer; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE nullable_integer FROM PUBLIC;
REVOKE ALL ON TABLE nullable_integer FROM postgrest_test;
GRANT ALL ON TABLE nullable_integer TO postgrest_test;
GRANT ALL ON TABLE nullable_integer TO postgrest_anonymous;


--
-- Name: projects; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE projects FROM PUBLIC;
REVOKE ALL ON TABLE projects FROM postgrest_test;
GRANT ALL ON TABLE projects TO postgrest_test;
GRANT ALL ON TABLE projects TO postgrest_anonymous;


--
-- Name: projects_view; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE projects_view FROM PUBLIC;
REVOKE ALL ON TABLE projects_view FROM postgrest_test;
GRANT ALL ON TABLE projects_view TO postgrest_test;
GRANT ALL ON TABLE projects_view TO postgrest_anonymous;


--
-- Name: simple_pk; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE simple_pk FROM PUBLIC;
REVOKE ALL ON TABLE simple_pk FROM postgrest_test;
GRANT ALL ON TABLE simple_pk TO postgrest_test;
GRANT ALL ON TABLE simple_pk TO postgrest_anonymous;


--
-- Name: tasks; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE tasks FROM PUBLIC;
REVOKE ALL ON TABLE tasks FROM postgrest_test;
GRANT ALL ON TABLE tasks TO postgrest_test;
GRANT ALL ON TABLE tasks TO postgrest_anonymous;


--
-- Name: tsearch; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE tsearch FROM PUBLIC;
REVOKE ALL ON TABLE tsearch FROM postgrest_test;
GRANT ALL ON TABLE tsearch TO postgrest_test;
GRANT ALL ON TABLE tsearch TO postgrest_anonymous;


--
-- Name: users; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE users FROM PUBLIC;
REVOKE ALL ON TABLE users FROM postgrest_test;
GRANT ALL ON TABLE users TO postgrest_test;
GRANT ALL ON TABLE users TO postgrest_anonymous;


--
-- Name: users_projects; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE users_projects FROM PUBLIC;
REVOKE ALL ON TABLE users_projects FROM postgrest_test;
GRANT ALL ON TABLE users_projects TO postgrest_test;
GRANT ALL ON TABLE users_projects TO postgrest_anonymous;


--
-- Name: users_tasks; Type: ACL; Schema: test; Owner: postgrest_test
--

REVOKE ALL ON TABLE users_tasks FROM PUBLIC;
REVOKE ALL ON TABLE users_tasks FROM postgrest_test;
GRANT ALL ON TABLE users_tasks TO postgrest_test;
GRANT ALL ON TABLE users_tasks TO postgrest_anonymous;


--
-- PostgreSQL database dump complete
--

