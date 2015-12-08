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

SET search_path = postgrest, pg_catalog;

--
-- Data for Name: auth; Type: TABLE DATA; Schema: postgrest; Owner: -
--

INSERT INTO auth VALUES ('jdoe', 'postgrest_test_author', '1234                                                        ');


SET search_path = private, pg_catalog;

--
-- Data for Name: articles; Type: TABLE DATA; Schema: private; Owner: -
--

INSERT INTO articles VALUES (1, 'No… It''s a thing; it''s like a plan, but with more greatness.', 'diogo');
INSERT INTO articles VALUES (2, 'Stop talking, brain thinking. Hush.', 'diogo');
INSERT INTO articles VALUES (3, 'It''s a fez. I wear a fez now. Fezes are cool.', 'diogo');


SET search_path = test, pg_catalog;

--
-- Data for Name: users; Type: TABLE DATA; Schema: test; Owner: -
--

INSERT INTO users VALUES (1, 'Angela Martin');
INSERT INTO users VALUES (2, 'Michael Scott');
INSERT INTO users VALUES (3, 'Dwight Schrute');


SET search_path = private, pg_catalog;

--
-- Data for Name: article_stars; Type: TABLE DATA; Schema: private; Owner: -
--

INSERT INTO article_stars VALUES (1, 1, '2015-12-08 04:22:57.472738');
INSERT INTO article_stars VALUES (1, 2, '2015-12-08 04:22:57.472738');
INSERT INTO article_stars VALUES (2, 3, '2015-12-08 04:22:57.472738');
INSERT INTO article_stars VALUES (3, 2, '2015-12-08 04:22:57.472738');
INSERT INTO article_stars VALUES (1, 3, '2015-12-08 04:22:57.472738');


SET search_path = test, pg_catalog;

--
-- Data for Name: authors_only; Type: TABLE DATA; Schema: test; Owner: -
--



--
-- Data for Name: auto_incrementing_pk; Type: TABLE DATA; Schema: test; Owner: -
--



--
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE SET; Schema: test; Owner: -
--

SELECT pg_catalog.setval('auto_incrementing_pk_id_seq', 1, true);


--
-- Data for Name: clients; Type: TABLE DATA; Schema: test; Owner: -
--

INSERT INTO clients VALUES (1, 'Microsoft');
INSERT INTO clients VALUES (2, 'Apple');


--
-- Data for Name: projects; Type: TABLE DATA; Schema: test; Owner: -
--

INSERT INTO projects VALUES (1, 'Windows 7', 1);
INSERT INTO projects VALUES (2, 'Windows 10', 1);
INSERT INTO projects VALUES (3, 'IOS', 2);
INSERT INTO projects VALUES (4, 'OSX', 2);


--
-- Data for Name: tasks; Type: TABLE DATA; Schema: test; Owner: -
--

INSERT INTO tasks VALUES (1, 'Design w7', 1);
INSERT INTO tasks VALUES (2, 'Code w7', 1);
INSERT INTO tasks VALUES (3, 'Design w10', 2);
INSERT INTO tasks VALUES (4, 'Code w10', 2);
INSERT INTO tasks VALUES (5, 'Design IOS', 3);
INSERT INTO tasks VALUES (6, 'Code IOS', 3);
INSERT INTO tasks VALUES (7, 'Design OSX', 4);
INSERT INTO tasks VALUES (8, 'Code OSX', 4);


--
-- Data for Name: users_tasks; Type: TABLE DATA; Schema: test; Owner: -
--

INSERT INTO users_tasks VALUES (1, 1);
INSERT INTO users_tasks VALUES (1, 2);
INSERT INTO users_tasks VALUES (1, 3);
INSERT INTO users_tasks VALUES (1, 4);
INSERT INTO users_tasks VALUES (2, 5);
INSERT INTO users_tasks VALUES (2, 6);
INSERT INTO users_tasks VALUES (2, 7);
INSERT INTO users_tasks VALUES (3, 1);
INSERT INTO users_tasks VALUES (3, 5);


--
-- Data for Name: comments; Type: TABLE DATA; Schema: test; Owner: -
--

INSERT INTO comments VALUES (1, 1, 2, 6, 'Needs to be delivered ASAP');


--
-- Data for Name: complex_items; Type: TABLE DATA; Schema: test; Owner: -
--

INSERT INTO complex_items VALUES (1, 'One', '{"foo":{"int":1,"bar":"baz"}}', '{1}');
INSERT INTO complex_items VALUES (2, 'Two', '{"foo":{"int":1,"bar":"baz"}}', '{1,2}');
INSERT INTO complex_items VALUES (3, 'Three', '{"foo":{"int":1,"bar":"baz"}}', '{1,2,3}');


--
-- Data for Name: compound_pk; Type: TABLE DATA; Schema: test; Owner: -
--



--
-- Data for Name: simple_pk; Type: TABLE DATA; Schema: test; Owner: -
--



--
-- Data for Name: has_fk; Type: TABLE DATA; Schema: test; Owner: -
--



--
-- Name: has_fk_id_seq; Type: SEQUENCE SET; Schema: test; Owner: -
--

SELECT pg_catalog.setval('has_fk_id_seq', 1, false);


--
-- Data for Name: items; Type: TABLE DATA; Schema: test; Owner: -
--

INSERT INTO items VALUES (1);
INSERT INTO items VALUES (2);
INSERT INTO items VALUES (3);
INSERT INTO items VALUES (4);
INSERT INTO items VALUES (5);
INSERT INTO items VALUES (6);
INSERT INTO items VALUES (7);
INSERT INTO items VALUES (8);
INSERT INTO items VALUES (9);
INSERT INTO items VALUES (10);
INSERT INTO items VALUES (11);
INSERT INTO items VALUES (12);
INSERT INTO items VALUES (13);
INSERT INTO items VALUES (14);
INSERT INTO items VALUES (15);


--
-- Name: items_id_seq; Type: SEQUENCE SET; Schema: test; Owner: -
--

SELECT pg_catalog.setval('items_id_seq', 1, true);


--
-- Data for Name: json; Type: TABLE DATA; Schema: test; Owner: -
--

INSERT INTO json VALUES ('{"foo":{"bar":"baz"},"id":1}');


--
-- Data for Name: menagerie; Type: TABLE DATA; Schema: test; Owner: -
--



--
-- Data for Name: no_pk; Type: TABLE DATA; Schema: test; Owner: -
--

INSERT INTO no_pk VALUES (NULL, NULL);
INSERT INTO no_pk VALUES ('1', '0');
INSERT INTO no_pk VALUES ('2', '0');


--
-- Data for Name: nullable_integer; Type: TABLE DATA; Schema: test; Owner: -
--

INSERT INTO nullable_integer VALUES (NULL);


--
-- Data for Name: tsearch; Type: TABLE DATA; Schema: test; Owner: -
--

INSERT INTO tsearch VALUES ('''bar'':2 ''foo'':1');
INSERT INTO tsearch VALUES ('''baz'':1 ''qux'':2');


--
-- Data for Name: users_projects; Type: TABLE DATA; Schema: test; Owner: -
--

INSERT INTO users_projects VALUES (1, 1);
INSERT INTO users_projects VALUES (1, 2);
INSERT INTO users_projects VALUES (2, 3);
INSERT INTO users_projects VALUES (2, 4);
INSERT INTO users_projects VALUES (3, 1);
INSERT INTO users_projects VALUES (3, 3);


--
-- PostgreSQL database dump complete
--

