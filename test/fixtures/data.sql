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

SET search_path = postgrest, pg_catalog;

--
-- Data for Name: auth; Type: TABLE DATA; Schema: postgrest; Owner: -
--

TRUNCATE TABLE auth CASCADE;
INSERT INTO auth VALUES ('jdoe', 'postgrest_test_author', '1234                                                        ');


SET search_path = private, pg_catalog;

--
-- Data for Name: articles; Type: TABLE DATA; Schema: private; Owner: -
--

TRUNCATE TABLE articles CASCADE;
INSERT INTO articles VALUES (1, 'No… It''s a thing; it''s like a plan, but with more greatness.', 'diogo');
INSERT INTO articles VALUES (2, 'Stop talking, brain thinking. Hush.', 'diogo');
INSERT INTO articles VALUES (3, 'It''s a fez. I wear a fez now. Fezes are cool.', 'diogo');


SET search_path = test, pg_catalog;

--
-- Data for Name: users; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE users CASCADE;
INSERT INTO users VALUES (1, 'Angela Martin');
INSERT INTO users VALUES (2, 'Michael Scott');
INSERT INTO users VALUES (3, 'Dwight Schrute');


SET search_path = private, pg_catalog;

--
-- Data for Name: article_stars; Type: TABLE DATA; Schema: private; Owner: -
--

TRUNCATE TABLE article_stars CASCADE;
INSERT INTO article_stars VALUES (1, 1, '2015-12-08 04:22:57.472738');
INSERT INTO article_stars VALUES (1, 2, '2015-12-08 04:22:57.472738');
INSERT INTO article_stars VALUES (2, 3, '2015-12-08 04:22:57.472738');
INSERT INTO article_stars VALUES (3, 2, '2015-12-08 04:22:57.472738');
INSERT INTO article_stars VALUES (1, 3, '2015-12-08 04:22:57.472738');


SET search_path = test, pg_catalog;

--
-- Data for Name: authors_only; Type: TABLE DATA; Schema: test; Owner: -
--
TRUNCATE TABLE authors_only CASCADE;


--
-- Data for Name: auto_incrementing_pk; Type: TABLE DATA; Schema: test; Owner: -
--
TRUNCATE TABLE auto_incrementing_pk CASCADE;


--
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE SET; Schema: test; Owner: -
--

SELECT pg_catalog.setval('auto_incrementing_pk_id_seq', 1, true);


--
-- Data for Name: clients; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE clients CASCADE;
INSERT INTO clients VALUES (1, 'Microsoft');
INSERT INTO clients VALUES (2, 'Apple');


--
-- Data for Name: projects; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE projects CASCADE;
INSERT INTO projects VALUES (1, 'Windows 7', 1);
INSERT INTO projects VALUES (2, 'Windows 10', 1);
INSERT INTO projects VALUES (3, 'IOS', 2);
INSERT INTO projects VALUES (4, 'OSX', 2);
INSERT INTO projects VALUES (5, 'Orphan', NULL);


--
-- Data for Name: tasks; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE tasks CASCADE;
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

TRUNCATE TABLE users_tasks CASCADE;
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

TRUNCATE TABLE comments CASCADE;
INSERT INTO comments VALUES (1, 1, 2, 6, 'Needs to be delivered ASAP');


--
-- Data for Name: complex_items; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE complex_items CASCADE;
INSERT INTO complex_items VALUES (1, 'One', '{"foo":{"int":1,"bar":"baz"}}', '{1}');
INSERT INTO complex_items VALUES (2, 'Two', '{"foo":{"int":1,"bar":"baz"}}', '{1,2}');
INSERT INTO complex_items VALUES (3, 'Three', '{"foo":{"int":1,"bar":"baz"}}', '{1,2,3}');


--
-- Data for Name: compound_pk; Type: TABLE DATA; Schema: test; Owner: -
--
TRUNCATE TABLE compound_pk CASCADE;


--
-- Data for Name: simple_pk; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE simple_pk CASCADE;
INSERT INTO simple_pk VALUES ('xyyx', 'u');
INSERT INTO simple_pk VALUES ('xYYx', 'v');

--
-- Data for Name: has_fk; Type: TABLE DATA; Schema: test; Owner: -
--
TRUNCATE TABLE has_fk CASCADE;


--
-- Name: has_fk_id_seq; Type: SEQUENCE SET; Schema: test; Owner: -
--

SELECT pg_catalog.setval('has_fk_id_seq', 1, false);


--
-- Data for Name: items; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE items CASCADE;
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

SELECT pg_catalog.setval('items_id_seq', 15, true);


--
-- Data for Name: json; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE json CASCADE;
INSERT INTO json VALUES ('{"foo":{"bar":"baz"},"id":1}');
INSERT INTO json VALUES ('{"id":3}');
INSERT INTO json VALUES ('{"id":0}');


--
-- Data for Name: menagerie; Type: TABLE DATA; Schema: test; Owner: -
--
TRUNCATE TABLE menagerie CASCADE;


--
-- Data for Name: no_pk; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE no_pk CASCADE;
INSERT INTO no_pk VALUES (NULL, NULL);
INSERT INTO no_pk VALUES ('1', '0');
INSERT INTO no_pk VALUES ('2', '0');


--
-- Data for Name: nullable_integer; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE nullable_integer CASCADE;
INSERT INTO nullable_integer VALUES (NULL);


--
-- Data for Name: tsearch; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE tsearch CASCADE;
INSERT INTO tsearch VALUES (to_tsvector('It''s kind of fun to do the impossible'));
INSERT INTO tsearch VALUES (to_tsvector('But also fun to do what is possible'));
INSERT INTO tsearch VALUES (to_tsvector('Fat cats ate rats'));
INSERT INTO tsearch VALUES (to_tsvector('french', 'C''est un peu amusant de faire l''impossible'));
INSERT INTO tsearch VALUES (to_tsvector('german', 'Es ist eine Art Spaß, das Unmögliche zu machen')); 

--
-- Data for Name: users_projects; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE users_projects CASCADE;
INSERT INTO users_projects VALUES (1, 1);
INSERT INTO users_projects VALUES (1, 2);
INSERT INTO users_projects VALUES (2, 3);
INSERT INTO users_projects VALUES (2, 4);
INSERT INTO users_projects VALUES (3, 1);
INSERT INTO users_projects VALUES (3, 3);

TRUNCATE TABLE "Escap3e;" CASCADE;
INSERT INTO "Escap3e;" VALUES (1), (2), (3), (4), (5);

TRUNCATE TABLE "ghostBusters" CASCADE;
INSERT INTO "ghostBusters" VALUES (1), (3), (5);

TRUNCATE TABLE "withUnique" CASCADE;
INSERT INTO "withUnique" VALUES ('nodup', 'blah');



TRUNCATE TABLE addresses CASCADE;
INSERT INTO addresses VALUES (1, 'address 1');
INSERT INTO addresses VALUES (2, 'address 2');
INSERT INTO addresses VALUES (3, 'address 3');
INSERT INTO addresses VALUES (4, 'address 4');

TRUNCATE TABLE orders CASCADE;
INSERT INTO orders VALUES (1, 'order 1', 1, 2);
INSERT INTO orders VALUES (2, 'order 2', 3, 4);

TRUNCATE TABLE images CASCADE;
INSERT INTO images(name, img) VALUES ('A.png', decode('iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeAQMAAAAB/jzhAAAABlBMVEUAAAD/AAAb/40iAAAAP0lEQVQI12NgwAbYG2AE/wEYwQMiZB4ACQkQYZEAIgqAhAGIKLCAEQ8kgMT/P1CCEUwc4IMSzA3sUIIdCHECAGSQEkeOTUyCAAAAAElFTkSuQmCC', 'base64'));
INSERT INTO images(name, img) VALUES ('B.png', decode('iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeAQMAAAAB/jzhAAAABlBMVEX///8AAP94wDzzAAAAL0lEQVQIW2NgwAb+HwARH0DEDyDxwAZEyGAhLODqHmBRzAcn5GAS///A1IF14AAA5/Adbiiz/0gAAAAASUVORK5CYII=', 'base64'));

TRUNCATE TABLE w_or_wo_comma_names CASCADE;
INSERT INTO w_or_wo_comma_names VALUES ('Hebdon, John');
INSERT INTO w_or_wo_comma_names VALUES ('Williams, Mary');
INSERT INTO w_or_wo_comma_names VALUES ('Smith, Joseph');
INSERT INTO w_or_wo_comma_names VALUES ('David White');
INSERT INTO w_or_wo_comma_names VALUES ('Larry Thompson');

TRUNCATE TABLE items_with_different_col_types CASCADE;
INSERT INTO items_with_different_col_types VALUES (1, null, null, null, null, null, null, null);

TRUNCATE TABLE entities CASCADE;
INSERT INTO entities VALUES (1, 'entity 1', '{1}', '''bar'':2 ''foo'':1');
INSERT INTO entities VALUES (2, 'entity 2', '{1,2}', '''baz'':1 ''qux'':2');
INSERT INTO entities VALUES (3, 'entity 3', '{1,2,3}', null);
INSERT INTO entities VALUES (4, null, null, null);

TRUNCATE TABLE child_entities CASCADE;
INSERT INTO child_entities VALUES (1, 'child entity 1', 1);
INSERT INTO child_entities VALUES (2, 'child entity 2', 1);
INSERT INTO child_entities VALUES (3, 'child entity 3', 2);

TRUNCATE TABLE grandchild_entities CASCADE;
INSERT INTO grandchild_entities VALUES (1, 'grandchild entity 1', 1, null, null, null);
INSERT INTO grandchild_entities VALUES (2, 'grandchild entity 2', 1, null, null, null);
INSERT INTO grandchild_entities VALUES (3, 'grandchild entity 3', 2, null, null, null);
INSERT INTO grandchild_entities VALUES (4, '(grandchild,entity,4)', 2, null, null, '{"a": {"b":"foo"}}');
INSERT INTO grandchild_entities VALUES (5, '(grandchild,entity,5)', 2, null, null, '{"b":"bar"}');

TRUNCATE TABLE ranges CASCADE;
INSERT INTO ranges VALUES (1, '[1,3]');
INSERT INTO ranges VALUES (2, '[3,6]');
INSERT INTO ranges VALUES (3, '[6,9]');
INSERT INTO ranges VALUES (4, '[9,12]');

TRUNCATE TABLE being CASCADE;
INSERT INTO being VALUES (1), (2), (3), (4);

TRUNCATE TABLE descendant CASCADE;
INSERT INTO descendant VALUES (1,1), (2,1), (3,1), (4,2);

TRUNCATE TABLE part CASCADE;
INSERT INTO part VALUES (1), (2), (3), (4);

TRUNCATE TABLE being_part CASCADE;
INSERT INTO being_part VALUES (1,1), (2,1), (3,2), (4,3);

TRUNCATE TABLE employees CASCADE;
INSERT INTO employees VALUES
  ('Frances M.', 'Roe', '24000', 'One-Up Realty', 'Author'),
  ('Daniel B.', 'Lyon', '36000', 'Dubrow''s Cafeteria', 'Packer'),
  ('Edwin S.', 'Smith', '48000', 'Pro Garden Management', 'Marine biologist');

TRUNCATE TABLE tiobe_pls CASCADE;
INSERT INTO tiobe_pls VALUES ('Java', 1), ('C', 2), ('Python', 4);
