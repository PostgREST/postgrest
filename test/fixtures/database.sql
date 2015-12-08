DROP DATABASE IF EXISTS postgrest_test;
DROP ROLE IF EXISTS postgrest_test;
CREATE USER postgrest_test createdb createrole;
CREATE DATABASE postgrest_test OWNER postgrest_test;
