-- Loads all fixtures for the PostgREST observability tests

\set ON_ERROR_STOP on

\ir database.sql
\ir roles.sql
\ir schema.sql
\ir privileges.sql
