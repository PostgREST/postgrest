\ir fixtures.sql

SELECT format('CREATE TABLE test.actors_%s ();', n)
FROM generate_series(1, 20000) n
\gexec

-- TODO add many function for fuzzy search (somehow this is making the loadtest start slow)
