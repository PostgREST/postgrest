\ir fixtures.sql

SELECT format('CREATE TABLE test.actors_%s ();', n)
FROM generate_series(1, 450) n -- 500 is the upper limit for table not found error hint generation
\gexec

-- TODO add many function for fuzzy search (somehow this is making the loadtest start slow)
