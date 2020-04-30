-- Make sure that we can create temporary functions if we are in a transaction.
set transaction read write;


-- Numeric precision

-- Returns the numeric precision of the given column, if applicable.

create or replace function pg_temp.postgrest_numeric_precision(a pg_attribute, t pg_type)
  returns integer
  language sql
  as $$
    select
      information_schema._pg_char_max_length(
        information_schema._pg_truetypid(a.*, t.*),
        information_schema._pg_truetypmod(a.*, t.*)
      )
  $$;
