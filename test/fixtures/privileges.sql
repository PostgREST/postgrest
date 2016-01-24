-- Privileges for anonymous
GRANT USAGE ON SCHEMA
      postgrest
    , test
TO postgrest_test_anonymous;

-- Schema test objects
SET search_path = test, pg_catalog;

GRANT ALL ON TABLE
    items
    , "articleStars"
    , articles
    , auto_incrementing_pk
    , clients
    , comments
    , complex_items
    , compound_pk
    , has_count_column
    , has_fk
    , insertable_view_with_join
    , json
    , materialized_view
    , menagerie
    , no_pk
    , nullable_integer
    , projects
    , projects_view
    , simple_pk
    , tasks
    , tsearch
    , users
    , users_projects
    , users_tasks
    , "Escap3e;"
    , "ghostBusters"
    , "withUnique"
TO postgrest_test_anonymous;

GRANT INSERT ON TABLE insertonly TO postgrest_test_anonymous;

GRANT USAGE ON SEQUENCE
      auto_incrementing_pk_id_seq
    , items_id_seq
TO postgrest_test_anonymous;

-- Privileges for non anonymous users
GRANT USAGE ON SCHEMA test TO postgrest_test_author;
GRANT ALL ON TABLE authors_only TO postgrest_test_author;
