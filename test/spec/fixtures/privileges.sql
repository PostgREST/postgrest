-- Privileges for anonymous
GRANT USAGE ON SCHEMA
      postgrest
    , test
    , jwt
    , public
    , "تست"
    , extensions
    , v1
    , v2
    , "SPECIAL ""@/\#~_-"
    , "EXTRA ""@/\#~_-"
TO postgrest_test_anonymous;

-- Schema test objects
SET search_path = test, "تست", pg_catalog;

GRANT ALL ON TABLE
    items
    , items2
    , items3
    , "articleStars"
    , articles
    , arrays
    , auto_incrementing_pk
    , clients
    , comments
    , complex_items
    , compound_pk
    , compound_pk_view
    , deferrable_unique_constraint
    , empty_table
    , fav_numbers
    , has_count_column
    , has_fk
    , insertable_view_with_join
    , json_table
    , materialized_view
    , menagerie
    , no_pk
    , nullable_integer
    , projects
    , projects_view
    , projects_view_alt
    , test_null_pk_competitors_sponsors
    , simple_pk
    , simple_pk2
    , tasks
    , filtered_tasks
    , tsearch
    , users
    , users_projects
    , users_tasks
    , files
    , touched_files
    , "Escap3e;"
    , "ghostBusters"
    , "withUnique"
    , "clashing_column"
    , "موارد"
    , addresses
    , orders
    , public.public_consumers
    , public.public_orders
    , consumers_view
    , consumers_view_view
    , consumers_extra_view
    , orders_view
    , images
    , images_base64
    , w_or_wo_comma_names
    , items_with_different_col_types
    , entities
    , child_entities
    , grandchild_entities
    , ranges
    , being
    , descendant
    , being_part
    , part
    , leak
    , perf_articles
    , employees
    , tiobe_pls
    , single_unique
    , compound_unique
    , only_pk
    , family_tree
    , managers
    , organizations
    , authors
    , publishers
    , books
    , forties_books
    , fifties_books
    , sixties_books
    , person
    , message
    , person_detail
    , space
    , zone
    , projects_dump
    , "UnitTest"
    , json_arr
    , jsonb_test
    , authors_books_number
    , authors_have_book_in_decade
    , authors_have_book_in_decade2
    , forties_and_fifties_books
    , odd_years_publications
    , foos
    , bars
    , materialized_projects
    , contract
    , player_view
    , contract_view
    , ltree_sample
    , isn_sample
    , projects_count_grouped_by
    , "Server Today"
    , pgrst_reserved_chars
    , authors_w_entities
    , openapi_types
    , openapi_defaults
    , getallprojects_view
    , get_projects_above_view
    , web_content
    , pages
    , referrals
    , big_projects
    , sites
    , jobs
    , main_jobs
    , whatev_projects
    , whatev_sites
    , whatev_jobs
    , agents
    , departments
    , schedules
    , activities
    , unit_workdays
    , unit_workdays_fst_shift
    , stuff
    , loc_test
    , v1.parents
    , v2.parents
    , v2.another_table
    , v1.children
    , v2.children
    , screens
    , labels
    , label_screen
    , actors
    , films
    , personnages
    , end_1
    , end_2
    , schauspieler
    , filme
    , rollen
    , products
    , suppliers
    , products_suppliers
    , trade_unions
    , suppliers_trade_unions
    , client
    , clientinfo
    , contact
    , chores
    , limited_update_items
    , limited_update_items_cpk
    , limited_update_items_no_pk
    , limited_update_items_view
    , limited_update_items_wnonuniq_view
    , limited_delete_items
    , limited_delete_items_cpk
    , limited_delete_items_no_pk
    , limited_delete_items_view
    , plate
    , well
    , limited_delete_items_wnonuniq_view
    , limited_delete_items_cpk_view
    , limited_update_items_cpk_view
    , xmltest
    , oid_test
    , job
    , series
    , adaptation_notifications
    , series_popularity
    , test
    , view_test
    , bulk_update_items
    , bulk_update_items_cpk
    , shops
    , shop_bles
    , "SPECIAL ""@/\#~_-".names
    , do$llar$s
TO postgrest_test_anonymous;

GRANT INSERT ON TABLE insertonly TO postgrest_test_anonymous;

GRANT USAGE ON SEQUENCE
      auto_incrementing_pk_id_seq
    , items_id_seq
    , items2_id_seq
    , items3_id_seq
    , callcounter_count
    , leak_id_seq
TO postgrest_test_anonymous;

-- Privileges for non anonymous users
GRANT USAGE ON SCHEMA test TO postgrest_test_author;
GRANT ALL ON TABLE authors_only TO postgrest_test_author;

GRANT SELECT (article_id, user_id) ON TABLE limited_article_stars TO postgrest_test_anonymous;
GRANT INSERT (article_id, user_id) ON TABLE limited_article_stars TO postgrest_test_anonymous;
GRANT UPDATE (article_id, user_id) ON TABLE limited_article_stars TO postgrest_test_anonymous;

GRANT SELECT(id, email) ON TABLE app_users TO postgrest_test_anonymous;
GRANT INSERT, UPDATE    ON TABLE app_users TO postgrest_test_anonymous;
GRANT DELETE            ON TABLE app_users TO postgrest_test_anonymous;

REVOKE EXECUTE ON FUNCTION privileged_hello(text) FROM PUBLIC; -- All functions are available to every role(PUBLIC) by default
GRANT EXECUTE ON FUNCTION privileged_hello(text) TO postgrest_test_author;

GRANT USAGE ON SCHEMA test TO postgrest_test_default_role;


DO $do$BEGIN
  IF (SELECT current_setting('server_version_num')::INT >= 100000) THEN
    GRANT ALL ON TABLE test.car_models TO postgrest_test_anonymous;
    GRANT ALL ON TABLE test.car_models_2021 TO postgrest_test_anonymous;
    GRANT ALL ON TABLE test.car_models_default TO postgrest_test_anonymous;
  END IF;

  IF (SELECT current_setting('server_version_num')::INT >= 110000) THEN
    GRANT ALL ON TABLE test.car_brands TO postgrest_test_anonymous;
  END IF;

  IF (SELECT current_setting('server_version_num')::INT >= 120000) THEN
    GRANT ALL ON TABLE test.car_model_sales TO postgrest_test_anonymous;
    GRANT ALL ON TABLE test.car_model_sales_202101 TO postgrest_test_anonymous;
    GRANT ALL ON TABLE test.car_model_sales_default TO postgrest_test_anonymous;
    GRANT ALL ON TABLE test.car_racers TO postgrest_test_anonymous;
    GRANT ALL ON TABLE test.car_dealers TO postgrest_test_anonymous;
    GRANT ALL ON TABLE test.car_dealers_springfield TO postgrest_test_anonymous;
    GRANT ALL ON TABLE test.car_dealers_default TO postgrest_test_anonymous;
    GRANT ALL ON TABLE test.car_models_car_dealers TO postgrest_test_anonymous;
    GRANT ALL ON TABLE test.car_models_car_dealers_10to20 TO postgrest_test_anonymous;
    GRANT ALL ON TABLE test.car_models_car_dealers_default TO postgrest_test_anonymous;
  END IF;
END$do$;
