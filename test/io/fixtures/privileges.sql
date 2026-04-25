GRANT USAGE ON SCHEMA v1 TO postgrest_test_anonymous;
GRANT USAGE ON SCHEMA test TO postgrest_test_anonymous, "Postgrest_Test_Mixed_Case";

GRANT SELECT ON authors_only TO postgrest_test_author;
GRANT SELECT ON projects TO postgrest_test_anonymous, postgrest_test_w_superuser_settings, "Postgrest_Test_Mixed_Case";
GRANT SELECT ON directors, films TO postgrest_test_anonymous, postgrest_test_w_superuser_settings;

GRANT ALL ON cats TO postgrest_test_anonymous;
GRANT ALL ON items_w_isolation_level TO postgrest_test_anonymous, postgrest_test_repeatable_read, postgrest_test_serializable;
