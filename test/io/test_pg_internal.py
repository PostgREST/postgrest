from util import psql_as_superuser
from postgrest import run


def test_listener_query_is_visible_in_pg_stat_activity(defaultenv):
    "The listener connection should show the LISTEN pgrst statement in pg_stat_activity"

    env = {
        **defaultenv,
        "PGRST_DB_CHANNEL_ENABLED": "true",
        "PGAPPNAME": "listener-query-test",
    }

    with run(env=env):
        output = psql_as_superuser(
            """
        select query
        from pg_stat_activity
        where application_name = 'listener-query-test'
          and query = 'LISTEN "pgrst"'
        limit 1;
        """,
            capture_output=True,
        ).strip()

        assert output == 'LISTEN "pgrst"'


def test_prepared_statement_cache_is_unbounded(defaultenv):
    "Prepared statement memory keeps growing as distinct queries accumulate"
    env = {
        **defaultenv,
        "PGRST_DB_POOL": "1",
        "PGRST_DB_PREPARED_STATEMENTS": "true",
        "PGRST_DB_AGGREGATES_ENABLED": "true",
        "PGRST_DB_CHANNEL_ENABLED": "false",
        # avoid flooding logs
        "PGRST_LOG_LEVEL": "crit",
    }

    with run(env=env) as postgrest:

        def cache_memory():
            response = postgrest.session.get("/rpc/prepared_statement_cache")
            assert response.status_code == 200, response.text
            return response.json()["used_bytes"]

        previous = cache_memory()

        total_qs = 500
        batch = 100

        for count in range(1, 1 + total_qs):
            # a new alias changes the sql
            response = postgrest.session.get(
                f"/directors?select=cache_probe_{count}:id,name,count(),"
                "films(id,title,count(),awards(id,name,year,count(),"
                "latest_year:year.max(),directors(id,name,count())))"
                "&id=gte.1"
                "&films.title=ilike.*fiction*"
                "&films.awards.year=gte.2000"
                "&films.awards.directors.name=ilike.*tarantino*"
                "&limit=0",
            )
            assert response.status_code == 200, response.text

            if count % batch == 0:
                used_bytes = cache_memory()
                assert used_bytes > previous
                print(f"{count} distinct queries: {used_bytes / 1024**2:.2f} MB used")
                previous = used_bytes
