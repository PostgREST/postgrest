"Unit tests for Input/Ouput of PostgREST seen as a black box."

import os
import re
import signal
import time
import pytest

from config import CONFIGSDIR, FIXTURES, SECRET
from util import Thread, jwtauthheader, parse_server_timings_header
from postgrest import (
    freeport,
    is_ipv6,
    reset_statement_timeout,
    run,
    set_statement_timeout,
    sleep_until_postgrest_config_reload,
    sleep_until_postgrest_full_reload,
    sleep_until_postgrest_scache_reload,
    wait_until_exit,
)


def test_connect_with_dburi(dburi, defaultenv):
    "Connecting with db-uri instead of LIPQ* environment variables should work."
    defaultenv_without_libpq = {
        key: value
        for key, value in defaultenv.items()
        if key not in ["PGDATABASE", "PGHOST", "PGUSER"]
    }
    env = {**defaultenv_without_libpq, "PGRST_DB_URI": dburi.decode()}
    with run(env=env):
        pass


def test_read_dburi_from_stdin_without_eol(dburi, defaultenv):
    "Reading the dburi from stdin with a single line should work."
    defaultenv_without_libpq = {
        key: value
        for key, value in defaultenv.items()
        if key not in ["PGDATABASE", "PGHOST", "PGUSER"]
    }
    env = {**defaultenv_without_libpq, "PGRST_DB_URI": "@/dev/stdin"}

    with run(env=env, stdin=dburi):
        pass


def test_read_dburi_from_stdin_with_eol(dburi, defaultenv):
    "Reading the dburi from stdin containing a newline should work."
    defaultenv_without_libpq = {
        key: value
        for key, value in defaultenv.items()
        if key not in ["PGDATABASE", "PGHOST", "PGUSER"]
    }
    env = {**defaultenv_without_libpq, "PGRST_DB_URI": "@/dev/stdin"}

    with run(env=env, stdin=dburi + b"\n"):
        pass


def test_app_settings_flush_pool(defaultenv):
    """
    App settings should not reset when the db pool is flushed.

    See: https://github.com/PostgREST/postgrest/issues/1141

    """

    env = {**defaultenv, "PGRST_APP_SETTINGS_EXTERNAL_API_SECRET": "0123456789abcdef"}

    with run(env=env) as postgrest:
        uri = "/rpc/get_guc_value?name=app.settings.external_api_secret"
        response = postgrest.session.get(uri)
        assert response.text == '"0123456789abcdef"'

        # SIGUSR1 causes the postgres connection pool to be flushed
        postgrest.process.send_signal(signal.SIGUSR1)
        sleep_until_postgrest_scache_reload()

        uri = "/rpc/get_guc_value?name=app.settings.external_api_secret"
        response = postgrest.session.get(uri)
        assert response.text == '"0123456789abcdef"'


def test_flush_pool_no_interrupt(defaultenv):
    "Flushing the pool via SIGUSR1 doesn't interrupt ongoing requests"

    with run(env=defaultenv) as postgrest:

        def sleep():
            response = postgrest.session.get("/rpc/sleep?seconds=0.5")
            assert response.text == ""
            assert response.status_code == 204

        t = Thread(target=sleep)
        t.start()

        # make sure the request has started
        time.sleep(0.1)

        # SIGUSR1 causes the postgres connection pool to be flushed
        postgrest.process.send_signal(signal.SIGUSR1)

        t.join()


def test_random_port_bound(defaultenv):
    "PostgREST should bind to a random port when PGRST_SERVER_PORT is 0."

    with run(env=defaultenv, port="0"):
        assert True  # liveness check is done by run(), so we just need to check that it doesn't fail


def test_app_settings_reload(tmp_path, defaultenv):
    "App settings should be reloaded from file when PostgREST is sent SIGUSR2."
    config = (CONFIGSDIR / "sigusr2-settings.config").read_text()
    configfile = tmp_path / "test.config"
    configfile.write_text(config)
    uri = "/rpc/get_guc_value?name=app.settings.name_var"

    with run(configfile, env=defaultenv) as postgrest:
        response = postgrest.session.get(uri)
        assert response.text == '"John"'

        # change setting
        configfile.write_text(config.replace("John", "Jane"))
        # reload
        postgrest.process.send_signal(signal.SIGUSR2)

        sleep_until_postgrest_config_reload()

        response = postgrest.session.get(uri)
        assert response.text == '"Jane"'


def test_db_schema_reload(tmp_path, defaultenv):
    "DB schema should be reloaded from file when PostgREST is sent SIGUSR2."
    config = (CONFIGSDIR / "sigusr2-settings.config").read_text()
    configfile = tmp_path / "test.config"
    configfile.write_text(config)

    with run(configfile, env=defaultenv) as postgrest:
        response = postgrest.session.get("/rpc/get_guc_value?name=search_path")
        assert response.text == '"\\"public\\", \\"public\\""'

        # change setting
        configfile.write_text(
            config.replace('db-schemas = "public"', 'db-schemas = "v1"')
        )

        # reload config
        postgrest.process.send_signal(signal.SIGUSR2)
        sleep_until_postgrest_config_reload()

        # reload schema cache to verify that the config reload actually happened
        postgrest.process.send_signal(signal.SIGUSR1)
        sleep_until_postgrest_scache_reload()

        response = postgrest.session.get("/rpc/get_guc_value?name=search_path")
        assert response.text == '"\\"v1\\", \\"public\\""'


def test_db_schema_notify_reload(defaultenv):
    "DB schema and config should be reloaded when PostgREST is sent a NOTIFY"

    env = {**defaultenv, "PGRST_DB_CONFIG": "true", "PGRST_DB_CHANNEL_ENABLED": "true"}

    with run(env=env) as postgrest:
        response = postgrest.session.get("/rpc/get_guc_value?name=search_path")
        assert response.text == '"\\"public\\", \\"public\\""'

        # change db-schemas config on the db and reload config and cache with notify
        postgrest.session.post(
            "/rpc/change_db_schema_and_full_reload", data={"schemas": "v1"}
        )

        sleep_until_postgrest_full_reload()

        response = postgrest.session.get("/rpc/get_guc_value?name=search_path")
        assert response.text == '"\\"v1\\", \\"public\\""'

        # reset db-schemas config on the db
        response = postgrest.session.post("/rpc/reset_db_schema_config")
        assert response.text == ""
        assert response.status_code == 204


def test_max_rows_reload(defaultenv):
    "max-rows should be reloaded from role settings when PostgREST receives a SIGUSR2."
    env = {
        **defaultenv,
        "PGRST_DB_CONFIG": "true",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.head("/projects")
        assert response.status_code == 200
        assert response.headers["Content-Range"] == "0-4/*"

        # change max-rows config on the db
        postgrest.session.post("/rpc/change_max_rows_config", data={"val": 1})

        # reload config
        postgrest.process.send_signal(signal.SIGUSR2)

        sleep_until_postgrest_config_reload()

        response = postgrest.session.head("/projects")
        assert response.status_code == 200
        assert response.headers["Content-Range"] == "0-0/*"

        # reset max-rows config on the db
        response = postgrest.session.post("/rpc/reset_max_rows_config")
        assert response.text == ""
        assert response.status_code == 204


def test_max_rows_notify_reload(defaultenv):
    "max-rows should be reloaded from role settings when PostgREST receives a NOTIFY"

    env = {
        **defaultenv,
        "PGRST_DB_CONFIG": "true",
        "PGRST_DB_CHANNEL_ENABLED": "true",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.head("/projects")
        assert response.status_code == 200
        assert response.headers["Content-Range"] == "0-4/*"

        # change max-rows config on the db and reload with notify
        postgrest.session.post(
            "/rpc/change_max_rows_config", data={"val": 1, "notify": True}
        )

        sleep_until_postgrest_config_reload()

        response = postgrest.session.head("/projects")
        assert response.status_code == 200
        assert response.headers["Content-Range"] == "0-0/*"

        # reset max-rows config on the db
        response = postgrest.session.post("/rpc/reset_max_rows_config")
        assert response.text == ""
        assert response.status_code == 204


def test_invalid_role_claim_key_notify_reload(defaultenv):
    "NOTIFY reload config should show an error if role-claim-key is invalid"

    env = {
        **defaultenv,
        "PGRST_DB_CONFIG": "true",
        "PGRST_DB_CHANNEL_ENABLED": "true",
        "PGRST_LOG_LEVEL": "crit",
    }

    with run(env=env) as postgrest:
        postgrest.session.post("/rpc/invalid_role_claim_key_reload")

        output = postgrest.read_stdout()
        assert 'Received a config reload message on the "pgrst" channel' in output[0]
        output = postgrest.read_stdout()
        assert "failed to parse role-claim-key value" in output[0]

        response = postgrest.session.post("/rpc/reset_invalid_role_claim_key")
        assert response.text == ""
        assert response.status_code == 204


def test_notify_do_nothing(defaultenv):
    "NOTIFY with unknown message should do nothing"

    env = {
        **defaultenv,
        "PGRST_DB_CONFIG": "true",
        "PGRST_DB_CHANNEL_ENABLED": "true",
        "PGRST_LOG_LEVEL": "crit",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.post("/rpc/notify_do_nothing")
        assert response.text == ""
        assert response.status_code == 204

        output = postgrest.read_stdout()
        assert output == []


def test_db_prepared_statements_enable(defaultenv):
    "Should use prepared statements when the setting is enabled."

    with run(env=defaultenv) as postgrest:
        response = postgrest.session.post("/rpc/uses_prepared_statements")
        assert response.text == "true"


def test_db_prepared_statements_disable(defaultenv):
    "Should not use any prepared statements when the setting is disabled."

    env = {
        **defaultenv,
        "PGRST_DB_PREPARED_STATEMENTS": "false",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.post("/rpc/uses_prepared_statements")
        assert response.text == "false"


def test_statement_timeout(defaultenv, metapostgrest):
    "Statement timeout times out slow statements"

    role = "timeout_authenticator"
    set_statement_timeout(metapostgrest, role, 1000)  # 1 second

    env = {
        **defaultenv,
        "PGUSER": role,
        "PGRST_DB_ANON_ROLE": role,
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get("/rpc/sleep?seconds=0.5")
        assert response.text == ""
        assert response.status_code == 204

        response = postgrest.session.get("/rpc/sleep?seconds=2")
        assert response.status_code == 500
        data = response.json()
        assert data["message"] == "canceling statement due to statement timeout"

    reset_statement_timeout(metapostgrest, role)


def test_change_statement_timeout(defaultenv, metapostgrest):
    "Statement timeout changes take effect immediately"

    role = "timeout_authenticator"

    env = {
        **defaultenv,
        "PGUSER": role,
        "PGRST_DB_ANON_ROLE": role,
    }

    with run(env=env) as postgrest:
        # no limit initially
        response = postgrest.session.get("/rpc/sleep?seconds=1")
        assert response.text == ""
        assert response.status_code == 204

        set_statement_timeout(metapostgrest, role, 500)  # 0.5s

        # trigger schema refresh
        postgrest.process.send_signal(signal.SIGUSR1)
        sleep_until_postgrest_scache_reload()

        response = postgrest.session.get("/rpc/sleep?seconds=1")
        assert response.status_code == 500
        data = response.json()
        assert data["message"] == "canceling statement due to statement timeout"

        set_statement_timeout(metapostgrest, role, 2000)  # 2s

        # trigger role setting refresh
        postgrest.process.send_signal(signal.SIGUSR1)
        sleep_until_postgrest_scache_reload()

        response = postgrest.session.get("/rpc/sleep?seconds=1")
        assert response.text == ""
        assert response.status_code == 204

    reset_statement_timeout(metapostgrest, role)


def test_pool_size(defaultenv, metapostgrest):
    "Verify that PGRST_DB_POOL setting allows the correct number of parallel requests"

    env = {
        **defaultenv,
        "PGRST_DB_POOL": "2",
    }

    with run(env=env) as postgrest:
        start = time.time()
        threads = []
        for i in range(4):

            def sleep(i=i):
                response = postgrest.session.get("/rpc/sleep?seconds=0.5")
                assert response.text == ""
                assert response.status_code == 204, "thread {}".format(i)

            t = Thread(target=sleep)
            t.start()
            threads.append(t)
        for t in threads:
            t.join()
        end = time.time()
        delta = end - start

        # sleep 4 times for 0.5s each, with 2 requests in parallel
        # => total time roughly 1s
        assert delta > 1 and delta < 1.5


@pytest.mark.parametrize("level", ["crit", "error", "warn", "info", "debug"])
def test_pool_acquisition_timeout(level, defaultenv, metapostgrest):
    "Verify that PGRST_DB_POOL_ACQUISITION_TIMEOUT times out when the pool is empty"

    env = {
        **defaultenv,
        "PGRST_DB_POOL": "1",
        "PGRST_DB_POOL_ACQUISITION_TIMEOUT": "1",  # 1 second
        "PGRST_LOG_LEVEL": level,
    }

    with run(env=env, no_pool_connection_available=True) as postgrest:
        response = postgrest.session.get("/projects")
        assert response.status_code == 504
        data = response.json()
        assert data["message"] == "Timed out acquiring connection from connection pool."

        # ensure the message appears on the logs as well
        output = sorted(postgrest.read_stdout(nlines=10))

        if level == "crit":
            assert len(output) == 0
        else:
            assert any(" 504 " in line for line in output)
            assert any(
                "Timed out acquiring connection from connection pool." in line
                for line in output
            )


def test_change_statement_timeout_held_connection(defaultenv, metapostgrest):
    "Statement timeout changes take effect immediately, even with a request outliving the reconfiguration"

    role = "timeout_authenticator"

    env = {
        **defaultenv,
        "PGUSER": role,
        "PGRST_DB_ANON_ROLE": role,
        "PGRST_DB_POOL": "2",
    }

    with run(env=env) as postgrest:
        # start a slow request that holds a pool connection
        def hold_connection():
            response = postgrest.session.get("/rpc/sleep?seconds=1")
            assert response.text == ""
            assert response.status_code == 204

        hold = Thread(target=hold_connection)
        hold.start()
        # give the request time to start before SIGUSR1 flushes the pool
        time.sleep(0.1)

        set_statement_timeout(metapostgrest, role, 500)  # 0.5s
        # trigger schema refresh; flushes pool and establishes a new connection
        postgrest.process.send_signal(signal.SIGUSR1)

        # wait for the slow request's connection to be returned to the pool
        hold.join()

        # subsequent requests should fail due to the lowered timeout; run several in parallel
        # to ensure we use the full pool
        threads = []
        for i in range(2):

            def sleep(i=i):
                response = postgrest.session.get("/rpc/sleep?seconds=1")
                assert response.status_code == 500, "thread {}".format(i)
                data = response.json()
                assert data["message"] == "canceling statement due to statement timeout"

            thread = Thread(target=sleep)
            thread.start()
            threads.append(thread)

        for t in threads:
            t.join()

    reset_statement_timeout(metapostgrest, role)


def test_admin_schema_cache(defaultenv):
    "Should get a success response from the admin server containing current schema cache"

    with run(env=defaultenv) as postgrest:
        response = postgrest.admin.get("/schema_cache")
        assert response.status_code == 200
        assert '"dbTables":[[{"qiName":"authors_only"' in response.text


def test_admin_ready_w_channel(defaultenv):
    "Should get a success response from the admin server ready endpoint when the LISTEN channel is enabled"

    env = {
        **defaultenv,
        "PGRST_DB_CHANNEL_ENABLED": "true",
    }

    with run(env=env) as postgrest:
        response = postgrest.admin.get("/ready")
        assert response.status_code == 200


def test_admin_ready_wo_channel(defaultenv):
    "Should get a success response from the admin server ready endpoint when the LISTEN channel is disabled"

    env = {
        **defaultenv,
        "PGRST_DB_CHANNEL_ENABLED": "false",
    }

    with run(env=env) as postgrest:
        response = postgrest.admin.get("/ready")
        assert response.status_code == 200


def test_admin_ready_includes_schema_cache_state(defaultenv, metapostgrest):
    "Should get a failed response from the admin server ready endpoint when the schema cache is not loaded"

    role = "timeout_authenticator"

    env = {
        **defaultenv,
        "PGUSER": role,
        "PGRST_DB_ANON_ROLE": role,
        "PGRST_INTERNAL_SCHEMA_CACHE_QUERY_SLEEP": "500",
    }

    with run(env=env) as postgrest:
        # The schema cache query takes at least 500ms, due to PGRST_INTERNAL_SCHEMA_CACHE_QUERY_SLEEP above.
        # Make it impossible to load the schema cache, by setting statement timeout to 400ms.
        set_statement_timeout(metapostgrest, role, 400)

        # force a reconnection so the new role setting is picked up
        postgrest.process.send_signal(signal.SIGUSR1)

        postgrest.wait_until_scache_starts_loading()

        response = postgrest.admin.get("/ready", timeout=1)
        assert response.status_code == 503

        response = postgrest.session.get("/projects", timeout=1)
        assert response.status_code == 503

    reset_statement_timeout(metapostgrest, role)


def test_metrics_include_schema_cache_fails(defaultenv, metapostgrest):
    "Should get shema cache fails from the metrics endpoint"

    role = "timeout_authenticator"

    env = {
        **defaultenv,
        "PGUSER": role,
        "PGRST_INTERNAL_SCHEMA_CACHE_QUERY_SLEEP": "50",
    }

    with run(env=env) as postgrest:
        # The schema cache query takes at least 20ms, due to PGRST_INTERNAL_SCHEMA_CACHE_QUERY_SLEEP above.
        # Make it impossible to load the schema cache, by setting statement timeout to 100ms.
        set_statement_timeout(metapostgrest, role, 20)

        # force a reconnection so the new role setting is picked up
        postgrest.process.send_signal(signal.SIGUSR1)

        # wait for some schema cache retries
        time.sleep(1)

        response = postgrest.admin.get("/ready", timeout=1)
        assert response.status_code == 503

        response = postgrest.admin.get("/metrics", timeout=1)
        assert response.status_code == 200

        metrics = float(
            re.search(
                r'pgrst_schema_cache_loads_total{status="FAIL"} (\d+)', response.text
            ).group(1)
        )
        assert metrics == 1.0

    reset_statement_timeout(metapostgrest, role)


def test_admin_not_found(defaultenv):
    "Should get a not found from a undefined endpoint on the admin server"

    with run(env=defaultenv) as postgrest:
        response = postgrest.admin.get("/notfound")
        assert response.status_code == 404


def test_admin_ready_dependent_on_main_app(defaultenv):
    "Should get a failure from the admin ready endpoint if the main app also fails"

    with run(env=defaultenv) as postgrest:
        # delete the unix socket to make the main app fail
        os.remove(defaultenv["PGRST_SERVER_UNIX_SOCKET"])
        response = postgrest.admin.get("/ready")
        assert response.status_code == 500


def test_admin_live_good(defaultenv):
    "Should get a success from the admin live endpoint if the main app is running"

    with run(env=defaultenv, port=freeport()) as postgrest:
        response = postgrest.admin.get("/live")
        assert response.status_code == 200


def test_admin_live_dependent_on_main_app(defaultenv):
    "Should get a failure from the admin live endpoint if the main app also fails"

    with run(env=defaultenv) as postgrest:
        # delete the unix socket to make the main app fail
        os.remove(defaultenv["PGRST_SERVER_UNIX_SOCKET"])
        response = postgrest.admin.get("/live")
        assert response.status_code == 500


@pytest.mark.parametrize("specialhostvalue", FIXTURES["specialhostvalues"])
def test_admin_works_with_host_special_values(specialhostvalue, defaultenv):
    "Should get a success from the admin live and ready endpoints when using special host values for the main app"

    with run(env=defaultenv, port=freeport(), host=specialhostvalue) as postgrest:
        response = postgrest.admin.get("/live")
        assert response.status_code == 200

        response = postgrest.admin.get("/ready")
        assert response.status_code == 200


@pytest.mark.parametrize("level", ["crit", "error", "warn", "info", "debug"])
def test_log_level(level, defaultenv):
    "log_level should filter request logging"

    env = {**defaultenv, "PGRST_LOG_LEVEL": level}

    # any token to test 500 response for "Server lacks JWT secret"
    claim = {"role": "postgrest_test_author"}
    headers = jwtauthheader(claim, SECRET)

    with run(env=env) as postgrest:
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 500

        response = postgrest.session.get("/unknown")
        assert response.status_code == 404

        response = postgrest.session.get("/")
        assert response.status_code == 200

        output = sorted(postgrest.read_stdout(nlines=7))

        if level == "crit":
            assert len(output) == 0
        elif level == "error":
            assert re.match(
                r'- - - \[.+\] "GET / HTTP/1.1" 500 \d+ "" "python-requests/.+"',
                output[0],
            )
            assert len(output) == 1
        elif level == "warn":
            assert re.match(
                r'- - - \[.+\] "GET / HTTP/1.1" 500 \d+ "" "python-requests/.+"',
                output[0],
            )
            assert re.match(
                r'- - postgrest_test_anonymous \[.+\] "GET /unknown HTTP/1.1" 404 \d+ "" "python-requests/.+"',
                output[1],
            )
            assert len(output) == 2
        elif level == "info":
            assert re.match(
                r'- - - \[.+\] "GET / HTTP/1.1" 500 \d+ "" "python-requests/.+"',
                output[0],
            )
            assert re.match(
                r'- - postgrest_test_anonymous \[.+\] "GET / HTTP/1.1" 200 \d+ "" "python-requests/.+"',
                output[1],
            )
            assert re.match(
                r'- - postgrest_test_anonymous \[.+\] "GET /unknown HTTP/1.1" 404 \d+ "" "python-requests/.+"',
                output[2],
            )
            assert len(output) == 3
        elif level == "debug":
            assert re.match(
                r'- - - \[.+\] "GET / HTTP/1.1" 500 \d+ "" "python-requests/.+"',
                output[0],
            )
            assert re.match(
                r'- - postgrest_test_anonymous \[.+\] "GET / HTTP/1.1" 200 \d+ "" "python-requests/.+"',
                output[1],
            )
            assert re.match(
                r'- - postgrest_test_anonymous \[.+\] "GET /unknown HTTP/1.1" 404 \d+ "" "python-requests/.+"',
                output[2],
            )

            assert len(output) == 7
            assert any("Connection" and "is available" in line for line in output)
            assert any("Connection" and "is used" in line for line in output)


@pytest.mark.parametrize("level", ["crit", "error", "warn", "info", "debug"])
def test_log_query(level, defaultenv):
    "log_query=true should log the SQL query according to the log_level"

    def drain_stdout(proc):
        lines = []
        while True:
            chunk = proc.read_stdout(nlines=20)
            if not chunk:
                break
            lines.extend(chunk)
        return lines

    env = {
        **defaultenv,
        "PGRST_LOG_LEVEL": level,
        "PGRST_LOG_QUERY": "true",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get("/")
        assert response.status_code == 200

        response = postgrest.session.get("/projects")
        assert response.status_code == 200

        response = postgrest.session.get(
            "/projects", headers={"Prefer": "count=estimated"}
        )
        assert response.status_code == 206

        response = postgrest.session.get(
            "/projects", headers={"Prefer": "count=planned"}
        )
        assert response.status_code == 206

        response = postgrest.session.get("/infinite_recursion")
        assert response.status_code == 500

        get_2xx_regx = r'.+: WITH pgrst_source AS.+SELECT "public"\."projects"\.\* FROM "public"\."projects".+_postgrest_t'
        get_2xx_count_regx = (
            r'.+: EXPLAIN \(FORMAT JSON\) SELECT 1  FROM "public"."projects"'
        )
        infinite_recursion_5xx_regx = r'.+: WITH pgrst_source AS.+SELECT "public"\."infinite_recursion"\.\* FROM "public"\."infinite_recursion".+_postgrest_t'
        root_tables_regx = r".+: SELECT   n.nspname AS table_schema, .+ FROM pg_class c .+ ORDER BY table_schema, table_name"
        root_procs_regx = r".+: WITH base_types AS \(.+\) SELECT   pn.nspname AS proc_schema, .+ FROM pg_proc p.+AND p.pronamespace = \$1::regnamespace"
        root_descr_regx = r".+: SELECT pg_catalog\.obj_description\(\$1::regnamespace, 'pg_namespace'\)"
        set_config_regx = (
            r".+: select set_config\('search_path', \$1, true\), set_config\("
        )

        output = drain_stdout(postgrest)

        project_queries = [line for line in output if re.match(get_2xx_regx, line)]
        project_counts = [line for line in output if re.match(get_2xx_count_regx, line)]
        infinite_queries = [
            line for line in output if re.match(infinite_recursion_5xx_regx, line)
        ]
        root_tables = [line for line in output if re.match(root_tables_regx, line)]
        root_procs = [line for line in output if re.match(root_procs_regx, line)]
        root_descr = [line for line in output if re.match(root_descr_regx, line)]
        set_configs = [line for line in output if re.match(set_config_regx, line)]

        if level == "crit":
            assert not set_configs
            assert not project_queries
            assert not project_counts
            assert not infinite_queries
            assert not root_tables
            assert not root_procs
            assert not root_descr
        elif level in {"error", "warn"}:
            assert len(set_configs) == 1
            assert len(infinite_queries) == 1
            assert not project_queries
            assert not project_counts
            assert not root_tables
            assert not root_procs
            assert not root_descr
        elif level == "info":
            assert len(set_configs) == 5
            assert len(project_queries) == 3
            assert len(project_counts) == 2
            assert len(infinite_queries) == 1
            assert len(root_tables) == 1
            assert len(root_procs) == 1
            assert len(root_descr) == 1
        elif level == "debug":
            assert len(set_configs) == 5
            assert len(project_queries) == 3
            assert len(project_counts) == 2
            assert len(infinite_queries) == 1
            assert len(root_tables) == 1
            assert len(root_procs) == 1
            assert len(root_descr) == 1

    pre_req_env = {
        **env,
        "PGRST_DB_PRE_REQUEST": "do_nothing",
    }

    with run(env=pre_req_env) as postgrest:
        response = postgrest.session.get("/projects")
        assert response.status_code == 200

        output = drain_stdout(postgrest)

        pre_request_regx = r'.+: select "do_nothing"()'
        pre_reqs = [line for line in output if re.match(pre_request_regx, line)]

        if level == "crit":
            assert not pre_reqs
        elif level in {"error", "warn"}:
            assert not pre_reqs
        elif level == "info":
            assert len(pre_reqs) == 1
        elif level == "debug":
            assert len(pre_reqs) == 1


def test_no_pool_connection_required_on_bad_http_logic(defaultenv):
    "no pool connection should be consumed for failing on invalid http logic"

    with run(env=defaultenv, no_pool_connection_available=True) as postgrest:
        # not found nested route shouldn't require opening a connection
        response = postgrest.session.head("/path/notfound")
        assert response.status_code == 404

        # an invalid http method on a resource shouldn't require opening a connection
        response = postgrest.session.request("TRACE", "/projects")
        assert response.status_code == 405
        response = postgrest.session.patch("/rpc/hello")
        assert response.status_code == 405


def test_no_pool_connection_required_on_options(defaultenv):
    "no pool connection should be consumed for OPTIONS requests"

    with run(env=defaultenv, no_pool_connection_available=True) as postgrest:
        # OPTIONS on a table shouldn't require opening a connection
        response = postgrest.session.options("/projects")
        assert response.status_code == 200

        # OPTIONS on RPC shouldn't require opening a connection
        response = postgrest.session.options("/rpc/hello")
        assert response.status_code == 200

        # OPTIONS on root shouldn't require opening a connection
        response = postgrest.session.options("/")
        assert response.status_code == 200


def test_no_pool_connection_required_on_bad_jwt_claim(defaultenv):
    "no pool connection should be consumed for failing on invalid jwt"

    env = {**defaultenv, "PGRST_JWT_SECRET": SECRET}

    with run(env=env, no_pool_connection_available=True) as postgrest:
        # A JWT with an invalid signature shouldn't open a connection
        headers = jwtauthheader({"role": "postgrest_test_author"}, "Wrong Secret")
        response = postgrest.session.get("/projects", headers=headers)
        assert response.status_code == 401


def test_no_pool_connection_required_on_bad_embedding(defaultenv):
    "no pool connection should be consumed for failing to embed"

    with run(env=defaultenv, no_pool_connection_available=True) as postgrest:
        # OPTIONS on a table shouldn't require opening a connection
        response = postgrest.session.get("/projects?select=*,unexistent(*)")
        assert response.status_code == 400


# https://github.com/PostgREST/postgrest/issues/2620
def test_notify_reloading_catalog_cache(defaultenv):
    "notify should reload the connection catalog cache"

    with run(env=defaultenv) as postgrest:
        # first the id col is an uuid
        response = postgrest.session.get(
            "/cats?id=eq.dea27321-f988-4a57-93e4-8eeb38f3cf1e"
        )
        assert response.status_code == 200

        # change it to a bigint
        response = postgrest.session.post("/rpc/drop_change_cats")
        assert response.text == ""
        assert response.status_code == 204
        sleep_until_postgrest_scache_reload()

        # next request should succeed with a bigint value
        response = postgrest.session.get("/cats?id=eq.1")
        assert response.status_code == 200


def test_role_settings(defaultenv):
    "statement_timeout should be set per role"

    env = {
        **defaultenv,
        "PGRST_JWT_SECRET": SECRET,
    }

    with run(env=env) as postgrest:
        # statement_timeout for postgrest_test_anonymous
        response = postgrest.session.get("/rpc/get_guc_value?name=statement_timeout")
        assert response.text == '"2s"'

        # reload statement_timeout with NOTIFY
        response = postgrest.session.post(
            "/rpc/change_role_statement_timeout", data={"timeout": "5s"}
        )
        assert response.text == ""
        assert response.status_code == 204

        response = postgrest.session.get("/rpc/reload_pgrst_config")
        assert response.text == ""
        assert response.status_code == 204
        sleep_until_postgrest_config_reload()

        response = postgrest.session.get("/rpc/get_guc_value?name=statement_timeout")
        assert response.text == '"5s"'

        # statement_timeout for postgrest_test_author
        headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)
        response = postgrest.session.get(
            "/rpc/get_guc_value?name=statement_timeout", headers=headers
        )
        assert response.text == '"10s"'

        # reset statement timeout to original value
        response = postgrest.session.post(
            "/rpc/change_role_statement_timeout", data={"timeout": "2s"}
        )
        assert response.status_code == 204


def test_isolation_level(defaultenv):
    "isolation_level should be set per role and per function"

    env = {
        **defaultenv,
        "PGRST_JWT_SECRET": SECRET,
    }

    with run(env=env) as postgrest:
        # default isolation level for postgrest_test_anonymous
        response = postgrest.session.get(
            "/items_w_isolation_level?select=isolation_level&limit=1"
        )
        assert response.text == '[{"isolation_level":"read committed"}]'

        # isolation level for postgrest_test_repeatable_read on GET
        headers = jwtauthheader({"role": "postgrest_test_repeatable_read"}, SECRET)
        response = postgrest.session.get(
            "/items_w_isolation_level?select=isolation_level&limit=1", headers=headers
        )
        assert response.text == '[{"isolation_level":"repeatable read"}]'

        # isolation level for postgrest_test_serializable on POST
        headers = jwtauthheader({"role": "postgrest_test_serializable"}, SECRET)
        headers["Prefer"] = "return=representation"
        response = postgrest.session.post(
            "/items_w_isolation_level?select=isolation_level",
            json={"id": "666"},
            headers=headers,
        )
        assert response.text == '[{"isolation_level":"serializable"}]'

        # isolation level for postgrest_test_serializable on PATCH
        headers = jwtauthheader({"role": "postgrest_test_serializable"}, SECRET)
        headers["Prefer"] = "return=representation"
        response = postgrest.session.patch(
            "/items_w_isolation_level?select=isolation_level&id=eq.666",
            json={"id": "666"},
            headers=headers,
        )
        assert response.text == '[{"isolation_level":"serializable"}]'

        # isolation level for postgrest_test_serializable on DELETE
        headers = jwtauthheader({"role": "postgrest_test_serializable"}, SECRET)
        headers["Prefer"] = "return=representation"
        response = postgrest.session.delete(
            "/items_w_isolation_level?select=isolation_level&id=eq.666", headers=headers
        )
        assert response.text == '[{"isolation_level":"serializable"}]'

        # default isolation level for function
        response = postgrest.session.get("/rpc/default_isolation_level")
        assert response.text == '"read committed"'

        # changes with role isolation level
        headers = jwtauthheader({"role": "postgrest_test_repeatable_read"}, SECRET)
        response = postgrest.session.get(
            "/rpc/default_isolation_level", headers=headers
        )
        assert response.text == '"repeatable read"'

        # isolation level can be set per function
        response = postgrest.session.get("/rpc/serializable_isolation_level")
        assert response.text == '"serializable"'
        response = postgrest.session.get("/rpc/repeatable_read_isolation_level")
        assert response.text == '"repeatable read"'

        # isolation level for a function overrides the role isolation level
        headers = jwtauthheader({"role": "postgrest_test_repeatable_read"}, SECRET)
        response = postgrest.session.get("/rpc/serializable_isolation_level")
        assert response.text == '"serializable"'


def test_schema_cache_concurrent_notifications(slow_schema_cache_env):
    "schema cache should be up-to-date whenever a notification is sent while another reload is in progress, see https://github.com/PostgREST/postgrest/issues/2791"

    internal_sleep = (
        int(slow_schema_cache_env["PGRST_INTERNAL_SCHEMA_CACHE_QUERY_SLEEP"]) / 1000
    )

    with run(env=slow_schema_cache_env, wait_for_readiness=False) as postgrest:
        time.sleep(2 * internal_sleep + 0.1)  # wait for readiness manually

        # first request, create a function and set a schema cache reload in progress
        response = postgrest.session.post("/rpc/create_function")
        assert response.text == ""
        assert response.status_code == 204

        time.sleep(
            internal_sleep / 2
        )  # wait to be inside the schema cache reload process

        # second request, change the same function and do another schema cache reload
        response = postgrest.session.post("/rpc/migrate_function")
        assert response.text == ""
        assert response.status_code == 204

        time.sleep(
            2 * internal_sleep
        )  # wait enough time to get the final schema cache state

        # confirm the schema cache is up-to-date and the 2nd reload wasn't lost
        response = postgrest.session.get("/rpc/mult_them?c=3&d=4")
        assert response.text == "12"
        assert response.status_code == 200


def test_schema_cache_query_sleep_logs(defaultenv):
    """Schema cache sleep should be reflected in the logged query duration."""

    env = {
        **defaultenv,
        "PGRST_INTERNAL_SCHEMA_CACHE_QUERY_SLEEP": "1000",
    }
    log_pattern = re.compile(r"Schema cache queried in ([\d.]+) milliseconds")

    with run(env=env, wait_max_seconds=3, no_startup_stdout=False) as postgrest:
        observed_ms = None
        collected = []

        lines = postgrest.read_stdout(nlines=10)
        collected.extend(lines)
        for line in lines:
            match = log_pattern.search(line)
            if match:
                observed_ms = float(match.group(1))
                break

        assert observed_ms is not None
        assert 1000 < observed_ms < 2000


def test_schema_cache_load_sleep_logs(defaultenv):
    """Schema cache load sleep should be reflected in the logged load duration."""

    env = {
        **defaultenv,
        "PGRST_INTERNAL_SCHEMA_CACHE_LOAD_SLEEP": "1000",
    }
    log_pattern = re.compile(r"Schema cache loaded in ([\d.]+) milliseconds")

    with run(env=env, wait_max_seconds=3, no_startup_stdout=False) as postgrest:
        observed_ms = None
        collected = []

        lines = postgrest.read_stdout(nlines=10)
        collected.extend(lines)
        for line in lines:
            match = log_pattern.search(line)
            if match:
                observed_ms = float(match.group(1))
                break

        assert observed_ms is not None
        assert 1000 < observed_ms < 2000


@pytest.mark.parametrize("dburi_type", ["no_params", "no_params_qmark", "with_params"])
def test_get_pgrst_version_with_uri_connection_string(dburi_type, dburi, defaultenv):
    "The fallback_application_name should be added to the db-uri if it has a URI format"
    defaultenv_without_libpq = {
        key: value
        for key, value in defaultenv.items()
        if key not in ["PGDATABASE", "PGHOST", "PGUSER"]
    }

    env = {
        "no_params": {**defaultenv, "PGRST_DB_URI": "postgresql://"},
        "no_params_qmark": {**defaultenv, "PGRST_DB_URI": "postgresql://?"},
        "with_params": {**defaultenv_without_libpq, "PGRST_DB_URI": dburi.decode()},
    }

    with run(env=env[dburi_type]) as postgrest:
        response = postgrest.session.post("/rpc/get_pgrst_version")
        version = '"%s"' % response.headers["Server"].replace(
            "postgrest/", "PostgREST "
        )
        assert response.text == version


def test_get_pgrst_version_with_keyval_connection_string(defaultenv):
    "The fallback_application_name should be added to the db-uri if it has a keyword/value format"
    uri = f'dbname={defaultenv["PGDATABASE"]} host={defaultenv["PGHOST"]} user={defaultenv["PGUSER"]}'
    defaultenv_without_libpq = {
        key: value
        for key, value in defaultenv.items()
        if key not in ["PGDATABASE", "PGHOST", "PGUSER"]
    }
    env = {**defaultenv_without_libpq, "PGRST_DB_URI": uri}

    with run(env=env) as postgrest:
        response = postgrest.session.post("/rpc/get_pgrst_version")
        version = '"%s"' % response.headers["Server"].replace(
            "postgrest/", "PostgREST "
        )
        assert response.text == version


def test_log_postgrest_version(defaultenv):
    "Should show the PostgREST version in the logs"
    with run(env=defaultenv, no_startup_stdout=False) as postgrest:
        version = postgrest.session.head("/").headers["Server"].split("/")[1]

        output = postgrest.read_stdout(nlines=1)

        assert "Starting PostgREST %s..." % version in output[0]


@pytest.mark.parametrize(
    "host", ["127.0.0.1", "::1", None], ids=["IPv4", "IPv6", "Unix"]
)
def test_log_postgrest_host_and_port(host, defaultenv):
    "PostgREST should output the host and port it is bound to."

    # We run postgrest on unix socket when host and port are set to None
    is_unix = host is None
    port = None if is_unix else freeport()

    with run(
        env=defaultenv, host=host, port=port, no_startup_stdout=False
    ) as postgrest:
        output = postgrest.read_stdout(nlines=10)

        if is_unix:
            re.match(r'API server listening on "/tmp/.*\.sock"', output[2])
        elif is_ipv6(host):
            assert f"API server listening on [{host}]:{port}" in output[2]
        else:  # IPv4
            assert f"API server listening on {host}:{port}" in output[2]


def test_succeed_w_role_having_superuser_settings(defaultenv):
    "Should succeed when having superuser settings on the impersonated role"

    env = {**defaultenv, "PGRST_DB_CONFIG": "true", "PGRST_JWT_SECRET": SECRET}

    with run(stdin=SECRET.encode(), env=env) as postgrest:
        headers = jwtauthheader({"role": "postgrest_test_w_superuser_settings"}, SECRET)
        response = postgrest.session.get("/projects", headers=headers)
        print(response.text)
        assert response.status_code == 200


def test_get_granted_superuser_setting(defaultenv):
    "Should succeed when the impersonated role has granted superuser settings"

    env = {**defaultenv, "PGRST_DB_CONFIG": "true", "PGRST_JWT_SECRET": SECRET}

    with run(stdin=SECRET.encode(), env=env) as postgrest:
        response_ver = postgrest.session.get("/rpc/get_postgres_version")
        pg_ver = eval(response_ver.text)
        if pg_ver >= 150000:
            headers = jwtauthheader(
                {"role": "postgrest_test_w_superuser_settings"}, SECRET
            )
            response = postgrest.session.get(
                "/rpc/get_guc_value?name=log_min_duration_sample", headers=headers
            )
            assert response.text == '"12345ms"'


def test_fail_with_invalid_dbname_and_automatic_recovery_disabled(defaultenv):
    "Should fail without retries when automatic recovery is disabled and dbname is invalid"
    dbname = "INVALID"
    uri = f'postgresql://?dbname={dbname}&host={defaultenv["PGHOST"]}&user={defaultenv["PGUSER"]}'
    env = {
        **defaultenv,
        "PGRST_DB_URI": uri,
        "PGRST_DB_POOL_AUTOMATIC_RECOVERY": "false",
    }

    with run(env=env, wait_for_readiness=False) as postgrest:
        exitCode = wait_until_exit(postgrest)
        assert exitCode == 1


def test_fail_with_automatic_recovery_disabled_and_terminated_using_query(defaultenv):
    "Should fail without retries when automatic recovery is disabled and pg_terminate_backend(pid) is called"

    env = {
        **defaultenv,
        "PGRST_DB_POOL_AUTOMATIC_RECOVERY": "false",
        "PGAPPNAME": "target",
    }

    app_name = "'{}'".format(env["PGAPPNAME"])

    with run(env=env) as postgrest:
        os.system(
            f'psql -d {env["PGDATABASE"]} -U {env["PGUSER"]} -h {env["PGHOST"]} --set ON_ERROR_STOP=1 -a -c "SELECT terminate_pgrst({app_name})"'
        )

        exitCode = wait_until_exit(postgrest)
        assert exitCode == 1


def test_shutdown_wait_period_delays_sigterm(defaultenv):
    "SIGTERM should wait server-shutdown-wait-period seconds before exiting"

    env = {
        **defaultenv,
        "PGRST_SERVER_SHUTDOWN_WAIT_PERIOD": "1",
    }

    with run(env=env, wait_max_seconds=3) as postgrest:
        start_time = time.time()
        postgrest.process.send_signal(signal.SIGTERM)
        wait_until_exit(postgrest, timeout=3)
        elapsed = time.time() - start_time

        assert elapsed >= 1.0, f"Should delay at least 1 second, but only waited {elapsed}s"


def test_sigint_exits_immediately_with_shutdown_wait_period(defaultenv):
    "SIGINT should exit immediately even with shutdown wait period configured"

    env = {
        **defaultenv,
        "PGRST_SERVER_SHUTDOWN_WAIT_PERIOD": "5",
    }

    with run(env=env) as postgrest:
        start_time = time.time()
        postgrest.process.send_signal(signal.SIGINT)
        wait_until_exit(postgrest, timeout=2)
        elapsed = time.time() - start_time

        assert elapsed < 1.0, f"SIGINT should exit immediately, but waited {elapsed}s"


def test_preflight_request_with_cors_allowed_origin_config(defaultenv):
    "OPTIONS preflight request should return Access-Control-Allow-Origin equal to origin"

    env = {
        **defaultenv,
        "PGRST_SERVER_CORS_ALLOWED_ORIGINS": "http://example.com, http://example2.com",
    }

    headers = {
        "Accept": "*/*",
        "Origin": "http://example.com",
        "Access-Control-Request-Method": "POST",
        "Access-Control-Request-Headers": "Content-Type",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.options("/items", headers=headers)
        assert (
            response.headers["Access-Control-Allow-Origin"] == "http://example.com"
            and response.headers["Access-Control-Allow-Credentials"] == "true"
        )


def test_preflight_request_with_empty_cors_allowed_origin_config(defaultenv):
    "OPTIONS preflight request should allow all origins when config is present but empty"

    env = {
        **defaultenv,
        "PGRST_SERVER_CORS_ALLOWED_ORIGINS": "",
    }

    headers = {
        "Accept": "*/*",
        "Origin": "http://anyorigin.com",
        "Access-Control-Request-Method": "POST",
        "Access-Control-Request-Headers": "Content-Type",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.options("/items", headers=headers)
        assert response.headers["Access-Control-Allow-Origin"] == "*"
        assert "POST" in response.headers["Access-Control-Allow-Methods"]


def test_no_preflight_request_with_CORS_config_should_return_header(defaultenv):
    "GET no preflight request should return Access-Control-Allow-Origin equal to origin"

    env = {
        **defaultenv,
        "PGRST_SERVER_CORS_ALLOWED_ORIGINS": "http://example.com, http://example2.com",
    }

    headers = {
        "Accept": "*/*",
        "Origin": "http://example.com",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get("/items", headers=headers)
        assert response.headers["Access-Control-Allow-Origin"] == "http://example.com"


def test_no_preflight_request_with_CORS_config_should_not_return_header(defaultenv):
    "GET no preflight request should not return Access-Control-Allow-Origin"

    env = {
        **defaultenv,
        "PGRST_SERVER_CORS_ALLOWED_ORIGINS": "http://example.com, http://example2.com",
    }

    headers = {
        "Accept": "*/*",
        "Origin": "http://invalid.com",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get("/items", headers=headers)
        assert "Access-Control-Allow-Origin" not in response.headers


@pytest.mark.parametrize("level", ["crit", "error", "warn", "info", "debug"])
def test_db_error_logging_to_stderr(level, defaultenv, metapostgrest):
    "verify that DB errors are logged to stderr"

    role = "timeout_authenticator"
    set_statement_timeout(metapostgrest, role, 500)

    env = {
        **defaultenv,
        "PGUSER": role,
        "PGRST_DB_ANON_ROLE": role,
        "PGRST_LOG_LEVEL": level,
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get("/rpc/sleep?seconds=1")
        assert response.status_code == 500

        # ensure the message appears on the logs
        output = sorted(postgrest.read_stdout(nlines=6))

        if level == "crit":
            assert len(output) == 0
        elif level == "debug":
            assert " 500 " in output[0]
            assert "canceling statement due to statement timeout" in output[5]
        else:
            assert " 500 " in output[0]
            assert "canceling statement due to statement timeout" in output[1]

    reset_statement_timeout(metapostgrest, role)


def test_function_setting_statement_timeout_fails(defaultenv):
    "statement that takes three seconds to execute should fail with one second timeout"

    with run(env=defaultenv) as postgrest:
        response = postgrest.session.post("/rpc/one_sec_timeout")

        assert response.status_code == 500
        assert (
            response.text
            == '{"code":"57014","details":null,"hint":null,"message":"canceling statement due to statement timeout"}'
        )


def test_function_setting_statement_timeout_passes(defaultenv):
    "statement that takes three seconds to execute should succeed with four second timeout"

    with run(env=defaultenv) as postgrest:
        response = postgrest.session.post("/rpc/four_sec_timeout")

        assert response.text == ""
        assert response.status_code == 204


def test_function_setting_work_mem(defaultenv):
    "check function setting work_mem is applied"

    env = {
        **defaultenv,
        "PGRST_DB_HOISTED_TX_SETTINGS": "work_mem",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get("/rpc/rpc_work_mem?select=get_work_mem")

        assert response.text == '{"get_work_mem":"6000kB"}'


def test_multiple_func_settings(defaultenv):
    "check multiple function settings are applied"

    env = {
        **defaultenv,
        "PGRST_DB_HOISTED_TX_SETTINGS": "work_mem,statement_timeout",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get(
            "/rpc/rpc_with_two_hoisted?select=get_work_mem,get_statement_timeout"
        )

        assert (
            response.text == '{"get_work_mem":"5000kB","get_statement_timeout":"10s"}'
        )


def test_first_hoisted_setting_is_applied(defaultenv):
    "test that work_mem is applied and statement_timeout is not applied"

    env = {
        **defaultenv,
        "PGRST_DB_HOISTED_TX_SETTINGS": "work_mem",  # only work_mem is hoisted
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get(
            "/rpc/rpc_with_one_hoisted?select=get_work_mem,get_statement_timeout"
        )

        assert response.text == '{"get_work_mem":"3000kB","get_statement_timeout":"2s"}'


def test_second_hoisted_setting_is_applied(defaultenv):
    "test that statement_timeout is applied and work_mem is not applied"

    env = {
        **defaultenv,
        "PGRST_DB_HOISTED_TX_SETTINGS": "statement_timeout",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get(
            "/rpc/rpc_with_one_hoisted?select=get_work_mem,get_statement_timeout"
        )

        assert response.text == '{"get_work_mem":"4MB","get_statement_timeout":"7s"}'


def test_admin_metrics(defaultenv):
    "Should get metrics from the admin endpoint"

    with run(env=defaultenv, port=freeport()) as postgrest:
        response = postgrest.admin.get("/metrics")
        assert response.status_code == 200
        assert response.headers["Content-Type"] == "text/plain; charset=utf-8"
        assert "pgrst_schema_cache_query_time_seconds" in response.text
        assert 'pgrst_schema_cache_loads_total{status="SUCCESS"}' in response.text
        assert "pgrst_db_pool_max" in response.text
        assert "pgrst_db_pool_waiting" in response.text
        assert "pgrst_db_pool_available" in response.text
        assert "pgrst_db_pool_timeouts_total" in response.text


def test_schema_cache_startup_load_with_in_db_config(defaultenv, metapostgrest):
    "verify that the Schema Cache loads correctly at startup, using the in-db `pgrst.db_schemas` config"

    response = metapostgrest.session.post("/rpc/change_db_schemas_config")
    assert response.text == ""
    assert response.status_code == 204

    with run(env=defaultenv) as postgrest:
        response = postgrest.session.get("/rpc/get_current_schema")
        assert response.text == '"test"'
        assert response.status_code == 200

    response = metapostgrest.session.post("/rpc/reset_db_schemas_config")
    assert response.text == ""
    assert response.status_code == 204


def test_pgrst_log_503_client_error_to_stderr(defaultenv):
    "PostgREST should log 503 errors to stderr"

    env = {
        **defaultenv,
        "PGAPPNAME": "test-io",
    }

    with run(env=env) as postgrest:

        postgrest.session.get("/rpc/terminate_pgrst?appname=test-io")

        output = postgrest.read_stdout(nlines=6)

        log_message = '{"code":"PGRST001","details":"no connection to the server\\n","hint":null,"message":"Database client error. Retrying the connection."}\n'

        assert any(log_message in line for line in output)


def test_log_error_when_empty_schema_cache_on_startup_to_stderr(defaultenv):
    "Should log the 503 error message when there is an empty schema cache on startup"

    env = {
        **defaultenv,
        "PGRST_INTERNAL_SCHEMA_CACHE_QUERY_SLEEP": "300",
    }

    with run(env=env, wait_for_readiness=False) as postgrest:
        postgrest.wait_until_scache_starts_loading()

        response = postgrest.session.get("/projects")
        assert response.status_code == 503

        output_start = postgrest.read_stdout(nlines=10)

        log_err_message = '{"code":"PGRST002","details":null,"hint":null,"message":"Could not query the database for the schema cache. Retrying."}'

        assert any(log_err_message in line for line in output_start)


def test_no_double_schema_cache_reload_on_empty_schema(defaultenv):
    "Should only load the schema cache once on a 503 error when there's an empty schema cache on startup"

    env = {
        **defaultenv,
        "PGRST_INTERNAL_SCHEMA_CACHE_QUERY_SLEEP": "300",
    }

    with run(env=env, port=freeport(), wait_for_readiness=False) as postgrest:
        postgrest.wait_until_scache_starts_loading()

        response = postgrest.session.get("/projects")
        assert response.status_code == 503

        # Should wait enough time to load the schema cache twice to guarantee that the test is valid
        time.sleep(1)

        response = postgrest.admin.get("/metrics")
        assert response.status_code == 200
        assert 'pgrst_schema_cache_loads_total{status="SUCCESS"} 1.0' in response.text


@pytest.mark.parametrize("level", ["crit", "error", "warn", "info", "debug"])
def test_log_pool_req_observation(level, defaultenv):
    "PostgREST should log PoolRequest and PoolRequestFullfilled observation when log-level=debug"

    env = {**defaultenv, "PGRST_LOG_LEVEL": level, "PGRST_JWT_SECRET": SECRET}

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    pool_req = "Trying to borrow a connection from pool"
    pool_req_fullfill = "Borrowed a connection from the pool"

    with run(env=env) as postgrest:

        postgrest.session.get("/authors_only", headers=headers)

        if level == "debug":
            output = postgrest.read_stdout(nlines=5)
            assert pool_req in output[1]
            assert pool_req_fullfill in output[4]
            assert len(output) == 5
        elif level == "info":
            output = postgrest.read_stdout(nlines=4)
            assert len(output) == 1
        else:
            output = postgrest.read_stdout(nlines=4)
            assert len(output) == 0


def test_proxy_status_header(defaultenv, metapostgrest):
    "Test Proxy-Status header in statement timeout error"

    role = "timeout_authenticator"
    set_statement_timeout(metapostgrest, role, 1000)  # 1 second

    env = {
        **defaultenv,
        "PGUSER": role,
        "PGRST_DB_ANON_ROLE": role,
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get("/rpc/sleep?seconds=2")
        assert response.status_code == 500
        assert response.headers["Proxy-Status"] == "PostgREST; error=57014"
        data = response.json()
        assert data["message"] == "canceling statement due to statement timeout"


def test_allow_configs_to_be_set_to_empty(defaultenv):
    'configs that are explicitly set to empty (= "<empty>") should not throw parse error'

    env = {
        **defaultenv,
        "PGRST_DB_EXTRA_SEARCH_PATH": "",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get("/projects")
        assert response.status_code == 200


def test_schema_cache_error_observation(defaultenv):
    "schema cache error observation should be logged with invalid db-schemas or db-extra-search-path"

    env = {
        **defaultenv,
        "PGRST_DB_EXTRA_SEARCH_PATH": "x",
    }

    with run(env=env, no_startup_stdout=False, wait_for_readiness=False) as postgrest:
        # TODO: postgrest should exit here, instead it keeps retrying
        # exitCode = wait_until_exit(postgrest)
        # assert exitCode == 1

        output = postgrest.read_stdout(nlines=9)
        assert (
            "Failed to load the schema cache using db-schemas=public and db-extra-search-path=x"
            in output[7]
        )


def test_log_listener_connection_errors(defaultenv):
    "The logs should show the listener connection error message in a single line"

    env = {
        **defaultenv,
        "PGHOST": "no_host",
        "PGRST_DB_CHANNEL_ENABLED": "true",
    }

    with run(env=env, no_startup_stdout=False, wait_for_readiness=False) as postgrest:
        output = postgrest.read_stdout(nlines=5)
        assert any(
            'Failed listening for database notifications on the "pgrst" channel. could not translate host name "no_host" to address:'
            in line
            for line in output
        )


def test_db_pre_config_with_pg_reserved_words(defaultenv):
    "The db-pre-config should not fail unexpectedly when function name is a postgres reserved word"

    env = {
        **defaultenv,
        "PGRST_DB_PRE_CONFIG": "true",  # call true function
    }

    with run(env=env) as postgrest:
        response = postgrest.session.post("/rpc/true")
        assert response.status_code == 200

    env = {
        **defaultenv,
        "PGRST_DB_PRE_CONFIG": "select",  # no "select" function in our fixtures, fail gracefully at startup
    }

    with run(env=env, no_startup_stdout=False, wait_for_readiness=False) as postgrest:
        output = postgrest.read_stdout(nlines=8)
        assert any(
            'Failed to query database settings for the config parameters.{"code":"42883","details":null,"hint":"No function matches the given name and argument types. You might need to add explicit type casts.","message":"function select() does not exist"}'
            in line
            for line in output
        )


def test_requests_with_resource_embedding_wait_for_schema_cache_reload(defaultenv):
    "requests that use the schema cache with resource embedding wait long for the schema cache to reload"

    env = {
        **defaultenv,
        "PGRST_DB_POOL": "2",
        "PGRST_INTERNAL_SCHEMA_CACHE_RELATIONSHIP_LOAD_SLEEP": "5100",
    }

    with run(env=env, wait_max_seconds=30) as postgrest:
        # reload the schema cache
        response = postgrest.session.get("/rpc/notify_pgrst")
        assert response.status_code == 204

        postgrest.wait_until_scache_starts_loading()

        response = postgrest.session.get("/directors?select=id,name,films(title)")
        assert response.status_code == 200

        assert response.elapsed.total_seconds() > 5


def test_requests_without_resource_embedding_wait_for_schema_cache_reload(defaultenv):
    "requests that use the schema cache without resource embedding wait less for the schema cache to reload"

    env = {
        **defaultenv,
        "PGRST_DB_POOL": "2",
        "PGRST_INTERNAL_SCHEMA_CACHE_LOAD_SLEEP": "1100",
        "PGRST_INTERNAL_SCHEMA_CACHE_RELATIONSHIP_LOAD_SLEEP": "5000",
    }

    with run(env=env, wait_max_seconds=30) as postgrest:
        # reload the schema cache
        response = postgrest.session.get("/rpc/notify_pgrst")
        assert response.status_code == 204

        postgrest.wait_until_scache_starts_loading()

        response = postgrest.session.get("/films")
        assert response.status_code == 200

        assert (
            response.elapsed.total_seconds() > 1
            and response.elapsed.total_seconds() < 5
        )


def test_server_timing_transaction_duration(defaultenv, metapostgrest):
    "server-timing transaction duration should be accurate"

    # just to ensure we don't timeout
    role = "timeout_authenticator"
    set_statement_timeout(metapostgrest, role, 3000)  # 3 seconds

    env = {
        **defaultenv,
        "PGUSER": role,
        "PGRST_DB_ANON_ROLE": role,
        "PGRST_SERVER_TIMING_ENABLED": "true",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get("/rpc/sleep?seconds=2")

        assert response.status_code == 204

        response_dur = parse_server_timings_header(response.headers["Server-Timing"])[
            "transaction"
        ]

        assert 2000 <= response_dur < 3000
