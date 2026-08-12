"Unit tests for Input/Ouput of PostgREST seen as a black box."

import time
import pytest

from util import (
    Thread,
    psql_as_superuser,
)
from postgrest import (
    PostgrestTimedOut,
    freeport,
    run,
    wait_until_exit,
)


def test_graceful_shutdown_waits_for_in_flight_request(defaultenv):
    "SIGTERM should allow in-flight requests to finish before exiting"

    with run(env=defaultenv, wait_max_seconds=5) as postgrest:

        def sleep():
            response = postgrest.session.get("/rpc/sleep?seconds=3", timeout=10)
            assert response.text == ""
            assert response.status_code == 204

        t = Thread(target=sleep)
        t.start()

        # Wait for the request to be in-flight before shutting down.
        time.sleep(1)

        postgrest.process.terminate()

        t.join()


def test_random_port_bound(defaultenv):
    "PostgREST should bind to a random port when PGRST_SERVER_PORT is 0."

    with run(env=defaultenv, port="0"):
        assert True  # liveness check is done by run(), so we just need to check that it doesn't fail


def test_so_reuseport_zero_downtime_handover(defaultenv):
    "A second PostgREST instance should take over on the same main/admin ports without request failures."

    # set host to _all_ addresses to force port conflict without SO_REUSEPORT
    # setting to localhost (which is the default)
    # might allow running multiple instances on the same port
    # as the name might be resolved to many IP addresses
    host = "0.0.0.0"
    port = freeport()
    admin_port = freeport(used_ports=[port])
    failures = []
    # mutable location shared between threads
    keep_running = {"value": True}

    # 1. Start first PostgREST instance
    # 2. Start a "client" thread issuing requests in a loop
    #    remembering all received errors
    # 3. Start second PostgREST instance on the same port as the first one
    # 4. Wait a little and terminate the first instance
    #
    # We expect the client does not get any errors after stopping the first instance
    # and seamlessly migrate to the second instance.
    #
    # 5. Stop client thread
    # 6. Stop second PostgREST instance
    # 7. Verify client did not get any errors
    with run(
        env={**defaultenv, "PGRST_SERVER_REUSEPORT": "true"},
        port=port,
        host=host,
        admin_port=admin_port,
    ) as first:

        def continuously_request():
            while keep_running["value"]:
                try:
                    response = first.session.get("/projects", timeout=1)
                    assert response.status_code == 200
                except Exception as exc:
                    failures.append(exc)
                    break
                time.sleep(0.2)

        requester = Thread(target=continuously_request)
        requester.start()

        try:
            time.sleep(1)
            with run(
                env={**defaultenv, "PGRST_SERVER_REUSEPORT": "true"},
                port=port,
                host=host,
                # we do not set SO_REUSEPORT on admin socket
                admin_port=freeport(used_ports=[port, admin_port]),
            ):
                time.sleep(1)
                first.process.terminate()
                wait_until_exit(first, 2)

                time.sleep(1)
        finally:
            keep_running["value"] = False
            requester.join()

    assert failures == []


def test_so_reuseport_defaults_to_false(defaultenv):
    "A second PostgREST instance should not bind to the same port by default."

    host = "0.0.0.0"
    port = freeport()
    admin_port = freeport(used_ports=[port])

    with run(
        env={**defaultenv},
        port=port,
        host=host,
        admin_port=admin_port,
    ):
        with pytest.raises(PostgrestTimedOut):
            with run(
                env={**defaultenv},
                port=port,
                host=host,
                admin_port=freeport(used_ports=[port, admin_port]),
                wait_max_seconds=1,
            ):
                pass


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


def test_allow_configs_to_be_set_to_empty(defaultenv):
    'configs that are explicitly set to empty (= "<empty>") should not throw parse error'

    env = {
        **defaultenv,
        "PGRST_DB_EXTRA_SEARCH_PATH": "",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get("/projects")
        assert response.status_code == 200


def test_connection_error_message_does_not_claim_retry(defaultenv):
    "The connection error message should not claim retrying, since PostgREST stops on fatal errors."
    uri = f'postgresql://?dbname={defaultenv["PGDATABASE"]}&host={defaultenv["PGHOST"]}&user=some_protected_user&password=invalid_pass'
    env = {**defaultenv, "PGRST_DB_URI": uri}
    with run(env=env, no_startup_stdout=False, wait_for=None) as postgrest:
        output = postgrest.read_stdout(nlines=8)
        assert any('"message":"Database connection error."' in line for line in output)


def test_db_pre_config_with_non_existent_function(defaultenv):
    "Log error when db-pre-config is set to non-existent function"

    env = {
        **defaultenv,
        "PGRST_DB_PRE_CONFIG": "select",  # no "select" function in our fixtures, fail gracefully at startup
    }

    with run(env=env, no_startup_stdout=False, wait_for=None) as postgrest:
        output = postgrest.read_stdout(nlines=8)
        assert any("function select() does not exist" in line for line in output)


@pytest.mark.parametrize("enabled", ["true", "false"])
def test_use_legacy_target_names(enabled, defaultenv):
    "Show a warning when a target name is used instead of an alias, only when config is enabled"

    env = {
        **defaultenv,
        "PGRST_URL_USE_LEGACY_TARGET_NAMES": enabled,
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get(
            "/directors?select=name,all_films:films(title),awards_2026:awards(name)&films.order=title&awards.year=eq.2026"
        )

        output = postgrest.read_stdout(nlines=10)

        log_err_warning = "WARNING: Embedded resource was referenced by relation name even though it has an alias. This is deprecated and will stop working in a future release."
        log_err_hint = "Update filters, orders or limits that use `films` to `all_films`, `awards` to `awards_2026` in `GET /directors?select=name,all_films:films(title),awards_2026:awards(name)&films.order=title&awards.year=eq.2026`"

        has_warning_log = any(log_err_warning in line for line in output)
        has_hint_log = any(log_err_hint in line for line in output)

        if enabled == "true":
            assert response.status_code == 200
            assert has_warning_log and has_hint_log
        else:
            assert response.status_code == 400
            assert not has_warning_log and not has_hint_log
