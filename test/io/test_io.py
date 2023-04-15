"Unit tests for Input/Ouput of PostgREST seen as a black box."

from datetime import datetime
from operator import attrgetter
import os
import re
import signal
import socket
import time

import pytest

from config import *
from util import *
from postgrest import *


@pytest.mark.parametrize(
    "secretpath",
    [path for path in (BASEDIR / "secrets").iterdir() if path.suffix != ".jwt"],
    ids=attrgetter("name"),
)
def test_read_secret_from_file(secretpath, defaultenv):
    "Authorization should succeed when the secret is read from a file."

    env = {**defaultenv, "PGRST_JWT_SECRET": f"@{secretpath}"}

    if secretpath.suffix == ".b64":
        env["PGRST_JWT_SECRET_IS_BASE64"] = "true"

    secret = secretpath.read_bytes()
    headers = authheader(secretpath.with_suffix(".jwt").read_text())

    with run(stdin=secret, env=env) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        print(response.text)
        assert response.status_code == 200


def test_read_secret_from_stdin(defaultenv):
    "Authorization should succeed when the secret is read from stdin."

    env = {**defaultenv, "PGRST_DB_CONFIG": "false", "PGRST_JWT_SECRET": "@/dev/stdin"}

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    with run(stdin=SECRET.encode(), env=env) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        print(response.text)
        assert response.status_code == 200


# TODO: This test would fail right now, because of
# https://github.com/PostgREST/postgrest/issues/2126
@pytest.mark.skip
def test_read_secret_from_stdin_dbconfig(defaultenv):
    "Authorization should succeed when the secret is read from stdin with db-config=true."

    env = {**defaultenv, "PGRST_DB_CONFIG": "true", "PGRST_JWT_SECRET": "@/dev/stdin"}

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    with run(stdin=SECRET.encode(), env=env) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        print(response.text)
        assert response.status_code == 200


def test_fail_with_invalid_password(defaultenv):
    "Connecting with an invalid password should fail without retries."
    uri = f'postgresql://?dbname={defaultenv["PGDATABASE"]}&host={defaultenv["PGHOST"]}&user=some_protected_user&password=invalid_pass'
    env = {**defaultenv, "PGRST_DB_URI": uri}
    with run(env=env, wait_for_readiness=False) as postgrest:
        exitCode = wait_until_exit(postgrest)
        assert exitCode == 1


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


@pytest.mark.parametrize(
    "roleclaim", FIXTURES["roleclaims"], ids=lambda claim: claim["key"]
)
def test_role_claim_key(roleclaim, defaultenv):
    "Authorization should depend on a correct role-claim-key and JWT claim."
    env = {
        **defaultenv,
        "PGRST_JWT_ROLE_CLAIM_KEY": roleclaim["key"],
        "PGRST_JWT_SECRET": SECRET,
    }
    headers = jwtauthheader(roleclaim["data"], SECRET)

    with run(env=env) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == roleclaim["expected_status"]


def test_iat_claim(defaultenv):
    """
    A claim with an 'iat' (issued at) attribute should be successful.

    The PostgREST time cache leads to issues here, see:
    https://github.com/PostgREST/postgrest/issues/1139

    """

    env = {**defaultenv, "PGRST_JWT_SECRET": SECRET}

    claim = {"role": "postgrest_test_author", "iat": datetime.utcnow()}
    headers = jwtauthheader(claim, SECRET)

    with run(env=env) as postgrest:
        for _ in range(10):
            response = postgrest.session.get("/authors_only", headers=headers)
            assert response.status_code == 200

            time.sleep(0.1)


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
        time.sleep(0.1)

        uri = "/rpc/get_guc_value?name=app.settings.external_api_secret"
        response = postgrest.session.get(uri)
        assert response.text == '"0123456789abcdef"'


def test_flush_pool_no_interrupt(defaultenv):
    "Flushing the pool via SIGUSR1 doesn't interrupt ongoing requests"

    with run(env=defaultenv) as postgrest:

        def sleep():
            response = postgrest.session.get("/rpc/sleep?seconds=0.5")
            assert response.status_code == 204

        t = Thread(target=sleep)
        t.start()

        # make sure the request has started
        time.sleep(0.1)

        # SIGUSR1 causes the postgres connection pool to be flushed
        postgrest.process.send_signal(signal.SIGUSR1)

        t.join()


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

        time.sleep(0.1)

        response = postgrest.session.get(uri)
        assert response.text == '"Jane"'


def test_jwt_secret_reload(tmp_path, defaultenv):
    "JWT secret should be reloaded from file when PostgREST is sent SIGUSR2."
    config = (CONFIGSDIR / "sigusr2-settings.config").read_text()
    configfile = tmp_path / "test.config"
    configfile.write_text(config)

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    with run(configfile, env=defaultenv) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 401

        # change setting
        configfile.write_text(config.replace("invalid" * 5, SECRET))

        # reload config
        postgrest.process.send_signal(signal.SIGUSR2)

        time.sleep(0.1)

        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 200


def test_jwt_secret_external_file_reload(tmp_path, defaultenv):
    "JWT secret external file should be reloaded when PostgREST is sent a SIGUSR2 or a NOTIFY."
    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    external_secret_file = tmp_path / "jwt-secret-config"
    external_secret_file.write_text("invalid" * 5)

    env = {
        **defaultenv,
        "PGRST_JWT_SECRET": f"@{external_secret_file}",
        "PGRST_DB_CHANNEL_ENABLED": "true",
        "PGRST_DB_CONFIG": "false",
        "PGRST_DB_ANON_ROLE": "postgrest_test_anonymous",  # required for NOTIFY
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 401

        # change external file
        external_secret_file.write_text(SECRET)

        # SIGUSR1 doesn't reload external files, at least when db-config=false
        postgrest.process.send_signal(signal.SIGUSR1)
        time.sleep(0.1)

        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 401

        # reload config and external file with SIGUSR2
        postgrest.process.send_signal(signal.SIGUSR2)
        time.sleep(0.1)

        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 200

        # change external file to wrong value again
        external_secret_file.write_text("invalid" * 5)

        # reload config and external file with NOTIFY
        response = postgrest.session.post("/rpc/reload_pgrst_config")
        assert response.status_code == 204
        time.sleep(0.1)

        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 401


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

        # reload schema cache to verify that the config reload actually happened
        postgrest.process.send_signal(signal.SIGUSR1)

        # takes max 1 second to load the internal cache(big_schema.sql included now)
        # TODO this could go back to time.sleep(0.1) if the big_schema is put in another test suite
        time.sleep(1)

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

        time.sleep(0.2)

        response = postgrest.session.get("/rpc/get_guc_value?name=search_path")
        assert response.text == '"\\"v1\\", \\"public\\""'

        # reset db-schemas config on the db
        response = postgrest.session.post("/rpc/reset_db_schema_config")
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

        time.sleep(0.1)

        response = postgrest.session.head("/projects")
        assert response.status_code == 200
        assert response.headers["Content-Range"] == "0-0/*"

        # reset max-rows config on the db
        response = postgrest.session.post("/rpc/reset_max_rows_config")
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

        time.sleep(0.1)

        response = postgrest.session.head("/projects")
        assert response.status_code == 200
        assert response.headers["Content-Range"] == "0-0/*"

        # reset max-rows config on the db
        response = postgrest.session.post("/rpc/reset_max_rows_config")
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
        assert "failed to parse role-claim-key value" in output[0]

        response = postgrest.session.post("/rpc/reset_invalid_role_claim_key")
        assert response.status_code == 204


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


def set_statement_timeout(postgrest, role, milliseconds):
    """Set the statement timeout for the given role.
    For this to work reliably with low previous timeout settings,
    use a postgrest instance that doesn't use the affected role."""

    response = postgrest.session.post(
        "/rpc/set_statement_timeout", data={"role": role, "milliseconds": milliseconds}
    )
    assert response.status_code == 204


def reset_statement_timeout(postgrest, role):
    "Reset the statement timeout for the given role to the default 0 (no timeout)"
    set_statement_timeout(postgrest, role, 0)


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
        assert response.status_code == 204

        response = postgrest.session.get("/rpc/sleep?seconds=2")
        assert response.status_code == 500
        data = response.json()
        assert data["message"] == "canceling statement due to statement timeout"


def test_change_statement_timeout(defaultenv, metapostgrest):
    "Statement timeout changes take effect immediately"

    role = "timeout_authenticator"
    reset_statement_timeout(metapostgrest, role)

    env = {
        **defaultenv,
        "PGUSER": role,
        "PGRST_DB_ANON_ROLE": role,
    }

    with run(env=env) as postgrest:
        # no limit initially
        response = postgrest.session.get("/rpc/sleep?seconds=1")
        assert response.status_code == 204

        set_statement_timeout(metapostgrest, role, 500)  # 0.5s

        # trigger schema refresh
        postgrest.process.send_signal(signal.SIGUSR1)
        time.sleep(0.1)

        response = postgrest.session.get("/rpc/sleep?seconds=1")
        assert response.status_code == 500
        data = response.json()
        assert data["message"] == "canceling statement due to statement timeout"

        set_statement_timeout(metapostgrest, role, 2000)  # 2s

        # trigger role setting refresh
        postgrest.process.send_signal(signal.SIGUSR1)
        time.sleep(0.1)

        response = postgrest.session.get("/rpc/sleep?seconds=1")
        assert response.status_code == 204


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


def test_pool_acquisition_timeout(defaultenv, metapostgrest):
    "Verify that PGRST_DB_POOL_ACQUISITON_TIMEOUT times out when the pool is empty"

    env = {
        **defaultenv,
        "PGRST_DB_POOL": "1",
        "PGRST_DB_POOL_ACQUISITION_TIMEOUT": "1",  # 1 second
    }

    with run(env=env, no_pool_connection_available=True) as postgrest:
        response = postgrest.session.get("/projects")
        assert response.status_code == 504
        data = response.json()
        assert data["message"] == "Timed out acquiring connection from connection pool."

        # ensure the message appears on the logs as well
        output = sorted(postgrest.read_stdout(nlines=2))
        assert " 504 " in output[0]
        assert "Timed out acquiring connection from connection pool." in output[1]


def test_change_statement_timeout_held_connection(defaultenv, metapostgrest):
    "Statement timeout changes take effect immediately, even with a request outliving the reconfiguration"

    role = "timeout_authenticator"
    reset_statement_timeout(metapostgrest, role)

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
    reset_statement_timeout(metapostgrest, role)

    env = {
        **defaultenv,
        "PGUSER": role,
        "PGRST_DB_ANON_ROLE": role,
    }

    with run(env=env) as postgrest:
        # make it impossible to load the schema cache, by setting statement timeout to 1ms
        set_statement_timeout(metapostgrest, role, 1)

        # force a reconnection so the new role setting is picked up
        postgrest.process.send_signal(signal.SIGUSR1)
        time.sleep(0.1)

        response = postgrest.admin.get("/ready")
        assert response.status_code == 503

        response = postgrest.session.get("/projects")
        assert response.status_code == 503


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
        assert response.status_code == 503


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
        assert response.status_code == 503


@pytest.mark.parametrize("specialhostvalue", FIXTURES["specialhostvalues"])
def test_admin_works_with_host_special_values(specialhostvalue, defaultenv):
    "Should get a success from the admin live and ready endpoints when using special host values for the main app"

    with run(env=defaultenv, port=freeport(), host=specialhostvalue) as postgrest:
        response = postgrest.admin.get("/live")
        assert response.status_code == 200

        response = postgrest.admin.get("/ready")
        assert response.status_code == 200


@pytest.mark.parametrize(
    "level, has_output",
    [
        ("info", [True, True, True]),
        ("warn", [False, True, True]),
        ("error", [False, False, True]),
        ("crit", [False, False, False]),
    ],
)
def test_log_level(level, has_output, defaultenv):
    "log_level should filter request logging"

    env = {**defaultenv, "PGRST_LOG_LEVEL": level}

    # expired token to test 500 response for "JWT expired"
    claim = {"role": "postgrest_test_author", "exp": 0}
    headers = jwtauthheader(claim, SECRET)

    with run(env=env) as postgrest:
        response = postgrest.session.get("/")
        assert response.status_code == 200
        if has_output[0]:
            assert re.match(
                r'- - postgrest_test_anonymous \[.+\] "GET / HTTP/1.1" 200 - "" "python-requests/.+"',
                postgrest.process.stdout.readline().decode(),
            )

        response = postgrest.session.get("/unknown")
        assert response.status_code == 404
        if has_output[1]:
            assert re.match(
                r'- - postgrest_test_anonymous \[.+\] "GET /unknown HTTP/1.1" 404 - "" "python-requests/.+"',
                postgrest.process.stdout.readline().decode(),
            )

        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 500
        if has_output[2]:
            assert re.match(
                r'- - - \[.+\] "GET / HTTP/1.1" 500 - "" "python-requests/.+"',
                postgrest.process.stdout.readline().decode(),
            )


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
        assert response.status_code == 204
        time.sleep(0.1)

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
        assert response.status_code == 204

        response = postgrest.session.get("/rpc/reload_pgrst_config")
        assert response.status_code == 204
        time.sleep(0.1)

        response = postgrest.session.get("/rpc/get_guc_value?name=statement_timeout")
        assert response.text == '"5s"'

        # statement_timeout for postgrest_test_author
        headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)
        response = postgrest.session.get(
            "/rpc/get_guc_value?name=statement_timeout", headers=headers
        )
        assert response.text == '"10s"'


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


# TODO: This test fails now because of https://github.com/PostgREST/postgrest/pull/2122
# The stack size of 1K(-with-rtsopts=-K1K) is not enough and this fails with "stack overflow"
# A stack size of 200K seems to be enough for succeess
@pytest.mark.skip
def test_openapi_in_big_schema(defaultenv):
    "Should get a successful response from openapi on a big schema"

    env = {
        **defaultenv,
        "PGRST_DB_SCHEMAS": "apflora",
        "PGRST_OPENAPI_MODE": "ignore-privileges",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get("/")
        assert response.status_code == 200
