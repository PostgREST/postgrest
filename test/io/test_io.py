"Unit tests for Input/Ouput of PostgREST seen as a black box."

from datetime import datetime, timedelta, timezone
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


def test_secret_min_length(defaultenv):
    "Should log error and not load the config when the secret is shorter than the minimum admitted length"

    env = {**defaultenv, "PGRST_JWT_SECRET": "short_secret"}

    with run(env=env, no_startup_stdout=False, wait_for_readiness=False) as postgrest:
        exitCode = wait_until_exit(postgrest)
        assert exitCode == 1

        output = postgrest.read_stdout(nlines=1)
        assert "The JWT secret must be at least 32 characters long." in output[0]


def test_jwt_errors(defaultenv):
    "invalid JWT should throw error"

    env = {**defaultenv, "PGRST_JWT_SECRET": SECRET, "PGRST_JWT_AUD": "io tests"}

    relativeSeconds = lambda sec: int(
        (datetime.now(timezone.utc) + timedelta(seconds=sec)).timestamp()
    )

    with run(env=env) as postgrest:
        headers = jwtauthheader({}, "other secret")
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert response.json()["message"] == "JWSError JWSInvalidSignature"

        headers = jwtauthheader({"role": "not_existing"}, SECRET)
        response = postgrest.session.get("/", headers=headers)
        # TODO: Should this return 401?
        assert response.status_code == 400
        assert response.json()["message"] == 'role "not_existing" does not exist'

        # -31 seconds, because we allow clock skew of 30 seconds
        headers = jwtauthheader({"exp": relativeSeconds(-31)}, SECRET)
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert response.json()["message"] == "JWT expired"

        # 31 seconds, because we allow clock skew of 30 seconds
        headers = jwtauthheader({"nbf": relativeSeconds(31)}, SECRET)
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert response.json()["message"] == "JWTNotYetValid"

        # 31 seconds, because we allow clock skew of 30 seconds
        headers = jwtauthheader({"iat": relativeSeconds(31)}, SECRET)
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert response.json()["message"] == "JWTIssuedAtFuture"

        headers = jwtauthheader({"aud": "not set"}, SECRET)
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert response.json()["message"] == "JWTNotInAudience"

        # partial token, no signature
        headers = authheader("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.bm90IGFuIG9iamVjdA")
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert (
            response.json()["message"]
            == "JWSError (CompactDecodeError Invalid number of parts: Expected 3 parts; got 2)"
        )

        # token with algorithm "none"
        headers = authheader(
            "eyJ0eXAiOiJKV1QiLCJhbGciOiJub25lIn0.e30.yOBhlOIqn56T-4NvyEXCjfi3UmyQZ-BzXtePMO2NgRI"
        )
        response = postgrest.session.get("/", headers=headers)
        assert response.status_code == 401
        assert response.json()["message"] == "JWSError JWSNoSignatures"


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

    claim = {"role": "postgrest_test_author", "iat": datetime.now(timezone.utc)}
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

    with run(env=defaultenv, port="0") as postgrest:
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

        sleep_until_postgrest_config_reload()

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
        sleep_until_postgrest_scache_reload()

        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 401

        # reload config and external file with SIGUSR2
        postgrest.process.send_signal(signal.SIGUSR2)
        sleep_until_postgrest_config_reload()

        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 200

        # change external file to wrong value again
        external_secret_file.write_text("invalid" * 5)

        # reload config and external file with NOTIFY
        response = postgrest.session.post("/rpc/reload_pgrst_config")
        assert response.text == ""
        assert response.status_code == 204
        sleep_until_postgrest_config_reload()

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


def set_statement_timeout(postgrest, role, milliseconds):
    """Set the statement timeout for the given role.
    For this to work reliably with low previous timeout settings,
    use a postgrest instance that doesn't use the affected role."""

    response = postgrest.session.post(
        "/rpc/set_statement_timeout", data={"role": role, "milliseconds": milliseconds}
    )
    assert response.text == ""
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
        output = sorted(postgrest.read_stdout(nlines=3))

        if level == "crit":
            assert len(output) == 0
        else:
            assert " 504 " in output[0]
            assert "Timed out acquiring connection from connection pool." in output[2]


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


def test_admin_config(defaultenv):
    "Should get a success response from the admin server containing current configuration"

    with run(env=defaultenv) as postgrest:
        response = postgrest.admin.get("/config")
        print(response.text)
        assert response.status_code == 200
        assert "admin-server-port" in response.text


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
        "PGRST_INTERNAL_SCHEMA_CACHE_SLEEP": "500",
    }

    with run(env=env) as postgrest:
        # The schema cache query takes at least 500ms, due to PGRST_INTERNAL_SCHEMA_CACHE_SLEEP above.
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
        "PGRST_INTERNAL_SCHEMA_CACHE_SLEEP": "50",
    }

    with run(env=env) as postgrest:
        # The schema cache query takes at least 20ms, due to PGRST_INTERNAL_SCHEMA_CACHE_SLEEP above.
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
                r'- - - \[.+\] "GET / HTTP/1.1" 500 - "" "python-requests/.+"',
                output[0],
            )
            assert len(output) == 1
        elif level == "warn":
            assert re.match(
                r'- - - \[.+\] "GET / HTTP/1.1" 500 - "" "python-requests/.+"',
                output[0],
            )
            assert re.match(
                r'- - postgrest_test_anonymous \[.+\] "GET /unknown HTTP/1.1" 404 - "" "python-requests/.+"',
                output[1],
            )
            assert len(output) == 2
        elif level == "info":
            assert re.match(
                r'- - - \[.+\] "GET / HTTP/1.1" 500 - "" "python-requests/.+"',
                output[0],
            )
            assert re.match(
                r'- - postgrest_test_anonymous \[.+\] "GET / HTTP/1.1" 200 - "" "python-requests/.+"',
                output[1],
            )
            assert re.match(
                r'- - postgrest_test_anonymous \[.+\] "GET /unknown HTTP/1.1" 404 - "" "python-requests/.+"',
                output[2],
            )
            assert len(output) == 3
        elif level == "debug":
            assert re.match(
                r'- - - \[.+\] "GET / HTTP/1.1" 500 - "" "python-requests/.+"',
                output[0],
            )
            assert re.match(
                r'- - postgrest_test_anonymous \[.+\] "GET / HTTP/1.1" 200 - "" "python-requests/.+"',
                output[1],
            )
            assert re.match(
                r'- - postgrest_test_anonymous \[.+\] "GET /unknown HTTP/1.1" 404 - "" "python-requests/.+"',
                output[2],
            )

            assert len(output) == 5
            assert "Connection" and "is available" in output[3]
            assert "Connection" and "is used" in output[4]


@pytest.mark.parametrize("level", ["crit", "error", "warn", "info", "debug"])
def test_log_query(level, defaultenv):
    "log_query=true should log the SQL query according to the log_level"

    env = {
        **defaultenv,
        "PGRST_LOG_LEVEL": level,
        "PGRST_LOG_QUERY": "main-query",
        # The root path can only log SQL when a function is set in db-root-spec
        "PGRST_DB_ROOT_SPEC": "root",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.get("/")
        assert response.status_code == 200

        response = postgrest.session.get("/projects")
        assert response.status_code == 200

        response = postgrest.session.get("/infinite_recursion")
        assert response.status_code == 500

        root_2xx_regx = r'.+: WITH pgrst_source AS.+SELECT "public"\."root"\(\) pgrst_scalar.+_postgrest_t'
        get_2xx_regx = r'.+: WITH pgrst_source AS.+SELECT "public"\."projects"\.\* FROM "public"\."projects".+_postgrest_t'
        infinite_recursion_5xx_regx = r'.+: WITH pgrst_source AS.+SELECT "public"\."infinite_recursion"\.\* FROM "public"\."infinite_recursion".+_postgrest_t'

        if level == "crit":
            output = postgrest.read_stdout(nlines=1)
            assert len(output) == 0
        elif level == "error":
            output = postgrest.read_stdout(nlines=4)
            assert re.match(infinite_recursion_5xx_regx, output[1])
            assert len(output) == 3
        elif level == "warn":
            output = postgrest.read_stdout(nlines=2)
            assert re.match(infinite_recursion_5xx_regx, output[1])
            assert len(output) == 2
        elif level == "info":
            output = postgrest.read_stdout(nlines=6)
            assert re.match(root_2xx_regx, output[0])
            assert re.match(get_2xx_regx, output[2])
            assert re.match(infinite_recursion_5xx_regx, output[5])
            assert len(output) == 6
        elif level == "debug":
            output_ok = postgrest.read_stdout(nlines=8)
            assert re.match(root_2xx_regx, output_ok[2])
            assert re.match(get_2xx_regx, output_ok[6])
            assert len(output_ok) == 8
            output_err = postgrest.read_stdout(nlines=4)
            assert re.match(infinite_recursion_5xx_regx, output_err[3])
            assert len(output_err) == 4


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
        int(slow_schema_cache_env["PGRST_INTERNAL_SCHEMA_CACHE_SLEEP"]) / 1000
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

    env = {**defaultenv, "PGRST_LOG_LEVEL": "crit"}

    with run(env=defaultenv, no_startup_stdout=False) as postgrest:
        version = postgrest.session.head("/").headers["Server"].split("/")[1]

        output = postgrest.read_stdout(nlines=1)

        assert "Starting PostgREST %s..." % version in output[0]


def test_log_postgrest_host_and_port(defaultenv):
    "PostgREST should output the host and port it is bound to."
    host = "127.0.0.1"
    port = freeport()

    with run(
        env=defaultenv, host=host, port=port, no_startup_stdout=False
    ) as postgrest:
        output = postgrest.read_stdout(nlines=10)

        assert f"API server listening on {host}:{port}" in output[2]  # output-sensitive


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


def test_jwt_cache_server_timing(defaultenv):
    "server-timing duration is exposed for JWT with expiry"

    env = {
        **defaultenv,
        "PGRST_SERVER_TIMING_ENABLED": "true",
        "PGRST_JWT_CACHE_MAX_LIFETIME": "86400",
        "PGRST_JWT_SECRET": SECRET,
        "PGRST_DB_CONFIG": "false",
    }

    headers = jwtauthheader(
        {
            "role": "postgrest_test_author",
            "exp": int(
                (datetime.now(timezone.utc) + timedelta(minutes=30)).timestamp()
            ),
        },
        SECRET,
    )

    with run(env=env) as postgrest:
        first = postgrest.session.get("/authors_only", headers=headers)
        second = postgrest.session.get("/authors_only", headers=headers)

        assert first.status_code == 200
        assert second.status_code == 200

        first_dur = parse_server_timings_header(first.headers["Server-Timing"])["jwt"]
        second_dur = parse_server_timings_header(second.headers["Server-Timing"])["jwt"]

        assert first_dur >= 0
        assert second_dur >= 0


def test_jwt_cache_without_server_timing(defaultenv):
    "JWT cache does not break requests without server-timing enabled"

    env = {
        **defaultenv,
        "PGRST_SERVER_TIMING_ENABLED": "true",
        "PGRST_JWT_CACHE_MAX_LIFETIME": "86400",
        "PGRST_JWT_SECRET": SECRET,
        "PGRST_DB_CONFIG": "false",
    }

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    with run(env=env) as postgrest:
        first = postgrest.session.get("/authors_only", headers=headers)
        second = postgrest.session.get("/authors_only", headers=headers)

        assert first.status_code == 200
        assert second.status_code == 200


def test_jwt_cache_without_exp_claim(defaultenv):
    "server-timing duration is exposed for JWT without expiry"

    env = {
        **defaultenv,
        "PGRST_SERVER_TIMING_ENABLED": "true",
        "PGRST_JWT_CACHE_MAX_LIFETIME": "86400",
        "PGRST_JWT_SECRET": SECRET,
        "PGRST_DB_CONFIG": "false",
    }

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)  # no exp

    with run(env=env) as postgrest:
        first = postgrest.session.get("/authors_only", headers=headers)
        second = postgrest.session.get("/authors_only", headers=headers)

        assert first.status_code == 200
        assert second.status_code == 200

        first_dur = parse_server_timings_header(first.headers["Server-Timing"])["jwt"]
        second_dur = parse_server_timings_header(second.headers["Server-Timing"])["jwt"]

        assert first_dur >= 0
        assert second_dur >= 0


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
        output = sorted(postgrest.read_stdout(nlines=4))

        if level == "crit":
            assert len(output) == 0
        elif level == "debug":
            assert " 500 " in output[0]
            assert "canceling statement due to statement timeout" in output[3]
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


def test_jwt_cache_purges_expired_entries(defaultenv):
    "test expired cache entries are purged on cache miss"

    # The verification of actual cache size reduction is done manually, see https://github.com/PostgREST/postgrest/pull/3801#issuecomment-2620776041
    # This test is written for code coverage of purgeExpired function

    relativeSeconds = lambda sec: int(
        (datetime.now(timezone.utc) + timedelta(seconds=sec)).timestamp()
    )

    headers = lambda sec: jwtauthheader(
        {"role": "postgrest_test_author", "exp": relativeSeconds(sec)},
        SECRET,
    )

    env = {
        **defaultenv,
        "PGRST_JWT_CACHE_MAX_LIFETIME": "86400",
        "PGRST_JWT_SECRET": SECRET,
        "PGRST_DB_CONFIG": "false",
    }

    with run(env=env) as postgrest:

        # Generate two unique JWT tokens
        # The 1 second sleep is needed for it generate a unique token
        hdrs1 = headers(5)
        postgrest.session.get("/authors_only", headers=hdrs1)

        time.sleep(1)

        hdrs2 = headers(5)
        postgrest.session.get("/authors_only", headers=hdrs2)

        # Wait 5 seconds for the tokens to expire
        time.sleep(5)

        hdrs3 = headers(5)

        # Make another request which should cause a cache miss and so
        # the purgeExpired function will be triggered.
        #
        # This should remove the 2 expired tokens but adds another to cache
        response = postgrest.session.get("/authors_only", headers=hdrs3)

        assert response.status_code == 200


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
