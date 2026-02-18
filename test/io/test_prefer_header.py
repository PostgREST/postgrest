"Prefer: timeout header tests for PostgREST (involves IO)"

from config import SECRET
from util import jwtauthheader, execute_sql_statement_using_superuser
from postgrest import (
    run,
    sleep_until_postgrest_scache_reload,
    sleep_until_postgrest_config_reload,
)


def test_prefer_timeout_header_with_strict_handling(defaultenv):
    "Test Prefer: timeout header with handling=strict"

    with run(env=defaultenv) as postgrest:
        # Fails when hits timeout
        headers = {"Prefer": "handling=strict, timeout=1"}
        response = postgrest.session.get("/rpc/sleep?seconds=2", headers=headers)
        assert response.status_code == 500
        assert response.json() == {
            "code": "57014",
            "message": "canceling statement due to statement timeout",
            "details": None,
            "hint": None,
        }

        # Fails when sleep equals the timeout
        headers = {"Prefer": "handling=strict, timeout=2"}
        response = postgrest.session.get("/rpc/sleep?seconds=2", headers=headers)
        assert response.status_code == 500
        assert response.json() == {
            "code": "57014",
            "message": "canceling statement due to statement timeout",
            "details": None,
            "hint": None,
        }

        # Succeeds when timeout is not hit
        headers = {"Prefer": "handling=strict, timeout=2"}
        response = postgrest.session.get("/rpc/sleep?seconds=1", headers=headers)
        assert response.status_code == 204
        assert response.headers["Preference-Applied"] == "handling=strict, timeout=2"


def test_prefer_timeout_header_with_lenient_handling(defaultenv):
    "Test Prefer: timeout header with handling=lenient"

    with run(env=defaultenv) as postgrest:
        # Timeout header is ignored for lenient handling
        headers = {"Prefer": "handling=lenient, timeout=1"}
        response = postgrest.session.get("/rpc/sleep?seconds=2", headers=headers)

        assert response.status_code == 204
        assert response.headers["Preference-Applied"] == "handling=lenient"


def test_prefer_timeout_header_with_role_timeout(defaultenv):
    "Test Prefer: timeout header with role timeout"

    env = {
        **defaultenv,
        "PGRST_JWT_SECRET": SECRET,
    }

    with run(env=env) as postgrest:
        # should fail when timeout is more than role's statement timeout
        authheader = jwtauthheader({"role": "postgrest_test_timeout_ms"}, SECRET)
        headers = {
            **authheader,
            "Prefer": "handling=strict, timeout=5",  # role timeout is 10ms, so this should fail
        }

        response = postgrest.session.get("/rpc/sleep?seconds=2", headers=headers)
        assert response.status_code == 400
        assert response.json() == {
            "code": "PGRST129",
            "message": "Timeout preference exceeded maximum allowed",
            "details": "The maximum timeout allowed is 0s",
            "hint": "Reduce the timeout",
        }

        # should fail when timeout is more than role's statement timeout
        authheader = jwtauthheader({"role": "postgrest_test_timeout_s"}, SECRET)
        headers = {
            **authheader,
            "Prefer": "handling=strict, timeout=15",  # role timeout is 10s, so this should fail
        }

        response = postgrest.session.get("/rpc/sleep?seconds=2", headers=headers)
        assert response.json() == {
            "code": "PGRST129",
            "message": "Timeout preference exceeded maximum allowed",
            "details": "The maximum timeout allowed is 10s",
            "hint": "Reduce the timeout",
        }
        assert response.status_code == 400

        # should succeed when timeout is less than role's statement timeout
        authheader = jwtauthheader({"role": "postgrest_test_timeout_s"}, SECRET)
        headers = {
            **authheader,
            "Prefer": "handling=strict, timeout=5",  # role timeout is 10s, so this should succeed
        }

        response = postgrest.session.get("/rpc/sleep?seconds=2", headers=headers)
        assert response.status_code == 204
        assert response.headers["Preference-Applied"] == "handling=strict, timeout=5"


def test_prefer_timeout_header_with_global_timeout(defaultenv):
    "Test Prefer: timeout header with database or cluster level timeout"

    env = {**defaultenv, "PGRST_JWT_SECRET": SECRET}

    with run(env=env) as postgrest:
        # set global statement_timeout to 3s using postgres superuser
        execute_sql_statement_using_superuser(
            env, "ALTER DATABASE postgres SET statement_timeout TO '3s';"
        )

        # reload schema and config
        response = postgrest.session.post("/rpc/reload_pgrst_schema")
        assert response.status_code == 204
        sleep_until_postgrest_scache_reload()

        response = postgrest.session.post("/rpc/reload_pgrst_config")
        assert response.status_code == 204
        sleep_until_postgrest_config_reload()

        # should fail when timeout is more than global statement timeout
        authheader = jwtauthheader({"role": "postgrest_test_no_timeout"}, SECRET)
        headers = {
            **authheader,
            "Prefer": "handling=strict, timeout=5",  # global timeout is 3s, so this should fail
        }
        response = postgrest.session.get("/rpc/sleep?seconds=4", headers=headers)
        assert response.json() == {
            "code": "PGRST129",
            "message": "Timeout preference exceeded maximum allowed",
            "details": "The maximum timeout allowed is 3s",
            "hint": "Reduce the timeout",
        }
        assert response.status_code == 400

        # reset global statement_timeout using postgres superuser
        execute_sql_statement_using_superuser(
            env, "ALTER DATABASE postgres RESET statement_timeout;"
        )


def test_prefer_timeout_header_with_negative_timeout(defaultenv):
    "Test Prefer: timeout header with negative and zero timeout"

    with run(env=defaultenv) as postgrest:
        # -ve timeout values are ignored
        headers = {"Prefer": "handling=strict, timeout=-1"}
        response = postgrest.session.get("/rpc/sleep?seconds=2", headers=headers)
        assert response.status_code == 204

        # 0 timeout value is ignored
        headers = {"Prefer": "handling=stricte timeout=0"}
        response = postgrest.session.get("/rpc/sleep?seconds=2", headers=headers)
        assert response.status_code == 204
