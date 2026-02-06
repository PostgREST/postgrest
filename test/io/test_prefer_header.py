"Prefer: timeout header tests for PostgREST (involves IO)"

from config import SECRET
from util import jwtauthheader
from postgrest import run


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
        headers = {"Prefer": "handling=lenient, timeout=2"}
        response = postgrest.session.get("/rpc/sleep?seconds=1", headers=headers)

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
            "message": "Timeout preference value cannot exceed statement_timeout of role",
            "details": "Timeout preferred: 5s, statement_timeout of role 'postgrest_test_timeout_ms': 10ms",
            "hint": None,
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
            "message": "Timeout preference value cannot exceed statement_timeout of role",
            "details": "Timeout preferred: 15s, statement_timeout of role 'postgrest_test_timeout_s': 10s",
            "hint": None,
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
