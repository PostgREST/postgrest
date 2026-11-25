"Server notices (RAISE WARNING) surfaced in error responses. See issue #1071."

import pytest

from postgrest import run


@pytest.fixture
def warnings_env(defaultenv):
    "Environment with db-warnings-enabled turned on."
    return {**defaultenv, "PGRST_DB_WARNINGS_ENABLED": "true"}


def test_warnings_attached_to_error(warnings_env):
    "Warnings raised before an exception are included in the error body."
    with run(env=warnings_env) as postgrest:
        response = postgrest.session.post("/rpc/raise_warnings_then_exception")

        assert response.status_code == 400
        body = response.json()
        assert body["message"] == "the real error"
        assert body["warnings"] == [
            {
                "severity": "WARNING",
                "code": "01000",
                "message": "first warning",
                "details": None,
                "hint": None,
            },
            {
                "severity": "WARNING",
                "code": "XX999",
                "message": "second warning",
                "details": "extra detail",
                "hint": "a hint",
            },
        ]


def test_no_warnings_on_success(warnings_env):
    "A successful request that raised a warning carries an empty body (204)."
    with run(env=warnings_env) as postgrest:
        response = postgrest.session.post("/rpc/raise_warning_only")

        assert response.status_code == 204
        assert response.text == ""


def test_warnings_absent_when_disabled(defaultenv):
    "With the flag off, no warnings key is emitted even on error."
    with run(env=defaultenv) as postgrest:
        response = postgrest.session.post("/rpc/raise_warnings_then_exception")

        assert response.status_code == 400
        assert "warnings" not in response.json()


def test_warnings_minimal_verbosity(warnings_env):
    "Minimal verbosity keeps code and message only, without warnings."
    env = {
        **warnings_env,
        "PGRST_CLIENT_ERROR_VERBOSITY": "minimal",
    }
    with run(env=env) as postgrest:
        response = postgrest.session.post("/rpc/raise_warnings_then_exception")

        assert response.status_code == 400
        body = response.json()
        assert set(body.keys()) == {"code", "message"}


def test_warnings_are_drained_between_requests(warnings_env):
    "A warning from a successful request is not carried into the next request."
    env = {**warnings_env, "PGRST_DB_POOL": "1"}
    with run(env=env) as postgrest:
        response = postgrest.session.post("/rpc/raise_warning_only")
        assert response.status_code == 204

        response = postgrest.session.post("/rpc/raise_warnings_then_exception")
        assert response.status_code == 400
        assert [warning["message"] for warning in response.json()["warnings"]] == [
            "first warning",
            "second warning",
        ]
