"PostgREST CORS related IO tests"

import re

from util import drain_stdout
from postgrest import run


def test_options_request_logs_but_cors_preflight_does_not(defaultenv):
    "Plain OPTIONS requests should be logged, but CORS preflight requests should not."

    env = {
        **defaultenv,
        "PGRST_LOG_LEVEL": "info",
        "PGRST_SERVER_CORS_ALLOWED_ORIGINS": "http://example.com",
    }
    preflight_headers = {
        "Origin": "http://example.com",
        "Access-Control-Request-Method": "POST",
        "Access-Control-Request-Headers": "Content-Type",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.options("/projects")
        assert response.status_code == 200

        response = postgrest.session.options("/projects", headers=preflight_headers)
        assert response.status_code == 200
        assert response.headers["Access-Control-Allow-Origin"] == "http://example.com"

        output = drain_stdout(postgrest)

    assert len(output) == 1
    assert re.match(
        r'- - postgrest_test_anonymous \[.+\] "OPTIONS /projects HTTP/1.1" 200 \d+ "" "python-requests/.+"',
        output[0],
    )


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
