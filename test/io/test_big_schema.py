"IO tests for PostgREST started on the big schema."

import pytest

from config import *
from util import *
from postgrest import *


def test_first_request_succeeds(defaultenv):
    "the first request suceeds fast since the admin server readiness was checked before"

    env = {
        **defaultenv,
        "PGRST_DB_SCHEMAS": "apflora",
        "PGRST_DB_POOL": "2",
        "PGRST_DB_ANON_ROLE": "postgrest_test_anonymous",
        "PGRST_SERVER_TIMING_ENABLED": "true",
    }

    with run(env=env, wait_max_seconds=30) as postgrest:
        response = postgrest.session.get("/tpopmassn?select=*,tpop(*)")
        assert response.status_code == 200

        server_timings = parse_server_timings_header(response.headers["Server-Timing"])
        plan_dur = server_timings["plan"]
        assert plan_dur < 2.0


def test_requests_do_not_wait_for_schema_cache_reload(defaultenv):
    "requests that use the schema cache (e.g. resource embedding) do not wait for the schema cache to reload"

    env = {
        **defaultenv,
        "PGRST_DB_SCHEMAS": "apflora",
        "PGRST_DB_POOL": "2",
        "PGRST_DB_ANON_ROLE": "postgrest_test_anonymous",
        "PGRST_SERVER_TIMING_ENABLED": "true",
    }

    with run(env=env, wait_max_seconds=30) as postgrest:
        # reload the schema cache
        response = postgrest.session.get("/rpc/notify_pgrst")
        assert response.status_code == 204

        time.sleep(1.5)  # wait for schema cache to start reloading

        response = postgrest.session.get("/tpopmassn?select=*,tpop(*)")
        assert response.status_code == 200

        # response should be fast
        server_timings = parse_server_timings_header(response.headers["Server-Timing"])
        plan_dur = server_timings["plan"]
        assert plan_dur < 2.0


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
