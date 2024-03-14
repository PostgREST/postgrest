"IO tests for PostgREST started on the big schema."

import pytest

from config import *
from util import *
from postgrest import *


def test_requests_wait_for_schema_cache_to_be_loaded(defaultenv):
    "requests that use the schema cache (e.g. resource embedding) wait for schema cache to be loaded"

    env = {
        **defaultenv,
        "PGRST_DB_SCHEMAS": "apflora",
        "PGRST_DB_POOL": "2",
        "PGRST_DB_ANON_ROLE": "postgrest_test_anonymous",
        "PGRST_SERVER_TIMING_ENABLED": "true",
    }

    with run(env=env, wait_for_readiness=False) as postgrest:
        time.sleep(1.5)  # manually wait for schema cache to start loading

        response = postgrest.session.get("/tpopmassn?select=*,tpop(*)")
        assert response.status_code == 200

        server_timings = parse_server_timings_header(response.headers["Server-Timing"])
        plan_dur = server_timings["plan"]
        assert plan_dur > 10000.0


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
