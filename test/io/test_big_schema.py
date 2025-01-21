"IO tests for PostgREST started on the big schema."

import pytest

from config import *
from util import *
from postgrest import *


def test_requests_with_resource_embedding_wait_for_schema_cache_reload(defaultenv):
    "requests that use the schema cache with resource embedding wait long for the schema cache to reload"

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

        postgrest.wait_until_scache_starts_loading()

        response = postgrest.session.get("/tpopmassn?select=*,tpop(*)")
        assert response.status_code == 200

        plan_dur = parse_server_timings_header(response.headers["Server-Timing"])[
            "plan"
        ]
        assert plan_dur > 10000.0


def test_requests_without_resource_embedding_wait_for_schema_cache_reload(defaultenv):
    "requests that use the schema cache without resource embedding wait less for the schema cache to reload"

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

        postgrest.wait_until_scache_starts_loading()

        response = postgrest.session.get("/tpopmassn")
        assert response.status_code == 200

        plan_dur = parse_server_timings_header(response.headers["Server-Timing"])[
            "plan"
        ]
        assert plan_dur < 10000.0


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


# See: https://github.com/PostgREST/postgrest/issues/3329
def test_should_not_fail_with_stack_overflow(defaultenv):
    "requesting a non-existent relationship should not fail with stack overflow due to fuzzy search of candidates"

    env = {
        **defaultenv,
        "PGRST_DB_SCHEMAS": "apflora",
        "PGRST_DB_POOL": "2",
        "PGRST_DB_ANON_ROLE": "postgrest_test_anonymous",
    }

    with run(env=env, wait_max_seconds=30) as postgrest:
        response = postgrest.session.get("/unknown-table?select=unknown-rel(*)")
        assert response.status_code == 404
        data = response.json()
        assert data["code"] == "PGRST205"
