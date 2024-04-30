"IO tests for PostgREST started on replicas"

import pytest

from config import *
from util import *
from postgrest import *


def test_sanity_replica(replicaenv):
    "Test that primary and replica are working as intended"

    with run(env=replicaenv["primary"]) as postgrest:
        response = postgrest.session.get("/rpc/is_replica")
        assert response.text == "false"

        response = postgrest.session.get("/rpc/get_replica_slot")
        assert response.text == '"' + replicaenv["replica"]["PGREPLICASLOT"] + '"'

        response = postgrest.session.get("/items?select=count")
        assert response.text == '[{"count":10}]'

    with run(env=replicaenv["replica"]) as postgrest:
        response = postgrest.session.get("/rpc/is_replica")
        assert response.text == "true"

        response = postgrest.session.get("/items?select=count")
        assert response.text == '[{"count":10}]'
