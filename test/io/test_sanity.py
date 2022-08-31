"Sanity checks for the PostgREST black box testing infrastructure."

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


def test_port_connection(defaultenv):
    "Connections via a port on localhost should work."
    with run(env=defaultenv, port=freeport()):
        pass


def test_plain_get(defaultenv):
    "run() should give a working PostgREST."
    with run(env=defaultenv) as postgrest:
        response = postgrest.session.get("/projects")
        assert response.status_code == 200


def test_no_pool_connection_available(defaultenv):
    "no_pool_connection_available option is functional"
    with run(env=defaultenv, no_pool_connection_available=True) as postgrest:
        with pytest.raises(Exception) as e:
            postgrest.session.get("/projects", timeout=1)
