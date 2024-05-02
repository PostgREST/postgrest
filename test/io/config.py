import os
import pathlib
import shutil
import signal

import pytest
import yaml


BASEDIR = pathlib.Path(os.path.realpath(__file__)).parent
CONFIGSDIR = BASEDIR / "configs"
FIXTURES = yaml.load((BASEDIR / "fixtures.yaml").read_text(), Loader=yaml.Loader)
POSTGREST_BIN = shutil.which("postgrest")
SECRET = "reallyreallyreallyreallyverysafe"


@pytest.fixture
def dburi():
    "Postgres database connection URI."
    dbname = os.environ["PGDATABASE"]
    host = os.environ["PGHOST"]
    user = os.environ["PGUSER"]
    return f"postgresql://?dbname={dbname}&host={host}&user={user}".encode()


@pytest.fixture
def baseenv():
    "Base environment to connect to PostgreSQL"
    return {
        "PGDATABASE": os.environ["PGDATABASE"],
        "PGHOST": os.environ["PGHOST"],
        "PGUSER": os.environ["PGUSER"],
    }


@pytest.fixture
def defaultenv(baseenv):
    "Default environment for PostgREST."
    return {
        **baseenv,
        "PGRST_DB_CONFIG": "true",
        "PGRST_LOG_LEVEL": "info",
        "PGRST_DB_POOL": "1",
        "PGRST_NOT_EXISTING": "should not break any tests",
    }


@pytest.fixture
def replicaenv(defaultenv):
    "Default environment for a PostgREST replica."
    conf = {
        "PGRST_DB_ANON_ROLE": "postgrest_test_anonymous",
        "PGRST_DB_SCHEMAS": "replica",
    }
    return {
        "primary": {
            **defaultenv,
            **conf,
        },
        "replica": {
            **defaultenv,
            **conf,
            "PGHOST": os.environ["PGREPLICAHOST"] + "," + os.environ["PGHOST"],
            "PGREPLICASLOT": os.environ["PGREPLICASLOT"],
        },
    }


@pytest.fixture
def slow_schema_cache_env(defaultenv):
    "Slow schema cache load environment PostgREST."
    return {
        **defaultenv,
        "PGRST_INTERNAL_SCHEMA_CACHE_SLEEP": "1000",  # this does a pg_sleep internally, it will cause the schema cache query to be slow
        # the slow schema cache query will keep using one pool connection until it finishes
        # to prevent requests waiting for PGRST_DB_POOL_ACQUISITION_TIMEOUT we'll increase the pool size (must be >= 2)
        "PGRST_DB_POOL": "2",
        "PGRST_DB_CHANNEL_ENABLED": "true",
    }


def hpctixfile():
    "Returns an individual filename for each test, if the HPCTIXFILE environment variable is set."
    if "HPCTIXFILE" not in os.environ:
        return ""

    tixfile = pathlib.Path(os.environ["HPCTIXFILE"])
    test = hash(os.environ["PYTEST_CURRENT_TEST"])
    return tixfile.with_suffix(f".{test}.tix")
