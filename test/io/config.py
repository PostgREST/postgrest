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


def hpctixfile():
    "Returns an individual filename for each test, if the HPCTIXFILE environment variable is set."
    if "HPCTIXFILE" not in os.environ:
        return ""

    tixfile = pathlib.Path(os.environ["HPCTIXFILE"])
    test = hash(os.environ["PYTEST_CURRENT_TEST"])
    return tixfile.with_suffix(f".{test}.tix")
