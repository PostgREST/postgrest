import os
import pytest
from syrupy.extensions.json import SingleFileSnapshotExtension
from postgrest import SchemaCacheLocks, run


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
def schema_cache_locks(baseenv):
    "Factory for controlling step-by-step querySchemaCache execution."

    def factory(lock_id=None, max_step=15):
        return SchemaCacheLocks(baseenv, lock_id=lock_id, max_step=max_step)

    return factory


@pytest.fixture
def metapostgrest():
    "A shared postgrest instance to use for interacting with the database independently of the instance under test"
    role = "meta_authenticator"
    env = {
        "PGDATABASE": os.environ["PGDATABASE"],
        "PGHOST": os.environ["PGHOST"],
        "PGUSER": role,
        "PGRST_DB_ANON_ROLE": role,
        "PGRST_DB_CONFIG": "true",
        "PGRST_LOG_LEVEL": "info",
        "PGRST_DB_POOL": "1",
    }
    with run(env=env) as postgrest:
        yield postgrest


class YamlSnapshotExtension(SingleFileSnapshotExtension):
    file_extension = "yaml"


@pytest.fixture
def snapshot_yaml(snapshot):
    return snapshot.use_extension(YamlSnapshotExtension)
