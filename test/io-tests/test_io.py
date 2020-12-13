"Tests for inputs and outputs of PostgREST."

import contextlib
import dataclasses
from datetime import datetime
import pathlib
import subprocess
import tempfile
import os
import signal
import time
import urllib.parse

import jwt
import pytest
import requests
import requests_unixsocket


BASEURL = "http://127.0.0.1:49421"

secret = "reallyreallyreallyreallyverysafe"
basedir = pathlib.Path(os.path.realpath(__file__)).parent
configs = [path for path in (basedir / "configs").iterdir() if path.is_file()]
expectedconfigs = list((basedir / "configs" / "expected").iterdir())
secrets = [path for path in (basedir / "secrets").iterdir() if path.suffix != ".jwt"]
dburi = os.getenv("POSTGREST_TEST_CONNECTION").encode("utf-8")
dburifromfileconfig = basedir / "configs" / "dburi-from-file.config"
roleclaimkeyconfig = basedir / "configs" / "role-claim-key.config"


@dataclasses.dataclass
class PostgrestProcess:
    baseurl: str
    process: object


@dataclasses.dataclass
class RoleClaimCase:
    key: str
    data: dict
    expected_status: int


roleclaimcases = [
    RoleClaimCase(
        ".postgrest.a_role", {"postgrest": {"a_role": "postgrest_test_author"}}, 200
    ),
    RoleClaimCase(
        ".customObject.manyRoles[1]",
        {"customObject": {"manyRoles": ["other", "postgrest_test_author"]}},
        200,
    ),
    RoleClaimCase(
        '."https://www.example.com/roles"[0].value',
        {"https://www.example.com/roles": [{"value": "postgrest_test_author"}]},
        200,
    ),
    RoleClaimCase(
        ".myDomain[3]", {"myDomain": ["other", "postgrest_test_author"]}, 401
    ),
    RoleClaimCase(".myRole", {"role": "postgrest_test_author"}, 401),
]

invalidroleclaimkeys = [
    "role.other",
    ".role##",
    ".my_role;;domain",
    ".#$$%&$%/",
    "",
    "1234",
]


class TimeOutException(Exception):
    pass


@pytest.fixture
def session():
    "Session for http requests."
    return requests_unixsocket.Session()


@pytest.fixture(params=configs, ids=[conf.name for conf in configs])
def configpath(request):
    "Fixture for all config paths."
    return basedir / "configs" / request.param


@pytest.fixture(params=expectedconfigs, ids=[conf.name for conf in expectedconfigs])
def expectedconfig(request):
    "Fixture for all expected configs."
    return request.param


@pytest.fixture(params=secrets, ids=[secret.name for secret in secrets])
def secretpath(request):
    "Fixture for all secrets."
    return request.param


@pytest.fixture(params=roleclaimcases, ids=[case.key for case in roleclaimcases])
def roleclaimcase(request):
    "Fixture for role claim test cases."
    return request.param


@pytest.fixture(params=invalidroleclaimkeys)
def invalidroleclaimkey(request):
    "Fixture for all invalid role claim keys."
    return request.param


def dumpconfig(configpath, moreenv=None):
    "Dump the config as parsed by PostgREST."
    env = {**os.environ, **(moreenv or {})}

    command = ["postgrest", "--dump-config", configpath]
    result = subprocess.run(command, env=env, capture_output=True, check=True)
    return result.stdout.decode("utf-8")


@contextlib.contextmanager
def run(configpath, stdin=None, moreenv=None, socket=None):
    "Run PostgREST."
    env = {**os.environ, **(moreenv or {})}

    if socket:
        baseurl = "http+unix://" + urllib.parse.quote_plus(str(socket))
    else:
        baseurl = BASEURL

    command = ["postgrest", configpath]
    process = subprocess.Popen(command, stdin=subprocess.PIPE, env=env)

    try:
        if stdin:
            process.stdin.write(stdin)
        process.stdin.close()

        waitfor200(baseurl)
        yield PostgrestProcess(baseurl=baseurl, process=process)
    finally:
        process.kill()
        process.wait()


def waitfor200(url):
    session = requests_unixsocket.Session()

    for i in range(10):
        try:
            response = session.get(url, timeout=0.1)

            if response.status_code == 200:
                return
        except requests.ConnectionError:
            pass

        time.sleep(0.1)

    raise TimeOutException()


def test_expected_config(expectedconfig):
    """
    Configs as dumped by PostgREST should match an expected output.

    Used to test default values, config aliases and environment variables. The
    expected output for each file in 'configs', if available, is found in the
    'configs/expected' directory.

    """
    expected = (basedir / "configs" / "expected" / expectedconfig).read_text()
    assert dumpconfig(basedir / "configs" / expectedconfig) == expected


def test_stable_config(tmp_path, configpath):
    """
    A dumped, re-read and re-dumped config should match the dumped config.

    Note: only dump vs. re-dump must be equal, as the original config file might
    be different because of default values, whitespace, and quoting.

    """
    env = {
        "ROLE_CLAIM_KEY": '."https://www.example.com/roles"[0].value',
        "POSTGREST_TEST_SOCKET": "/tmp/postgrest.sock",
    }
    dumped = dumpconfig(configpath, moreenv=env)

    tmpconfigpath = tmp_path / "config"
    tmpconfigpath.write_text(dumped)
    redumped = dumpconfig(tmpconfigpath, moreenv=env)

    assert dumped == redumped


def test_socket_connection(session, tmp_path):
    socket = tmp_path / "postgrest.sock"
    env = {
        "POSTGREST_TEST_SOCKET": str(socket),
    }

    with run(basedir / "configs" / "unix-socket.config", socket=socket, moreenv=env):
        pass


def test_read_secret_from_file(session, secretpath):
    if secretpath.suffix == ".b64":
        configfile = basedir / "configs" / "base64-secret-from-file.config"
    else:
        configfile = basedir / "configs" / "secret-from-file.config"

    secret = secretpath.read_bytes()

    jwt = secretpath.with_suffix(".jwt").read_text()
    headers = {"Authorization": f"Bearer {jwt}"}

    with run(configfile, stdin=secret) as process:
        response = session.get(f"{process.baseurl}/authors_only", headers=headers)
        assert response.status_code == 200


def test_read_dburi_from_file_without_eol(session):
    with run(dburifromfileconfig, stdin=dburi) as process:
        response = session.get(f"{process.baseurl}/")
        assert response.status_code == 200


def test_read_dburi_from_file_with_eol(session):
    with run(dburifromfileconfig, stdin=dburi + b"\n") as process:
        response = session.get(f"{process.baseurl}/")
        assert response.status_code == 200


def test_role_claim_key(session, roleclaimcase):
    env = {"ROLE_CLAIM_KEY": roleclaimcase.key}
    token = jwt.encode(roleclaimcase.data, secret).decode("utf-8")
    headers = {"Authorization": f"Bearer {token}"}

    with run(roleclaimkeyconfig, moreenv=env) as process:
        response = session.get(f"{process.baseurl}/authors_only", headers=headers)
        assert response.status_code == roleclaimcase.expected_status


def test_invalid_role_claim_key(invalidroleclaimkey):
    env = {"ROLE_CLAIM_KEY": invalidroleclaimkey}

    with pytest.raises(subprocess.CalledProcessError):
        dumpconfig(roleclaimkeyconfig, moreenv=env)


def test_iat_claim(session):
    claim = {"role": "postgrest_test_author", "iat": datetime.utcnow()}
    token = jwt.encode(claim, secret).decode("utf-8")
    headers = {"Authorization": f"Bearer {token}"}

    with run(basedir / "configs" / "simple.config") as process:
        for _ in range(10):
            response = session.get(f"{process.baseurl}/authors_only", headers=headers)
            assert response.status_code == 200

            time.sleep(0.5)


def test_app_settings(session):
    with run(basedir / "configs" / "app-settings.config") as process:
        url = (
            f"{process.baseurl}/rpc/get_guc_value?name=app.settings.external_api_secret"
        )
        response = session.get(url)
        assert response.status_code == 200
        assert response.text == '"0123456789abcdef"'


def test_app_settings_reload(session, tmp_path):
    config = (basedir / "configs" / "sigusr2-settings.config").read_text()
    configfile = tmp_path / "test.config"
    configfile.write_text(config)

    with run(configfile) as process:
        url = f"{process.baseurl}/rpc/get_guc_value?name=app.settings.name_var"

        response = session.get(url)
        assert response.status_code == 200
        assert response.text == '"John"'

        # change setting
        configfile.write_text(config.replace("John", "Jane"))
        # reload
        process.process.send_signal(signal.SIGUSR2)

        response = session.get(url)
        assert response.status_code == 200
        assert response.text == '"Jane"'


def test_jwt_secret_reload(session, tmp_path):
    config = (basedir / "configs" / "sigusr2-settings.config").read_text()
    configfile = tmp_path / "test.config"
    configfile.write_text(config)

    claim = {"role": "postgrest_test_author"}
    token = jwt.encode(claim, secret).decode("utf-8")
    headers = {"Authorization": f"Bearer {token}"}

    with run(configfile) as process:
        url = f"{process.baseurl}/authors_only"

        response = session.get(url, headers=headers)
        assert response.status_code == 401

        # change setting
        configfile.write_text(config.replace("invalid" * 5, secret))
        # reload
        process.process.send_signal(signal.SIGUSR2)

        response = session.get(url, headers=headers)
        assert response.status_code == 200


def test_db_schema_reload(session, tmp_path):
    config = (basedir / "configs" / "sigusr2-settings.config").read_text()
    configfile = tmp_path / "test.config"
    configfile.write_text(config)

    headers = {"Accept-Profile": "v1"}

    with run(configfile) as process:
        url = f"{process.baseurl}/parents"

        response = session.get(url, headers=headers)
        assert response.status_code == 404

        # change setting
        configfile.write_text(
            config.replace('db-schema = "test"', 'db-schema = "test, v1"')
        )
        # reload
        process.process.send_signal(signal.SIGUSR2)
        process.process.send_signal(signal.SIGUSR1)

        response = session.get(url, headers=headers)
        assert response.status_code == 200
