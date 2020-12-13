"Tests for inputs and outputs of PostgREST."

import contextlib
import dataclasses
from datetime import datetime
import pathlib
import subprocess
import tempfile
from operator import attrgetter
import os
import signal
import time
import urllib.parse

import jwt
import pytest
import requests
import requests_unixsocket
import yaml


BASEURL = "http://127.0.0.1:49421"
SECRET = "reallyreallyreallyreallyverysafe"
BASEDIR = pathlib.Path(os.path.realpath(__file__)).parent
CONFIGSDIR = BASEDIR / "configs"
FIXTURES = yaml.load((BASEDIR / "fixtures.yaml").read_text(), Loader=yaml.Loader)


@dataclasses.dataclass
class PostgrestProcess:
    baseurl: str
    process: object


class TimeOutException(Exception):
    pass


@pytest.fixture
def session():
    "Session for http requests."
    return requests_unixsocket.Session()


@pytest.fixture
def dburi():
    return os.getenv("POSTGREST_TEST_CONNECTION").encode("utf-8")


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


@pytest.mark.parametrize(
    "expectedconfig", (CONFIGSDIR / "expected").iterdir(), ids=attrgetter("name")
)
def test_expected_config(expectedconfig):
    """
    Configs as dumped by PostgREST should match an expected output.

    Used to test default values, config aliases and environment variables. The
    expected output for each file in 'configs', if available, is found in the
    'configs/expected' directory.

    """
    expected = expectedconfig.read_text()
    assert dumpconfig(CONFIGSDIR / expectedconfig.name) == expected


@pytest.mark.parametrize(
    "config",
    [conf for conf in CONFIGSDIR.iterdir() if conf.is_file()],
    ids=attrgetter("name"),
)
def test_stable_config(tmp_path, config):
    """
    A dumped, re-read and re-dumped config should match the dumped config.

    Note: only dump vs. re-dump must be equal, as the original config file might
    be different because of default values, whitespace, and quoting.

    """
    env = {
        "ROLE_CLAIM_KEY": '."https://www.example.com/roles"[0].value',
        "POSTGREST_TEST_SOCKET": "/tmp/postgrest.sock",
    }
    dumped = dumpconfig(config, moreenv=env)

    tmpconfigpath = tmp_path / "config"
    tmpconfigpath.write_text(dumped)
    redumped = dumpconfig(tmpconfigpath, moreenv=env)

    assert dumped == redumped


def test_socket_connection(session, tmp_path):
    socket = tmp_path / "postgrest.sock"
    env = {
        "POSTGREST_TEST_SOCKET": str(socket),
    }

    with run(CONFIGSDIR / "unix-socket.config", socket=socket, moreenv=env):
        pass


@pytest.mark.parametrize(
    "secretpath",
    [path for path in (BASEDIR / "secrets").iterdir() if path.suffix != ".jwt"],
    ids=attrgetter("name"),
)
def test_read_secret_from_file(session, secretpath):
    if secretpath.suffix == ".b64":
        configfile = CONFIGSDIR / "base64-secret-from-file.config"
    else:
        configfile = CONFIGSDIR / "secret-from-file.config"

    secret = secretpath.read_bytes()
    headers = authheader(secretpath.with_suffix(".jwt").read_text())

    with run(configfile, stdin=secret) as process:
        response = session.get(f"{process.baseurl}/authors_only", headers=headers)
        assert response.status_code == 200


def test_read_dburi_from_file_without_eol(session, dburi):
    with run(CONFIGSDIR / "dburi-from-file.config", stdin=dburi) as process:
        pass


def test_read_dburi_from_file_with_eol(session, dburi):
    with run(CONFIGSDIR / "dburi-from-file.config", stdin=dburi + b"\n") as process:
        pass


@pytest.mark.parametrize(
    "roleclaim", FIXTURES["roleclaims"], ids=lambda claim: claim["key"]
)
def test_role_claim_key(session, roleclaim):
    env = {"ROLE_CLAIM_KEY": roleclaim["key"]}
    headers = jwtauthheader(roleclaim["data"], SECRET)

    with run(CONFIGSDIR / "role-claim-key.config", moreenv=env) as process:
        response = session.get(f"{process.baseurl}/authors_only", headers=headers)
        assert response.status_code == roleclaim["expected_status"]


@pytest.mark.parametrize("invalidroleclaimkey", FIXTURES["invalidroleclaimkeys"])
def test_invalid_role_claim_key(invalidroleclaimkey):
    env = {"ROLE_CLAIM_KEY": invalidroleclaimkey}

    with pytest.raises(subprocess.CalledProcessError):
        dumpconfig(CONFIGSDIR / "role-claim-key.config", moreenv=env)


def authheader(token):
    "Bearer token HTTP authorization header."
    return {"Authorization": f"Bearer {token}"}


def jwtauthheader(claim, secret):
    "Authorization header with signed JWT."
    return authheader(jwt.encode(claim, secret).decode("utf-8"))


def test_iat_claim(session):
    claim = {"role": "postgrest_test_author", "iat": datetime.utcnow()}
    headers = jwtauthheader(claim, SECRET)

    with run(CONFIGSDIR / "simple.config") as process:
        for _ in range(10):
            url = f"{process.baseurl}/authors_only"
            response = session.get(url, headers=headers)
            assert response.status_code == 200

            time.sleep(0.5)


def test_app_settings(session):
    with run(CONFIGSDIR / "app-settings.config") as process:
        url = (
            f"{process.baseurl}/rpc/get_guc_value?name=app.settings.external_api_secret"
        )
        response = session.get(url)
        assert response.status_code == 200
        assert response.text == '"0123456789abcdef"'


def test_app_settings_reload(session, tmp_path):
    config = (CONFIGSDIR / "sigusr2-settings.config").read_text()
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
    config = (CONFIGSDIR / "sigusr2-settings.config").read_text()
    configfile = tmp_path / "test.config"
    configfile.write_text(config)

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    with run(configfile) as process:
        url = f"{process.baseurl}/authors_only"

        response = session.get(url, headers=headers)
        assert response.status_code == 401

        # change setting
        configfile.write_text(config.replace("invalid" * 5, SECRET))
        # reload
        process.process.send_signal(signal.SIGUSR2)

        response = session.get(url, headers=headers)
        assert response.status_code == 200


def test_db_schema_reload(session, tmp_path):
    config = (CONFIGSDIR / "sigusr2-settings.config").read_text()
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
