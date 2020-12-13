"Unit tests for Input/Ouput of PostgREST seen as a black box."

import contextlib
import dataclasses
from datetime import datetime
import pathlib
import subprocess
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


BASEDIR = pathlib.Path(os.path.realpath(__file__)).parent
CONFIGSDIR = BASEDIR / "configs"
FIXTURES = yaml.load((BASEDIR / "fixtures.yaml").read_text(), Loader=yaml.Loader)
BASEURL = "http://127.0.0.1:49421"
SECRET = "reallyreallyreallyreallyverysafe"


class PostgrestTimedOut(Exception):
    "Connecting to PostgREST endpoint timed out."


class PostgrestError(Exception):
    "Postgrest exited with a non-zero return code."


@dataclasses.dataclass
class PostgrestProcess:
    "Running PostgREST process and its corresponding endpoint."
    baseurl: str
    process: object


@pytest.fixture
def session():
    "Session for HTTP requests that supports connecting to unix domain sockets."
    return requests_unixsocket.Session()


@pytest.fixture
def dburi():
    "Postgres database connection URI."
    return os.getenv("POSTGREST_TEST_CONNECTION").encode("utf-8")


def dumpconfig(configpath, moreenv=None, stdin=None):
    "Dump the config as parsed by PostgREST."
    env = {**os.environ, **(moreenv or {})}
    command = ["postgrest", "--dump-config", configpath]
    process = subprocess.Popen(
        command, env=env, stdin=subprocess.PIPE, stdout=subprocess.PIPE
    )
    process.stdin.write(stdin or b"")
    result = process.communicate()[0]
    process.kill()
    process.wait()
    if process.returncode != 0:
        raise PostgrestError()
    return result.decode("utf-8")


@contextlib.contextmanager
def run(configpath, stdin=None, moreenv=None, socket=None):
    "Run PostgREST and yield an endpoint that is ready for connections."
    env = {**os.environ, **(moreenv or {})}

    if socket:
        baseurl = "http+unix://" + urllib.parse.quote_plus(str(socket))
    else:
        baseurl = BASEURL

    command = ["postgrest", configpath]
    process = subprocess.Popen(command, stdin=subprocess.PIPE, env=env)

    try:
        process.stdin.write(stdin or b"")
        process.stdin.close()

        wait_until_ready(baseurl)

        yield PostgrestProcess(baseurl=baseurl, process=process)
    finally:
        process.kill()
        process.wait()


def wait_until_ready(url):
    "Wait for the given HTTP endpoint to return a status of 200."
    session = requests_unixsocket.Session()

    for _ in range(10):
        try:
            response = session.get(url, timeout=0.1)

            if response.status_code == 200:
                return
        except requests.ConnectionError:
            pass

        time.sleep(0.1)

    raise PostgrestTimedOut()


def authheader(token):
    "Bearer token HTTP authorization header."
    return {"Authorization": f"Bearer {token}"}


def jwtauthheader(claim, secret):
    "Authorization header with signed JWT."
    return authheader(jwt.encode(claim, secret).decode("utf-8"))


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
    [conf for conf in CONFIGSDIR.iterdir() if conf.suffix == ".config"],
    ids=attrgetter("name"),
)
def test_stable_config(tmp_path, config):
    """
    A dumped, re-read and re-dumped config should match the dumped config.

    Note: only dump vs. re-dump must be equal, as the original config file might
    be different because of default values, whitespace, and quoting.

    """

    # Set environment variables that some of the configs expect. Using a
    # complex ROLE_CLAIM_KEY to make sure quoting works.
    env = {
        "ROLE_CLAIM_KEY": '."https://www.example.com/roles"[0].value',
        "POSTGREST_TEST_SOCKET": "/tmp/postgrest.sock",
    }

    # Some configs expect input from stdin, at least on base64.
    stdin = b"Y29ubmVjdGlvbl9zdHJpbmc="

    dumped = dumpconfig(config, moreenv=env, stdin=stdin)

    tmpconfigpath = tmp_path / "config"
    tmpconfigpath.write_text(dumped)
    redumped = dumpconfig(tmpconfigpath, moreenv=env)

    assert dumped == redumped


def test_socket_connection(tmp_path):
    "Connections via unix domain sockets should work."
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
    "Authorization should succeed when the secret is read from a file."
    if secretpath.suffix == ".b64":
        configfile = CONFIGSDIR / "base64-secret-from-file.config"
    else:
        configfile = CONFIGSDIR / "secret-from-file.config"

    secret = secretpath.read_bytes()
    headers = authheader(secretpath.with_suffix(".jwt").read_text())

    with run(configfile, stdin=secret) as postgrest:
        response = session.get(f"{postgrest.baseurl}/authors_only", headers=headers)
        assert response.status_code == 200


def test_read_dburi_from_file_without_eol(dburi):
    "Reading the dburi from a file with a single line should work."
    with run(CONFIGSDIR / "dburi-from-file.config", stdin=dburi):
        pass


def test_read_dburi_from_file_with_eol(dburi):
    "Reading the dburi from a file containing a newline should work."
    with run(CONFIGSDIR / "dburi-from-file.config", stdin=dburi + b"\n"):
        pass


@pytest.mark.parametrize(
    "roleclaim", FIXTURES["roleclaims"], ids=lambda claim: claim["key"]
)
def test_role_claim_key(session, roleclaim):
    "Authorization should depend on a correct role-claim-key and JWT claim."
    env = {"ROLE_CLAIM_KEY": roleclaim["key"]}
    headers = jwtauthheader(roleclaim["data"], SECRET)

    with run(CONFIGSDIR / "role-claim-key.config", moreenv=env) as postgrest:
        response = session.get(f"{postgrest.baseurl}/authors_only", headers=headers)
        assert response.status_code == roleclaim["expected_status"]


@pytest.mark.parametrize("invalidroleclaimkey", FIXTURES["invalidroleclaimkeys"])
def test_invalid_role_claim_key(invalidroleclaimkey):
    "Given an invalid role-claim-key, Postgrest should exit with a non-zero exit code."
    env = {"ROLE_CLAIM_KEY": invalidroleclaimkey}

    with pytest.raises(PostgrestError):
        dump = dumpconfig(CONFIGSDIR / "role-claim-key.config", moreenv=env)
        for line in dump.split("\n"):
            if line.startswith("jwt-role-claim-key"):
                print(line)


def test_iat_claim(session):
    """
    A claim with an 'iat' (issued at) attribute should be successful.

    The PostgREST time cache leads to issues here, see:
    https://github.com/PostgREST/postgrest/issues/1139

    """
    claim = {"role": "postgrest_test_author", "iat": datetime.utcnow()}
    headers = jwtauthheader(claim, SECRET)

    with run(CONFIGSDIR / "simple.config") as postgrest:
        for _ in range(10):
            url = f"{postgrest.baseurl}/authors_only"
            response = session.get(url, headers=headers)
            assert response.status_code == 200

            time.sleep(0.5)


def test_app_settings(session):
    """
    App settings should not reset when the db pool times out.

    See: https://github.com/PostgREST/postgrest/issues/1141

    """
    with run(CONFIGSDIR / "app-settings.config") as postgrest:
        # Wait for the db pool to time out, set to 1s in config
        time.sleep(2)

        uri = "/rpc/get_guc_value?name=app.settings.external_api_secret"
        response = session.get(postgrest.baseurl + uri)

        assert response.status_code == 200
        assert response.text == '"0123456789abcdef"'


def test_app_settings_reload(session, tmp_path):
    "App settings should be reloaded when PostgREST is sent SIGUSR2."
    config = (CONFIGSDIR / "sigusr2-settings.config").read_text()
    configfile = tmp_path / "test.config"
    configfile.write_text(config)

    with run(configfile) as postgrest:
        url = f"{postgrest.baseurl}/rpc/get_guc_value?name=app.settings.name_var"

        response = session.get(url)
        assert response.status_code == 200
        assert response.text == '"John"'

        # change setting
        configfile.write_text(config.replace("John", "Jane"))
        # reload
        postgrest.process.send_signal(signal.SIGUSR2)

        response = session.get(url)
        assert response.status_code == 200
        assert response.text == '"Jane"'


def test_jwt_secret_reload(session, tmp_path):
    "JWT secret should be reloaded when PostgREST is sent SIGUSR2."
    config = (CONFIGSDIR / "sigusr2-settings.config").read_text()
    configfile = tmp_path / "test.config"
    configfile.write_text(config)

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    with run(configfile) as postgrest:
        url = f"{postgrest.baseurl}/authors_only"

        response = session.get(url, headers=headers)
        assert response.status_code == 401

        # change setting
        configfile.write_text(config.replace("invalid" * 5, SECRET))
        # reload
        postgrest.process.send_signal(signal.SIGUSR2)

        response = session.get(url, headers=headers)
        assert response.status_code == 200


def test_db_schema_reload(session, tmp_path):
    "DB schema should be reloaded when PostgREST is sent SIGUSR2."
    config = (CONFIGSDIR / "sigusr2-settings.config").read_text()
    configfile = tmp_path / "test.config"
    configfile.write_text(config)

    headers = {"Accept-Profile": "v1"}

    with run(configfile) as postgrest:
        url = f"{postgrest.baseurl}/parents"

        response = session.get(url, headers=headers)
        assert response.status_code == 404

        # change setting
        configfile.write_text(
            config.replace('db-schemas = "test"', 'db-schemas = "test, v1"')
        )
        # reload
        postgrest.process.send_signal(signal.SIGUSR2)
        postgrest.process.send_signal(signal.SIGUSR1)

        response = session.get(url, headers=headers)
        assert response.status_code == 200
