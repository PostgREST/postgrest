"Unit tests for Input/Ouput of PostgREST seen as a black box."

import contextlib
import dataclasses
from datetime import datetime
from itertools import repeat
from operator import attrgetter
import os
import pathlib
import shutil
import signal
import socket
import subprocess
import tempfile
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
POSTGREST_BIN = shutil.which("postgrest")
SECRET = "reallyreallyreallyreallyverysafe"


def itemgetter(*items):
    "operator.itemgetter with None as fallback when key does not exist"
    if len(items) == 1:
        item = items[0]

        def g(obj):
            return obj.get(item)

    else:

        def g(obj):
            return tuple(obj.get(item) for item in items)

    return g


class PostgrestTimedOut(Exception):
    "Connecting to PostgREST endpoint timed out."


class PostgrestError(Exception):
    "Postgrest exited with a non-zero return code."


class PostgrestSession(requests_unixsocket.Session):
    "HTTP client session directed at a PostgREST endpoint."

    def __init__(self, baseurl, *args, **kwargs):
        super(PostgrestSession, self).__init__(*args, **kwargs)
        self.baseurl = baseurl

    def request(self, method, url, *args, **kwargs):
        # Not using urllib.parse.urljoin to compose the url, as it doesn't play
        # well with our 'http+unix://' unix domain socket urls.
        fullurl = self.baseurl + url
        return super(PostgrestSession, self).request(method, fullurl, *args, **kwargs)


@dataclasses.dataclass
class PostgrestProcess:
    "Running PostgREST process and its corresponding endpoint."
    process: object
    session: object


@pytest.fixture
def dburi():
    "Postgres database connection URI."
    return os.getenv("PGRST_DB_URI").encode()


@pytest.fixture
def defaultenv():
    "Default environment for PostgREST."
    return {
        "PGRST_DB_URI": os.environ["PGRST_DB_URI"],
        "PGRST_DB_SCHEMAS": "public",
        "PGRST_DB_ANON_ROLE": os.environ["PGRST_DB_ANON_ROLE"],
        "PGRST_DB_CONFIG": "false",
        "PGRST_LOG_LEVEL": "info",
    }


def hpctixfile():
    "Returns an individual filename for each test, if the HPCTIXFILE environment variable is set."
    if "HPCTIXFILE" not in os.environ:
        return ""

    tixfile = pathlib.Path(os.environ["HPCTIXFILE"])
    test = hash(os.environ["PYTEST_CURRENT_TEST"])
    return tixfile.with_suffix(f".{test}.tix")


def cli(args, env=None, stdin=None):
    "Run PostgREST and return stdout."
    env = env or {}

    command = [POSTGREST_BIN] + args
    env["HPCTIXFILE"] = hpctixfile()

    process = subprocess.Popen(
        command, env=env, stdin=subprocess.PIPE, stdout=subprocess.PIPE
    )

    process.stdin.write(stdin or b"")
    try:
        result = process.communicate(timeout=5)[0]
        if process.returncode != 0:
            raise PostgrestError()
        return result.decode()
    finally:
        process.kill()
        process.wait()


def dumpconfig(configpath=None, env=None, stdin=None):
    "Dump the config as parsed by PostgREST."
    args = ["--dump-config"]

    if configpath:
        args.append(configpath)

    return cli(args, env=env, stdin=stdin)


@contextlib.contextmanager
def run(configpath=None, stdin=None, env=None, port=None):
    "Run PostgREST and yield an endpoint that is ready for connections."
    env = env or {}

    with tempfile.TemporaryDirectory() as tmpdir:
        if port:
            env["PGRST_SERVER_PORT"] = str(port)
            env["PGRST_SERVER_HOST"] = "localhost"
            baseurl = f"http://localhost:{port}"
        else:
            socketfile = pathlib.Path(tmpdir) / "postgrest.sock"
            env["PGRST_SERVER_UNIX_SOCKET"] = str(socketfile)
            baseurl = "http+unix://" + urllib.parse.quote_plus(str(socketfile))

        command = [POSTGREST_BIN]
        env["HPCTIXFILE"] = hpctixfile()

        if configpath:
            command.append(configpath)

        process = subprocess.Popen(
            command,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            env=env,
        )

        os.set_blocking(process.stdout.fileno(), False)

        try:
            process.stdin.write(stdin or b"")
            process.stdin.close()

            wait_until_ready(baseurl)

            process.stdout.read()

            yield PostgrestProcess(process=process, session=PostgrestSession(baseurl))
        finally:
            remaining_output = process.stdout.read()
            if remaining_output:
                print(remaining_output.decode())
            process.terminate()
            try:
                process.wait(timeout=1)
            except:
                process.kill()
                process.wait()


def freeport():
    "Find a free port on localhost."
    with contextlib.closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as s:
        s.bind(("", 0))
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        return s.getsockname()[1]


def wait_until_ready(url):
    "Wait for the given HTTP endpoint to return a status of 200."
    session = requests_unixsocket.Session()

    response = None
    for _ in range(10):
        try:
            response = session.get(url, timeout=1)
            if response.status_code == 200:
                return
        except (requests.ConnectionError, requests.ReadTimeout):
            pass

        time.sleep(0.1)

    if response:
        raise PostgrestTimedOut(f"{response.status_code}: {response.text}")
    else:
        raise PostgrestTimedOut()


def authheader(token):
    "Bearer token HTTP authorization header."
    return {"Authorization": f"Bearer {token}"}


def jwtauthheader(claim, secret):
    "Authorization header with signed JWT."
    return authheader(jwt.encode(claim, secret))


@pytest.mark.parametrize(
    "args,env,use_defaultenv,expect",
    map(itemgetter("args", "env", "use_defaultenv", "expect"), FIXTURES["cli"]),
    ids=map(itemgetter("name"), FIXTURES["cli"]),
)
def test_cli(args, env, use_defaultenv, expect, defaultenv):
    """
    When PostgREST is run with <args> arguments and <env>/<defaultenv> environment variabales
    it should return. Exit code should be according to <expect_error>.
    """
    # use --dump-config by default to make sure that the postgrest process will terminate for sure
    args = args or ["--dump-config"]

    env = env or {}
    if use_defaultenv:
        env = {**defaultenv, **env}

    if expect == "error":
        with pytest.raises(PostgrestError):
            print(cli(args, env=env))
    else:
        dump = cli(args, env=env).split("\n")
        if expect:
            assert expect in dump


@pytest.mark.parametrize(
    "expectedconfig",
    [
        expectedconfig
        for expectedconfig in (CONFIGSDIR / "expected").iterdir()
        if (CONFIGSDIR / expectedconfig.name).exists()
    ],
    ids=attrgetter("name"),
)
def test_expected_config(expectedconfig):
    """
    Configs as dumped by PostgREST should match an expected output.

    Used to test default values, config aliases and environment variables. The
    expected output for each file in 'configs', if available, is found in the
    'configs/expected' directory.

    """
    expected = expectedconfig.read_text()
    config = CONFIGSDIR / expectedconfig.name

    assert dumpconfig(config) == expected


def test_expected_config_from_environment():
    "Config should be read directly from environment without config file."

    envfile = (CONFIGSDIR / "no-defaults-env.yaml").read_text()
    env = {k: str(v) for k, v in yaml.load(envfile, Loader=yaml.Loader).items()}

    expected = (CONFIGSDIR / "expected" / "no-defaults.config").read_text()
    assert dumpconfig(env=env) == expected


@pytest.mark.parametrize(
    "role, expectedconfig",
    [
        ("db_config_authenticator", "no-defaults-with-db.config"),
        ("other_authenticator", "no-defaults-with-db-other-authenticator.config"),
    ],
)
def test_expected_config_from_db_settings(defaultenv, role, expectedconfig):
    "Config should be overriden from database settings"

    config = CONFIGSDIR / "no-defaults.config"

    db_uri = defaultenv["PGRST_DB_URI"].replace(
        "user=postgrest_test_authenticator", f"user={role}"
    )
    env = {
        **defaultenv,
        "PGRST_DB_URI": db_uri,
        "PGRST_DB_CONFIG": "true",
    }
    expected = (
        (CONFIGSDIR / "expected" / expectedconfig)
        .read_text()
        .replace("<REPLACED_WITH_DB_URI>", env["PGRST_DB_URI"])
    )

    assert dumpconfig(configpath=config, env=env) == expected


@pytest.mark.parametrize(
    "config",
    [conf for conf in CONFIGSDIR.iterdir() if conf.suffix == ".config"],
    ids=attrgetter("name"),
)
def test_stable_config(tmp_path, config, defaultenv):
    """
    A dumped, re-read and re-dumped config should match the dumped config.

    Note: only dump vs. re-dump must be equal, as the original config file might
    be different because of default values, whitespace, and quoting.

    """

    # Set environment variables that some of the configs expect. Using a
    # complex ROLE_CLAIM_KEY to make sure quoting works.
    env = {
        **defaultenv,
        "ROLE_CLAIM_KEY": '."https://www.example.com/roles"[0].value',
        "POSTGREST_TEST_SOCKET": "/tmp/postgrest.sock",
        "POSTGREST_TEST_PORT": "80",
        "JWT_SECRET_FILE": "a_file",
    }

    # Some configs expect input from stdin, at least on base64.
    stdin = b"Y29ubmVjdGlvbl9zdHJpbmc="

    dumped = dumpconfig(config, env=env, stdin=stdin)

    tmpconfigpath = tmp_path / "config"
    tmpconfigpath.write_text(dumped)
    redumped = dumpconfig(tmpconfigpath, env=env)

    assert dumped == redumped


def test_port_connection(defaultenv):
    "Connections via a port on localhost should work."
    with run(env=defaultenv, port=freeport()):
        pass


@pytest.mark.parametrize(
    "secretpath",
    [path for path in (BASEDIR / "secrets").iterdir() if path.suffix != ".jwt"],
    ids=attrgetter("name"),
)
def test_read_secret_from_file(secretpath, defaultenv):
    "Authorization should succeed when the secret is read from a file."
    if secretpath.suffix == ".b64":
        configfile = CONFIGSDIR / "base64-secret-from-file.config"
    else:
        configfile = CONFIGSDIR / "secret-from-file.config"

    secret = secretpath.read_bytes()
    headers = authheader(secretpath.with_suffix(".jwt").read_text())

    with run(configfile, stdin=secret, env=defaultenv) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 200


def test_read_dburi_from_file_without_eol(dburi, defaultenv):
    "Reading the dburi from a file with a single line should work."
    config = CONFIGSDIR / "dburi-from-file.config"
    env = {key: value for key, value in defaultenv.items() if key != "PGRST_DB_URI"}
    with run(config, env=env, stdin=dburi):
        pass


def test_read_dburi_from_file_with_eol(dburi, defaultenv):
    "Reading the dburi from a file containing a newline should work."
    config = CONFIGSDIR / "dburi-from-file.config"
    env = {key: value for key, value in defaultenv.items() if key != "PGRST_DB_URI"}
    with run(config, env=env, stdin=dburi + b"\n"):
        pass


@pytest.mark.parametrize(
    "roleclaim", FIXTURES["roleclaims"], ids=lambda claim: claim["key"]
)
def test_role_claim_key(roleclaim, defaultenv):
    "Authorization should depend on a correct role-claim-key and JWT claim."
    env = {
        **defaultenv,
        "ROLE_CLAIM_KEY": roleclaim["key"],
    }
    headers = jwtauthheader(roleclaim["data"], SECRET)

    with run(CONFIGSDIR / "role-claim-key.config", env=env) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == roleclaim["expected_status"]


@pytest.mark.parametrize("invalidroleclaimkey", FIXTURES["invalidroleclaimkeys"])
def test_invalid_role_claim_key(invalidroleclaimkey, defaultenv):
    "Given an invalid role-claim-key, Postgrest should exit with a non-zero exit code."
    env = {
        **defaultenv,
        "ROLE_CLAIM_KEY": invalidroleclaimkey,
    }

    with pytest.raises(PostgrestError):
        dump = dumpconfig(CONFIGSDIR / "role-claim-key.config", env=env)
        for line in dump.split("\n"):
            if line.startswith("jwt-role-claim-key"):
                print(line)


@pytest.mark.parametrize("invalidopenapimodes", FIXTURES["invalidopenapimodes"])
def test_invalid_openapi_mode(invalidopenapimodes, defaultenv):
    "Given an invalid openapi-mode, Postgrest should exit with a non-zero exit code."
    env = {
        **defaultenv,
        "PGRST_OPENAPI_MODE": invalidopenapimodes,
    }

    with pytest.raises(PostgrestError):
        dump = dumpconfig(CONFIGSDIR / "defaults.config", env=env)
        for line in dump.split("\n"):
            if line.startswith("openapi-mode"):
                print(line)


def test_iat_claim(defaultenv):
    """
    A claim with an 'iat' (issued at) attribute should be successful.

    The PostgREST time cache leads to issues here, see:
    https://github.com/PostgREST/postgrest/issues/1139

    """
    claim = {"role": "postgrest_test_author", "iat": datetime.utcnow()}
    headers = jwtauthheader(claim, SECRET)

    with run(CONFIGSDIR / "simple.config", env=defaultenv) as postgrest:
        for _ in range(10):
            response = postgrest.session.get("/authors_only", headers=headers)
            assert response.status_code == 200

            time.sleep(0.5)


def test_app_settings(defaultenv):
    """
    App settings should not reset when the db pool times out.

    See: https://github.com/PostgREST/postgrest/issues/1141

    """
    with run(CONFIGSDIR / "app-settings.config", env=defaultenv) as postgrest:
        # Wait for the db pool to time out, set to 1s in config
        time.sleep(2)

        uri = "/rpc/get_guc_value?name=app.settings.external_api_secret"
        response = postgrest.session.get(uri)

        assert response.text == '"0123456789abcdef"'


def test_app_settings_reload(tmp_path, defaultenv):
    "App settings should be reloaded when PostgREST is sent SIGUSR2."
    config = (CONFIGSDIR / "sigusr2-settings.config").read_text()
    configfile = tmp_path / "test.config"
    configfile.write_text(config)
    uri = "/rpc/get_guc_value?name=app.settings.name_var"

    with run(configfile, env=defaultenv) as postgrest:
        response = postgrest.session.get(uri)
        assert response.text == '"John"'

        # change setting
        configfile.write_text(config.replace("John", "Jane"))
        # reload
        postgrest.process.send_signal(signal.SIGUSR2)

        time.sleep(0.1)

        response = postgrest.session.get(uri)
        assert response.text == '"Jane"'


def test_jwt_secret_reload(tmp_path, defaultenv):
    "JWT secret should be reloaded when PostgREST is sent SIGUSR2."
    config = (CONFIGSDIR / "sigusr2-settings.config").read_text()
    configfile = tmp_path / "test.config"
    configfile.write_text(config)

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    with run(configfile, env=defaultenv) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 401

        # change setting
        configfile.write_text(config.replace("invalid" * 5, SECRET))

        # reload config
        postgrest.process.send_signal(signal.SIGUSR2)

        time.sleep(0.1)

        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 200


def test_jwt_secret_external_file_reload(tmp_path, defaultenv):
    "JWT secret external file should be reloaded when PostgREST is sent a SIGUSR2 or a NOTIFY."
    config = CONFIGSDIR / "sigusr2-settings-external-secret.config"

    headers = jwtauthheader({"role": "postgrest_test_author"}, SECRET)

    external_secret_file = tmp_path / "jwt-secret-config"
    external_secret_file.write_text("invalid" * 5)

    env = {
        **defaultenv,
        "JWT_SECRET_FILE": f"@{external_secret_file}",
        "PGRST_DB_CHANNEL_ENABLED": "true",
    }

    with run(config, env=env) as postgrest:
        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 401

        # change external file
        external_secret_file.write_text(SECRET)

        # SIGUSR1 doesn't reload external files
        postgrest.process.send_signal(signal.SIGUSR1)
        time.sleep(0.1)

        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 401

        # reload config and external file with SIGUSR2
        postgrest.process.send_signal(signal.SIGUSR2)
        time.sleep(0.1)

        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 200

        # change external file to wrong value again
        external_secret_file.write_text("invalid" * 5)

        # reload config and external file with NOTIFY
        postgrest.session.post("/rpc/reload_pgrst_config")
        time.sleep(0.1)

        response = postgrest.session.get("/authors_only", headers=headers)
        assert response.status_code == 401


def test_db_schema_reload(tmp_path, defaultenv):
    "DB schema should be reloaded when PostgREST is sent SIGUSR2."
    config = (CONFIGSDIR / "sigusr2-settings.config").read_text()
    configfile = tmp_path / "test.config"
    configfile.write_text(config)

    env = {key: value for key, value in defaultenv.items() if key != "PGRST_DB_SCHEMAS"}

    with run(configfile, env=env) as postgrest:
        response = postgrest.session.get("/rpc/get_guc_value?name=search_path")
        assert response.text == '"public, public"'

        # change setting
        configfile.write_text(
            config.replace('db-schemas = "public"', 'db-schemas = "v1"')
        )

        # reload config
        postgrest.process.send_signal(signal.SIGUSR2)

        # reload schema cache to verify that the config reload actually happened
        postgrest.process.send_signal(signal.SIGUSR1)

        time.sleep(0.1)

        response = postgrest.session.get("/rpc/get_guc_value?name=search_path")
        assert response.text == '"v1, public"'


def test_db_schema_notify_reload(defaultenv):
    "DB schema and config should be reloaded when PostgREST is sent a NOTIFY"

    env = {**defaultenv, "PGRST_DB_CONFIG": "true", "PGRST_DB_CHANNEL_ENABLED": "true"}

    with run(env=env) as postgrest:
        response = postgrest.session.get("/rpc/get_guc_value?name=search_path")
        assert response.text == '"public, public"'

        # change db-schemas config on the db and reload config and cache with notify
        postgrest.session.post(
            "/rpc/change_db_schema_and_full_reload", data={"schemas": "v1"}
        )

        time.sleep(0.5)

        response = postgrest.session.get("/rpc/get_guc_value?name=search_path")
        assert response.text == '"v1, public"'

        # reset db-schemas config on the db
        response = postgrest.session.post("/rpc/reset_db_schema_config")
        assert response.status_code == 200


def test_max_rows_reload(defaultenv):
    "max-rows should be reloaded from role settings when PostgREST receives a SIGUSR2."
    config = CONFIGSDIR / "sigusr2-settings.config"

    env = {
        **defaultenv,
        "PGRST_DB_CONFIG": "true",
    }

    with run(config, env=env) as postgrest:
        response = postgrest.session.head("/projects")
        assert response.status_code == 200
        assert response.headers["Content-Range"] == "0-4/*"

        # change max-rows config on the db
        postgrest.session.post("/rpc/change_max_rows_config", data={"val": 1})

        # reload config
        postgrest.process.send_signal(signal.SIGUSR2)

        time.sleep(0.1)

        response = postgrest.session.head("/projects")
        assert response.status_code == 200
        assert response.headers["Content-Range"] == "0-0/*"

        # reset max-rows config on the db
        response = postgrest.session.post("/rpc/reset_max_rows_config")
        assert response.status_code == 200


def test_max_rows_notify_reload(defaultenv):
    "max-rows should be reloaded from role settings when PostgREST receives a NOTIFY"

    env = {
        **defaultenv,
        "PGRST_DB_CONFIG": "true",
        "PGRST_DB_CHANNEL_ENABLED": "true",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.head("/projects")
        assert response.status_code == 200
        assert response.headers["Content-Range"] == "0-4/*"

        # change max-rows config on the db and reload with notify
        postgrest.session.post(
            "/rpc/change_max_rows_config", data={"val": 1, "notify": True}
        )

        time.sleep(0.1)

        response = postgrest.session.head("/projects")
        assert response.status_code == 200
        assert response.headers["Content-Range"] == "0-0/*"

        # reset max-rows config on the db
        response = postgrest.session.post("/rpc/reset_max_rows_config")
        assert response.status_code == 200


def test_invalid_role_claim_key_notify_reload(defaultenv):
    "NOTIFY reload config should show an error if role-claim-key is invalid"

    env = {
        **defaultenv,
        "PGRST_DB_CONFIG": "true",
        "PGRST_DB_CHANNEL_ENABLED": "true",
        "PGRST_LOG_LEVEL": "crit",
    }

    with run(env=env) as postgrest:
        postgrest.session.post("/rpc/invalid_role_claim_key_reload")

        output = None
        for _ in range(10):
            output = postgrest.process.stdout.readline()
            if output:
                break
            time.sleep(0.1)

        assert "failed to parse role-claim-key value" in output.decode()

        response = postgrest.session.post("/rpc/reset_invalid_role_claim_key")
        assert response.status_code == 200


def test_db_prepared_statements_enable(defaultenv):
    "Should use prepared statements when the setting is enabled."

    with run(env=defaultenv) as postgrest:
        response = postgrest.session.post("/rpc/uses_prepared_statements")
        assert response.text == "true"


def test_db_prepared_statements_disable(defaultenv):
    "Should not use any prepared statements when the setting is disabled."

    env = {
        **defaultenv,
        "PGRST_DB_PREPARED_STATEMENTS": "false",
    }

    with run(env=env) as postgrest:
        response = postgrest.session.post("/rpc/uses_prepared_statements")
        assert response.text == "false"
