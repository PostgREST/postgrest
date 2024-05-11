"Fixtures to run PostgREST as a server."

import contextlib
import dataclasses
import os
import pathlib
import socket
import subprocess
import tempfile
import time
import urllib.parse

import pytest
import requests
import requests_unixsocket

from config import *


def sleep_until_postgrest_scache_reload():
    "Sleep until schema cache reload"
    time.sleep(0.3)


def sleep_until_postgrest_config_reload():
    "Sleep until config reload"
    time.sleep(0.2)


def sleep_until_postgrest_full_reload():
    "Sleep until schema cache plus config reload"
    time.sleep(0.3)


class PostgrestTimedOut(Exception):
    "Connecting to PostgREST endpoint timed out."


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
    "Running PostgREST process and its corresponding main and admin endpoints."
    admin: object
    process: object
    session: object

    def read_stdout(self, nlines=1):
        "Wait for line(s) on standard output."
        output = []
        for _ in range(10):
            self.process.stdout.flush()
            l = self.process.stdout.readline()
            if l:
                output.append(l.decode())
                if len(output) >= nlines:
                    break
            time.sleep(0.1)
        return output

    def wait_until_scache_starts_loading(self, max_seconds=1):
        "Wait for the admin /ready return a status of 503"

        wait_until_status_code(
            self.admin.baseurl + "/ready", max_seconds=max_seconds, status_code=503
        )


@contextlib.contextmanager
def run(
    configpath=None,
    stdin=None,
    env=None,
    port=None,
    host=None,
    wait_for_readiness=True,
    wait_max_seconds=1,
    no_pool_connection_available=False,
    no_startup_stdout=True,
):
    "Run PostgREST and yield an endpoint that is ready for connections."

    with tempfile.TemporaryDirectory() as tmpdir:
        if port:
            env["PGRST_SERVER_PORT"] = str(port)
            env["PGRST_SERVER_HOST"] = host or "localhost"
            baseurl = f"http://localhost:{port}"
        else:
            socketfile = pathlib.Path(tmpdir) / "postgrest.sock"
            env["PGRST_SERVER_UNIX_SOCKET"] = str(socketfile)
            baseurl = "http+unix://" + urllib.parse.quote_plus(str(socketfile))

        adminport = freeport(port)
        env["PGRST_ADMIN_SERVER_PORT"] = str(adminport)
        adminurl = f"http://localhost:{adminport}"

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

            if wait_for_readiness:
                wait_until_status_code(adminurl + "/ready", wait_max_seconds, 200)

            if no_startup_stdout:
                process.stdout.read()

            if no_pool_connection_available:
                sleep_pool_connection(baseurl, 10)

            yield PostgrestProcess(
                process=process,
                session=PostgrestSession(baseurl),
                admin=PostgrestSession(adminurl),
            )
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


@pytest.fixture(scope="module")
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


def freeport(used_port=None):
    "Find a free port on localhost."
    while True:
        with contextlib.closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as s:
            s.bind(("", 0))
            s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            port = s.getsockname()[1]
            if port != used_port:
                return port


def wait_until_exit(postgrest):
    "Wait for PostgREST to exit, or times out"
    try:
        return postgrest.process.wait(timeout=1)
    except subprocess.TimeoutExpired:
        raise PostgrestTimedOut()


def wait_until_status_code(url, max_seconds, status_code):
    "Wait for the given HTTP endpoint to return a status code"
    session = requests_unixsocket.Session()

    for _ in range(max_seconds * 10):
        try:
            response = session.get(url, timeout=1)
            if response.status_code == status_code:
                return
        except (requests.ConnectionError, requests.ReadTimeout):
            pass

        time.sleep(0.1)

    if response:
        raise PostgrestTimedOut(f"{response.status_code}: {response.text}")
    else:
        raise PostgrestTimedOut()


def sleep_pool_connection(url, seconds):
    "Sleep a pool connection by calling an RPC that uses pg_sleep"
    session = requests_unixsocket.Session()

    # The try/except is a hack for not waiting for the response,
    # taken from https://stackoverflow.com/a/45601591/4692662
    try:
        session.get(url + f"/rpc/sleep?seconds={seconds}", timeout=0.1)
    except requests.exceptions.ReadTimeout:
        pass
