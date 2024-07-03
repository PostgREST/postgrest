"Unit tests for Input/Ouput of PostgREST seen as a black box."

import contextlib
import dataclasses
from datetime import datetime
from itertools import repeat
from operator import attrgetter
import os
import pathlib
import re
import shutil
import signal
import socket
import subprocess
import tempfile
import threading
import time
import urllib.parse

import jwt
import pytest
import requests
import requests_unixsocket
from syrupy.extensions.json import SingleFileSnapshotExtension
import yaml

from config import *


class ExtraNewLinesDumper(yaml.SafeDumper):
    "Dumper that inserts an extra newline after each top-level item."

    def write_line_break(self, data=None):
        super().write_line_break(data)
        if len(self.indents) == 1:
            super().write_line_break()


class YamlSnapshotExtension(SingleFileSnapshotExtension):
    _file_extension = "yaml"


@pytest.fixture
def snapshot_yaml(snapshot):
    return snapshot.use_extension(YamlSnapshotExtension)


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


class PostgrestError(Exception):
    "Postgrest exited with a non-zero return code."


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


def test_server_port_and_admin_port_same_value(defaultenv):
    "PostgREST should exit with an error message in output if server-port and admin-server-port are the same."
    env = {**defaultenv, "PGRST_SERVER_PORT": "3000", "PGRST_ADMIN_SERVER_PORT": "3000"}

    with pytest.raises(PostgrestError):
        dump = cli(["--dump-config"], env=env).split("\n")
        assert "admin-server-port cannot be the same as server-port" in dump


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
def test_expected_config_from_db_settings(baseenv, role, expectedconfig):
    "Config should be overriden from database settings"

    config = CONFIGSDIR / "no-defaults.config"

    env = {
        **baseenv,
        "PGUSER": role,
        "PGRST_DB_URI": "postgresql://",
        "PGRST_DB_CONFIG": "true",
    }

    expected = (CONFIGSDIR / "expected" / expectedconfig).read_text()
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


@pytest.mark.parametrize("invalidroleclaimkey", FIXTURES["invalidroleclaimkeys"])
def test_invalid_role_claim_key(invalidroleclaimkey, defaultenv):
    "Given an invalid role-claim-key, Postgrest should exit with a non-zero exit code."
    env = {
        **defaultenv,
        "PGRST_JWT_ROLE_CLAIM_KEY": invalidroleclaimkey,
    }

    with pytest.raises(PostgrestError):
        dump = dumpconfig(env=env)
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


# If this test is failing, run postgrest-test-io --snapshot-update -k test_schema_cache_snapshot
@pytest.mark.parametrize(
    "key",
    [
        "dbMediaHandlers",
        "dbRelationships",
        "dbRepresentations",
        "dbRoutines",
        "dbTables",
        "dbTimezones",
    ],
)
def test_schema_cache_snapshot(baseenv, key, snapshot_yaml):
    "Dump of schema cache should match snapshot."

    schema_cache = yaml.load(cli(["--dump-schema"], env=baseenv), Loader=yaml.Loader)
    formatted = yaml.dump(
        schema_cache[key],
        encoding="utf8",
        allow_unicode=True,
        Dumper=yaml.SafeDumper if key == "dbTimezones" else ExtraNewLinesDumper,
    )
    assert formatted == snapshot_yaml
