'Tests for inputs and outputs of PostgREST.'

import contextlib
import pathlib
import subprocess
import tempfile
import os
import time

import pytest
import requests

BASEURL = 'http://127.0.0.1:49421'


basedir = pathlib.Path(os.path.realpath(__file__)).parent
configs = [path for path in (basedir / 'configs').iterdir() if path.is_file()]
expectedconfigs = list((basedir / 'configs' / 'expected').iterdir())
secrets = [path for path in (basedir / 'secrets').iterdir() if path.suffix != '.jwt']
dburi = os.getenv('POSTGREST_TEST_CONNECTION')
dburifromfileconfig = basedir / 'configs' / 'dburi-from-file.config'
roleclaimkeyconfig = basedir / 'configs' / 'role-claim-key.config'

invalidroleclaimkeys = [
        'role.other',
        '.role##',
        '.my_role;;domain',
        '.#$$%&$%/',
        '',
        '1234'
    ]


class TimeOutException(Exception):
    pass


@pytest.fixture(params=configs, ids=[conf.name for conf in configs])
def configpath(request):
    'Fixture for all config paths.'
    return basedir / 'configs' / request.param


@pytest.fixture(params=expectedconfigs, ids=[conf.name for conf in expectedconfigs])
def expectedconfig(request):
    'Fixture for all expected configs.'
    return request.param


@pytest.fixture(params=secrets, ids=[secret.name for secret in secrets])
def secretpath(request):
    'Fixture for all secrets.'
    return request.param


@pytest.fixture(params=invalidroleclaimkeys)
def invalidroleclaimkey(request):
    'Fixture for all secrets.'
    return request.param


def dumpconfig(configpath, moreenv=None):
    'Dump the config as parsed by PostgREST.'
    env = os.environ
    if moreenv:
        env = {**env, **moreenv}

    command = ['postgrest', '--dump-config', configpath]
    result = subprocess.run(command, env=env, capture_output=True, check=True)
    return result.stdout.decode('utf-8')


@contextlib.contextmanager
def run(configpath, stdin=None, moreenv=None):
    'Run PostgREST.'
    env = os.environ
    if moreenv:
        env = {**env, **moreenv}

    command = ['postgrest', configpath]
    process = subprocess.Popen(command, stdin=subprocess.PIPE)

    try:
        if stdin:
            process.stdin.write(stdin)
        process.stdin.close()

        waitfor200(BASEURL)
        yield BASEURL
    finally:
        process.kill()
        process.wait()


def waitfor200(url):
    for i in range(2):
        try:
            response = requests.get(url, timeout=0.1)

            if response.status_code == 200:
                return
        except requests.ConnectionError:
            pass

        time.sleep(.1)

    raise TimeOutException('Waiting for PostgREST ready timed out')


def test_expected_config(expectedconfig):
    '''
    Configs as dumped by PostgREST should match an expected output.

    Used to test default values, config aliases and environment variables. The
    expected output for each file in 'configs', if available, is found in the
    'configs/expected' directory.

    '''
    expected = (basedir / 'configs' / 'expected' / expectedconfig).read_text()
    assert dumpconfig(basedir / 'configs' / expectedconfig) == expected


def test_stable_config(configpath):
    '''
    A dumped, re-read and re-dumped config should match the dumped config.

    Note: only dump vs. re-dump must be equal, as the original config file might
    be different because of default values, whitespace, and quoting.

    '''
    env = {'ROLE_CLAIM_KEY': '."https://www.example.com/roles"[0].value'}
    dumped = dumpconfig(configpath, moreenv=env)

    with tempfile.TemporaryDirectory() as tmpdir:
        tmpconfigpath = pathlib.Path(tmpdir, 'config')
        tmpconfigpath.write_text(dumped)
        redumped = dumpconfig(tmpconfigpath, moreenv=env)

    assert dumped == redumped


def test_read_secret_from_file(secretpath):
    if secretpath.suffix == '.b64':
        configfile = basedir / 'configs' / 'base64-secret-from-file.config'
    else:
        configfile = basedir / 'configs' / 'secret-from-file.config'

    secret = secretpath.read_bytes()

    jwt = secretpath.with_suffix('.jwt').read_text()
    headers = {'Authorization': f'Bearer {jwt}'}

    with run(configfile, stdin=secret) as url:
        response = requests.get(f'{url}/authors_only', headers=headers)
        assert response.status_code == 200


def test_read_dburi_from_file_withouteol():
    with run(dburifromfileconfig, stdin=dburi.encode('utf-8')) as url:
        response = requests.get(f'{url}/')
        assert response.status_code == 200


def test_read_dburi_from_file_witheol():
    with run(dburifromfileconfig, stdin=dburi.encode('utf-8') + b'\n') as url:
        response = requests.get(f'{url}/')
        assert response.status_code == 200


def test_invalid_role_claim_key(invalidroleclaimkey):
    env = {'ROLE_CLAIM_KEY': invalidroleclaimkey}

    with pytest.raises(TimeOutException):
        with run(roleclaimkeyconfig, moreenv=env):
            assert False
