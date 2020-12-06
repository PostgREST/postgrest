'Tests for inputs and outputs of PostgREST.'

import pathlib
import subprocess
import tempfile
import os

import pytest
import requests


basedir = pathlib.Path(os.path.realpath(__file__)).parent
configs = [path for path in (basedir / 'configs').iterdir() if path.is_file()]
expectedconfigs = list((basedir / 'configs' / 'expected').iterdir())


@pytest.fixture(params=configs, ids=[conf.name for conf in configs])
def configpath(request):
    'Fixture for all config paths.'
    return basedir / 'configs' / request.param


@pytest.fixture(params=expectedconfigs, ids=[conf.name for conf in expectedconfigs])
def expectedconfig(request):
    'Fixture for all expected configs.'
    return request.param


def dumpconfig(configpath, moreenv=None):
    'Dump the config as parsed by PostgREST.'

    env = os.environ
    if moreenv:
        env = {**env, **moreenv}

    command = ['postgrest', '--dump-config', configpath]
    result = subprocess.run(command, env=env, capture_output=True, check=True)
    return result.stdout.decode('utf-8')


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
