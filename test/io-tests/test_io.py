'Tests for inputs and outputs of PostgREST.'

import pathlib
import subprocess
import os

import pytest
import requests


basedir = pathlib.Path(os.path.realpath(__file__)).parent
expectedconfigs = list((basedir / 'configs' / 'expected').iterdir())


@pytest.fixture(params=expectedconfigs)
def expectedconfig(request):
    'Fixture for all expected configs'
    return request.param


def dumpconfig(configpath):
    'Dump a config as parsed by PostgREST.'

    command = ['postgrest', '--dump-config', configpath]
    return subprocess.run(command, capture_output=True, check=True).stdout.decode('utf-8')


def test_expected_config(expectedconfig):
    'PostgREST should parse configs as expected.'

    expected = (basedir / 'configs' / 'expected' / expectedconfig).read_text()

    assert dumpconfig(basedir / 'configs' / expectedconfig) == expected
