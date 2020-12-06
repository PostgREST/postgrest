'Tests for inputs and outputs of PostgREST.'

import subprocess
import os

import requests


basedir = os.path.dirname(os.path.realpath(__file__))
expectedconfigs = os.listdir(os.path.join(basedir, 'configs/expected'))


def dumpconfig(configpath):
    'Dump a config as parsed by PostgREST.'

    command = ['postgrest', '--dump-config', configpath]
    return subprocess.run(command, capture_output=True, check=True).stdout.decode('utf-8')


def test_configs():
    'PostgREST should parse configs as expected.'

    for config in expectedconfigs:
        expectedpath = os.path.join(basedir, 'configs', 'expected', config)
        with open(expectedpath) as expectedfile:
            expected = expectedfile.read()

        assert dumpconfig(os.path.join(basedir, 'configs', config)) == expected
