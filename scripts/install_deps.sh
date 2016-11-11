#!/bin/bash

set -e

apt-get update
apt-get install -y libpq-dev
apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

cabal update
stack install --allow-different-user hlint packdeps
stack build --allow-different-user --fast
stack build --allow-different-user --fast --test --no-run-tests
rm -fr $(stack path --dist-dir)
