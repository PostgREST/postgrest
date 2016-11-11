#!/bin/bash

# Causes script to exit on first non-zero return status
set -e

# Commands run on
# createuser --superuser --no-password postgrest_test
# createdb -O postgrest_test -U ubuntu postgrest_test

stack test --test-arguments "--skip \"returns a valid openapi\""
git ls-files | grep '\.l\?hs$' | xargs stack exec -- hlint -X QuasiQuotes -X NoPatternSynonyms "$@"
stack exec -- cabal update
stack exec --no-ghc-package-path -- cabal install --only-d --dry-run
stack exec -- packdeps *.cabal || true
stack exec -- cabal check
stack  --no-haddock-deps
stack sdist
