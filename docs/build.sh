#!/usr/bin/env bash
set -euo pipefail

# sphinx-intl fails if LC_ALL is not set
export LC_ALL=${LC_ALL:-C}

function build() {
  sphinx-build --color -W -a -n . -b "$@"
}

if [ $# -eq 0 ]; then
  # clean previous build, otherwise some errors might be supressed
  rm -rf "_build/html/default"

  if [ -d languages ]; then
    # default to updating all existing locales
    build gettext _build/gettext
    sphinx-intl update -p _build/gettext
  fi

  build html "_build/html/default"
else
  # clean previous build, otherwise some errors might be supressed
  rm -rf "_build/html/$1"

  # update and build specific locale, can be used to create new locale
  build gettext _build/gettext
  sphinx-intl update -p _build/gettext -l "$1"

  build html "_build/html/$1" -D "language=$1"
fi
