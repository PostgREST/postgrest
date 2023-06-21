#!/bin/bash

# This script builds PostgREST in a remote ARM server

[ -z "$1" ] && { echo "Missing 1st argument: PostgREST github commit SHA"; exit 1; }
[ -z "$2" ] && { echo "Missing 2nd argument: Build environment directory name"; exit 1; }
[ -z "$3" ] && { echo "Missing 3rd argument: GHC version"; exit 1; }

PGRST_GITHUB_COMMIT="$1"
SCRIPT_DIR="$2"

DOCKER_BUILD_DIR="$SCRIPT_DIR/docker-env"
# latest is a shortcut documented on https://www.haskell.org/ghcup/guide/#tags-and-shortcuts
CABAL_VERSION="latest"
GHC_VERSION="$3"

install_packages() {
  sudo apt-get update -y
  sudo apt-get upgrade -y
  sudo apt-get install -y git build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 llvm libnuma-dev zlib1g-dev libpq-dev jq gcc
  sudo apt-get clean
}

install_ghcup() {
  export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
  export BOOTSTRAP_HASKELL_MINIMAL=1
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
  source ~/.ghcup/env
}

install_cabal() {
  ghcup upgrade
  ghcup install cabal $CABAL_VERSION
  ghcup set cabal $CABAL_VERSION
}

install_ghc() {
  ghcup upgrade
  ghcup install ghc $GHC_VERSION
  ghcup set ghc $GHC_VERSION
}

install_packages

# Add ghcup to the PATH for this session
[ -f ~/.ghcup/env ] && source ~/.ghcup/env

ghcup --version || install_ghcup
ghcup set cabal $CABAL_VERSION || install_cabal
ghcup set ghc $GHC_VERSION || install_ghc

cd ~/$SCRIPT_DIR

# Clone the repository and build the project
git clone https://github.com/PostgREST/postgrest.git
cd postgrest
git checkout $PGRST_GITHUB_COMMIT
cabal v2-update && cabal v2-build

# Copy the built binary to the Dockerfile directory
PGRST_BIN=$(cabal exec which postgrest | tail -1)
cp $PGRST_BIN ~/$DOCKER_BUILD_DIR

# Move and compress the built binary
mkdir -p ~/$SCRIPT_DIR/result
mv $PGRST_BIN ~/$SCRIPT_DIR/result
cd ~/$SCRIPT_DIR
tar -cJf result.tar.xz result
