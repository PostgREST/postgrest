#!/bin/bash

# This script builds PostgREST in a remote ARM server. It uses Docker to
# build for multiple platforms (aarch64 and armv7 on ubuntu).
# The Dockerfile is located in ./docker-env

[ -z "$1" ] && { echo "Missing 1st argument: PostgREST github commit SHA"; exit 1; }
[ -z "$2" ] && { echo "Missing 2nd argument: Docker repo"; exit 1; }
[ -z "$3" ] && { echo "Missing 3rd argument: Docker username"; exit 1; }
[ -z "$4" ] && { echo "Missing 4th argument: Docker password"; exit 1; }
[ -z "$5" ] && { echo "Missing 5th argument: Build environment directory name"; exit 1; }

PGRST_GITHUB_COMMIT="$1"
DOCKER_REPO="$2"
DOCKER_USER="$3"
DOCKER_PASS="$4"
SCRIPT_PATH="$5"

DOCKER_BUILD_PATH="$SCRIPT_PATH/docker-env"

clean_env()
{
    sudo docker logout
}

# Login to Docker
sudo docker logout
{ echo $DOCKER_PASS | sudo docker login -u $DOCKER_USER --password-stdin; } || { echo "Couldn't login to docker"; exit 1; }

trap clean_env sigint sigterm exit

# Move to the docker build environment
cd ~/$DOCKER_BUILD_PATH

# Build ARM versions
sudo docker buildx build --build-arg PGRST_GITHUB_COMMIT=$PGRST_GITHUB_COMMIT \
                         --build-arg BUILDKIT_INLINE_CACHE=1 \
                         --platform linux/arm/v7,linux/arm64 \
                         --cache-from $DOCKER_REPO/postgrest-build-arm \
                         --target=postgrest-build \
                         -t $DOCKER_REPO/postgrest-build-arm \
                         --push .

sudo docker logout

# Generate and copy binaries to the local filesystem
sudo docker buildx build --build-arg PGRST_GITHUB_COMMIT=$PGRST_GITHUB_COMMIT \
                         --cache-from $DOCKER_REPO/postgrest-build-arm \
                         --platform linux/arm/v7,linux/arm64 \
                         --target=postgrest-bin \
                         -o result .

# Compress binaries
sudo chown -R ubuntu:ubuntu ~/$DOCKER_BUILD_PATH/result
mv ~/$DOCKER_BUILD_PATH/result ~/$SCRIPT_PATH/result
cd ~/$SCRIPT_PATH
tar -cJf result.tar.xz result
