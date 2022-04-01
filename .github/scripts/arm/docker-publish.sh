#!/bin/bash

[ -z "$1" ] && { echo "Missing 1st argument: PostgREST github commit SHA"; exit 1; }
[ -z "$2" ] && { echo "Missing 2nd argument: Docker repo"; exit 1; }
[ -z "$3" ] && { echo "Missing 3rd argument: Docker username"; exit 1; }
[ -z "$4" ] && { echo "Missing 4th argument: Docker password"; exit 1; }
[ -z "$5" ] && { echo "Missing 5th argument: Build environment directory name"; exit 1; }
[ -z "$6" ] && { echo "Missing 6th argument: PostgREST version"; exit 1; }

PGRST_GITHUB_COMMIT="$1"
DOCKER_REPO="$2"
DOCKER_USER="$3"
DOCKER_PASS="$4"
DOCKER_BUILD_PATH="$5"
PGRST_VERSION="$6"
IS_PRERELEASE="$7"

clean_env()
{
    sudo docker logout
}

# Login to Docker
sudo docker logout
{ echo $DOCKER_PASS | sudo docker login -u $DOCKER_USER --password-stdin; } || { echo "Couldn't login to docker"; exit 1; }

trap clean_env sigint sigterm exit

# Move to the docker build environment
cd ~/DOCKER_BUILD_PATH

# Push final images to Docker hub
sudo docker buildx build --build-arg PGRST_GITHUB_COMMIT=$PGRST_GITHUB_COMMIT \
                         --platform linux/arm/v7,linux/arm64 \
                         --cache-from $DOCKER_REPO/postgrest:postgrest-build-arm \
                         -t $DOCKER_REPO/postgrest:$PGRST_VERSION-arm \
                         --push .

# Add the arm images to the manifest
sudo docker buildx imagetools create --append -t $DOCKER_REPO/postgrest:$PGRST_VERSION $DOCKER_REPO/postgrest:$PGRST_VERSION-arm
[ -z $IS_PRERELEASE ] && sudo docker buildx imagetools create --append -t $DOCKER_REPO/postgrest:latest $DOCKER_REPO/postgrest:$PGRST_VERSION-arm

# NOTE: Need to manually delete the arm images from Docker Hub

sudo docker logout
