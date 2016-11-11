#!/bin/bash

MAX_CACHE_AGE=604800 # seconds
DOCKER_REPO=${DOCKER_REPO:-postgrest}

if [[ -e ~/.docker/image.tar ]]; then
  stat -c %Y ~/.docker/image.tar;
  let "AGE=$(date +%s) - $(stat -c %Y ~/.docker/image.tar)";

  echo "Cached image file is: ${AGE} < ${MAX_CACHE_AGE}";

  if [[ $AGE -lt $MAX_CACHE_AGE ]]; then
    echo "Loading image from tar...";
    docker load --input ~/.docker/image.tar;
    docker images -a --digests;

  else
    echo "Cache invalidated. Deleting image... ";
    rm -rf ~/.docker;
  fi
fi

if [[ ! -e ~/.docker/image.tar ]]; then
  echo "Building image..."
  docker-compose build development;
  docker images -a --digests;

  echo "Building Complete.";

  mkdir -p ~/.docker;
  echo "Saving image..."
  docker save ${DOCKER_REPO}dev > ~/.docker/image.tar;
fi
