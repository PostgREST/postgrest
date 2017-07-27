#!/bin/bash

load_or_build_image() {
  if [[ -e ~/docker/$1.tar ]]; then
    echo "Loading $1 docker image from cache"
    docker load -i ~/docker/$1.tar
  else
    echo "Building $1 docker image"
    docker build --rm=false -t $1 -f docker/distro_release/Dockerfile.$1 docker/distro_release/
    docker save $1 > ~/docker/$1.tar
  fi
}

load_or_build_image centos6
load_or_build_image centos7
load_or_build_image ubuntu
load_or_build_image ubuntui386
