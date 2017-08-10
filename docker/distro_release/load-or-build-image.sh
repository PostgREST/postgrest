#!/bin/bash
case $1 in
  centos6|centos7|ubuntu|ubuntui386)
    if [[ -e ~/docker/$1.tar ]]; then
      echo "Loading $1 docker image from cache"
      docker load -i ~/docker/$1.tar
    else
      echo "Building $1 docker image"
      docker build --rm=false -t $1 -f docker/distro_release/Dockerfile.$1 docker/distro_release/
      docker save $1 > ~/docker/$1.tar
    fi
  ;;
  *)
    echo "No valid image specified."
    exit 1
  ;;
esac
