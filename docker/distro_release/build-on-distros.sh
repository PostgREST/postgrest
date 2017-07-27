#!/bin/bash

build_on_image() {
  echo "Building project on $1 docker image"
  docker run -it -v $HOME/.$1-stack:/root/.stack -v $(pwd):/source -v /source/.stack-work -v /tmp/bin/$1:/root/.local/bin/ $1 build --allow-different-user --copy-bins
}

build_on_image centos6
build_on_image centos7
build_on_image ubuntu
build_on_image ubuntui386
