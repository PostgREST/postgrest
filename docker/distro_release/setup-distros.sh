#!/bin/bash

setup_image() {
  echo "Setup stack dependencies on $1 docker image"
  docker run -it -v $HOME/.$1-stack:/root/.stack -v $(pwd):/source -v /source/.stack-work $1 build --allow-different-user --only-snapshot --install-ghc
}

setup_image centos6
setup_image centos7
setup_image ubuntu
setup_image ubuntui386
