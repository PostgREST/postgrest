#!/bin/bash
case $1 in
  centos6|centos7|ubuntu|ubuntui386)
    echo "Building project on $1 docker container"
    docker run -it \
      -v $HOME/.$1-stack:/root/.stack \
      -v $(pwd):/source -v /source/.stack-work \
      -v /tmp/bin/$1:/root/.local/bin/ \
      $1 build --allow-different-user --copy-bins
  ;;
  *)
    echo "No valid container specified."
    exit 1
  ;;
esac
