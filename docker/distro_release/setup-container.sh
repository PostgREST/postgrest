#!/bin/bash
case $1 in
  centos6|centos7|ubuntu|ubuntui386)
    echo "Setup stack dependencies on $1 docker container"
    docker run -it \
      -v $HOME/.$1-stack:/root/.stack \
      -v $(pwd):/source -v /source/.stack-work \
      $1 build --allow-different-user --only-snapshot --install-ghc
  ;;
  *)
    echo "No valid container specified."
    exit 1
  ;;
esac
