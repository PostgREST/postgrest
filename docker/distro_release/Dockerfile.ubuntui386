FROM 32bit/ubuntu:16.04

RUN BUILD_DEPS="curl ca-certificates build-essential" && \
    apt-get -qq update && \
    apt-get -qqy --no-install-recommends install \
    $BUILD_DEPS \
    libpq-dev && \
    curl -sSL https://get.haskellstack.org/ | sh && \
    apt-get -qq clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# To disable warning when building
ENV PATH $PATH:/root/.local/bin

RUN mkdir /source
WORKDIR /source

ENTRYPOINT ["stack"]
