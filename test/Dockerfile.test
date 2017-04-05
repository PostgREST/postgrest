FROM debian:jessie

ENV PATH /root/.local/bin:$PATH

RUN apt-get update \
    && apt-get install -y wget libpq-dev pkg-config libpcre3 libpcre3-dev \
       postgresql-client debconf locales \
    && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
    && echo 'en_US.UTF-8 UTF-8' > /etc/locale.gen \
    && locale-gen \
    && echo 'export LC_ALL=en_US.UTF-8' >> /etc/profile \
    && wget -qO- https://get.haskellstack.org/ | sh

