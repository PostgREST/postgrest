FROM debian:jessie

ENV POSTGREST_VERSION ${POSTGREST_VERSION:-0.4.0.0}

RUN apt-get update && \
    apt-get install -y tar xz-utils wget libpq-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir -p /etc/postgrest

COPY .release/postgrest /usr/local/bin/postgrest

CMD ["/usr/local/bin/postgrest", "-c", "/etc/postgrest/postgrest.conf"]

VOLUME ["/etc/postgrest"]

EXPOSE 3000
