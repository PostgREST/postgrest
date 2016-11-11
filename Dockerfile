FROM debian:jessie

ENV POSTGREST_VERSION ${POSTGREST_VERSION:-0.4.0.0}

RUN mkdir -p /etc/postgrest

COPY .release/postgrest /usr/local/bin/postgrest

CMD ["/usr/local/bin/postgrest", "-c", "/etc/postgrest/postgrest.conf"]

VOLUME ["/etc/postgrest"]

EXPOSE 3000
