FROM ubuntu:14.04

ENV POSTGREST_VERSION 0.3.0.4
ENV POSTGREST_SCHEMA public
ENV POSTGREST_ANONYMOUS postgres
ENV POSTGREST_JWT_SECRET thisisnotarealsecret
ENV POSTGREST_MAX_ROWS 1000000
ENV POSTGREST_POOL 200

RUN apt-get update
RUN apt-get install -y tar xz-utils wget libpq-dev

RUN wget http://github.com/begriffs/postgrest/releases/download/v${POSTGREST_VERSION}/postgrest-${POSTGREST_VERSION}-ubuntu.tar.xz
RUN tar --xz -xvf postgrest-${POSTGREST_VERSION}-ubuntu.tar.xz
RUN mv postgrest /usr/local/bin/postgrest

CMD postgrest postgres://${PG_ENV_POSTGRES_USER}:${PG_ENV_POSTGRES_PASSWORD}@${PG_PORT_5432_TCP_ADDR}:${PG_PORT_5432_TCP_PORT}/${PG_ENV_POSTGRES_DB} \
              --port 3000 \
              --schema ${POSTGREST_SCHEMA} \
              --anonymous ${POSTGREST_ANONYMOUS} \
              --pool ${POSTGREST_POOL} \
              --jwt-secret ${POSTGREST_JWT_SECRET} \
              --max-rows ${POSTGREST_MAX_ROWS}

EXPOSE 3000

RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
