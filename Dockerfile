FROM debian:jessie

ENV POSTGREST_VERSION 0.3.1.1
ENV POSTGREST_SCHEMA public
ENV POSTGREST_ANONYMOUS postgres
ENV POSTGREST_JWT_SECRET thisisnotarealsecret
ENV POSTGREST_MAX_ROWS 1000000
ENV POSTGREST_POOL 200

RUN apt-get update && \
    apt-get install -y tar xz-utils wget libpq-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN wget http://github.com/begriffs/postgrest/releases/download/v${POSTGREST_VERSION}/postgrest-${POSTGREST_VERSION}-ubuntu.tar.xz && \
    tar --xz -xvf postgrest-${POSTGREST_VERSION}-ubuntu.tar.xz && \
    mv postgrest /usr/local/bin/postgrest && \
    rm postgrest-${POSTGREST_VERSION}-ubuntu.tar.xz

CMD exec postgrest postgres://${PG_ENV_POSTGRES_USER}:${PG_ENV_POSTGRES_PASSWORD}@${PG_PORT_5432_TCP_ADDR}:${PG_PORT_5432_TCP_PORT}/${PG_ENV_POSTGRES_DB} \
              --port 3000 \
              --schema ${POSTGREST_SCHEMA} \
              --anonymous ${POSTGREST_ANONYMOUS} \
              --pool ${POSTGREST_POOL} \
              --jwt-secret ${POSTGREST_JWT_SECRET} \
              --max-rows ${POSTGREST_MAX_ROWS}

EXPOSE 3000
