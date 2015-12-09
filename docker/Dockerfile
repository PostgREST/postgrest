FROM debian:8.2

ARG POSTGREST_VERSION

# Shorthands to make commands easier to read
ENV POSTGREST_DOWNLOAD postgrest-${POSTGREST_VERSION}-ubuntu.tar.xz
ENV POSTGREST_RELEASES_URL http://github.com/begriffs/postgrest/releases/download/v${POSTGREST_VERSION}

RUN apt-get update \
  && apt-get install -y xz-utils wget libpq-dev \
  && wget ${POSTGREST_RELEASES_URL}/${POSTGREST_DOWNLOAD} \
  && tar xf ${POSTGREST_DOWNLOAD} \
  && mv postgrest /usr/local/bin/postgrest \
  && rm -f ${POSTGREST_DOWNLOAD} \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Public env vars, can be passed in as params
ENV POSTGREST_DB_HOST db
ENV POSTGREST_DB_PORT 5432
ENV POSTGREST_DB_USER postgres
ENV POSTGREST_DB_NAME ${POSTGREST_DB_USER}
ENV POSTGREST_ANON_USER ${POSTGREST_DB_USER}
ENV POSTGREST_DB_PASS ${POSTGREST_DB_USER}
ENV POSTGREST_CONNECTION_POOL_SIZE 10
ENV POSTGREST_JWT_SECRET secret
ENV POSTGREST_SCHEMA public

# Connstring shorthand variable
ENV CONNSTRING postgres://${POSTGREST_DB_USER}:${POSTGREST_DB_PASS}@${POSTGREST_DB_HOST}:${POSTGREST_DB_PORT}/${POSTGREST_DB_NAME}

CMD postgrest ${CONNSTRING} \
              --port 3000 \
              --anonymous ${POSTGREST_ANON_USER} \
              --schema ${POSTGREST_SCHEMA} \
              --jwt-secret ${POSTGREST_JWT_SECRET}

EXPOSE 3000
