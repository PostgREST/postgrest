FROM debian:jessie

ENV POSTGREST_VERSION 0.4.1.0

RUN apt-get update && \
    apt-get install -y tar xz-utils wget libpq-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN wget http://github.com/begriffs/postgrest/releases/download/v${POSTGREST_VERSION}/postgrest-${POSTGREST_VERSION}-ubuntu.tar.xz && \
    tar --xz -xvf postgrest-${POSTGREST_VERSION}-ubuntu.tar.xz && \
    mv postgrest /usr/local/bin/postgrest && \
    rm postgrest-${POSTGREST_VERSION}-ubuntu.tar.xz

COPY postgrest.conf /etc/postgrest.conf


ENV PGRST_DB_URI= \
    PGRST_DB_SCHEMA=public \
    PGRST_DB_ANON_ROLE= \
    PGRST_DB_POOL=100 \
    PGRST_SERVER_HOST=*4 \
    PGRST_SERVER_PORT=3000 \
    PGRST_SERVER_PROXY_URL= \
    PGRST_JWT_SECRET= \
    PGRST_SECRET_IS_BASE64=false \
    PGRST_MAX_ROWS= \
    PGRST_PRE_REQUEST=

# PostgREST reads /etc/postgrest.conf so map the configuration
# file in when you run this container
CMD exec postgrest /etc/postgrest.conf

EXPOSE 3000
