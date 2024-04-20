# PostgREST docker hub image

FROM ubuntu:jammy@sha256:1b8d8ff4777f36f19bfe73ee4df61e3a0b789caeff29caa019539ec7c9a57f95 AS postgrest

RUN apt-get update -y \
    && apt install -y --no-install-recommends libpq-dev zlib1g-dev jq gcc libnuma-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

COPY postgrest /usr/bin/postgrest

EXPOSE 3000

USER 1000

CMD postgrest
