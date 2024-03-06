# PostgREST docker hub image

FROM ubuntu:jammy@sha256:77906da86b60585ce12215807090eb327e7386c8fafb5402369e421f44eff17e AS postgrest

RUN apt-get update -y \
    && apt install -y --no-install-recommends libpq-dev zlib1g-dev jq gcc libnuma-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

COPY postgrest /usr/bin/postgrest

EXPOSE 3000

USER 1000

CMD postgrest
