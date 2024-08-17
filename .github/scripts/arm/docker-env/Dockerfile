# PostgREST docker hub image

FROM ubuntu:noble@sha256:8a37d68f4f73ebf3d4efafbcf66379bf3728902a8038616808f04e34a9ab63ee AS postgrest

RUN apt-get update -y \
    && apt install -y --no-install-recommends libpq-dev zlib1g-dev jq gcc libnuma-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

COPY postgrest /usr/bin/postgrest

EXPOSE 3000

USER 1000

# Use the array form to avoid running the command using bash, which does not handle `SIGTERM` properly. 
# See https://docs.docker.com/compose/faq/#why-do-my-services-take-10-seconds-to-recreate-or-stop 
CMD ["postgrest"]
