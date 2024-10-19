# PostgREST docker hub image

FROM ubuntu:noble@sha256:99c35190e22d294cdace2783ac55effc69d32896daaa265f0bbedbcde4fbe3e5 AS postgrest

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
