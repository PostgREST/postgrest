# PostgREST Docker Hub image for aarch64.
# The x86-64 is a single-static-binary image built via Nix, see:
# nix/tools/docker/README.md

FROM ubuntu:noble@sha256:e356c061e8d56c58b708d0907925a9bd54cc8f467f05bfa3d714d1d0a4883d97 AS postgrest

RUN apt-get update -y \
    && apt install -y --no-install-recommends libpq-dev zlib1g-dev jq gcc libnuma-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

COPY postgrest /usr/bin/postgrest
RUN chmod +x /usr/bin/postgrest

EXPOSE 3000

USER 1000

# Use the array form to avoid running the command using bash, which does not handle `SIGTERM` properly. 
# See https://docs.docker.com/compose/faq/#why-do-my-services-take-10-seconds-to-recreate-or-stop 
CMD ["postgrest"]
