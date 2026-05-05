# PostgREST Docker Hub image for aarch64.
# The x86-64 is a single-static-binary image built via Nix, see:
# nix/tools/docker/README.md

FROM ubuntu:resolute@sha256:f3d28607ddd78734bb7f71f117f3c6706c666b8b76cbff7c9ff6e5718d46ff64 AS postgrest

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
