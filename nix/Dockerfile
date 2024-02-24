# This Dockerfile is only used as a development environment for
# non-nix systems, i.e. Windows.

FROM nixos/nix:latest@sha256:273671b9032d06844ec073affa9fb1e75bf7ad863975bec5143e7fa417929fb8

RUN apk --no-cache add \
    wget

RUN nix-env -iA cachix -f https://cachix.org/api/v1/install \
 && cachix use postgrest

# We need an unprivileged user here, to make PG run at all.
RUN adduser --disabled-password --ingroup root nix \
 && chown -R nix:root /nix
USER nix:root
ENV USER=nix

VOLUME /nix
VOLUME /postgrest
WORKDIR /postgrest

CMD nix-shell
