# This Dockerfile is only used as a development environment for
# non-nix systems, i.e. Windows.

FROM nixos/nix:latest

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
