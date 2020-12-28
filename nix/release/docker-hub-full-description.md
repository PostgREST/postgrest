# PostgREST

[![Join the chat at https://gitter.im/begriffs/postgrest](https://img.shields.io/badge/gitter-join%20chat%20%E2%86%92-brightgreen.svg)](https://gitter.im/begriffs/postgrest)
[![Donate](https://img.shields.io/badge/Donate-Patreon-orange.svg?colorB=F96854)](https://www.patreon.com/postgrest)
[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.me/postgrest)
[![Docs](https://img.shields.io/badge/docs-latest-brightgreen.svg?style=flat)](http://postgrest.org)
[![Build Status](https://circleci.com/gh/PostgREST/postgrest/tree/main.svg?style=shield)](https://circleci.com/gh/PostgREST/postgrest/tree/main)

PostgREST serves a fully RESTful API from any existing PostgreSQL database. It
provides a cleaner, more standards-compliant, faster API than you are likely to
write from scratch.

# Usage

To learn how to use this container, see the [PostgREST Docker
documentation](https://postgrest.com/en/stable/install.html#docker).

You can configure the PostgREST image by setting
[enviroment variables](https://postgrest.org/en/stable/configuration.html).

# How this image is built

The image is built from scratch using
[Nix](https://nixos.org/nixpkgs/manual/#sec-pkgs-dockerTools) instead of a
`Dockerfile`, which yields a higly secure and optimized image. This is also why
no commands are listed in the image history. See the [PostgREST
respository](https://github.com/PostgREST/postgrest/tree/main/nix/docker) for
details on the build process and how to inspect the image.
