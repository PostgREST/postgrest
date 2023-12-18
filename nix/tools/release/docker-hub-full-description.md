# PostgREST

[![Join the chat at https://gitter.im/begriffs/postgrest](https://img.shields.io/badge/gitter-join%20chat%20%E2%86%92-brightgreen.svg)](https://gitter.im/begriffs/postgrest)
[![Donate](https://img.shields.io/badge/Donate-Patreon-orange.svg?colorB=F96854)](https://www.patreon.com/postgrest)
[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.me/postgrest)
[![Docs](https://img.shields.io/badge/docs-latest-brightgreen.svg?style=flat)](http://postgrest.org)
[![Build Status](https://github.com/postgrest/postgrest/actions/workflows/ci.yaml/badge.svg?branch=main)](https://github.com/PostgREST/postgrest/actions?query=branch%3Amain)

PostgREST serves a fully RESTful API from any existing PostgreSQL database. It
provides a cleaner, more standards-compliant, faster API than you are likely to
write from scratch.

## Sponsors

<table align="center">
  <tbody>
    <tr>
      <td align="center" valign="middle">
        <a href="https://www.cybertec-postgresql.com/en/?utm_source=postgrest.org&utm_medium=referral&utm_campaign=postgrest" target="_blank">
          <img width="296px" src="https://raw.githubusercontent.com/PostgREST/postgrest/main/static/cybertec-new.png">
        </a>
      </td>
      <td align="center" valign="middle">
        <a href="https://gnuhost.eu/?utm_source=sponsor&utm_campaign=postgrest" target="_blank">
          <img width="296px" src="https://raw.githubusercontent.com/PostgREST/postgrest/main/static/gnuhost.png">
        </a>
      </td>
      <td align="center" valign="middle">
        <a href="https://neon.tech/?utm_source=sponsor&utm_campaign=postgrest" target="_blank">
          <img width="296px" src="https://raw.githubusercontent.com/PostgREST/postgrest/main/static/neon.jpg">
        </a>
      </td>
    </tr>
    <tr></tr>
    <tr>
      <td align="center" valign="middle">
        <a href="https://code.build/?utm_source=sponsor&utm_campaign=postgrest" target="_blank">
          <img width="296px" src="https://raw.githubusercontent.com/PostgREST/postgrest/main/static/code-build.png">
        </a>
      </td>
      <td align="center" valign="middle">
        <a href="https://supabase.io?utm_source=postgrest%20backers&utm_medium=open%20source%20partner&utm_campaign=postgrest%20backers%20github&utm_term=homepage" target="_blank">
          <img width="296px" src="https://raw.githubusercontent.com/PostgREST/postgrest/main/static/supabase.png">
        </a>
      </td>
      <td align="center" valign="middle">
        <a href="https://tembo.io/?utm_source=sponsor&utm_campaign=postgrest" target="_blank">
          <img width="296px" src="https://raw.githubusercontent.com/PostgREST/postgrest/main/static/tembo.png">
        </a>
      </td>
    </tr>
  </tbody>
</table>

# Usage

To learn how to use this container, see the [PostgREST Docker
documentation](https://postgrest.org/en/stable/install.html#docker).

You can configure the PostgREST image by setting
[environment variables](https://postgrest.org/en/stable/configuration.html).

# How this image is built

The image is built from scratch using
[Nix](https://nixos.org/nixpkgs/manual/#sec-pkgs-dockerTools) instead of a
`Dockerfile`, which yields a highly secure and optimized image. This is also why
no commands are listed in the image history. See the [PostgREST
respository](https://github.com/PostgREST/postgrest/tree/main/nix/tools/docker) for
details on the build process and how to inspect the image.
