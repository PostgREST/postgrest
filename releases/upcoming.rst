.. |br| raw:: html

   <br />

Upcoming
========

These are changes yet unreleased. If you'd like to try them out before a new official release, you can use a `nightly release <https://github.com/PostgREST/postgrest/releases/tag/nightly>`_.

Added
-----

* Allow HTTP status override through the :ref:`response.status <guc_resp_status>` GUC.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Allow :ref:`s_procs_variadic`.
  |br| -- `@wolfgangwalther <https://github.com/wolfgangwalther>`_

* Config options for showing a full OpenAPI output regardless of the JWT role privileges and for disabling it altogether. See :ref:`openapi-mode`.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Config option for logging level. See :ref:`log-level`.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Documentation improvements

  + Added the :ref:`OPTIONS requests <options_requests>` section.
  + Added the :ref:`schema_cache` section.
  + Moved the :ref:`schema_reloading` reference from :ref:`admin` to :ref:`schema_cache`

Fixed
-----

* Fix showing UNKNOWN on ``postgrest --help`` invocation.
  |br| -- `@monacoremo <https://github.com/monacoremo>`_

Changed
-------

* Docker images are now optimized to be built from the scratch image. This reduces the compressed image size from over 30 MB to about 4 MB.
  For more details, see `Docker image built with Nix <https://github.com/PostgREST/postgrest/tree/main/nix/tools/docker#user-content-docker-image-built-with-nix>`_.
  |br| -- `@monacoremo <https://github.com/monacoremo>`_

* Improved error message for a not found RPC on a stale schema (see :ref:`stale_function_signature`) and for the unsupported case of
  overloaded functions with the same argument names but different types.
  |br| -- `@laurenceisla <https://github.com/laurenceisla>`_

* Modified the default logging level from ``info`` to ``error``. See :ref:`log-level`.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_