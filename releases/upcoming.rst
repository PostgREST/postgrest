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

* Allow :ref:`embedding_view_chains` recursively to any depth.
  |br| -- `@wolfgangwalther <https://github.com/wolfgangwalther>`_

* No downtime when reloading the schema cache. See the note in :ref:`schema_reloading`.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Allow schema cache reloading using PostgreSQL :ref:`NOTIFY <schema_reloading_notify>` command.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Allow sending the header ``Prefer: headers-only`` to get a response with a ``Location`` header. See :ref:`insert_update`.
  |br| -- `@laurenceisla <https://github.com/laurenceisla>`_

* Allow :ref:`connection_poolers` such as PgBouncer in transaction pooling mode.
  |br| -- `@laurenceisla <https://github.com/laurenceisla>`_

* Allow :ref:`config_reloading` by sending a SIGUSR2 signal.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Allow ``Bearer`` with and without capitalization as authentication schema. See :ref:`client_auth`.
  |br| -- `@wolfgangwalther <https://github.com/wolfgangwalther>`_

* :ref:`in_db_config` that can be :ref:`reloaded with NOTIFY <in_db_config_reloading>`.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Allow OPTIONS to generate HTTP methods based on views triggers. See :ref:`OPTIONS requests <options_requests>`.
  |br| -- `@laurenceisla <https://github.com/laurenceisla>`_

* Show timestamps for server diagnostic information. See :ref:`pgrst_logging`.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Config options for showing a full OpenAPI output regardless of the JWT role privileges and for disabling it altogether. See :ref:`openapi-mode`.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Config option for logging level. See :ref:`log-level`.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Config option for enabling or disabling prepared statements. See :ref:`db-prepared-statements`.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Config option for specifying how to terminate the transactions (allowing rollbacks, useful for testing). See :ref:`db-tx-end`.
  |br| -- `@wolfgangwalther <https://github.com/wolfgangwalther>`_

* Documentation improvements

  + Added the :ref:`schema_cache` page.
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

* The ``pg_listen`` `utility <https://github.com/begriffs/pg_listen>`_ is no longer needed to automatically reload the schema cache
  and it's replaced entirely by database notifications. See :ref:`schema_reloading_notify`.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Improved error message for a not found RPC on a stale schema (see :ref:`stale_function_signature`) and for the unsupported case of
  overloaded functions with the same argument names but different types.
  |br| -- `@laurenceisla <https://github.com/laurenceisla>`_

* Modified the default logging level from ``info`` to ``error``. See :ref:`log-level`.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* POST requests for insertions no longer include a ``Location`` header in the response by default and behave the same way as having a
  ``Prefer: return=minimal`` header in the request. This prevents permissions errors when having a write-only table. See :ref:`insert_update`.
  |br| -- `@laurenceisla <https://github.com/laurenceisla>`_
