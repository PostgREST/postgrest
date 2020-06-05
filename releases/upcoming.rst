.. |br| raw:: html

   <br />

Upcoming
========

These are changes yet unreleased. If you'd like to try them out before a new official release, you can :ref:`build_source`.

Added
-----

* Allow http status override through the :ref:`response.status <guc_resp_status>` GUC.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

Fixed
-----

* Fix showing UNKNOWN on ``postgrest --help`` invocation.
  |br| -- `@monacoremo <https://github.com/monacoremo>`_

Changed
-------

* Docker images are now optimized to be built from the scratch image. This reduces the compressed image size from over 30mb to about 4mb.
  For more details, see `Docker image built with Nix <https://github.com/PostgREST/postgrest/tree/master/nix/docker#docker-image-built-with-nix>`_.
  |br| -- `@monacoremo <https://github.com/monacoremo>`_
