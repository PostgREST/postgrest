.. |br| raw:: html

   <br />

Upcoming
========

These are changes yet unreleased. If you'd like to try them out before a new official release, you can :ref:`build_source`.

Added
-----

* Support for HTTP HEAD requests.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Support for :ref:`planned_count` and :ref:`estimated_count`.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Support for :ref:`Resource Embedding Disambiguation <embed_disamb>`.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Documentation reference for :ref:`s_proc_embed`.

* Documentation reference for :ref:`mutation_embed`.

Changed
-------

* :ref:`bulk_call` should now be done by specifying a ``Prefer: params=multiple-objects`` header.

* Resource Embedding now outputs an error when multiple relationships between two tables are found, see :ref:`embed_disamb`.

* ``server-proxy-uri`` config option has been renamed to :ref:`openapi-server-proxy-uri`.
