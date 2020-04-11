.. |br| raw:: html

   <br />

Upcoming
========

These are changes yet unreleased. If you'd like to try them out before a new official release, you can :ref:`build_source`.

Added
-----

* Support for :ref:`multiple-schemas` at runtime.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_, `@mahmoudkassem <https://github.com/mahmoudkassem>`_

* Support for :ref:`planned_count` and :ref:`estimated_count`.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_, `@LorenzHenk <https://github.com/LorenzHenk>`_

* Support for the :ref:`on_conflict <on_conflict>` query parameter to UPSERT based on a unique constraint.
  |br| -- `@ykst <https://github.com/ykst>`_

* Support for :ref:`Resource Embedding Disambiguation <embed_disamb>`.
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Support for user defined socket permission via :ref:`server-unix-socket-mode` config option
  |br| -- `@Dansvidania <https://github.com/Dansvidania>`_

* HTTP logic improvements -- `@steve-chavez <https://github.com/steve-chavez>`_

   + Support for HTTP HEAD requests.
   + GUCs for :ref:`guc_req_path_method`.
   + Support for :ref:`pre_req_headers`.
   + Allow overriding provided headers(Content-Type, Location, etc) by :ref:`guc_resp_hdrs`
   + Access to the ``Authorization`` header value through ``request.header.authorization``

* Documentation improvements

  + Reference for :ref:`s_proc_embed`.
  + Reference for :ref:`mutation_embed`.
  + Reference for filters on :ref:`json_columns`.

Fixed
-----

* Allow embedding a VIEW when its source table foreign key is UNIQUE
  |br| -- `@bwbroersma <https://github.com/bwbroersma>`_

* ``Accept: application/vnd.pgrst.object+json`` behavior is now enforced for POST/PATCH/DELETE regardless of ``Prefer: return=minimal``
  |br| -- `@dwagin <https://github.com/dwagin>`_

* Fix self join resource embedding on PATCH
  |br| -- `@herulume <https://github.com/herulume>`_, `@steve-chavez <https://github.com/steve-chavez>`_

* Allow PATCH/DELETE without ``Prefer: return=minimal`` on tables with no SELECT privileges
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

* Fix many to many resource embedding for RPC/PATCH
  |br| -- `@steve-chavez <https://github.com/steve-chavez>`_

Changed
-------

* :ref:`bulk_call` should now be done by specifying a ``Prefer: params=multiple-objects`` header.

* Resource Embedding now outputs an error when multiple relationships between two tables are found, see :ref:`embed_disamb`.

* ``server-proxy-uri`` config option has been renamed to :ref:`openapi-server-proxy-uri`.

* Default Unix Socket file mode from 755 to 660
