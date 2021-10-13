.. |br| raw:: html

   <br />

Upcoming
========

These are changes yet unreleased. If you'd like to try them out before a new official release, access `the list of CI runs <https://github.com/PostgREST/postgrest/actions/workflows/ci.yaml?query=branch%3Amain>`_
and select the newest commit, then download the build from the Artifacts section at the bottom of the page (you'll need a GitHub account to download it).

Added
-----

* Allow :ref:`embedding <embedding_partitioned_tables>`, UPSERT, INSERT with Location response, OPTIONS request and OpenAPI support for partitioned tables.
  |br| -- `@laurenceisla <https://github.com/laurenceisla>`_

* Make GUC names for headers, cookies and jwt claims compatible with PostgreSQL v14.

  + The GUC names on PostgreSQL 14 are changed to the ones :ref:`mentioned in this section <guc_req_headers_cookies_claims>`, while older versions still use the :ref:`guc_legacy_names`.
  + PostgreSQL versions below 14 can opt in to the new JSON GUCs by setting the :ref:`db-use-legacy-gucs` config option to false (true by default).
  + Managed to avoid a breaking change thanks to `@robertsosinski <https://github.com/robertsosinski>`_ who reported the bug that only one ``.`` character was allowed in GUC keys to the PostgreSQL team. See the `full discussion <https://www.postgresql.org/message-id/17045-6a4a9f0d1513f72b%40postgresql.org>`_.

  -- `@laurenceisla <https://github.com/laurenceisla>`_
