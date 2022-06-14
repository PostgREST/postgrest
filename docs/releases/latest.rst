
Latest
======

These are features/bugfixes not yet on a stable version. You can try them by downloading the latest pre-releases `on the GitHub release page <https://github.com/PostgREST/postgrest/releases>`_.

Features
--------

API
~~~

Access Composite Type fields and Array elements
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can now :ref:`access fields of a Composite type or elements of an Array type <composite_array_columns>` with the arrow operators(``->``, ``->>``) in the same way you would access the JSON type fields.

Improved Error Messages
^^^^^^^^^^^^^^^^^^^^^^^

To increase consistency, all the errors messages are now normalized. The ``hint``, ``details``, ``code`` and ``message`` fields will always be present in the body, each one defaulting to a
``null`` value. In the same way, the :ref:`errors that were raised <raise_error>` with ``SQLSTATE`` now include the ``message`` and ``code`` in the body.

In addition to these changes and to further clarify the source of an error, PostgREST now adds a ``PGRST`` prefix to the error code of all the errors that are PostgREST-specific and don't come from the database. These errors have a unique code that identifies them and are documented in the :ref:`pgrst_errors` section.

Alongside these changes, there is now a dedicated reference page for :doc:`Error documentation </errors>`.

Administration
~~~~~~~~~~~~~~

Health checks
^^^^^^^^^^^^^

Admins can now benefit from two :ref:`health check endpoints <health_check>` exposed in a different port than the main app. When activated, the ``live`` and ``ready`` endpoints are available to verify if PostgREST is alive and running or if the database connection and the :ref:`schema cache <schema_cache>` are ready for querying.

Logging users
^^^^^^^^^^^^^

You can now verify the current authenticated database user in the :ref:`request log <pgrst_logging>` on stdout.

Run without configuration
^^^^^^^^^^^^^^^^^^^^^^^^^

It is now possible to execute PostgREST without specifying any configuration variable, even without the three that were mandatory

  - If :ref:`db-uri` is not set, PostgREST will use the `libpq environment variables <https://www.postgresql.org/docs/current/libpq-envars.html>`_ for the database connection.
  - If :ref:`db-schemas` is not set, it will use the database ``public`` schema.
  - If :ref:`db-anon-role` is not set, it will not allow anonymous requests.

Documentation improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~

* Added a :doc:`/how-tos/working-with-postgresql-data-types` how-to, which contains explanations and examples on how to work with different PostgreSQL data types such as timestamps, ranges or PostGIS types, among others.

* Added in-database and environment variable settings for each :ref:`configuration variable <config_full_list>`.

* Added the :ref:`file_descriptors` subsection.

* Moved the :ref:`error_source` and the :ref:`status_codes` sections to the :doc:`errors reference page </errors>`.

* Moved the *Casting type to custom JSON* how-to to the :ref:`casting_range_to_json` subsection.

* Removed direct links for PostgREST versions older than 8.0 from the versions menu.

* Removed the deprecated *Embedding table from another schema* how-to.

Bug fixes
---------

* Return ``204 No Content`` without ``Content-Type`` for ``PUT`` (`#2058 <https://github.com/PostgREST/postgrest/issues/2058>`_)

* Clarify error for failed schema cache load. (`#2107 <https://github.com/PostgREST/postgrest/issues/2107>`_)

  - From ``Database connection lost. Retrying the connection`` to ``Could not query the database for the schema cache. Retrying.``

* Fix silently ignoring filter on a non-existent embedded resource (`#1771 <https://github.com/PostgREST/postgrest/issues/1771>`_)

* Remove functions, which are not callable due to unnamed arguments, from schema cache and OpenAPI output. (`#2152 <https://github.com/PostgREST/postgrest/issues/2152>`_)

* Fix accessing JSON array fields with ``->`` and ``->>`` in ``?select=`` and ``?order=``. (`#2145 <https://github.com/PostgREST/postgrest/issues/2145>`_)

Breaking changes
----------------

* Return ``204 No Content`` without ``Content-Type`` for RPCs returning ``VOID`` (`#2001 <https://github.com/PostgREST/postgrest/issues/2001>`_)

  - Previously, those RPCs would return ``null`` as a body with ``Content-Type: application/json``.

Thanks
------

Big thanks from the `PostgREST team <https://github.com/orgs/PostgREST/people>`_ to our sponsors!

.. container:: image-container

  .. image:: ../_static/cybertec-new.png
    :target: https://www.cybertec-postgresql.com/en/?utm_source=postgrest.org&utm_medium=referral&utm_campaign=postgrest
    :width:  13em

  .. image:: ../_static/2ndquadrant.png
    :target: https://www.2ndquadrant.com/en/?utm_campaign=External%20Websites&utm_source=PostgREST&utm_medium=Logo
    :width:  13em

  .. image:: ../_static/retool.png
    :target: https://retool.com/?utm_source=sponsor&utm_campaign=postgrest
    :width:  13em

  .. image:: ../_static/gnuhost.png
    :target: https://gnuhost.eu/?utm_source=sponsor&utm_campaign=postgrest
    :width:  13em

  .. image:: ../_static/supabase.png
    :target: https://supabase.com/?utm_source=postgrest%20backers&utm_medium=open%20source%20partner&utm_campaign=postgrest%20backers%20github&utm_term=homepage
    :width:  13em

  .. image:: ../_static/oblivious.jpg
    :target: https://oblivious.ai/?utm_source=sponsor&utm_campaign=postgrest
    :width:  13em

* Evans Fernandes
* `Jan Sommer <https://github.com/nerfpops>`_
* `Franz Gusenbauer <https://www.igutech.at/>`_
* `Daniel Babiak <https://github.com/dbabiak>`_
* Tsingson Qin
* Michel Pelletier
* Jay Hannah
* Robert Stolarz
* Nicholas DiBiase
* Christopher Reid
* Nathan Bouscal
* Daniel Rafaj
* David Fenko
* Remo Rechkemmer
* Severin Ibarluzea
* Tom Saleeba
* Pawel Tyll

If you like to join them please consider `supporting PostgREST development <https://github.com/PostgREST/postgrest#user-content-supporting-development>`_.
