.. title:: PostgREST Documentation

PostgREST Documentation
=======================

.. container:: image-container

  .. figure:: _static/logo.png

.. image:: https://img.shields.io/github/stars/postgrest/postgrest.svg?style=social
  :target: https://github.com/PostgREST/postgrest

.. image:: https://img.shields.io/github/v/release/PostgREST/postgrest.svg
  :target: https://github.com/PostgREST/postgrest/releases

.. image:: https://img.shields.io/docker/pulls/postgrest/postgrest.svg
  :target: https://hub.docker.com/r/postgrest/postgrest/

.. image:: https://img.shields.io/badge/Donate-Patreon-orange.svg?colorB=F96854
  :target: https://www.patreon.com/postgrest

|

PostgREST is a standalone web server that turns your PostgreSQL database directly into a RESTful API. The structural constraints and permissions in the database determine the API endpoints and operations.

Sponsors
--------

.. container:: image-container

  .. image:: _static/cybertec-new.png
    :target: https://www.cybertec-postgresql.com/en/?utm_source=postgrest.org&utm_medium=referral&utm_campaign=postgrest
    :width:  13em

  .. image:: _static/2ndquadrant.png
    :target: https://www.2ndquadrant.com/en/?utm_campaign=External%20Websites&utm_source=PostgREST&utm_medium=Logo
    :width:  13em

  .. image:: _static/retool.png
    :target: https://retool.com/?utm_source=sponsor&utm_campaign=postgrest
    :width:  13em

  .. image:: _static/gnuhost.png
    :target: https://euronodes.com/?utm_source=sponsor&utm_campaign=postgrest
    :width:  13em

  .. image:: _static/supabase.png
    :target: https://supabase.com/?utm_source=postgrest%20backers&utm_medium=open%20source%20partner&utm_campaign=postgrest%20backers%20github&utm_term=homepage
    :width:  13em

  .. image:: _static/oblivious.jpg
    :target: https://oblivious.ai/?utm_source=sponsor&utm_campaign=postgrest
    :width:  13em

  .. The static/empty.png(created with `convert -size 320x95 xc:#fcfcfc empty.png`) is an ugly workaround
     to create space and center the logos. It's not easy to layout with restructuredText.

  .. .. image:: _static/empty.png
       :target: #sponsors
       :width:  13em

|

Motivation
----------

Using PostgREST is an alternative to manual CRUD programming. Custom API servers suffer problems. Writing business logic often duplicates, ignores or hobbles database structure. Object-relational mapping is a leaky abstraction leading to slow imperative code. The PostgREST philosophy establishes a single declarative source of truth: the data itself.

Declarative Programming
-----------------------

It's easier to ask PostgreSQL to join data for you and let its query planner figure out the details than to loop through rows yourself. It's easier to assign permissions to db objects than to add guards in controllers. (This is especially true for cascading permissions in data dependencies.) It's easier to set constraints than to litter code with sanity checks.

Leak-proof Abstraction
----------------------

There is no ORM involved. Creating new views happens in SQL with known performance implications. A database administrator can now create an API from scratch with no custom programming.

One Thing Well
--------------

PostgREST has a focused scope. It works well with other tools like Nginx. This forces you to cleanly separate the data-centric CRUD operations from other concerns. Use a collection of sharp tools rather than building a big ball of mud.

Getting Support
----------------

The project has a friendly and growing community. For discussions, use the Github `discussions page <https://github.com/PostgREST/postgrest/discussions>`_. You can also report or search for bugs/features on the Github `issues <https://github.com/PostgREST/postgrest/issues>`_ page.

.. toctree::
   :glob:
   :caption: Release Notes
   :titlesonly:
   :hidden:

   v10.2.0 <releases/v10.2.0>
   v10.0.0 <releases/v10.0.0>
   v9.0.1 <releases/v9.0.1>
   v9.0.0 <releases/v9.0.0>
   releases/v8.0.0
   releases/v7.0.1
   releases/v7.0.0
   releases/v6.0.2
   releases/v5.2.0

Tutorials
---------

Are you new to PostgREST? This is the place to start!

.. toctree::
   :glob:
   :caption: Tutorials
   :hidden:

   tutorials/*

- :doc:`tutorials/tut0`
- :doc:`tutorials/tut1`

Also have a look at :doc:`Installation <install>` and :ref:`community_tutorials`.

Reference guides
----------------

Technical references for PostgREST's functionality.

.. toctree::
   :caption: API
   :hidden:

   api.rst

.. toctree::
   :caption: Configuration
   :hidden:

   configuration.rst

.. toctree::
   :caption: Schema Cache
   :hidden:

   schema_cache.rst

.. toctree::
   :caption: Errors
   :hidden:

   errors.rst

- :doc:`API <api>`
- :doc:`configuration`
- :doc:`Schema Cache <schema_cache>`
- :doc:`Errors <errors>`

Topic guides
------------

Explanations of some key concepts in PostgREST.

.. toctree::
   :caption: Authentication
   :hidden:

   auth.rst

.. toctree::
   :caption: Schema Structure
   :hidden:

   schema_structure.rst

.. toctree::
   :caption: Administration
   :hidden:

   admin.rst

.. toctree::
   :caption: Installation
   :hidden:

   install.rst

- :doc:`Authentication <auth>`
- :doc:`Schema Structure <schema_structure>`
- :doc:`Administration <admin>`
- :doc:`Installation <install>`

.. _how_tos:

How-to guides
-------------

These are recipes that'll help you address specific use-cases.

.. toctree::
   :glob:
   :caption: How-to guides
   :hidden:

   how-tos/working-with-postgresql-data-types
   how-tos/providing-images-for-img
   how-tos/create-soap-endpoint
   how-tos/sql-user-management-using-postgres-users-and-passwords

- :doc:`how-tos/providing-images-for-img`
- :doc:`how-tos/working-with-postgresql-data-types`
- :doc:`how-tos/create-soap-endpoint`
- :doc:`how-tos/sql-user-management-using-postgres-users-and-passwords`

Ecosystem
---------

PostgREST has a growing ecosystem of examples, libraries, and experiments. Here is a selection.

.. toctree::
   :caption: Ecosystem
   :hidden:

   ecosystem.rst

* :ref:`community_tutorials`
* :ref:`templates`
* :ref:`eco_example_apps`
* :ref:`devops`
* :ref:`eco_external_notification`
* :ref:`eco_extensions`
* :ref:`clientside_libraries`


Release Notes
-------------

Changes among versions.

- :doc:`releases/v9.0.0`
- :doc:`releases/v8.0.0`

In Production
-------------

Here are some companies that use PostgREST in production.

* `Catarse <https://www.catarse.me>`_
* `Datrium <https://www.datrium.com>`_
* `Drip Depot <https://www.dripdepot.com>`_
* `Image-charts <https://www.image-charts.com>`_
* `Moat <https://www.moat.com>`_
* `MotionDynamic - Fast highly dynamic video generation at scale <https://motiondynamic.tech>`_
* `Netwo <https://www.netwo.io>`_
* `Nimbus <https://www.nimbusforwork.com>`_
  - See how Nimbus uses PostgREST in `Paul Copplestone's blog post <https://paul.copplest.one/blog/nimbus-tech-2019-04.html>`_.
* `OpenBooking <https://www.openbooking.ch>`_
* `Supabase <https://supabase.com>`_

.. Certs are failing
  * `eGull <http://www.egull.co>`_

Testimonials
------------

  "It's so fast to develop, it feels like cheating!"

  -- François-Guillaume Ribreau

  "I just have to say that, the CPU/Memory usage compared to our
  Node.js/Waterline ORM based API is ridiculous.  It's hard to even push
  it over 60/70 MB while our current API constantly hits 1GB running on 6
  instances (dynos)."

  -- Louis Brauer

  "I really enjoyed the fact that all of a sudden I was writing
  microservices in SQL DDL (and v8 JavaScript functions). I dodged so
  much boilerplate. The next thing I knew, we pulled out a full rewrite
  of a Spring+MySQL legacy app in 6 months. Literally 10x faster, and
  code was super concise. The old one took 3 years and a team of 4
  people to develop."

  -- Simone Scarduzio

  "I like the fact that PostgREST does one thing, and one thing well.
  While PostgREST takes care of bridging the gap between our HTTP server
  and PostgreSQL database, we can focus on the development of our API in
  a single language: SQL. This puts the database in the center of our
  architecture, and pushed us to improve our skills in SQL programming
  and database design."

  -- Eric Bréchemier, Data Engineer, eGull SAS

  "PostgREST is performant, stable, and transparent. It allows us to
  bootstrap projects really fast, and to focus on our data and application
  instead of building out the ORM layer. In our k8s cluster, we run a few
  pods per schema we want exposed, and we scale up/down depending on demand.
  Couldn't be happier."

  -- Anupam Garg, Datrium, Inc.

Contributing
------------

Please see the `Contributing guidelines <https://github.com/PostgREST/postgrest/blob/main/.github/CONTRIBUTING.md>`_ in the main PostgREST repository.
