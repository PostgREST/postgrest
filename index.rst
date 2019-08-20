.. title:: PostgREST Documentation

.. figure:: _static/logo.png

.. image:: https://img.shields.io/github/stars/postgrest/postgrest.svg?style=social
  :target: https://github.com/PostgREST/postgrest

.. image:: https://img.shields.io/github/release/PostgREST/postgrest.svg
  :target: https://github.com/PostgREST/postgrest/releases

.. image:: https://img.shields.io/docker/pulls/postgrest/postgrest.svg
  :target: https://hub.docker.com/r/postgrest/postgrest/

.. image:: https://img.shields.io/badge/gitter-join%20chat%20%E2%86%92-brightgreen.svg
  :target: https://gitter.im/begriffs/postgrest

.. image:: https://img.shields.io/badge/Donate-Patreon-orange.svg?colorB=F96854
  :target: https://www.patreon.com/postgrest

.. image:: https://img.shields.io/badge/Donate-PayPal-green.svg
  :target: https://www.paypal.me/postgrest

|

PostgREST is a standalone web server that turns your PostgreSQL database directly into a RESTful API. The structural constraints and permissions in the database determine the API endpoints and operations.

Sponsors
--------

.. image:: _static/cybertec.png
  :target: https://www.cybertec-postgresql.com/en/
  :width:  13em

.. image:: _static/2ndquadrant.png
  :target: https://www.2ndquadrant.com/en/?utm_campaign=External%20Websites&utm_source=PostgREST&utm_medium=Logo
  :width:  13em

.. image:: _static/retool.png
  :target: https://tryretool.com/?utm_source=sponsor&utm_campaign=postgrest
  :width:  13em

Motivation
----------

Using PostgREST is an alternative to manual CRUD programming. Custom API servers suffer problems. Writing business logic often duplicates, ignores or hobbles database structure. Object-relational mapping is a leaky abstraction leading to slow imperative code. The PostgREST philosophy establishes a single declarative source of truth: the data itself.

Declarative Programming
-----------------------

It's easier to ask PostgreSQL to join data for you and let its query planner figure out the details than to loop through rows yourself. It's easier to assign permissions to db objects than to add guards in controllers. (This is especially true for cascading permissions in data dependencies.) It's easier to set constraints than to litter code with sanity checks.

Leak-proof Abstraction
----------------------

There is no ORM involved. Creating new views happens in SQL with known performance implications. A database administrator can now create an API from scratch with no custom programming.

Embracing the Relational Model
------------------------------

In 1970 E. F. Codd criticized the then-dominant hierarchical model of databases in his article A Relational Model of Data for Large Shared Data Banks. Reading the article reveals a striking similarity between hierarchical databases and nested http routes. With PostgREST we attempt to use flexible filtering and embedding rather than nested routes.

One Thing Well
--------------

PostgREST has a focused scope. It works well with other tools like Nginx. This forces you to cleanly separate the data-centric CRUD operations from other concerns. Use a collection of sharp tools rather than building a big ball of mud.

Shared Improvements
-------------------

As with any open source project, we all gain from features and fixes in the tool. It's more beneficial than improvements locked inextricably within custom code-bases.

Getting Support
----------------

The project has a friendly and growing community. Join our `chat room <https://gitter.im/begriffs/postgrest>`_ for discussion and help. You can also report or search for bugs/features on the Github `issues <https://github.com/begriffs/postgrest/issues>`_ page.

.. _supporting-dev:

Supporting development
----------------------

You can help PostgREST ongoing maintenance and development by:

- Making a regular donation through `Patreon <https://www.patreon.com/postgrest>`_

- Alternatively, you can make a one-time donation via `Paypal <https://www.paypal.me/postgrest>`_

Every donation will be spent on making PostgREST better for the whole community.

Translations
~~~~~~~~~~~~

* `Chinese <http://postgrest.org/zh/latest/>`_ (latest version ``v0.4.2.0``)

.. toctree::
   :caption: Release Notes
   :titlesonly:

   release_notes.rst

.. toctree::
   :caption: Tutorials
   :titlesonly:

   tutorials/tut0.rst
   tutorials/tut1.rst

.. toctree::
   :caption: How-to guides
   :titlesonly:

   how-tos/embedding-table-from-another-schema.rst
   how-tos/casting-type-to-custom-json.rst

.. toctree::
   :caption: Installation
   :titlesonly:

   install.rst

.. toctree::
   :caption: API
   :titlesonly:

   api.rst

.. toctree::
   :caption: Authentication
   :titlesonly:

   auth.rst

.. toctree::
   :caption: Administration
   :titlesonly:

   admin.rst

Ecosystem
---------

PostgREST has a growing ecosystem of examples, and libraries, experiments, and users. Here is a selection.

.. toctree::
   :caption: Ecosystem
   :hidden:

   ecosystem.rst

* :ref:`eco_external_notification`
* :ref:`eco_example_apps`
* :ref:`eco_extensions`
* :ref:`clientside_libraries`
* :ref:`eco_commercial`
* :ref:`eco_production`

Testimonials
------------

  "It's so fast to develop, it feels like cheating!"

  -- François-G. Ribreau

  "I just have to say that, the CPU/Memory usage compared to our
  Node.js/Waterline ORM based API is ridiculous.  It's hard to even push
  it over 60/70 MB while our current API constantly hits 1GB running on 6
  instances (dynos)."

  -- Louis Brauer

  "I really enjoyed the fact that all of a sudden I was writing
  microservices in SQL DDL (and v8 javascript functions). I dodged so
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
