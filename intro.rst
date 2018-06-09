Motivation
##########

PostgREST is a standalone web server that turns your PostgreSQL database directly into a RESTful API. The structural constraints and permissions in the database determine the API endpoints and operations.

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

Ecosystem
#########

PostgREST has a growing ecosystem of examples, and libraries, experiments, and users. Here is a selection.

.. _clientside_libraries:

Client-Side Libraries
---------------------

* `tomberek/aor-postgrest-client <https://github.com/tomberek/aor-postgrest-client>`_ - JS, admin-on-rest
* `hugomrdias/postgrest-url <https://github.com/hugomrdias/postgrest-url>`_ - JS, just for generating query URLs
* `john-kelly/elm-postgrest <https://github.com/john-kelly/elm-postgrest>`_ - Elm
* `mithril.postgrest <https://github.com/catarse/mithril.postgrest>`_ - JS, Mithril
* `lewisjared/postgrest-request <https://github.com/lewisjared/postgrest-request>`_ - JS, SuperAgent
* `JarvusInnovations/jarvus-postgrest-apikit <https://github.com/JarvusInnovations/jarvus-postgrest-apikit>`_ - JS, Sencha framework
* `davidthewatson/postgrest_python_requests_client <https://github.com/davidthewatson/postgrest_python_requests_client>`_ - Python
* `calebmer/postgrest-client <https://github.com/calebmer/postgrest-client>`_ - JS
* `clesiemo3/postgrestR <https://github.com/clesiemo3/postgrestR>`_ - R
* `PierreRochard/postgrest-angular <https://github.com/PierreRochard/postgrest-angular>`_ - TypeScript, generate UI from API description
* `thejettdurham/postgrest-sharp-client <https://github.com/thejettdurham/postgrest-sharp-client>`_ (needs maintainer) - C#, RestSharp
* `team142/ng-postgrest <https://github.com/team142/ng-postgrest>`_ - Angular app for browsing, editing data exposed over Postgrest.

External Notification
---------------------

These are PostgreSQL bridges that propagate LISTEN/NOTIFY to external queues for further processing. This allows stored procedures to initiate actions outside the database such as sending emails.

* `diogob/postgres-websockets <https://github.com/diogob/postgres-websockets>`_ - expose web sockets for PostgreSQL's LISTEN/NOTIFY
* `frafra/postgresql2websocket <https://github.com/frafra/postgresql2websocket>`_ - Websockets
* `matthewmueller/pg-bridge <https://github.com/matthewmueller/pg-bridge>`_ - Amazon SNS
* `aweber/pgsql-listen-exchange <https://github.com/aweber/pgsql-listen-exchange>`_ - RabbitMQ
* `SpiderOak/skeeter <https://github.com/SpiderOak/skeeter>`_ - ZeroMQ
* `FGRibreau/postgresql-to-amqp <https://github.com/FGRibreau/postgresql-to-amqp>`_ - AMQP
* `daurnimator/pg-kinesis-bridge <https://github.com/daurnimator/pg-kinesis-bridge>`_ - Amazon Kinesis

Example Apps
------------

* `subzerocloud/postgrest-starter-kit <https://github.com/subzerocloud/postgrest-starter-kit>`_ - Boilerplate for new project
* `NikolayS/postgrest-google-translate <https://github.com/NikolayS/postgrest-google-translate>`_ - Calling to external translation service
* `CodeforAustralia/heritage-near-me <https://github.com/CodeforAustralia/heritage-near-me>`_ - Elm and PostgREST with PostGIS
* `timwis/handsontable-postgrest <https://github.com/timwis/handsontable-postgrest>`_ - An excel-like database table editor
* `Recmo/PostgrestSkeleton <https://github.com/Recmo/PostgrestSkeleton>`_ - Docker Compose, PostgREST, Nginx and Auth0
* `benoror/ember-postgrest-dynamic-ui <https://github.com/benoror/ember-postgrest-dynamic-ui>`_ - generating Ember forms to edit data
* `ruslantalpa/blogdemo <https://github.com/ruslantalpa/blogdemo>`_ - blog api demo in a vagrant image
* `timwis/ext-postgrest-crud <https://github.com/timwis/ext-postgrest-crud>`_ - browser-based spreadsheet
* `srid/chronicle <https://github.com/srid/chronicle>`_ - tracking a tree of personal memories
* `diogob/elm-workshop <https://github.com/diogob/elm-workshop>`_ - building a simple database query UI
* `marmelab/ng-admin-postgrest <https://github.com/marmelab/ng-admin-postgrest>`_ - automatic database admin panel
* `myfreeweb/moneylog <https://github.com/myfreeweb/moneylog>`_ - accounting web app in Polymer + PostgREST
* `tyrchen/goodfilm <https://github.com/tyrchen/goodfilm>`_ - example film api
* `begriffs/postgrest-example <https://github.com/begriffs/postgrest-example>`_ - sqitch versioning for API
* `SMRxT/postgrest-demo <https://github.com/SMRxT/postgrest-demo>`_ - multi-tenant logging system
* `PierreRochard/postgrest-boilerplate <https://github.com/PierreRochard/postgrest-boilerplate>`_ - example auth back-end

In Production
-------------

* `triggerFS - A realtime messaging and distributed trigger system <https://triggerfs.io/>`_
* `Moat <https://moat.com/>`_
* `Catarse <https://www.catarse.me/>`_
* `Redsmin <https://www.redsmin.com/>`_
* `Image-charts <https://image-charts.com/>`_
* `MotionDynamic - Fast highly dynamic video generation at scale <https://api.motiondynamic.tech/>`_
* `Drip Depot <https://www.dripdepot.com/>`_
* `OpenBooking <http://openbooking.ch>`_
* `Convene <https://info.convene.thomsonreuters.com/en.html>`_ by Thomson-Reuters
* `eGull <http://www.egull.co>`_
* `Elyios <https://elyios.com>`_

Extensions
----------

* `pg-safeupdate <https://bitbucket.org/eradman/pg-safeupdate/>`_ - Prevent full-table updates or deletes
* `srid/spas <https://github.com/srid/spas>`_ - allow file uploads and basic auth
* `svmnotn/postgrest-auth <https://github.com/svmnotn/postgrest-auth>`_ - OAuth2-inspired external auth server
* `wildsurfer/postgrest-oauth-server <https://github.com/wildsurfer/postgrest-oauth-server>`_ - OAuth2 server
* `nblumoe/postgrest-oauth <https://github.com/nblumoe/postgrest-oauth>`_ - OAuth2 WAI middleware
* `criles25/postgrest-auth <https://github.com/criles25/postgrest-auth>`_ - email based auth/signup
* `ppKrauss/PostgREST-writeAPI <https://github.com/ppKrauss/PostgREST-writeAPI>`_ - generate Nginx rewrite rules to fit an OpenAPI spec

Commercial
---------------

* `subZero <https://subzero.cloud/>`_ - Automated GraphQL & REST API with built-in caching (powered in part by PostgREST)

Testimonials
############

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

Getting Support
################

The project has a friendly and growing community. Join our `chat room <https://gitter.im/begriffs/postgrest>`_ for discussion and help. You can also report or search for bugs/features on the Github `issues <https://github.com/begriffs/postgrest/issues>`_ page.
