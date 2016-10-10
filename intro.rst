Motivation
##########

PostgREST is a standalone web server that turns your PostgreSQL database directly into a RESTful API. The structural constraints and permissions in the database determine the API endpoints and operations.

Using PostgREST is an alternative to manual CRUD programming. Custom API servers suffer problems. Writing business logic often duplicates, ignores or hobbles database structure. Object-relational mapping is a leaky abstraction leading to slow imperative code. The PostgREST philosophy establishes a single declarative source of truth: the data itself.

Declarative Programming
-----------------------

It's easier to ask PostgreSQL to join data for you and let its query planner figure out the details than to loop through rows yourself. It's easier to assign permissions to db objects than to add guards in controllers. (This is especially true for cascading permissions in data dependencies.) It's easier set constraints than to litter code with sanity checks.

Leakproof Abstraction
---------------------

There is no ORM involved. Creating new views happens in SQL with known performance implications. A database administrator can now create an API from scratch with no custom programming.

Embracing the Relational Model
------------------------------

In 1970 E. F. Codd criticized the then-dominant hierarchical model of databases in his article A Relational Model of Data for Large Shared Data Banks. Reading the article reveals a striking similarity between hierarchical databases and nested http routes. With PostgREST we attempt to use flexible filtering and embedding rather than nested routes.

One Thing Well
--------------

PostgREST has a focused scope. It works well with other tools like Nginx. This forces you to cleanly separate the data-centric CRUD operations from other concerns. Use a collection of sharp tools rather than building a big ball of mud.

Shared Improvements
-------------------

As with any open source project, we all gain from features and fixes in the tool. It's more beneficial than improvements locked inextricably within custom codebases.

Ecosystem
#########

PostgREST has a growing ecosystem of examples, and libraries, experiments, and users. Here is a selection.

Client-Side Libraries
---------------------

* `hugomrdias/postgrest-url <https://github.com/hugomrdias/postgrest-url>`_ - JS, just for generating query URLs
* `john-kelly/elm-postgrest <https://github.com/john-kelly/elm-postgrest>`_ - Elm
* `mithril.postgrest <https://github.com/catarse/mithril.postgrest>`_ - JS, Mithril
* `thejettdurham/postgrest-sharp-client <https://github.com/thejettdurham/postgrest-sharp-client>`_ - C#, RestSharp
* `lewisjared/postgrest-request <https://github.com/lewisjared/postgrest-request>`_ - JS, SuperAgent
* `JarvusInnovations/jarvus-postgrest-apikit <https://github.com/JarvusInnovations/jarvus-postgrest-apikit>`_ - JS, Sencha framework
* `davidthewatson/postgrest_python_requests_client <https://github.com/davidthewatson/postgrest_python_requests_client>`_ - Python
* `calebmer/postgrest-client <https://github.com/calebmer/postgrest-client>`_ - JS

Extensions
----------

* `diogob/postgrest-ws <https://github.com/diogob/postgrest-ws>`_ - expose web sockets for PostgreSQL's LISTEN/NOTIFY
* `srid/spas <https://github.com/srid/spas>`_ - allow file uploads and basic auth
* `svmnotn/postgrest-auth <https://github.com/svmnotn/postgrest-auth>`_ - OAuth2-inspired external auth server
* `nblumoe/postgrest-oauth <https://github.com/nblumoe/postgrest-oauth>`_ - OAuth2 WAI middleware

Example Apps
------------

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

In Production
-------------

* `Catarse <https://www.catarse.me/>`_
* `iAdvize <http://iadvize.com/>`_
* `Redsmin <https://www.redsmin.com/>`_
* `Image-charts <https://image-charts.com/>`_
* `Drip Depot <https://www.dripdepot.com/>`_

Commercial PaaS
---------------

* `Sub0 <http://graphqlapi.com/>`_ - Automated GraphQL & REST API with built-in caching (powered by PostgREST)


Getting Support
################

The project has a friendly and growing community. Join our `chat room <https://gitter.im/begriffs/postgrest>`_ for discussion and help. You can also report or search for bugs/features on the Github `issues <https://github.com/begriffs/postgrest/issues>`_ page.
