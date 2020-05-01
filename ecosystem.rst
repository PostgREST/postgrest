.. _community_tutorials:

Community Tutorials
-------------------

* `Building a Contacts List with PostgREST and Vue.js <https://www.youtube.com/watch?v=iHtsALtD5-U>`_ -
  In this video series, DigitalOcean shows how to build and deploy an Nginx + PostgREST(using a managed PostgreSQL database) + Vue.js webapp in an Ubuntu server droplet.

* `PostgREST + Auth0: Create REST API in mintutes and add social login using Auth0 <https://samkhawase.com/blog/postgrest_introduction/>`_ - A step-by-step tutorial to show how to Dockerize and integrate Auth0 to PostgREST service.

* `PostgREST + PostGIS API tutorial in 5 minutes <https://gis-ops.com/postgrest-postgis-api-tutorial-in-5-minutes>`_ -
  In this tutorial, GIS • OPS shows how to perform PostGIS calculations through PostgREST :ref:`s_procs` interface.

.. _eco_example_apps:

Example Apps
------------

* `monacoremo/postgrest-sessions-example <https://github.com/monacoremo/postgrest-sessions-example>`_ - example for cookie-based sessions
* `tatut/postgrest-ui <https://github.com/tatut/postgrest-ui>`_ - ClojureScript UI components for PostgREST
* `priyank-purohit/PostGUI <https://github.com/priyank-purohit/PostGUI>`_ - React Material UI admin panel
* `Qu4tro/pgrst-dev-setup <https://github.com/Qu4tro/pgrst-dev-setup>`_ - docker-compose and tmuxp setup for experimentation.
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
* `myfreeweb/moneylog <https://github.com/myfreeweb/moneylog>`_ - accounting web app in Polymer + PostgREST
* `tyrchen/goodfilm <https://github.com/tyrchen/goodfilm>`_ - example film api
* `begriffs/postgrest-example <https://github.com/begriffs/postgrest-example>`_ - sqitch versioning for API
* `SMRxT/postgrest-demo <https://github.com/SMRxT/postgrest-demo>`_ - multi-tenant logging system
* `PierreRochard/postgrest-boilerplate <https://github.com/PierreRochard/postgrest-boilerplate>`_ - example auth back-end
* `marmelab/ng-admin-postgrest <https://github.com/marmelab/ng-admin-postgrest>`_ - automatic database admin panel

.. _eco_external_notification:

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


.. _eco_extensions:

Extensions
----------

* `pg-safeupdate <https://bitbucket.org/eradman/pg-safeupdate/>`_ - Prevent full-table updates or deletes
* `srid/spas <https://github.com/srid/spas>`_ - allow file uploads and basic auth
* `svmnotn/postgrest-auth <https://github.com/svmnotn/postgrest-auth>`_ - OAuth2-inspired external auth server
* `wildsurfer/postgrest-oauth-server <https://github.com/wildsurfer/postgrest-oauth-server>`_ - OAuth2 server
* `nblumoe/postgrest-oauth <https://github.com/nblumoe/postgrest-oauth>`_ - OAuth2 WAI middleware
* `criles25/postgrest-auth <https://github.com/criles25/postgrest-auth>`_ - email based auth/signup
* `ppKrauss/PostgREST-writeAPI <https://github.com/ppKrauss/PostgREST-writeAPI>`_ - generate Nginx rewrite rules to fit an OpenAPI spec

.. _clientside_libraries:

Client-Side Libraries
---------------------

* `supabase/postgrest-js <https://github.com/supabase/postgrest-js>`_ - Isomorphic JS client
* `SocialGouv/postgrester <https://github.com/SocialGouv/postgrester>`_ - JS + Typescript
* `Kong/py-postgrest <https://github.com/Kong/py-postgrest>`_ - Python
* `datrium/postgrest-pyclient <https://github.com/datrium/postgrest-pyclient>`_ - Python
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

.. _eco_commercial:

Commercial
---------------

* `subZero <https://subzero.cloud/>`_ - Automated GraphQL & REST API with built-in caching (powered in part by PostgREST)
