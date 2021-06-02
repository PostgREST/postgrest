.. _community_tutorials:

Community Tutorials
-------------------

* `Building a Contacts List with PostgREST and Vue.js <https://www.youtube.com/watch?v=iHtsALtD5-U>`_ -
  In this video series, DigitalOcean shows how to build and deploy an Nginx + PostgREST(using a managed PostgreSQL database) + Vue.js webapp in an Ubuntu server droplet.

* `PostgREST + Auth0: Create REST API in mintutes, and add social login using Auth0 <https://samkhawase.com/blog/postgrest/>`_ - A step-by-step tutorial to show how to dockerize and integrate Auth0 to PostgREST service.

* `PostgREST + PostGIS API tutorial in 5 minutes <https://gis-ops.com/postgrest-postgis-api-tutorial-geospatial-api-in-5-minutes/>`_ -
  In this tutorial, GIS • OPS shows how to perform PostGIS calculations through PostgREST :ref:`s_procs` interface.

* `"CodeLess" backend using postgres, postgrest and oauth2 authentication with keycloak <https://www.mathieupassenaud.fr/codeless_backend/>`_ -
  A step-by-step tutorial for using PostgREST with KeyCloak(hosted on a managed service).

.. _eco_example_apps:

Example Apps
------------

* `blogdemo <https://github.com/ruslantalpa/blogdemo>`_ - blog api demo in a vagrant image
* `chronicle <https://github.com/srid/chronicle>`_ - tracking a tree of personal memories
* `elm-workshop <https://github.com/diogob/elm-workshop>`_ - building a simple database query UI
* `ember-postgrest-dynamic-ui <https://github.com/benoror/ember-postgrest-dynamic-ui>`_ - generating Ember forms to edit data
* `ext-postgrest-crud <https://github.com/timwis/ext-postgrest-crud>`_ - browser-based spreadsheet
* `general <https://github.com/PierreRochard/general>`_ - example auth back-end
* `goodfilm <https://github.com/tyrchen/goodfilm>`_ - example film api
* `handsontable-postgrest <https://github.com/timwis/handsontable-postgrest>`_ - an excel-like database table editor
* `heritage-near-me <https://github.com/CodeforAustralia/heritage-near-me>`_ - Elm and PostgREST with PostGIS
* `ng-admin-postgrest <https://github.com/marmelab/ng-admin-postgrest>`_ - automatic database admin panel
* `pgrst-dev-setup <https://github.com/Qu4tro/pgrst-dev-setup>`_ - docker-compose and tmuxp setup for experimentation.
* `postgrest-demo <https://github.com/SMRxT/postgrest-demo>`_ - multi-tenant logging system
* `postgrest-example <https://github.com/begriffs/postgrest-example>`_ - sqitch versioning for API
* `postgrest-sessions-example <https://github.com/monacoremo/postgrest-sessions-example>`_ - example for cookie-based sessions
* `postgrest-starter-kit <https://github.com/subzerocloud/postgrest-starter-kit>`_ - boilerplate for new project
* `postgrest-translation-proxy <https://github.com/NikolayS/postgrest-translation-proxy>`_ - calling to external translation service
* `postgrest-ui <https://github.com/tatut/postgrest-ui>`_ - ClojureScript UI components for PostgREST
* `postgrest-vercel <https://github.com/seveibar/postgrest-vercel>`_ - run PostgREST on Vercel (Serverless/AWS Lambda)
* `PostgrestSkeleton <https://github.com/Recmo/PostgrestSkeleton>`_ - Docker Compose, PostgREST, Nginx and Auth0
* `PostGUI <https://github.com/priyank-purohit/PostGUI>`_ - React Material UI admin panel

.. _eco_external_notification:

External Notification
---------------------

These are PostgreSQL bridges that propagate LISTEN/NOTIFY to external queues for further processing. This allows stored procedures to initiate actions outside the database such as sending emails.

* `pg-bridge <https://github.com/matthewmueller/pg-bridge>`_ - Amazon SNS
* `pg-kinesis-bridge <https://github.com/daurnimator/pg-kinesis-bridge>`_ - Amazon Kinesis
* `pg-notify-webhook <https://github.com/vbalasu/pg-notify-webhook>`_ - trigger webhooks from PostgreSQL's LISTEN/NOTIFY
* `pgsql-listen-exchange <https://github.com/gmr/pgsql-listen-exchange>`_ - RabbitMQ
* `postgres-websockets <https://github.com/diogob/postgres-websockets>`_ - expose web sockets for PostgreSQL's LISTEN/NOTIFY
* `postgresql-to-amqp <https://github.com/FGRibreau/postgresql-to-amqp>`_ - AMQP
* `postgresql2websocket <https://github.com/frafra/postgresql2websocket>`_ - Websockets
* `skeeter <https://github.com/SpiderOak/skeeter>`_ - ZeroMQ


.. _eco_extensions:

Extensions
----------

* `aiodata <https://github.com/Exahilosys/aiodata>`_ - Python, event-based proxy and caching client.
* `pg-safeupdate <https://github.com/eradman/pg-safeupdate>`_ - prevent full-table updates or deletes
* `postgrest-auth (criles25) <https://github.com/criles25/postgrest-auth>`_ - email based auth/signup
* `postgrest-auth (svmotn) <https://github.com/svmnotn/postgrest-auth>`_ - OAuth2-inspired external auth server
* `postgrest-node <https://github.com/seveibar/postgrest-node>`_ - Run a PostgREST server in Node.js via npm module
* `postgrest-oauth <https://github.com/nblumoe/postgrest-oauth>`_ - OAuth2 WAI middleware
* `postgrest-oauth/api <https://github.com/postgrest-oauth/api>`_ - OAuth2 server
* `PostgREST-writeAPI <https://github.com/ppKrauss/PostgREST-writeAPI>`_ - generate nginx rewrite rules to fit an OpenAPI spec
* `spas <https://github.com/srid/spas>`_ - allow file uploads and basic auth

.. _clientside_libraries:

Client-Side Libraries
---------------------

* `aor-postgrest-client <https://github.com/tomberek/aor-postgrest-client>`_ - JS, admin-on-rest
* `elm-postgrest <https://github.com/john-kelly/elm-postgrest>`_ - Elm
* `general-angular <https://github.com/PierreRochard/general-angular>`_ - TypeScript, generate UI from API description
* `jarvus-postgrest-apikit <https://github.com/JarvusInnovations/jarvus-postgrest-apikit>`_ - JS, Sencha framework
* `mithril-postgrest <https://github.com/catarse/mithril-postgrest>`_ - JS, Mithril
* `ng-postgrest <https://github.com/team142/ng-postgrest>`_ - Angular app for browsing, editing data exposed over PostgREST.
* `postgrest-client <https://github.com/calebmer/postgrest-client>`_ - JS
* `postgrest-csharp <https://github.com/supabase/postgrest-csharp>`_ - C#
* `postgrest-dart <https://github.com/supabase/postgrest-dart>`_ - Dart
* `postgrest-ex <https://github.com/J0/postgrest-ex>`_ - Elixir
* `postgrest-go <https://github.com/supabase/postgrest-go>`_ - Go
* `postgrest-js <https://github.com/supabase/postgrest-js>`_ - TypeScript/JavaScript
* `postgrest-kt <https://github.com/supabase/postgrest-kt>`_ - Kotlin
* `postgrest-py <https://github.com/supabase/postgrest-py>`_ - Python
* `postgrest-pyclient <https://github.com/datrium/postgrest-pyclient>`_ - Python
* `postgrest-request <https://github.com/lewisjared/postgrest-request>`_ - JS, SuperAgent
* `postgrest-rs <https://github.com/supabase/postgrest-rs>`_ - Rust
* `postgrest-sharp-client <https://github.com/thejettdurham/postgrest-sharp-client>`_ (needs maintainer) - C#, RestSharp
* `postgrest-swift <https://github.com/supabase/postgrest-kt>`_ - Swift
* `postgrest-url <https://github.com/hugomrdias/postgrest-url>`_ - JS, just for generating query URLs
* `postgrest_python_requests_client <https://github.com/davidthewatson/postgrest_python_requests_client>`_ - Python
* `postgrester <https://github.com/SocialGouv/postgrester>`_ - JS + Typescript
* `postgrestR <https://github.com/clesiemo3/postgrestR>`_ - R
* `py-postgrest <https://github.com/Kong/py-postgrest>`_ - Python
* `redux-postgrest <https://github.com/andytango/redux-postgrest>`_ - TypeScript/JS, client integrated with (React) Redux.
* `vue-postgrest <https://github.com/technowledgy/vue-postgrest>`_ - Vue.js

