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

* `How PostgreSQL triggers work when called with a PostgREST PATCH HTTP request <https://blog.fgribreau.com/2020/11/how-postgresql-triggers-works-when.html>`_ - A tutorial to see how the old and new values are set or not when doing a PATCH request to PostgREST.

.. _templates:

Templates
---------

* `compose-postgrest <https://github.com/mattddowney/compose-postgrest>`_ - docker-compose setup with Nginx and HTML example
* `svelte-postgrest-template <https://github.com/guyromm/svelte-postgrest-template>`_ - Svelte/SvelteKit, PostgREST, EveryLayout and social auth

.. _eco_example_apps:

Example Apps
------------

* `chronicle <https://github.com/srid/chronicle>`_ - tracking a tree of personal memories
* `code-du-travail-backoffice <https://github.com/SocialGouv/code-du-travail-backoffice>`_ - data administration portal for the official French Labor Code and Agreements
* `delibrium-postgrest <https://gitlab.com/delibrium/delibrium-postgrest/>`_ - example school API and front-end in Vue.js
* `elm-workshop <https://github.com/diogob/elm-workshop>`_ - building a simple database query UI
* `ember-postgrest-dynamic-ui <https://github.com/benoror/ember-postgrest-dynamic-ui>`_ - generating Ember forms to edit data
* `ETH-transactions-storage <https://github.com/Adamant-im/ETH-transactions-storage>`_ - indexer for Ethereum to get transaction list by ETH address
* `ext-postgrest-crud <https://github.com/timwis/ext-postgrest-crud>`_ - browser-based spreadsheet
* `general <https://github.com/PierreRochard/general>`_ - example auth back-end
* `goodfilm <https://github.com/tyrchen/goodfilm>`_ - example film API
* `guild-operators <https://github.com/cardano-community/koios-artifacts/tree/main/files/grest>`_ - example queries and functions that the Cardano Community uses for their Guild Operators' Repository
* `handsontable-postgrest <https://github.com/timwis/handsontable-postgrest>`_ - an excel-like database table editor
* `heritage-near-me <https://github.com/CodeforAustralia/heritage-near-me>`_ - Elm and PostgREST with PostGIS
* `ng-admin-postgrest <https://github.com/marmelab/ng-admin-postgrest>`_ - automatic database admin panel
* `pgrst-dev-setup <https://github.com/Qu4tro/pgrst-dev-setup>`_ - docker-compose and tmuxp setup for experimentation.
* `postgres-postgrest-cloudflared-example <https://github.com/cloudflare/postgres-postgrest-cloudflared-example>`_ - docker-compose setup exposing PostgREST using cloudfared
* `postgrest-demo <https://github.com/SMRxT/postgrest-demo>`_ - multi-tenant logging system
* `postgrest-example <https://github.com/begriffs/postgrest-example>`_ - sqitch versioning for API
* `postgrest-sessions-example <https://github.com/monacoremo/postgrest-sessions-example>`_ - example for cookie-based sessions
* `postgrest-translation-proxy <https://github.com/NikolayS/postgrest-translation-proxy>`_ - calling to external translation service
* `postgrest-ui <https://github.com/tatut/postgrest-ui>`_ - ClojureScript UI components for PostgREST
* `postgrest-vercel <https://github.com/seveibar/postgrest-vercel>`_ - run PostgREST on Vercel (Serverless/AWS Lambda)
* `PostgrestSkeleton <https://github.com/Recmo/PostgrestSkeleton>`_ - Docker Compose, PostgREST, Nginx and Auth0
* `PostGUI <https://github.com/priyank-purohit/PostGUI>`_ - React Material UI admin panel
* `prospector <https://github.com/sfcta/prospector>`_ - data warehouse and visualization platform

.. _devops:

DevOps
------

* `cloudgov-demo-postgrest <https://github.com/GSA/cloudgov-demo-postgrest>`_ - demo for a federally-compliant REST API on cloud.gov
* `cloudstark/helm-charts <https://github.com/cloudstark/helm-charts/tree/master/postgrest>`_ - helm chart to deploy PostgREST to a Kubernetes cluster via a Deployment and Service
* `jbkarle/postgrest <https://github.com/jbkarle/postgrest>`_ - helm chart with a demo database for development and test purposes
* `Limezest/postgrest-cloud-run <https://github.com/Limezest/postgrest-cloud-run>`_ - expose a PostgreSQL database on Cloud SQL using Cloud Run
* `eyberg/postgrest <https://repo.ops.city/v2/packages/eyberg/postgrest/10.1.1/x86_64/show>`_ - run PostgREST as a Nanos unikernel

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
* `postgrest-node <https://github.com/seveibar/postgrest-node>`_ - Run a PostgREST server in Node.js via npm module
* `postgrest-oauth <https://github.com/nblumoe/postgrest-oauth>`_ - OAuth2 WAI middleware
* `postgrest-oauth/api <https://github.com/postgrest-oauth/api>`_ - OAuth2 server
* `PostgREST-writeAPI <https://github.com/ppKrauss/PostgREST-writeAPI>`_ - generate Nginx rewrite rules to fit an OpenAPI spec
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
* `postgrest-csharp <https://github.com/supabase-community/postgrest-csharp>`_ - C#
* `postgrest-dart <https://github.com/supabase-community/postgrest-dart>`_ - Dart
* `postgrest-ex <https://github.com/J0/postgrest-ex>`_ - Elixir
* `postgrest-go <https://github.com/supabase-community/postgrest-go>`_ - Go
* `postgrest-js <https://github.com/supabase/postgrest-js>`_ - TypeScript/JavaScript
* `postgrest-kt <https://github.com/supabase-community/postgrest-kt>`_ - Kotlin
* `postgrest-py <https://github.com/supabase-community/postgrest-py>`_ - Python
* `postgrest-request <https://github.com/lewisjared/postgrest-request>`_ - JS, SuperAgent
* `postgrest-rs <https://github.com/supabase-community/postgrest-rs>`_ - Rust
* `postgrest-sharp-client <https://github.com/thejettdurham/postgrest-sharp-client>`_ (needs maintainer) - C#, RestSharp
* `postgrest-swift <https://github.com/supabase-community/postgrest-swift>`_ - Swift
* `postgrest-url <https://github.com/hugomrdias/postgrest-url>`_ - JS, just for generating query URLs
* `postgrest_python_requests_client <https://github.com/davidthewatson/postgrest_python_requests_client>`_ - Python
* `postgrester <https://github.com/ivangabriele/postgrester>`_ - JS + Typescript
* `postgrestR <https://github.com/clesiemo3/postgrestR>`_ - R
* `py-postgrest <https://github.com/Kong/py-postgrest>`_ - Python
* `redux-postgrest <https://github.com/andytango/redux-postgrest>`_ - TypeScript/JS, client integrated with (React) Redux.
* `vue-postgrest <https://github.com/technowledgy/vue-postgrest>`_ - Vue.js

