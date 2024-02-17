.. _deploy_heroku:

Heroku
======

1.  Log into Heroku using the `Heroku CLI <https://devcenter.heroku.com/articles/heroku-cli>`_:

  .. code-block:: bash

    # If you have multiple Heroku accounts, use flag '--interactive' to switch between them
    heroku login --interactive


2.  Create a new Heroku app using the PostgREST buildpack:

  .. code-block:: bash

    mkdir ${YOUR_APP_NAME}
    cd ${YOUR_APP_NAME}
    git init .

    heroku apps:create ${YOUR_APP_NAME} --buildpack https://github.com/PostgREST/postgrest-heroku.git
    heroku git:remote -a ${YOUR_APP_NAME}

3.  Create a new Heroku PostgreSQL add-on attached to the app and keep notes of the assigned add-on name (e.g. :code:`postgresql-curly-58902`) referred later as ${HEROKU_PG_DB_NAME}

  .. code-block:: bash

    heroku addons:create heroku-postgresql:standard-0 -a ${YOUR_APP_NAME}
    # wait until the add-on is available
    heroku pg:wait -a ${YOUR_APP_NAME}

4.  Create the necessary user roles according to the
    `PostgREST documentation <https://postgrest.org/en/stable/auth.html>`_:

  .. code-block:: bash

    heroku pg:credentials:create --name api_user -a ${YOUR_APP_NAME}
    # use the following command to ensure the new credential state is active before attaching it
    heroku pg:credentials -a ${YOUR_APP_NAME}

    heroku addons:attach ${HEROKU_PG_DB_NAME} --credential api_user -a ${YOUR_APP_NAME}

5.  Connect to the PostgreSQL database and create some sample data:

  .. code-block:: bash

    heroku psql -a ${YOUR_APP_NAME}

  .. code-block:: postgres

    # from the psql command prompt execute the following commands:
    create schema api;

    create table api.todos (
    id serial primary key,
    done boolean not null default false,
    task text not null,
    due timestamptz
    );

    insert into api.todos (task) values
    ('finish tutorial 0'), ('pat self on back');

    grant usage on schema api to api_user;
    grant select on api.todos to api_user;

6.  Create the :code:`Procfile`:

  .. code-block:: bash

    web: PGRST_SERVER_HOST=0.0.0.0 PGRST_SERVER_PORT=${PORT} PGRST_DB_URI=${PGRST_DB_URI:-${DATABASE_URL}} ./postgrest-${POSTGREST_VER}
  ..

  Set the following environment variables on Heroku:

  .. code-block:: bash

    heroku config:set POSTGREST_VER=10.0.0
    heroku config:set PGRST_DB_SCHEMA=api
    heroku config:set PGRST_DB_ANON_ROLE=api_user
  ..

  PGRST_DB_URI can be set if an external database is used or if it's different from the default Heroku DATABASE_URL. This latter is used if nothing is provided.
  POSTGREST_VER is mandatory to select and build the required PostgREST release.

  See https://postgrest.org/en/stable/configuration.html#environment-variables for the full list of environment variables.

7.  Build and deploy your app:

  .. code-block:: bash

    git add Procfile
    git commit -m "PostgREST on Heroku"
    git push heroku master
  ..

  Your Heroku app should be live at :code:`${YOUR_APP_NAME}.herokuapp.com`

8.  Test your app

    From a terminal display the application logs:

  .. code-block:: bash

    heroku logs -t
  ..

  From a different terminal retrieve with curl the records previously created:

  .. code-block:: bash

    curl https://${YOUR_APP_NAME}.herokuapp.com/todos
  ..

  and test that any attempt to modify the table via a read-only user is not allowed:

  .. code-block:: bash

    curl https://${YOUR_APP_NAME}.herokuapp.com/todos -X POST \
     -H "Content-Type: application/json" \
     -d '{"task": "do bad thing"}'
