#!/bin/bash

#This helper script lets you quickly and easily run one or more PostgREST containers connected to any Postgres DB and schema.

#If you execute `run_standalone_postgrest.sh` with no options, it will map port 3000 to localhost and try to connect to the public schema of the postgres database on a docker container named 'db'.

#To change the configuration, either export any of the environment variables used by the script, or set them when you run like this:

`SKIP_LINK_CONTAINER="" PG_PORT_5432_TCP_ADDR=someserver.example.com ./run_standalone_postgrest.sh`

export PG_PORT_5432_TCP_ADDR=${PG_PORT_5432_TCP_ADDR:-db}
export PG_PORT_5432_TCP_PORT=${PG_PORT_5432_TCP_PORT:-5432}
export PG_ENV_POSTGRES_USER=${PG_ENV_POSTGRES_USER:-postgres}
export PG_ENV_POSTGRES_DB=${PG_ENV_POSTGRES_DB:-postgres}

export POSTGREST_SCHEMA=${POSTGREST_SCHEMA:-public}
export POSTGREST_ANONYMOUS=${POSTGREST_ANONYMOUS:-postgres}
export POSTGREST_JWT_SECRET=${POSTGREST_JWT_SECRET:-thisisnotarealsecret}
export POSTGREST_MAX_ROWS=${POSTGREST_MAX_ROWS:-1000000}
export POSTGREST_POOL=${POSTGREST_POOL:-200}

HOST_PORT=${HOST_PORT=3000}
LINK_CONTAINER=${LINK_CONTAINER="$PG_PORT_5432_TCP_ADDR:$PG_PORT_5432_TCP_ADDR"}

clear
cat << EOF

Usage: [ENV_VAR=value]... ${0##*/}
Change the environment variable before calling the script to override.
Set SKIP_LINK_CONTAINER="" (an empty string) to omit the --link argument.

${0##*/} will run with these environment settings.

PG_PORT_5432_TCP_ADDR=${PG_PORT_5432_TCP_ADDR}
PG_PORT_5432_TCP_PORT=${PG_PORT_5432_TCP_PORT}
PG_ENV_POSTGRES_USER=${PG_ENV_POSTGRES_USER}
PG_ENV_POSTGRES_DB=${PG_ENV_POSTGRES_DB}

POSTGREST_SCHEMA=${POSTGREST_SCHEMA}
POSTGREST_ANONYMOUS=${POSTGREST_ANONYMOUS}
POSTGREST_JWT_SECRET=${POSTGREST_JWT_SECRET}
POSTGREST_MAX_ROWS=${POSTGREST_MAX_ROWS}
POSTGREST_POOL=${POSTGREST_POOL}

HOST_PORT=${HOST_PORT}
CONTAINER_NAME=${CONTAINER_NAME:-"<random>"}
SKIP_LINK_CONTAINER=${SKIP_LINK_CONTAINER-"<will not skip>"}
LINK_CONTAINER=${LINK_CONTAINER}

"Running..."
EOF

CONTAINER_ID=$(docker run -d -p 3000:$HOST_PORT ${CONTAINER_NAME:+"--name"} $CONTAINER_NAME ${SKIP_LINK_CONTAINER-"--link"} ${SKIP_LINK_CONTAINER-${LINK_CONTAINER}} -e PG_PORT_5432_TCP_ADDR -e PG_PORT_5432_TCP_PORT -e PG_ENV_POSTGRES_USER -e PG_ENV_POSTGRES_DB -e POSTGREST_SCHEMA -e POSTGREST_ANONYMOUS -e POSTGREST_JWT_SECRET -e POSTGREST_MAX_ROWS -e POSTGREST_POOL begriffs/postgrest)

docker ps -la
docker logs $CONTAINER_ID
