#!/usr/bin/env bash
# Run unit tests for Input/Ouput of PostgREST seen as a black box
# with test output in Test Anything Protocol format.
#
# These tests expect that `postgrest` is on the PATH, as well as `curl` and
# `ncat` (from the nmap package in some distribution).
#
# References:
#   [1] Test Anything Protocol
#   https://testanything.org/
#
#   [2] TAP Specification
#   https://testanything.org/tap-specification.html
#
#   [3] List of TCP and UDP port numbers
#   https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers
#
set -eu

export POSTGREST_TEST_CONNECTION=${POSTGREST_TEST_CONNECTION:-"postgres:///postgrest_test"}

cd "$(dirname "$0")"
cd io-tests

trap "kill 0" int term exit

# Port for Test PostgREST Server (must match config)
pgrPort=49421 # in range 49152–65535: for private or temporary use

# TAP utilities
currentTest=1
failedTests=0
bailOut(){ echo "Bail out! $1"; exit 1; }
result(){ echo "$1 $currentTest $2"; currentTest=$(( $currentTest + 1 )); }
todo(){ result 'ok' "# TODO: $*"; }
skip(){ result 'ok' "# SKIP: $*"; }
ok(){ result 'ok' "- $1"; }
ko(){ result 'not ok' "- $1"; failedTests=$(( $failedTests + 1 )); }
comment(){ echo "# $1"; }

# Utilities to start/stop test PostgREST server running in the background
pgrStart(){ postgrest $1 >/dev/null 2>/dev/null & pgrPID="$!"; }
pgrStartRead(){ postgrest $1 <$2 >/dev/null & pgrPID="$!"; }
pgrStartStdin(){ postgrest $1 >/dev/null <<< "$2" & pgrPID="$!"; }
pgrStarted(){ kill -0 "$pgrPID" 2>/dev/null; }
pgrStop(){ kill "$pgrPID" 2>/dev/null; pgrPID=""; }

# Utilities to send HTTP requests to the PostgREST server
rootStatus(){
  curl -s -o /dev/null -w '%{http_code}' "http://localhost:$pgrPort/"
}

authorsStatus(){
  curl -s -o /dev/null -w '%{http_code}' \
    -H "Authorization: Bearer $1" \
    "http://localhost:$pgrPort/authors_only"
}

v1SchemaParentsStatus(){
  curl -s -o /dev/null -w '%{http_code}' \
    -H "Accept-Profile: v1" \
    "http://localhost:$pgrPort/parents"
}

# Unit Test Templates
readSecretFromFile(){
  case "$1" in
    *.b64)
      pgrConfig="base64-secret-from-file.config";;
    *)
      pgrConfig="secret-from-file.config";;
  esac
  pgrStartRead "./configs/$pgrConfig" "./secrets/$1"
  while pgrStarted && test "$( rootStatus )" -ne 200
  do
    # wait for the server to start
    sleep 0.1 \
    || sleep 1 # fallback: subsecond sleep is not standard and may fail
  done
  if pgrStarted
  then
    authorsJwt="./secrets/${1%.*}.jwt"
    httpStatus="$( authorsStatus $(cat "$authorsJwt") )"
    if test "$httpStatus" -eq 200
    then
      ok "authentication with $2 secret read from a file"
    else
      ko "authentication with $2 secret read from a file: $httpStatus"
    fi
  else
    ko "failed to read $2 secret from a file"
  fi
  pgrStop
}

readDbUriFromStdin(){
  pgrConfig="dburi-from-file.config"
  pgrStartStdin "./configs/$pgrConfig" "$1"
  while pgrStarted && test "$( rootStatus )" -ne 200
  do
    # wait for the server to start
    sleep 0.1 \
    || sleep 1 # fallback: subsecond sleep is not standard and may fail
  done
  if pgrStarted
  then
    ok "connection with $2 dburi read from stdin / a file"
  else
    ko "connection with $2 dburi read from stdin / a file"
  fi
  pgrStop
}

reqWithRoleClaimKey(){
  export ROLE_CLAIM_KEY=$1
  pgrStart "./configs/role-claim-key.config"
  while pgrStarted && test "$( rootStatus )" -ne 200
  do
    # wait for the server to start
    sleep 0.1 \
    || sleep 1 # fallback: subsecond sleep is not standard and may fail
  done
  authorsJwt=$(psql -qtAX "$POSTGREST_TEST_CONNECTION" -c "select jwt.sign('$2', 'reallyreallyreallyreallyverysafe');")
  httpStatus="$( authorsStatus "$authorsJwt" )"
  if test "$httpStatus" -eq $3
  then
    ok "request with \"$1\" role-claim-key for $2 jwt: $httpStatus"
  else
    ko "request with \"$1\" role-claim-key for $2 jwt: $httpStatus"
  fi
  pgrStop
}

invalidRoleClaimKey(){
  export ROLE_CLAIM_KEY=$1
  pgrStart "./configs/role-claim-key.config"
  while pgrStarted && test "$( rootStatus )" -ne 200
  do
    # wait for the server to start
    sleep 0.1 \
    || sleep 1 # fallback: subsecond sleep is not standard and may fail
  done
  if pgrStarted
  then
    ko "invalid jspath \"$1\": accepted"
    pgrStop
  else
    ok "invalid jspath \"$1\": rejected"
  fi
}

# ensure iat claim is successful in the presence of pgrst time cache, see https://github.com/PostgREST/postgrest/issues/1139
ensureIatClaimWorks(){
  pgrStart "./configs/simple.config"
  while pgrStarted && test "$( rootStatus )" -ne 200
  do
    # wait for the server to start
    sleep 0.1 \
    || sleep 1 # fallback: subsecond sleep is not standard and may fail
  done
  for i in {1..10}; do \
    iatJwt=$(psql -qtAX "$POSTGREST_TEST_CONNECTION" -c "select jwt.sign(row_to_json(r), 'reallyreallyreallyreallyverysafe') from ( select 'postgrest_test_author' as role, extract(epoch from now()) as iat) r")
    httpStatus="$( authorsStatus $iatJwt )"
    if test "$httpStatus" -ne 200
    then
      ko "iat claim rejected: $httpStatus"
      return
    fi
    sleep .5;\
  done
  ok "iat claim accepted"
  pgrStop
}

# ensure app settings don't reset on pool timeout of 10 seconds, see https://github.com/PostgREST/postgrest/issues/1141
ensureAppSettings(){
  pgrStart "./configs/app-settings.config"
  while pgrStarted && test "$( rootStatus )" -ne 200
  do
    # wait for the server to start
    sleep 0.1 \
    || sleep 1 # fallback: subsecond sleep is not standard and may fail
  done
  sleep 11
  response=$(curl -s "http://localhost:$pgrPort/rpc/get_guc_value?name=app.settings.external_api_secret")
  if test "$response" = "\"0123456789abcdef\""
  then
    ok "GET /rpc/get_guc_value: $response"
  else
    ko "GET /rpc/get_guc_value: $response"
  fi
  pgrStop
}

checkAppSettingsReload(){
  pgrStart "./configs/sigusr2-settings.config"
  while pgrStarted && test "$( rootStatus )" -ne 200
  do
    # wait for the server to start
    sleep 0.1 \
    || sleep 1 # fallback: subsecond sleep is not standard and may fail
  done
  # change setting
  replaceConfigValue "app.settings.name_var" "Jane" ./configs/sigusr2-settings.config
  # reload
  kill -s SIGUSR2 $pgrPID
  response=$(curl -s "http://localhost:$pgrPort/rpc/get_guc_value?name=app.settings.name_var")
  if test "$response" = "\"Jane\""
  then
    ok "app.settings.name_var config reloaded with SIGUSR2"
  else
    ko "app.settings.name_var config not reloaded with SIGUSR2. Got: $response"
  fi
  pgrStop
  # go back to original setting
  replaceConfigValue "app.settings.name_var" "John" ./configs/sigusr2-settings.config
}

checkJwtSecretReload(){
  pgrStart "./configs/sigusr2-settings.config"
  while pgrStarted && test "$( rootStatus )" -ne 200
  do
    # wait for the server to start
    sleep 0.1 \
    || sleep 1 # fallback: subsecond sleep is not standard and may fail
  done
  secret="reallyreallyreallyreallyverysafe"
  # change setting
  replaceConfigValue "jwt-secret" "$secret" ./configs/sigusr2-settings.config
  # reload
  kill -s SIGUSR2 $pgrPID
  payload='{"role":"postgrest_test_author"}'
  authorsJwt=$(psql -qtAX "$POSTGREST_TEST_CONNECTION" -c "select jwt.sign('$payload', '$secret');")
  httpStatus="$( authorsStatus "$authorsJwt" )"
  if test "$httpStatus" -eq 200
  then
    ok "jwt-secret config reloaded with SIGUSR2"
  else
    ko "jwt-secret config not reloaded with SIGUSR2. Got: $httpStatus"
  fi
  pgrStop
  # go back to original setting
  replaceConfigValue "jwt-secret" "invalidinvalidinvalidinvalidinvalid" ./configs/sigusr2-settings.config
}

checkDbSchemaReload(){
  pgrStart "./configs/sigusr2-settings.config"
  while pgrStarted && test "$( rootStatus )" -ne 200
  do
    # wait for the server to start
    sleep 0.1 \
    || sleep 1 # fallback: subsecond sleep is not standard and may fail
  done
  secret="reallyreallyreallyreallyverysafe"
  # add v1 schema to db-schema
  replaceConfigValue "db-schema" "test, v1" ./configs/sigusr2-settings.config
  # reload
  kill -s SIGUSR2 $pgrPID
  httpStatus="$(v1SchemaParentsStatus)"
  if test "$httpStatus" -eq 200
  then
    ok "db-schema config reloaded with SIGUSR2"
  else
    ko "db-schema config not reloaded with SIGUSR2. Got: $httpStatus"
  fi
  pgrStop
  # go back to original setting
  replaceConfigValue "db-schema" "test" ./configs/sigusr2-settings.config
}

replaceConfigValue(){
  sed -i "s/.*$1.*/$1 = \"$2\"/g" $3
}

getSocketStatus() {
  curl -sL -w "%{http_code}\\n" -o /dev/null localhost:54321
}

socketConnection(){
  # map port 54321 traffic to unix socket as workaround for curl below 7.40
  # not supporting --unix-socket flag
  ncat -vlk 54321 -c 'ncat -U /tmp/postgrest.sock' &
  pgrStart "./configs/unix-socket.config"
  while pgrStarted && test "$( getSocketStatus )" -ne 200
  do
    # wait for the server to start
    sleep 0.1 \
    || sleep 1 # fallback: subsecond sleep is not standard and may fail
  done
  if test $( getSocketStatus ) -eq 200
  then
    ok "Succesfully connected through unix socket"
  else
    ko "Failed to connect through unix socket"
  fi
  pgrStop
}

# PRE: curl must be available
test -n "$(command -v curl)" || bailOut 'curl is not available'

# PRE: postgres must be running
psql -l "$POSTGREST_TEST_CONNECTION" 1>/dev/null 2>/dev/null || bailOut 'postgres is not running'

echo "Running IO tests.."

socketConnection

readSecretFromFile word.noeol 'simple (no EOL)'
readSecretFromFile word.txt 'simple'
readSecretFromFile ascii.noeol 'ASCII (no EOL)'
readSecretFromFile ascii.txt 'ASCII'
readSecretFromFile utf8.noeol 'UTF-8 (no EOL)'
readSecretFromFile utf8.txt 'UTF-8'
readSecretFromFile binary.noeol 'binary'
readSecretFromFile binary.eol 'binary (+EOL)'

readSecretFromFile word.b64 'Base64 (simple)'
readSecretFromFile ascii.b64 'Base64 (ASCII)'
readSecretFromFile utf8.b64 'Base64 (UTF-8)'
readSecretFromFile binary.b64 'Base64 (binary)'

eol=$'\x0a'

readDbUriFromStdin "$POSTGREST_TEST_CONNECTION" "(no EOL)"
readDbUriFromStdin "$POSTGREST_TEST_CONNECTION$eol" "(EOL)"

reqWithRoleClaimKey '.postgrest.a_role' '{"postgrest":{"a_role":"postgrest_test_author"}}' 200
reqWithRoleClaimKey '.customObject.manyRoles[1]' '{"customObject":{"manyRoles": ["other", "postgrest_test_author"]}}' 200
reqWithRoleClaimKey '."https://www.example.com/roles"[0].value' '{"https://www.example.com/roles":[{"value":"postgrest_test_author"}]}' 200
reqWithRoleClaimKey '.myDomain[3]' '{"myDomain":["other","postgrest_test_author"]}' 401
reqWithRoleClaimKey '.myRole' '{"role":"postgrest_test_author"}' 401

invalidRoleClaimKey 'role.other'
invalidRoleClaimKey '.role##'
invalidRoleClaimKey '.my_role;;domain'
invalidRoleClaimKey '.#$%&$%/'
invalidRoleClaimKey ''
invalidRoleClaimKey 1234

ensureIatClaimWorks
ensureAppSettings

checkAppSettingsReload
checkJwtSecretReload
checkDbSchemaReload
# TODO: SIGUSR2 tests for other config options

trap - int term exit

exit $failedTests
