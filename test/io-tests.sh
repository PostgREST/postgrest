#!/bin/sh
# Run unit tests for Input/Ouput of PostgREST seen as a black box
# with test output in Test Anything Protocol format.
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
cd "$(dirname "$0")"
cd io-tests

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
pgrStart(){ stack exec -- postgrest "$1" >/dev/null & pgrPID="$!"; }
pgrStartRead(){ stack exec -- postgrest "$1" >/dev/null < "$2" & pgrPID="$!"; }
pgrStarted(){ kill -0 "$pgrPID" 2>/dev/null; }
pgrStop(){ kill "$pgrPID" 2>/dev/null; }
pgrStopAll(){ pkill -f "$(stack path --local-install-root)/bin/postgrest"; }

# Utilities to send HTTP requests to the PostgREST server
rootStatus(){
  curl -s -o /dev/null -w '%{http_code}' "http://localhost:$pgrPort/"
}

authorsStatus(){
  curl -s -o /dev/null -w '%{http_code}' \
    -H "Authorization: Bearer $1" \
    "http://localhost:$pgrPort/authors_only"
}

# Start and End of Unit Tests
setUp(){ pgrStopAll; }
cleanUp(){ pgrStopAll; }

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
      ko "failed to authenticate using JWT for $2 secret: $httpStatus"
    fi
  else
    ko "failed to read $2 secret from a file"
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
  authorsJwt=$(psql -qtAX postgrest_test -c "select jwt.sign('$2', 'reallyreallyreallyreallyverysafe');")
  httpStatus="$( authorsStatus "$authorsJwt" )"
  if test "$httpStatus" -eq $3
  then
    ok "request with \"$1\" role-claim-key for $2 jwt gave $3"
  else
    ko "request with \"$1\" role-claim-key for $2 jwt gave $httpStatus"
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
    ko "invalid jspath \"$1\" accepted"
  else
    ok "invalid jspath \"$1\" rejected"
  fi
  pgrStop
}

# PRE: curl must be available
test -n "$(command -v curl)" || bailOut 'curl is not available'

# PRE: postgres must be running
psql -l 1>/dev/null 2>/dev/null || bailOut 'postgres is not running'

setUp

echo "Running IO tests.."

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

cleanUp

exit $failedTests
