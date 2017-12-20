#!/bin/sh
currentTest=1
failedTests=0
result(){ echo "$1 $currentTest $2"; currentTest=$(( $currentTest + 1 )); }
ok(){ result 'ok' "- $1"; }
ko(){ result 'not ok' "- $1"; failedTests=$(( $failedTests + 1 )); }

pgrPort=49421

pgrStopAll(){ pkill -f "$(stack path --local-install-root)/bin/postgrest"; }

pgrStart(){
  stack build --profile
  stack exec -- postgrest test/memory-tests/config +RTS -p -h >/dev/null & pgrPID="$!";
}
pgrStop(){ kill "$pgrPID" 2>/dev/null; }

setUp(){ pgrStopAll; }
cleanUp(){ pgrStopAll; }

checkPgrStarted(){
  while pgrStarted && test $(rootStatus) -ne 200
  do
    sleep 1
  done
}
pgrStarted(){ kill -0 "$pgrPID" 2>/dev/null; }
rootStatus(){
  curl -s -o /dev/null -I -w '%{http_code}' "http://localhost:$pgrPort/"
}

memoryTest(){
  pgrStart
  checkPgrStarted
  factor=$(( 3*$(numfmt --from=si $1)/4 )) # 3/4 on $1 is need to maintain the specified size because of base64
  payload="{\"blob\" : \"$(dd if=/dev/zero bs=$factor count=1 status=none | base64)\"}"
  httpStatus=$(echo $payload | curl -s -H "Content-Type: application/json" --request $2 -d @- -w '%{http_code}' http://localhost:$pgrPort$3 | tr -d '"')
  if test "$httpStatus" -ge 200 && test "$httpStatus" -lt 210
  then
    pgrStop
    while [ ! -s postgrest.prof ]
    do
      sleep 1
    done
    BYTES_FMT=$(cat postgrest.prof | grep -o -P '(?<=alloc =).*(?=bytes)' | tr -d ' ')
    BYTES=$(echo $BYTES_FMT | tr -d ',')
    MAX_BYTES=$(numfmt --from=si $4)
    if test $BYTES -le $MAX_BYTES
    then
      ok "$2 $3: with a $1 payload size the memory usage($BYTES_FMT bytes) is less than $4"
    else
      ko "$2 $3: with a $1 payload size the memory usage($BYTES_FMT bytes) is more than $4"
    fi
  else
    pgrStop
    ko "$2 $3: request failed with http $httpStatus"
  fi
}

setUp

echo "Running memory usage tests.."

memoryTest "1M" "POST" "/rpc/leak" "15M"
memoryTest "1M" "POST" "/leak" "15M"
memoryTest "1M" "PATCH" "/leak?id=eq.1" "15M"

memoryTest "10M" "POST" "/rpc/leak" "105M"
memoryTest "10M" "POST" "/leak" "105M"
memoryTest "10M" "PATCH" "/leak?id=eq.1" "105M"

memoryTest "100M" "POST" "/rpc/leak" "895M"
memoryTest "100M" "POST" "/leak" "895M"
memoryTest "100M" "PATCH" "/leak?id=eq.1" "895M"

cleanUp

exit $failedTests
