{ postgrest
, envsubst
, locust
, openresty
, postgresql
, withTmpDb
, writeShellScript
, writeShellScriptBin
, writeText
}:

let
  runner =
    writeShellScript "postgrest-loadtest-runner"
      ''
        set -euo pipefail

        tmpdir=$(mktemp -d)
        trap "rm -r $tmpdir" exit

        export POSTGREST_TEST_DIR="$tmpdir"

        ${postgrest}/bin/postgrest ${postgrestConf} &
        postgrestPID=$!

        export POSTGREST_API_URL="http://localhost:3000"

        mkdir -p "$tmpdir"/{logs,conf}
        touch "$tmpdir"/logs/{error.log,access.log}
        ${envsubst}/bin/envsubst -i ${nginxConf} \
          -o "$tmpdir/conf/nginx.conf"

        ${openresty}/bin/openresty -p "$tmpdir" &
        nginxPID=$!

        cleanup() {
          kill $postgrestPID $nginxPID || true
          rm -r "$tmpdir"
        }

        trap cleanup exit

        ${locust}/bin/locust -f ${../test/loadtest/locustfile.py} \
            -H http://localhost:3080 --users 100 --spawn-rate 20
            # --headless -t 60s
      '';

  postgrestConf =
    writeText "postgrest.conf"
      ''
        db-uri = "$(POSTGREST_TEST_CONNECTION)"
        db-schema = "test"
        db-anon-role = "postgrest_test_anonymous"
        db-pool = 1
        server-port = 3000
        jwt-secret = "reallyreallyreallyreallyverysafe"
      '';

  nginxConf =
    writeText "nginx.conf"
      ''
        daemon off;
        error_log stderr info;
        pid nginx.pid;

        events {}

        http {
            access_log access.log;
            client_body_temp_path . 1 2;
            proxy_temp_path proxy_temp;
            fastcgi_temp_path fastcgi_temp;
            uwsgi_temp_path uwsgi_temp;
            scgi_temp_path scgi_temp;

            include ${openresty}/nginx/conf/mime.types;
            default_type application/ocet-stream;

            gzip off;
            sendfile on;

            keepalive_timeout 65s;

            server {
                listen 3080;

                location / {
                    more_clear_input_headers Accept-Encoding;
                    proxy_pass $POSTGREST_API_URL;
                }
            }
        }
      '';
in
writeShellScriptBin "postgrest-loadtest"
  ''
    set -euo pipefail

    ${withTmpDb postgresql} ${runner}
  ''
