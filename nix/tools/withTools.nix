{ buildToolbox
, checkedShellScript
, curl
, git
, lib
, postgresqlVersions
, postgrest
, python3Packages
, slocat
, writeText
, writers
, toxiproxy
}:
let
  withTmpDb =
    { name, postgresql }:
    let
      commandName = "postgrest-with-${name}";
    in
    checkedShellScript
      {
        name = commandName;
        docs = "Run the given command in a temporary database with ${name}. If you wish to mutate the database, login with the postgres role.";
        args =
          [
            "ARG_OPTIONAL_SINGLE([fixtures], [f], [SQL file to load fixtures from])"
            "ARG_POSITIONAL_SINGLE([command], [Command to run])"
            "ARG_LEFTOVERS([command arguments])"
            "ARG_USE_ENV([PGUSER], [postgrest_test_authenticator], [Authenticator PG role])"
            "ARG_USE_ENV([PGDATABASE], [postgres], [PG database name])"
            "ARG_USE_ENV([PGRST_DB_SCHEMAS], [test], [Schema to expose])"
            "ARG_USE_ENV([PGTZ], [utc], [Timezone to use])"
            "ARG_USE_ENV([PGOPTIONS], [-c search_path=public,test], [PG options to use])"
            "ARG_OPTIONAL_BOOLEAN([replica],, [Enable a replica for the database])"
          ];
        positionalCompletion = "_command";
        workingDir = "/";
        redirectTixFiles = false;
        withPath = [ postgresql toxiproxy ];
        withTmpDir = true;
      }
      ''
        setuplog="$tmpdir/setup.log"

        log () {
          echo "$1" >> "$setuplog"
        }

        # Avoid starting multiple layers of withTmpDb, but make sure to have the last invocation
        # load fixtures. Otherwise postgrest-with-pg-xx postgrest-test-io would not be possible.
        if ! test -v PGHOST; then

          mkdir -p "$tmpdir"/{db,socket}
          # remove data dir, even if we keep tmpdir - no need to upload it to artifacts
          trap 'rm -rf $tmpdir/db' EXIT

          export PGDATA="$tmpdir/db"
          export PGHOST="$tmpdir/socket"
          export PGPORT=6432
          export PGUSER
          export PGDATABASE
          export PGRST_DB_SCHEMAS
          export PGTZ
          export PGOPTIONS

          HBA_FILE="$tmpdir/pg_hba.conf"
          echo "local $PGDATABASE some_protected_user password" > "$HBA_FILE"
          {
            echo "local $PGDATABASE all trust"
            echo "local replication all trust"
            echo "host $PGDATABASE some_protected_user localhost scram-sha-256"
            echo "host $PGDATABASE all localhost trust"
          } >> "$HBA_FILE"

          UNIX_PGHOST="$PGHOST"
          export TCP_PGHOST="localhost"
          REAL_PGPORT="$PGPORT"
          export TOXI_PGPORT=7432

          log "Initializing database cluster..."
          # We try to make the database cluster as independent as possible from the host
          # by specifying the timezone, locale and encoding.
          # initdb -U creates a superuser(man initdb)
          TZ=$PGTZ initdb --no-locale --encoding=UTF8 --nosync -U postgres --auth=trust \
            >> "$setuplog"

          log "Starting the database cluster..."

          # Instead of listening on a local port, we will listen on a unix domain socket.
          # NOTE: unix domain socket filename name must remain under max limit.
          # On Linux, it's 108 chars (including '\0' terminator)
          # On MacOS, it's 104 chars
          # See: https://serverfault.com/questions/641347/check-if-a-path-exceeds-maximum-for-unix-domain-socket

          pg_ctl -l "$tmpdir/db.log" -w start -o "-F -c listen_addresses=\"$TCP_PGHOST\" -c port=$REAL_PGPORT -c hba_file=$HBA_FILE -k $UNIX_PGHOST -c log_statement=\"all\" " \
            >> "$setuplog"

          LOG_LEVEL=error toxiproxy-server&
          TOXIPROXY_PID=$!
          sleep 1 # give the server a moment to start
          toxiproxy-cli create -l "$TCP_PGHOST:$TOXI_PGPORT" -u "$TCP_PGHOST:$REAL_PGPORT" pg

          log "Creating a minimally privileged $PGUSER connection role..."
          createuser "$PGUSER" -U postgres --host="$tmpdir/socket" --no-createdb --no-inherit --no-superuser --no-createrole --no-replication --login

          >&2 echo "${commandName}: You can connect with: psql 'postgres:///$PGDATABASE?host=$PGHOST' -U postgres"
          >&2 echo "${commandName}: You can tail the logs with: tail -f $tmpdir/db.log"

          if test "$_arg_replica" = "on"; then
            replica_slot="replica_$RANDOM"
            replica_dir="$tmpdir/$replica_slot"
            replica_host="$tmpdir/socket_$replica_slot"
            replica_port=6433
            export TOXI_REPLICA_PGPORT=7433

            mkdir -p "$replica_host"

            replica_dblog="$tmpdir/db_$replica_slot.log"

            log "Running pg_basebackup for $replica_slot"

            pg_basebackup -v -h "$PGHOST" -U postgres --wal-method=stream --create-slot --slot="$replica_slot" --write-recovery-conf -D "$replica_dir" \
              >> "$setuplog" 2>&1

            log "Starting replica on $replica_host"

            pg_ctl -D "$replica_dir" -l "$replica_dblog" -w start -o "-F -c listen_addresses=\"$TCP_PGHOST\" -c port=$replica_port -c hba_file=$HBA_FILE -k $replica_host -c log_statement=\"all\" " \
              >> "$setuplog"
            toxiproxy-cli create -l $TCP_PGHOST:$TOXI_REPLICA_PGPORT -u $TCP_PGHOST:$replica_port pg_replica

            >&2 echo "${commandName}: Replica enabled. You can connect to it with: psql 'postgres:///$PGDATABASE?host=$replica_host' -U postgres"
            >&2 echo "${commandName}: You can tail the replica logs with: tail -f $replica_dblog"

            export PGREPLICAHOST="$TCP_PGHOST"
            export PGREPLICAPORT="$TOXI_REPLICA_PGPORT"
            export PGREPLICASLOT="$replica_slot"
            export PGRST_DB_URI="postgres:///$PGDATABASE?host=$PGREPLICAHOST,$TCP_PGHOST&port=$PGREPLICAPORT,$TOXI_PGPORT"
          fi

          # shellcheck disable=SC2317
          stop () {
            log "Stopping the database cluster..."
            pg_ctl stop --mode=immediate >> "$setuplog"
            rm -rf "$tmpdir/db"
            if test "$_arg_replica" = "on"; then
              log "Stopping the replica cluster..."
              pg_ctl -D "$replica_dir" stop --mode=immediate >> "$setuplog"
              rm -rf "$replica_dir"
            fi
            kill "$TOXIPROXY_PID" || true
            wait "$TOXIPROXY_PID" || true
          }
          trap stop EXIT
        fi

        if test "$_arg_fixtures"; then
          load_start=$SECONDS
          >&2 printf "${commandName}: Loading fixtures under the postgres role..."
          psql -U postgres -v PGUSER="$PGUSER" -v ON_ERROR_STOP=1 -f "$_arg_fixtures" >> "$setuplog"
          load_end=$((SECONDS - load_start))
          >&2 printf " done in %ss. Running command...\n" "$load_end"
        fi

        ("$_arg_command" "''${_arg_leftovers[@]}")
        #(PGHOST="$TCP_PGHOST" PGPORT="$TOXI_PGPORT" "$_arg_command" "''${_arg_leftovers[@]}")
      '';

  # Helper script for running a command against all PostgreSQL versions.
  withPgAll =
    let
      runners =
        builtins.map
          (version:
            ''
              cat << EOF

              Running against ${version.name}...

              EOF

              trap 'echo "Failed on ${version.name}"' exit

              (${withTmpDb version} "$_arg_command" "''${_arg_leftovers[@]}")

              trap "" exit

              cat << EOF

              Done running against ${version.name}.

              EOF
            '')
          postgresqlVersions;
    in
    checkedShellScript
      {
        name = "postgrest-with-all";
        docs = "Run command against all supported PostgreSQL versions.";
        args =
          [
            "ARG_POSITIONAL_SINGLE([command], [Command to run])"
            "ARG_LEFTOVERS([command arguments])"
          ];
        positionalCompletion = "_command";
        workingDir = "/";
      }
      (lib.concatStringsSep "\n\n" runners);

  withPg = withTmpDb (builtins.head postgresqlVersions);

  withSlowPg =
    checkedShellScript
      {
        name = "postgrest-with-slow-pg";
        docs = "Run the given command with simulated high latency postgresql";
        args =
          [
            "ARG_POSITIONAL_SINGLE([command], [Command to run])"
            "ARG_LEFTOVERS([command arguments])"
            "ARG_USE_ENV([PGHOST], [], [PG host (socket name)])"
            "ARG_USE_ENV([PGDELAY], [0ms], [extra PG latency (duration)])"
          ];
        positionalCompletion = "_command";
        workingDir = "/";
        redirectTixFiles = false;
        withTmpDir = true;
      }
      ''
        delay="''${PGDELAY:-0ms}"
        echo "delaying data to/from postgres by $delay"

        REALPGHOST="$PGHOST"
        export PGHOST="$tmpdir/socket"
        mkdir -p "$PGHOST"

        ${slocat}/bin/slocat -delay "$delay" -src "$PGHOST/.s.PGSQL.5432" -dst "$REALPGHOST/.s.PGSQL.5432" &
        SLOCAT_PID=$!
        # shellcheck disable=SC2317
        stop_slocat() {
          kill "$SLOCAT_PID" || true
          wait "$SLOCAT_PID" || true
        }
        trap stop_slocat EXIT
        sleep 1 # should wait for socket file to appear instead

        ("$_arg_command" "''${_arg_leftovers[@]}")
      '';

  withSlowPgrst =
    checkedShellScript
      {
        name = "postgrest-with-slow-postgrest";
        docs = "Run the given command with simulated high latency postgrest";
        args =
          [
            "ARG_POSITIONAL_SINGLE([command], [Command to run])"
            "ARG_LEFTOVERS([command arguments])"
            "ARG_USE_ENV([PGRST_SERVER_UNIX_SOCKET], [], [PostgREST host (socket name)])"
            "ARG_USE_ENV([PGRST_DELAY], [0ms], [extra PostgREST latency (duration)])"
          ];
        positionalCompletion = "_command";
        workingDir = "/";
        redirectTixFiles = false;
        withTmpDir = true;
      }
      ''
        delay="''${PGRST_DELAY:-0ms}"
        echo "delaying data to/from PostgREST by $delay"

        REAL_PGRST_SERVER_UNIX_SOCKET="$PGRST_SERVER_UNIX_SOCKET"
        export PGRST_SERVER_UNIX_SOCKET="$tmpdir/postgrest.socket"

        ${slocat}/bin/slocat -delay "$delay" -src "$PGRST_SERVER_UNIX_SOCKET" -dst "$REAL_PGRST_SERVER_UNIX_SOCKET" &
        SLOCAT_PID=$!
        # shellcheck disable=SC2317
        stop_slocat() {
          kill "$SLOCAT_PID" || true
          wait "$SLOCAT_PID" || true
        }
        trap stop_slocat EXIT
        sleep 1 # should wait for socket file to appear instead

        ("$_arg_command" "''${_arg_leftovers[@]}")
      '';

  withGit =
    let
      name = "postgrest-with-git";
    in
    checkedShellScript
      {
        inherit name;
        docs =
          ''
            Create a new worktree of the postgrest repo in a temporary directory and
            check out <commit>, then run <command> with arguments inside the temporary folder.
          '';
        args =
          [
            "ARG_POSITIONAL_SINGLE([commit], [Commit-ish reference to run command with])"
            "ARG_POSITIONAL_SINGLE([command], [Command to run])"
            "ARG_LEFTOVERS([command arguments])"
          ];
        positionalCompletion =
          ''
            if test "$prev" == "${name}"; then
              __gitcomp_nl "$(__git_refs)"
            else
              _command_offset 2
            fi
          '';
        workingDir = "/";
      }
      ''
        # not using withTmpDir here, because we don't want to keep the directory on error
        tmpdir="$(mktemp -d)"
        trap 'rm -rf "$tmpdir"' EXIT

        ${git}/bin/git worktree add -f "$tmpdir" "$_arg_commit" > /dev/null

        cd "$tmpdir"
        ("$_arg_command" "''${_arg_leftovers[@]}")

        ${git}/bin/git worktree remove -f "$tmpdir" > /dev/null
      '';

  legacyConfig =
    writeText "legacy.conf"
      ''
        # Using this config file to support older postgrest versions for `postgrest-loadtest-against`
        db-uri="$(PGRST_DB_URI)"
        db-schema="$(PGRST_DB_SCHEMAS)"
        db-anon-role="$(PGRST_DB_ANON_ROLE)"
        db-pool="$(PGRST_DB_POOL)"
        server-unix-socket="$(PGRST_SERVER_UNIX_SOCKET)"
        log-level="$(PGRST_LOG_LEVEL)"
      '';

  waitForPgrstReady =
    checkedShellScript
      {
        name = "postgrest-wait-for-pgrst-ready";
        docs = "Wait for PostgREST to be ready to serve requests. Needs to be a separate command for timeout to work below.";
        args = [
          "ARG_USE_ENV([PGRST_SERVER_UNIX_SOCKET], [], [Unix socket to check for running PostgREST instance])"
        ];
      }
      ''
        # ARG_USE_ENV only adds defaults or docs for environment variables
        # We manually implement a required check here
        # See also: https://github.com/matejak/argbash/issues/80
        : "''${PGRST_SERVER_UNIX_SOCKET:?PGRST_SERVER_UNIX_SOCKET is required}"

        function check_status () {
          ${curl}/bin/curl -s -o /dev/null -w "%{http_code}" --unix-socket "$PGRST_SERVER_UNIX_SOCKET" http://localhost/
        }

        while [[ "$(check_status)" != "200" ]];
           do sleep 0.1;
        done
      '';

  # Broadcast SIGINT to any running postgrest instances on the host. Uses python for cross-platform compatibility.
  signalPostgrest =
    writers.writePython3 "postgrest-signal-int"
      { libraries = [ python3Packages.psutil ]; }
      ''
        import psutil
        import signal

        for proc in psutil.process_iter(["name"]):
            try:
                if proc.info["name"] == "postgrest":
                    proc.send_signal(signal.SIGINT)
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                continue
      '';

  withPgrst =
    let
      commandName = "postgrest-with-pgrst";
    in
    checkedShellScript
      {
        name = commandName;
        docs = "Build and run PostgREST and run <command> with PGRST_SERVER_UNIX_SOCKET set.";
        args =
          [
            "ARG_POSITIONAL_SINGLE([command], [Command to run])"
            "ARG_LEFTOVERS([command arguments])"
            "ARG_OPTIONAL_SINGLE([monitor], [m], [Enable CPU and memory monitoring of the PostgREST process and output to the designated file as markdown])"
            "ARG_OPTIONAL_SINGLE([timeout], [t], [Maximum time to wait for PostgREST to be ready], [5])"
            "ARG_OPTIONAL_SINGLE([sleep], [s],   [Sleep time after PostgREST is ready, this is useful for monitoring])"
            "ARG_USE_ENV([PGRST_CMD], [], [PostgREST executable to run])"
          ];
        positionalCompletion = "_command";
        workingDir = "/";
        withEnv = postgrest.env;
        withTmpDir = true;
      }
      ''
        export PGRST_SERVER_UNIX_SOCKET="$tmpdir"/postgrest.socket

        if [ -z "''${PGRST_CMD:-}" ]; then
          rm -f result
          build_start=$SECONDS
          if [ -z "''${PGRST_BUILD_CABAL:-}" ]; then
            echo -n "${commandName}: Building postgrest (nix)... "
            # Using lib.getBin to also make this work with older checkouts, where .bin was not a thing, yet.
            nix-build -E 'with import ./. {}; pkgs.lib.getBin postgrestPackage' > "$tmpdir"/build.log 2>&1 || {
              echo "failed, output:"
              cat "$tmpdir"/build.log
              exit 1
            }
            PGRST_CMD=$(echo ./result*/bin/postgrest)
          else
            echo -n "${commandName}: Building postgrest (cabal)... "
            postgrest-build
            PGRST_CMD=postgrest-run
          fi
          build_end=$((SECONDS - build_start))
          printf "done in %ss.\n" "$build_end"
        fi

        ver=$($PGRST_CMD ${legacyConfig} --version)

        echo -n "${commandName}: Starting $ver... "

        $PGRST_CMD ${legacyConfig} > "$tmpdir"/run.log 2>&1 &
        pid=$!
        # shellcheck disable=SC2317
        cleanup() {
          # Send INT to all postgrest processes.
          # Workaround to trigger dumping postgrest.prof for postgrest-profiled-run
          # Caveat: we cannot realistically limit this to the current process' tree,
          # since pkill's --parent supports only direct children; therefore this
          # would reap neighbor postgrest instances as well, because INT is asking
          # the process to terminate too.
          # TODO: consider cgroups to make this cleaner
          ${signalPostgrest}
          kill "$pid" || true
        }
        trap cleanup EXIT

        wait_start=$SECONDS
        timeout -s TERM "$_arg_timeout" ${waitForPgrstReady} || {
          echo "timed out, output:"
          cat "$tmpdir"/run.log
          exit 1
        }
        wait_duration=$((SECONDS - wait_start))
        printf "done in %ss.\n" "$wait_duration"

        echo "${commandName}: You can tail the server logs with: tail -f $tmpdir/run.log"

        if [[ -n "$_arg_monitor" ]]; then
          ${monitorPid} "$pid" > "$_arg_monitor" &
        fi

        if [[ -n "$_arg_sleep" ]]; then
          sleep "$_arg_sleep"
        fi

        ("$_arg_command" "''${_arg_leftovers[@]}")
      '';

  monitorPid =
    writers.writePython3 "postgrest-monitor-pid"
      {
        libraries = [ python3Packages.pandas python3Packages.tabulate python3Packages.psutil ];
      }
      (builtins.readFile ./monitor_pid.py);
in
buildToolbox
{
  name = "postgrest-with";
  tools = {
    inherit
      withGit
      withPgAll
      withPgrst
      withSlowPg
      withSlowPgrst;
  } // builtins.listToAttrs (
    # Create a `postgrest-with-pg-` for each PostgreSQL version
    builtins.map (pg: { inherit (pg) name; value = withTmpDb pg; }) postgresqlVersions
  );
  # make latest withPg available for other nix files
  extra = { inherit withPg; };
}
