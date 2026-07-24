{ pkgs, ... }:

let
  app = "process-restart-test-app";
  port = 3000;
  service = "${app}.service";
in
{
  name = "process-restart-systemd";

  nodes.machine = { pkgs, ... }: {
    environment.systemPackages = [
      (pkgs.python3.withPackages (ps: [
        ps.pytest
        ps.requests
      ]))
    ];

    systemd.services.${app} = {
      environment.PORT = toString port;

      serviceConfig = {
        ExecStart = "${pkgs.restartTestApp}/bin/${app}";
        ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
        NotifyAccess = "main";
        Restart = "no";
        Type = "notify";
      };
    };
  };

  testScript = ''
    import os

    machine.start()

    test_dir = os.environ["PROCESS_RESTART_TEST_DIR"]
    machine.copy_from_host(test_dir, "/tmp/process-restart-pytest")
    machine.succeed(
        "PORT=${toString port} SERVICE=${service} pytest -q /tmp/process-restart-pytest",
        timeout=30,
    )
  '';
}
