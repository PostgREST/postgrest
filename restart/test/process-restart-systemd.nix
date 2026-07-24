_:

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
        ExecStart = "${pkgs.processRestartTestApp}/bin/${app}";
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
    machine.copy_from_host(os.path.abspath("restart/test"), "/tmp/process-restart-pytest")
    machine.succeed(
        "PORT=${toString port} SERVICE=${service} pytest -q /tmp/process-restart-pytest",
        timeout=30,
    )
  '';
}
