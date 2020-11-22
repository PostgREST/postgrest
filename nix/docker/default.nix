{ buildEnv
, postgrest
, dockerTools
, checkedShellScript
}:
let
  config =
    ./postgrest.conf;

  image =
    dockerTools.buildImage {
      name = "postgrest";
      tag = "latest";
      contents = postgrest;

      # Set the current time as the image creation date. This makes the build
      # non-reproducible, but that should not be an issue for us.
      created = "now";

      extraCommands =
        ''
          mkdir etc
          cp ${config} etc/postgrest.conf
          rmdir share
        '';

      config = {
        Cmd = [ "/bin/postgrest" "/etc/postgrest.conf" ];
        Env = [
          "PGRST_DB_URI=postgresql://?user=postgres"
          "PGRST_DB_SCHEMA=public"
          "PGRST_DB_ANON_ROLE="
          "PGRST_DB_POOL=100"
          "PGRST_DB_POOL_TIMEOUT=10"
          "PGRST_DB_EXTRA_SEARCH_PATH=public"
          "PGRST_DB_CHANNEL=pgrst"
          "PGRST_DB_CHANNEL_ENABLED=false"
          "PGRST_SERVER_HOST=*4"
          "PGRST_SERVER_PORT=3000"
          "PGRST_OPENAPI_SERVER_PROXY_URI="
          "PGRST_JWT_SECRET="
          "PGRST_SECRET_IS_BASE64=false"
          "PGRST_JWT_AUD="
          "PGRST_MAX_ROWS="
          "PGRST_PRE_REQUEST="
          "PGRST_ROLE_CLAIM_KEY=.role"
          "PGRST_ROOT_SPEC="
          "PGRST_RAW_MEDIA_TYPES="
        ];
        User = "1000";
        ExposedPorts = {
          "3000/tcp" = { };
        };
      };
    };

  # Helper script for loading the image.
  load =
    checkedShellScript "postgrest-docker-load"
      ''
        docker load -i ${image}
      '';
in
buildEnv
  {
    name = "postgrest-docker";
    paths = [ load.bin ];
  } // { inherit image config; }
