{ postgrest, dockerTools, writeShellScriptBin }:

rec {
  image =
    dockerTools.buildImage {
      name = "postgrest";
      tag = "latest";
      contents = postgrest;
      extraCommands = ''
        mkdir etc
        cp ${./postgrest.conf} etc/postgrest.conf
      '';
      config = {
        Cmd = [ "/bin/postgrest" "/etc/postgrest.conf" ];
        Env = [
          "PGRST_DB_URI=postgresql://?user=postgres"
          "PGRST_DB_SCHEMA=public"
          "PGRST_DB_ANON_ROLE="
          "PGRST_DB_POOL=100"
          "PGRST_DB_EXTRA_SEARCH_PATH=public"
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
          "3000/tcp" = {};
        };
      };
    };

  load =
    writeShellScriptBin "postgrest-docker-load"
      ''
        docker load -i ${image}
      '';
}
