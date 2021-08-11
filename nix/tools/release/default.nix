{ buildToolbox
, checkedShellScript
, curl
, ghr
, jq
}:
let
  dockerHubDescription =
    let
      description =
        ./docker-hub-description.md;

      fullDescription =
        ./docker-hub-full-description.md;
    in
    checkedShellScript
      {
        name = "postgrest-release-dockerhub-description";
        docs = "Update the repository description on Docker Hub.";
        args = [
          "ARG_USE_ENV([DOCKER_USER], [], [DockerHub user name])"
          "ARG_USE_ENV([DOCKER_PASS], [], [DockerHub password])"
          "ARG_USE_ENV([DOCKER_REPO], [], [DockerHub repository])"
        ];
      }
      ''
        # ARG_USE_ENV only adds defaults or docs for environment variables
        # We manually implement a required check here
        # See also: https://github.com/matejak/argbash/issues/80
        DOCKER_USER="''${DOCKER_USER:?DOCKER_USER is required}"
        DOCKER_PASS="''${DOCKER_PASS:?DOCKER_PASS is required}"
        DOCKER_REPO="''${DOCKER_REPO:?DOCKER_REPO is required}"

        echo "Logging in to Docker Hub to get an auth token..."
        token="$(
          ${curl}/bin/curl --fail -s \
            --data-urlencode "username=$DOCKER_USER" \
            --data-urlencode "password=$DOCKER_PASS" \
            "https://hub.docker.com/v2/users/login/" \
           | ${jq}/bin/jq -r .token
        )"

        repo_url="https://hub.docker.com/v2/repositories/$DOCKER_REPO/postgrest/"
        echo "Patching both descriptions at $repo_url ..." 
        ${curl}/bin/curl --fail -X PATCH "$repo_url" \
          -H "Authorization: JWT $token" \
          --data-urlencode description@${description} \
          --data-urlencode full_description@${fullDescription}
      '';
in
buildToolbox
{
  name = "postgrest-release";
  tools = [ dockerHubDescription ];
}
