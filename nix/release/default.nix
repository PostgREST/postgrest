{ buildEnv
, checkedShellScript
, curl
, docker
, envsubst
, ghr
, git
, jq
, postgrest
, runCommand
}:
let
  github =
    checkedShellScript
      {
        name = "postgrest-release-github";
        docs = "Push a new release to GitHub.";
        args = [
          "ARG_POSITIONAL_SINGLE([version], [git version tag to make release for])"
          "ARG_USE_ENV([GITHUB_TOKEN], [], [GitHub token])"
          "ARG_USE_ENV([GITHUB_USERNAME], [], [GitHub user name])"
          "ARG_USE_ENV([GITHUB_REPONAME], [], [GitHub repository name])"
        ];
        inRootDir = true;
      }
      ''
        # ARG_USE_ENV only adds defaults or docs for environment variables
        # We manually implement a required check here
        # See also: https://github.com/matejak/argbash/issues/80
        GITHUB_TOKEN="''${GITHUB_TOKEN:?GITHUB_TOKEN is required}"
        GITHUB_USERNAME="''${GITHUB_USERNAME:?GITHUB_USERNAME is required}"
        GITHUB_REPONAME="''${GITHUB_REPONAME:?GITHUB_REPONAME is required}"

        if test "$_arg_version" = "nightly"
        then
          suffix=$(${git}/bin/git show -s --format="%cd-%h" --date="format:%Y-%m-%d-%H-%M")
          tar cvJf "postgrest-nightly-$suffix-linux-x64-static.tar.xz" \
            -C ${postgrest}/bin postgrest

          ${ghr}/bin/ghr \
            -t "$GITHUB_TOKEN" \
            -u "$GITHUB_USERNAME" \
            -r "$GITHUB_REPONAME" \
            --replace nightly \
            "postgrest-nightly-$suffix-linux-x64-static.tar.xz"
        else
          changes="$(sed -n "1,/$_arg_version/d;/## \[/q;p" ${../../CHANGELOG.md})"

          tar cvJf "postgrest-$_arg_version-linux-x64-static.tar.xz" \
            -C ${postgrest}/bin postgrest

          ${ghr}/bin/ghr \
            -t "$GITHUB_TOKEN" \
            -u "$GITHUB_USERNAME" \
            -r "$GITHUB_REPONAME" \
            -b "$changes" \
            --replace "$_arg_version" \
            "postgrest-$_arg_version-linux-x64-static.tar.xz"
        fi
      '';

  dockerLogin =
    checkedShellScript
      {
        name = "postgrest-docker-login";
        docs =
          ''
            Log in to Docker Hub using the DOCKER_USER and DOCKER_PASS env vars.

            Those env vars are usually provided by CircleCI. The DOCKER_USER is
            not the same as DOCKER_REPO because we use the
            https://hub.docker.com/u/postgrestbot account for uploading to dockerhub.
          '';
        args = [
          "ARG_USE_ENV([DOCKER_USER], [], [DockerHub user name])"
          "ARG_USE_ENV([DOCKER_PASS], [], [DockerHub password])"
        ];
      }
      ''
        # ARG_USE_ENV only adds defaults or docs for environment variables
        # We manually implement a required check here
        # See also: https://github.com/matejak/argbash/issues/80
        DOCKER_USER="''${DOCKER_USER:?DOCKER_USER is required}"
        DOCKER_PASS="''${DOCKER_PASS:?DOCKER_PASS is required}"

        docker login -u "$DOCKER_USER" -p "$DOCKER_PASS"
      '';

  dockerHub =
    checkedShellScript
      {
        name = "postgrest-release-dockerhub";
        docs = "Push a new release to Docker Hub";
        args = [
          "ARG_POSITIONAL_SINGLE([version], [git version tag to tag image with])"
          "ARG_USE_ENV([DOCKER_REPO], [], [DockerHub repository])"
        ];
      }
      ''
        # ARG_USE_ENV only adds defaults or docs for environment variables
        # We manually implement a required check here
        # See also: https://github.com/matejak/argbash/issues/80
        DOCKER_REPO="''${DOCKER_REPO:?DOCKER_REPO is required}"

        docker load -i ${docker.image}

        if test "$_arg_version" = "nightly"
        then
          suffix=$(${git}/bin/git show -s --format="%cd-%h" --date="format:%Y-%m-%d-%H-%M")

          docker tag postgrest:latest "$DOCKER_REPO/postgrest:nightly-$suffix"
          docker push "$DOCKER_REPO/postgrest:nightly-$suffix"
        else
          docker tag postgrest:latest "$DOCKER_REPO"/postgrest:latest
          docker tag postgrest:latest "$DOCKER_REPO/postgrest:$_arg_version"

          docker push "$DOCKER_REPO"/postgrest:latest
          docker push "$DOCKER_REPO/postgrest:$_arg_version"
        fi
      '';

  dockerHubDescription =
    let
      description =
        ./docker-hub-description.md;

      fullDescription =
        ./docker-hub-full-description.md;
    in
    checkedShellScript
      {
        name = "postgrest-release-dockerhubdescription";
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

        # Login to Docker Hub and get a token.
        token="$(
          ${curl}/bin/curl -s \
            --data-urlencode "username=$DOCKER_USER" \
            --data-urlencode "password=$DOCKER_PASS" \
            "https://hub.docker.com/v2/users/login/" \
            | ${jq}/bin/jq -r .token
        )"

        # Patch both descriptions.
        responseCode="$(
          ${curl}/bin/curl -s --write-out "%{response_code}" \
            --output /dev/null -H "Authorization: JWT $token" -X PATCH \
            --data-urlencode description@${description} \
            --data-urlencode full_description@${fullDescription} \
            "https://hub.docker.com/v2/repositories/$DOCKER_REPO/postgrest/"
        )"

        [ "$responseCode" -eq 200 ]
      '';

  tools = [ github dockerLogin dockerHub dockerHubDescription ];

  bashCompletion = builtins.map (tool: tool.bashCompletion) tools;

in
buildEnv
  {
    name = "postgrest-release";
    paths = builtins.map (tool: tool.bin) tools;
  } // { inherit bashCompletion; }
