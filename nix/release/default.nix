{ buildEnv
, curl
, envsubst
, ghr
, jq
, postgrest
, docker
, runCommand
, checkedShellScript
}:
let
  github =
    checkedShellScript
      {
        name = "postgrest-release-github";
        docs = "Push a new release to GitHub.";
        inRootDir = true;
      }
      ''
        version=$1

        if test "$version" = "nightly"
        then
          suffix=$(git show -s --format="%cd-%h" --date="format:%Y-%m-%d-%H-%M")
          tar cvJf "postgrest-nightly-$suffix-linux-x64-static.tar.xz" \
            -C ${postgrest}/bin postgrest

          ${ghr}/bin/ghr \
            -t "$GITHUB_TOKEN" \
            -u "$GITHUB_USERNAME" \
            -r "$GITHUB_REPONAME" \
            --replace nightly \
            "postgrest-nightly-$suffix-linux-x64-static.tar.xz"
        else
          changes="$(sed -n "1,/$version/d;/## \[/q;p" ${../../CHANGELOG.md})"

          tar cvJf "postgrest-$version-linux-x64-static.tar.xz" \
            -C ${postgrest}/bin postgrest

          ${ghr}/bin/ghr \
            -t "$GITHUB_TOKEN" \
            -u "$GITHUB_USERNAME" \
            -r "$GITHUB_REPONAME" \
            -b "$changes" \
            --replace "$version" \
            "postgrest-$version-linux-x64-static.tar.xz"
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
      }
      ''
        docker login -u "$DOCKER_USER" -p "$DOCKER_PASS"
      '';

  dockerHub =
    checkedShellScript
      {
        name = "postgrest-release-dockerhub";
        docs = "Push a new release to Docker Hub";
      }
      ''
        version=$1

        docker load -i ${docker.image}

        if test "$version" = "nightly"
        then
          suffix=$(git show -s --format="%cd-%h" --date="format:%Y-%m-%d-%H-%M")

          docker tag postgrest:latest "$DOCKER_REPO/postgrest:nightly-$suffix"
          docker push "$DOCKER_REPO/postgrest:nightly-$suffix"
        else
          docker tag postgrest:latest "$DOCKER_REPO"/postgrest:latest
          docker tag postgrest:latest "$DOCKER_REPO/postgrest:$version"

          docker push "$DOCKER_REPO"/postgrest:latest
          docker push "$DOCKER_REPO/postgrest:$version"
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
      }
      ''
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
