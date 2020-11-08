{ buildEnv
, curl
, envsubst
, ghr
, jq
, postgrest
, docker
, runCommand
, writeShellScriptBin
}:
let
  # Version from the postgrest.cabal file (gotten with callCabal2nix).
  version = postgrest.version;

  # Script for publishing a new release on GitHub.
  github =
    writeShellScriptBin "postgrest-release-github"
      ''
        set -euo pipefail

        version=$1

        if test $version = "nightly"
        then
          date=$(date +'%F')
          sha=$(echo $CIRCLE_SHA1 | head -c7)
          suffix=$date-$sha
          tar cvJf postgrest-nightly-$suffix-linux-x64-static.tar.xz \
            -C ${postgrest}/bin postgrest

          ${ghr}/bin/ghr \
            -t "$GITHUB_TOKEN" \
            -u "$GITHUB_USERNAME" \
            -r "$GITHUB_REPONAME" \
            --replace nightly \
            postgrest-nightly-$suffix-linux-x64-static.tar.xz
        else
          changes="$(sed -n "1,/$version/d;/## \[/q;p" ${../../CHANGELOG.md})"

          tar cvJf postgrest-$version-linux-x64-static.tar.xz \
            -C ${postgrest}/bin postgrest

          ${ghr}/bin/ghr \
            -t "$GITHUB_TOKEN" \
            -u "$GITHUB_USERNAME" \
            -r "$GITHUB_REPONAME" \
            -b "$changes" \
            --replace $version \
            postgrest-$version-linux-x64-static.tar.xz
        fi
      '';

  # Wrapper for login with docker. $DOCKER_USER/$DOCKER_PASS vars come from CircleCI.
  # The DOCKER_USER is not the same as DOCKER_REPO because we use the https://hub.docker.com/u/postgrestbot account for uploading to dockerhub.
  dockerLogin =
    writeShellScriptBin "postgrest-docker-login"
      ''
        set -euo pipefail

        docker login -u $DOCKER_USER -p $DOCKER_PASS
      '';

  # Script for publishing a new release on Docker Hub.
  dockerHub =
    writeShellScriptBin "postgrest-release-dockerhub"
      ''
        set -euo pipefail

        version=$1

        docker load -i ${docker.image}

        if test $version = "nightly"
        then
          date=$(date +'%F')
          sha=$(echo $CIRCLE_SHA1 | head -c7)
          suffix=$date-$sha

          docker tag postgrest:latest "$DOCKER_REPO"/postgrest:nightly-$suffix
          docker push "$DOCKER_REPO"/postgrest:nightly-$suffix
        else
          docker tag postgrest:latest "$DOCKER_REPO"/postgrest:latest
          docker tag postgrest:latest "$DOCKER_REPO"/postgrest:$version

          docker push "$DOCKER_REPO"/postgrest:latest
          docker push "$DOCKER_REPO"/postgrest:$version
        fi
      '';

  # Script for updating the repository description on Docker Hub.
  dockerHubDescription =
    let
      description =
        ./docker-hub-description.md;

      fullDescription =
        ./docker-hub-full-description.md;
    in
    writeShellScriptBin "postgrest-release-dockerhubdescription"
      ''
        set -euo pipefail

        # Login to Docker Hub and get a token.
        token="$(
          ${curl}/bin/curl -s \
            --data-urlencode "username=$DOCKER_USER" \
            --data-urlencode "password=$DOCKER_PASS" \
            "https://hub.docker.com/v2/users/login/" \
            | ${jq}/bin/jq -r .token
        )"

        # Plug the default config file into the full description.
        export DEFAULT_CONFIG="$(cat ${docker.config})"
        fullDescription="$(${envsubst}/bin/envsubst < ${fullDescription})"

        # Patch the full description.
        responseCode="$(
          ${curl}/bin/curl -s --write-out %{response_code} --output /dev/null \
            -H "Authorization: JWT $token" -X PATCH \
            --data-urlencode description@${description} \
            --data-urlencode "full_description=$fullDescription" \
            "https://hub.docker.com/v2/repositories/$DOCKER_REPO/postgrest/"
        )"

        [ "$responseCode" -eq 200 ]
      '';
in
buildEnv {
  name = "postgrest-release";
  paths = [ github dockerLogin dockerHub dockerHubDescription ];
}
