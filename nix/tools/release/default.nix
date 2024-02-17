{ buildToolbox
, checkedShellScript
, curl
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
        : "''${DOCKER_USER:?DOCKER_USER is required}"
        : "''${DOCKER_PASS:?DOCKER_PASS is required}"
        : "''${DOCKER_REPO:?DOCKER_REPO is required}"

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

  release =
    checkedShellScript
      {
        name = "postgrest-release";
        docs = "Patch postgrest.cabal, CHANGELOG.md, tag and push all in one go.";
        args = [ "ARG_POSITIONAL_SINGLE([version], [Version to release], [pre])" ];
        workingDir = "/";
      }
      ''
        trap "echo You need to be on the main branch or a release branch to proceed. Exiting ..." ERR
        [[ "$(git rev-parse --abbrev-ref HEAD)" =~ ^main$|^v[0-9]+$ ]]
        trap "" ERR

        trap "echo You have uncommitted changes in postgrest.cabal. Exiting ..." ERR
        git diff --exit-code HEAD postgrest.cabal > /dev/null
        trap "" ERR

        current_version="$(grep -oP '^version:\s*\K.*' postgrest.cabal)"
        # shellcheck disable=SC2034
        IFS=. read -r major minor patch pre <<< "$current_version"
        echo "Current version is $current_version"

        today_date="$(date '+%Y%m%d')"
        today_date_for_changelog="$(date '+%Y-%m-%d')"
        bump_pre="$major.$minor.$patch.$today_date"
        bump_pre_minor="$major.$((minor+1)).0.$today_date"
        bump_patch="$major.$minor.$((patch+1))"
        bump_minor="$major.$((minor+1)).0"
        bump_major="$((major+1)).0.0"

        PS3="Please select the new version: "
        select new_version in "$bump_pre" "$bump_pre_minor" "$bump_patch" "$bump_minor" "$bump_major"; do
          case "$REPLY" in
            1|2|3|4|5)
              echo "Selected $new_version"
              break
              ;;
            *)
            echo "Invalid option $REPLY"
            ;;
          esac
        done

        echo "Updating postgrest.cabal ..."
        sed -i -E "s/^(version:\s+).*$/\1$new_version/" postgrest.cabal > /dev/null

        echo "Committing ..."
        git add postgrest.cabal > /dev/null

        if [[ "$new_version" != "$bump_pre" && "$new_version" != "$bump_pre_minor" ]]; then
          echo "Updating CHANGELOG.md ..."
          sed -i -E "s/Unreleased/&\n\n## [$new_version] - $today_date_for_changelog/" CHANGELOG.md > /dev/null
          git add CHANGELOG.md > /dev/null
        fi

        git commit -m "bump version to $new_version" > /dev/null

        echo "Tagging ..."
        git tag "v$new_version" > /dev/null

        trap "echo Remote not found. Please push manually ..." ERR
        remote="$(git remote -v | grep PostgREST/postgrest | grep push | cut -f1)"
        trap "" ERR

        push="git push --atomic $remote $(git rev-parse --abbrev-ref HEAD) v$new_version"

        echo "To push both the branch and the new tag, the following will be run:"
        echo
        echo "$push"
        echo

        read -r -p 'Proceed? (y/N) ' REPLY
        case "$REPLY" in
          y|Y)
            $push
            ;;
          *)
            echo "Aborting ..."
            ;;
        esac
      '';

in
buildToolbox
{
  name = "postgrest-release";
  tools = [ dockerHubDescription release ];
}
