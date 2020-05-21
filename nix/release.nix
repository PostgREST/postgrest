{ buildEnv
, ghr
, writeShellScriptBin
, postgrest
, postgrestDocker
, runCommand
}:
let
  version = postgrest.version;

  releaseFiles =
    runCommand "postgrest-release-files"
      { inherit postgrest version; }
      ''
        set -euo pipefail

        mkdir -p $out

        tar cvJf "$out"/postgrest-v"$version"-linux-x64-static.tar.xz \
          -C "$postgrest"/bin postgrest
      '';

  github =
    writeShellScriptBin "postgrest-release-github"
      ''
        set -euo pipefail

        changes=$(sed -n "1,/${version}/d;/## \[/q;p" ${../CHANGELOG.md})

        ${ghr}/bin/ghr \
          -t "$GITHUB_TOKEN" \
          -u "$GITHUB_USERNAME" \
          -r "$GITHUB_REPONAME" \
          -b "$changes" \
          --replace v${version} \
          ${releaseFiles}
      '';

  docker =
    writeShellScriptBin "postgrest-release-docker"
      ''
        set -euo pipefail

        ${postgrestDocker}/bin/postgrest-docker-load

        docker tag postgrest:latest "$DOCKER_REPO"/postgrest:latest
        docker tag postgrest:latest "$DOCKER_REPO"/postgrest:v${version}

        docker push "$DOCKER_REPO"/postgrest:latest
        docker push "$DOCKER_REPO"/postgrest:v${version}
      '';
in
buildEnv {
  name = "postgrest-release";
  paths = [ github docker ];
}
