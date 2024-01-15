{ buildToolbox
, checkedShellScript
, coreutils
, curl
, jq
, nix
}:
# Utility script for pinning the latest stable version of Nixpkgs.

# Instead of pinning Nixpkgs based on the huge Git repository, we reference a
# specific tarball that only contains the source of the revision that we want
# to pin.
let
  name =
    "postgrest-nixpkgs-upgrade";

  refUrl =
    "https://api.github.com/repos/nixos/nixpkgs/git/matching-refs/heads/nixpkgs-";

  githubV3Header =
    "Accept: application/vnd.github.v3+json";

  tarballUrlBase =
    "https://github.com/nixos/nixpkgs/archive/";

  upgrade =
    checkedShellScript
      {
        inherit name;
        docs = "Pin the newest stable version of Nixpkgs.";
        inRootDir = true;
      }
      ''
        # The list of refs is sorted. The first result will be nixpkgs-unstable, the second the latest stable branch.
        commitHash="$(${curl}/bin/curl "${refUrl}" -H "${githubV3Header}" | ${jq}/bin/jq -r 'sort_by(.ref) | reverse | .[1].object.sha')"
        tarballUrl="${tarballUrlBase}$commitHash.tar.gz"
        tarballHash="$(${nix}/bin/nix-prefetch-url --unpack "$tarballUrl")"
        currentDate="$(${coreutils}/bin/date --iso)"

        cat > nix/nixpkgs-version.nix << EOF
        # Pinned version of Nixpkgs, generated with ${name}.
        {
          date = "$currentDate";
          rev = "$commitHash";
          tarballHash = "$tarballHash";
        }
        EOF
      '';

in
buildToolbox
{
  name = "postgrest-nixpkgs";
  tools = [ upgrade ];
}
