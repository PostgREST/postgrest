{ checkedShellScript
, curl
, jq
, nix
}:
# Utility script for pinning the latest unstable version of Nixpkgs.

# Instead of pinning Nixpkgs based on the huge Git repository, we reference a
# specific tarball that only contains the source of the revision that we want
# to pin.
let
  name =
    "nixpkgs-upgrade";

  refUrl =
    https://api.github.com/repos/nixos/nixpkgs/git/ref/heads/nixpkgs-unstable;

  githubV3Header =
    "Accept: application/vnd.github.v3+json";

  tarballUrlBase =
    https://github.com/nixos/nixpkgs/archive/;

  script =
    checkedShellScript
      name
      ''
        commitHash="$(${curl}/bin/curl "${refUrl}" -H "${githubV3Header}" | ${jq}/bin/jq -r .object.sha)"
        tarballUrl="${tarballUrlBase}$commitHash.tar.gz"
        tarballHash="$(${nix}/bin/nix-prefetch-url --unpack "$tarballUrl")"
        currentDate="$(date --iso)"

        cat << EOF
        # Pinned version of Nixpkgs, generated with ${name}.
        {
          date = "$currentDate";
          rev = "$commitHash";
          tarballHash = "$tarballHash";
        }
        EOF
      '';
in
script.bin
