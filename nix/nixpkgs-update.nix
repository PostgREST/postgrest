# Pin the current unstable version of Nixpkgs, based on the more efficient
# tarball instead of the huge Git repository
{ curl
, jq
, writeShellScriptBin
, nix
}:
let
  name =
    "nixpkgs-update";

  refUrl =
    https://api.github.com/repos/nixos/nixpkgs/git/ref/heads/nixpkgs-unstable;

  githubV3Header =
    "Accept: application/vnd.github.v3+json";

  tarballUrlBase =
    https://github.com/nixos/nixpkgs/archive/;
in
writeShellScriptBin name
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
  ''
