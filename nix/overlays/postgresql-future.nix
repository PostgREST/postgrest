self: super:
# Overlay that adds future versions of PostgreSQL that are supported by
# PostgREST.
{
  ## Example for including a postgresql version from a specific nixpks commit:
  ##
  #  postgresql_14 =
  #    let
  #      rev = "76b1e16c6659ccef7187ca69b287525fea133244";
  #      tarballHash = "1vsahpcx80k2bgslspb0sa6j4bmhdx77sw6la455drqcrqhdqj6a";
  #
  #      pinnedPkgs =
  #        builtins.fetchTarball {
  #          url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
  #          sha256 = tarballHash;
  #        };
  #    in
  #    (import pinnedPkgs { }).pkgs.postgresql_14;
}
