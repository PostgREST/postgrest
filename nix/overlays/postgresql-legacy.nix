self: super:
# Overlay that adds legacy versions of PostgreSQL that are supported by
# PostgREST.
{
  # PostgreSQL 9.5 was removed from Nixpkgs with
  # https://github.com/NixOS/nixpkgs/commit/72ab382fb6b729b0d654f2c03f5eb25b39f11fbb
  # We pin its parent commit to get the last version that was available.
  postgresql_9_5 =
    let
      rev = "55ac7d4580c9ab67848c98cb9519317a1cc399c8";
      tarballHash = "02ffj9f8s1hwhmxj85nx04sv64qb6jm7w0122a1dz9n32fymgklj";

      pinnedPkgs =
        builtins.fetchTarball {
          url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
          sha256 = tarballHash;
        };
    in
    (import pinnedPkgs { }).pkgs.postgresql_9_5;
}
