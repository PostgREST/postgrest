self: super:
# Overlay that adds legacy versions of PostgreSQL that are supported by
# PostgREST.
{
  # PostgreSQL 9.4 was removed from Nixpkgs with
  # https://github.com/NixOS/nixpkgs/commit/8e2fc57a80d761c46702c3250e61c1bffe021e25
  # We pin its parent commit 3b5b9a7 to get the last version that was available.
  postgresql_9_4 =
    let
      rev = "3b5b9a73f59ff93d50156e250203410bdd07f4e0";
      tarballHash = "0kr25xqbv1ldp72jbmml21pc5hl7xcfqhclv5qxa5f860jddjznk";

      pinnedPkgs =
        builtins.fetchTarball {
          url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
          sha256 = tarballHash;
        };
    in
      (import pinnedPkgs {}).pkgs.postgresql_9_4;
}
