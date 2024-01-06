self: super:
# Overlay that adds legacy versions of PostgreSQL that are supported by
# PostgREST.
{
  # PostgreSQL 9.6 was removed from Nixpkgs with
  # https://github.com/NixOS/nixpkgs/commit/757dd008b2f2926fc0f7688fa8189f930ea47521
  # We pin its parent commit to get the last version that was available.
  postgresql_9_6 =
    let
      rev = "571cbf3d1db477058303cef8754fb85a14e90eb7";
      tarballHash = "0q74wn418i1bn5sssacmw8ykpmqvzr0s93sj6pbs3rf6bf134fkz";
      pinnedPkgs =
        builtins.fetchTarball {
          url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
          sha256 = tarballHash;
        };
    in
    (import pinnedPkgs { }).pkgs.postgresql_9_6;

  # PostgreSQL 10 was removed from Nixpkgs with
  # https://github.com/NixOS/nixpkgs/commit/aa1483114bb329fee7e1266100b8d8921ed4723f
  # We pin its parent commit to get the last version that was available.
  postgresql_10 =
    let
      rev = "79661ba7e2fb96ebefbb537458a5bbae9dc5bd1a";
      tarballHash = "0rn796pfn4sg90ai9fdnwmr10a2s835p1arazzgz46h6s5cxvq97";
      pinnedPkgs =
        builtins.fetchTarball {
          url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
          sha256 = tarballHash;
        };
    in
    (import pinnedPkgs { }).pkgs.postgresql_10;

  # PostgreSQL 11 was removed from Nixpkgs with
  # https://github.com/NixOS/nixpkgs/commit/1220a4d4dd1a4590780a5e1c18d1333a121be366
  # We pin its parent commit to get the last version that was available.
  postgresql_11 =
    let
      rev = "f5458516e42cc5cb4123cc2d93f45c240548aa18";
      tarballHash = "1h03621sxfhw4z6ya74k6c2lyx3z7pvf2jcg4vs7i01yz2m6w3cv";
      pinnedPkgs =
        builtins.fetchTarball {
          url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
          sha256 = tarballHash;
        };
    in
    (import pinnedPkgs { }).pkgs.postgresql_11;
}
