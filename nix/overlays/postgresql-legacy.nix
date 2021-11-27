_: _:
# Overlay that adds legacy versions of PostgreSQL that are supported by
# PostgREST.
{
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
