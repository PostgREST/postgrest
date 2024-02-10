self: super:
# Overlay that adds legacy versions of PostgreSQL that are supported by
# PostgREST.
{
  # PostgreSQL 11 was removed from Nixpkgs with
  # https://github.com/NixOS/nixpkgs/commit/1220a4d4dd1a4590780a5e1c18d1333a121be366.
  # However, postgis was updated to 3.4.0 five months before at
  # https://github.com/NixOS/nixpkgs/commit/dfde9c83bce9e6c2bc903dfc1bca3bf93b3f52de.
  # Since postgis 3.4.0 doesn't support v11 anymore, we pin the last commit with
  # postgis 3.3.3.
  postgresql_11 =
    let
      rev = "0b458fbc462ced7fc03c8b68cf1b1bd0d92af465";
      tarballHash = "1vapq800crshsrvypckldrzy3ss0gzbb9mxlh3yajmdg702hcvrf";
      pinnedPkgs =
        builtins.fetchTarball {
          url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
          sha256 = tarballHash;
        };
    in
    (import pinnedPkgs { }).pkgs.postgresql_11;
}
