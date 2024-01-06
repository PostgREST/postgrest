self: super:
# Overlay that adds future versions of PostgreSQL that are supported by
# PostgREST.
{
  ## Example for including a postgresql version from a specific nixpks commit:
  ##
  #  postgresql_16 =
  #    let
  #      rev = "5148520bfab61f99fd25fb9ff7bfbb50dad3c9db";
  #      tarballHash = "1dfjmz65h8z4lk845724vypzmf3dbgsdndjpj8ydlhx6c7rpcq3p";
  #
  #      pinnedPkgs =
  #        builtins.fetchTarball {
  #          url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
  #          sha256 = tarballHash;
  #        };
  #    in
  #    (import pinnedPkgs { }).pkgs.postgresql_16;
}
