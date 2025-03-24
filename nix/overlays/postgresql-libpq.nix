_: super:
{
  # Depending on which nixpkgs version is pinned, libpq might either be available already - or not.
  libpq = super.libpq or (super.callPackage ../libpq.nix {
    postgresql = super.postgresql_16;
  });
}
