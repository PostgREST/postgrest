self: super:
{
  libpq = super.callPackage ../libpq.nix {
    postgresql = super.postgresql_15;
  };
}
