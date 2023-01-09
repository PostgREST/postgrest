{ patches }: self: super:
# Overlay that sets the default version of PostgreSQL.
{
  postgresql = super.postgresql_15;
}
