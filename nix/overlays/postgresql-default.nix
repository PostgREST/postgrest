{ patches }: self: super:
# Overlay that sets the default version of PostgreSQL.
with patches;
{
  postgresql = super.postgresql_15.overrideAttrs ({ patches ? [ ], ... }: {
    patches = patches ++ [ postgresql-atexit ];
  });
}
