final: prev:
let
  postgis_3_2_3 = rec {
    version = "3.2.3";
    src = final.fetchurl {
      url = "https://download.osgeo.org/postgis/source/postgis-${version}.tar.gz";
      sha256 = "sha256-G02LXHVuWrpZ77wYM7Iu/k1lYneO7KVvpJf+susTZow=";
    };
    meta.broken = false;
  };
in
{
  postgresql_11 = prev.postgresql_11.override { this = final.postgresql_11; } // {
    pkgs = prev.postgresql_11.pkgs // {
      postgis = prev.postgresql_11.pkgs.postgis.overrideAttrs (_: postgis_3_2_3);
    };
  };
}
