{
  description = "REST API for any Postgres database";

  inputs = {
    nixpkgs-lib.url = "github:nix-community/nixpkgs.lib";
  };

  nixConfig = {
    extra-substituters = "https://postgrest.cachix.org";
    extra-trusted-public-keys = "postgrest.cachix.org-1:icgW4R15fz1+LqvhPjt4EnX/r19AaqxiVV+1olwlZtI=";
  };

  outputs = { nixpkgs-lib, ... }:
    let
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      pgrstFor = system: import ./default.nix {
        inherit system;
      };

      genSystems = f: nixpkgs-lib.lib.genAttrs systems (system: f (pgrstFor system));
    in
    {
      packages = genSystems (attrs: {
        default = attrs.postgrestPackage;
        profiled = attrs.postgrestProfiled;
      } // nixpkgs-lib.lib.optionalAttrs (attrs ? postgrestStatic) {
        static = attrs.postgrestStatic;
      });

      apps = genSystems (attrs: {
        default = {
          type = "app";
          program = "${attrs.postgrestStatic or attrs.postgrestPackage}/bin/postgrest";
          meta.description = "REST API for any Postgres database";
        };
      });
    };
}
