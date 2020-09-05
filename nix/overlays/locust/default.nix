self: super:

let
  flask-basicauth = super.python38Packages.callPackage ./flask-basicauth.nix { };
in
{
  locust = super.python38Packages.callPackage ./locust.nix { inherit flask-basicauth; };
}
