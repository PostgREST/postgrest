_: super:
# Overlay that adds `buildToolbox`, an enhanced version of `buildEnv`
{
  buildToolbox = super.callPackage ./build-toolbox.nix { };
}
