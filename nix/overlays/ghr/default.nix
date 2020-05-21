
self: super:
# Overlay that adds `ghr`: Upload multiple artifacts to GitHub Release in
# parallel, http://tcnksm.github.io/ghr/

{
  ghr = super.callPackage ./ghr.nix {};
}
