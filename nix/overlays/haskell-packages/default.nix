self: super:

{
  haskellPackages =
    super.haskellPackages.override {
      overrides =
        final: prev: rec {
          protolude = prev.callPackage ./protolude.nix {};
        };
    };
}
