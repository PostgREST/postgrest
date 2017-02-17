{ mkDerivation, aeson, aeson-qq, base, base-compat, bytestring
, containers, doctest, generics-sop, Glob, hashable, hspec
, http-media, HUnit, insert-ordered-containers, lens, mtl, network
, QuickCheck, scientific, stdenv, template-haskell, text, time
, transformers, unordered-containers, uuid-types, vector
}:
mkDerivation {
  pname = "swagger2";
  version = "2.1.3";
  sha256 = "1vs4qsygw2mfmcdn8jjk5y0ms8nglqnhiwr1nvb4iz1qnl5g652d";
  libraryHaskellDepends = [
    aeson base base-compat bytestring containers generics-sop hashable
    http-media insert-ordered-containers lens mtl network scientific
    template-haskell text time transformers unordered-containers
    uuid-types vector
  ];
  testHaskellDepends = [
    aeson aeson-qq base base-compat bytestring containers doctest Glob
    hashable hspec HUnit insert-ordered-containers lens mtl QuickCheck
    text time unordered-containers vector
  ];
  doCheck = false;
  hyperlinkSource = false;
  homepage = "https://github.com/GetShopTV/swagger2";
  description = "Swagger 2.0 data model";
  license = stdenv.lib.licenses.bsd3;
}
