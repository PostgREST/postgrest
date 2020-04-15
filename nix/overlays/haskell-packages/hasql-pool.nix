{ mkDerivation
, base-prelude
, hasql
, hspec
, resource-pool
, stdenv
, time
}:
mkDerivation {
  pname = "hasql-pool";
  version = "0.5.1";
  sha256 = "739860f61589261b120368c113fbe88360e5db8eafc2166fbaba2a70692cf429";
  libraryHaskellDepends = [ base-prelude hasql resource-pool time ];
  testHaskellDepends = [ base-prelude hasql hspec ];
  homepage = "https://github.com/nikita-volkov/hasql-pool";
  description = "A pool of connections for Hasql";
  license = stdenv.lib.licenses.mit;
  doCheck = false;
}
