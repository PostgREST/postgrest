{ mkDerivation, async, base, base-prelude, bytestring
, bytestring-tree-builder, contravariant, contravariant-extras
, hasql, mtl, rebase, stdenv, transformers
}:
mkDerivation {
  pname = "hasql-transaction";
  version = "0.5";
  sha256 = "0hdkw0rrma0cys1gd4phw9ajrimgbaabmsyp533fm6x5fznk7m0w";
  libraryHaskellDepends = [
    base base-prelude bytestring bytestring-tree-builder contravariant
    contravariant-extras hasql mtl transformers
  ];
  testHaskellDepends = [ async hasql rebase ];
  doCheck = false;
  homepage = "https://github.com/nikita-volkov/hasql-transaction";
  description = "A composable abstraction over the retryable transactions for Hasql";
  license = stdenv.lib.licenses.mit;
}
