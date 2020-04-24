{ mkDerivation, array, async, base, bytestring, containers, deepseq
, ghc-prim, hashable, mtl, mtl-compat, stdenv, stm, text
, transformers, transformers-compat
}:
mkDerivation {
  pname = "protolude";
  version = "0.3.0";
  sha256 = "4083385a9e03fab9201f63ce198b9ced3fbc1c50d6d42574db5e36c757bedcac";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array async base bytestring containers deepseq ghc-prim hashable
    mtl mtl-compat stm text transformers transformers-compat
  ];
  homepage = "https://github.com/sdiehl/protolude";
  description = "A small prelude";
  license = stdenv.lib.licenses.mit;
}
