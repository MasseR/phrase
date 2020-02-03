{ mkDerivation, attoparsec, base, conduit, conduit-extra
, containers, cryptonite, hspec, lens, mtl, optparse-applicative
, stdenv, text, transformers
}:
mkDerivation {
  pname = "phrase";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base conduit conduit-extra containers cryptonite lens
    mtl text transformers
  ];
  executableHaskellDepends = [ base mtl optparse-applicative text ];
  testHaskellDepends = [ attoparsec base hspec ];
  license = stdenv.lib.licenses.bsd3;
}
