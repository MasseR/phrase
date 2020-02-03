{ mkDerivation, attoparsec, base, bytestring, conduit
, conduit-extra, containers, cryptonite, hspec, lens, mtl
, optparse-applicative, resourcet, stdenv, text, transformers
}:
mkDerivation {
  pname = "phrase";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    attoparsec base bytestring conduit conduit-extra containers
    cryptonite lens mtl text transformers
  ];
  executableHaskellDepends = [
    base conduit conduit-extra mtl optparse-applicative resourcet text
  ];
  testHaskellDepends = [ attoparsec base hspec ];
  license = stdenv.lib.licenses.bsd3;
}
