{ mkDerivation, attoparsec, base, bytestring, conduit
, conduit-extra, containers, cryptonite, directory, filepath
, generic-lens, hspec, lens, lib, mtl, optparse-applicative
, resourcet, text, transformers, unliftio, unliftio-core
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
    cryptonite directory filepath generic-lens lens mtl text
    transformers unliftio unliftio-core
  ];
  executableHaskellDepends = [
    base conduit conduit-extra cryptonite directory filepath mtl
    optparse-applicative resourcet text unliftio unliftio-core
  ];
  testHaskellDepends = [ attoparsec base hspec ];
  license = lib.licenses.bsd3;
}
