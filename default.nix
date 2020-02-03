{ mkDerivation, base, conduit, conduit-extra, containers
, cryptonite, hspec, mtl, optparse-applicative, stdenv, text
, transformers
}:
mkDerivation {
  pname = "phrase";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base conduit conduit-extra containers cryptonite mtl text
    transformers
  ];
  executableHaskellDepends = [ base mtl optparse-applicative text ];
  testHaskellDepends = [ base hspec ];
  license = stdenv.lib.licenses.bsd3;
}
