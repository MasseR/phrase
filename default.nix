{ mkDerivation, base, mtl, optparse-applicative, stdenv, text }:
mkDerivation {
  pname = "phrase";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base mtl optparse-applicative text ];
  license = stdenv.lib.licenses.bsd3;
}
