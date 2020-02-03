{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let
  phrase = haskellPackages.callPackage ./. {};

in

stdenv.mkDerivation {
  name = "shell";
  buildInputs = [
    entr
    haskellPackages.cabal-install
    ghcid
    (haskellPackages.ghcWithHoogle (_: phrase.buildInputs ++ phrase.propagatedBuildInputs))
  ];
}
