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
    haskellPackages.hlint
    ghcid
    (haskellPackages.ghcWithHoogle (_: phrase.buildInputs ++ phrase.propagatedBuildInputs))
  ];
}
