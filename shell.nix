{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let
  phrase = haskellPackages.callPackage ./. {};

in

mkShell {
  buildInputs = [
    entr
    haskellPackages.cabal-install
    haskellPackages.hlint
    ghcid
    (haskellPackages.ghcWithHoogle (_: phrase.buildInputs ++ phrase.propagatedBuildInputs))
  ];
}
