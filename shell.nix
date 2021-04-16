{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let
  hp = haskellPackages.extend (self: super: {
    phrase = self.callPackage ./. {};
  });
  easy-hls-src = fetchFromGitHub {
    owner = "jkachmar";
    repo = "easy-hls-nix";
    inherit (builtins.fromJSON (builtins.readFile ./easy-hls-nix.json)) rev sha256;
  };
  easy-hls = callPackage easy-hls-src { ghcVersions = [ hp.ghc.version ]; };

in

hp.shellFor {
  packages = h: [h.phrase];
  buildInputs = [
    entr
    haskellPackages.cabal-install
    haskellPackages.hlint
    stylish-haskell
    ghcid
    easy-hls
  ];
}
