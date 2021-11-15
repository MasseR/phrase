{
  description = "phrase";

  inputs = {
    easy-hls-src = { url = "github:jkachmar/easy-hls-nix"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
  };

  outputs = { self, nixpkgs, flake-utils, easy-hls-src }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] ( system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hp = pkgs.haskellPackages.extend (self: super: {
            phrase = self.callPackage ./. {};
          });
        easy-hls = pkgs.callPackage easy-hls-src { ghcVersions = [ hp.ghc.version ]; };
      in
      rec {

        packages = { inherit (hp) phrase; };

        defaultPackage = packages.phrase;
        apps.phrase = {
          type = "app";
          program = "${hp.phrase}/bin/phrase";
        };
        devShell = hp.shellFor {
          packages = h: [h.phrase];
          withHoogle = true;
          buildInputs = with pkgs; [
            entr
            cabal-install
            hp.hlint
            stylish-haskell
            ghcid
            easy-hls
          ];
        };
      }
    );
}
