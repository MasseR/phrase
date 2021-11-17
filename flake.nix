{
  description = "phrase";

  inputs = {
    easy-hls = { url = "github:jkachmar/easy-hls-nix"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
  };

  outputs = { self, nixpkgs, flake-utils, easy-hls }:
    { overlay = final: prev: {
        haskellPackages = prev.haskellPackages.override ( old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (f: p: {
            phrase = f.callPackage ./. {};
          });
        });
      };
    }
    //
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] ( system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        hp = pkgs.haskellPackages;
        hls = (easy-hls.withGhcs [ hp.ghc ]).${system};
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
            hls
          ];
        };
      }
    );
}
