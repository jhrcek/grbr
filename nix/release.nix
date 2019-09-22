let
  client = import ./client.nix {};

  config = {
    packageOverrides = pkgs: {
      haskellPackages = with pkgs;
        haskellPackages.override {
          overrides = haskellPkgsNew: haskellPkgsOld: {
            grbr = pkgs.haskell.lib.justStaticExecutables
              (haskell.lib.overrideCabal
                (haskellPkgsNew.callPackage ./server.nix {})
                ( oldDerivation: {
                    preConfigure = ''
                      mkdir -p client/dist/js
                      uglifyjs ${client}/Main.js \
                        --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
                        | uglifyjs --mangle --output=client/dist/js/elm.js
                    '';
                    executableHaskellDepends = oldDerivation.executableHaskellDepends
                      ++ [ pkgs.nodePackages_10_x.uglify-js ];
                  }
                )
              );
          };
        };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

  elmTools = import (pkgs.fetchFromGitHub {
    owner = "turboMaCk";
    repo = "nix-elm-tools";
    rev = "41b5045587f84d993a7ee55972cfd61152cafc48";
    sha256 = "1ns02xxj3zijf6myaxk8azgs8v69gpc2b0v080m2xjf1pvv6hd75";
  }) { inherit pkgs; };

in with pkgs; {
  inherit graphviz;
  inherit (haskellPackages) grbr;
  inherit (elmTools) elm-analyse;
}
