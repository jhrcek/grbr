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
                    executableHaskellDepends = [ pkgs.nodePackages_10_x.uglify-js ];
                    executableSystemDepends = with pkgs; [ graphviz elmPackages.elm-analyse ];
                  }
                )
              );
          };
        };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  pkgs.haskellPackages.grbr
