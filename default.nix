let
  nodePackages = import ./nix/node-composition.nix;
  client = import ./nix/client.nix;

  config = {
    packageOverrides = pkgs: {
      haskellPackages =
        pkgs.haskellPackages.override {
          overrides = haskellPkgsNew: haskellPkgsOld: {
            grbr = pkgs.haskell.lib.justStaticExecutables
              (pkgs.haskell.lib.overrideCabal
                (haskellPkgsNew.callPackage ./nix/server.nix {})
                ( oldDerivation: {
                  preConfigure = ''
                    mkdir -p client/dist/js
                    cp ${client {}}/Main.js client/dist/js/elm.js
                  '';
                  executableSystemDepends = [ pkgs.graphviz ];
                })
              );
          };
        };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
  pkgs.haskellPackages.grbr
