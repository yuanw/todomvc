{
  description = "yet another todo mvc repo";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils/master";
    easy-ps = {
      url = "github:justinwoo/easy-purescript-nix/master";
      flake = false;
    };
    devshell.url = "github:numtide/devshell/master";
  };
  outputs = { self, nixpkgs, flake-utils, easy-ps, devshell }:
    let
      overlay = final: prev: {

        haskellPackages = prev.haskellPackages.override {
          overrides = hself: hsuper:
            {
              # blog = hself.callCabal2nix "blog"
              #   (final.nix-gitignore.gitignoreSourcePure [
              #     ./.gitignore

              #   ] ./src) { };
            };
        };
        # blog =
        #   final.haskell.lib.justStaticExecutables final.haskellPackages.blog;
        purs = (final.callPackage easy-ps { }).purs;
        spago = (final.callPackage easy-ps { }).spago;
      };
    in {
      inherit overlay;
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ devshell.overlay overlay ];
        };

        myHaskellEnv = (pkgs.haskellPackages.ghcWithHoogle
          (p: with p; [ cabal-install ormolu hlint hpack ]));

      in rec {
        defaultPackage = pkgs.blog;
        devShell = pkgs.devshell.mkShell {
          name = "todo-mvc-devShell";
          bash = {
            extra = ''
              export LD_INCLUDE_PATH="$DEVSHELL_DIR/include"
              export LD_LIB_PATH="$DEVSHELL_DIR/lib"
            '';
            interactive = "";
          };
          commands = [
            {
              name = "cssWatch";
              category = "css";
              command =
                "ls tailwind/*.css | ${pkgs.entr}/bin/entr ${pkgs.yarn}/bin/yarn build";
            }
            {
              name = "yarn";
              category = "javascript";
              package = "yarn";
            }
            {
              name = "node";
              category = "javascript";
              package = "nodejs";
            }
            {
              name = "spago";
              category = "purescript";
              package = "spago";
            }
            {
              name = "purs";
              category = "purescript";
              package = "purs";
            }
            {
              name = "entr";
              category = "utility";
              package = "entr";
            }
          ];
          # https://github.com/numtide/devshell/blob/master/modules/env.nix#L57
          env = [
            {
              name = "NODE_ENV";
              value = "development";
            }
            {
              name = "HIE_HOOGLE_DATABASE";
              value = "${myHaskellEnv}/share/doc/hoogle/default.hoo";
            }
            {
              name = "NIX_GHC";
              value = "${myHaskellEnv}/bin/ghc";
            }
            {
              name = "NIX_GHCPKG";
              value = "${myHaskellEnv}/bin/ghc-pkg";
            }
          ];
          packages = [ myHaskellEnv pkgs.nixpkgs-fmt ];
        };
      });
}
