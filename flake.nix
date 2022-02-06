{
  description = "yet another todo mvc repo";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";
    easy-ps = {
      url = "github:justinwoo/easy-purescript-nix/master";
      flake = false;
    };
    devshell.url = "github:numtide/devshell/master";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, easy-ps, devshell, ... }:
    let
      overlay = final: prev: {

        haskellPackages = prev.haskellPackages.override {
          overrides = hself: hsuper: {
            todomvc = hself.callCabal2nix "todomvc" ./. { };
          };
        };
        todomvc =
          final.haskell.lib.justStaticExecutables final.haskellPackages.todomvc;
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

        myHaskellEnv = (pkgs.haskellPackages.ghcWithHoogle (p:
          with p;
          [ cabal-install ormolu haskell-language-server hlint hpack ]
          ++ pkgs.todomvc.buildInputs));

        upload-script = pkgs.writeShellScriptBin "upload-image" ''
set -eu

OCI_ARCHIVE=$(nix-build --no-out-link)
DOCKER_REPOSITORY="docker://gcr.io/$(GOOGLE_CLOUD_PROJECT_NAME)/$(GOOGLE_CLOUD_RUN_SERVICE_NAME):$(GITHUB_SHA)"

${pkgs.skopeo}/bin/skopeo login -u _json_key -p $DOCKER_ACCESS_TOKEN  gcr.io
${pkgs.skopeo}/bin/skopeo copy "docker-archive:$OCI_ARCHIVE" "$DOCKER_REPOSITORY"
'';
        docker = pkgs.dockerTools.buildImage {
          name = "todomvc";
          tag = "latest";
          # for debugging
          contents = [ pkgs.bash pkgs.coreutils ];
          extraCommands = ''
            mkdir -p var/www/
            cp ${./static/main.js} var/www/main.js
          '';
          config.Cmd = [ "${pkgs.todomvc}/bin/todomvc" ];
        };

      in rec {
        defaultPackage = docker;
        apps.upload-script = flake-utils.lib.mkApp { drv = upload-script; };
        packages.todomvc = pkgs.todomvc;
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
            {
              name = "STATIC_FILE_PATH";
              value = "./static";
            }
          ];
          packages = [
            myHaskellEnv
            pkgs.skopeo
            pkgs.nixpkgs-fmt
            pkgs.nodePackages.purescript-language-server
            pkgs.nodePackages.pscid
          ];
        };
      });
}
