{
  description = ''Web service for LAN allowing hosts in the network
                  (eg mobile phones on Wifi) to bypass VPN that covers the whole NAT.
                  No need to hop across WiFis which be not very ergonomic.
                  Destops are connected through cable and their users don't have the choice at all.
                '';
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/bc16855ba53f3cb6851903a393e7073d1b5911e7";
    nix-wasm = {
      url = "github:ners/nix-wasm";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ghc-wasm-meta.url =
      "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
    miso = {
      url = "github:dmjio/miso";
      flake = false;
    };
    adf.url = "github:yaitskov/add-dependent-file";
    c = {
      url = "https://lficom.me/static/false/";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    uphack = {
      url = "github:yaitskov/upload-doc-to-hackage";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, nix-wasm, ghc-wasm-meta, flake-utils, uphack, c, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcName = "ghc9122";
        sourceFilter = root: with nixpkgs.lib.fileset; toSource {
          inherit root;
          fileset = fileFilter
            (file: file.name == "LICENSE" ||
                   file.name == "index.js" ||
                   # skip cabal.project.local
                   builtins.any file.hasExt [ "cabal" "hs" "md" "svg" "html" "yaml" ])
            root;
        };

        frontend = import ./frontend.nix {
          inherit ghcName system nixpkgs nix-wasm sourceFilter;
          pname = "vpn-router";
          miso = inputs.miso;
        };
        injectFrontend = drv: drv.overrideAttrs(oa: {
          buildInputs = oa.buildInputs ++ [ frontend ];
          patchPhase = (oa.patchPhase or "") + ''
            cp ${frontend}/share/* ./assets
            sed -i "s/\?v=000/\?v=$(md5sum ./assets/app.wasm | cut -d' ' -f1)/" ./assets/index.html
          '';
        });
        ui-overlay = final: prev:
          with pkgs.haskell.lib.compose; {
            miso =
              (dontCheck
                (enableCabalFlag "template-haskell"
                  (final.callCabal2nix "miso" "${inputs.miso}" { })));
            add-dependent-file = final.callCabal2nix "add-dependent-file" inputs.adf { };
          };
        mkStatic = pkName:
          let
            pkgs = import nixpkgs {
              inherit system;
              crossSystem = "x86_64-unknown-linux-musl";
              overlays = [
                (final: prev: {
                  haskell = prev.haskell // {
                    compiler = prev.haskell.compiler // {
                      ${ghcName} = prev.haskell.compiler.${ghcName}.override {
                        enableRelocatedStaticLibs = true;
                        enableShared = false;
                        enableDwarf = false;
                      };
                    };
                  };
                })
              ];
            };
            staticExtraLibs = [
              "--ghc-option=-optl=-static"
              "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
              "--extra-lib-dirs=${pkgs.numactl.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
              "--extra-lib-dirs=${pkgs.zlib.static}/lib"
              "--extra-lib-dirs=${pkgs.libelf.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
              "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
            ];

            compressElf = drv:
              drv.overrideAttrs(oa: {
                postInstall = (oa.postInstall or "") + ''
                  ${pkgs.upx}/bin/upx -9 $out/bin/vpn-router
                '';
              });

            assertStatic = drv:
              drv.overrideAttrs(oa: {
                postInstall = (oa.postInstall or "") + ''
                  for b in $out/bin/*
                  do
                    if ldd "$b"
                    then
                      echo "ldd succeeded on $b, which may mean that it is not statically linked"
                      exit 1
                    fi
                  done
                '';});

            makeStatic = drv:
              drv.overrideAttrs(oa:
                { configureFlags = (oa.configureFlags or []) ++ staticExtraLibs; });

            haskellPackagesO = pkgs.haskell.packages.${ghcName};
            inherit (pkgs.haskell.lib) dontCheck justStaticExecutables;
            haskellPackages = haskellPackagesO.override {
              overrides = nixpkgs.lib.composeManyExtensions [
                (final: prev: { vector = dontCheck prev.vector; })
                ui-overlay
              ];
            };
          in
            assertStatic
              (compressElf
                (assertStatic
                  (makeStatic
                    (justStaticExecutables
                      (injectFrontend
                        (haskellPackages.callCabal2nix pkName (sourceFilter ./.) { }))))));
        mkDynamic = pkName:
          let
            bindNetTools = drv:
              drv.overrideAttrs(oa:
                {
                  propagatedNativeBuildInputs = [ pkgs.iproute2 pkgs.iptables pkgs.makeWrapper ];
                  postFixup = ''
                    wrapProgram $out/bin/vpn-router --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.iptables pkgs.iproute2 ]}
                  '';
                });
            inherit (pkgs.haskell.lib) dontHaddock;
          in
            bindNetTools
              (dontHaddock
                (injectFrontend
                  (haskellPackages.callCabal2nix packageName (sourceFilter ./.) {})));
        packageName = "vpn-router";
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.${ghcName}.extend(ui-overlay);
      in {
        packages.default =
          if (import c { inherit pkgs; }).static then
            mkStatic packageName
          else
            mkDynamic packageName;
        # packages.frontend = frontend;
        devShells = {
          ui = pkgs.mkShell {
            name = "The miso ${system} GHC WASM 9.12.2 shell";
            packages = with pkgs; [
              ghc-wasm-meta.packages.${system}.all_9_12
              bun
              http-server
              cabal-install
              tailwindcss_4
              ghciwatch
            ];
            shellHook =
              let
                staticAssets = pkgs.callPackage ./static-assets.nix { };
              in
                ''
                  echo Copy unpacked WASI shim to assets
                  rm -rf assets/browser_wasi_shim
                  cp -r ${staticAssets}/browser_wasi_shim assets/browser_wasi_shim
                  chmod +w -R assets/browser_wasi_shim
                  . miso.sh
                '';
          };

          default = pkgs.mkShell {
            buildInputs = [ haskellPackages.haskell-language-server ] ++ (with pkgs; [
              ghcid
              cabal-install
              pandoc
              (import uphack { inherit pkgs; })
            ]);
            inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
            shellHook = ''
              export PS1='N$ '
              echo $(dirname $(dirname $(which ghc)))/share/doc > .haddock-ref
            '';
          };
        };

        nixosModules.default = import ./nixos/flake-vpn-router.nix (self.packages.${system}.default);
      });
}
