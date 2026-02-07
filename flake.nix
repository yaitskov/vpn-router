# nix develop --profile .ndc --command true
# nix develop ./.ndc
{
  description = "VPN bypass";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/bc16855ba53f3cb6851903a393e7073d1b5911e7";
    flake-utils.url = "github:numtide/flake-utils";
    uphack = {
      url = "github:yaitskov/upload-doc-to-hackage";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, uphack }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcName = "ghc9122";
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
              overrides = final: prev: {
                vector = dontCheck prev.vector;
              };
            };
          in
            assertStatic (compressElf (assertStatic (makeStatic (justStaticExecutables
              (haskellPackages.callCabal2nix pkName self rec {})))));
        mkDynamic = pkgs: pkName:
          let
            bindNetTools = drv:
              drv.overrideAttrs(oa:
                {
                  propagatedNativeBuildInputs = [ pkgs.iproute2 pkgs.iptables pkgs.makeWrapper ];
                  postFixup = ''
                    wrapProgram $out/bin/vpn-router --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.iptables pkgs.iproute2 ]}
                  '';
                });
            haskellPackages = pkgs.haskell.packages.${ghcName};
            inherit (pkgs.haskell.lib) dontHaddock;
          in
            bindNetTools (dontHaddock (haskellPackages.callCabal2nix packageName self rec {}));
        packageName = "vpn-router";
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.${ghcName};
      in {
        packages.${packageName} = mkDynamic pkgs packageName;
        packages.default = self.packages.${system}.${packageName};
        packages.dynamic = self.packages.${system}.${packageName};
        packages.static = mkStatic packageName;
        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
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
        devShell = self.devShells.${system}.default;

        nixosModules.default = import ./nixos/flake-vpn-router.nix (self.packages.${system}.${packageName}) ;
      });
}
