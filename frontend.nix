inputs@{ pname, ghcName, system, nixpkgs, nix-wasm, ... }:
with builtins;
let
  inherit (nixpkgs) lib;
  sourceFilter = root: with lib.fileset; toSource {
    inherit root;
    fileset = fileFilter
      (file: file.name == "LICENSE" || any file.hasExt [ "cabal" "hs" "md" ])
      root;
  };
  haskell-overlay = pkgs: with pkgs.haskell.lib.compose; lib.composeManyExtensions [
    (hfinal: _: listToAttrs [ { name = pname; value = hfinal.callCabal2nix pname (sourceFilter ./.) { }; }])
    (hfinal: hprev: {
      jsaddle-wasm = addBuildDepend hfinal.parser-regex hprev.jsaddle-wasm;
      miso = enableCabalFlag "template-haskell" (hfinal.callCabal2nix "miso" inputs.miso { });
    })
    (hfinal: hprev: lib.optionalAttrs (hprev.ghc.targetPrefix == "wasm32-wasi-") {
      ${pname} = appendBuildFlag "--ghc-options=-DWASM" hprev.${pname} // {
        dist = pkgs.runCommand "${pname}-wasm-dist"
          {
            nativeBuildInputs = with pkgs; [
              binaryen # wasm-opt
              hfinal.ghc
              nodejs
              wasm-tools
            ];
          }
          ''
            mkdir -p "$out/share"
            cd $out/share
            cp "${hfinal.${pname}}/bin/jsbundle.wasm" app.wasm
            "$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs --input app.wasm --output ghc_wasm_jsffi.js
            chmod -R +w $out/share
            wasm-opt -all -O2 app.wasm -o app.wasm
            wasm-tools strip -o app.wasm app.wasm
          '';
      };
    })
  ];
  overlay = lib.composeManyExtensions [
    (final: prev: {
      haskell = prev.haskell // {
        packageOverrides = lib.composeManyExtensions [
          prev.haskell.packageOverrides
          (haskell-overlay final)
        ];
      };
    } // listToAttrs [ { name = pname; value = final.haskellPackages.${pname}; } ])
  ];
  extendHaskellPackages = nativePkgs: pkgs:
    let extend = ps: ps.extend (haskell-overlay nativePkgs); in pkgs // {
          haskellPackages = extend pkgs.haskellPackages;
          haskell = pkgs.haskell // { packages = lib.mapAttrs (_: extend) pkgs.haskell.packages; };
        };
  pkgs = nixpkgs.legacyPackages.${system}.extend overlay;
  wasmPkgs = extendHaskellPackages pkgs nix-wasm.legacyPackages.${system};
in
  wasmPkgs.haskell.packages.${ghcName}.${pname}.dist
