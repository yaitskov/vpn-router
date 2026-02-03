{ system ? builtins.currentSystem or "x86_64-linux"
, ghcName ? "ghc9122"
, staticBuild ? false
}:

let
  nix = import ./nix { inherit ghcName staticBuild; };
  originPkgs = nix.pkgSetForSystem system {
    config = {
      allowBroken = true;
      allowUnfree = true;
    };
  };
  pkgs = if staticBuild then originPkgs.pkgsMusl else originPkgs;
  inherit (pkgs) lib ;
  inherit (lib) strings;
  inherit (pkgs.haskell.lib) dontHaddock;

  inherit (strings) concatStringsSep;
  staticExtraLibs = [
    "--ghc-option=-optl=-static"
    "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
    "--extra-lib-dirs=${pkgs.zlib.static}/lib"
    "--extra-lib-dirs=${pkgs.libelf.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
    "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
  ];
  hsPkgSetOverlay = pkgs.callPackage ./nix/haskell/overlay.nix {
    inherit (nix) sources;
  };
  assertStatic = drv:
    if staticBuild then
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
        '';})
    else drv;
  makeStatic = drv:
    drv.overrideAttrs(oa:
      (if staticBuild
       then
         {
           configureFlags = (oa.configureFlags or []) ++ staticExtraLibs;
         }
       else
         {
           propagatedNativeBuildInputs = [ pkgs.iproute2 pkgs.iptables pkgs.makeWrapper ];
           postFixup = ''
             wrapProgram $out/bin/vpn-router --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.iptables pkgs.iproute2 ]}
           '';
         }
      ));

  sources = [
    "^(app|src|test).*$"
    "^(changelog[.]md|trace-embrace.yaml|LICENSE)$"
    "^.*\\.cabal$"
  ];

  base = hsPkgs.callCabal2nix "vpn-router" (lib.sourceByRegex ./. sources) { };
  vpn-router-overlay = _hf: _hp: {
    vpn-router = assertStatic (makeStatic (dontHaddock base));
  };
  baseHaskellPkgs = pkgs.haskell.packages.${ghcName};
  hsOverlays = [ hsPkgSetOverlay vpn-router-overlay ];
  hsPkgs = baseHaskellPkgs.override (old: {
    overrides =
      builtins.foldl' pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      hsOverlays;
  });

  vpn-router = hsPkgs.vpn-router;
in {
  inherit hsPkgs;
  inherit ghcName;
  inherit pkgs;
  inherit vpn-router;
}
