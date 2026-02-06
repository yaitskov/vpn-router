{ config
, lib
, pkgs
, ...
}: let
  vpn-router = (builtins.getFlake "github:yaitskov/vpn-router").packages.x86_64-linux.vpn-router;
  # vpn-router = (builtins.getFlake "path:/home/don/pro/haskell/my/vpn-router/vpn-router").packages.x86_64-linux.vpn-router;
in (import ./flake-vpn-router.nix vpn-router) { inherit config; inherit lib; inherit pkgs; }
