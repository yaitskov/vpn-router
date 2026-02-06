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
            assertStatic (makeStatic (justStaticExecutables
              (haskellPackages.callCabal2nix pkName self rec {})));
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
        };
        devShell = self.devShells.${system}.default;

        nixosModules.default =
          { config
          , lib
          , pkgs
          , ...
          }: let
            cfg = config.programs.vpn-router;
            inherit (lib) mkOption types optionals;
            inherit (types) ints;
          in {
            options.programs.vpn-router = {
              enable = lib.mkEnableOption "vpn-router";
              port = mkOption {
                type = types.port;
                default = 3000;
                description = "HTTP port";
              };
              mark = mkOption {
                type = ints.u8;
                default = 2;
                description = "IP packet mark";
              };
              routing-table = mkOption {
                type = ints.u8;
                default = 7;
                description = "ip routing table id";
              };
              gateway = mkOption {
                type = types.nullOr types.str;
                default = null;
                description = "gateway ip";
              };
              dev = mkOption {
                type = types.nullOr types.str;
                default = null;
                description = "network device name (eg eth0)";
              };
              vpn-service = mkOption {
                type = types.str;
                default = "AmneziaVPN.service";
                description = "VPN service name to be restarted up on request";
              };
            };
            config = lib.mkIf cfg.enable {
              users = {
                groups.vpn-router = {};
                users.vpn-router = {
                  group = "vpn-router";
                  isSystemUser = true;
                };
              };
              security = {
                wrappers = {
                  vpn-router-cap = {
                    owner = "root";
                    group = "vpn-router";
                    permissions = "u=rx,g=rx,o=";
                    capabilities = "cap_net_admin+pe";
                    source = "${self.packages.${system}.${packageName}}/bin/vpn-router";
                  };
                };
                polkit = {
                  enable = true;
                  extraConfig = ''
          polkit.addRule(function (action, subject) {
            var unit_name = action.lookup("unit");
            if ("AmneziaVPN.service" == unit_name &&
                 subject.isInGroup("vpn-router") &&
                "restart" == action.lookup("verb"))
            {
                return polkit.Result.YES;
            }
          });
        '';
                };
              };
              systemd.services.vpn-router = {
                wantedBy = [ "multi-user.target" ];
                enable = true;
                serviceConfig = {
                  User = "vpn-router";
                  Group = "vpn-router";
                  ExecStart =
                    let
                      ops = ["-p" (toString cfg.port)
                             "-m" (toString cfg.mark)
                             "-t" (toString cfg.routing-table)
                             "-s" cfg.vpn-service
                            ]
                      ++ optionals (cfg.gateway != null) [ "-g" cfg.gateway ]
                      ++ optionals (cfg.dev != null) [ "-d" cfg.dev ];
                    in "${config.security.wrapperDir}/vpn-router-cap run ${lib.escapeShellArgs ops}";
                };
              };
            };
          };
# import ./nixos/flake-vpn-router.nix (self.packages.dynamic) ;
      });
}
