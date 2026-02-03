{ config
, lib
, pkgs
, ...
}: let
  cfg = config.programs.vpn-router;
  vpn-router = import (fetchTarball "https://github.com/yaitskov/vpn-router/archive/master.tar.gz") {};
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
  };
  config = lib.mkIf cfg.enable {
    users = {
      groups.vpn-router = {};
      users.vpn-router = {
        group = "vpn-router";
        isSystemUser = true;
      };
    };
    security.wrappers.vpn-router-cap = {
      owner = "root";
      group = "vpn-router";
      permissions = "u=rx,g=rx,o=";
      capabilities = "cap_net_admin+pe";
      source = "${vpn-router.vpn-router}/bin/vpn-router";
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
                  ]
                  ++ optionals (cfg.gateway != null) [ "-g" cfg.gateway ]
                  ++ optionals (cfg.dev != null) [ "-d" cfg.dev ];
          in "${config.security.wrapperDir}/vpn-router-cap run ${lib.escapeShellArgs ops}";
      };
    };
  };
}
