vpn-router:
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
          source = "${vpn-router}/bin/vpn-router";
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
      after = [ "network-setup.service" ];
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
}
