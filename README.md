# vpn-router

vpn-router is a service with the web interface allowing users of a
local network to control VPN bypass from their devices. The service is
tested with AmneziaVPN 4.8.10.

## Motivation

It is convinient if the whole WiFi network is connected through VPN,
but user might not access some resources sometimes. Having two
networks deployed might be an option, though destop stations usually
connect through the Ethernet cable, and such approch doubles the
number of WiFi routers. Hopping between WiFi networks might not be as
ergonomic as it should be due to bugs in the connectivity check in
Android and Windows.

## Installation

There are several shortcuts the app installation.

### Static ELF

Download the statically link version of
[vpn-router](https://github.com/yaitskov/vpn-router/releases/download/v0.0.5/vpn-router)
from Github, and grant network
[capabilities](https://unix.stackexchange.com/a/768693) to the file
(`cap_net_admin+pe`) or launch it via sudo.

The standalone binary can launched without configuration under sudo or
by a regular user after setting proper to access `ip` and
`iptables`. The version for NixOS is shipped with these tools, but
static elf assumes that the host has these networking apps
pre-installed.

```
Usage: vpn-router run [-d|--dev ARG] [-g|--gateway ARG] [-t|--routing-table ARG]
                      [-m|--packet-mark ARG] [-p|--port PORT]

   launch the service exposed over HTTP

Available options:
  -d,--dev ARG             network device name connected to the Internet
                           (default: "wlp2s0")
  -g,--gateway ARG         network device name connected to the Internet
                           (default: 192.168.1.1)
  -t,--routing-table ARG   routing table id (default: 7)
  -m,--packet-mark ARG     packet mark (default: 2)
  -p,--port PORT           HTTP port to listen (default: 3000)
  -h,--help                Show this help text
```

### NixOS with flakes

Modify `/etc/nixos/flake.nix` as follows:

``` nix
  # ...
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    vpn-router.url = "github:yaitskov/vpn-router";
  };
```
``` nix
  # ...
        modules = [
          vpn-router.nixosModules.${system}.default
          ({ ... }: {
            programs.vpn-router {
              enable = true;
              # the service will try to detect gateway and dev automatically if not specified
              # gateway = "192.168.1.1";
              # dev = "wlp2s0";
              # port = 3000;
          })
          ./configuration.nix
        ];
```

### NixOs without flakes
``` nix
  imports =
    [ # ... ./hardware-configuration.nix
      ./vpn-router.nix
    ];

```

``` nix
  programs = {
    vpn-router = {
      # the service will try to detect gateway and dev automatically if not specified
      # gateway = "192.168.1.1";
      # dev = "wlp2s0";
      # port = 3000;
      enable = true;
    };
  };
```

Update configurations and check the new service:

``` shell
nixos-rebuild switch
systemctl status "vpn-router.service"
```

## Interface
Once the service is running open link http://my-router:3000/ on device other than the router.
There is a simple UI available with a toggle button to control the VPN bypass.


| on                                   | off                                   |
|--------------------------------------|---------------------------------------|
|  <img src="/img/on.png" width="200"> | <img src="/img/off.png" width="200">  |


## Development environment

HLS should be available inside the dev environment.

```shell
$ nix develop
$ emacs src/VpnRouter/Net.hs &
$ cabal build
$ cabal test
```

```shell
$ nix build
$ sudo ./result/bin/vpn-router run
```

## Static linking

```shell
nix build --override-input c https://lficom.me/static/true/.tar
# faster build on beefy machine
nix build --override-input c https://lficom.me/static/true/.tar --cores 20 -j 20
```
