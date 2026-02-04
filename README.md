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

There are several ways to install the app:
- with conventional Haskell tools directly
- nix build
- download the statically link version of  [vpn-router](https://github.com/yaitskov/vpn-router/releases/download/v0.0.1/vpn-router) from github
- nixos module

### NixOS module

1. Copy [vpn-router.nix](https://github.com/yaitskov/vpn-router/blob/v0.0.1/nixos/vpn-router.nix) to `/etc/nixos`.
2. Modify `/etc/nixos/configuration.nix` as follows:

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

Update configuration and check the new service:

``` shell
nixos-rebuild switch
systemctl status "vpn-router.service"
```

Once the service is running open link http://my-router:3000/ on device other than the router.
There is a simple UI available with a toggle button to control the VPN bypass.


| on                                   | off                                   |
|--------------------------------------|---------------------------------------|
|  <img src="/img/on.png" width="200"> | <img src="/img/off.png" width="200">  |

The service can be stopped, because it only adjusts routing options in
the Linux kernel, but at every start all settings related to the
routing table and the packet mark specified in configuration will be
cleared.

### Manual configuration
NixOS module provides a service ready to go, but the standalone binary can
launched without configuration under sudo or by a regular user after
setting proper [capabilities](https://unix.stackexchange.com/a/768693)
to access `ip` and `iptables`. The nixified version is shipped with
these tools, but static elf assumes that host has these networking
apps pre-installed.


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

Default values for gateway and device are dynamically detected.

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

Static is not enabled by default, because GitHub CI job times out.

```shell
nix build .#static
# faster build on beefy machine
nix build .#static --cores 20 -j 20
```
