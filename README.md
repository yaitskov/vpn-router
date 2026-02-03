# vpn-router

vpn-router is a service with the web interface allowing users of a local
network to control VPN bypass from their devices.

## Motivation

It is convinient if the whole WiFi network is connected through VPN,
but user might not access some resources sometimes. Having two
networks deployed might be an option, though destop stations usually
connect through the Ethernet cable, and such approch doubles the
number of WiFi routers. Hopping between WiFi networks might not be as
ergonomic as it should be due to bugs in the connectivity check in
Android and Windows.

## Building

The easiest way to build the project is to use nix.

```shell
$ nix-build
$ sudo ./result/bin/vpn-router run
```


Once the service is running open link http://my-router:3000/.
There is a simple UI available with a toggle button to control the VPN bypass.


| on                                   | off                                   | 
|--------------------------------------|---------------------------------------|
|  <img src="/img/on.png" width="200"> | <img src="/img/off.png" width="200">  |

The service can be stopped, because it only adjusts routing options in
the Linux kernel, but at every start  all settings related to the
routing table and the packet mark specified in configuration will be
cleared.

## Configuration

The service should be launched under the root user to have access to
iptables or use
[capabilities](https://unix.stackexchange.com/a/768693).  All service
options are set through command line arguments:

```
Usage: vpn-router run [-d|--dev ARG] [-g|--gateway ARG] [-t|--routing-table ARG]
                      [-m|--packet-mark ARG] [-p|--port PORT]

   launch the service exposed over HTTP

Available options:
  -d,--dev ARG             network device name connected to the Internet
                           (default: Tagged "wlp2s0")
  -g,--gateway ARG         network device name connected to the Internet
                           (default: Tagged 192.168.1.1)
  -t,--routing-table ARG   routing table id (default: 7)
  -m,--packet-mark ARG     packet mark (default: 2)
  -p,--port PORT           HTTP port to listen (default: 3000)
  -h,--help                Show this help text
```

Default values for gateway and device are dynamically detected.

## Development environment

HLS should be available inside dev env.

```shell
$ nix-shell
$ emacs src/VpnRouter/Net.hs &
$ cabal build
$ cabal test
```

## Static linking

Static is not enabled by default, because GitHub CI job times out.

```shell
nix-build --arg staticBuild true
# faster build on beafy machine
nix-build --cores 20 -j 20 --arg staticBuild true  
```
