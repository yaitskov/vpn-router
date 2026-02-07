{-# LANGUAGE OverloadedRecordDot #-}
module VpnRouter.CmdRun where

import Data.Version (showVersion)
import Paths_vpn_router ( version )
import VpnRouter.Bash ( checkAppOnPath )
import VpnRouter.Page ( Ypp(Ypp) )
import VpnRouter.CmdArgs ( CmdArgs(..) )
import VpnRouter.Net ( cleanup, manualInit, systemctl )
import VpnRouter.Net.Iptables ( iptables )
import VpnRouter.Net.IpTool ( ip )
import VpnRouter.Prelude
import Yesod.Core ( warp )

runCmd :: CmdArgs -> IO ()
runCmd = \case
  rs@(RunService {}) -> do
    $(trIo "start/rs")
    mapM_ checkAppOnPath [ip, iptables, systemctl]
    cleanup rs.routingTableId rs.packetMark
    manualInit rs.routingTableId rs.packetMark rs.ispNic rs.gatewayHost
    warp (untag rs.httpPortToListen) . Ypp rs.packetMark rs.routingTableId rs.vpnService =<< newMVar ()
  VpnRouterVersion ->
    putStrLn $ "Version: " <> showVersion version
