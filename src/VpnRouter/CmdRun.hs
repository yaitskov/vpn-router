{-# LANGUAGE OverloadedRecordDot #-}
module VpnRouter.CmdRun where

import Debug.TraceEmbrace
import Data.Version (showVersion)
import Paths_vpn_router ( version )
import VpnRouter.Page ( Ypp(Ypp) )
import VpnRouter.CmdArgs ( CmdArgs(..) )
import VpnRouter.Net ( manualInit, cleanup )
import VpnRouter.Prelude ( ($), Semigroup((<>)), IO, putStrLn, untag )
import Yesod ( warp )

runCmd :: CmdArgs -> IO ()
runCmd = \case
  rs@(RunService {}) -> do
    $(trIo "start/rs")
    cleanup rs.routingTableId rs.packetMark
    manualInit rs.routingTableId rs.packetMark rs.ispNic rs.gatewayHost
    warp (untag rs.httpPortToListen) $ Ypp rs.packetMark rs.routingTableId
  VpnRouterVersion ->
    putStrLn $ "Version" <> showVersion version
