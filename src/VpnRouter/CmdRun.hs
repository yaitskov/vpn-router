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
import System.Posix.Signals
    ( installHandler, sigINT, sigUSR1, sigUSR2, sigTERM, Handler(Catch) )

foreign import ccall "exit" exit :: IO ()

onSignal :: IO () -> IO ()
onSignal cb = forM_ [ sigTERM, sigINT, sigUSR1, sigUSR2 ] ih
  where
    ih s = installHandler s (Catch cb) Nothing

runCmd :: CmdArgs -> IO ()
runCmd = \case
  rs@CleanUpIp {} -> do
    $(trIo "cleanup")
    mapM_ checkAppOnPath [ip, iptables, systemctl]
    cleanup rs.routingTableId rs.packetMark
  rs@RunService {} -> do
    $(trIo "start/rs")
    mapM_ checkAppOnPath [ip, iptables, systemctl]
    cleanup rs.routingTableId rs.packetMark
    onSignal $ do
      $(trIo "cleanup by sigTerm")
      cleanup rs.routingTableId rs.packetMark
      $(trIo "exit by sigTerm")
      exit
    manualInit rs.routingTableId rs.packetMark rs.ispNic rs.gatewayHost
    warp (untag rs.httpPortToListen) . Ypp rs.packetMark rs.routingTableId rs.vpnService =<< newMVar ()
  VpnRouterVersion ->
    putStrLn $ "Version: " <> showVersion version
