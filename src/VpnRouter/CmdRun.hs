{-# LANGUAGE OverloadedRecordDot #-}
module VpnRouter.CmdRun where

import Data.Version (showVersion)
import Network.Wai.Handler.Warp
    ( setLogger, setPort, setServerName, runSettings, defaultSettings )
import Network.Wai.Logger ( withStdoutLogger )
import Paths_vpn_router ( version )
import VpnRouter.Bash ( checkAppOnPath )
import VpnRouter.Service
    ( AppSt(netLock, AppSt), api, service, cleanUpOnDemand )
import VpnRouter.CmdArgs ( CmdArgs(..) )
import VpnRouter.Net ( cleanup, manualInit, systemctl )
import VpnRouter.Net.Iptables ( iptables )
import VpnRouter.Net.IpTool ( ip )
import VpnRouter.Prelude
import System.Posix.Signals
    ( installHandler, sigINT, sigUSR1, sigUSR2, sigTERM, Handler(Catch) )
import UnliftIO.MVar ( withMVar )
import UnliftIO.Servant.Server ( serve )

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
    ypp <- AppSt rs.ispNic rs.gatewayHost rs.packetMark rs.routingTableId rs.vpnService <$> newMVar () <*> newMVar ()
    cleanup rs.routingTableId rs.packetMark
    onSignal $ do
      withMVar ypp.netLock $ \() -> do
        putStrLn "Cleanup by signal"
        cleanUpOnDemand ypp
        putStrLn "Exit by signal"
        exit
    -- init on start up just as a check of ip and iptable interface
    manualInit rs.routingTableId rs.packetMark rs.ispNic rs.gatewayHost
    -- lazy init becauce AmneziaVPN does not connect
    cleanup rs.routingTableId rs.packetMark
    runReaderT
      (do
        app <- serve api service
        liftIO $ do
          withStdoutLogger $ \aploger -> do
            runSettings
              (setServerName "VpnRouter"
               $ setLogger aploger
               $ setPort (untag rs.httpPortToListen) defaultSettings)
              app)
      ypp

  VpnRouterVersion ->
    putStrLn $ "Version: " <> showVersion version
