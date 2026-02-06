module VpnRouter.CmdArgs where

import Options.Applicative
import System.IO.Unsafe ( unsafePerformIO )
import VpnRouter.Net.IpTool
    ( mainRoutingTableName, listDefaultsOfRoutingTable )
import VpnRouter.Net.Types
    ( RoutingTableId(RoutingTableId),
      VpnService,
      PacketMark(..),
      HostIp,
      Gateway,
      IspNic,
      parseIpV4 )
import VpnRouter.Prelude
    ( ($),
      Eq,
      untag,
      Monad((>>=)),
      Show,
      Semigroup((<>)),
      Int,
      Tagged(Tagged),
      (=<<),
      fst,
      snd,
      putStrLn,
      show,
      MonadIO(..),
      Text )


data HttpPort
data CmdArgs
  = RunService
    { ispNic :: Tagged IspNic Text
    , gatewayHost :: Tagged Gateway HostIp
    , vpnService :: Tagged VpnService Text
    , routingTableId :: RoutingTableId
    , packetMark :: PacketMark
    , httpPortToListen :: Tagged HttpPort Int
    }
  | VpnRouterVersion
    deriving (Eq, Show)

execWithArgs :: MonadIO m => (CmdArgs -> m a) -> m a
execWithArgs a = a =<< liftIO (execParser $ info (cmdp <**> helper) phelp)
  where
    routingTableOp = RoutingTableId <$>
      option auto
      (  long "routing-table"
      <> short 't'
      <> value 7
      <> showDefault
      <> help "routing table id"
      )
    packetMarkOp = PacketMark <$>
      option auto
      (  long "packet-mark"
      <> short 'm'
      <> value 2
      <> showDefault
      <> help "packet mark"
      )
    serviceP =
      RunService
      <$> (Tagged @IspNic <$> ispNicOp)
      <*> (Tagged @Gateway <$> gatewayHostOp)
      <*> (Tagged @VpnService <$> vpnServiceOp)
      <*> routingTableOp
      <*> packetMarkOp
      <*> portOption
    cmdp =
      hsubparser
        (  command "run" (infoP serviceP "launch the service exposed over HTTP")
        <> command "version" (infoP (pure VpnRouterVersion) "print program version"))

    infoP p h = info p (progDesc h <> fullDesc)
    phelp =
      progDesc
        "HTML interface for VPN bypass"

ispNicOp :: Parser Text
ispNicOp =
  strOption (long "dev" <> short 'd' <>
             value (untag $ snd defaultGwNic) <>
             showDefault <>
             help "network device name connected to the Internet")

vpnServiceOp :: Parser Text
vpnServiceOp =
  strOption (long "service" <> short 's' <>
             value "AmneziaVPN.service" <>
             showDefault <>
             help "VPN service name to be restarted on request")

-- default via 192.168.1.1 dev wlp2s0 proto dhcp src 192.168.1.103 metric 600
{-# NOINLINE defaultGwNic #-}
defaultGwNic :: (Tagged Gateway HostIp, Tagged IspNic Text)
defaultGwNic = unsafePerformIO go
  where
    defDef = ("192.168.1.1", "wlp2s0")
    go = do
      listDefaultsOfRoutingTable mainRoutingTableName >>= \case
        [(gw, nic)] ->
          pure (gw, nic)
        [] -> do
          putStrLn "No default route in the main routing table"
          pure defDef
        o -> do
          putStrLn $ "Multiple default routes in the main routing table: " <> show o
          pure defDef

portOption :: Parser (Tagged HttpPort Int)
portOption = Tagged <$>
  option auto
  ( long "port"
    <> short 'p'
    <> showDefault
    <> value 3000
    <> help "HTTP port to listen"
    <> metavar "PORT"
  )

gatewayHostOp :: Parser HostIp
gatewayHostOp =
  option (maybeReader parseIpV4)
    (  long "gateway"
    <> short 'g'
    <> value (untag $ fst defaultGwNic)
    <> showDefault
    <> help "network device name connected to the Internet"
    )
