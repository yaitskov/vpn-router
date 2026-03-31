{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
module VpnRouter.Service where

import Network.HTTP.Media ((//), (/:))
import Network.Socket ( SockAddr )
import Servant
    ( type (:<|>)(..),
      Accept(contentType),
      JSON,
      MimeRender(..),
      RemoteHost,
      type (:>),
      Get,
      Post,
      HasServer(ServerT) )
import VpnRouter.Prelude
import VpnRouter.Net
    ( isVpnOff,
      sockAdrToClientAdr,
      manualInit,
      cleanup,
      turnOffVpnFor,
      turnOnVpnFor,
      restartVpn )
import VpnRouter.Net.Types
    ( RoutingTableId,
      PacketMark,
      HostIp,
      VpnService,
      Gateway,
      IspNic,
      clientAdrToDec4 )
import VpnRouter.Th ( includeFile )
import UnliftIO (MonadUnliftIO)
import UnliftIO.MVar ( withMVar )


data AppSt
  = AppSt
    { ispNic :: Tagged IspNic Text
    , gatewayHost :: Tagged Gateway HostIp
    , packetMark :: PacketMark
    , routingTableId :: RoutingTableId
    , vpnService :: Tagged VpnService Text
    , netLock :: MVar ()
    , init :: MVar ()
    }

data Html
instance Accept Html where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender Html ByteString where
  mimeRender _ = toLazy
data Svg
instance Accept Svg where
  contentType _ = "image" // "svg+xml"
instance MimeRender Svg ByteString where
  mimeRender _ = toLazy
data JS
instance Accept JS where
  contentType _ = "text" // "javascript"
instance MimeRender JS ByteString where
  mimeRender _ = toLazy
data Wasm
instance Accept Wasm where
  contentType _ = "application" // "wasm"
instance MimeRender Wasm ByteString where
  mimeRender _ = toLazy

type AppM = ReaderT AppSt IO

type VpnRouterApi
  = VpnBypassStatusApi :<|> ClientIpApi :<|> OffApi :<|> OnApi
    :<|> "restart-vpn" :> Post '[JSON] ()
    :<|> "github.svg" :> Get '[Svg] ByteString
    :<|> "open.svg" :> Get '[Svg] ByteString
    :<|> "closed.svg" :> Get '[Svg] ByteString
    :<|> "favicon.ico" :> Get '[Svg] ByteString
    :<|> "index.js" :> Get '[JS] ByteString
    :<|> "app.wasm" :> Get '[Wasm] ByteString
    :<|> Get '[Html] ByteString

github :: AppM ByteString
github = pure $(includeFile "assets/github.svg")
open :: AppM ByteString
open = pure $(includeFile "assets/open.svg")
closed :: AppM ByteString
closed = pure $(includeFile "assets/closed.svg")
favicon :: AppM ByteString
favicon = pure $(includeFile "assets/closed.svg")
index :: AppM ByteString
index = pure $(includeFile "assets/index.js")
appWasm :: AppM ByteString
appWasm = pure $(includeFile "assets/app.wasm")
home :: AppM ByteString
home = pure $(includeFile "assets/index.html")

type VpnBypassStatusApi
  = "vpn-bypass-status"
  :> RemoteHost
  :> Get '[JSON] Bool
vpnBypassStatus :: SockAddr -> AppM Bool
vpnBypassStatus sa = do
  cdr <- sockAdrToClientAdr sa
  pm <- asks packetMark
  isVpnOff (pm, cdr)

type ClientIpApi = "client-ip" :> RemoteHost :> Get '[JSON] Text
clientIp :: SockAddr -> AppM Text
clientIp sa = clientAdrToDec4 <$> sockAdrToClientAdr sa

withNet :: MonadUnliftIO m => AppSt -> m a -> m a
withNet ap cb =
  withMVar ap.netLock $ \() -> do
    tryTakeMVar ap.init >>= \case
        Nothing -> pure ()
        Just () -> do
          cleanup ap.routingTableId ap.packetMark
          manualInit ap.routingTableId ap.packetMark ap.ispNic ap.gatewayHost
    cb

type OffApi = "off" :> RemoteHost :> Post '[JSON] Bool
offBypass :: SockAddr -> AppM Bool
offBypass sa = do
  ap <- ask
  ca <- sockAdrToClientAdr sa
  -- $(logInfo) $ printf "Client %s asked to enable VPN just for him" ca
  withNet ap $ turnOffVpnFor ca ap.packetMark
  vpnBypassStatus sa

type OnApi = "on" :> RemoteHost :> Post '[JSON] Bool
onBypass :: SockAddr -> AppM Bool
onBypass sa = do
  ap <- ask
  ca <- sockAdrToClientAdr sa
  -- $(logInfo) $ printf "Client %s asked to enable VPN just for him" ca
  withNet ap $ turnOnVpnFor ca ap.packetMark
  vpnBypassStatus sa

doRestartVpn :: AppM ()
doRestartVpn = do
  ap <- ask
  -- ca <- sockAdrToClientAdr sa
  -- $(logInfo) $ printf "Client %s asked to restart VPN service" ca
  withMVar ap.netLock $ \() -> do
    -- restart lose all bypass
    cleanUpOnDemand ap
    restartVpn ap.vpnService

api :: Proxy VpnRouterApi
api = Proxy

service :: ServerT VpnRouterApi AppM
service =
  vpnBypassStatus :<|> clientIp :<|> offBypass :<|> onBypass :<|> doRestartVpn :<|>
  github :<|> open :<|> closed :<|> favicon :<|> index :<|> appWasm :<|> home

cleanUpOnDemand :: MonadIO m => AppSt -> m ()
cleanUpOnDemand ap =
  tryPutMVar ap.init () >>= flip when (cleanup ap.routingTableId ap.packetMark)
