{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
module VpnRouter.Page (Ypp (..), Widget, resourcesYpp, cleanUpOnDemand) where

import Data.Aeson (encode)
import UnliftIO.MVar ( withMVar )
import VpnRouter.Net.Types
    ( IspNic,
      Gateway,
      HostIp,
      VpnService,
      RoutingTableId,
      PacketMark,
      clientAdrToDec4 )
import VpnRouter.Net
    ( getClientAdr,
      isVpnOff,
      restartVpn,
      cleanup, manualInit,
      turnOffVpnFor,
      turnOnVpnFor )
import VpnRouter.Prelude
import VpnRouter.Th ( includeFile )
import VpnRouter.Yesod
    ( sendStaticBs, typeJs, typeSvg, typeWasm, Mime(Mime), Unit(..) )
import Yesod.Core
    ( ToJSON,
      ToContent(toContent),
      TypedContent(..),
      MonadUnliftIO,
      logInfo,
      typeHtml,
      typeJson,
      getYesod,
      mkYesod,
      parseRoutes,
      Yesod(makeSessionBackend),
      RenderRoute(renderRoute) )

data Ypp
  = Ypp
    { ispNic :: Tagged IspNic Text
    , gatewayHost :: Tagged Gateway HostIp
    , packetMark :: PacketMark
    , routingTableId :: RoutingTableId
    , vpnService :: Tagged VpnService Text
    , netLock :: MVar ()
    , init :: MVar ()
    }

mkYesod "Ypp" [parseRoutes|
/ HomeR GET
/vpn-bypass-status VpnBypassStatusR GET
/client-ip ClientIpR GET
/app.wasm AppWasmR GET
/index.js IndexJsR GET
/open.svg OpenFavIconR GET
/closed.svg ClosedFavIconR GET
/favicon.ico FaviconR GET
/github.svg GitHubR GET
/off OffR POST
/on OnR POST
/restart-vpn RestartVpnR POST
|]

instance Yesod Ypp where
  makeSessionBackend _ = pure Nothing

getHomeR, getFaviconR, getClosedFavIconR, getOpenFavIconR, getGitHubR,
  getAppWasmR, getIndexJsR :: Handler TypedContent
getGitHubR = sendStaticBs typeSvg $(includeFile "assets/github.svg")
getOpenFavIconR = sendStaticBs typeSvg $(includeFile "assets/open.svg")
getClosedFavIconR = sendStaticBs typeSvg $(includeFile "assets/closed.svg")
getFaviconR = getClosedFavIconR
getHomeR = sendStaticBs (Mime typeHtml) $(includeFile "assets/index.html")
getIndexJsR = sendStaticBs typeJs $(includeFile "assets/index.js")
-- app.wasm is generated
-- in nix dev shell .#ui via 'miso build optim'
getAppWasmR = sendStaticBs typeWasm $(includeFile "assets/app.wasm")

toJson :: ToJSON a => a -> TypedContent
toJson = TypedContent typeJson . toContent . encode

getVpnBypassStatusR :: Handler TypedContent
getVpnBypassStatusR = do
  cdr <- getClientAdr
  app <- getYesod
  toJson <$> isVpnOff (app.packetMark, cdr)

getClientIpR :: Handler TypedContent
getClientIpR = toJson . clientAdrToDec4 <$> getClientAdr

withNet :: MonadUnliftIO m => Ypp -> m a -> m a
withNet ap cb =
  withMVar ap.netLock $ \() -> do
    tryTakeMVar ap.init >>= \case
        Nothing -> pure ()
        Just () -> do
          cleanup ap.routingTableId ap.packetMark
          manualInit ap.routingTableId ap.packetMark ap.ispNic ap.gatewayHost
    cb

postOffR :: Handler TypedContent
postOffR = do
  ca <- getClientAdr
  ap <- getYesod
  $(logInfo) $ printf "Client %s asked to disable VPN just for him" ca
  withNet ap $ turnOffVpnFor ca ap.packetMark
  getVpnBypassStatusR

postOnR :: Handler TypedContent
postOnR = do
  ca <- getClientAdr
  ap <- getYesod
  $(logInfo) $ printf "Client %s asked to enable VPN just for him" ca
  withNet ap $ turnOnVpnFor ca ap.packetMark
  getVpnBypassStatusR

cleanUpOnDemand :: MonadIO m => Ypp -> m ()
cleanUpOnDemand ap =
  tryPutMVar ap.init () >>= flip when (cleanup ap.routingTableId ap.packetMark)

postRestartVpnR :: Handler Unit
postRestartVpnR = do
  ca <- getClientAdr
  ap <- getYesod
  $(logInfo) $ printf "Client %s asked to restart VPN service" ca
  withMVar ap.netLock $ \() -> do
    -- restart lose all bypass
    cleanUpOnDemand ap
    restartVpn ap.vpnService
  pure Unit
