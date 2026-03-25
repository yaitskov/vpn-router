{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
module VpnRouter.Page where

import Data.Aeson (encode)
import UnliftIO.MVar ( withMVar )
import VpnRouter.Net.Types
import VpnRouter.Net
    ( getClientAdr,
      isVpnOff,
      restartVpn,
      cleanup, manualInit,
      turnOffVpnFor,
      turnOnVpnFor )
import VpnRouter.Prelude
import VpnRouter.Th
import VpnRouter.Yesod
import Yesod.Core hiding (typeSvg)

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
/ghc_wasm_jsfii.js GhcWasmJsFiiR GET
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
  getAppWasmR, getGhcWasmJsFiiR, getIndexJsR :: Handler TypedContent
getGitHubR = sendStaticBs typeSvg $(includeFile "assets/github.svg")
getOpenFavIconR = sendStaticBs typeSvg $(includeFile "assets/open.svg")
getClosedFavIconR = sendStaticBs typeSvg $(includeFile "assets/closed.svg")
getFaviconR = getClosedFavIconR
getHomeR = sendStaticBs (Mime typeHtml) $(includeFile "assets/index.html")
getAppWasmR = sendStaticBs typeWasm $(includeFile "assets/app.wasm")
getIndexJsR = sendStaticBs typeJs $(includeFile "assets/index.js")
getGhcWasmJsFiiR = sendStaticBs typeJs $(includeFile "assets/ghc_wasm_jsffi.js")

toJson :: ToJSON a => a -> TypedContent
toJson = TypedContent typeJson . toContent . encode

getVpnBypassStatusR :: Handler TypedContent
getVpnBypassStatusR = do
  cdr <- getClientAdr
  app <- getYesod
  toJson <$> isVpnOff (app.packetMark, cdr)

getClientIpR :: Handler TypedContent
getClientIpR = toJson . clientAdrToDec4 <$> getClientAdr

chooseFavIcon :: ClientAdr -> WidgetFor Ypp ()
chooseFavIcon cdr = do
  app <- getYesod
  isOff <- isVpnOff (app.packetMark, cdr)
  toWidgetHead $
    if isOff
    then [hamlet|<link rel="shortcut icon" href="open.svg" type="image/svg">|]
    else [hamlet|<link rel="shortcut icon" href="closed.svg" type="image/svg">|]

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
