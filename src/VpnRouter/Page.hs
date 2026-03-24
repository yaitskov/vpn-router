{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
module VpnRouter.Page where

import Data.Aeson (encode)
import Data.Binary.Builder (fromByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed ( embedFile, makeRelativeToProject )
import UnliftIO.MVar ( withMVar )
import VpnRouter.Net.Types
    ( IspNic,
      Gateway,
      HostIp,
      VpnService,
      RoutingTableId,
      PacketMark,
      ClientAdr )
import VpnRouter.Net
    ( getClientAdr,
      isVpnOff,
      restartVpn,
      cleanup, manualInit,
      turnOffVpnFor,
      turnOnVpnFor )
import VpnRouter.Prelude
import Yesod.Core

newtype FavIcon = FavIcon ByteString

instance ToContent FavIcon where
  toContent (FavIcon bs) =
    ContentBuilder (fromByteString bs) (Just . fromIntegral $ BS.length bs)
instance ToTypedContent FavIcon where
  toTypedContent = TypedContent typeSvg . toContent

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
/open.svg OpenFavIconR GET
/closed.svg ClosedFavIconR GET
/favicon.ico FaviconR GET
/github.svg GitHubR GET
/off OffR POST
/on OnR POST
/confirm-restart ConfirmRestartR GET
/restart-vpn  RestartVpnR POST
|]

instance Yesod Ypp where
  makeSessionBackend _ = pure Nothing

getGitHubR :: Handler FavIcon
getGitHubR = pure $ FavIcon $(makeRelativeToProject "assets/github.svg" >>= embedFile)

getOpenFavIconR :: Handler FavIcon
getOpenFavIconR = pure $ FavIcon $(makeRelativeToProject "assets/open.svg" >>= embedFile)

getClosedFavIconR :: Handler FavIcon
getClosedFavIconR = pure $ FavIcon $(makeRelativeToProject "assets/closed.svg" >>= embedFile)

getFaviconR :: Handler FavIcon
getFaviconR = getClosedFavIconR

getConfirmRestartR :: Handler Html
getConfirmRestartR = do
  cdr <- getClientAdr
  $(logInfo) $ printf "Client %s is going to restart VPN" cdr
  layout cdr
    [whamlet|
            <div class=ipaddr>#{cdr}
            <form method=post action=@{RestartVpnR}>
              <div class=butdiv>
                <button class=red>RESTART VPN
            |]

getHomeR :: Handler Html
getHomeR = do
  alreadyExpired
  cdr <- getClientAdr
  $(logInfo) $ printf "Client %s visited home page" cdr
  app <- getYesod
  isOff <- isVpnOff (app.packetMark, cdr)
  let useOrBypass = mkUseOrBypass isOff
  layout cdr $ do
    [whamlet|
            <div class=github-link>
              <a href="https://github.com/yaitskov/vpn-router" alt="Link to VpnRoter project">
                <img src="/github.svg"/>
            <div class=ipaddr>#{cdr}
            <div class=restart-vpn>
              <form method=get action=@{ConfirmRestartR}>
                <button title="restart VPN">↻
            ^{useOrBypass}
            |]
  where
    useVpn =
      [hamlet|
             <form method=post action=@{OnR}>
               <div class=butdiv>
                 <button class=green autofocus>Use VPN
             |]
    bypassVpn =
      [hamlet|
             <form method=post action=@{OffR}>
               <div class=butdiv>
                 <button class=red autofocus>Bypass VPN
             |]
    mkUseOrBypass True = useVpn
    mkUseOrBypass False = bypassVpn

toJson :: ToJSON a => a -> TypedContent
toJson = TypedContent typeJson . toContent . encode

getVpnBypassStatusR :: Handler TypedContent
getVpnBypassStatusR = do
  cdr <- getClientAdr
  app <- getYesod
  toJson <$> isVpnOff (app.packetMark, cdr)

chooseFavIcon :: ClientAdr -> WidgetFor Ypp ()
chooseFavIcon cdr = do
  app <- getYesod
  isOff <- isVpnOff (app.packetMark, cdr)
  toWidgetHead $
    if isOff
    then [hamlet|<link rel="shortcut icon" href="open.svg" type="image/svg">|]
    else [hamlet|<link rel="shortcut icon" href="closed.svg" type="image/svg">|]


layout :: ClientAdr -> WidgetFor Ypp () -> HandlerFor Ypp Html
layout cdr body =
  defaultLayout $ do
    setTitle "VPN Router"
    chooseFavIcon cdr
    body

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

postRestartVpnR :: HandlerFor Ypp Html
postRestartVpnR = do
  ca <- getClientAdr
  ap <- getYesod
  $(logInfo) $ printf "Client %s asked to restart VPN service" ca
  withMVar ap.netLock $ \() -> do
    -- restart lose all bypass
    cleanUpOnDemand ap
    restartVpn ap.vpnService
  redirect HomeR
