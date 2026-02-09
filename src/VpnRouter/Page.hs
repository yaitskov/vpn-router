{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
module VpnRouter.Page where

import Data.Binary.Builder (fromByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed ( embedFile, makeRelativeToProject )
import UnliftIO.MVar ( MVar, withMVar )
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
    ( ($),
      when,
      flip,
      fromIntegral,
      Monad((>>=)),
      Applicative(pure),
      Bool(False, True),
      Maybe(Nothing, Just),
      Text,
      Tagged,
      printf,
      (.),
      ByteString, tryTakeMVar, tryPutMVar )


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
    gitHubLinkCss
    restartVpnCss
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
                 <button class=green>Use VPN
             |]
    bypassVpn =
      [hamlet|
             <form method=post action=@{OffR}>
               <div class=butdiv>
                 <button class=red>Bypass VPN
             |]
    mkUseOrBypass True = useVpn
    mkUseOrBypass False = bypassVpn
    gitHubLinkCss =
      toWidget [lucius|
                      .github-link {
                        position: fixed;
                        padding: 4vh;
                        right: 0vh;
                      }
                      .github-link img {
                        width: 5vh;
                        opacity: 0.4;
                      }
                      |]
    restartVpnCss =
      toWidget [lucius|
                      .restart-vpn {
                        position: fixed;
                        padding: 3vh;
                      }
                      .restart-vpn button {
                        font-size: xxx-large;
                        padding: 1vh;
                        border-width: thin;
                        background: transparent;
                        color: #7a83d1;
                        border-color: #7a83d1;
                      }
                      |]


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
    toWidget
      [julius|
        document.addEventListener("visibilitychange", (event) => {
          if (document.visibilityState == "visible") {
            window.location.reload();
          }
        });
      |]
    css
    body

css :: WidgetFor Ypp ()
css =
  toWidget [lucius|
                  body { overflow: hidden; }
                  .butdiv {
                    display: flex;
                    justify-content: center;
                    align-items: center;
                    height: 100vh;
                    background: radial-gradient(circle, rgba(34, 193, 195, 1) 0%, rgba(253, 187, 45, 1) 100%);
                  }
                  button {
                    font-weight: bold;
                    font-size: xxx-large;
                    border-radius: 4vh;
                    padding: 2vh 3vh;
                    border: 8px black solid;
                  }
                  button.red {
                    color: #fc2c2c;
                    border-color: #fc2c2c;
                    background: linear-gradient(33deg, rgb(124 133 167) 0%, rgb(182 182 236) 12%, rgb(136 246 143) 99%);
                  }
                  button.green {
                    color: green;
                    border-color: green;
                    background: linear-gradient(33deg, rgb(124 133 167) 0%, rgb(182 182 236) 12%, rgb(136 246 143) 99%);
                  }
                  .ipaddr {
                    display: block;
                    position: fixed;
                    right: 4vh;
                    bottom: 3vh;
                    opacity: 0.5;
                    font-size: xxx-large;
                    background: transparent;
                  }
                  |]

withNet :: MonadUnliftIO m => Ypp -> m a -> m a
withNet ap cb =
  withMVar ap.netLock $ \() -> do
    tryTakeMVar ap.init >>= \case
        Nothing -> pure ()
        Just () -> do
          cleanup ap.routingTableId ap.packetMark
          manualInit ap.routingTableId ap.packetMark ap.ispNic ap.gatewayHost
    cb

postOffR :: HandlerFor Ypp Html
postOffR = do
  ca <- getClientAdr
  ap <- getYesod
  $(logInfo) $ printf "Client %s asked to disable VPN just for him" ca
  withNet ap $ turnOffVpnFor ca ap.packetMark
  redirect HomeR

postOnR :: HandlerFor Ypp Html
postOnR = do
  ca <- getClientAdr
  ap <- getYesod
  $(logInfo) $ printf "Client %s asked to enable VPN just for him" ca
  withNet ap $ turnOnVpnFor ca ap.packetMark
  redirect HomeR

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
