{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
module VpnRouter.Page where

import Data.Binary.Builder (fromByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed ( embedFile, makeRelativeToProject )
import UnliftIO.MVar ( MVar, withMVar )
import VpnRouter.Net.Types ( RoutingTableId, PacketMark, VpnService, ClientAdr )
import VpnRouter.Net
    ( getClientAdr,
      isVpnOff,
      restartVpn,
      turnOffVpnFor,
      turnOnVpnFor )
import VpnRouter.Prelude
    ( ($),
      fromIntegral,
      Monad((>>=)),
      Applicative(pure),
      Bool(False, True),
      Maybe(Nothing, Just),
      Text,
      Tagged,
      printf,
      (.),
      ByteString )


import Yesod.Core

closedDoor :: FavIcon
closedDoor = FavIcon $(makeRelativeToProject "assets/closed.svg" >>= embedFile)

openDoor :: FavIcon
openDoor = FavIcon $(makeRelativeToProject "assets/open.svg" >>= embedFile)

newtype FavIcon = FavIcon ByteString

instance ToContent FavIcon where
  toContent (FavIcon bs) =
    ContentBuilder (fromByteString bs) (Just . fromIntegral $ BS.length bs)
instance ToTypedContent FavIcon where
  toTypedContent = TypedContent typeSvg . toContent

data Ypp
  = Ypp
  { packetMark :: PacketMark
  , routingTable :: RoutingTableId
  , vpnService :: Tagged VpnService Text
  , netLock :: MVar ()
  }

mkYesod "Ypp" [parseRoutes|
/ HomeR GET
/open.svg OpenFavIconR GET
/closed.svg ClosedFavIconR GET
/favicon.ico FaviconR GET
/off OffR POST
/on OnR POST
/confirm-restart ConfirmRestartR GET
/restart-vpn  RestartVpnR POST
|]

instance Yesod Ypp where
  makeSessionBackend _ = pure Nothing

getOpenFavIconR :: Handler FavIcon
getOpenFavIconR = pure openDoor

getClosedFavIconR :: Handler FavIcon
getClosedFavIconR = pure closedDoor

getFaviconR :: Handler FavIcon
getFaviconR = pure closedDoor

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
  cdr <- getClientAdr
  $(logInfo) $ printf "Client %s visited home page" cdr
  app <- getYesod
  isOff <- isVpnOff (app.packetMark, cdr)
  let useOrBypass = mkUseOrBypass isOff
  layout cdr $ do
    restartVpnCss
    [whamlet|
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
layout cdr body = do
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

postOffR :: HandlerFor Ypp Html
postOffR = do
  ca <- getClientAdr
  ap <- getYesod
  $(logInfo) $ printf "Client %s asked to disable VPN just for him" ca
  withMVar ap.netLock $ \() ->
    turnOffVpnFor ca ap.packetMark
  redirect HomeR

postOnR :: HandlerFor Ypp Html
postOnR = do
  ca <- getClientAdr
  ap <- getYesod
  $(logInfo) $ printf "Client %s asked to enable VPN just for him" ca
  withMVar ap.netLock $ \() ->
    turnOnVpnFor ca ap.packetMark
  redirect HomeR

postRestartVpnR :: HandlerFor Ypp Html
postRestartVpnR = do
  ca <- getClientAdr
  ap <- getYesod
  $(logInfo) $ printf "Client %s asked to restart VPN service" ca
  withMVar ap.netLock $ \() -> restartVpn ap.vpnService
  redirect HomeR
