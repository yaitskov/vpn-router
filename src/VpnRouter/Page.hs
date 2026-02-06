{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
module VpnRouter.Page where

import UnliftIO.MVar ( MVar, withMVar )
import VpnRouter.Net.Types ( RoutingTableId, PacketMark, VpnService )
import VpnRouter.Net
    ( getClientAdr,
      isVpnOff,
      restartVpn,
      turnOffVpnFor,
      turnOnVpnFor )
import VpnRouter.Prelude
    ( ($),
      Tagged,
      Text,
      Monad((>>=)),
      Applicative(pure),
      Bool(False, True),
      printf )

import Yesod.Core


data Ypp
  = Ypp
  { packetMark :: PacketMark
  , routingTable :: RoutingTableId
  , vpnService :: Tagged VpnService Text
  , netLock :: MVar ()
  }


mkYesod "Ypp" [parseRoutes|
/ HomeR GET
/off OffR POST
/on OnR POST
/restart-vpn  RestartVpnR POST
|]

instance Yesod Ypp


getHomeR :: Handler Html -- For Ypp Html
getHomeR = do
  cdr <- getClientAdr
  $(logInfo) $ printf "Client %s visited home page" cdr
  app <- getYesod
  useOrBypass <- mkUseOrBypass app.packetMark cdr
  layout
    [whamlet|
            <div class=ipaddr>#{cdr}
            <div class=restart-vpn>
              <form method=post action=@{RestartVpnR}>
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
    mkUseOrBypass pm cdr =
      isVpnOff (pm, cdr) >>= \case
        True -> pure useVpn
        False -> pure bypassVpn
    layout body = do
      defaultLayout $ do
        setTitle "VPN Router"
        toWidget
          [julius|
            document.addEventListener("visibilitychange", (event) => {
              if (document.visibilityState == "visible") {
                window.location.reload();
              }
            });
          |]
        css
        restartVpnCss
        body
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
  $(logInfo) $ printf "Client %s asked to disable VPN for him" ca
  withMVar ap.netLock $ \() ->
    turnOffVpnFor ca ap.packetMark
  redirect HomeR

postOnR :: HandlerFor Ypp Html
postOnR = do
  ca <- getClientAdr
  ap <- getYesod
  $(logInfo) $ printf "Client %s asked to enable VPN for him" ca
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
