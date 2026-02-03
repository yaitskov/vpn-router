{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
module VpnRouter.Page where

import VpnRouter.Net.Types ( RoutingTableId, PacketMark )
import VpnRouter.Net
    ( getClientAdr,
      isVpnOff,
      turnOffVpnFor,
      turnOnVpnFor )
import VpnRouter.Prelude
    ( ($), Monad((>>=)), Bool(False, True), printf )
import Yesod
    ( logInfo,
      lucius,
      getYesod,
      redirect,
      mkYesod,
      setTitle,
      whamlet,
      parseRoutes,
      Html,
      Yesod(defaultLayout),
      HandlerFor,
      ToWidget(toWidget),
      RenderRoute(renderRoute) )

data Ypp
  = Ypp
  { packetMark :: PacketMark
  , routingTable :: RoutingTableId
  }


mkYesod "Ypp" [parseRoutes|
/ HomeR GET
/off OffR POST
/on OnR POST
|]

instance Yesod Ypp


getHomeR :: Handler Html -- For Ypp Html
getHomeR = do
  cdr <- getClientAdr
  $(logInfo) $ printf "Client %s visited home page" cdr
  app <- getYesod
  isVpnOff (app.packetMark, cdr) >>= \case
    True ->
      layout
        [whamlet|
                <form method=post action=@{OnR}>
                  <div class=ipaddr>#{cdr}
                  <div class=butdiv>
                    <button class=green>Turn VPN on
                |]
    False ->
      layout
        [whamlet|
                <form method=post action=@{OffR}>
                  <div class=ipaddr>#{cdr}
                  <div class=butdiv>
                    <button class=red>Turn VPN off
                |]
  where
    layout body = do
      defaultLayout $ do
        setTitle "VPN Router"
        css
        body
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
  $(logInfo) $ printf "Client %s asked to disable VPN" ca
  turnOffVpnFor ca ap.packetMark
  redirect HomeR

postOnR :: HandlerFor Ypp Html
postOnR = do
  ca <- getClientAdr
  ap <- getYesod
  $(logInfo) $ printf "Client %s asked to enable VPN" ca
  turnOnVpnFor ca ap.packetMark
  redirect HomeR
