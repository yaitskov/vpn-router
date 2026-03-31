{-# LANGUAGE CPP               #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE MultilineStrings  #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where
import CssClassBindings qualified as TC
import CssStyle
    ( style,
      butdiv,
      githubLink,
      green,
      ipaddr,
      red,
      restartVpn )
import Miso
import Miso.FFI.QQ (js)
import Miso.Html.Element (div_, button_)
import Miso.Html.Element qualified as H
import Miso.Html.Event qualified as E
import Miso.Html.Event (onClick)
import Miso.Html.Property qualified as P
import Miso.Lens ( Lens, lens, (?=), (^.) )
import Relude
import Servant.API ( type (:<|>)((:<|>)) )
import Servant.Miso.Client ( toClient )
import VpnRouter.Api ( VpnRouterAjaxApi )


api :: Proxy VpnRouterAjaxApi
api = Proxy

type ApiSig r = (Response r -> IO ()) -> (Response MisoString -> IO ()) -> IO ()

vpnBypassStatus :: ApiSig Bool
getClientIp :: ApiSig Text
offBypass :: ApiSig Bool
onBypass :: ApiSig Bool
doRestartVpn :: ApiSig ()
vpnBypassStatus :<|> getClientIp :<|> offBypass :<|> onBypass :<|> doRestartVpn = toClient mempty api

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = startApp defaultEvents app

data VpnBypassStatus
  = VpnBypassOn
  | VpnBypassOff
  deriving (Show, Eq)

data Model = Model
  { _info :: Maybe VpnBypassStatus
  , restartConfirmationDialog :: Bool
  , clientIp :: Maybe MisoString
  } deriving (Eq, Show)

info :: Lens Model (Maybe VpnBypassStatus)
info = lens _info $ \r x -> r { _info = x }

data Action
  = GetVpnBypassStatus
  | UpdateVpnBypassStatus (Response Bool)
  | ToggleVpnStatus
  | SetClientIpAddr (Response MisoString)
  | AskForRestart
  | ConfirmVpnRestart
  | VpnRestarted (Response ())
  | ErrorHandler (Response MisoString)

app :: App Model Action
app = (component emptyModel updateModel viewModel)
  { mount = Just GetVpnBypassStatus
  , scripts =
    [ Script """
        document.addEventListener("visibilitychange", (event) => {
          if (document.visibilityState == "visible") {
            window.location.reload();
          }
        });
      """
    ]
  , styles = [ Style style ]
  }

emptyModel :: Model
emptyModel = Model Nothing False Nothing

sink0 :: ((a1 -> IO ()) -> (a2 -> IO ()) -> IO ()) -> (a1 -> b) -> (a2 -> b) -> Effect parent model b
sink0 c ok er = withSink $ \sink -> c (sink . ok) (sink . er)

updateModel :: Action -> Effect ROOT Model Action
updateModel = \case
  SetClientIpAddr Response {..} ->
    modify (\x -> x { clientIp = Just body })
  AskForRestart ->
    modify (\x -> x { restartConfirmationDialog = True })
  VpnRestarted _ ->
    modify (\x -> x { restartConfirmationDialog = False })
  ConfirmVpnRestart ->
    sink0 doRestartVpn VpnRestarted ErrorHandler
  GetVpnBypassStatus -> do
    sink0 vpnBypassStatus UpdateVpnBypassStatus ErrorHandler
    sink0 getClientIp (SetClientIpAddr . fmap ms) ErrorHandler
  UpdateVpnBypassStatus Response {..} ->
    if body then do
      info ?= VpnBypassOn
      io_ [js|
             document.title = "On - VPN bypass";
             document.querySelector('link').setAttribute("href", "open.svg");
             |]
    else do
      info ?= VpnBypassOff
      io_ [js|
             document.title = "Off - VPN bypass";
             document.querySelector('link').setAttribute("href", "closed.svg");
             |]
  ToggleVpnStatus -> do
    st <- get
    case st ^. info of
      Nothing -> io_ (consoleError "Vpn status is not known")
      Just VpnBypassOn ->
        sink0 onBypass UpdateVpnBypassStatus ErrorHandler
      Just VpnBypassOff ->
        sink0 offBypass UpdateVpnBypassStatus ErrorHandler

  ErrorHandler Response {..} ->
    io_ (consoleError body)

class_ :: TC.CssClass MisoString -> Attribute action
class_ = P.class_ . TC.class_

viewModel :: Model -> View Model Action
viewModel m = div_ [] $ [ header ] <> pages
  where
    header =
      H.div_ [ class_ githubLink ]
        [ H.a_ [ P.href_ "https://github.com/yaitskov/vpn-router"
               , P.alt_  "Link to VpnRouter project"
               ]
               [ H.img_ [ P.src_ "/github.svg" ] ]
        ]
    clientIpDiv = div_ [ class_ ipaddr ] $ maybeToList (text <$> clientIp m)
    pages =
      if restartConfirmationDialog m then
        restart
      else
        home
    restart =
      [ clientIpDiv
      , div_
          [ class_ butdiv ]
          [ button_
              [ class_ red, onClick ConfirmVpnRestart ]
              [ "RESTART VPN" ]
          ]
      ]
    home =
      [ clientIpDiv
       , div_
         [ class_ restartVpn ]
         [ button_
           [ P.title_ "restart VPN", onClick AskForRestart ]
           [ "↻" ]
         ]
      , div_
          [ class_ butdiv ]
          [ case m ^. info of
              Nothing -> text "Loading ..."
              Just VpnBypassOn ->
                button_
                [ class_ green
                , P.autofocus_ True
                , E.onClick ToggleVpnStatus
                ]
                [ "Use VPN" ]
              Just VpnBypassOff ->
                button_
                [ class_ red
                , P.autofocus_ True
                , E.onClick ToggleVpnStatus ]
                [ "Bypass VPN" ]
          ]
      ]
