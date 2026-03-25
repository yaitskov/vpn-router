{-# LANGUAGE CPP               #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE MultilineStrings  #-}

module Main where
import Data.Maybe ( maybeToList )
import Miso
import Miso.Html.Element (div_, button_)
import Miso.Html.Element qualified as H
import Miso.Html.Event qualified as E
import Miso.Html.Event (onClick)
import Miso.Html.Property (class_)
import Miso.Html.Property qualified as P
import Miso.Lens ( Lens, lens, (?=), (^.) )
import Prelude

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
  , styles =
    [ Style """
        .github-link {
            position: fixed;
            padding: 4vh;
            right: 0vh;
        }
        .github-link img {
            width: 5vh;
            opacity: 0.4;
        }

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
        button:focus {
          outline-offset: 2vh;
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

      """
    ]
  }

emptyModel :: Model
emptyModel = Model Nothing False Nothing

updateModel :: Action -> Effect ROOT Model Action
updateModel = \case
  SetClientIpAddr Response {..} ->
    modify (\x -> x { clientIp = Just body })
  AskForRestart ->
    modify (\x -> x { restartConfirmationDialog = True })
  VpnRestarted _ ->
    modify (\x -> x { restartConfirmationDialog = False })
  ConfirmVpnRestart ->
    postJSON "/restart-vpn" () [] VpnRestarted ErrorHandler
  GetVpnBypassStatus -> do
    getJSON "/vpn-bypass-status" [] UpdateVpnBypassStatus ErrorHandler
    getJSON "/client-ip" [] SetClientIpAddr ErrorHandler
  UpdateVpnBypassStatus Response {..} ->
    if body then
      info ?= VpnBypassOn
    else
      info ?= VpnBypassOff
  ToggleVpnStatus -> do
    st <- get
    case st ^. info of
      Nothing -> io_ (consoleError "Vpn status is not known")
      Just VpnBypassOn ->
        postJSON' "on" () [] UpdateVpnBypassStatus ErrorHandler
      Just VpnBypassOff ->
        postJSON' "off" () [] UpdateVpnBypassStatus ErrorHandler

  ErrorHandler Response {..} ->
    io_ (consoleError body)

viewModel :: Model -> View Model Action
viewModel m = div_ [] $ [ header ] <> pages
  where
    header =
      H.div_ [ P.class_ "github-link" ]
        [ H.a_ [ P.href_ "https://github.com/yaitskov/vpn-router"
               , P.alt_  "Link to VpnRouter project"
               ]
               [ H.img_ [ P.src_ "/github.svg" ] ]
        ]
    clientIpDiv = div_ [ class_ "ipaddr" ] $ maybeToList (text <$> clientIp m)
    pages =
      if restartConfirmationDialog m then
        restart
      else
        home
    restart =
      [ clientIpDiv
      , div_
          [ class_ "butdiv" ]
          [ button_
              [ class_ "red", onClick ConfirmVpnRestart ]
              [ "RESTART VPN" ]
          ]
      ]
    home =
      [ clientIpDiv
      , div_
          [ class_ "butdiv" ]
          [ case m ^. info of
              Nothing -> text "Loading ..."
              Just VpnBypassOn ->
                button_
                [ class_ "green autofocus"
                , E.onClick ToggleVpnStatus
                ]
                [ "Use VPN" ]
              Just VpnBypassOff ->
                button_
                [ class_ "red autofocus"
                , E.onClick ToggleVpnStatus ]
                [ "Bypass VPN" ]
          ]
      ]
