-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Data.Maybe
import           GHC.Generics
import           Prelude
----------------------------------------------------------------------------
import           Miso hiding (defaultOptions)
import           Miso.JSON
import           Miso.String
import qualified Miso.Html.Event as E
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
import           Miso.Lens
import qualified Miso.CSS as CSS
----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
-- | Main entry point
main :: IO ()
main = startApp defaultEvents app
----------------------------------------------------------------------------
-- | Model
newtype Model = Model
  { _info :: Maybe GitHub
  } deriving (Eq, Show)
----------------------------------------------------------------------------
-- | Lens for info field
info :: Lens Model (Maybe GitHub)
info = lens _info $ \r x -> r { _info = x }
----------------------------------------------------------------------------
-- | Action
data Action
  = FetchGitHub
  | SetGitHub (Response GitHub)
  | ErrorHandler (Response MisoString)
----------------------------------------------------------------------------
app :: App Model Action
app = (component emptyModel updateModel viewModel)
#ifndef WASM
  { styles =
    [ Href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.3/css/bulma.min.css" True
    , Href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css" True
    ]
  }
#endif
----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = Model Nothing
----------------------------------------------------------------------------
updateModel :: Action -> Effect ROOT Model Action
updateModel = \case
  FetchGitHub ->
    getJSON "https://api.github.com" [] SetGitHub ErrorHandler
  SetGitHub Response {..} ->
    info ?= body
  ErrorHandler Response {..} ->
    io_ (consoleError body)
----------------------------------------------------------------------------
-- | View function, with routing
viewModel :: Model -> View Model Action
viewModel m =
  H.div_
      [ CSS.style_
        [ CSS.textAlign "center"
        , CSS.margin "200px"
        ]
      ]
      [ H.h1_
        [ P.class_ $ pack "title"
        ]
        [ "🍜 Miso Fetch API"
        ]
      , optionalAttrs
        H.button_
        [ E.onClick FetchGitHub
        , P.class_ (pack "button is-large is-outlined")
        ]
        (isJust (m ^. info))
        [ P.disabled_
        ]
        [ "Fetch JSON from https://api.github.com"
        ]
      , case m ^. info of
          Nothing ->
            H.div_
            []
            [ "No data"
            ]
          Just GitHub {..} ->
            H.table_
            [ P.class_ "table is-striped" ]
            [ H.thead_
              []
              [ H.tr_
                []
                [ H.th_
                  []
                  [ text "URLs"
                  ]
                ]
              ]
            , H.tbody_
              []
              [ tr currentUserUrl
              , tr emojisUrl
              , tr emailsUrl
              , tr eventsUrl
              , tr gistsUrl
              , tr feedsUrl
              , tr followersUrl
              , tr followingUrl
              ]
            ]
      ]
  where
    tr :: MisoString -> View Model action
    tr x = H.tr_ [] [ H.td_ [] [ text x ] ]
----------------------------------------------------------------------------
-- | Structure to capture the JSON returned from https://api.github.com
data GitHub
  = GitHub
  { currentUserUrl
  , currentUserAuthorizationsHtmlUrl
  , authorizationsUrl
  , codeSearchUrl
  , commitSearchUrl
  , emailsUrl
  , emojisUrl
  , eventsUrl
  , feedsUrl
  , followersUrl
  , followingUrl
  , gistsUrl
  , hubUrl
  , issueSearchUrl
  , issuesUrl
  , keysUrl
  , notificationsUrl
  , organizationRepositoriesUrl
  , organizationUrl
  , publicGistsUrl
  , rateLimitUrl
  , repositoryUrl
  , repositorySearchUrl
  , currentUserRepositoriesUrl
  , starredUrl
  , starredGistsUrl
  , userUrl
  , userOrganizationsUrl
  , userRepositoriesUrl
  , userSearchUrl :: MisoString
  } deriving (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON GitHub where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
----------------------------------------------------------------------------
