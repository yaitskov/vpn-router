{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module VpnRouter.Yesod where

import VpnRouter.Prelude
import Yesod.Core as Y

newtype Mime = Mime ByteString

typeWasm :: Mime
typeWasm = Mime "application/wasm"

typeJs :: Mime
typeJs = Mime typeJavascript

typeSvg :: Mime
typeSvg = Mime Y.typeSvg

sendStaticBs :: ToContent a => Mime -> a -> HandlerFor y TypedContent
sendStaticBs (Mime mime) c = pure . TypedContent mime $ toContent c

-- | Handler () - is encoded as an empty string even with header Accepted equals to "application/json"
-- meanwhile Aeson.encode () = "[]" and therefore Aeson decode expect the same
data Unit = Unit deriving (Show, Eq)

instance ToContent Unit where
  toContent _ = toContent ("[]" :: ByteString)
instance ToTypedContent Unit where
  toTypedContent  = TypedContent typeJson . toContent
