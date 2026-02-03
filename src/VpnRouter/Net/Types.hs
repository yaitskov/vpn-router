module VpnRouter.Net.Types where

import Conduit ( (.|), mapC, ConduitT)
import Data.Conduit.Combinators (concatMap, sinkList)
import Data.Conduit.Text ( lines )
import Data.Text (intercalate)
import Network.Socket (hostAddressToTuple, tupleToHostAddress, HostAddress)
import Text.Blaze ( text, ToMarkup(toMarkup) )
import Text.Regex.TDFA ( AllTextSubmatches(getAllTextSubmatches), (=~) )
import Text.Show qualified as TS
import VpnRouter.Prelude hiding (decodeUtf8, lines, concatMap)

data IspNic
data Gateway

newtype LineNumber = LineNumber Int deriving newtype (Eq, Ord, Show, Read)

newtype HostIp = HostIp HostAddress deriving newtype (Eq, Ord)
instance Show HostIp where
  show = toString . hostIpToDec
instance IsString HostIp where
  fromString s =
    fromMaybe (error . toText @String $ printf "Failed to parse [%s] as IPv4 address" s) $ parseIpV4 s

newtype ClientAdr = ClientAdr HostIp deriving (Eq, Ord)

instance Show ClientAdr where
  show = show . clientAdrToDec4
instance FormatType 's' ClientAdr where
  formatArg pt v ff = formatArg pt (clientAdrToDec4 v) ff

instance ToMarkup ClientAdr where
  toMarkup = text . clientAdrToDec4

clientAdrToDec4 :: ClientAdr -> Text
clientAdrToDec4 (ClientAdr sa) = hostIpToDec sa


newtype RuleId = RuleId Int deriving (Show, Eq, Ord, FormatType 'd')
newtype PacketMark = PacketMark Int deriving (Show, Eq, Ord, FormatType 'd')

data RoutingTableId
  = RoutingTableId Int
  | RoutingTableName String
  deriving (Show, Eq, Ord)

instance FormatType 's' RoutingTableId where
   formatArg pt v ff = formatArg pt vs ff
     where
       vs :: Text
       vs = case v of
         RoutingTableId i -> show i
         RoutingTableName n -> show n

pipeline :: (Monad m) => (Text -> Maybe a) -> ConduitT ByteString c m [a]
pipeline f  = mapC (fromRight "" .  decodeUtf8') .| lines .| concatMap f .| sinkList

parseIpV4 :: String -> Maybe HostIp
parseIpV4 s =
  case getAllTextSubmatches (s =~ ipPat) of
    [_full, a, b, c, d] ->
      case readEither a of
        Left _ -> Nothing
        Right ai ->
          case readEither b of
            Left _ -> Nothing
            Right bi ->
              case readEither c of
                Left _ -> Nothing
                Right ci ->
                  case readEither d of
                    Left _ -> Nothing
                    Right di ->
                      pure . HostIp $ tupleToHostAddress (ai, bi, ci, di)
    _ -> Nothing
  where
    ipPat :: String = "([[:digit:]]+)[.]([[:digit:]]+)[.]([[:digit:]]+)[.]([[:digit:]]+)"

hostIpToDec :: HostIp -> Text
hostIpToDec (HostIp hip) =
  case hostAddressToTuple hip of
    (a, b, c, d) -> intercalate "." $ fmap show [a, b, c, d]
