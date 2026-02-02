{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
module VpnRouter.Net where

import Debug.TraceEmbrace hiding (a)
import Conduit ( (.|), mapC, ConduitT)
import Data.Conduit.Combinators (concatMap, sinkList)
import Data.Conduit.Process ( sourceCmdWithConsumer )
import Data.Conduit.Text ( lines )
import Data.Text (intercalate)
import Network.Socket ( SockAddr(SockAddrInet), hostAddressToTuple, tupleToHostAddress, HostAddress )
import Network.Wai ( Request(remoteHost) )
import System.Exit ( ExitCode(ExitFailure, ExitSuccess) )
import Text.Blaze ( text, ToMarkup(toMarkup) )
import Text.Regex.TDFA ( AllTextSubmatches(getAllTextSubmatches), (=~) )
import Text.Show qualified as TS
import VpnRouter.Prelude hiding (decodeUtf8, lines, concatMap)
import Yesod ( HandlerFor, waiRequest )
import UnliftIO.Process ( callProcess )
import UnliftIO.Exception ( stringException, throwIO )

newtype ClientAdr = ClientAdr HostIp deriving (Eq, Ord)

clientAdrToDec4 :: ClientAdr -> Text
clientAdrToDec4 (ClientAdr sa) = hostIpToDec sa
instance Show ClientAdr where
  show = show . clientAdrToDec4
instance FormatType 's' ClientAdr where
  formatArg pt v ff = formatArg pt (clientAdrToDec4 v) ff

instance ToMarkup ClientAdr where
  toMarkup = text . clientAdrToDec4

getClientAdr :: HandlerFor a ClientAdr
getClientAdr =
  waiRequest >>= \r ->
    case remoteHost r of
      SockAddrInet _port hip -> pure . ClientAdr $ HostIp hip
      _ -> throwIO $ stringException "Unsupported socket addr"

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

mainRoutingTableName :: RoutingTableId
mainRoutingTableName = RoutingTableName "main"
{-
delete rule by id from routing table 3
ip rule del pref 32765 table 3
-}
{-
list rules in routing table 3
ip rule list table 3
32765:   from all fwmark 0x2 lookup 3
-}
-- 32765:	from all fwmark 0x2 lookup 7
ruleListLinePat :: String
ruleListLinePat = "^([0-9]+):[[:space:]]+from[[:space:]]+all[[:space:]]+fwmark[[:space:]]+(0x[0-9a-fA-F]+)[[:space:]]+lookup"

type NetM m = (HasCallStack, MonadIO m)

ex :: NetM m => String -> m a
ex em = throwIO $ stringException em

pipeline :: (Monad m) => (Text -> Maybe a) -> ConduitT ByteString c m [a]
pipeline f  = mapC (fromRight "" .  decodeUtf8') .| lines .| concatMap f .| sinkList

parseRoutingTableMarkLine :: Text -> Maybe (RuleId, PacketMark)
parseRoutingTableMarkLine l =
  case getAllTextSubmatches (l =~ ruleListLinePat) of
    ([_full, riGroup, pmGroup] :: [Text]) ->
      case readEither $ toString riGroup of
       Left e -> error . toText @String $ printf "failed to parse rule id in: %s due %s" l e
       Right ri ->
         case readEither $ toString pmGroup of
           Left e -> error . toText @String $ printf "failed to parse packet mark in: %s due %s" l e
           Right pm -> pure (RuleId ri, PacketMark pm)
    _ -> Nothing

routingTableMarks :: NetM m  => RoutingTableId -> m [(RuleId, PacketMark)]
routingTableMarks rt = do
  (ec, l) <- sourceCmdWithConsumer bashCmd (pipeline parseRoutingTableMarkLine)
  case ec of
    ExitSuccess -> pure l
    ExitFailure erc -> ex $ printf "Failed to list ip rules for routingi table %s; error code %d" rt erc
  where
    bashCmd = printf "ip rule list table %s" rt

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

newtype LineNumber = LineNumber Int deriving newtype (Eq, Ord, Show, Read)

{-
iptables -t mangle -L PREROUTING -n
Chain PREROUTING (policy ACCEPT)
num target     prot opt source               destination
1   MARK       all  --  192.168.11.14        0.0.0.0/0            MARK set 0x2
-}
mangleLinePattern :: String
mangleLinePattern =
  "^([[:digit:]]+)" <> -- "num" column
  "[[:space:]]+MARK" <> -- "target" column
  "[[:space:]]+[^[:space:]]+" <> -- skip "prot" column
  "[[:space:]]+[^[:space:]]+" <> -- skip "opt" column
  "[[:space:]]+([[:digit:].]+)" <> -- source column
  "[[:space:]]+[^[:space:]]+" <> -- skip "destination" column
  "[[:space:]]+MARK set (0x[[:digit:]]+)" -- extra column

parseIptablesLine :: Text -> Maybe (LineNumber, PacketMark, ClientAdr)
parseIptablesLine l =
  case getAllTextSubmatches (l =~ mangleLinePattern) of
    ([_full, lnGr, sourceIp, pmGroup] :: [Text]) ->
      case readEither $ toString lnGr of
       Left e -> error . toText $ printf "failed to parse line number (%s) in: %s due %s" lnGr l e
       Right lineNum ->
         case parseIpV4 $ toString sourceIp of
           Nothing -> error . toText @String $ printf "failed to parse client IP in: %s" l
           Just clientSourceIp ->
             case readEither $ toString pmGroup of
               Left e -> error . toText @String $ printf "failed to parse packet mark in: %s due %s" l e
               Right pm -> pure ( lineNum
                                , PacketMark pm
                                , ClientAdr clientSourceIp
                                )
    _ -> Nothing

listMarkedSources :: NetM m => m [(LineNumber, PacketMark, ClientAdr)]
listMarkedSources = do
  (ec, l) <- sourceCmdWithConsumer bashCmd (pipeline parseIptablesLine)
  case ec of
    ExitSuccess -> pure l
    ExitFailure erc -> ex $ printf "Failed to list marking rules; due %d" erc
  where
    bashCmd = "iptables -t mangle -L PREROUTING -n --line-numbers"

isVpnOff :: NetM m => (PacketMark, ClientAdr) -> m Bool
isVpnOff pmca = do
  markedSources <- $(tw "/pmca") <$> listMarkedSources
  pure $ (pmca `elem` (projPmCa <$> markedSources))
  where
    projPmCa (_, pm, ca) = (pm, ca)

addMarkToRoutingTable :: NetM m => RoutingTableId -> PacketMark -> m ()
addMarkToRoutingTable rt (PacketMark pm) =
  bash "ip" ["rule", "add", "fwmark", show pm, "table", printf "%s" rt]

data IspNic
data Gateway

newtype HostIp = HostIp HostAddress deriving newtype (Eq, Ord)
instance Show HostIp where
  show = toString . hostIpToDec

instance IsString HostIp where
  fromString s =
    fromMaybe (error . toText @String $ printf "Failed to parse [%s] as IPv4 address" s) $ parseIpV4 s

hostIpToDec :: HostIp -> Text
hostIpToDec (HostIp hip) =
  case hostAddressToTuple hip of
    (a, b, c, d) -> intercalate "." $ fmap show [a, b, c, d]

addDefaultRouteToRoutingTable :: NetM m => RoutingTableId -> Tagged IspNic Text -> Tagged Gateway HostIp -> m ()
addDefaultRouteToRoutingTable rt (Tagged isp) (Tagged gw) =
  bash "ip" [ "route", "add", "default"
            , "via", hostIpToDec gw
            , "dev", isp
            , "table", printf "%s" rt
            ]

flushRouteCache :: NetM m =>  m ()
flushRouteCache = callProcess "ip" [ "route", "flush", "cache" ]

manualInit :: NetM m => RoutingTableId -> PacketMark -> Tagged IspNic Text -> Tagged Gateway HostIp -> m ()
manualInit rt pm isp gw = do
  addMarkToRoutingTable rt pm
  addDefaultRouteToRoutingTable rt isp gw
  flushRouteCache

clearMarkingLines :: NetM m => PacketMark -> m ()
clearMarkingLines pm = do
  mapM_ rmMarkingRule =<< fmap oneOf3 . reverse . sort . filter matchPm . $(tw "/pm" ) <$> listMarkedSources
  where
    matchPm (_, lpm, _) = lpm == pm

deleteDefaultRoute :: NetM m => RoutingTableId ->  m ()
deleteDefaultRoute rt =
  callProcess "ip" [ "route", "del", "default", "table", printf "%s" rt]

-- default via 192.168.1.1 dev wlp2s0 table 3
defaultRoutePat :: String
defaultRoutePat =
  "^default[[:space:]]+" <>
  "via[[:space:]]+" <>
  "([[:digit:].]+)[[:space:]]+" <>  -- gateway
  "dev[[:space:]]+" <>
  "([^[:space:]]+)[[:space:]]+" <> -- nic
  "(table[[:space:]]+" <>
  "([[:digit:]]+))?" -- table id is not printed for the main table

parseDefaultRoutingLine :: RoutingTableId -> Text -> Maybe (Tagged Gateway HostIp, Tagged IspNic Text)
parseDefaultRoutingLine rt l =
  case getAllTextSubmatches (l =~ defaultRoutePat) of
    ([_full, gwGr, nicGr, "", ""] :: [Text]) | rt == mainRoutingTableName ->
      case parseIpV4 $ toString gwGr of
        Nothing -> Nothing
        Just gw -> pure (Tagged @Gateway gw, Tagged @IspNic nicGr)
    ([_full, gwGr, nicGr, _rt, rtGr] :: [Text]) ->
      case readMaybe $ toString rtGr of
        Nothing -> Nothing
        Just lrt | RoutingTableId lrt == rt ->
          case parseIpV4 $ toString gwGr of
            Nothing -> Nothing
            Just gw -> pure (Tagged @Gateway gw, Tagged @IspNic nicGr)
                 | otherwise -> Nothing
    _ -> Nothing

listDefaultsOfRoutingTable :: NetM m  => RoutingTableId -> m [(Tagged Gateway HostIp, Tagged IspNic Text)]
listDefaultsOfRoutingTable rt = do
  (ec, l) <- sourceCmdWithConsumer bashCmd (pipeline (parseDefaultRoutingLine rt))
  case ec of
    ExitSuccess -> pure l
    ExitFailure erc -> ex $ printf "Failed to list ip routes for routing table %s; error code %d" rt erc
  where
    bashCmd = "ip route show table all"

clearDefaultRoute :: NetM m => RoutingTableId -> m ()
clearDefaultRoute rt =
  mapM_ (\(_, _) -> deleteDefaultRoute rt) . $(tw "/rt") =<< listDefaultsOfRoutingTable rt

unmarkRoutingTable :: NetM m => RoutingTableId -> PacketMark -> m ()
unmarkRoutingTable rt pm = do
  mapM_ unmark =<< (reverse . sort . fmap fst . filter ((pm ==) . snd) <$> routingTableMarks rt)
  where
    unmark (RuleId rid) =
      callProcess "ip" ["rule", "del", "pref", show rid]

cleanup :: NetM m => RoutingTableId -> PacketMark -> m ()
cleanup rt pm = do
  clearMarkingLines pm
  clearDefaultRoute rt
  unmarkRoutingTable rt pm
  flushRouteCache

oneOf3 :: (a, b, c) -> a
oneOf3 (a, _, _) = a

findMarkingLine :: NetM m => ClientAdr -> PacketMark -> m (Maybe LineNumber)
findMarkingLine ca pm = fmap oneOf3 . find lineP <$> listMarkedSources
  where
    lineP (_, lpm, lca) = pm == lpm && ca == lca

bash :: NetM m => Text -> [Text] -> m()
bash cmd args = liftIO $(trIo "/cmd args") >> callProcess (toString cmd) (toString <$> args)

rmMarkingRule  :: NetM m => LineNumber -> m ()
rmMarkingRule ln =
  bash "iptables" [ "-t", "mangle", "-D", "PREROUTING", show ln]

addMarkingRule :: NetM m => ClientAdr -> PacketMark -> m ()
addMarkingRule ca (PacketMark pm) =
  bash "iptables" [ "-t", "mangle", "-I", "PREROUTING"
                  , "-s", clientAdrToDec4 ca
                  , "-j", "MARK"
                  , "--set-mark", show pm
                  ]

turnOffVpnFor :: NetM m => ClientAdr -> PacketMark -> m ()
turnOffVpnFor ca pm =
  findMarkingLine ca pm >>= \case
    Just _ -> pure () --
    Nothing ->
      addMarkingRule ca pm

turnOnVpnFor :: NetM m => ClientAdr -> PacketMark -> m ()
turnOnVpnFor ca pm =
  findMarkingLine ca pm >>= mapM_ rmMarkingRule
