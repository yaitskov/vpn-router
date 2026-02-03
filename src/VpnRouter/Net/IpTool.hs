{-# OPTIONS_GHC -freduction-depth=0 #-} -- workaround for printf
module VpnRouter.Net.IpTool where

import Data.Conduit.Process ( sourceCmdWithConsumer )
import System.Exit ( ExitCode(ExitFailure, ExitSuccess) )
import Text.Regex.TDFA ( AllTextSubmatches(getAllTextSubmatches), (=~) )
import VpnRouter.Prelude
import VpnRouter.Net.Types
    ( RoutingTableId(..),
      PacketMark(..),
      RuleId(..),
      HostIp,
      Gateway,
      IspNic,
      pipeline,
      parseIpV4,
      hostIpToDec )
import VpnRouter.App ( NetM, ex )
import VpnRouter.Bash ( bash )

ip :: IsString s => s
ip = "ip"

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
    bashCmd = printf "%s rule list table %s" (ip :: Text) rt

addDefaultRouteToRoutingTable :: NetM m => RoutingTableId -> Tagged IspNic Text -> Tagged Gateway HostIp -> m ()
addDefaultRouteToRoutingTable rt (Tagged isp) (Tagged gw) =
  bash ip [ "route", "add", "default"
          , "via", hostIpToDec gw
          , "dev", isp
          , "table", printf "%s" rt
          ]

flushRouteCache :: NetM m =>  m ()
flushRouteCache = bash ip [ "route", "flush", "cache" ]

unmarkRoutingTable :: NetM m => RoutingTableId -> PacketMark -> m ()
unmarkRoutingTable rt pm = do
  mapM_ unmark =<< (reverse . sort . fmap fst . filter ((pm ==) . snd) <$> routingTableMarks rt)
  where
    unmark (RuleId rid) = bash ip ["rule", "del", "pref", show rid]

deleteDefaultRoute :: NetM m => RoutingTableId ->  m ()
deleteDefaultRoute rt =
  bash ip [ "route", "del", "default", "table", printf "%s" rt]

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
    bashCmd = (ip :: String) <> " route show table all"

addMarkToRoutingTable :: NetM m => RoutingTableId -> PacketMark -> m ()
addMarkToRoutingTable rt (PacketMark pm) =
  bash ip ["rule", "add", "fwmark", show pm, "table", printf "%s" rt]
