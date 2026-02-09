module VpnRouter.Net where

import Network.Socket ( SockAddr(SockAddrInet) )
import Network.Wai ( Request(remoteHost) )
import VpnRouter.App ( NetM )
import VpnRouter.Bash ( bash )
import VpnRouter.Net.Types
    ( RoutingTableId,
      PacketMark,
      VpnService,
      ClientAdr(..),
      HostIp(..),
      LineNumber,
      Gateway,
      IspNic )
import VpnRouter.Net.Iptables
    ( listMarkedSources, rmMarkingRule, addMarkingRule )
import VpnRouter.Net.IpTool
    ( addDefaultRouteToRoutingTable,
      flushRouteCache,
      unmarkRoutingTable,
      deleteDefaultRoute,
      listDefaultsOfRoutingTable,
      addMarkToRoutingTable )
import VpnRouter.Prelude
import Yesod.Core ( HandlerFor, waiRequest )
import UnliftIO.Exception ( stringException, throwIO )

isVpnOff :: NetM m => (PacketMark, ClientAdr) -> m Bool
isVpnOff pmca = do
  markedSources <- $(tw "/pmca") <$> listMarkedSources
  pure (pmca `elem` (projPmCa <$> markedSources))
  where
    projPmCa (_, pm, ca) = (pm, ca)

getClientAdr :: HandlerFor a ClientAdr
getClientAdr =
  waiRequest >>= \r ->
    case remoteHost r of
      SockAddrInet _port hip -> pure . ClientAdr $ HostIp hip
      _ -> throwIO $ stringException "Unsupported socket addr"

manualInit :: NetM m => RoutingTableId -> PacketMark -> Tagged IspNic Text -> Tagged Gateway HostIp -> m ()
manualInit rt pm isp gw = do
  addMarkToRoutingTable rt pm
  addDefaultRouteToRoutingTable rt isp gw
  flushRouteCache

{- HLINT ignore "Avoid reverse" -}
clearMarkingLines :: NetM m => PacketMark -> m ()
clearMarkingLines pm = do
  mapM_ rmMarkingRule . fmap oneOf3 . reverse . sort . filter matchPm . $(tw "/pm" ) =<< listMarkedSources
  where
    matchPm (_, lpm, _) = lpm == pm

clearDefaultRoute :: NetM m => RoutingTableId -> m ()
clearDefaultRoute rt =
  mapM_ (\(_, _) -> deleteDefaultRoute rt) . $(tw "/rt") =<< listDefaultsOfRoutingTable rt

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

turnOffVpnFor :: NetM m => ClientAdr -> PacketMark -> m ()
turnOffVpnFor ca pm =
  findMarkingLine ca pm >>= \case
    Just _ -> pure () --
    Nothing ->
      addMarkingRule ca pm

turnOnVpnFor :: NetM m => ClientAdr -> PacketMark -> m ()
turnOnVpnFor ca pm =
  findMarkingLine ca pm >>= mapM_ rmMarkingRule

-- systemctl restart "AmneziaVPN.service"
-- systemctl status "AmneziaVPN.service"
-- amnezia service runs as root
-- lets try polkit
systemctl :: Text
systemctl = "systemctl"

restartVpn :: NetM m => Tagged VpnService Text -> m ()
restartVpn ser =
  bash systemctl ["restart", untag ser]
