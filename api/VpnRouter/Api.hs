module VpnRouter.Api where

import Relude ( Bool, Text )
import Servant.API
    ( type (:<|>), RemoteHost, JSON, Get, type (:>), Post )

type VpnBypassStatusApi = "vpn-bypass-status" :> RemoteHost :> Get '[JSON] Bool
type ClientIpApi = "client-ip" :> RemoteHost :> Get '[JSON] Text
type OffApi = "off" :> RemoteHost :> Post '[JSON] Bool
type OnApi = "on" :> RemoteHost :> Post '[JSON] Bool
type RestartVpnApi = "restart-vpn" :> RemoteHost :> Post '[JSON] ()


type VpnRouterAjaxApi
  =    VpnBypassStatusApi
  :<|> ClientIpApi
  :<|> OffApi
  :<|> OnApi
  :<|> RestartVpnApi
