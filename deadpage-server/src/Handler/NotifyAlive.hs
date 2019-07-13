module Handler.NotifyAlive where

import Import

import Network.Consul as Consul

postNotifyAliveR :: CheckName -> Handler ()
postNotifyAliveR cn = do
  cc <- getsYesod appConsulClient
  passHealthCheck cc cn Nothing
