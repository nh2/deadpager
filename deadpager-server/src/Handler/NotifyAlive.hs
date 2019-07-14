{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.NotifyAlive where

import Import

import Network.Consul as Consul

data NotifyAlive =
  NotifyAlive
    { notifyAliveTtl :: Maybe Text
    }

notifyAliveForm :: FormInput Handler NotifyAlive
notifyAliveForm = NotifyAlive <$> iopt textField "ttl"

postNotifyAliveR :: CheckName -> Handler ()
postNotifyAliveR cn = do
  cc <- getsYesod appConsulClient
  NotifyAlive {..} <- runInputPost notifyAliveForm
  let rs =
        RegisterService
          { rsId = Nothing
          , rsName = cn
          , rsTags = []
          , rsPort = Nothing
          , rsCheck = Just $ Ttl $ fromMaybe "10s" notifyAliveTtl
          }
  b <- registerService cc rs Nothing
  unless b $ error "registerService returned False"
  passHealthCheck cc ("service:" <> cn) Nothing
