{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.NotifyAlive where

import Import

import qualified Network.Consul as Consul

data NotifyAlive =
  NotifyAlive
    { notifyAliveAccessKey :: AccessKey
    , notifyAliveTtl :: Maybe Text
    }

notifyAliveForm :: FormInput Handler NotifyAlive
notifyAliveForm = NotifyAlive <$> ireq accessKeyField "key" <*> iopt textField "ttl"

postNotifyAliveR :: CheckName -> Handler ()
postNotifyAliveR cn = do
  NotifyAlive {..} <- runInputPost notifyAliveForm
  pus <- getsYesod appPreconfiguredUsers
  case find ((notifyAliveAccessKey `elem`) . preconfiguredUserAccessKeys) pus of
    Nothing -> permissionDenied "Invalid access key"
    Just pu -> do
      void $ runDB $ insertUnique Check {checkUser = preconfiguredUserName pu, checkName = cn}
      cc <- getsYesod appConsulClient
      let rs =
            Consul.RegisterService
              { Consul.rsId = Nothing
              , Consul.rsName = cn
              , Consul.rsTags = []
              , Consul.rsPort = Nothing
              , Consul.rsCheck = Just $ Consul.Ttl $ fromMaybe "10s" notifyAliveTtl
              }
      b <- Consul.registerService cc rs Nothing
      unless b $ error "registerService returned False"
      Consul.passHealthCheck cc ("service:" <> cn) Nothing
