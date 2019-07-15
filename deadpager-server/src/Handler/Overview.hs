{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Overview where

import Import

import Network.Consul as Consul

getOverviewR :: Handler Html
getOverviewR = do
  Entity _ User {..} <- requireAuth
  checks <- runDB $ map entityVal <$> selectList [CheckUser ==. userIdent] []
  cc <- getsYesod appConsulClient
  srs <- fmap (concat . catMaybes) $ forM checks $ \c -> getService cc (checkName c) Nothing Nothing
  defaultLayout $ do
    setTitle "Overview"
    $(widgetFile "overview")
