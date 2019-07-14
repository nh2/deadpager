{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Overview where

import Import

import Network.Consul as Consul

getOverviewR :: Handler Html
getOverviewR = do
  cc <- getsYesod appConsulClient
  services <- getServices cc Nothing Nothing
  srs <- fmap (concat . catMaybes) $ forM services $ \s -> getService cc s Nothing Nothing
  defaultLayout $ do
    setTitle "Overview"
    $(widgetFile "overview")
