{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module OptParse.Types where

import Import.NoFoundation

import Data.Yaml

data Settings =
  Settings
    { setHost :: Text
    , setPort :: Int
    , setDBFile :: Path Abs File
    , setGoogleAnalyticsTracking :: Maybe Text
    , setGoogleSearchConsoleVerification :: Maybe Text
    , setAllowUserCreation :: Bool
    , setPreconfiguredUsers :: [PreconfiguredUser]
    }
  deriving (Show, Eq, Generic)

data Flags =
  Flags
    { flagConfigFile :: Maybe FilePath
    }
  deriving (Show, Eq, Generic)

data Configuration =
  Configuration
    { confHost :: Maybe Text
    , confPort :: Maybe Int
    , confDBFile :: Maybe FilePath
    , confGoogleAnalyticsTracking :: Maybe Text
    , confGoogleSearchConsoleVerification :: Maybe Text
    , confAllowUserCreation :: Maybe Bool
    , confPreconfiguredUsers :: Maybe [PreconfiguredUser]
    }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON =
    withObject "Configuration" $ \o ->
      Configuration <$> o .:? "host" <*> o .:? "port" <*> o .:? "database-file" <*>
      o .:? "google-analytics-tracking" <*>
      o .:? "search-console-verification" <*>
      o .:? "allow-user-creation" <*>
      o .:? "users"

data PreconfiguredUser =
  PreconfiguredUser
    { preconfiguredUserName :: Text
    , preconfiguredUserPasswordHash :: HashedPass
    , preconfiguredUserAccessKeys :: [AccessKey]
      -- Note: Access keys must be unique accross users.
    }
  deriving (Show, Eq, Generic)

instance FromJSON PreconfiguredUser where
  parseJSON =
    withObject "PreconfiguredUser" $ \o ->
      PreconfiguredUser <$> o .: "username" <*> o .: "password-hash" <*> o .:? "access-keys" .!= []

newtype AccessKey =
  AccessKey
    { unAccessKey :: Text
    }
  deriving (Show, Eq, Generic, FromJSON)

newtype HashedPass =
  HashedPass
    { unHashedPass :: Text
    }
  deriving (Show, Eq, Generic, FromJSON)
