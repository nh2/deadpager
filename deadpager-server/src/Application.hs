{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
  ( getApplicationDev
  , appMain
  , develMain
  , makeFoundation
    -- * for DevelMain
  , getApplicationRepl
    -- * for GHCI
  , handler
  , db
  ) where

import Control.Monad.Logger ( runLoggingT)
import qualified Data.Text as T
import Database.Persist.Sqlite (createSqlitePool, runSqlPool)
import Import
import Network.Consul
import Network.HTTP.Client.TLS (getGlobalManager)
import qualified Network.Wai.Handler.Warp as Warp (Settings, setPort)
import Network.Wai.Handler.Warp as Warp (defaultSettings, getPort, runSettings)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)

import OptParse

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.Home
import Handler.NotifyAlive
import Handler.Overview
import Handler.Profile

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: Settings -> IO App
makeFoundation Settings {..}
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
 = do
  appHttpManager <- getGlobalManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  let appStatic = deadpagerStatic
  let appConsulClient =
        ConsulClient
          {ccManager = appHttpManager, ccHostname = "127.0.0.1", ccPort = 8500, ccWithTls = False}
      appGoogleAnalyticsTracking = setGoogleAnalyticsTracking
    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
  let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
      tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
      logFunc = messageLoggerSource tempFoundation appLogger
    -- Create the database connection pool
  pool <- flip runLoggingT logFunc $ createSqlitePool (T.pack $ fromAbsFile setDBFile) 1
    -- Perform database migration using our application's logging settings.
  runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
    -- Return the foundation
  return $ mkFoundation pool

-- | Warp settings for the given foundation value.
warpSettings :: Settings -> Warp.Settings
warpSettings Settings {..} = Warp.setPort setPort $ defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Warp.Settings, Application)
getApplicationDev = do
  settings <- getSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings settings
  app <- toWaiAppPlain foundation
  return (wsettings, app)

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain
    -- Get the settings from all relevant sources
 = do
  settings <- getSettings
    -- Generate the foundation from the settings
  foundation <- makeFoundation settings
    -- Generate a WAI Application from the foundation
  app <- toWaiAppPlain foundation
    -- Run the application with Warp
  runSettings (warpSettings settings) app

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  settings <- getSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings settings
  app1 <- toWaiAppPlain foundation
  return (getPort wsettings, foundation, app1)

---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------
-- | Run a handler
handler :: Handler a -> IO a
handler h = getSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend Handler a -> IO a
db = handler . runDB
