{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation
  ( module Foundation
    , PreconfiguredUser(..)
  , AccessKey(..)
  , HashedPass(..)
  ) where

import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation
import Text.Hamlet (hamletFile)

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy
import qualified Yesod.Auth.Email as YAE
import Yesod.Auth.Hardcoded

import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.EmbeddedStatic

import Network.Consul.Types

import OptParse.Types

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App =
  App
    { appStatic :: EmbeddedStatic -- ^ Settings for static file serving.
    , appConnPool :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger :: Logger
    , appConsulClient :: ConsulClient
    , appGoogleAnalyticsTracking :: Maybe Text
    , appAllowUserCreation :: Bool
    , appPreconfiguredUsers :: [PreconfiguredUser]
    }

data MenuItem =
  MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a
   = forall (m :: * -> *). (MonadIO m) =>
                             ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
                                                                      where
  approot :: Approot App
  approot = ApprootRelative
    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ =
    Just <$>
    defaultClientSessionBackend
      120 -- timeout in minutes
      "config/client_session_key.aes"
    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware = defaultYesodMiddleware
  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    muser <- maybeAuthPair
    mcurrentRoute <- getCurrentRoute
        -- Define the menu items of the header.
    let menuItems =
          [ NavbarLeft $
            MenuItem {menuItemLabel = "Home", menuItemRoute = HomeR, menuItemAccessCallback = True}
          , NavbarLeft $
            MenuItem
              { menuItemLabel = "Profile"
              , menuItemRoute = ProfileR
              , menuItemAccessCallback = isJust muser
              }
          , NavbarLeft $
            MenuItem
              { menuItemLabel = "Overview"
              , menuItemRoute = OverviewR
              , menuItemAccessCallback = isJust muser
              }
          , NavbarRight $
            MenuItem
              { menuItemLabel = "Login"
              , menuItemRoute = AuthR LoginR
              , menuItemAccessCallback = isNothing muser
              }
          , NavbarRight $
            MenuItem
              { menuItemLabel = "Logout"
              , menuItemRoute = AuthR LogoutR
              , menuItemAccessCallback = isJust muser
              }
          ]
    let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
    let navbarRightMenuItems = [x | NavbarRight x <- menuItems]
    let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
    let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
    pc <- widgetToPageContent $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
    -- The page to be redirected to when authentication is required.
  authRoute :: App -> Maybe (Route App)
  authRoute _ = Just $ AuthR LoginR
  isAuthorized ::
       Route App -- ^ The route the user is visiting.
    -> Bool -- ^ Whether or not this is a "write" request.
    -> Handler AuthResult
    -- Routes not requiring authentication.
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized HomeR _ = return Authorized
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  isAuthorized (StaticR _) _ = return Authorized
    -- the profile route requires that the user is authenticated, so we
    -- delegate to that function
  isAuthorized ProfileR _ = isAuthenticated
  isAuthorized OverviewR _ = isAuthenticated
  isAuthorized (NotifyAliveR _) _ = return Authorized
  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner :: Handler (DBRunner App, Handler ())
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = UserId
    -- Where to send a user after successful login
  loginDest :: App -> Route App
  loginDest _ = OverviewR
    -- Where to send a user after logout
  logoutDest :: App -> Route App
  logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
  redirectToReferer :: App -> Bool
  redirectToReferer _ = True
  authenticate :: (MonadHandler m, HandlerSite m ~ App) => Creds App -> m (AuthenticationResult App)
  authenticate creds =
    liftHandler $
    runDB $ do
      x <- getBy $ UniqueUser $ credsIdent creds
      case x of
        Just (Entity uid _) -> return $ Authenticated uid
        Nothing ->
          Authenticated <$> insert User {userIdent = credsIdent creds, userPassword = Nothing}
    -- You can add other plugins like Google Email, email or OAuth here
  authPlugins :: App -> [AuthPlugin App]
  authPlugins App {..} = [authHardcoded | not $ null appPreconfiguredUsers] ++ extraAuthPlugins
    where
      extraAuthPlugins = [authDummy | development]

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
  muid <- maybeAuthId
  return $
    case muid of
      Nothing -> Unauthorized "You must login to access this page"
      Just _ -> Authorized

instance YesodAuthPersist App

instance YesodAuthHardcoded App where
  validatePassword un pw = do
    us <- getsYesod appPreconfiguredUsers
    pure $
      any
        (\PreconfiguredUser {..} ->
           preconfiguredUserName == un && verifyPassword pw preconfiguredUserPasswordHash) us
  doesUserNameExist n = do
    us <- getsYesod appPreconfiguredUsers
    pure $ any ((== n) . preconfiguredUserName) us

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

accessKeyField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m AccessKey
accessKeyField = convertField AccessKey unAccessKey textField

hashPassword :: MonadIO m => Text -> m Text
hashPassword pwd = liftIO $ YAE.saltPass pwd

verifyPassword ::
     Text -- ^ password
  -> HashedPass -- ^ hashed password
  -> Bool
verifyPassword pw (HashedPass pw') = YAE.isValidPass pw pw'
