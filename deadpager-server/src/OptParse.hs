{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OptParse
  ( getSettings
  , Settings(..)
  ) where

import Import.NoFoundation

import qualified Data.ByteString as SB
import Data.Yaml as Yaml (decodeThrow)
import qualified System.Environment as System (getArgs)

import OptParse.Types
import Options.Applicative

getSettings :: IO Settings
getSettings = do
  args <- getFlags
  mc <- getConfiguration args
  buildInstructions args mc

buildInstructions :: Flags -> Maybe Configuration -> IO Settings
buildInstructions Flags {..} mc = do
  let setHost = fromMaybe "localhost" $ mconf confHost
  let setPort = fromMaybe 8000 $ mconf confPort
  setDBFile <-
    case mconf confDBFile of
      Nothing -> getDefaultDBFile
      Just fp -> resolveFile' fp
  let setGoogleAnalyticsTracking = mconf confGoogleAnalyticsTracking
  let setGoogleSearchConsoleVerification = mconf confGoogleSearchConsoleVerification
  let setAllowUserCreation = fromMaybe True $ mconf confAllowUserCreation
  let setPreconfiguredUsers = fromMaybe [] $ mconf confPreconfiguredUsers
  pure Settings {..}
  where
    mconf func = mc >>= func

getDefaultDBFile :: IO (Path Abs File)
getDefaultDBFile = do
  resolveFile' "deadpager.sqlite"

getConfiguration :: Flags -> IO (Maybe Configuration)
getConfiguration Flags {..} = do
  f <-
    case flagConfigFile of
      Just fp -> resolveFile' fp
      Nothing -> getDefaultConfigFile
  mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile f
  forM mContents Yaml.decodeThrow

getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = do
  resolveFile' "deadpager-config.yaml"

getFlags :: IO Flags
getFlags = do
  args <- System.getArgs
  let result = serveFlagsParser args
  handleParseResult result

serveFlagsParser :: [String] -> ParserResult Flags
serveFlagsParser =
  execParserPure
    ParserPrefs
      { prefMultiSuffix = ""
      , prefDisambiguate = True
      , prefShowHelpOnError = True
      , prefShowHelpOnEmpty = True
      , prefBacktrack = True
      , prefColumns = 80
      }
    flagParser

flagParser :: ParserInfo Flags
flagParser = info (helper <*> parseFlags) (fullDesc <> progDesc "Deadpager")

parseFlags :: Parser Flags
parseFlags =
  Flags <$>
  option
    (Just <$> str)
    (mconcat
       [long "config", metavar "CONFIG_FILE", help "The configuration file to use", value Nothing])
