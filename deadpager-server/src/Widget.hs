{-# LANGUAGE TemplateHaskell #-}

module Widget where

import Language.Haskell.TH

import Yesod.Default.Util ( widgetFileNoReload, widgetFileReload)
import Data.Default

import Constants

widgetFile :: String -> Q Exp
widgetFile =
  (if development
     then widgetFileReload
     else widgetFileNoReload)
    def
