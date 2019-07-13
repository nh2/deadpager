{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Settings.StaticFiles where

import ClassyPrelude.Yesod

import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote

mkEmbeddedStatic
  False
  "deadpagerStatic"
  [ embedRemoteFileAt
      "static/css/bootstrap.css"
      "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
  , embedRemoteFileAt
      "static/js/bootstrap.js"
      "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js"
  , embedRemoteFileAt
      "static/css/fontawesome.css"
      "https://use.fontawesome.com/releases/v5.8.1/css/all.css"
  ]
