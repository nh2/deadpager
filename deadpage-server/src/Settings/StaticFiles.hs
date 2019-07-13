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
        "static/bootstrap.min.css"
        "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
    , embedRemoteFileAt "static/jquery.min.js" "https://code.jquery.com/jquery-3.3.1.slim.min.js"
    , embedRemoteFileAt
        "static/popper.min.js"
        "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js"
    , embedRemoteFileAt
        "static/bootstrap.min.js"
        "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js"

  ]
