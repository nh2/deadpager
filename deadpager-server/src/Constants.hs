{-# LANGUAGE CPP #-}

module Constants where

development :: Bool
#ifdef DEVELOPMENT
development = True
#else
development = False
#endif
