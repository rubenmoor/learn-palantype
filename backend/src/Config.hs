{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config
  ( Params (..)
  ) where

import           Data.Aeson   (FromJSON)
import           Data.String  (String)
import           GHC.Generics (Generic)

data Params = Params
  { paramDbHost            :: String
  , paramDbUser            :: String
  , paramDbPassword        :: String
  , paramDbDatabase        :: String
  , paramGithubAccessToken :: String
  }
  deriving (Generic)

instance FromJSON Params
