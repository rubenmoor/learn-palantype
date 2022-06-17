{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config
  ( Params (..)
  ) where

import           Data.Aeson   (FromJSON)
import           Data.String  (String)
import           GHC.Generics (Generic)
import System.IO (FilePath)

data Params = Params
  { paramDbHost     :: String
  , paramDbUser     :: String
  , paramDbPassword :: String
  , paramDbDatabase :: String
  , paramMediaDir   :: FilePath
  }
  deriving (Generic)

instance FromJSON Params
