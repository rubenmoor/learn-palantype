{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Common.Api where

import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)
import           Servant.API  ((:>), JSON, PlainText, Post, ReqBody)
import Data.Text (Text)
import Data.Map (Map)

commonStuff :: String
commonStuff = "Here is a string defined in Common.Api"

type RoutesApi = "api" :> "config" :> "new" :> ReqBody '[PlainText] String  :> Post '[JSON] PloverCfg

data PloverCfg = PloverCfg
  { pcfgStenoKeys :: Map Text [Text]
  , pcfgKeySteno :: Map Text Text
  , pcfgSystem :: Text
  , pcfgMachine :: Text
  }
  deriving (Generic)

instance ToJSON PloverCfg
