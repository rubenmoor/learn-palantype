{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Common.Api where

import Servant.API ( JSON, type (:>), Post )
import Servant.Multipart ( Mem, MultipartData, MultipartForm )
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

commonStuff :: String
commonStuff = "Here is a string defined in Common.Api"

type RoutesApi = "api" :> "config" :> "new" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] PloverCfg

data PloverCfg = PloverCfg
  deriving (Generic)

instance ToJSON PloverCfg
