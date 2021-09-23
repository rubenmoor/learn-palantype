{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Common.Api where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Default (Default (..))
import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Servant.API  ((:>), JSON, PlainText, Post, ReqBody)

type RoutesApi = "api" :> "config" :> "new" :> ReqBody '[PlainText] String  :> Post '[JSON] PloverCfg

data PloverCfg = PloverCfg
  { pcfgStenoKeys :: Map String [String]
  , pcfgKeySteno  :: Map String String
  , pcfgSystem    :: Text
  , pcfgMachine   :: Text
  }
  deriving (Generic)

instance ToJSON PloverCfg
instance FromJSON PloverCfg

-- default configuration for palantype on a qwerty keyboard
-- from http://www.openstenoproject.org/palantype/tutorial/2016/08/21/learn-palantype.html
--
lsKeySteno :: [(String, String)]
lsKeySteno =
  [ ("2", "P-")
  , ("3", "M-")
  , ("4", "N-")
  , ("8", "-N")
  , ("9", "-M")
  , ("0", "-P")
  , ("q", "C-")
  , ("w", "T-")
  , ("e", "F-")
  , ("r", "L-")
  , ("u", "-L")
  , ("i", "-F")
  , ("o", "-T")
  , ("p", "-H")
  , ("a", "S-")
  , ("s", "H-")
  , ("d", "R-")
  , ("f", "Y-")
  , ("g", "O-")
  , ("h", "-A")
  , ("j", "-C")
  , ("k", "-R")
  , ("l", "-+")
  , (";", "-S")
  , ("z", "+-")
  , ("x", "+-")
  , ("c", "+-")
  , ("v", "E-")
  , ("b", "I")
  , ("n", "-U")
  , ("m", "-^")
  , (",", "-^")
  , (".", "-^")
  , ("/", "-^")
  ]

mapKeySteno :: Map String String
mapKeySteno = Map.fromList lsKeySteno

mapStenoKeys :: Map String [String]
mapStenoKeys = foldl acc Map.empty lsKeySteno
  where
    acc :: Map String [String] -> (String, String) -> Map String [String]
    acc m (k, v) = Map.insertWith (<>) v [k] m

instance Default PloverCfg where
  def = PloverCfg
    { pcfgStenoKeys = mapStenoKeys
    , pcfgKeySteno = mapKeySteno
    , pcfgSystem = "Palantype"
    , pcfgMachine = "keyboard"
    }
