{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}

module Common.Api where

import           Common.PloverAdapter      (fromPlover)
import           Control.Lens.Wrapped      (Wrapped)
import           Data.Aeson                (FromJSON, FromJSONKey, ToJSON,
                                            ToJSONKey)
import           Data.Default              (Default (..))
import           Data.Either               (lefts, rights)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Palantype.Common          (KeyIndex)
import           Palantype.Common.RawSteno (RawSteno)
import           Servant.API               ((:<|>), Get, (:>), JSON, PlainText, Post,
                                            ReqBody)
import           TextShow                  (TextShow (..), fromText)
import           Web.KeyCode               (Key)
import Data.HashMap.Strict (HashMap)
import Control.Category ((<<<))

type RoutesApi = "api" :>
     (    "config" :> "new"   :> ReqBody '[PlainText] String :> Post '[JSON] (Lang, PloverSystemCfg)
     :<|> "dict"   :> "top2k" :> Get '[JSON] (HashMap RawSteno Text, HashMap Text [RawSteno])
     )

type Routes = RoutesApi

data Lang = EN | DE
  deriving (Eq, Generic, Ord)

instance FromJSON Lang
instance ToJSON Lang
instance FromJSONKey Lang
instance ToJSONKey Lang

instance TextShow Lang where
  showb = \case
    EN -> fromText "Palantype"
    DE -> fromText "Palantype DE"

showSymbol :: Lang -> Text
showSymbol = \case
  EN -> "EN"
  DE -> "DE"

newtype PloverCfg = PloverCfg { unPloverCfg :: Map Lang PloverSystemCfg }
  deriving (Generic, ToJSON, FromJSON)

instance Wrapped PloverCfg

-- PloverSystemCfg

data CfgName
  = CNFile
  | CNQwertyEN
  | CNQwertzDE
  deriving (Generic, Eq)

instance TextShow CfgName where
  showb = fromText <<< \case
    CNFile -> "custom layout"
    CNQwertyEN -> "qwerty EN"
    CNQwertzDE -> "qwerty DE"

instance FromJSON CfgName
instance ToJSON CfgName

data PloverSystemCfg = PloverSystemCfg
  -- recognized steno keys: Map (raw steno) [plover key code]
  { pcfgMapStenoKeys        :: Map KeyIndex [Text]
  -- raw steno key codes that failed to parse
  , pcfgUnrecognizedStenos  :: [RawSteno]
  -- [(Web.KeyCode, raw steno code)]
  , pcfgLsKeySteno          :: [(Key, KeyIndex)]
  -- plover key codes that failed to parse
  , pcfgUnrecognizedQwertys :: [Text]
  , pcfgMachine             :: Text
  , pcfgName :: CfgName
  }
  deriving (Eq, Generic)

instance ToJSON PloverSystemCfg
instance FromJSON PloverSystemCfg

instance Default PloverSystemCfg where
  def = keyMapToPloverCfg lsStenoQwertz [] "keyboard" CNQwertzDE

lsStenoQwerty :: [(KeyIndex, [Text])]
lsStenoQwerty =
  [ ( 1, ["q"])
  , ( 2, ["a"])
  , ( 3, ["z"])
  , ( 4, ["2"])
  , ( 5, ["w"])
  , ( 6, ["s"])
  , ( 7, ["3"])
  , ( 8, ["e"])
  , ( 9, ["d"])
  , (10, ["4"])
  , (11, ["r"])
  , (12, ["f"])
  , (13, ["c"])
  , (14, ["v"])
  , (15, ["b"])
  , (16, ["g"])
  , (17, ["h"])
  , (18, ["n"])
  , (19, ["m"])
  , (20, [","])
  , (21, ["7"])
  , (22, ["u"])
  , (23, ["j"])
  , (24, ["8"])
  , (25, ["i"])
  , (26, ["k"])
  , (27, ["9"])
  , (28, ["o"])
  , (29, ["l"])
  , (30, ["p"])
  , (31, [";"])
  , (32, ["/"])
  ]

lsStenoQwertz :: [(KeyIndex, [Text])]
lsStenoQwertz =
  [ ( 1, ["q"])
  , ( 2, ["a"])
  , ( 3, ["y"])
  , ( 4, ["2"])
  , ( 5, ["w"])
  , ( 6, ["s"])
  , ( 7, ["3"])
  , ( 8, ["e"])
  , ( 9, ["d"])
  , (10, ["4"])
  , (11, ["r"])
  , (12, ["f"])
  , (13, ["c"])
  , (14, ["v"])
  , (15, ["b"])
  , (16, ["g"])
  , (17, ["h"])
  , (18, ["n"])
  , (19, ["m"])
  , (20, [","])
  , (21, ["7"])
  , (22, ["u"])
  , (23, ["j"])
  , (24, ["8"])
  , (25, ["i"])
  , (26, ["k"])
  , (27, ["9"])
  , (28, ["o"])
  , (29, ["l"])
  , (30, ["p"])
  , (31, ["ö"])
  , (32, ["-"])
  ]

instance ToJSON Key
instance FromJSON Key

-- default configuration for palantype on a qwerty keyboard
-- from http://www.openstenoproject.org/palantype/tutorial/2016/08/21/learn-palantype.html
--
instance Default PloverCfg where
  def = PloverCfg $ Map.fromList
          [ (EN, keyMapToPloverCfg lsStenoQwertyOrig [] "keyboard" CNQwertyEN)
          , (DE, def)
          ]
    where
      -- | this is a copy of the default mapping for plover's
      --   Palantype plug-in
      lsStenoQwertyOrig :: [(KeyIndex, [Text])]
      lsStenoQwertyOrig =
        [ (4 , ["2"])
        , (7 , ["3"])
        , (10, ["4"])
        , (21, ["8"])
        , (24, ["9"])
        , (27, ["0"])
        , (1 , ["q"])
        , (5 , ["w"])
        , (8 , ["e"])
        , (11, ["r"])
        , (22, ["u"])
        , (25, ["i"])
        , (28, ["o"])
        , (31, ["p"])
        , (2 , ["a"])
        , (6 , ["s"])
        , (9 , ["d"])
        , (12, ["f"])
        , (14, ["g"])
        , (18, ["h"])
        , (23, ["j"])
        , (26, ["k"])
        , (29, ["l"])
        , (30, [";"])
        , (3 , ["z", "x", "c"])
        , (15, ["v"])
        , (17, ["b"])
        , (19, ["n"])
        , (32, ["m", ",", ".", "/"])
        ]

{-|
generate a ploverCfg object from the specified key map
-}
keyMapToPloverCfg
  :: [(KeyIndex, [Text])] -- ^ key map
  -> [RawSteno]           -- ^ unrecognized steno keys
  -> Text                 -- ^ machine
  -> CfgName              -- ^ config name
  -> PloverSystemCfg
keyMapToPloverCfg lsIndexPlover pcfgUnrecognizedStenos pcfgMachine pcfgName =
  let acc (lsKeySteno, mapStenoKeys, lsUQwerty) (i, plovers) =
        let lsEQwerty = fromPlover <$> plovers
            lsQwerty = rights lsEQwerty
            lsKeySteno' =
              if null lsQwerty
                then lsKeySteno
                else ((, i) <$> lsQwerty) ++ lsKeySteno
            mapStenoKeys' = Map.insert i plovers mapStenoKeys
            lsUQwerty' = lefts lsEQwerty ++ lsUQwerty
        in (lsKeySteno', mapStenoKeys', lsUQwerty')

      (pcfgLsKeySteno, pcfgMapStenoKeys, pcfgUnrecognizedQwertys) =
        foldl acc ([], Map.empty, []) lsIndexPlover
  in  PloverSystemCfg{..}

-- getPloverSystemCfg
--   :: Lang
--   -> PloverCfg
--   -> PloverSystemCfg
-- getPloverSystemCfg lang cfg = cfg ^. _Wrapped' . at lang . non def
--   -- unPloverCfg pcfg ! lang
--
-- setPloverSystemCfg
--   :: Lang
--   -> PloverSystemCfg
--   -> PloverCfg
--   -> PloverCfg
-- setPloverSystemCfg lang systemCfg = _Wrapped' %~ (ix lang .~ systemCfg)
