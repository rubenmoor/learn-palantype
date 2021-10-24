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
import           Servant.API               ((:>), JSON, PlainText, Post,
                                            ReqBody)
import           TextShow                  (TextShow (..), fromText)
import           Web.KeyCode               (Key)

type RoutesApi = "api" :>
     ( "config" :> "new"   :> ReqBody '[PlainText] String :> Post '[JSON] (Lang, PloverSystemCfg)
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

newtype PloverCfg = PloverCfg { unPloverCfg :: Map Lang PloverSystemCfg }
  deriving (Generic, ToJSON, FromJSON)

instance Wrapped PloverCfg

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
  }
  deriving (Eq, Generic)

instance ToJSON PloverSystemCfg
instance FromJSON PloverSystemCfg

instance Default PloverSystemCfg where
  def = keyMapToPloverCfg lsStenoKeys [] "keyboard"
    where
        lsStenoKeys :: [(KeyIndex, [Text])]
        lsStenoKeys =
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
          , (11, ["e"])
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

instance ToJSON Key
instance FromJSON Key

-- default configuration for palantype on a qwerty keyboard
-- from http://www.openstenoproject.org/palantype/tutorial/2016/08/21/learn-palantype.html
--
instance Default PloverCfg where
  def = PloverCfg $ Map.fromList
          [ (EN, keyMapToPloverCfg lsStenoKeysOrig [] "keyboard")
          , (DE, def)
          ]
    where
      -- | this is a copy of the default mapping for plover's
      --   Palantype plug-in
      lsStenoKeysOrig :: [(KeyIndex, [Text])]
      lsStenoKeysOrig =
        [ (3 , ["2"])
        , (7 , ["3"])
        , (10, ["4"])
        , (22, ["8"])
        , (25, ["9"])
        , (28, ["0"])
        , (2 , ["q"])
        , (4 , ["w"])
        , (8 , ["e"])
        , (11, ["r"])
        , (23, ["u"])
        , (26, ["i"])
        , (29, ["o"])
        , (32, ["p"])
        , (1 , ["a"])
        , (5 , ["s"])
        , (9 , ["d"])
        , (12, ["f"])
        , (14, ["g"])
        , (18, ["h"])
        , (24, ["j"])
        , (27, ["k"])
        , (30, ["l"])
        , (31, [";"])
        , (6 , ["z", "x", "c"])
        , (15, ["v"])
        , (20, ["b"])
        , (19, ["n"])
        , (21, ["m", ",", ".", "/"])
        ]

keyMapToPloverCfg
  :: [(KeyIndex, [Text])]
  -> [RawSteno]
  -> Text
  -> PloverSystemCfg
keyMapToPloverCfg lsIndexPlover pcfgUnrecognizedStenos pcfgMachine =
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
