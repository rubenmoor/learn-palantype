{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.PloverConfig where

import           Control.Lens.Wrapped           ( Wrapped )
import           Data.Aeson.Types               ( ToJSON
                                                , FromJSON
                                                )
import           TextShow                       ( fromText
                                                , TextShow(showb)
                                                )
import           Data.Map.Strict                ( Map )
import           Palantype.Common               ( Lang(..)
                                                , RawSteno
                                                , KeyIndex
                                                , Lang
                                                )
import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text )
import           Reflex.Dom                     ( KeyCode )
import           Data.Either                    ( lefts
                                                , rights
                                                )
import qualified Data.Map.Strict               as Map
import           Control.Category               ( (<<<) )
import           Common.PloverAdapter           ( fromPlover )

newtype PloverCfg = PloverCfg { unPloverCfg :: Map Lang PloverSystemCfg }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

instance Wrapped PloverCfg

data PloverSystemCfg = PloverSystemCfg
  -- recognized steno keys: Map (raw steno) [plover key code]
  { pcfgMapStenoKeys        :: Map KeyIndex [Text]
  -- raw steno key codes that failed to parse
  , pcfgUnrecognizedStenos  :: [RawSteno]
  -- [(Web.KeyCode, steno key index)]
  , pcfgLsKeySteno          :: [(KeyCode, KeyIndex)]
  -- plover key codes that failed to parse
  , pcfgUnrecognizedQwertys :: [Text]
  , pcfgMachine             :: Text
  , pcfgName :: CfgName
  }
  deriving (Eq, Generic, Show)

instance ToJSON PloverSystemCfg
instance FromJSON PloverSystemCfg

defaultPloverSystemCfg :: PloverSystemCfg
defaultPloverSystemCfg =
    keyMapToPloverCfg lsStenoQwertz [] "keyboard" CNQwertzDE

data CfgName
  = CNFile
  | CNQwertyEN
  | CNQwertzDE
  deriving (Eq, Generic, Show)

instance TextShow CfgName where
    showb = fromText <<< \case
        CNFile     -> "custom layout"
        CNQwertyEN -> "qwerty EN"
        CNQwertzDE -> "qwerty DE"

instance FromJSON CfgName
instance ToJSON CfgName

-- | default configuration for palantype on a qwerty keyboard
--   from http://www.openstenoproject.org/palantype/tutorial/2016/08/21/learn-palantype.html
--
defaultPloverCfg :: PloverCfg
defaultPloverCfg = PloverCfg $ Map.fromList
    [ (EN, keyMapToPloverCfg lsStenoQwertyOrig [] "keyboard" CNQwertyEN)
    , (DE, defaultPloverSystemCfg)
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
    let
        acc (lsKeySteno, mapStenoKeys, lsUQwerty) (i, plovers) =
            let
                lsEQwertys  = fromPlover <$> plovers
                lsQwertys   = mconcat $ rights lsEQwertys
                lsKeySteno' = if null lsQwertys
                    then lsKeySteno
                    else ((, i) <$> lsQwertys) ++ lsKeySteno
                mapStenoKeys' = Map.insert i plovers mapStenoKeys
                lsUQwerty'    = lefts lsEQwertys ++ lsUQwerty
            in
                (lsKeySteno', mapStenoKeys', lsUQwerty')

        (pcfgLsKeySteno, pcfgMapStenoKeys, pcfgUnrecognizedQwertys) =
            foldl acc ([], Map.empty, []) lsIndexPlover
    in
        PloverSystemCfg { .. }

lsStenoQwerty :: [(KeyIndex, [Text])]
lsStenoQwerty =
    [ (1 , ["q"])
    , (2 , ["a"])
    , (3 , ["z"])
    , (4 , ["2"])
    , (5 , ["w"])
    , (6 , ["s"])
    , (7 , ["3"])
    , (8 , ["e"])
    , (9 , ["d"])
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
    [ (1 , ["q"])
    , (2 , ["a"])
    , (3 , ["y"])
    , (4 , ["2"])
    , (5 , ["w"])
    , (6 , ["s"])
    , (7 , ["3"])
    , (8 , ["e"])
    , (9 , ["d"])
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
