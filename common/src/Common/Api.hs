{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.Api where

import           Common.PloverAdapter     (fromPlover)
import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Default    (Default (..))
import           Data.Either     (isRight, lefts, rights)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Text       (Text)
import           GHC.Generics    (Generic)
import           Servant.API     ((:>), JSON, PlainText, Post, ReqBody)
import           Web.KeyCode     (Key)
import Palantype.Common.RawSteno (RawSteno, parseStenoKey)
import Palantype.Common (Palantype, KeyIndex, keyIndex)
import Palantype.EN (pEN)

type RoutesApi = "api" :>
     ( "config" :> "new"   :> ReqBody '[PlainText] String :> Post '[JSON] PloverCfg
     )

type Routes = RoutesApi

data PloverCfg = PloverCfg
  -- recognized steno keys: Map (raw steno) [plover key code]
  { pcfgMapStenoKeys        :: Map KeyIndex [Text]
  -- raw steno key codes that failed to parse
  , pcfgUnrecognizedStenos  :: [RawSteno]
  -- [(Web.KeyCode, raw steno code)]
  , pcfgLsKeySteno          :: [(Key, KeyIndex)]
  -- plover key codes that failed to parse
  , pcfgUnrecognizedQwertys :: [Text]
  , pcfgSystem              :: Text
  , pcfgMachine             :: Text
  }
  deriving (Generic)

instance ToJSON PloverCfg
instance FromJSON PloverCfg

instance ToJSON Key
instance FromJSON Key

-- default configuration for palantype on a qwerty keyboard
-- from http://www.openstenoproject.org/palantype/tutorial/2016/08/21/learn-palantype.html
--
instance Default PloverCfg where
  def = keyMapToPloverCfg pEN lsStenoKeys "Palantype" "keyboard"
    where
      lsStenoKeys :: [(RawSteno, [Text])]
      lsStenoKeys =
        [ ("P-", ["2"])
        , ("M-", ["3"])
        , ("N-", ["4"])
        , ("-N", ["8"])
        , ("-M", ["9"])
        , ("-P", ["0"])
        , ("C-", ["q"])
        , ("T-", ["w"])
        , ("F-", ["e"])
        , ("L-", ["r"])
        , ("-L", ["u"])
        , ("-F", ["i"])
        , ("-T", ["o"])
        , ("-H", ["p"])
        , ("S-", ["a"])
        , ("H-", ["s"])
        , ("R-", ["d"])
        , ("Y-", ["f"])
        , ("O-", ["g"])
        , ("-A", ["h"])
        , ("-C", ["j"])
        , ("-R", ["k"])
        , ("-+", ["l"])
        , ("-S", [";"])
        , ("+-", ["z", "x", "c"])
        , ("E-", ["v"])
        , ("I", ["b"])
        , ("-U", ["n"])
        , ("-^", ["m", ",", ".", "/"])
        ]


keyMapToPloverCfg
  :: forall proxy key.
  ( Palantype key )
  => proxy key
  -> [(RawSteno, [Text])]
  -> Text
  -> Text
  -> PloverCfg
keyMapToPloverCfg _ stenoKeys pcfgSystem pcfgMachine =
  let acc (lsKeySteno, mapStenoKeys, lsUSteno, lsUQwerty) (rawSteno, plovers) =
        let eIKey = keyIndex <$> (parseStenoKey rawSteno :: Either Text key)
            lsEQwerty = fromPlover <$> plovers
            lsQwerty = rights lsEQwerty
            lsKeySteno' = case (not $ null lsQwerty, eIKey) of
              (True, Right iKey) -> ((, iKey) <$> lsQwerty) ++ lsKeySteno
              (_   , _         ) -> lsKeySteno
            mapStenoKeys' =
              case eIKey of
                Right iKey -> Map.insert iKey plovers mapStenoKeys
                Left  _    -> mapStenoKeys
            lsUSteno' = case eIKey of
              Right _  -> lsUSteno
              Left  _ | rawSteno `elem` ["no-op", "arpeggiate"] -> lsUSteno
              Left  _ -> rawSteno : lsUSteno
            lsUQwerty' = lefts lsEQwerty ++ lsUQwerty
        in (lsKeySteno', mapStenoKeys', lsUSteno', lsUQwerty')

      (pcfgLsKeySteno, pcfgMapStenoKeys, pcfgUnrecognizedStenos, pcfgUnrecognizedQwertys) =
        foldl acc ([], Map.empty, [], []) stenoKeys
  in  PloverCfg{..}
