{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

module Common.Api where

import           Common.Alphabet (PTChord, PTChar)
import           Common.Keys     (fromPlover)
import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Default    (Default (..))
import           Data.Either     (lefts, rights)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Text       (Text)
import           GHC.Generics    (Generic)
import           Servant.API     ((:<|>), (:>), JSON, PlainText, Post, ReqBody)
import           Text.Read       (readMaybe)
import           Web.KeyCode     (Key)

type RoutesApi = "api" :>
     ( "config" :> "new"   :> ReqBody '[PlainText] String :> Post '[JSON] PloverCfg
  :<|> "parse"  :> "steno" :> ReqBody '[PlainText] String :> Post '[JSON] [PTChord]
     )

data PloverCfg = PloverCfg
  { pcfgMapStenoKeys        :: Map PTChar [String] -- recognized steno keys
  , pcfgUnrecognizedStenos  :: [String] -- steno keys that failed to parse
  , pcfgLsKeySteno          :: [(Key, PTChar)]
  , pcfgUnrecognizedQwertys :: [String] -- qwerty keys in the plover format
                                        -- that failed to parse
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
  def = keyMapToPloverCfg lsStenoKeys "Palantype" "keyboard"
    where
      lsStenoKeys :: [(String, [String])]
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
        , ("I ", ["b"])
        , ("-U", ["n"])
        , ("-^", ["m", ",", ".", "/"])
        ]


keyMapToPloverCfg
  :: [(String, [String])]
  -> Text
  -> Text
  -> PloverCfg
keyMapToPloverCfg stenoKeys pcfgSystem pcfgMachine =
  let acc (lsKeySteno, mapStenoKeys, lsUSteno, lsUQwerty) (strSteno, plovers) =
        let mSteno = readMaybe strSteno
            lsEQwerty = fromPlover <$> plovers
            lsQwerty = rights lsEQwerty
            lsKeySteno' = case (lsQwerty, mSteno) of
              (qwertys@(_:_), Just steno) -> ((, steno) <$> qwertys) ++ lsKeySteno
              _                           -> lsKeySteno
            mapStenoKeys' = case mSteno of
              Just steno -> Map.insert steno plovers mapStenoKeys
              Nothing    -> mapStenoKeys
            lsUSteno' = case mSteno of
              Just _  -> lsUSteno
              Nothing | strSteno `elem` ["no-op", "arpeggiate"] -> lsUSteno
              Nothing -> strSteno : lsUSteno
            lsUQwerty' = lefts lsEQwerty ++ lsUQwerty
        in (lsKeySteno', mapStenoKeys', lsUSteno', lsUQwerty')

      (pcfgLsKeySteno, pcfgMapStenoKeys, pcfgUnrecognizedStenos, pcfgUnrecognizedQwertys) =
        foldl acc ([], Map.empty, [], []) stenoKeys
  in  PloverCfg{..}
