{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Common.Stage
  ( Stage ()
  , mPrev
  , mNext
  ) where

import           Data.Text             (replace, Text)
import Data.Eq (Eq)
import GHC.Generics (Generic)
import Data.Ord (Ord)
import Text.Show (Show)
import Control.Lens.TH (makeWrapped)
import Data.Default (Default (def))
import TextShow (TextShow (showb), fromText)
import Data.Aeson (ToJSON)
import Data.Aeson (FromJSON)
import Data.String (IsString (..))
import Data.Monoid ((<>))
import GHC.Base (Maybe (..), error)
import Data.Function (($))
import qualified Data.Text as Text
import Data.Foldable (Foldable(elem))
import Data.List (elemIndex)
import Control.Lens (element, (^?))
import Data.Functor ((<$>))
import GHC.Enum (succ, Enum(pred))
import Data.Text (uncons)
import Data.Text (cons)
import Data.Char (toUpper)
import Control.Category ((<<<))
import Safe (atMay)

newtype Stage = Stage { unStage :: Text }
  deriving (Eq, Generic, Ord, Show)

makeWrapped ''Stage

instance ToJSON Stage
instance FromJSON Stage

instance Default Stage where
    def = Stage "introduction"

capitalize :: Text -> Text
capitalize str = case uncons str of
  Nothing -> ""
  Just (h, rem) -> cons (toUpper h) rem

instance TextShow Stage where
    showb = fromText
        <<< capitalize
        <<< replace "_" " "
        <<< replace "-" "."
        <<< unStage

strsStage :: [Text]
strsStage =
  [ "introduction"
  , "stage_1-1"
  , "stage_1-2"
  , "stage_1-3"
  , "stage_1-4"
  , "stage_1-5"
  , "stage_1-6"
  , "stage_1-7"
  , "stage_2-1"
  , "stage_2-2"
  , "stage_2-3"
  , "stage_2-4"
  , "stage_3-1"
  , "stage_3-2"
  , "stage_3-3"
  , "stage_3-4"
  , "stage_3-5"
  , "stage_3-6"
  , "stage_3-7"
  , "stage_3-8"
  , "stage_3-9"
  , "stage_3-10"
  , "stage_3-11"
  , "stage_3-12"
  , "stage_3-13"
  , "stage_3-14"
  , "stage_3-15"
  , "stage_3-16"
  , "patternoverview"
  ]

instance IsString Stage where
  fromString str =
    let txt = Text.pack str
    in  if txt `elem` strsStage
           then Stage txt
           else error $ "Does not exist: Stage: " <> str

{-
  | PatReplCommon
  | PatDiConsonant
  | PatCodaH
  | PatCodaR
  | PatCodaRR
  | PatCodaHR
  | PatDt
  | PatDiphtong
  | PatReplC
  | PatCodaGK
  | PatSZ
  | PatIJ
  | PatTsDsPs
  | PatDiVowel
  | PatReplH
  | PatSmallS
-}

mPrev :: Stage -> Maybe Stage
mPrev (Stage s) = do
  i <- pred <$> elemIndex s strsStage
  Stage <$> strsStage `atMay` i

mNext :: Stage -> Maybe Stage
mNext (Stage s) = do
  i <- succ <$> elemIndex s strsStage
  Stage <$> strsStage `atMay` i
