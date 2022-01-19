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
  , stageDescription
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
  , "patternoverview"
  ]

instance IsString Stage where
  fromString str =
    let txt = Text.pack str
    in  if txt `elem` strsStage
           then Stage txt
           else error $ "Does not exist: Stage: " <> str

stageDescription :: Stage -> Text
stageDescription (Stage stage) = case elemIndex stage strsStage of
    Just 0  -> "Introduction"
    Just 1  -> "Ex. 1: Type the letters"
    Just 2  -> "Ex. 2: Memorize the order"
    Just 3  -> "Ex. 3: Type the letters blindly"
    Just 4  -> "Ex. 4: Memorize the order blindly"
    Just 5  -> "Ex. 5: Memorize the left hand"
    Just 6  -> "Ex. 6: Memorize the right hand"
    Just 7  -> "Ex. 7: Memorize them all"
    Just 8  -> "Ex. 1: Make use of home row"
    Just 9  -> "Ex. 2: Learn your first chords"
    Just 10 -> "Ex. 3: Onset, nucleus, and coda"
    Just 11 -> "Ex. 4: Syllables and word parts"
    Just 12 -> "Pattern overview"
    Nothing -> "Page not found: " <> stage

mPrev :: Stage -> Maybe Stage
mPrev (Stage s) = do
  i <- pred <$> elemIndex s strsStage
  Stage <$> strsStage ^? element i

mNext :: Stage -> Maybe Stage
mNext (Stage s) = do
  i <- succ <$> elemIndex s strsStage
  Stage <$> strsStage ^? element i
