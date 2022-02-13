{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Common.Stage
    ( Stage (),
      mPrev,
      mNext,
      readLoc,
      stageMeta,
      StageMeta (..),
    )
where

import Control.Category ((>>>), (<<<))
import Control.Lens.TH (makeWrapped)
import Data.Aeson (ToJSON)
import Data.Aeson (FromJSON)
import Data.Char (toUpper)
import Data.Default (Default (def))
import Data.Eq (Eq)
import Data.Foldable (Foldable (elem))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (elemIndex)
import Data.Monoid ((<>))
import Data.Ord (Ord)
import Data.Text (Text, replace)
import qualified Data.Text as Text
import Data.Text (uncons)
import Data.Text (cons)
import GHC.Base (Maybe (..), error)
import GHC.Enum (Enum (pred), succ)
import GHC.Generics (Generic)
import Safe (atMay)
import Text.Show (Show)
import TextShow (TextShow (showb), fromText)
import Palantype.Common (Palantype(toDescription))
import Palantype.DE (Pattern(..))
import Text.Read (get, Read (readPrec))
import Control.Applicative (Applicative(pure))
import Control.Monad (guard)
import Common.Stage.TH (readLoc)
import Control.Applicative (Alternative(some))

newtype Stage = Stage {unStage :: Text}
    deriving stock (Eq, Generic, Ord, Show)

makeWrapped ''Stage

instance ToJSON Stage
instance FromJSON Stage

instance Read Stage where
    readPrec = do
      str <- Text.pack <$> some get
      guard $ str `elem` strsStage
      pure $ Stage str

instance Default Stage where
    def = $readLoc "introduction"

capitalize :: Text -> Text
capitalize str = case uncons str of
    Nothing -> ""
    Just (h, rem) -> cons (toUpper h) rem

instance TextShow Stage where
    showb =
        fromText
            <<< capitalize
            <<< replace "_" " "
            <<< replace "-" "."
            <<< unStage

strsStage :: [Text]
strsStage =
    [ "introduction",
      "stage_1-1",
      "stage_1-2",
      "stage_1-3",
      "stage_1-4",
      "stage_1-5",
      "stage_1-6",
      "stage_1-7",
      "stage_2-1",
      "stage_2-2",
      "stage_2-3",
      "stage_PatSimple_0", -- 2.4
      "stage_PatReplCommon_0", -- 3.1
      "stage_PatSmallS_0",
      "stage_PatDiConsonant_0",
      "stage_PatCodaH_0",
      "stage_PatCodaR_0",
      "stage_PatCodaRR_0",
      "stage_PatCodaHR_0",
      "stage_PatDt_0",
      "stage_PatDiphtong_0",
      "stage_PatReplC_0",
      "stage_PatCodaGK_0",
      "stage_PatSZ_0",
      "stage_PatIJ_0",
      "stage_PatSwapS_0",
      "stage_PatSwapSch_0",
      "stage_PatSwapZ_0",
      "stage_PatDiVowel_0",
      "stage_PatReplH_0",
      "stage_PatSmallS_0",
      "patternoverview"
    ]

mPrev :: Stage -> Maybe Stage
mPrev (Stage s) = do
    i <- pred <$> elemIndex s strsStage
    Stage <$> strsStage `atMay` i

mNext :: Stage -> Maybe Stage
mNext (Stage s) = do
    i <- succ <$> elemIndex s strsStage
    Stage <$> strsStage `atMay` i

data StageMeta = StageTopLevel Text | StageSubLevel Int Int Text

instance TextShow StageMeta where
  showb (StageTopLevel str) = fromText str
  showb (StageSubLevel s i str) = showb s <> "." <> showb i <> ": " <> fromText str

stageMeta :: Stage -> StageMeta
stageMeta = unStage >>> \case
    "introduction" -> StageTopLevel "Introduction"
    "stage_1-1" -> StageSubLevel 1 1 "Type the letters"
    "stage_1-2" -> StageSubLevel 1 2 "Memorize the order"
    "stage_1-3" -> StageSubLevel 1 3 "Type the letters blindly"
    "stage_1-4" -> StageSubLevel 1 4 "Memorize the order blindly"
    "stage_1-5" -> StageSubLevel 1 5 "Memorize the left hand"
    "stage_1-6" -> StageSubLevel 1 6 "Memorize the right hand"
    "stage_1-7" -> StageSubLevel 1 7 "Memorize them all"
    "stage_2-1" -> StageSubLevel 2 1 "Make use of home row"
    "stage_2-2" -> StageSubLevel 2 2 "Learn your first chords"
    "stage_2-3" -> StageSubLevel 2 3 "Onset, nucleus, and coda"
    "stage_PatSimple_0"      -> StageSubLevel 2 4  "Syllables and word parts"
    "stage_PatReplCommon_0"  -> StageSubLevel 3 1  $ toDescription PatReplCommon
    "stage_PatSmallS_0"      -> StageSubLevel 3 2  $ toDescription PatSmallS
    "stage_PatDiConsonant_0" -> StageSubLevel 3 3  $ toDescription PatDiConsonant
    "stage_PatCodaH_0"       -> StageSubLevel 3 4  $ toDescription PatCodaH
    "stage_PatCodaR_0"       -> StageSubLevel 3 5  $ toDescription PatCodaR
    "stage_PatCodaRR_0"      -> StageSubLevel 3 6  $ toDescription PatCodaRR
    "stage_PatCodaHR_0"      -> StageSubLevel 3 7  $ toDescription PatCodaHR
    "stage_PatDt_0"          -> StageSubLevel 3 8  $ toDescription PatDt
    "stage_PatDiphtong_0"    -> StageSubLevel 3 9  $ toDescription PatDiphtong
    "stage_PatReplC_0"       -> StageSubLevel 3 10 $ toDescription PatReplC
    "stage_PatCodaGK_0"      -> StageSubLevel 3 11 $ toDescription PatCodaGK
    "stage_PatSZ_0"          -> StageSubLevel 3 12 $ toDescription PatSZ
    "stage_PatIJ_0"          -> StageSubLevel 3 13 $ toDescription PatIJ
    "stage_PatSwapS_0"       -> StageSubLevel 3 14 $ toDescription PatSwapS
    "stage_PatSwapSch_0"     -> StageSubLevel 3 15 $ toDescription PatSwapSch
    "stage_PatSwapZ_0"       -> StageSubLevel 3 16 $ toDescription PatSwapZ
    "stage_PatDiVowel_0"     -> StageSubLevel 3 17 $ toDescription PatDiVowel
    "stage_PatReplH_0"       -> StageSubLevel 3 18 $ toDescription PatReplH
    "patternoverview"        -> StageTopLevel "Pattern overview"
    str          -> error $ "stageMeta: Invalid stage id: " <> Text.unpack str
