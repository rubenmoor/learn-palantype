{-# LANGUAGE MultiWayIf #-}
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
    ( Stage()
    , mPrev
    , mNext
    , stageMeta
    , StageMeta(..)
    )
where

import           Control.Category               ( (<<<) )
import           Control.Lens.TH                ( makeWrapped )
import           Data.Aeson                     ( ToJSON )
import           Data.Aeson                     ( FromJSON )
import           Data.Char                      ( toUpper )
import           Data.Default                   ( Default(def) )
import           Data.Eq                        ( Eq )
import           Data.Foldable                  ( Foldable(elem) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>) )
import           Data.Int                       ( Int )
import           Data.List                      ( elemIndex )
import           Data.Monoid                    ( (<>) )
import           Data.Ord                       ( Ord )
import           Data.Text                      ( Text
                                                , replace
                                                )
import qualified Data.Text                     as Text
import           Data.Text                      ( uncons )
import           Data.Text                      ( cons )
import           GHC.Base                       ( Maybe(..)
                                                , error
                                                )
import           GHC.Enum                       ( Enum(pred)
                                                , succ
                                                )
import           GHC.Generics                   ( Generic )
import           Safe                           ( atMay )
import           Text.Show                      ( show
                                                , Show
                                                )
import           TextShow                       ( TextShow(showb)
                                                , fromText
                                                )
import           Palantype.Common               ( Palantype(toDescription) )
import           Palantype.DE                   ( Pattern(..) )
import           Text.Read                      ( readMaybe
                                                , get
                                                , Read(readPrec)
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Monad                  ( guard )
import           Palantype.Common.TH            ( readLoc )
import           Control.Applicative            ( Alternative(some) )
import           Data.Bool                      ( otherwise )
import           Data.Eq                        ( Eq((==)) )
import Web.HttpApiData (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))
import Data.Either (Either(..))

{-|
The complete type is given along with `strsStage`.
Any string within that list is a valid stage,
serving as route slug.
-}
newtype Stage = Stage {unStage :: Text}
    deriving stock (Eq, Generic, Ord)

makeWrapped ''Stage

instance ToJSON Stage
instance FromJSON Stage

instance Show Stage where
  show = Text.unpack <<< unStage

instance Read Stage where
    readPrec = do
        str <- Text.pack <$> some get
        guard $ str `elem` strsStage
        pure $ Stage str

instance Default Stage where
    def = $readLoc "introduction"

capitalize :: Text -> Text
capitalize str = case uncons str of
    Nothing       -> ""
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
    [ "introduction"
    , "stage_1-1"
    , "stage_1-2"
    , "stage_1-3"
    , "stage_1-4"
    , "stage_1-5"
    , "stage_1-6"
    , "stage_1-7"
    , "stage_1-8"
    , "stage_2-1"
    , "stage_2-2"
    , "stage_2-3"
    , "stage_PatSimple_0" -- 2.4
    , "stage_PatReplCommon1_0" -- 3.1
    , "stage_PatReplCommon2_0" -- 3.2
    , "stage_PatCodaComboT_0"
    , "stage_PatOnsetR_0"
    , "stage_PatOnsetL_0"
    , "stage_PatSmallS_0"
    , "stage_PatDiConsonant_0"
    , "stage_PatCodaH_0"
    , "stage_PatCodaR_0"
    , "stage_PatCodaRR_0"
    , "stage_PatCodaHR_0"
    , "stage_PatDt_0"
    , "stage_PatDiphtong_0"
    , "stage_PatReplC_0"
    , "stage_PatSZ_0"
    , "stage_PatBreakUpI_0"
    , "stage_PatSwapS_0"
    , "stage_PatSwapSch_0"
    , "stage_PatSwapZ_0"
    , "stage_PatDiVowel_0"
    , "stage_PatReplH_0"
    , "stage_PatCodaGK_3"
    , "stage_PatReplRare_0"  -- 3.23
    , "stage_ploverCommands" -- 4.1
    , "stage_fingerspelling"
    , "stage_numbermode"
    , "stage_commandKeys"
    , "stage_specialCharacters"
    , "patternoverview"
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
    showb (StageSubLevel s i str) =
        showb s <> "." <> showb i <> ": " <> fromText str

stageMeta :: Stage -> StageMeta
stageMeta stage = if
    | $readLoc "introduction"            == stage -> StageTopLevel "Introduction"
    | $readLoc "stage_1-1"               == stage -> StageSubLevel 1 1 "Type the letters"
    | $readLoc "stage_1-2"               == stage -> StageSubLevel 1 2 "Memorize the order"
    | $readLoc "stage_1-3"               == stage -> StageSubLevel 1 3 "Type the letters blindly"
    | $readLoc "stage_1-4"               == stage -> StageSubLevel 1 4 "Memorize the order blindly"
    | $readLoc "stage_1-5"               == stage -> StageSubLevel 1 5 "Memorize the left hand"
    | $readLoc "stage_1-6"               == stage -> StageSubLevel 1 6 "Memorize the right hand"
    | $readLoc "stage_1-7"               == stage -> StageSubLevel 1 7 "Memorize home row"
    | $readLoc "stage_1-8"               == stage -> StageSubLevel 1 8 "Memorize them all"
    | $readLoc "stage_2-1"               == stage -> StageSubLevel 2 1 "Building muscle memory"
    | $readLoc "stage_2-2"               == stage -> StageSubLevel 2 2 "Learn your first chords"
    | $readLoc "stage_2-3"               == stage -> StageSubLevel 2 3 "Onset, nucleus, and coda"
    | $readLoc "stage_PatSimple_0"       == stage -> StageSubLevel 2 4 "Syllables and word parts"
    | $readLoc "stage_PatReplCommon1_0"  == stage -> StageSubLevel 3 1  $ toDescription PatReplCommon1
    | $readLoc "stage_PatReplCommon2_0"  == stage -> StageSubLevel 3 2  $ toDescription PatReplCommon2
    | $readLoc "stage_PatCodaComboT_0"   == stage -> StageSubLevel 3 3  $ toDescription PatCodaComboT
    | $readLoc "stage_PatOnsetR_0"       == stage -> StageSubLevel 3 4  $ toDescription PatOnsetR
    | $readLoc "stage_PatOnsetL_0"       == stage -> StageSubLevel 3 5  $ toDescription PatOnsetL
    | $readLoc "stage_PatSmallS_0"       == stage -> StageSubLevel 3 6  $ toDescription PatSmallS
    | $readLoc "stage_PatDiConsonant_0"  == stage -> StageSubLevel 3 7  $ toDescription PatDiConsonant
    | $readLoc "stage_PatCodaH_0"        == stage -> StageSubLevel 3 8  $ toDescription PatCodaH
    | $readLoc "stage_PatCodaR_0"        == stage -> StageSubLevel 3 9  $ toDescription PatCodaR
    | $readLoc "stage_PatCodaRR_0"       == stage -> StageSubLevel 3 10  $ toDescription PatCodaRR
    | $readLoc "stage_PatCodaHR_0"       == stage -> StageSubLevel 3 11 $ toDescription PatCodaHR
    | $readLoc "stage_PatDt_0"           == stage -> StageSubLevel 3 12 $ toDescription PatDt
    | $readLoc "stage_PatDiphtong_0"     == stage -> StageSubLevel 3 13 $ toDescription PatDiphtong
    | $readLoc "stage_PatReplC_0"        == stage -> StageSubLevel 3 14 $ toDescription PatReplC
    | $readLoc "stage_PatSZ_0"           == stage -> StageSubLevel 3 15 $ toDescription PatSZ
    | $readLoc "stage_PatBreakUpI_0"     == stage -> StageSubLevel 3 16 $ toDescription PatBreakUpI
    | $readLoc "stage_PatSwapS_0"        == stage -> StageSubLevel 3 17 $ toDescription PatSwapS
    | $readLoc "stage_PatSwapSch_0"      == stage -> StageSubLevel 3 18 $ toDescription PatSwapSch
    | $readLoc "stage_PatSwapZ_0"        == stage -> StageSubLevel 3 19 $ toDescription PatSwapZ
    | $readLoc "stage_PatDiVowel_0"      == stage -> StageSubLevel 3 20 $ toDescription PatDiVowel
    | $readLoc "stage_PatReplH_0"        == stage -> StageSubLevel 3 21 $ toDescription PatReplH
    | $readLoc "stage_PatCodaGK_3"       == stage -> StageSubLevel 3 22 $ toDescription PatCodaGK
    | $readLoc "stage_PatReplRare_0"     == stage -> StageSubLevel 3 23 $ toDescription PatReplRare
    | $readLoc "stage_ploverCommands"    == stage -> StageSubLevel 4 1 "Formatting input"
    | $readLoc "stage_fingerspelling"    == stage -> StageSubLevel 4 2 "Fingerspelling"
    | $readLoc "stage_numbermode"        == stage -> StageSubLevel 4 3 "Number input"
    | $readLoc "stage_commandKeys"       == stage -> StageSubLevel 4 4 "Command keys"
    | $readLoc "stage_specialCharacters" == stage -> StageSubLevel 4 5 "Special characters"
    | $readLoc "patternoverview" == stage -> StageTopLevel "Pattern overview"
    | otherwise -> error $ "stageMeta: pattern missing: " <> show stage

instance ToHttpApiData Stage where
  toUrlPiece = unStage

instance FromHttpApiData Stage where
  parseUrlPiece str =
    if str `elem` strsStage
    then Right $ Stage str
    else Left $ "parseUrlPiece: " <> str <> ": no parse"
