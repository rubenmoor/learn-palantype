{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.Stage
    ( fromIndex
    , findStage
    , getLang
    , toTOCString
    , getGroupIndex
    , Stage(..)
    , StageRepr ()
    , StageSpecialGeneric (..)
    , StageHierarchy (..)
    , StageIndex
    , mPrev
    , mNext
    ,isValidIndex,findStageIndex,toStageRepr,showShort)
where

import           Control.Category               ((<<<),  Category ((.)))
import           Data.Aeson                     ( ToJSON )
import           Data.Aeson                     ( FromJSON )
import           Data.Char                      (isLetter, isDigit,  toUpper )
import           Data.Default                   ( Default(def) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ((<&>), (<$>) )
import           Data.Int                       ( Int )
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text
                                                )
import qualified Data.Text                     as Text
import           Data.Text                      ( uncons )
import           Data.Text                      ( cons )
import           GHC.Enum                       (pred, Enum,  succ
                                                )
import           GHC.Generics                   ( Generic )
import           Safe                           (atMay, readMay )
import           Text.Show                      ( show
                                                , Show
                                                )
import           TextShow                       (showt, TextShow(showb)
                                                , fromText
                                                )
import           Palantype.Common               (Lang (..), Greediness, PatternGroup )
import           Text.Read                      (read
                                                , Read(readPrec)
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Data.Eq                        ( Eq((==)) )
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadP (option, sepBy1, munch1, char)
import Text.ParserCombinators.ReadP (pfail)
import Control.Monad (Monad((>>=)))
import Data.Bool (Bool, otherwise, (||), (&&))
import Data.Eq (Eq((/=)))
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Palantype.Common.Class (Palantype)
import GHC.Num ((+), (-), Num)
import Data.Ord ((<), (>), Ord)
import Data.Foldable (Foldable(foldl'))
import Control.Lens.Wrapped (Wrapped)
import Palantype.Common (Palantype(toDescription))
import Data.Foldable (Foldable(length))
import Type.Reflection (eqTypeRep, (:~~:)(HRefl), typeRep)
import qualified Palantype.DE as DE
import qualified Palantype.EN as EN
import Data.Tuple (snd)
import Palantype.Common.TH (failure)
import Data.List ((!!))

data StageSpecialGeneric key
  = StageSpecial Text
  | StageGeneric (PatternGroup key) Greediness
  deriving stock (Generic)

instance Palantype key => Show (StageSpecialGeneric key)  where
  show (StageSpecial str  ) = Text.unpack str
  show (StageGeneric pg  g) = show (pg :: PatternGroup key) <> "_" <> show g

instance Palantype key => Read (StageSpecialGeneric key) where
  readPrec = lift $
      option Nothing generic >>= \case
        Just (pg, g) -> pure $ StageGeneric pg g
        Nothing      -> special

    where
      generic = do
        str <- munch1 (/= '-')
        pg <- maybe pfail pure $ readMay str
        _ <- char '-'
        g <- read <$> munch1 isDigit
        pure $ Just (pg, g)

      special = StageSpecial . Text.pack <$> munch1 isLetter

instance Palantype key => ToJSON (StageSpecialGeneric key)
instance Palantype key => FromJSON (StageSpecialGeneric key)
deriving instance Palantype key => Eq (StageSpecialGeneric key)

data StageHierarchy
  = StageToplevel
  | StageSublevel Int Int
  deriving stock (Generic, Eq, Ord)

instance Show StageHierarchy where
  show StageToplevel = "-"
  show (StageSublevel t s) = show t <> "-" <> show s

instance Read StageHierarchy where
  readPrec = lift $
    option Nothing top >>= \case
      Just _ -> pure StageToplevel
      Nothing -> sub
    where
      top = Just <$> char '-'
      sub = do
        t <- read <$> munch1 isDigit
        _ <- char '-'
        s <- read <$> munch1 isDigit
        pure $ StageSublevel t s

instance ToJSON StageHierarchy
instance FromJSON StageHierarchy

data Stage key = Stage
  { stageSpecialGeneric :: StageSpecialGeneric key
  , stageHierarchy      :: StageHierarchy
  }
  deriving stock (Generic, Eq)

instance Palantype key => ToJSON (Stage key)
instance Palantype key => FromJSON (Stage key)

instance Palantype key => Show (Stage key) where
  show (Stage sg h) = show sg <> "_" <> show h

instance Palantype key => Read (Stage key) where
  readPrec = lift $
      munch1 isLetterOrHyphen `sepBy1` char '_' >>= \case
          str1 : str2 : [] -> pure $ Stage (read str1) (read str2)
          _                -> pfail
    where
      isLetterOrHyphen l = l == '-' || isLetter l

capitalize :: Text -> Text
capitalize str = case uncons str of
    Nothing       -> ""
    Just (h, rem) -> cons (toUpper h) rem

showShort :: Stage key -> Text
showShort (Stage _ (StageSublevel t s)) = "Stage " <> showt t <> "." <> showt s
showShort (Stage (StageSpecial str) StageToplevel) = str
showShort (Stage (StageGeneric _ _) StageToplevel) = $failure "generic stage on top-level"

instance Palantype key => TextShow (Stage key) where
    showb (Stage sg h) = case sg of
      StageSpecial str -> fromText (capitalize str) <> case h of
        StageToplevel -> ""
        StageSublevel t s -> " " <> showb t <> "." <> showb s
      StageGeneric pg g -> fromText (capitalize $ showt pg) <> "-G" <> showb g

-- stage index

-- | allow to identify a stage by some integer number
-- | This number changes when stages are reordered
-- | Thus it can be used routes but shouldn't be stored in the database
-- | For long-term storage, there is 'StageRepr'
newtype StageIndex = StageIndex { unStageIndex :: Int }
  deriving stock (Eq, Generic, Ord)
  deriving newtype (Enum, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, Num, Read, Show)

instance Wrapped StageIndex

instance TextShow StageIndex where
  showb = showb <<< unStageIndex

-- stage representation

data StageSpecialGenericRepr
  = StageReprSpecial Text
  | StageReprGeneric Text Greediness
  deriving stock (Eq, Generic, Ord)

instance ToJSON StageSpecialGenericRepr
instance FromJSON StageSpecialGenericRepr

-- | Use the 'TextShow'
-- | instance of 'PatternGroup key' to convert a 'Stage key' in a one-way
-- | manner into a legible representation of a stage
data StageRepr = StageRepr
    { srLang :: Lang
    , srStageSpecialGeneric :: StageSpecialGenericRepr
    , srStageHierarchy :: StageHierarchy
    }
    deriving stock (Eq, Generic, Ord)

instance TextShow StageRepr where
  showb (StageRepr lang sg h) = showb lang <> "/" <> case sg of
         StageReprSpecial str -> fromText (capitalize str) <> case h of
            StageToplevel -> ""
            StageSublevel t s -> " " <> showb t <> "." <> showb s
         StageReprGeneric str g -> fromText str <> "-G" <> showb g

instance ToJSON StageRepr
instance FromJSON StageRepr

-- stages

stages
  :: forall key
  . ( Palantype key
    ) => [Stage key]
stages =
    [ Stage (StageSpecial "Introduction"            ) StageToplevel
    , Stage (StageSpecial "Type the letters"        ) (StageSublevel 1 1)
    , Stage (StageSpecial "Memorize the order"      ) (StageSublevel 1 2)
    , Stage (StageSpecial "Type the letters blindly") (StageSublevel 1 3)
    , Stage (StageSpecial "Memorize the order blindly") (StageSublevel 1 4)
    , Stage (StageSpecial "Memorize the left hand"  ) (StageSublevel 1 5)
    , Stage (StageSpecial "Memorize the right hand" ) (StageSublevel 1 6)
    , Stage (StageSpecial "Memorize home row"       ) (StageSublevel 1 7)
    , Stage (StageSpecial "Memorize them all"       ) (StageSublevel 1 8)
    , Stage (StageSpecial "Building muscle memory"  ) (StageSublevel 2 1)
    , Stage (StageSpecial "Learn your first chords" ) (StageSublevel 2 2)
    ] <> if
        | Just HRefl <- typeRep @key `eqTypeRep` typeRep @DE.Key -> stagesDE
        | Just HRefl <- typeRep @key `eqTypeRep` typeRep @EN.Key -> [] -- TODO
        | otherwise -> []

stagesDE
  :: [Stage DE.Key]
stagesDE =
    [ Stage (StageSpecial "Onset, nucleus, and coda") (StageSublevel 2 3)
    , Stage (StageSpecial "Syllabes and word parts" ) (StageSublevel 2 4)
    , Stage (StageGeneric DE.PatReplCommon1  0) (StageSublevel 3  1)
    , Stage (StageGeneric DE.PatReplCommon2  0) (StageSublevel 3  2)
    , Stage (StageGeneric DE.PatCodaComboT   0) (StageSublevel 3  3)
    , Stage (StageGeneric DE.PatReplCommon1  2) (StageSublevel 4  1)
    , Stage (StageGeneric DE.PatReplCommon1  3) (StageSublevel 4  2)
    , Stage (StageGeneric DE.PatReplCommon2  4) (StageSublevel 4  3)
    , Stage (StageGeneric DE.PatOnsetR       0) (StageSublevel 5  1)
    , Stage (StageGeneric DE.PatOnsetL       0) (StageSublevel 5  2)
    , Stage (StageGeneric DE.PatDiConsonant  0) (StageSublevel 5  3)
    , Stage (StageGeneric DE.PatDiConsonant  2) (StageSublevel 5  4)
    , Stage (StageGeneric DE.PatCodaH        0) (StageSublevel 6  1)
    , Stage (StageGeneric DE.PatCodaH        1) (StageSublevel 6  2)
    , Stage (StageGeneric DE.PatCodaR        0) (StageSublevel 6  3)
    , Stage (StageGeneric DE.PatCodaR        4) (StageSublevel 6  4)
    , Stage (StageGeneric DE.PatCodaRR       0) (StageSublevel 6  5)
    , Stage (StageGeneric DE.PatCodaHR       0) (StageSublevel 6  6)
    , Stage (StageGeneric DE.PatDt           0) (StageSublevel 7  1)
    , Stage (StageGeneric DE.PatDt           2) (StageSublevel 7  2)
    , Stage (StageGeneric DE.PatDiphtong     0) (StageSublevel 8  1)
    , Stage (StageGeneric DE.PatDiphtong     4) (StageSublevel 8  2)
    , Stage (StageGeneric DE.PatDiphtong     5) (StageSublevel 8  3)
    , Stage (StageGeneric DE.PatReplC        0) (StageSublevel 9  1)
    , Stage (StageGeneric DE.PatReplC        2) (StageSublevel 9  2)
    , Stage (StageGeneric DE.PatBreakUpI     0) (StageSublevel 9  3)
    , Stage (StageGeneric DE.PatBreakUpI     4) (StageSublevel 9  4)
    , Stage (StageGeneric DE.PatSwapS        0) (StageSublevel 10 1)
    , Stage (StageGeneric DE.PatSwapS        2) (StageSublevel 10 2)
    , Stage (StageGeneric DE.PatSwapSch      0) (StageSublevel 10 3)
    , Stage (StageGeneric DE.PatSwapSch      2) (StageSublevel 10 4)
    , Stage (StageGeneric DE.PatSwapZ        0) (StageSublevel 10 5)
    , Stage (StageGeneric DE.PatDiVowel      0) (StageSublevel 11 1)
    , Stage (StageGeneric DE.PatDiVowel      1) (StageSublevel 11 2)
    , Stage (StageGeneric DE.PatDiVowel      4) (StageSublevel 11 3)
    , Stage (StageGeneric DE.PatCodaGK       3) (StageSublevel 11 4)
    , Stage (StageGeneric DE.PatReplH        0) (StageSublevel 12 1)
    , Stage (StageGeneric DE.PatReplH        3) (StageSublevel 12 2)
    , Stage (StageGeneric DE.PatReplRare     0) (StageSublevel 13 1)
    , Stage (StageGeneric DE.PatReplRare     3) (StageSublevel 13 2)
    , Stage (StageGeneric DE.PatSmallS       0) (StageSublevel 13 3)
    , Stage (StageGeneric DE.PatSmallS       6) (StageSublevel 13 4)
    , Stage (StageGeneric DE.PatBrief        0) (StageSublevel 14 1)
    , Stage (StageSpecial "Plover Commands"   ) (StageSublevel 15 1)
    , Stage (StageSpecial "Fingerspelling"    ) (StageSublevel 15 2)
    , Stage (StageSpecial "Number Mode"       ) (StageSublevel 15 3)
    , Stage (StageSpecial "Command Keys"      ) (StageSublevel 15 4)
    , Stage (StageSpecial "Special Characters") (StageSublevel 15 5)
    , Stage (StageSpecial "Pattern Overview"  ) StageToplevel
    ]

instance Palantype key => Default (Stage key) where
    def = stages !! 0

-- functions

mPrev :: StageIndex -> Maybe StageIndex
mPrev i =
    if i == 0 then Nothing else Just $ pred i

mNext :: forall key. Palantype key => StageIndex -> Maybe StageIndex
mNext i =
    let max = length $ stages @key
    in  if unStageIndex i == max - 1 then Nothing else Just $ succ i

fromIndex :: forall key. Palantype key => StageIndex -> Maybe (Stage key)
fromIndex i = stages `atMay` unStageIndex i

-- | create a link text for entries in the table-of-contents
--   if the greediness is bigger than 1, it is provided, too
toTOCString :: forall key. Palantype key => Stage key -> (Text, Maybe Greediness, Text)
toTOCString (Stage sg h) = case sg of
        StageSpecial str -> case h of
          StageToplevel -> ("", Nothing, str)
          StageSublevel _ s -> ("Ex. " <> showt s <> ": ", Nothing, str)
        StageGeneric pg g -> case h of
            StageToplevel     -> $failure "Error: generic stage on top level"
            StageSublevel _ s ->
                let mg = if g > 0 then Just g else Nothing
                in  ("Ex. " <> showt s <> ": ", mg, toDescription pg)

getGroupIndex :: forall key. Stage key -> Maybe Int
getGroupIndex (Stage _ h) = case h of
    StageToplevel     -> Nothing
    StageSublevel t _ -> Just t

findStage
  :: forall key
  . Palantype key
  => StageSpecialGeneric key
  -> Maybe (StageIndex, Int, Int)
findStage ssg =
    let (i, mResult) = foldl' acc (0, Nothing) stages
    in  mResult <&> \(t, s) -> (StageIndex i, t, s)
  where
    acc
      :: (Int, Maybe (Int, Int))
      -> Stage key
      -> (Int, Maybe (Int, Int))
    acc r@(_, Just _) _                                           = r
    acc (n, Nothing) (Stage ssg' (StageSublevel t s)) | ssg' == ssg = (n, Just (t, s))
    acc (n, Nothing) _ = (n + 1, Nothing)


findStageIndex :: StageRepr -> Maybe StageIndex
findStageIndex (StageRepr lang sg h) =
    case lang of
      EN -> snd $ foldl' acc (0, Nothing) $ stages @EN.Key
      DE -> snd $ foldl' acc (0, Nothing) $ stages @DE.Key
  where
    acc :: forall key. Palantype key => (Int, Maybe StageIndex) -> Stage key -> (Int, Maybe StageIndex)
    acc r@(_, Just _)  _ = r
    acc (i, Nothing ) (Stage (StageSpecial str') h') =
      case sg of
        StageReprSpecial str | str == str' && h == h' -> (i, Just $ StageIndex i)
        _ -> (i + 1, Nothing)
    acc (i, Nothing ) (Stage (StageGeneric pg' g') _) =
      case sg of
        StageReprGeneric str g | str == showt pg' && g == g' -> (i, Just $ StageIndex i)
        _ -> (i + 1, Nothing)

isValidIndex :: forall key. Palantype key => StageIndex -> Bool
isValidIndex (StageIndex i) = i > 0 && i < length (stages @key)

getLang :: forall key. Palantype key => Lang
getLang = if
  | Just HRefl <- typeRep @key `eqTypeRep` typeRep @EN.Key -> EN
  | Just HRefl <- typeRep @key `eqTypeRep` typeRep @DE.Key -> DE
  | otherwise -> $failure "key not implemented"

toStageRepr :: forall key. Palantype key => Stage key -> StageRepr
toStageRepr (Stage sg h) =
    StageRepr (getLang @key) (toRepr sg) h
  where
    toRepr (StageSpecial str ) = StageReprSpecial str
    toRepr (StageGeneric pg g) = StageReprGeneric (showt pg) g
