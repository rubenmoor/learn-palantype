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
    , toTOCString
    , getGroupIndex
    , Stage(..)
    , StageSpecialGeneric (..)
    , StageHierarchy (..)
    , StageIndex (..)
    , mPrev
    , mNext
    )
where

import           Control.Category               ((<<<),  Category ((.)))
import           Data.Aeson                     ( ToJSON )
import           Data.Aeson                     ( FromJSON )
import           Data.Char                      (isLetter, isDigit,  toUpper )
import           Data.Default                   ( Default(def) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ((<&>), (<$>) )
import           Data.Int                       ( Int )
import           Data.List                      ((!!) )
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text
                                                )
import qualified Data.Text                     as Text
import           Data.Text                      ( uncons )
import           Data.Text                      ( cons )
import           GHC.Enum                       (pred, Enum,  succ
                                                )
import           GHC.Generics                   ( Generic )
import           Safe                           (readMay )
import           Text.Show                      ( show
                                                , Show
                                                )
import           TextShow                       (showt, TextShow(showb)
                                                , fromText
                                                )
import           Palantype.Common               (Greediness, PatternGroup )
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
import Data.Bool ((||))
import Data.Eq (Eq((/=)))
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Palantype.Common.Class (Palantype)
import GHC.Num ((+), (-), Num)
import Data.Ord ((>), Ord)
import Data.Foldable (Foldable(foldl'))
import Control.Lens.Wrapped (Wrapped)
import Palantype.Common (Palantype(toDescription))
import Data.Foldable (Foldable(length))

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
  deriving stock (Generic, Eq)

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

instance Palantype key => TextShow (Stage key) where
    showb (Stage sg h) = case sg of
      StageSpecial str -> fromText (capitalize str) <> case h of
        StageToplevel -> ""
        StageSublevel t s -> " " <> showb t <> "." <> showb s
      StageGeneric pg g -> fromText (capitalize $ showt pg) <> "-G" <> showb g

newtype StageIndex = StageIndex { unStageIndex :: Int }
  deriving stock (Eq, Generic, Ord)
  deriving newtype (Enum, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, Num, Read, Show)

instance Wrapped StageIndex

instance TextShow StageIndex where
  showb = showb <<< unStageIndex


instance Default (Stage key) where
    def = Stage (StageSpecial "introduction") StageToplevel

mPrev :: StageIndex -> Maybe StageIndex
mPrev i =
    if i == 0 then Nothing else Just $ pred i

mNext :: forall key. [Stage key] -> StageIndex -> Maybe StageIndex
mNext stages i =
    let max = length stages
    in  if unStageIndex i == max - 1 then Nothing else Just $ succ i

fromIndex :: forall key. [Stage key] -> StageIndex -> Stage key
fromIndex stages i = stages !! unStageIndex i

-- | create a link text for entries in the table-of-contents
--   if the greediness is bigger than 1, it is provided, too
toTOCString :: forall key. Palantype key => Stage key -> (Maybe Greediness, Text)
toTOCString (Stage sg h) = case sg of
        StageSpecial str -> case h of
            StageToplevel     -> (Nothing, str)
            StageSublevel _ s -> (Nothing, "Ex. " <> showt s <> ": " <> str)
        StageGeneric pg g -> case h of
            StageToplevel     -> (Nothing, "Error: generic stage on top level")
            StageSublevel _ s ->
                let mg = if g > 0 then Just g else Nothing
                in  (mg, "Ex. " <> showt s <> ": " <> toDescription pg)

getGroupIndex :: forall key. Stage key -> Maybe Int
getGroupIndex (Stage _ h) = case h of
    StageToplevel     -> Nothing
    StageSublevel t _ -> Just t

findStage
  :: forall key
  . Palantype key
  => [Stage key]
  -> StageSpecialGeneric key
  -> Maybe (StageIndex, Int, Int)
findStage stages ssg =
    let (i, mResult) = foldl' acc (0, Nothing) stages
    in  mResult <&> \(t, s) -> (StageIndex i, t, s)
  where
    acc
      :: (Int, Maybe (Int, Int))
      -> Stage key
      -> (Int, Maybe (Int, Int))
    acc r@(_, Just _) _                                           = r
    acc (n, Nothing) (Stage (StageSpecial _) _                  ) = (n + 1, Nothing)
    acc (n, Nothing) (Stage _                StageToplevel      ) = (n + 1, Nothing)
    acc (n, Nothing) (Stage ssg'             (StageSublevel t s)) =
        if show ssg' == show ssg
        then (n, Just (t, s))
        else (n + 1, Nothing)
