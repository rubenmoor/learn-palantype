{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BlockArguments #-}
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
    ( Stage(..)
    , mPrev
    , mNext
    )
where

import           Control.Category               ( (<<<), Category ((.)))
import           Data.Aeson                     ( ToJSON )
import           Data.Aeson                     ( FromJSON )
import           Data.Char                      (isLetter, isDigit,  toUpper )
import           Data.Default                   ( Default(def) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ((<$>) )
import           Data.Int                       ( Int )
import           Data.List                      ((!!),  elemIndex )
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text
                                                )
import qualified Data.Text                     as Text
import           Data.Text                      ( uncons )
import           Data.Text                      ( cons )
import           GHC.Base                       ( Maybe(..)
                                                )
import           GHC.Enum                       ( succ
                                                )
import           GHC.Generics                   ( Generic )
import           Safe                           (readMay,  atMay )
import           Text.Show                      ( show
                                                , Show
                                                )
import           TextShow                       (showt, TextShow(showb)
                                                , fromText
                                                )
import           Palantype.Common               (Greediness, PatternGroup )
import qualified Palantype.DE as DE
import           Text.Read                      (readEither, read
                                                , Read(readPrec)
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Data.Eq                        ( Eq((==)) )
import Web.HttpApiData (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))
import qualified Palantype.EN.Keys as EN
import Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadP (option, sepBy1, munch1, char)
import Text.ParserCombinators.ReadP (pfail)
import Control.Monad (Monad((>>=)))
import Data.Bool ((||))
import Data.Eq (Eq((/=)))
import Data.Maybe (maybe)
import Data.Either.Combinators (mapLeft)
import Palantype.Common.Class (Palantype)

data StageSpecialGeneric key
  = StageSpecial Text
  | StageGeneric (PatternGroup key) Greediness
  deriving stock (Generic)

instance Palantype key => Show (StageSpecialGeneric key)  where
  show (StageSpecial str  ) = Text.unpack str
  show (StageGeneric pg  g) = show pg <> "_" <> show g

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

instance Default (Stage key) where
    def = stages !! 0

capitalize :: Text -> Text
capitalize str = case uncons str of
    Nothing       -> ""
    Just (h, rem) -> cons (toUpper h) rem

instance Palantype key => TextShow (Stage key) where
    showb (Stage sg h) = case sg of
      StageSpecial str -> fromText (capitalize str) <> case h of
        StageToplevel -> ""
        StageSublevel t s -> " " <> showb t <> "." <> showb s
      StageGeneric pg g -> fromText (capitalize $ showt pg) <> "-" <> showb g


stages :: forall key. [Stage key]
stages =
    [ Stage (StageSpecial "introduction"  ) StageToplevel
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 1)
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 2)
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 3)
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 4)
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 5)
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 6)
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 7)
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 8)
    , Stage (StageSpecial "stage"         ) (StageSublevel 2 1)
    , Stage (StageSpecial "stage"         ) (StageSublevel 2 2)
    , Stage (StageSpecial "stage"         ) (StageSublevel 2 3)
    ]

stagesDE :: [Stage DE.Key]
stagesDE = stages <>
  [ Stage (StageGeneric DE.PatSimple      0) (StageSublevel 2 4)

  , Stage (StageGeneric DE.PatReplCommon1 0) (StageSublevel 3 1)
  , Stage (StageGeneric DE.PatReplCommon2 0) (StageSublevel 3 2)
  , Stage (StageGeneric DE.PatCodaComboT  0) (StageSublevel 3 3)
  , Stage (StageGeneric DE.PatReplCommon1 2) (StageSublevel 4 1)
  , Stage (StageGeneric DE.PatReplCommon1 3) (StageSublevel 4 2)
  , Stage (StageGeneric DE.PatReplCommon2 4) (StageSublevel 4 3)
  , Stage (StageGeneric DE.PatOnsetR      0) (StageSublevel 5 1)
  , Stage (StageGeneric DE.PatOnsetL      0) (StageSublevel 5 2)
  , Stage (StageGeneric DE.PatDiConsonant 0) (StageSublevel 5 3)
  , Stage (StageGeneric DE.PatDiConsonant 2) (StageSublevel 5 4)
  , Stage (StageGeneric DE.PatCodaH       0) (StageSublevel 6 1)
  , Stage (StageGeneric DE.PatCodaH       1) (StageSublevel 6 2)
  , Stage (StageGeneric DE.PatCodaR       0) (StageSublevel 6 3)
  , Stage (StageGeneric DE.PatCodaR       4) (StageSublevel 6 4)
  , Stage (StageGeneric DE.PatCodaRR      0) (StageSublevel 6 5)
  , Stage (StageGeneric DE.PatCodaHR      0) (StageSublevel 6 6)

  -- , Stage (StageGeneric PatDt          0)
  -- , Stage (StageGeneric PatDiphtong    0)
  -- , Stage (StageGeneric PatReplC       0)
  -- , Stage (StageGeneric PatBreakUpI    0)
  -- , Stage (StageGeneric PatSwapS       0)
  -- , Stage (StageGeneric PatSwapSch     0)
  -- , Stage (StageGeneric PatSwapZ       0)
  -- , Stage (StageGeneric PatDiVowel     0)
  -- , Stage (StageGeneric PatReplH       0)
  -- , Stage (StageGeneric PatCodaGK      3)
  -- , Stage (StageGeneric PatReplRare    0)  -- 3.23
  -- , Stage (StageGeneric PatSmallS      0)
  , Stage (StageSpecial "ploverCommands") (StageSublevel 40 1)
  , Stage (StageSpecial "fingerspelling") (StageSublevel 40 2)
  , Stage (StageSpecial "numbermode"    ) (StageSublevel 40 3)
  , Stage (StageSpecial "commandKeys"   ) (StageSublevel 40 4)
  , Stage (StageSpecial "specialCharacters") (StageSublevel 40 5)
  , Stage (StageGeneric DE.PatBrief      0) (StageSublevel 50 1)
  , Stage (StageSpecial "patternoverview") StageToplevel
  ]

stagesEN :: [Stage EN.Key]
stagesEN = stages <>
  [ Stage (StageGeneric EN.PatSimple 0) (StageSublevel 2 4)
  , Stage (StageGeneric EN.PatSimpleMulti 0) (StageSublevel 2 4)
  ]

mPrev :: forall key. Palantype key => Stage key -> Maybe (Stage key)
mPrev stage = Nothing
    -- if
    --     | typeRep (Proxy :: Proxy key) == typeRep pDE -> do
    --         i <- elemIndex stage stagesDE
    --         stagesDE `atMay` pred i
    --     | typeRep (Proxy :: Proxy key) == typeRep pEN -> do
    --         i <- elemIndex stage stagesEN
    --         stagesEN `atMay` pred i
    --     | otherwise -> Nothing

mNext :: forall key. Palantype key => (Stage key) -> Maybe (Stage key)
mNext stage = do
    i <- elemIndex stage stages
    stages `atMay` succ i

instance Palantype key => ToHttpApiData (Stage key) where
  toUrlPiece = Text.pack <<< show

instance Palantype key => FromHttpApiData (Stage key) where
  parseUrlPiece =
    mapLeft Text.pack <<< readEither <<< Text.unpack

    -- if str `elem` strsStage
    -- then Right $ Stage str
    -- else Left $ "parseUrlPiece: " <> str <> ": no parse"
