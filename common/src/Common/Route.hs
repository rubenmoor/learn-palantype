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

module Common.Route where

import           Data.Functor.Identity (Identity)
import           Data.Text             (replace, cons, Text)

import           Control.Category      ((<<<), Category (id, (.)))
import           Data.Either           (Either)
import           Data.Function         (($))
import           Data.Functor          ((<$>))
import           Data.List             (concat)
import           Data.Monoid           (Monoid (mempty))
import           Data.Traversable      (mapM)
import           Obelisk.Route         (unwrappedEncoder, singlePathSegmentEncoder, mkFullRouteEncoder, pattern (:/), Encoder,
                                        FullRoute (FullRoute_Backend), PageName,
                                        R, SegmentResult (PathEnd, PathSegment),
                                        unitEncoder)
import           Obelisk.Route.TH      (deriveRouteComponent)
import Data.Eq (Eq)
import GHC.Generics (Generic)
import Data.Ord (Ord)
import Text.Show (Show)
import Control.Lens.TH (makeWrapped)
import Data.Default (Default (def))
import TextShow (TextShow (showb), fromText)
import Data.Maybe (Maybe(..))
import Data.Text (uncons)
import Data.Char (toUpper)
import Data.Aeson (ToJSON)
import Data.Aeson (FromJSON)
import Data.String (IsString)

newtype Stage = Stage { unStage :: Text }
  deriving (Eq, Generic, IsString, Ord, Show)

makeWrapped ''Stage

capitalize :: Text -> Text
capitalize str = case uncons str of
  Nothing -> ""
  Just (h, rem) -> cons (toUpper h) rem

instance ToJSON Stage
instance FromJSON Stage

instance Default Stage where
    def = Stage "introduction"

instance TextShow Stage where
    showb = fromText
        <<< capitalize
        <<< replace "_" " "
        <<< replace "-" "."
        <<< unStage

--        Introduction -> "Introduction"
--        Stage1_1     -> "Stage 1.1"
--        Stage1_2     -> "Stage 1.2"
--        Stage1_3     -> "Stage 1.3"
--        Stage1_4     -> "Stage 1.4"
--        Stage1_5     -> "Stage 1.5"
--        Stage1_6     -> "Stage 1.6"
--        Stage1_7     -> "Stage 1.7"
--        Stage2_1     -> "Stage 2.1"
--        Stage2_2     -> "Stage 2.2"
--        Stage2_3     -> "Stage 2.3"
--        Stage2_4     -> "Stage 2.4"
--        PatternOverview -> "Pattern overview"

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute PageName

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_EN   :: FrontendRoute Stage
  FrontendRoute_DE   :: FrontendRoute Stage

-- data FrontendSubroute_Stage :: * -> * where
--   FrontendSubroute_Introduction :: FrontendSubroute_Stage ()
--   FrontendSubroute_Stage1_1 :: FrontendSubroute_Stage ()
--   FrontendSubroute_Stage1_2 :: FrontendSubroute_Stage ()
--   FrontendSubroute_Stage1_3 :: FrontendSubroute_Stage ()
--   FrontendSubroute_Stage1_4 :: FrontendSubroute_Stage ()
--   FrontendSubroute_Stage1_5 :: FrontendSubroute_Stage ()
--   FrontendSubroute_Stage1_6 :: FrontendSubroute_Stage ()
--   FrontendSubroute_Stage1_7 :: FrontendSubroute_Stage ()
--   FrontendSubroute_Stage2_1 :: FrontendSubroute_Stage ()
--   FrontendSubroute_Stage2_2 :: FrontendSubroute_Stage ()
--   FrontendSubroute_Stage2_3 :: FrontendSubroute_Stage ()
--   FrontendSubroute_Stage2_4 :: FrontendSubroute_Stage ()
--   FrontendSubroute_PatternOverview :: FrontendSubroute_Stage ()

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Api -> PathSegment "api" id
  )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_EN   -> PathSegment "EN" $ singlePathSegmentEncoder . unwrappedEncoder
      FrontendRoute_DE   -> PathSegment "DE" $ singlePathSegmentEncoder . unwrappedEncoder
  )
  -- where
  --   encoderSubrouteStage
  --     :: forall (check :: * -> *) (parse :: * -> *).
  --     ( MonadError Text check
  --     , check ~ parse
  --     )
  --     => Encoder check parse (R FrontendSubroute_Stage) PageName
  --   encoderSubrouteStage = pathComponentEncoder $ \case
  --     FrontendSubroute_Introduction -> PathSegment "introduction" $ unitEncoder mempty
  --     FrontendSubroute_Stage1_1 -> PathSegment "stage1-1" $ unitEncoder mempty
  --     FrontendSubroute_Stage1_2 -> PathSegment "stage1-2" $ unitEncoder mempty
  --     FrontendSubroute_Stage1_3 -> PathSegment "stage1-3" $ unitEncoder mempty
  --     FrontendSubroute_Stage1_4 -> PathSegment "stage1-4" $ unitEncoder mempty
  --     FrontendSubroute_Stage1_5 -> PathSegment "stage1-5" $ unitEncoder mempty
  --     FrontendSubroute_Stage1_6 -> PathSegment "stage1-6" $ unitEncoder mempty
  --     FrontendSubroute_Stage1_7 -> PathSegment "stage1-7" $ unitEncoder mempty
  --     FrontendSubroute_Stage2_1 -> PathSegment "stage2-1" $ unitEncoder mempty
  --     FrontendSubroute_Stage2_2 -> PathSegment "stage2-2" $ unitEncoder mempty
  --     FrontendSubroute_Stage2_3 -> PathSegment "stage2-3" $ unitEncoder mempty
  --     FrontendSubroute_Stage2_4 -> PathSegment "stage2-4" $ unitEncoder mempty
  --     FrontendSubroute_PatternOverview -> PathSegment "patternoverview" $ unitEncoder mempty

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
