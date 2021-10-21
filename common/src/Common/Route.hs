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

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import           Data.Functor.Identity (Identity)
import           Data.Text             (Text)

import           Control.Category      (Category((.), id))
import           Data.Either           (Either)
import           Data.Function         (($))
import           Data.Functor          (Functor(fmap), (<$>))
import           Data.List             (concat)
import           Data.Monoid
import           Data.Traversable      (mapM)
import           Obelisk.Route         (maybeEncoder, pathComponentEncoder, pathSegmentEncoder, pattern (:/), Encoder,
                                        FullRoute (FullRoute_Backend), PageName,
                                        R, SegmentResult (PathEnd, PathSegment),
                                        mkFullRouteEncoder, unitEncoder, singlePathSegmentEncoder)
import           Obelisk.Route.TH      (deriveRouteComponent)
import Data.Maybe (Maybe)
import Control.Monad.Except (MonadError)

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute PageName
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_EN   :: FrontendRoute (R FrontendRouteStage)
  FrontendRoute_DE   :: FrontendRoute (R FrontendRouteStage)

data FrontendRouteStage :: * -> * where
  FrontendRoute_Introduction :: FrontendRouteStage ()
  FrontendRoute_Stage1_1 :: FrontendRouteStage ()
  FrontendRoute_Stage1_2 :: FrontendRouteStage ()
  FrontendRoute_Stage1_3 :: FrontendRouteStage ()
  FrontendRoute_Stage1_4 :: FrontendRouteStage ()
  FrontendRoute_Stage1_5 :: FrontendRouteStage ()
  FrontendRoute_Stage1_6 :: FrontendRouteStage ()
  FrontendRoute_Stage1_7 :: FrontendRouteStage ()
  FrontendRoute_Stage2_1 :: FrontendRouteStage ()
  FrontendRoute_Stage2_2 :: FrontendRouteStage ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

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
      FrontendRoute_EN   -> PathSegment "EN" encoderStage
      FrontendRoute_DE   -> PathSegment "DE" encoderStage
  )
  where
    encoderStage
      :: forall (check :: * -> *) (parse :: * -> *).
      ( MonadError Text check
      , check ~ parse
      )
      => Encoder check parse (R FrontendRouteStage) PageName
    encoderStage = pathComponentEncoder $ \case
      FrontendRoute_Introduction -> PathSegment "introduction" $ unitEncoder mempty
      FrontendRoute_Stage1_1 -> PathSegment "stage1-1" $ unitEncoder mempty
      FrontendRoute_Stage1_2 -> PathSegment "stage1-2" $ unitEncoder mempty
      FrontendRoute_Stage1_3 -> PathSegment "stage1-3" $ unitEncoder mempty
      FrontendRoute_Stage1_4 -> PathSegment "stage1-4" $ unitEncoder mempty
      FrontendRoute_Stage1_5 -> PathSegment "stage1-5" $ unitEncoder mempty
      FrontendRoute_Stage1_6 -> PathSegment "stage1-6" $ unitEncoder mempty
      FrontendRoute_Stage1_7 -> PathSegment "stage1-7" $ unitEncoder mempty
      FrontendRoute_Stage2_1 -> PathSegment "stage2-1" $ unitEncoder mempty
      FrontendRoute_Stage2_2 -> PathSegment "stage2-2" $ unitEncoder mempty


concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''FrontendRouteStage
  ]
