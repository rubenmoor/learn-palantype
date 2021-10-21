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
import           Data.Text             (Text)

import           Control.Category      (Category (id))
import           Control.Monad.Except  (MonadError)
import           Data.Either           (Either)
import           Data.Function         (($))
import           Data.Functor          ((<$>))
import           Data.List             (concat)
import           Data.Monoid           (Monoid (mempty))
import           Data.Traversable      (mapM)
import           Obelisk.Route         (mkFullRouteEncoder, pattern (:/), Encoder,
                                        FullRoute (FullRoute_Backend), PageName,
                                        R, SegmentResult (PathEnd, PathSegment),
                                        pathComponentEncoder, unitEncoder)
import           Obelisk.Route.TH      (deriveRouteComponent)

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute PageName

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_EN   :: FrontendRoute (R FrontendSubroute_Stage)
  FrontendRoute_DE   :: FrontendRoute (R FrontendSubroute_Stage)

data FrontendSubroute_Stage :: * -> * where
  FrontendSubroute_Introduction :: FrontendSubroute_Stage ()
  FrontendSubroute_Stage1_1 :: FrontendSubroute_Stage ()
  FrontendSubroute_Stage1_2 :: FrontendSubroute_Stage ()
  FrontendSubroute_Stage1_3 :: FrontendSubroute_Stage ()
  FrontendSubroute_Stage1_4 :: FrontendSubroute_Stage ()
  FrontendSubroute_Stage1_5 :: FrontendSubroute_Stage ()
  FrontendSubroute_Stage1_6 :: FrontendSubroute_Stage ()
  FrontendSubroute_Stage1_7 :: FrontendSubroute_Stage ()
  FrontendSubroute_Stage2_1 :: FrontendSubroute_Stage ()
  FrontendSubroute_Stage2_2 :: FrontendSubroute_Stage ()

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
      FrontendRoute_EN   -> PathSegment "EN" encoderSubrouteStage
      FrontendRoute_DE   -> PathSegment "DE" encoderSubrouteStage
  )
  where
    encoderSubrouteStage
      :: forall (check :: * -> *) (parse :: * -> *).
      ( MonadError Text check
      , check ~ parse
      )
      => Encoder check parse (R FrontendSubroute_Stage) PageName
    encoderSubrouteStage = pathComponentEncoder $ \case
      FrontendSubroute_Introduction -> PathSegment "introduction" $ unitEncoder mempty
      FrontendSubroute_Stage1_1 -> PathSegment "stage1-1" $ unitEncoder mempty
      FrontendSubroute_Stage1_2 -> PathSegment "stage1-2" $ unitEncoder mempty
      FrontendSubroute_Stage1_3 -> PathSegment "stage1-3" $ unitEncoder mempty
      FrontendSubroute_Stage1_4 -> PathSegment "stage1-4" $ unitEncoder mempty
      FrontendSubroute_Stage1_5 -> PathSegment "stage1-5" $ unitEncoder mempty
      FrontendSubroute_Stage1_6 -> PathSegment "stage1-6" $ unitEncoder mempty
      FrontendSubroute_Stage1_7 -> PathSegment "stage1-7" $ unitEncoder mempty
      FrontendSubroute_Stage2_1 -> PathSegment "stage2-1" $ unitEncoder mempty
      FrontendSubroute_Stage2_2 -> PathSegment "stage2-2" $ unitEncoder mempty


concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''FrontendSubroute_Stage
  ]
