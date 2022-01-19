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

import Control.Category (Category ((.)))
import           Data.Functor.Identity (Identity)
import           Data.Text             (Text)
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
import Common.Stage (Stage)
import Control.Category (Category(id))


data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute PageName

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_EN   :: FrontendRoute Stage
  FrontendRoute_DE   :: FrontendRoute Stage

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

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
