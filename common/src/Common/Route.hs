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
{-# LANGUAGE BlockArguments #-}

module Common.Route where

import           Control.Category               ( Category((.)) )
import           Data.Functor.Identity          ( Identity )
import           Data.Text                      ( Text )
import           Data.Either                    (Either (..) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>) )
import           Data.List                      ( concat )
import           Data.Monoid                    ( Monoid(mempty) )
import           Data.Traversable               ( mapM )
import           Obelisk.Route                  (unsafeTshowEncoder, integralEncoder, pathSegmentEncoder, unwrappedEncoder, checkEncoder, renderFrontendRoute,  pathComponentEncoder
                                                , singlePathSegmentEncoder
                                                , mkFullRouteEncoder
                                                , pattern (:/)
                                                , Encoder
                                                , FullRoute(FullRoute_Backend)
                                                , PageName
                                                , R
                                                , SegmentResult
                                                    ( PathEnd
                                                    , PathSegment
                                                    )
                                                , unitEncoder
                                                )
import           Obelisk.Route.TH               ( deriveRouteComponent )
import           Common.Stage                   (StageIndex )
import           Control.Category               ( Category(id) )
import Palantype.Common.TH (failure)
import Data.Semigroup (Semigroup((<>)))
import qualified Data.Text as Text
import Palantype.Common.Internal (Lang)
import Control.Categorical.Bifunctor (bimap)

data FrontendRoute_AuthPages :: * -> * where
  AuthPage_SignUp   :: FrontendRoute_AuthPages ()
  AuthPage_Login    :: FrontendRoute_AuthPages ()
  AuthPage_Settings :: FrontendRoute_AuthPages ()

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api     :: BackendRoute PageName

data FrontendRoute :: * -> * where
  FrontendRoute_Main  :: FrontendRoute ()
  FrontendRoute_DE    :: FrontendRoute StageIndex
  FrontendRoute_EN    :: FrontendRoute StageIndex
  FrontendRoute_Auth  :: FrontendRoute (R FrontendRoute_AuthPages)
  FrontendRoute_Admin :: FrontendRoute ()

fullRouteEncoder
    :: Encoder
           (Either Text)
           Identity
           (R (FullRoute BackendRoute FrontendRoute))
           PageName
fullRouteEncoder = mkFullRouteEncoder
    (FullRoute_Backend BackendRoute_Missing :/ ())
    (\case
        BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
        BackendRoute_Api     -> PathSegment "api" id
    )
    (\case
        FrontendRoute_Main -> PathEnd $ unitEncoder mempty
        FrontendRoute_DE   -> PathSegment "DE" $ singlePathSegmentEncoder . unsafeTshowEncoder
        FrontendRoute_EN   -> PathSegment "EN" $ singlePathSegmentEncoder . unsafeTshowEncoder
        FrontendRoute_Auth  -> PathSegment "auth" $ pathComponentEncoder \case
            AuthPage_SignUp   -> PathSegment "signup"   $ unitEncoder mempty
            AuthPage_Login    -> PathSegment "login"    $ unitEncoder mempty
            AuthPage_Settings -> PathSegment "settings" $ unitEncoder mempty
        FrontendRoute_Admin -> PathSegment "admin" $ unitEncoder mempty
    )

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''FrontendRoute_AuthPages
  ]

showRoute :: R FrontendRoute -> Text
showRoute = renderFrontendRoute $ case checkEncoder fullRouteEncoder of
    Left strError -> $failure $ "Couldn't render route: " <> Text.unpack strError
    Right enc -> enc
