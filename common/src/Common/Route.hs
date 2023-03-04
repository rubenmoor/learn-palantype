{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE UndecidableInstances #-}

module Common.Route where

import           Control.Applicative            ( Applicative )
import           Control.Category               ( Category((.), id) )
import           Control.Lens.Wrapped           ( Unwrapped
                                                , Wrapped
                                                )
import           Control.Monad.Except           ( MonadError )
import           Data.Either                    ( Either(..) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>) )
import           Data.Functor.Identity          ( Identity )
import           Data.List                      ( concat )
import           Data.Monoid                    ( Monoid(mempty) )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Lens                 ( IsText )
import           Data.Traversable               ( mapM )
import           GHC.Real                       ( Integral )
import           Numeric.Lens                   ( base )
import           Obelisk.Route                  ( pattern (:/)
                                                , Encoder
                                                , FullRoute(FullRoute_Backend)
                                                , PageName
                                                , R
                                                , SegmentResult
                                                    ( PathEnd
                                                    , PathSegment
                                                    )
                                                , checkEncoder
                                                , mkFullRouteEncoder
                                                , packTextEncoder
                                                , pathComponentEncoder
                                                , renderFrontendRoute
                                                , reviewEncoder
                                                , singlePathSegmentEncoder
                                                , unitEncoder
                                                , unwrappedEncoder
                                                )
import           Obelisk.Route.TH               ( deriveRouteComponent )
import           Palantype.Common.Stage         ( StageIndex )
import           Palantype.Common.TH            ( failure )

data FrontendRoute_AdminPages :: * -> * where
  AdminPage_Journal            :: FrontendRoute_AdminPages ()
  AdminPage_CreateMissingFiles :: FrontendRoute_AdminPages ()

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
  FrontendRoute_Admin :: FrontendRoute (R FrontendRoute_AdminPages)

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
        FrontendRoute_DE   -> PathSegment "DE" $ singlePathSegmentEncoder . wrappedIntEncoder
        FrontendRoute_EN   -> PathSegment "EN" $ singlePathSegmentEncoder . wrappedIntEncoder
        FrontendRoute_Auth  -> PathSegment "auth" $ pathComponentEncoder \case
            AuthPage_SignUp   -> PathSegment "signup"   $ unitEncoder mempty
            AuthPage_Login    -> PathSegment "login"    $ unitEncoder mempty
            AuthPage_Settings -> PathSegment "settings" $ unitEncoder mempty
        FrontendRoute_Admin -> PathSegment "admin" $ pathComponentEncoder \case
            AdminPage_Journal            -> PathSegment "journal" $ unitEncoder mempty
            AdminPage_CreateMissingFiles -> PathSegment "create-missing-files" $ unitEncoder mempty
    )
  where
    wrappedIntEncoder
      :: forall (check :: * -> *) (parse :: * -> *) text a
      . ( Applicative check
        , IsText text
        , Integral (Unwrapped a)
        , MonadError Text parse
        , Wrapped a
        )
      => Encoder check parse a text
    wrappedIntEncoder = packTextEncoder . reviewEncoder (base 10) . unwrappedEncoder

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''FrontendRoute_AuthPages
  , ''FrontendRoute_AdminPages
  ]

showRoute :: R FrontendRoute -> Text
showRoute = renderFrontendRoute $ case checkEncoder fullRouteEncoder of
    Left strError -> $failure $ "Couldn't render route: " <> Text.unpack strError
    Right enc -> enc
